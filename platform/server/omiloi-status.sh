#!/bin/sh
# Health check of the platform on a school's own server: one line per check,
# ✓ (fine), ⚠ (look again tomorrow) or ✗ (act now, WORKFLOW §7.5). Read-only:
# it changes nothing and prints no secrets, addresses or student data, so the
# output can be sent as is. Installed as /usr/local/sbin/omiloi-status
# (README §6.7). Messages are in Greek: the school's administrator reads them.
#
#   sudo omiloi-status
#
# Exit status 1 if any check is ✗. Same layout assumptions as omiloi-update;
# OMILOI_ENV and OMILOI_BACKUP_DIR override the paths (tests only).
set -u
A=/opt/omiloi/app
E=${OMILOI_ENV:-/etc/omiloi.env}
B=${OMILOI_BACKUP_DIR:-/var/backups}

[ "$(id -u)" -eq 0 ] || { echo "Τρέξτε το με sudo: sudo omiloi-status"; exit 1; }
P=$(sed -n 's/^PORT=//p' "$E"); P=${P:-8080}
D=$(sed -n 's/^DATA_DIR=//p' "$E"); D=${D:-$A/platform/.data}
U=$(sed -n 's/^BASE_URL=//p' "$E")
H=$(echo "$U" | sed -E 's#^[a-z]+://##; s#[/:].*##')

bad=0
ok()   { echo "✓ $*"; }
warn() { echo "⚠ $*"; }
fail() { echo "✗ $*"; bad=1; }
mb()   { echo $(( $1 / 1048576 )); }

echo "Έλεγχος πλατφόρμας ομίλων · $(date '+%d/%m/%Y %H:%M') · VM σε λειτουργία $(uptime -p | sed 's/^up //')"

# Service, restarts and memory
if systemctl is-active --quiet omiloi; then
  since=$(systemctl show omiloi -p ActiveEnterTimestamp --value)
  ok "Η πλατφόρμα τρέχει (από $(date -d "$since" '+%d/%m %H:%M' 2>/dev/null || echo "$since"))"
else
  fail "Η πλατφόρμα δεν τρέχει: sudo systemctl restart omiloi"
fi
n=$(systemctl show omiloi -p NRestarts --value); n=${n:-0}
if [ "$n" -eq 0 ]; then ok "Καμία αυτόματη επανεκκίνηση"
else warn "Αυτόματες επανεκκινήσεις μετά από πρόβλημα: $n (στείλτε το στο Claude)"; fi
# Memory against the service's own limit (MemoryMax, README §6.2): a warning
# above 80% of it
m=$(systemctl show omiloi -p MemoryCurrent --value)
x=$(systemctl show omiloi -p MemoryMax --value)
case "$m" in
  ''|*[!0-9]*) ;;
  *) case "$x" in
       ''|*[!0-9]*) ok "Μνήμη πλατφόρμας: $(mb "$m") MB (χωρίς όριο)" ;;
       *) if [ $(( m * 5 )) -lt $(( x * 4 )) ]; then ok "Μνήμη πλατφόρμας: $(mb "$m") MB από $(mb "$x")"
          else warn "Μνήμη πλατφόρμας: $(mb "$m") MB από $(mb "$x") (κοντά στο όριο)"; fi ;;
     esac ;;
esac

# Answers: directly, and through the web server (Apache, or nginx: README §6.5)
# with the HTTPS certificate
W=apache2; systemctl is-active --quiet nginx && W=nginx
j=$(curl -fs -m 10 "http://127.0.0.1:$P/api/public")
if [ -n "$j" ]; then
  ph=$(echo "$j" | sed -n 's/.*"phase":"\([a-z]*\)".*/\1/p')
  case "$ph" in
    setup) ph="Προετοιμασία" ;; teachers) ph="Εκπαιδευτικοί" ;; parents) ph="Δηλώσεις γονέων" ;;
    closed) ph="Κλειστές δηλώσεις" ;; allocated) ph="Κατανομή" ;; published) ph="Ανακοίνωση" ;;
  esac
  ok "Η πλατφόρμα απαντά · φάση: $ph"
else
  fail "Η πλατφόρμα δεν απαντά: sudo journalctl -u omiloi -n 20 --no-pager"
fi
if [ -n "$H" ]; then
  if curl -fs -m 10 -o /dev/null --resolve "$H:443:127.0.0.1" "https://$H/api/public"; then
    ok "Ο $W και το HTTPS απαντούν"
  else
    fail "Ο $W ή το HTTPS δεν απαντά: sudo systemctl restart $W"
  fi
  if command -v openssl >/dev/null; then
    c=$(echo | openssl s_client -connect 127.0.0.1:443 -servername "$H" 2>/dev/null | openssl x509 -noout -enddate 2>/dev/null | sed 's/^notAfter=//')
    if [ -n "$c" ]; then
      days=$(( ($(date -d "$c" +%s) - $(date +%s)) / 86400 ))
      if [ "$days" -ge 14 ]; then ok "Πιστοποιητικό HTTPS: λήγει σε $days ημέρες"
      else warn "Πιστοποιητικό HTTPS: λήγει σε $days ημέρες (κανονικά ανανεώνεται μόνο του· WORKFLOW §7.4)"; fi
    fi
  fi
fi

# Data, disks, memory of the VM
if grep -qs '[[:space:]]/srv/data[[:space:]]' /etc/fstab && ! findmnt -n /srv/data >/dev/null; then
  fail "Ο δίσκος δεδομένων δεν είναι συνδεδεμένος: sudo mount /srv/data"
fi
S="$D/store.json"
if [ -s "$S" ]; then
  ok "Δεδομένα: $(du -k "$S" | cut -f1) KB, τελευταία αλλαγή $(date -r "$S" '+%d/%m %H:%M') · δηλώσεις: $(grep -c '^ "submission:' "$S")"
else
  fail "Δεν βρέθηκαν τα δεδομένα ($S)"
fi
seen=
for f in / "$D"; do
  mp=$(df -P "$f" 2>/dev/null | awk 'NR==2 { print $6 }')
  u=$(df -P "$f" 2>/dev/null | awk 'NR==2 { sub("%", "", $5); print $5 }')
  [ -n "$u" ] && [ "$mp" != "$seen" ] || continue
  seen=$mp
  t="Δίσκος $mp: γεμάτος $u%"
  if [ "$u" -ge 95 ]; then fail "$t (στείλτε στο Claude το: df -h / /srv/data)"
  elif [ "$u" -ge 85 ]; then warn "$t"
  else ok "$t"; fi
done
a=$(awk '/^MemAvailable:/ { print int($2 / 1024) }' /proc/meminfo)
if [ "$a" -ge 150 ]; then ok "Ελεύθερη μνήμη VM: $a MB"
else warn "Ελεύθερη μνήμη VM: μόνο $a MB"; fi

# Nightly backup (cron at 02:15): the newest must be at most ~26 hours old
b=$(ls -t "$B"/omiloi-[1-7].json 2>/dev/null | head -1)
if [ -z "$b" ]; then
  fail "Δεν υπάρχει νυχτερινό αντίγραφο στο $B"
elif [ -z "$(find "$b" -mmin -1560)" ]; then
  fail "Το τελευταίο νυχτερινό αντίγραφο είναι παλιό: $(date -r "$b" '+%d/%m %H:%M')"
elif ! node -e 'JSON.parse(require("fs").readFileSync(process.argv[1], "utf8"))' "$b" 2>/dev/null; then
  fail "Το τελευταίο νυχτερινό αντίγραφο είναι κενό ή κομμένο (γεμάτος δίσκος;): $(date -r "$b" '+%d/%m %H:%M')"
else
  ok "Νυχτερινό αντίγραφο: $(date -r "$b" '+%d/%m %H:%M'), $(du -k "$b" | cut -f1) KB"
fi

# Errors today
e=$(journalctl -u omiloi --since today -q --no-pager -o cat | grep -ciE 'error|failed|✗')
if [ "$e" -eq 0 ]; then ok "Κανένα λάθος σήμερα στην καταγραφή της πλατφόρμας"
else warn "Γραμμές με λάθος σήμερα: $e (στείλτε στο Claude: sudo journalctl -u omiloi --since today --no-pager)"; fi
L=/var/log/apache2/omiloi-access.log
if [ -f "$L" ]; then
  r=$(grep -c '" 5[0-9][0-9] ' "$L")
  if [ "$r" -eq 0 ]; then ok "Καμία αποτυχημένη σελίδα από το τελευταίο πρωί"
  else warn "Αποτυχημένες σελίδες από το τελευταίο πρωί: $r (στείλτε στο Claude: sudo tail -20 /var/log/apache2/omiloi-error.log)"; fi
fi

[ "$bad" -eq 0 ] && echo "Όλα καλά." || echo "Κάτι χρειάζεται προσοχή: WORKFLOW.md §7.5."
exit "$bad"
