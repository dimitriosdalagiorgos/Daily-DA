#!/bin/sh
# Update the platform on a school's own server from the main branch on GitHub,
# with a data backup first and an automatic return to the previous version if
# the new one does not start. Installed as /usr/local/sbin/omiloi-update
# (README §6.7). Messages are in Greek: the school's administrator reads them.
#
#   sudo omiloi-update           update
#   sudo omiloi-update --check   only report whether a newer version exists
#
# Assumes the layout of README §6: code in /opt/omiloi/app (a clone of the
# main branch), settings in /etc/omiloi.env, data in DATA_DIR/store.json and
# the systemd unit «omiloi».
set -u
A=/opt/omiloi/app
E=/etc/omiloi.env

[ "$(id -u)" -eq 0 ] || { echo "Τρέξτε το με sudo: sudo omiloi-update"; exit 1; }
P=$(sed -n 's/^PORT=//p' "$E"); P=${P:-8080}
D=$(sed -n 's/^DATA_DIR=//p' "$E"); D=${D:-$A/platform/.data}

cur=$(git -C "$A" rev-parse HEAD)
new=$(GIT_TERMINAL_PROMPT=0 git -C "$A" ls-remote origin refs/heads/main | cut -f1)
[ -n "$new" ] || { echo "✗ Δεν ήταν δυνατή η σύνδεση με το GitHub. Δεν άλλαξε τίποτα."; exit 1; }
echo "Τρέχουσα έκδοση: $(git -C "$A" log -1 --format='%h %ci %s')"
if [ "$cur" = "$new" ]; then echo "✓ Η πλατφόρμα είναι ήδη ενημερωμένη."; exit 0; fi
echo "Υπάρχει νεότερη έκδοση στο GitHub: $(echo "$new" | cut -c1-7)"
[ "${1:-}" = "--check" ] && exit 0

# Back up to a temporary name first, so a failed copy (e.g. a full disk)
# neither starts the update nor truncates the previous backup.
B=/var/backups/omiloi-before-update.json
if [ -f "$D/store.json" ] && ! { install -m 600 "$D/store.json" "$B.new" && mv -f "$B.new" "$B"; }; then
  rm -f "$B.new"; echo "✗ Δεν ήταν δυνατό να γίνει αντίγραφο των δεδομένων (γεμάτος δίσκος;). Δεν άλλαξε τίποτα."; exit 1
fi
if ! { GIT_TERMINAL_PROMPT=0 git -C "$A" fetch -q --depth 1 origin main && git -C "$A" reset -q --hard FETCH_HEAD; }; then
  echo "✗ Η λήψη απέτυχε. Η πλατφόρμα έμεινε όπως ήταν."; exit 1
fi
systemctl restart omiloi; sleep 3
if systemctl is-active --quiet omiloi && curl -fs -o /dev/null "http://127.0.0.1:$P/api/public"; then
  echo "✓ Ενημερώθηκε: $(git -C "$A" log -1 --format='%h %ci %s')"
else
  echo "✗ Η νέα έκδοση δεν ξεκίνησε. Μήνυμα:"; journalctl -u omiloi -n 5 --no-pager -o cat
  git -C "$A" reset -q --hard "$cur"; systemctl restart omiloi; sleep 3
  if systemctl is-active --quiet omiloi; then
    echo "↩ Επιστροφή στην προηγούμενη έκδοση: $(git -C "$A" log -1 --format='%h')"
  else
    echo "✗ Ούτε η προηγούμενη έκδοση ξεκίνησε. Δείτε: sudo journalctl -u omiloi -e"
  fi
  exit 1
fi
