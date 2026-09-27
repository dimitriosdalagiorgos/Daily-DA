-- Πλατφόρμα ομίλων: πίνακες στο Supabase.
-- Εκτελείται μία φορά: Supabase → SQL Editor → New query → επικόλληση → Run.

-- Όλα τα δεδομένα (ρυθμίσεις, μαθητές, όμιλοι, δηλώσεις, αποτελέσματα):
-- μία γραμμή ανά κλειδί. Το version εμποδίζει δύο ταυτόχρονες αλλαγές να
-- σβήσουν η μία την άλλη.
create table if not exists public.kv (
  key        text primary key,
  value      jsonb,
  version    integer not null default 1,
  updated_at timestamptz not null default now()
);

-- Ιστορικό ενεργειών και εξερχόμενα email: μόνο προσθήκες.
create table if not exists public.log (
  id         bigint generated always as identity primary key,
  kind       text not null,
  data       jsonb not null,
  created_at timestamptz not null default now()
);
create index if not exists log_kind_id on public.log (kind, id);

-- Πρόσβαση μόνο από τον διακομιστή της πλατφόρμας (secret key).
-- Με RLS ενεργό και χωρίς policies, τα δημόσια κλειδιά δεν βλέπουν τίποτα.
alter table public.kv  enable row level security;
alter table public.log enable row level security;
revoke all on public.kv, public.log from anon, authenticated;
