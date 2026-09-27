// Netlify Function for /api/* — the same handler as the local dev server.
//
// Environment variables (Netlify → Project configuration → Environment variables):
//   SUPABASE_URL          Supabase → Project Settings → API → Project URL
//   SUPABASE_SECRET_KEY   Supabase → Project Settings → API Keys → Secret key
//                         (legacy name SUPABASE_SERVICE_ROLE_KEY also accepted)
//   ADMIN_PASSWORD        the admin password (choose one, ≥ 10 characters)
// Optional:
//   SESSION_SECRET        if missing, a random one is created once and kept
//                         in the database
// Until an e-mail service is configured, e-mails (including teachers' login
// links) are shown in the admin's «Εξερχόμενα» tab.

import { randomBytes } from "node:crypto";
import { createApp } from "../../src/server/app.js";
import { createSupabaseStore } from "../../src/server/store-supabase.js";

let handle = null;

const problem = (status, error) =>
  new Response(JSON.stringify({ error }), { status, headers: { "content-type": "application/json; charset=utf-8" } });

async function getHandler() {
  if (handle) return handle;
  const env = process.env;
  const key = env.SUPABASE_SECRET_KEY ?? env.SUPABASE_SERVICE_ROLE_KEY;
  if (!env.SUPABASE_URL || !key) return { missing: "Η αποθήκευση δεν έχει ρυθμιστεί ακόμα (SUPABASE_URL / SUPABASE_SECRET_KEY)." };
  if (!env.ADMIN_PASSWORD || env.ADMIN_PASSWORD.length < 10) return { missing: "Ορίστε ADMIN_PASSWORD (τουλάχιστον 10 χαρακτήρες) στο Netlify." };

  const store = createSupabaseStore({ url: env.SUPABASE_URL, key });
  const secret = env.SESSION_SECRET ?? (await store.update("sessionSecret", (s) => s ?? randomBytes(32).toString("base64url")));
  handle = createApp({
    store,
    env: { SESSION_SECRET: secret, ADMIN_PASSWORD: env.ADMIN_PASSWORD, BASE_URL: env.URL, SHOW_OUTBOX: true },
  });
  return handle;
}

export default async (request) => {
  try {
    const h = await getHandler();
    if (typeof h !== "function") return problem(503, h.missing);
    return await h(request);
  } catch (err) {
    console.error(err);
    handle = null;
    return problem(503, "Η βάση δεδομένων δεν απαντά. Ελέγξτε τις ρυθμίσεις Supabase ή δοκιμάστε σε λίγο.");
  }
};

export const config = { path: "/api/*" };
