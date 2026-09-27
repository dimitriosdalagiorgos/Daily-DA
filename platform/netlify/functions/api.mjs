// Netlify Function for /api/* — the same handler as the local dev server.
// Storage: Supabase (to be configured in step 4). Until then the API
// answers 503 instead of keeping data in memory that would be lost.

import { createApp } from "../../src/server/app.js";

let handle = null;

async function getHandler() {
  if (handle) return handle;
  const { SUPABASE_URL, SUPABASE_SERVICE_ROLE_KEY, SESSION_SECRET, ADMIN_PASSWORD, URL: siteUrl } = process.env;
  if (!SUPABASE_URL || !SUPABASE_SERVICE_ROLE_KEY) return null;
  const { createSupabaseStore } = await import("../../src/server/store-supabase.js");
  handle = createApp({
    store: createSupabaseStore({ url: SUPABASE_URL, key: SUPABASE_SERVICE_ROLE_KEY }),
    env: { SESSION_SECRET, ADMIN_PASSWORD, BASE_URL: siteUrl },
  });
  return handle;
}

export default async (request) => {
  const h = await getHandler();
  if (!h) {
    return new Response(JSON.stringify({ error: "Η αποθήκευση δεν έχει ρυθμιστεί ακόμα (Supabase)." }), {
      status: 503,
      headers: { "content-type": "application/json; charset=utf-8" },
    });
  }
  return h(request);
};

export const config = { path: "/api/*" };
