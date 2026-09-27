// Supabase storage (PostgREST over fetch, no dependencies). Same interface
// as createMemoryStore() in store.js. Tables: supabase/schema.sql.
//
//   kv(key, value jsonb, version)   one row per key; update() is atomic by
//                                   optimistic locking on `version`
//   log(id, kind, data jsonb)       append-only (events, outbox)
//
// Only the server talks to Supabase, with the secret key; the tables have
// row-level security on and no policies, so the public keys see nothing.

const MAX_RETRIES = 8;

export function createSupabaseStore({ url, key, fetch: fetchImpl = globalThis.fetch }) {
  if (!url || !key) throw new Error("Λείπουν SUPABASE_URL / SUPABASE_SECRET_KEY.");
  const base = `${url.replace(/\/$/, "")}/rest/v1`;
  // New secret keys (sb_secret_…) go only in `apikey`; legacy JWT keys also
  // as Bearer token.
  const auth = key.startsWith("sb_") ? { apikey: key } : { apikey: key, authorization: `Bearer ${key}` };

  async function call(method, path, { body, prefer } = {}) {
    const res = await fetchImpl(`${base}${path}`, {
      method,
      headers: { ...auth, "content-type": "application/json", ...(prefer ? { prefer } : {}) },
      body: body === undefined ? undefined : JSON.stringify(body),
    });
    if (res.status === 409) return { conflict: true };
    if (!res.ok) throw new Error(`Supabase ${method} ${path.split("?")[0]}: ${res.status} ${await res.text()}`);
    const text = await res.text();
    return { rows: text ? JSON.parse(text) : [] };
  }

  const eq = (k) => `eq.${encodeURIComponent(k)}`;

  async function read(key) {
    const { rows } = await call("GET", `/kv?key=${eq(key)}&select=value,version`);
    return rows[0] ?? null;
  }

  async function update(key, fn) {
    for (let attempt = 0; attempt < MAX_RETRIES; attempt++) {
      const row = await read(key);
      const next = await fn(row ? structuredClone(row.value) : undefined);
      if (row) {
        const { rows } = await call("PATCH", `/kv?key=${eq(key)}&version=eq.${row.version}`, {
          body: { value: next ?? null, version: row.version + 1, updated_at: new Date().toISOString() },
          prefer: "return=representation",
        });
        if (rows.length === 1) return next;
      } else {
        const r = await call("POST", "/kv", { body: { key, value: next ?? null, version: 1 }, prefer: "return=minimal" });
        if (!r.conflict) return next;
      }
      // Someone else changed the row in between: read again and retry.
      await new Promise((resolve) => setTimeout(resolve, 20 * (attempt + 1) + Math.random() * 30));
    }
    throw new Error(`Supabase: η εγγραφή «${key}» άλλαζε συνεχώς· δοκιμάστε ξανά.`);
  }

  return {
    async get(key) {
      const row = await read(key);
      return row ? row.value : undefined;
    },
    async set(key, value) {
      await update(key, () => value);
    },
    update,
    async list(prefix) {
      // PostgREST `like` uses * as wildcard; escape the SQL wildcards.
      const pattern = `${prefix.replace(/[\\%_]/g, (c) => `\\${c}`)}*`;
      const { rows } = await call("GET", `/kv?key=like.${encodeURIComponent(pattern)}&select=key,value&order=key`);
      return rows;
    },
    async delete(key) {
      await call("DELETE", `/kv?key=${eq(key)}`);
    },
    async append(kind, item) {
      await call("POST", "/log", { body: { kind, data: item }, prefer: "return=minimal" });
    },
    async readLog(kind, limit = 200) {
      const { rows } = await call("GET", `/log?kind=${eq(kind)}&select=data&order=id.desc&limit=${Number(limit)}`);
      return rows.map((r) => r.data).reverse();
    },
  };
}
