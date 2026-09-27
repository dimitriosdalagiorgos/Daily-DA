// In-memory stand-in for Supabase's REST API (PostgREST), covering what
// store-supabase.js uses: eq / like filters, select, order, limit,
// Prefer: return=representation|minimal, 409 on duplicate key.
// Requests are handled one at a time with a random delay first, so tests
// see interleavings like real concurrent requests.

export function createFakePostgrest({ key = "sb_secret_test" } = {}) {
  const tables = { kv: [], log: [] };
  let nextId = 1;
  const requests = [];

  const matches = (row, filters) => filters.every(([col, op, val]) => {
    const v = row[col];
    if (op === "eq") return String(v) === val;
    if (op === "like") {
      let re = "";
      for (let i = 0; i < val.length; i++) {
        const c = val[i];
        if (c === "\\") re += val[++i].replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
        else if (c === "*" || c === "%") re += ".*";
        else if (c === "_") re += ".";
        else re += c.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
      }
      return new RegExp(`^${re}$`, "s").test(String(v));
    }
    throw new Error(`fake: unsupported op ${op}`);
  });

  async function fetch(url, { method = "GET", headers = {}, body } = {}) {
    await new Promise((r) => setTimeout(r, Math.random() * 3));
    const u = new URL(url);
    requests.push({ method, path: u.pathname, search: u.search, headers });
    if (headers.apikey !== key) return new Response('{"message":"bad key"}', { status: 401 });
    const table = u.pathname.replace(/^\/rest\/v1\//, "");
    const rows = tables[table];
    if (!rows) return new Response("", { status: 404 });
    const filters = [];
    let select = null, order = null, limit = Infinity;
    for (const [k, v] of u.searchParams) {
      if (k === "select") select = v.split(",");
      else if (k === "order") order = v.split(".");
      else if (k === "limit") limit = Number(v);
      else {
        const dot = v.indexOf(".");
        filters.push([k, v.slice(0, dot), v.slice(dot + 1)]);
      }
    }
    const pick = (r) => (select ? Object.fromEntries(select.map((c) => [c, structuredClone(r[c])])) : structuredClone(r));
    const prefer = headers.prefer ?? "";

    if (method === "GET") {
      let out = rows.filter((r) => matches(r, filters));
      if (order) out.sort((a, b) => (a[order[0]] < b[order[0]] ? -1 : a[order[0]] > b[order[0]] ? 1 : 0) * (order[1] === "desc" ? -1 : 1));
      return Response.json(out.slice(0, limit).map(pick));
    }
    if (method === "POST") {
      const data = JSON.parse(body);
      if (table === "kv") {
        if (rows.some((r) => r.key === data.key)) return new Response('{"code":"23505"}', { status: 409 });
        rows.push({ version: 1, updated_at: new Date().toISOString(), ...data });
      } else {
        rows.push({ id: nextId++, created_at: new Date().toISOString(), ...data });
      }
      return prefer.includes("return=representation") ? Response.json([rows.at(-1)], { status: 201 }) : new Response("", { status: 201 });
    }
    if (method === "PATCH") {
      const data = JSON.parse(body);
      const hit = rows.filter((r) => matches(r, filters));
      hit.forEach((r) => Object.assign(r, data));
      return prefer.includes("return=representation") ? Response.json(hit.map(pick)) : new Response(null, { status: 204 });
    }
    if (method === "DELETE") {
      tables[table] = rows.filter((r) => !matches(r, filters));
      return new Response(null, { status: 204 });
    }
    return new Response("", { status: 405 });
  }

  return { fetch, tables, requests, key };
}
