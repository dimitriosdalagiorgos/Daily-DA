// Plain Node.js HTTP server for the platform: the site (public/), the
// teachers' and admin's pages at their hidden addresses (see paths.js), the
// browser-safe parts of src/ as /lib/, the clubs template, and /api/*
// through the same handler as the Netlify Function. Used by the local dev
// server (dev/server.mjs) and the production server (server/start.mjs).

import { createServer } from "node:http";
import { existsSync, readFileSync, statSync } from "node:fs";
import { extname, join, normalize } from "node:path";
import { ROLE_PAGES } from "./paths.js";

const TYPES = {
  ".html": "text/html; charset=utf-8", ".js": "text/javascript; charset=utf-8", ".mjs": "text/javascript; charset=utf-8",
  ".css": "text/css; charset=utf-8", ".svg": "image/svg+xml", ".xlsx": "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
  ".json": "application/json", ".ico": "image/x-icon", ".pdf": "application/pdf",
  ".docx": "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
};

function staticFile(root, pathname, paths) {
  for (const [role, file] of Object.entries(ROLE_PAGES)) {
    if (pathname === paths[role]) return join(root, "public", file);
    // the page files themselves are reachable only at the hidden address
    if (pathname === `/${file}`) return null;
  }
  const map = [
    ["/lib/", join(root, "src")],
    ["/templates/", join(root, "templates")],
    ["/vendor/xlsx.mjs", join(root, "node_modules", "xlsx", "xlsx.mjs")],
    ["/", join(root, "public")],
  ];
  for (const [prefix, dir] of map) {
    if (!pathname.startsWith(prefix)) continue;
    const rel = prefix.endsWith("/") ? pathname.slice(prefix.length) : "";
    let file = normalize(join(dir, decodeURIComponent(rel)));
    if (!file.startsWith(dir)) return null;
    if (existsSync(file) && statSync(file).isDirectory()) file = join(file, "index.html");
    // /lib/ exposes only the browser-safe parts of src/
    if (prefix === "/lib/" && !/^(algorithm|import|export)\//.test(rel)) return null;
    return existsSync(file) ? file : null;
  }
  return null;
}

/**
 * @param {{handle: (req: Request) => Promise<Response>, root: string, port: number, host?: string,
 *          paths: {teacher: string, admin: string}, headers?: Record<string, string>, onListen?: () => void}} opts
 * @returns {import("node:http").Server}
 */
export function serveNode({ handle, root, port, host, paths, headers = {}, onListen }) {
  const server = createServer(async (req, res) => {
    const url = new URL(req.url, `http://localhost:${port}`);
    try {
      if (url.pathname.startsWith("/api/")) {
        const chunks = [];
        for await (const c of req) chunks.push(c);
        const request = new Request(url, {
          method: req.method,
          headers: req.headers,
          body: ["GET", "HEAD"].includes(req.method) ? undefined : Buffer.concat(chunks),
        });
        const response = await handle(request);
        res.writeHead(response.status, { ...headers, ...Object.fromEntries(response.headers) });
        res.end(Buffer.from(await response.arrayBuffer()));
        return;
      }
      if (Object.values(paths).includes(`${url.pathname}/`)) {
        res.writeHead(301, { ...headers, location: `${url.pathname}/` }).end();
        return;
      }
      const file = staticFile(root, url.pathname, paths);
      if (!file) {
        res.writeHead(404, { ...headers, "content-type": "text/plain; charset=utf-8" }).end("Δεν βρέθηκε");
        return;
      }
      res.writeHead(200, { ...headers, "content-type": TYPES[extname(file)] ?? "application/octet-stream", "cache-control": "no-store" });
      res.end(readFileSync(file));
    } catch (err) {
      console.error(err);
      res.writeHead(500).end("Σφάλμα");
    }
  });
  server.listen(port, host, onListen);
  return server;
}
