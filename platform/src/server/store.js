// Storage behind a small interface, so the backend can change: memory /
// JSON file locally, Supabase in production (store-supabase.js).
//
//   get(key)              → value or undefined
//   set(key, value)
//   update(key, fn)       → fn(current) returns the new value; atomic per key
//   list(prefix)          → [{key, value}] for keys starting with prefix
//   delete(key)
//   deletePrefix(prefix)  delete every key starting with prefix, at once
//                         (platform reset: hundreds of keys, one request)
//   append(kind, item)    → add to an append-only log (no lost writes when
//                           many people act at once)
//   readLog(kind, limit)  → the last `limit` items, oldest first
//   deleteLog(kind)       → remove a whole log (platform reset)
//   setMany(entries)      → write many keys at once ([{key, value}]); for
//                           admin bulk operations (few requests to Supabase)
//
// Values are JSON-serializable. Keys used by the app:
//   settings, students, clubs, teachers, teacherList:<code>,
//   submission:<am>, results, resultsLog, sessionSecret
// Logs: events, outbox

import { existsSync, mkdirSync, readFileSync, renameSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";

const clone = (v) => (v === undefined ? undefined : structuredClone(v));

export function createMemoryStore(initial = {}) {
  const data = new Map(Object.entries(structuredClone(initial)));
  let chain = Promise.resolve();
  const serialize = (fn) => (chain = chain.then(fn, fn));
  return {
    async get(key) {
      return clone(data.get(key));
    },
    set(key, value) {
      return serialize(() => {
        data.set(key, clone(value));
      });
    },
    update(key, fn) {
      return serialize(async () => {
        const next = await fn(clone(data.get(key)));
        data.set(key, clone(next));
        return clone(next);
      });
    },
    async list(prefix) {
      return [...data].filter(([k]) => k.startsWith(prefix)).map(([key, value]) => ({ key, value: clone(value) }));
    },
    delete(key) {
      return serialize(() => {
        data.delete(key);
      });
    },
    deletePrefix(prefix) {
      return serialize(() => {
        for (const k of [...data.keys()]) if (k.startsWith(prefix)) data.delete(k);
      });
    },
    append(kind, item) {
      return serialize(() => {
        const log = data.get(`log:${kind}`) ?? [];
        log.push(clone(item));
        data.set(`log:${kind}`, log);
      });
    },
    async readLog(kind, limit = 200) {
      return clone((data.get(`log:${kind}`) ?? []).slice(-limit));
    },
    deleteLog(kind) {
      return serialize(() => {
        data.delete(`log:${kind}`);
      });
    },
    setMany(entries) {
      return serialize(() => {
        for (const { key, value } of entries) data.set(key, clone(value));
      });
    },
    snapshot: () => Object.fromEntries([...data].map(([k, v]) => [k, clone(v)])),
  };
}

/** Memory store persisted to a JSON file after every write (local dev only). */
export function createFileStore(path) {
  const initial = existsSync(path) ? JSON.parse(readFileSync(path, "utf8")) : {};
  const mem = createMemoryStore(initial);
  const persist = () => {
    mkdirSync(dirname(path), { recursive: true });
    writeFileSync(`${path}.tmp`, JSON.stringify(mem.snapshot(), null, 1));
    renameSync(`${path}.tmp`, path);
  };
  const after = (p) => p.then((v) => (persist(), v));
  return {
    ...mem,
    set: (key, value) => after(mem.set(key, value)),
    update: (key, fn) => after(mem.update(key, fn)),
    delete: (key) => after(mem.delete(key)),
    deletePrefix: (prefix) => after(mem.deletePrefix(prefix)),
    append: (kind, item) => after(mem.append(kind, item)),
    deleteLog: (kind) => after(mem.deleteLog(kind)),
    setMany: (entries) => after(mem.setMany(entries)),
  };
}
