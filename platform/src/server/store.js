// Storage behind a small key-value interface, so the backend can change
// (local JSON file now; Supabase later — one table kv(key, value jsonb)).
//
//   get(key)            → value or undefined
//   set(key, value)
//   update(key, fn)     → fn(current) returns the new value; serialized per store
//   list(prefix)        → [{key, value}] for keys starting with prefix
//   delete(key)
//
// Values are JSON-serializable. Keys used by the app:
//   settings, students, clubs, teachers, teacherList:<code>,
//   submission:<am>, results, outbox, events

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
  };
}
