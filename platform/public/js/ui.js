// Small DOM and API helpers shared by the pages.

/** el("div.card#x", {onclick}, "text", child…) */
export function el(spec, attrs = {}, ...children) {
  const [, tag = "div", id, classes] = spec.match(/^([a-z0-9]+)?(?:#([\w-]+))?((?:\.[\w-]+)*)$/i) ?? [];
  const node = document.createElement(tag);
  if (id) node.id = id;
  if (classes) node.className = classes.slice(1).replace(/\./g, " ");
  for (const [k, v] of Object.entries(attrs ?? {})) {
    if (v === undefined || v === null || v === false) continue;
    if (k.startsWith("on")) node.addEventListener(k.slice(2), v);
    else if (k === "dataset") Object.assign(node.dataset, v);
    else if (k in node && typeof v !== "string") node[k] = v;
    else node.setAttribute(k, v === true ? "" : v);
  }
  for (const c of children.flat()) if (c !== null && c !== undefined && c !== false) node.append(c instanceof Node ? c : String(c));
  return node;
}

export const $ = (sel, root = document) => root.querySelector(sel);

export function message(kind, text, list = []) {
  return el(`div.msg.${kind}`, { role: kind === "err" ? "alert" : "status" }, text, list.length ? el("ul", {}, list.map((i) => el("li", {}, i))) : null);
}

export function show(container, ...nodes) {
  container.replaceChildren(...nodes.flat().filter(Boolean));
}

export function formatDateTime(iso) {
  if (!iso) return "";
  return new Date(iso).toLocaleString("el-GR", { weekday: "long", day: "numeric", month: "long", year: "numeric", hour: "2-digit", minute: "2-digit" });
}

// ---------- API ----------

export function createApi(role) {
  const key = `omiloi.${role}.token`;
  const store = {
    get: () => { try { return sessionStorage.getItem(key); } catch { return null; } },
    set: (t) => { try { t ? sessionStorage.setItem(key, t) : sessionStorage.removeItem(key); } catch { /* private mode */ } },
  };
  let token = store.get();
  const api = async (method, path, body) => {
    const res = await fetch(path, {
      method,
      headers: { "content-type": "application/json", ...(token ? { authorization: `Bearer ${token}` } : {}) },
      body: body === undefined ? undefined : JSON.stringify(body),
    });
    const isJson = (res.headers.get("content-type") ?? "").includes("json");
    const data = isJson ? await res.json() : res;
    if (!res.ok) {
      if (res.status === 401 && token && !path.endsWith("/login")) api.onExpired?.();
      const err = new Error(isJson ? data.error : `Σφάλμα ${res.status}`);
      err.status = res.status;
      err.data = data;
      throw err;
    }
    return data;
  };
  api.setToken = (t) => { token = t; store.set(t); };
  api.hasToken = () => Boolean(token);
  api.download = async (path, filename) => {
    const res = await fetch(path, { headers: token ? { authorization: `Bearer ${token}` } : {} });
    if (!res.ok) throw new Error((await res.json().catch(() => ({}))).error ?? `Σφάλμα ${res.status}`);
    const url = URL.createObjectURL(await res.blob());
    const a = el("a", { href: url, download: filename });
    document.body.append(a);
    a.click();
    a.remove();
    setTimeout(() => URL.revokeObjectURL(url), 1000);
  };
  return api;
}

/** Disable a button while an async action runs; show errors in `out`. */
export async function busy(button, out, fn) {
  button.disabled = true;
  try {
    return await fn();
  } catch (err) {
    if (out) show(out, message("err", err.message, err.data?.problems ?? []));
    else alert(err.message);
  } finally {
    button.disabled = false;
  }
}
