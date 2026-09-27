// Sessions, magic links and passwords. No session table: tokens are
// signed (HMAC-SHA256 with SESSION_SECRET) and carry their own expiry.

import { createHmac, randomBytes, scryptSync, timingSafeEqual } from "node:crypto";

const b64url = (buf) => Buffer.from(buf).toString("base64url");

/** Token = base64url(JSON payload) + "." + signature. */
export function signToken(secret, payload, ttlSeconds, now = Date.now()) {
  const body = b64url(JSON.stringify({ ...payload, exp: Math.floor(now / 1000) + ttlSeconds }));
  const sig = b64url(createHmac("sha256", secret).update(body).digest());
  return `${body}.${sig}`;
}

/** Payload if the token is authentic and not expired, else null. */
export function verifyToken(secret, token, now = Date.now()) {
  if (typeof token !== "string" || !token.includes(".")) return null;
  const [body, sig] = token.split(".");
  const expected = createHmac("sha256", secret).update(body).digest();
  const given = Buffer.from(sig ?? "", "base64url");
  if (given.length !== expected.length || !timingSafeEqual(given, expected)) return null;
  try {
    const payload = JSON.parse(Buffer.from(body, "base64url").toString("utf8"));
    return payload.exp * 1000 > now ? payload : null;
  } catch {
    return null;
  }
}

export function hashPassword(password) {
  const salt = randomBytes(16);
  const hash = scryptSync(String(password), salt, 32);
  return `scrypt:${b64url(salt)}:${b64url(hash)}`;
}

export function verifyPassword(password, stored) {
  if (typeof stored !== "string" || !stored.startsWith("scrypt:")) return false;
  const [, salt, hash] = stored.split(":");
  const expected = Buffer.from(hash, "base64url");
  const actual = scryptSync(String(password ?? ""), Buffer.from(salt, "base64url"), expected.length);
  return timingSafeEqual(actual, expected);
}

/** Constant-time comparison of two strings (admin password from env). */
export function safeEqual(a, b) {
  const x = Buffer.from(String(a ?? ""));
  const y = Buffer.from(String(b ?? ""));
  return x.length === y.length && timingSafeEqual(x, y);
}

/**
 * Fixed-window attempt counter (per key, e.g. IP or ΑΜ). In memory: on
 * Netlify it is per function instance, which still slows down guessing.
 */
export function createRateLimiter({ limit, windowMs }) {
  const hits = new Map();
  return {
    /** true if this attempt is allowed */
    hit(key, now = Date.now()) {
      const h = hits.get(key);
      if (!h || now - h.start >= windowMs) {
        hits.set(key, { start: now, count: 1 });
        return true;
      }
      h.count++;
      return h.count <= limit;
    },
    reset(key) {
      hits.delete(key);
    },
  };
}
