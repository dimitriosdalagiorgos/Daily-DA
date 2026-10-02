// Separate, unguessable addresses for the teachers' and the admin's pages,
// so that nobody reaches them by navigating from the parents' page. They
// are set outside the repository (TEACHER_PATH, ADMIN_PATH: Netlify
// environment variables, or /etc/omiloi.env), so publishing the code does
// not reveal them. The parents' page is the site's root.
//
// This only hides the pages; every page still needs its own password.

const PATH = /^[A-Za-z0-9_-]{8,64}$/;
const RESERVED = new Set(["api", "css", "js", "lib", "docs", "templates", "vendor"]);

/**
 * @param {{TEACHER_PATH?: string, ADMIN_PATH?: string}} env
 * @returns {{teacher: string, admin: string}} paths like "/e-7kq3m9xa/"
 */
export function rolePaths(env) {
  const teacher = String(env.TEACHER_PATH ?? "").trim().replace(/^\/+|\/+$/g, "");
  const admin = String(env.ADMIN_PATH ?? "").trim().replace(/^\/+|\/+$/g, "");
  const problems = [];
  for (const [name, value] of [["TEACHER_PATH", teacher], ["ADMIN_PATH", admin]]) {
    if (!value) problems.push(`Ορίστε ${name}.`);
    else if (!PATH.test(value) || RESERVED.has(value.toLowerCase())) problems.push(`${name}: 8–64 λατινικοί χαρακτήρες, ψηφία, - ή _ (π.χ. e-7kq3m9xa).`);
  }
  if (!problems.length && teacher.toLowerCase() === admin.toLowerCase()) problems.push("Τα TEACHER_PATH και ADMIN_PATH πρέπει να διαφέρουν.");
  if (problems.length) throw new Error(problems.join(" "));
  return { teacher: `/${teacher}/`, admin: `/${admin}/` };
}

/** The page files behind the hidden addresses (not served under their own names). */
export const ROLE_PAGES = { teacher: "teacher.html", admin: "admin.html" };
