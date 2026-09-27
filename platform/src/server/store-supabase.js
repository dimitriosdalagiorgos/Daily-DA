// Supabase storage — step 4 (see SPEC «Υποδομή»). Same interface as
// createMemoryStore() in store.js. Not implemented yet: the Netlify
// Function answers 503 until SUPABASE_URL is configured, and this makes a
// misconfiguration fail loudly instead of losing data.

export function createSupabaseStore() {
  throw new Error("Η αποθήκευση Supabase δεν έχει υλοποιηθεί ακόμα (βήμα 4).");
}
