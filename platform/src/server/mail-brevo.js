// Sending e-mail through Brevo (EU provider, free tier 300 mails/day).
// https://developers.brevo.com/reference/sendtransacemail
//
//   BREVO_API_KEY   Brevo → SMTP & API → API keys
//   MAIL_FROM       a sender address verified in Brevo → Senders
//   MAIL_FROM_NAME  optional display name (e.g. the school's name)

export function createBrevoMailer({ apiKey, from, fromName, fetch: fetchImpl = globalThis.fetch }) {
  if (!apiKey || !from) throw new Error("Λείπουν BREVO_API_KEY / MAIL_FROM.");
  return async function sendMail({ to, subject, text }) {
    const res = await fetchImpl("https://api.brevo.com/v3/smtp/email", {
      method: "POST",
      headers: { "api-key": apiKey, "content-type": "application/json", accept: "application/json" },
      body: JSON.stringify({
        sender: { email: from, ...(fromName ? { name: fromName } : {}) },
        to: [{ email: to }],
        subject,
        textContent: text,
      }),
    });
    if (!res.ok) throw new Error(`Brevo ${res.status}: ${(await res.text()).slice(0, 300)}`);
  };
}
