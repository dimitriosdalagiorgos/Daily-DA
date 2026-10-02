// HTML → PDF (A4) with Chromium, for docs/build.py.
//   node docs/pdf.mjs in.html out.pdf
// Needs Playwright (npm i playwright; npx playwright install chromium), or
// PLAYWRIGHT=<path to playwright/index.mjs>.
import { pathToFileURL } from "node:url";

const [input, output] = process.argv.slice(2);
const { chromium } = await import(process.env.PLAYWRIGHT ?? "playwright");
const browser = await chromium.launch();
const page = await browser.newPage();
await page.goto(pathToFileURL(input).href);
await page.pdf({
  path: output, format: "A4", printBackground: true,
  margin: { top: "20mm", bottom: "20mm", left: "18mm", right: "18mm" },
  displayHeaderFooter: true, headerTemplate: "<span></span>",
  footerTemplate: '<div style="font-size:8pt;width:100%;text-align:center;color:#666"><span class="pageNumber"></span> / <span class="totalPages"></span></div>',
});
await browser.close();
