/* =========================================================
   BIBLIOGRAPHY DOWNLOAD

   Renders to /publications.bib at build time, following the
   same endpoint pattern as sitemap.xml.js.

   Generated rather than serving src/data/papers.bib directly:
   that file carries site-internal bookkeeping - `pdf` paths
   and `themes` tags - which would land in a reader's own
   bibliography and mean nothing there. loadBib already
   reconstructs a clean record per entry for the Cite button,
   so this is the same text, concatenated.

   Newest first, matching the order of the page itself.
========================================================= */

import { loadBib } from "../lib/bib.js";

export async function GET() {
  const papers = loadBib("./src/data/papers.bib")
    .sort((a, b) => Number(b.year) - Number(a.year));

  const header = [
    "% Publications - Eirik Myrvoll-Nilsen",
    "% https://eirikmyrvollnilsen.com/publications",
    `% ${papers.length} entries, generated ${new Date().toISOString().slice(0, 10)}`,
    ""
  ].join("\n");

  const body = header + papers.map((p) => p.bibtex).join("\n\n") + "\n";

  return new Response(body, {
    headers: {
      "Content-Type": "application/x-bibtex; charset=utf-8",
      "Content-Disposition": 'inline; filename="myrvoll-nilsen-publications.bib"'
    }
  });
}
