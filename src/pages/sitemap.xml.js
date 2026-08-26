/* =========================================================
   SITEMAP

   Written as an endpoint rather than pulling in
   @astrojs/sitemap, to avoid another dependency for eleven
   URLs. Astro renders this to /sitemap.xml at build time.

   Routes are listed explicitly so that drafts and retired
   pages (research_OLD, math, 404) stay out of the index —
   an automatic crawl would happily submit all of them.
   Add new pages here.

   The one exception is the per-publication pages: those are
   generated from papers.bib, so listing them by hand would
   mean a new paper silently missing from the sitemap. They
   are read from the same file the pages themselves are built
   from, which is the only way the two cannot drift.
========================================================= */

import { loadBib } from "../lib/bib.js";

const PAGES = [
  { path: "/",                       changefreq: "monthly", priority: "1.0" },
  { path: "/research",               changefreq: "monthly", priority: "0.9" },
  { path: "/research/longmemory",    changefreq: "yearly",  priority: "0.8" },
  { path: "/research/agemodeling",   changefreq: "yearly",  priority: "0.8" },
  { path: "/research/earlywarning",  changefreq: "yearly",  priority: "0.8" },
  { path: "/publications",           changefreq: "monthly", priority: "0.9" },
  { path: "/software",               changefreq: "monthly", priority: "0.8" },
  { path: "/cv",                     changefreq: "monthly", priority: "0.8" },
  { path: "/contact",                changefreq: "yearly",  priority: "0.5" }
];

export async function GET({ site }) {
  const base = site ?? new URL("https://eirikmyrvollnilsen.com");
  const lastmod = new Date().toISOString().slice(0, 10);

  /* Newest first, matching the listing page. Priority sits just
     under /publications itself: these are the leaves. */
  const papers = loadBib("./src/data/papers.bib")
    .sort((a, b) => Number(b.year) - Number(a.year))
    .map((p) => ({
      path: `/publications/${p.key}`,
      changefreq: "yearly",
      priority: "0.7"
    }));

  const urls = [...PAGES, ...papers].map(
    ({ path, changefreq, priority }) => `  <url>
    <loc>${new URL(path, base).href}</loc>
    <lastmod>${lastmod}</lastmod>
    <changefreq>${changefreq}</changefreq>
    <priority>${priority}</priority>
  </url>`
  ).join("\n");

  const body = `<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
${urls}
</urlset>
`;

  return new Response(body, {
    headers: { "Content-Type": "application/xml; charset=utf-8" }
  });
}
