/* =========================================================
   CITABLE ENTRIES

   Resolves a BibTeX key into the two things a "please cite
   this" block needs: a formatted reference to read, and the
   raw record to copy.

   Both come from the .bib files, which is the point. The
   software page used to carry hand-written HTML for each
   citation plus a `bibtex` field literally set to
   "@article{...}" - so the references were styled unlike the
   rest of the site and the copy buttons had nothing real to
   copy. Every work cited there already exists in papers.bib
   or references.bib, so nothing needs to be written twice.

   Two loaders over the same files, deliberately:

     references.js  formatReference() - the same renderer the
                    research pages use for their reference
                    lists, so the styling matches by
                    construction rather than by imitation.

     bib.js         loadBib() - reconstructs a clean BibTeX
                    record per entry, already used by the Cite
                    buttons on /publications.
========================================================= */

import { loadBibliography, formatReference } from "./references.js";
import { loadBib } from "./bib.js";

const SOURCES = [
  "./src/data/papers.bib",
  "./src/data/references.bib"
];

/* The author whose name is emboldened in a reference list. */
const HIGHLIGHT = "Myrvoll-Nilsen";

let cache = null;

function build() {
  const out = new Map();

  const slot = (key) => {
    if (!out.has(key)) out.set(key, {});
    return out.get(key);
  };

  for (const path of SOURCES) {
    /* formatted reference. loadBibliography returns an ARRAY of entries,
       each carrying its own `key` - not a Map keyed by it. */
    for (const entry of loadBibliography(path)) {
      slot(entry.key).html = formatReference(entry, { highlight: HIGHLIGHT });
    }
    /* raw record */
    for (const entry of loadBib(path)) {
      slot(entry.key).bibtex = entry.bibtex;
    }
  }

  return out;
}

/* Built once per build rather than per component instance - the software
   page renders three packages, each with two citation blocks. */
export function citable() {
  if (!cache) cache = build();
  return cache;
}

/* Resolve a list of entries to render.
 *
 * Each item is either a bare key, or { key, note } where the note is the
 * short label the software page puts in front of some citations - "INLA",
 * "Nested AR(1) model" - explaining why that work is being cited.
 *
 * An unknown key is dropped rather than rendered as a hole, and reported
 * so a typo surfaces in the build log instead of silently vanishing. */
export function resolveCitations(items = [], context = "") {
  const all = citable();

  return items
    .map((item) => {
      const { key, note } = typeof item === "string" ? { key: item } : item;
      const found = all.get(key);

      if (!found) {
        console.warn(
          `[citable] unknown key "${key}"${context ? ` in ${context}` : ""} - dropped`
        );
        return null;
      }

      return { key, note: note ?? null, html: found.html, bibtex: found.bibtex };
    })
    .filter(Boolean);
}
