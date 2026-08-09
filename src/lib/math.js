/* =========================================================
   INLINE MATHS IN BIBLIOGRAPHIC TEXT

   Titles and abstracts occasionally carry notation - "O(n)
   computational cost" is meaningless set in body text with a
   literal caret for the exponent. Anything between single
   dollars is handed to KaTeX; everything else is escaped as
   ordinary text, because the result is injected with set:html.

   Shared by Publication.astro (the front page cards) and
   Publication-list.astro (the publications page). It lived in
   the latter alone at first, which is why the same paper
   rendered correctly on /publications and showed a raw "$O(n)$"
   on the home page. One implementation, one behaviour.

   Note the interaction with cleanTex in references.js: that
   strips braces and rewrites "--", and skips $...$ spans for
   exactly this reason. Maths reaches here with its delimiters
   and its braces intact.
========================================================= */

import katex from "katex";

const escapeHtml = (s) =>
  String(s)
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;");

export function withMath(value) {
  if (value == null) return null;

  return String(value)
    .split(/(\$[^$]*\$)/)
    .map((part) => {
      if (part.startsWith("$") && part.endsWith("$") && part.length > 1) {
        /* inline, never displayMode: this is a run of prose, not an
           equation block. A malformed expression renders in KaTeX's
           error style rather than throwing, so one bad abstract cannot
           take the whole build down. */
        return katex.renderToString(part.slice(1, -1), {
          throwOnError: false,
          displayMode: false
        });
      }
      return escapeHtml(part);
    })
    .join("");
}

/* The same text with the maths flattened to plain characters, for search
   haystacks and alt text - "$O(n)$" becomes "O(n)", which is what a
   reader would type. */
export function stripMath(value) {
  return value == null ? "" : String(value).replace(/\$/g, "");
}
