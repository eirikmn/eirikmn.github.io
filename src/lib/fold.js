/* =========================================================
   SEARCH FOLDING

   Reduces a string to a form where "sorbye" matches "Sørbye"
   and "1539-1556" matches "1539–1556".

   Used in two places that must agree: the haystack baked into
   each publication at build time (Publication-list.astro) and
   the query typed into the box (publications.astro). If the
   two ever folded differently the search would silently miss,
   so they import the same function rather than each keeping a
   copy.
========================================================= */

/* NFD decomposition handles most accents - ö becomes o + combining
   diaeresis, which the diacritic strip then removes. It does NOT handle
   letters that are their own codepoint rather than base + mark:
   ø, æ, đ, ð and ł have no decomposition, so NFD leaves them untouched
   and they have to be mapped by hand.

   This matters here more than it might elsewhere: ø appears in Sørbye
   and Sørensen, two of the most frequent co-authors on the list, so
   relying on NFD alone would fail on the names most likely to be typed
   without their accents. */
const SINGLETONS = [
  [/ø/g, "o"], [/Ø/g, "O"],
  [/æ/g, "ae"], [/Æ/g, "AE"],
  [/đ/g, "d"], [/Đ/g, "D"],
  [/ð/g, "d"], [/Ð/g, "D"],
  [/ł/g, "l"], [/Ł/g, "L"],
  [/ß/g, "ss"],
  [/œ/g, "oe"], [/Œ/g, "OE"]
];

export function fold(value) {
  if (value == null) return "";

  let text = String(value);

  for (const [re, to] of SINGLETONS) text = text.replace(re, to);

  return text
    .normalize("NFD")
    /* strip combining marks left by the decomposition */
    .replace(/\p{Diacritic}/gu, "")
    /* en dash, em dash and minus all fold to a plain hyphen, so a page
       range typed as "1539-1556" finds "1539–1556" */
    .replace(/[‒–—―−]/g, "-")
    /* curly quotes and apostrophes, which get pasted in from elsewhere */
    .replace(/[‘’‛]/g, "'")
    .replace(/[“”]/g, '"')
    .toLowerCase()
    .replace(/\s+/g, " ")
    .trim();
}
