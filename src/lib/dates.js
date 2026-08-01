/* =========================================================
   CV DATE FORMATTING

   The JSON files store dates as ISO `YYYY-MM` (or bare
   `YYYY`), which sorts correctly and can't be misread the
   way `03-04` can. That's the right thing to store, but the
   wrong thing to show: a CV is scanned rather than read, and
   `2016-09` costs the reader a lookup that `Sep 2016` does
   not.

   These helpers do the storage → display conversion in one
   place, so every CV component renders ranges identically.

   The important design point is the PASS-THROUGH: anything
   that isn't ISO comes back untouched. Some entries have no
   meaningful month — a four-year reviewing stint is just
   "2021 – 2024" — and teaching is naturally described in
   academic terms ("Spring 2026"). Those stay exactly as
   written in the JSON.
========================================================= */

/* Three letters rather than full month names. The date sits
   in a right-aligned column of secondary information; full
   names are wide enough to wrap on narrow screens and pull
   attention away from the role title, which should be read
   first. */
const MONTHS = [
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
];

/* Spaced en dash. Not a hyphen (too short for a range) and
   not an em dash (too long). Defined once so the components
   can't drift apart on it, which they previously had. */
export const RANGE_DASH = " – ";

/**
 * Format a single stored date for display.
 *
 *   "2016-09"     -> "Sep 2016"
 *   "2021"        -> "2021"          (already display-ready)
 *   "Spring 2024" -> "Spring 2024"   (already display-ready)
 *   null          -> null
 */
export function formatDate(value) {
  if (value == null) return null;

  const text = String(value).trim();
  if (text === "") return null;

  const match = /^(\d{4})-(\d{2})$/.exec(text);
  if (!match) return text;

  const year = match[1];
  const monthIndex = Number(match[2]) - 1;

  /* guard against a typo like "2016-13" silently rendering
     as "undefined 2016" */
  if (monthIndex < 0 || monthIndex > 11) return text;

  return `${MONTHS[monthIndex]} ${year}`;
}

/**
 * Format a start/end pair as a range.
 *
 * A missing `end` means the entry is ongoing. A missing
 * `start` returns just the end rather than a stray leading
 * dash, so a half-filled record degrades quietly.
 *
 *   ("2016-09", "2020-05")  -> "Sep 2016 – May 2020"
 *   ("2023-02", null)       -> "Feb 2023 – Present"
 *   ("2021", "2024")        -> "2021 – 2024"
 *   (null, null)            -> null
 */
export function formatRange(start, end, options = {}) {
  const { ongoing = "Present", dash = RANGE_DASH } = options;

  const from = formatDate(start);
  const to = formatDate(end) ?? (from ? ongoing : null);

  if (!from) return to;
  if (!to) return from;

  return `${from}${dash}${to}`;
}
