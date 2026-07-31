/* Shared slug helper, so anchor ids generated for headings
   and the ids referenced by SectionNav can never disagree. */

export function slugify(value = "") {
  return String(value)
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-|-$/g, "");
}
