import fs from "fs";
import bibtexParse from "bibtex-parse-js";
import { cleanTex } from "./references.js";

export function loadBib(path) {
  const bib = fs.readFileSync(path, "utf-8");
  const parsed = bibtexParse.toJSON(bib);

  return parsed.map(entry => {
    const fields = entry.entryTags;

    /* Shorthand for "this field is shown to a reader, so decode the TeX".
       `--` becomes an en dash and {\"o} becomes ö, matching what the
       research pages already do.

       Deliberately NOT applied to doi, url or pdf: those are machine
       identifiers, and a DOI is allowed to contain a double hyphen that
       must not be rewritten into an en dash. Nor to `bibtex` below, which
       has to stay valid BibTeX. */
    const t = (v) => cleanTex(v) ?? undefined;

    return {
      key: entry.citationKey,
      entryType: entry.entryType, // @article, @phdthesis, etc.

      title: t(fields.title),
      authors: t(fields.author),
      year: fields.year,
      journal: t(fields.journal || fields.booktitle),

      /* Thesis entries carry `school` rather than `journal`.
         publications.astro has always read `paper.school`; until
         now nothing put it here, so it was silently undefined. */
      school: t(fields.school),
      publisher: t(fields.publisher),

      doi: fields.doi,
      url: fields.url,
      pdf: fields.pdf,

      abstract: t(fields.abstract),

      volume: t(fields.volume),
      number: t(fields.number),
      pages: t(fields.pages),

      // 🔥 IMPORTANT: normalize your custom type
      type: fields.type?.toLowerCase()?.trim() ?? null,

      /* Research themes, as a list. Comma-separated in the .bib so an entry
         can belong to more than one area. Entries with no themes field come
         back as [] and simply never match a theme filter - two papers are
         deliberately in that position, having no research page of their own. */
      themes: (fields.themes ?? "")
        .split(",")
        .map((t) => t.trim().toLowerCase())
        .filter(Boolean),

      /* A BibTeX record reconstructed from the parsed entry, for the copy
         button on /publications. Built here rather than kept in a second
         file, so it cannot drift from the entry it cites.

         `pdf` and `themes` are site-internal bookkeeping, and `abstract` is
         long enough to bury the record, so all three are left out of what
         the reader copies. */
      bibtex: (() => {
        const skip = new Set(["pdf", "themes", "abstract"]);
        const body = Object.entries(fields)
          .filter(([k, v]) => !skip.has(k.toLowerCase()) && v)
          .map(([k, v]) => `  ${k} = {${v}}`)
          .join(",\n");
        return `@${entry.entryType}{${entry.citationKey},\n${body}\n}`;
      })(),
    };
  });
}