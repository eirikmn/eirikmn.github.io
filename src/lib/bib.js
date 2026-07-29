import fs from "fs";
import bibtexParse from "bibtex-parse-js";

export function loadBib(path) {
  const bib = fs.readFileSync(path, "utf-8");
  const parsed = bibtexParse.toJSON(bib);

  return parsed.map(entry => {
    const fields = entry.entryTags;

    return {
      key: entry.citationKey,
      entryType: entry.entryType, // @article, @phdthesis, etc.

      title: fields.title,
      authors: fields.author,
      year: fields.year,
      journal: fields.journal || fields.booktitle,

      doi: fields.doi,
      url: fields.url,
      pdf: fields.pdf,

      abstract: fields.abstract,

      volume: fields.volume,
      number: fields.number,
      pages: fields.pages,

      // 🔥 IMPORTANT: normalize your custom type
      type: fields.type?.toLowerCase()?.trim() ?? null,
    };
  });
}