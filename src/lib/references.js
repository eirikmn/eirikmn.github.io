/* =========================================================
   CITATIONS FOR THE RESEARCH PAGES

   Reads src/data/references.bib and turns citation keys into
   inline citations and a reference list.

   The point of the whole thing is that the reference list is
   DERIVED from what a page actually cites, rather than kept
   alongside it by hand. A hand-maintained list is a parallel
   copy of the inline citations, and parallel copies drift:
   you add a citation and forget the list, or delete a
   paragraph and leave an orphan behind. Here neither is
   possible.

   Usage in a page's frontmatter:

     import { createCitations } from "../../lib/references.js";
     const { cite, bibliography } = createCitations();

   then in the text, write [[hurst1951]], and put

     { type: "html", value: bibliography() }

   at the end. `cite` is applied by the page's text renderer;
   see expandCitations below.

   Each page calls createCitations() to get its own recorder,
   so pages cannot leak citations into each other's lists.
========================================================= */

import fs from "node:fs";
import bibtexParse from "bibtex-parse-js";

const BIB_PATH = "./src/data/references.bib";

/* -------------------------------------------------------
   LOAD

   Parsed once per build. The @comment entries bibtex-parse-js
   emits for stray text are dropped.
------------------------------------------------------- */

function loadReferences(path = BIB_PATH) {
  const raw = fs.readFileSync(path, "utf-8");

  const entries = new Map();

  for (const entry of bibtexParse.toJSON(raw)) {
    if (!entry.citationKey) continue;

    const f = entry.entryTags ?? {};

    entries.set(entry.citationKey.toLowerCase(), {
      key: entry.citationKey,
      type: entry.entryType,
      author: clean(f.author),
      title: clean(f.title),
      year: clean(f.year),
      journal: clean(f.journal ?? f.booktitle),
      publisher: clean(f.publisher),
      school: clean(f.school),
      howpublished: clean(f.howpublished),
      volume: clean(f.volume),
      number: clean(f.number),
      pages: clean(f.pages),
      doi: clean(f.doi),
      url: clean(f.url)
    });
  }

  return entries;
}

/* -------------------------------------------------------
   BIBTEX CLEANUP

   Strip the braces that protect capitalisation ({GISTEMP}),
   convert the handful of LaTeX escapes that appear in Nordic
   and Spanish names, and turn `--` into a real en dash.
------------------------------------------------------- */

const LATEX = [
  [/\{\\aa\}/g, "å"], [/\{\\AA\}/g, "Å"],
  [/\{\\o\}/g, "ø"],  [/\{\\O\}/g, "Ø"],
  [/\{\\ae\}/g, "æ"], [/\{\\AE\}/g, "Æ"],
  [/\{\\'e\}/g, "é"], [/\{\\'a\}/g, "á"], [/\{\\'o\}/g, "ó"],
  [/\{\\"o\}/g, "ö"], [/\{\\"a\}/g, "ä"], [/\{\\"u\}/g, "ü"],
  [/\\&/g, "&"]
];

function clean(value) {
  if (value == null) return null;

  let text = String(value);
  for (const [re, to] of LATEX) text = text.replace(re, to);

  return text
    .replace(/[{}]/g, "")
    .replace(/--/g, "–")
    .replace(/\s+/g, " ")
    .trim();
}

/* -------------------------------------------------------
   AUTHOR FORMATTING

   BibTeX gives "Surname, Given and Surname, Given". Produce
   "Surname, G. I." for the reference list, and a short
   "Surname", "A and B" or "A et al." form for inline use.
------------------------------------------------------- */

function splitAuthors(author) {
  if (!author) return [];
  return author.split(/\s+and\s+/i).map((a) => a.trim()).filter(Boolean);
}

function surnameOf(name) {
  /* "{GISTEMP Team}" survives cleanup as "GISTEMP Team" and
     has no comma — treat the whole string as the surname. */
  return name.includes(",") ? name.split(",")[0].trim() : name.trim();
}

function initialsOf(name) {
  if (!name.includes(",")) return "";

  const given = name.split(",").slice(1).join(",").trim();
  if (!given) return "";

  return given
    .split(/[\s.]+/)
    .filter(Boolean)
    .map((part) =>
      /* keep hyphenated given names as H.-B. */
      part.split("-").map((p) => p[0].toUpperCase() + ".").join("-")
    )
    .join(" ");
}

function formatAuthorsFull(author) {
  const names = splitAuthors(author).map((n) => {
    const initials = initialsOf(n);
    return initials ? `${surnameOf(n)}, ${initials}` : surnameOf(n);
  });

  if (names.length === 0) return "";
  if (names.length === 1) return names[0];

  return names.slice(0, -1).join(", ") + " and " + names[names.length - 1];
}

function formatAuthorsInline(author) {
  const names = splitAuthors(author).map(surnameOf);

  if (names.length === 0) return "";
  if (names.length === 1) return names[0];
  if (names.length === 2) return `${names[0]} and ${names[1]}`;

  return `${names[0]} et al.`;
}

/* -------------------------------------------------------
   RENDERING
------------------------------------------------------- */

function escapeHtml(text) {
  return String(text)
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;");
}

function formatEntry(e) {
  const bits = [];

  const authors = formatAuthorsFull(e.author);
  bits.push(authors ? `${escapeHtml(authors)} (${e.year}).` : `(${e.year}).`);

  /* Books, theses and datasets take italics on the title;
     articles do not, since the italics belong to the journal
     name instead. */
  const standalone =
    e.type === "book" ||
    e.type === "misc" ||
    e.type === "phdthesis" ||
    e.type === "mastersthesis";

  bits.push(
    standalone
      ? `<em>${escapeHtml(e.title)}</em>.`
      : `${escapeHtml(e.title)}.`
  );

  /* A thesis's institution is stored in `journal` in
     papers.bib, but it is not a journal — label it. */
  if (e.type === "phdthesis" || e.type === "mastersthesis") {
    const degree = e.type === "phdthesis" ? "PhD thesis" : "Master's thesis";
    const where = e.school ?? e.journal;
    bits.push(where ? `${degree}, ${escapeHtml(where)}.` : `${degree}.`);
  } else if (e.journal) {
    let where = `<em>${escapeHtml(e.journal)}</em>`;
    if (e.volume) where += `, ${escapeHtml(e.volume)}`;
    if (e.number) where += `(${escapeHtml(e.number)})`;
    if (e.pages) where += `, ${escapeHtml(e.pages)}`;
    bits.push(where + ".");
  } else if (e.publisher) {
    bits.push(escapeHtml(e.publisher) + ".");
  } else if (e.howpublished) {
    bits.push(escapeHtml(e.howpublished) + ".");
  }

  if (e.doi) {
    const href = `https://doi.org/${e.doi}`;
    bits.push(
      `<a href="${href}" target="_blank" rel="noopener noreferrer">doi:${escapeHtml(e.doi)}</a>`
    );
  } else if (e.url) {
    bits.push(
      `<a href="${e.url}" target="_blank" rel="noopener noreferrer">${escapeHtml(e.url.replace(/^https?:\/\//, ""))}</a>`
    );
  }

  return bits.join(" ");
}

/* -------------------------------------------------------
   PUBLIC API
------------------------------------------------------- */

/* Load any .bib as an array of normalised entries. Exported
   so the CV can render its publication list with the same
   formatter the research pages use for their references —
   one implementation, so the two can never diverge in
   author style, page ranges or how a thesis is labelled. */
export function loadBibliography(path) {
  return [...loadReferences(path).values()];
}

/* One entry as a formatted reference.

   `highlight` bolds a surname wherever it appears in the
   author list — the convention on a CV, where the reader is
   scanning for your position among the authors. It runs on
   the formatted output rather than the raw BibTeX so it
   matches "Myrvoll-Nilsen, E." as rendered, initials and
   all, and cannot accidentally match inside a title. */
export function formatReference(entry, options = {}) {
  const html = formatEntry(entry);
  const { highlight } = options;

  if (!highlight) return html;

  const escaped = String(highlight).replace(/[.*+?^${}()|[\]\\]/g, "\\$&");

  /* Surname, then one or more initials, each optionally
     hyphenated: "Myrvoll-Nilsen, E.", "Fredriksen, H.-B.",
     "Rypdal, M. W."

     The match deliberately stops at the last initial rather
     than consuming the space after it. Swallowing that space
     and re-adding it around the <strong> pushed the following
     separator out to "E. , Riechers". */
  const initial = "[A-ZÅØÆ]\\.(?:-[A-ZÅØÆ]\\.)?";
  const name = new RegExp(
    `${escaped},\\s${initial}(?:\\s${initial})*`,
    "g"
  );

  return html.replace(name, (match) => `<strong>${match}</strong>`);
}

/* =========================================================
   SOURCES

   A page can cite from more than one bibliography, and they
   do not all belong in the reference list.

   references.bib holds other people's work: cited in the
   text, listed under References, anchored at #ref-KEY.

   papers.bib holds your own: cited in the text too, but
   already shown as publication cards further down the page.
   Repeating them under References would list the same work
   twice, so `listed: false` keeps them out of it and their
   citations anchor at #pub-KEY instead — the card.
========================================================= */

const DEFAULT_SOURCES = [
  { path: BIB_PATH, anchor: "ref", listed: true }
];

export function createCitations(options = {}) {
  const sources = options.sources ?? DEFAULT_SOURCES;

  /* Later sources win on a key collision, which is only
     reachable if the same key appears in two .bib files. */
  const entries = options.entries ?? new Map();

  if (!options.entries) {
    for (const source of sources) {
      for (const [key, entry] of loadReferences(source.path)) {
        entries.set(key, {
          ...entry,
          anchor: source.anchor ?? "ref",
          listed: source.listed !== false
        });
      }
    }
  }

  /* insertion-ordered set of the keys this page has used */
  const used = new Set();

  function lookup(key) {
    const entry = entries.get(String(key).toLowerCase().trim());

    if (!entry) {
      /* Fail the build rather than render nothing. A silently
         missing citation is the exact failure this system
         exists to prevent. */
      throw new Error(
        `Unknown citation key "${key}". ` +
          `Add it to one of ${sources.map((s) => s.path).join(" or ")}, ` +
          `or fix the spelling. ` +
          `Known keys: ${[...entries.keys()].sort().join(", ")}`
      );
    }

    return entry;
  }

  /* Wrap a citation in a link down to its entry in the
     reference list. bibliography() gives each <li> the
     matching id, so the two halves cannot disagree.

     title= rather than aria-label: the link text already
     reads "(Hurst, 1951)", which is a fine accessible name.
     The title just explains where it goes on hover. */
  function link(entry, label) {
    const anchor = entry.anchor ?? "ref";
    const where = anchor === "pub" ? "publication" : "reference";

    return (
      `<a class="cite-link" href="#${anchor}-${entry.key}"` +
      ` title="Jump to ${where}">${label}</a>`
    );
  }

  /* record and render one key in the "Author, Year" form */
  function bare(key) {
    const e = lookup(key);
    used.add(e.key.toLowerCase());
    return link(e, `${formatAuthorsInline(e.author)}, ${e.year}`);
  }

  /* =========================
     THE THREE FORMS

     Mirroring natbib, because the distinction is grammatical
     rather than decorative:

       cite   \citep    (Hurst, 1951)
                        an aside; the sentence stands without it

       citet  \citet    Hurst (1951)
                        the author is a noun in the sentence:
                        "Granger (1980) showed that…"

       citea  \citealp  Hurst, 1951
                        no brackets of its own, for a citation
                        inside brackets that are already open:
                        "(INLA, Rue et al., 2009)"

     Each accepts several keys, which are then grouped into
     ONE bracket rather than several — \citep{a,b} giving
     "(A, 1999; B, 2000)", not "(A, 1999)(B, 2000)".
  ========================= */

  const asList = (keys) =>
    (Array.isArray(keys) ? keys : String(keys).split(/[;,]/))
      .map((k) => k.trim())
      .filter(Boolean);

  function cite(keys) {
    return `(${asList(keys).map(bare).join("; ")})`;
  }

  function citea(keys) {
    return asList(keys).map(bare).join("; ");
  }

  function citet(keys) {
    return asList(keys)
      .map((key) => {
        const e = lookup(key);
        used.add(e.key.toLowerCase());
        return link(e, `${formatAuthorsInline(e.author)} (${e.year})`);
      })
      .join("; ");
  }

  /* Expand markers in a block of text:

       [[hurst1951]]            (Hurst, 1951)
       [[t:granger1980]]        Granger (1980)
       [[b:rue2009]]            Rue et al., 2009
       [[gistemp2018; lenssen2019]]
                                (GISTEMP Team, 2018; Lenssen et al., 2019)

     The prefix applies to every key in the marker. */
  function expandCitations(text = "") {
    return String(text).replace(
      /\[\[\s*(?:([tb]):)?\s*([^\]]+?)\s*\]\]/g,
      (_, mode, keys) => {
        if (mode === "t") return citet(keys);
        if (mode === "b") return citea(keys);
        return cite(keys);
      }
    );
  }

  /* Only what this page cited, alphabetical by surname then
     year — so the list cannot contain an orphan, and cannot
     omit something the text refers to. */
  function bibliography() {
    const list = [...used]
      .map((k) => entries.get(k))
      /* own papers are shown as cards elsewhere on the page */
      .filter((e) => e.listed !== false)
      .sort((a, b) => {
        const byName = formatAuthorsFull(a.author)
          .localeCompare(formatAuthorsFull(b.author), "en");
        return byName !== 0 ? byName : String(a.year).localeCompare(String(b.year));
      });

    if (list.length === 0) return "";

    return (
      '<ul class="reflist">' +
      list.map((e) => `<li id="ref-${e.key}">${formatEntry(e)}</li>`).join("") +
      "</ul>"
    );
  }

  function usedKeys() {
    return [...used];
  }

  return { cite, citet, expandCitations, bibliography, usedKeys, entries };
}
