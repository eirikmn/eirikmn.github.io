# Originals

Nothing here is published. Astro only copies `public/` into the
build, so these files stay in the repository without being
served or counted against page weight.

## unused/

Images that no page referenced. They cost visitors nothing —
a browser only fetches what a page asks for — but they were
being copied into every deploy.

## replaced/

Full-resolution originals of files still in use, kept at the
size they were exported or shot at.

- `profile-original.jpg` — 3024x4032, 3.1 MB. `public/profile.jpg`
  is now a resized copy; the homepage displays it a few hundred
  pixels wide.
- `unsyncboth.svg`, `syncboth.svg`, `adolphi.svg` — vector exports
  carrying every plotted data point, 1.3-2.9 MB each. The
  agemodeling page now uses the PNG exports already sitting
  beside them, at roughly a tenth the size. Restore these if a
  figure ever needs to be zoomed or re-edited.
