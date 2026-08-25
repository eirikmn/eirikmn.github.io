from PIL import Image, ImageDraw
import struct, io, os

G0 = (0x2b, 0x3a, 0x72)   # gradient start
G1 = (0x1b, 0x24, 0x50)   # gradient end
RIM = (0x6d, 0x7f, 0xd6)
INK = (255, 255, 255)

def gradient(size):
    """Linear gradient along the (1,1) diagonal, matching the SVG's
    x1=0 y1=0 -> x2=1 y2=1 objectBoundingBox vector."""
    im = Image.new("RGB", (size, size))
    px = im.load()
    denom = max(1, 2 * (size - 1))
    for y in range(size):
        for x in range(size):
            t = (x + y) / denom
            px[x, y] = tuple(round(a + (b - a) * t) for a, b in zip(G0, G1))
    return im

def poly_round(draw, pts, width, fill, scale):
    """Polyline with round caps and joins: PIL has no round cap, so draw
    the segments then stamp a disc at every vertex."""
    w = width * scale
    draw.line([(x * scale, y * scale) for x, y in pts], fill=fill, width=int(round(w)))
    r = w / 2
    for x, y in pts:
        cx, cy = x * scale, y * scale
        draw.ellipse([cx - r, cy - r, cx + r, cy + r], fill=fill)

MN_A = [(17, 46), (17, 19), (24.5, 32.5), (32, 19), (32, 46)]
MN_B = [(32, 19), (47, 46), (47, 19)]

def render(size, shape, mn_scale=1.0, ss=8):
    """shape: 'disc' (transparent corners + rim) or 'square' (full bleed)."""
    S = size * ss
    base = gradient(S).convert("RGBA")

    if shape == "disc":
        sc = S / 64.0

        def disc_mask(r):
            m = Image.new("L", (S, S), 0)
            ImageDraw.Draw(m).ellipse([(32 - r) * sc, (32 - r) * sc,
                                       (32 + r) * sc, (32 + r) * sc], fill=255)
            return m

        # Build the rim as a true annulus rather than an outlined ellipse:
        # a solid rim-coloured disc at r=31 with the gradient laid back over
        # it at r=29.7. PIL's ellipse(width=) strokes inward from the bounding
        # box and lands heavier than the SVG's centred 1.6 stroke.
        rim = Image.new("RGBA", (S, S), RIM + (255,))
        rim.putalpha(disc_mask(31))
        inner = base.copy()
        inner.putalpha(disc_mask(29.7))
        rim.alpha_composite(inner)
        base = rim

    scale = S / 64.0

    d = ImageDraw.Draw(base)

    def tr(pts):
        return [(32 + (x - 32) * mn_scale, 32 + (y - 32) * mn_scale) for x, y in pts]

    for pts in (MN_A, MN_B):
        poly_round(d, tr(pts), 6.5, INK + (255,), scale)

    out = base.resize((size, size), Image.LANCZOS)
    return out if shape == "disc" else out.convert("RGB")

os.makedirs("/tmp/mk/out", exist_ok=True)
for s in (16, 32, 48):
    render(s, "disc", ss=16 if s <= 32 else 8).save(f"/tmp/mk/out/disc-{s}.png", optimize=True)
for s, name in ((180, "apple-touch-icon.png"), (192, "icon-192.png"), (512, "icon-512.png")):
    render(s, "square", mn_scale=1.13, ss=4).save(f"/tmp/mk/out/{name}", optimize=True)

# multi-frame .ico with PNG-encoded frames
sizes = [16, 32, 48]
blobs = []
for s in sizes:
    buf = io.BytesIO()
    Image.open(f"/tmp/mk/out/disc-{s}.png").convert("RGBA").save(buf, format="PNG", optimize=True)
    blobs.append(buf.getvalue())
head = struct.pack("<HHH", 0, 1, len(sizes))
off = 6 + 16 * len(sizes)
entries = b""
data = b""
for s, blob in zip(sizes, blobs):
    entries += struct.pack("<BBBBHHII", s, s, 0, 0, 1, 32, len(blob), off)
    off += len(blob)
    data += blob
open("/tmp/mk/out/favicon.ico", "wb").write(head + entries + data)

im = Image.open("/tmp/mk/out/favicon.ico")
im.load()
print("ico ok, frames:", sorted(im.info["sizes"]))
for f in sorted(os.listdir("/tmp/mk/out")):
    print(f, os.path.getsize("/tmp/mk/out/" + f))
