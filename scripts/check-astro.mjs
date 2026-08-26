/* transform() alone is not enough: it happily emits JS that esbuild
   then refuses to parse. Emit, then actually parse the output. */
import { transform } from '../node_modules/@astrojs/compiler/dist/node/index.js';
import { readFileSync, writeFileSync, readdirSync, statSync } from 'fs';
import { execFileSync } from 'child_process';
import { join } from 'path';

function walk(dir, out = []) {
  for (const e of readdirSync(dir)) {
    const p = join(dir, e);
    if (statSync(p).isDirectory()) walk(p, out);
    else if (p.endsWith('.astro')) out.push(p);
  }
  return out;
}

let bad = 0;
for (const f of walk('src').sort()) {
  const r = await transform(readFileSync(f, 'utf8'), { filename: f, sourcemap: false });
  writeFileSync('/tmp/_chk.mjs', r.code);
  try {
    execFileSync(process.execPath, ['--input-type=module', '--check'],
                 { input: r.code, stdio: ['pipe', 'pipe', 'pipe'] });
    console.log('OK    ' + f);
  } catch (e) {
    bad++;
    const msg = (e.stderr || '').toString().split('\n').filter(Boolean).slice(0, 4).join('\n      ');
    console.log('FAIL  ' + f + '\n      ' + msg);
  }
}
console.log(bad === 0 ? '\nAll .astro files emit parseable JS.' : `\n${bad} file(s) would break the build.`);
