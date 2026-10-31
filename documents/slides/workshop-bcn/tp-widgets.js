/* Interactive widgets for the transport poverty lecture.
   Self-contained: no network access needed. Data: tp-data.js (final IMPT results, parish level). */
(function () {
  const $ = (s, r) => (r || document).querySelector(s);
  const $$ = (s, r) => Array.from((r || document).querySelectorAll(s));
  const fmt = (x, d) => Number(x).toFixed(d === undefined ? 1 : d);

  /* ---------- 1. Household calculator ---------- */
  function initCalc(root) {
    const presets = {
      A: { income: 2200, housing: 350, transport: 330 },
      B: { income: 1100, housing: 520, transport: 45 },
      C: { income: 800, housing: 280, transport: 25 },
      D: { income: 7000, housing: 1400, transport: 1100 },
    };
    const inc = $('[data-k=income]', root), hou = $('[data-k=housing]', root),
      tra = $('[data-k=transport]', root), med = $('[data-k=median]', root);
    function update() {
      const I = +inc.value, H = +hou.value, T = +tra.value, M = +med.value;
      $('[data-o=income]', root).textContent = I;
      $('[data-o=housing]', root).textContent = H;
      $('[data-o=transport]', root).textContent = T;
      $('[data-o=median]', root).textContent = M;
      const share = (T / I) * 100, ht = ((H + T) / I) * 100;
      const tests = [
        ['t10', share > 10, fmt(share) + '% of income on transport (threshold 10%)'],
        ['t2m', share > 2 * M, fmt(share) + '% vs. twice the median (' + fmt(2 * M) + '%)'],
        ['tht', ht > 45, fmt(ht) + '% of income on housing + transport (threshold 45%)'],
      ];
      tests.forEach(([k, flag, txt]) => {
        const el = $('[data-t=' + k + ']', root);
        el.className = 'verdict ' + (flag ? 'poor' : 'ok');
        $('.v-flag', el).textContent = flag ? 'Transport poor' : 'Not flagged';
        $('.v-txt', el).textContent = txt;
      });
      const n = tests.filter(t => t[1]).length;
      $('[data-o=summary]', root).textContent =
        n === 0 ? 'No rule flags this household.' :
        n === 3 ? 'All three rules flag this household.' :
        n + ' of 3 rules flag this household.';
    }
    $$('input[type=range]', root).forEach(i => i.addEventListener('input', () => {
      $$('.preset', root).forEach(b => b.classList.remove('on'));
      update();
    }));
    $$('.preset', root).forEach(b => b.addEventListener('click', () => {
      const p = presets[b.dataset.p];
      inc.value = p.income; hou.value = p.housing; tra.value = p.transport;
      $$('.preset', root).forEach(x => x.classList.toggle('on', x === b));
      update();
    }));
    update();
  }

  /* ---------- 2. Aggregation toy (compensation) ---------- */
  function initAgg(root) {
    const keys = ['a', 'm', 'f', 's'];
    const presets = { bal: [78, 77, 78, 77], unev: [100, 100, 100, 10], rural: [10, 10, 90, 95] };
    function update() {
      const v = keys.map(k => +$('[data-k=' + k + ']', root).value);
      keys.forEach((k, i) => $('[data-o=' + k + ']', root).textContent = v[i]);
      const ar = v.reduce((a, b) => a + b, 0) / 4;
      const ge = Math.pow(v.reduce((a, b) => a * (b + 1), 1), 1 / 4) - 1;
      const pa = 100 - ar, pg = 100 - ge;
      $('[data-o=arv]', root).textContent = fmt(pa);
      $('[data-o=gev]', root).textContent = fmt(pg);
      $('[data-b=ar]', root).style.width = pa + '%';
      $('[data-b=ge]', root).style.width = pg + '%';
    }
    $$('input[type=range]', root).forEach(i => i.addEventListener('input', update));
    $$('.preset', root).forEach(b => b.addEventListener('click', () => {
      const p = presets[b.dataset.p];
      keys.forEach((k, i) => $('[data-k=' + k + ']', root).value = p[i]);
      update();
    }));
    update();
  }

  /* ---------- 3. Parish comparison / explorer ---------- */
  function initCmp(root) {
    const P = window.TP_PARISHES || [];
    const gate = root.dataset.gate === '1';
    const defs = [
      ['a', 'Accessibility', 'higher = better'],
      ['m', 'Mobility', 'higher = better'],
      ['f', 'Affordability', 'higher = better'],
      ['s', 'Safety', 'higher = safer'],
    ];
    const sels = $$('select', root);
    sels.forEach((s, i) => {
      P.forEach(p => s.add(new Option(p.n, p.n)));
      s.value = root.dataset['p' + (i + 1)] || P[i].n;
      s.addEventListener('change', () => render());
    });
    let revealed = !gate;
    const btn = $('.reveal-btn', root);
    if (btn) btn.addEventListener('click', () => { revealed = !revealed; render(); });
    const rnd = $('.rand-btn', root);
    if (rnd) rnd.addEventListener('click', () => {
      sels.forEach(s => s.value = P[Math.floor(Math.random() * P.length)].n);
      if (gate) revealed = false;
      render();
    });
    function card(p) {
      const dims = defs.map(([k, label, hint]) =>
        '<div class="row"><span class="lab">' + label + '</span>' +
        '<span class="track"><span class="fill dim" style="width:' + p[k] + '%"></span></span>' +
        '<span class="num">' + fmt(p[k], 0) + '</span></div>').join('');
      const comp = [['e', 'Contrasting (entropy)'], ['v', 'Balanced (arithmetic)'], ['g', 'Critical (geometric)']].map(([k, label]) =>
        '<div class="row"><span class="lab">' + label + '</span>' +
        '<span class="track"><span class="fill poor" style="width:' + (revealed ? p[k] : 0) + '%"></span></span>' +
        '<span class="num">' + (revealed ? fmt(p[k]) : '?') + '</span></div>').join('');
      return '<div class="pc"><h4>' + p.n + '</h4>' +
        '<div class="sub">Dimension scores (0–100, higher = better)</div>' + dims +
        '<div class="sub">IMPT (0–100, higher = more transport poverty)</div>' + comp + '</div>';
    }
    function render() {
      const out = $('.cards', root);
      out.innerHTML = sels.map(s => card(P.find(p => p.n === s.value))).join('');
      if (btn) btn.textContent = revealed ? 'Hide the IMPT scores' : 'Reveal the IMPT scores';
    }
    render();
  }

  /* ---------- 4. PCA illustration ---------- */
  function initPca(root) {
    const P = window.TP_PARISHES || [];
    const cv = $('canvas', root), ctx = cv.getContext('2d'), W = cv.width, H = cv.height;
    const mean = a => a.reduce((s, x) => s + x, 0) / a.length;
    const xs = P.map(p => p.a), ys = P.map(p => p.m);
    const mx = mean(xs), my = mean(ys);
    const sd = (a, m) => Math.sqrt(mean(a.map(x => (x - m) * (x - m))));
    const sx = sd(xs, mx), sy = sd(ys, my);
    const Z = P.map(p => [(p.a - mx) / sx, (p.m - my) / sy]);
    const r = mean(Z.map(z => z[0] * z[1]));            // correlation
    const best = 0.5 * Math.atan2(2 * r, 1 - 1) * 180 / Math.PI; // PC1 angle (equal variances)
    const slider = $('[data-k=ang]', root);
    const R = 3.4, HS = 300, sc = Math.min(W, HS) / (2 * R);
    const X = x => W / 2 + x * sc, Y = y => HS / 2 - y * sc;
    function draw() {
      const ang = +slider.value, t = ang * Math.PI / 180, c = Math.cos(t), s = Math.sin(t);
      $('[data-o=ang]', root).textContent = ang + '°';
      const varp = mean(Z.map(z => { const u = z[0] * c + z[1] * s; return u * u; }));
      $('[data-o=share]', root).textContent = Math.round(varp / 2 * 100) + '%';
      ctx.clearRect(0, 0, W, H);
      ctx.strokeStyle = '#ddd'; ctx.lineWidth = 1;
      ctx.beginPath(); ctx.moveTo(0, HS / 2); ctx.lineTo(W, HS / 2); ctx.moveTo(W / 2, 0); ctx.lineTo(W / 2, HS); ctx.stroke();
      ctx.strokeStyle = 'rgba(6,168,210,.35)';
      Z.forEach(z => { const u = z[0] * c + z[1] * s; ctx.beginPath(); ctx.moveTo(X(z[0]), Y(z[1])); ctx.lineTo(X(u * c), Y(u * s)); ctx.stroke(); });
      ctx.fillStyle = '#727678';
      Z.forEach(z => { ctx.beginPath(); ctx.arc(X(z[0]), Y(z[1]), 3.2, 0, 6.2832); ctx.fill(); });
      ctx.strokeStyle = '#E8890C'; ctx.lineWidth = 3;
      ctx.beginPath(); ctx.moveTo(X(-R * c), Y(-R * s)); ctx.lineTo(X(R * c), Y(R * s)); ctx.stroke();
      // strip: the score of every parish on this axis
      const sy = 345, ss = (W - 40) / (2 * R);
      ctx.strokeStyle = '#bbb'; ctx.lineWidth = 1.5; ctx.beginPath(); ctx.moveTo(20, sy); ctx.lineTo(W - 20, sy); ctx.stroke();
      ctx.fillStyle = 'rgba(232,137,12,.55)';
      Z.forEach(z => { const u = z[0] * c + z[1] * s; ctx.beginPath(); ctx.arc(W / 2 + u * ss, sy, 4, 0, 6.2832); ctx.fill(); });
      ctx.fillStyle = '#404040'; ctx.font = '17px Verdana, sans-serif'; ctx.fillText('Scores along the orange axis', 20, sy - 16);
    }
    slider.addEventListener('input', draw);
    $('[data-act=snap]', root).addEventListener('click', () => { slider.value = Math.round(((best % 180) + 180) % 180); draw(); });
    draw();
  }

  /* box style options: ?boxes=a|b|c */
  try { document.documentElement.dataset.boxes = new URLSearchParams(location.search).get('boxes') || 'a'; } catch (e) {}

  /* ---------- 5. Reach within a time threshold (schematic) ---------- */
  function initReach(root) {
    const cv = $('canvas', root), ctx = cv.getContext('2d'), W = cv.width, H = cv.height, cx = W / 2, cy = H / 2;
    let seed = 7; const rnd = () => (seed = (seed * 16807) % 2147483647) / 2147483647;
    const pts = [];
    while (pts.length < 90) { const r = -Math.log(1 - rnd() * 0.999) * 6, a = rnd() * 6.2832; if (r < 20.5) pts.push([r * Math.cos(a), r * Math.sin(a)]); }
    const speed = { walk: 5, bike: 12, pt: 15, car: 30 }, pxkm = 9;
    let mode = 'walk';
    const slider = $('[data-k=time]', root);
    function draw() {
      const t = +slider.value, rkm = speed[mode] * t / 60;
      $('[data-o=time]', root).textContent = t + ' min';
      ctx.clearRect(0, 0, W, H);
      ctx.strokeStyle = '#eee'; ctx.lineWidth = 1;
      for (let x = 0; x <= W; x += 40) { ctx.beginPath(); ctx.moveTo(x, 0); ctx.lineTo(x, H); ctx.stroke(); }
      for (let y = 0; y <= H; y += 40) { ctx.beginPath(); ctx.moveTo(0, y); ctx.lineTo(W, y); ctx.stroke(); }
      ctx.beginPath(); ctx.arc(cx, cy, rkm * pxkm, 0, 6.2832);
      ctx.fillStyle = 'rgba(6,168,210,.16)'; ctx.fill(); ctx.strokeStyle = '#06A8D2'; ctx.lineWidth = 2.5; ctx.stroke();
      let n = 0;
      pts.forEach(p => {
        const d = Math.hypot(p[0], p[1]), inside = d <= rkm; if (inside) n++;
        ctx.beginPath(); ctx.arc(cx + p[0] * pxkm, cy + p[1] * pxkm, inside ? 5 : 3.5, 0, 6.2832);
        ctx.fillStyle = inside ? '#06A8D2' : '#aaa'; ctx.fill();
      });
      ctx.fillStyle = '#E8890C'; ctx.fillRect(cx - 7, cy - 7, 14, 14);
      $('[data-o=count]', root).textContent = n + ' of ' + pts.length;
    }
    slider.addEventListener('input', draw);
    $$('[data-m]', root).forEach(b => b.addEventListener('click', () => {
      mode = b.dataset.m; $$('[data-m]', root).forEach(x => x.classList.toggle('on', x === b)); draw();
    }));
    draw();
  }

  function boot() {
    $$('.reach').forEach(initReach);
    $$('.pca').forEach(initPca);
    $$('.calc').forEach(initCalc);
    $$('.agg').forEach(initAgg);
    $$('.cmp').forEach(initCmp);
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', boot);
  else boot();
})();
