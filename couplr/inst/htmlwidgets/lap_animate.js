// =============================================================================
// lap_animate htmlwidget
// =============================================================================
// Two-column ladder visualisation of an assignment-algorithm trace.
//
// LAYOUT
//   Left column = rows (R1..Rn), right column = columns (C1..Cm), both in
//   natural index order. A single shared spacing is used so R_i and C_i sit
//   at the same y for square inputs. Nodes never move.
//
// EMPHASIS
//   Matched pairs are visually "settled": the row node and column node both
//   get a .matched modifier that fades them to low opacity, while the
//   matched edge stays bright green. The result is that the algorithm's
//   active work (un-faded nodes, orange active edges, red dashed path
//   edges) stays prominent against a backdrop of dimmed finished pairs.
//
// EDGE STATES (CSS priority: path > active > matched)
//   matched  green       -- in the current matching
//   active   orange      -- being explored this frame
//   path     red dashed  -- on the current augmenting path
//
// Edges are drawn lazily -- the scene starts with only dots and grows
// edges in via the stroke-dashoffset trick. There is no pre-drawn n*m
// backdrop. This keeps the picture readable up to n = 100.
//
// Trace shape (from R/animate.R):
//   x = {
//     meta: { algorithm, n_rows, n_cols, cost_matrix[][], maximize,
//             total_cost, description },
//     frames: [
//       { step, phase, description,
//         matching: int[n_rows]   // 1-indexed col; 0 = unmatched
//         dual_u : number[] | null,
//         dual_v : number[] | null,
//         active_edges: [[r,c], ...],
//         path        : [[r,c], ...] },
//       ...
//     ]
//   }
// =============================================================================

HTMLWidgets.widget({
  name: 'lap_animate',
  type: 'output',

  factory: function (el, width, height) {

    // -----------------------------------------------------------------------
    // DOM scaffolding
    // -----------------------------------------------------------------------
    el.classList.add('couplr-lap-animate');
    el.innerHTML = '';

    const header = document.createElement('div');
    header.className = 'couplr-header';
    header.innerHTML =
      '<div><span class="algo-name"></span><span class="phase-badge"></span></div>' +
      '<div><span class="match-counter"></span><span class="step-counter"></span></div>';
    el.appendChild(header);

    const canvas = document.createElement('div');
    canvas.className = 'couplr-canvas';
    el.appendChild(canvas);

    const footer = document.createElement('div');
    footer.className = 'couplr-footer';
    footer.innerHTML =
      '<div class="couplr-description"></div>' +
      '<div class="couplr-controls">' +
        '<button class="reset" title="Reset to first frame">&#x23EE;</button>' +
        '<button class="step-prev" title="Step backward">&#x25C0;</button>' +
        '<button class="play-pause" title="Play / pause">Play</button>' +
        '<button class="step-next" title="Step forward">&#x25B6;</button>' +
        '<span class="speed-control">' +
          '<label>Speed</label>' +
          '<input type="range" min="100" max="2000" step="100" value="800">' +
        '</span>' +
      '</div>';
    el.appendChild(footer);

    const svgNS = 'http://www.w3.org/2000/svg';
    const svg = document.createElementNS(svgNS, 'svg');
    svg.setAttribute('preserveAspectRatio', 'xMidYMid meet');
    canvas.appendChild(svg);

    // Layers (back-to-front)
    const gEdges      = document.createElementNS(svgNS, 'g');
    const gEdgeCosts  = document.createElementNS(svgNS, 'g');
    const gNodes      = document.createElementNS(svgNS, 'g');
    const gNodeLabels = document.createElementNS(svgNS, 'g');
    const gDuals      = document.createElementNS(svgNS, 'g');
    svg.appendChild(gEdges);
    svg.appendChild(gEdgeCosts);
    svg.appendChild(gNodes);
    svg.appendChild(gNodeLabels);
    svg.appendChild(gDuals);

    // -----------------------------------------------------------------------
    // State
    // -----------------------------------------------------------------------
    const state = {
      trace: null,
      n: 0, m: 0,
      frameIdx: 0,
      isPlaying: false,
      speedMs: 800,
      timer: null,
      layout: null,
      rowNodes: [], colNodes: [],
      rowLabels: [], colLabels: [],
      liveEdges: new Map()   // "r-c" -> { line, costEl, type }
    };

    // -----------------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------------
    function edgeKey(r, c) { return r + '-' + c; }

    function normalizeEdgeList(edges) {
      if (!Array.isArray(edges)) return [];
      const out = [];
      for (const e of edges) {
        if (Array.isArray(e) && e.length >= 2) out.push([+e[0], +e[1]]);
      }
      return out;
    }

    function fmtNum(x) {
      if (x === null || x === undefined || Number.isNaN(x)) return '';
      if (!Number.isFinite(x)) return '';
      if (Math.abs(x) >= 1000 || (Math.abs(x) < 0.01 && x !== 0)) {
        return x.toExponential(1);
      }
      if (Number.isInteger(x)) return String(x);
      return x.toFixed(2);
    }

    // -----------------------------------------------------------------------
    // Layout
    // -----------------------------------------------------------------------
    // Single shared spacing for both columns. R_i sits at slot (i-1), C_j
    // at slot (j-1). Total slot count is max(n, m).
    // -----------------------------------------------------------------------
    function computeLayout() {
      const rect = canvas.getBoundingClientRect();
      const W = Math.max(rect.width, 320);
      const H = Math.max(rect.height, 200);

      svg.setAttribute('viewBox', '0 0 ' + W + ' ' + H);
      svg.setAttribute('width', W);
      svg.setAttribute('height', H);

      const nSlots = Math.max(state.n, state.m, 1);
      const padY = 24;
      const spacing = (H - 2 * padY) / Math.max(nSlots - 1, 1);

      const showNodeLabels = nSlots <= 10;
      const showCostLabels = state.n <= 8 && state.m <= 8;
      const showDualLabels = nSlots <= 15;

      const padX = showNodeLabels ? 64 : 36;
      const nodeR = Math.max(2, Math.min(spacing * 0.42, 22));

      const slotY = new Array(nSlots);
      if (nSlots === 1) {
        slotY[0] = H / 2;
      } else {
        const totalH = (nSlots - 1) * spacing;
        const startY = (H - totalH) / 2;
        for (let s = 0; s < nSlots; s++) slotY[s] = startY + s * spacing;
      }

      state.layout = {
        W: W, H: H,
        rowX: padX, colX: W - padX,
        nodeR: nodeR,
        spacing: spacing,
        slotY: slotY,
        showNodeLabels: showNodeLabels,
        showCostLabels: showCostLabels,
        showDualLabels: showDualLabels
      };
    }

    // -----------------------------------------------------------------------
    // Stage construction: row and column dots in natural order.
    // -----------------------------------------------------------------------
    function buildScene() {
      gEdges.innerHTML = '';
      gEdgeCosts.innerHTML = '';
      gNodes.innerHTML = '';
      gNodeLabels.innerHTML = '';
      gDuals.innerHTML = '';
      state.liveEdges.clear();
      state.rowNodes = []; state.colNodes = [];
      state.rowLabels = []; state.colLabels = [];

      const L = state.layout;
      const strokeW = L.nodeR >= 6 ? 1.5 : 0;

      for (let i = 1; i <= state.n; i++) {
        const y = L.slotY[i - 1];
        const node = document.createElementNS(svgNS, 'circle');
        node.setAttribute('cx', L.rowX);
        node.setAttribute('cy', y);
        node.setAttribute('r', L.nodeR);
        node.setAttribute('stroke-width', strokeW);
        node.setAttribute('class', 'couplr-node row');
        gNodes.appendChild(node);
        state.rowNodes[i] = node;

        if (L.showNodeLabels) {
          const label = document.createElementNS(svgNS, 'text');
          label.setAttribute('x', L.rowX);
          label.setAttribute('y', y);
          label.setAttribute('class', 'couplr-node-label');
          label.textContent = 'R' + i;
          gNodeLabels.appendChild(label);
          state.rowLabels[i] = label;
        }
      }

      for (let j = 1; j <= state.m; j++) {
        const y = L.slotY[j - 1];
        const node = document.createElementNS(svgNS, 'circle');
        node.setAttribute('cx', L.colX);
        node.setAttribute('cy', y);
        node.setAttribute('r', L.nodeR);
        node.setAttribute('stroke-width', strokeW);
        node.setAttribute('class', 'couplr-node col');
        gNodes.appendChild(node);
        state.colNodes[j] = node;

        if (L.showNodeLabels) {
          const label = document.createElementNS(svgNS, 'text');
          label.setAttribute('x', L.colX);
          label.setAttribute('y', y);
          label.setAttribute('class', 'couplr-node-label');
          label.textContent = 'C' + j;
          gNodeLabels.appendChild(label);
          state.colLabels[j] = label;
        }
      }
    }

    function rowY(r) { return state.layout.slotY[r - 1]; }
    function colY(c) { return state.layout.slotY[c - 1]; }

    // -----------------------------------------------------------------------
    // Edge lifecycle
    // -----------------------------------------------------------------------
    function createEdge(r, c, type) {
      const L = state.layout;
      const cost = state.trace.meta.cost_matrix;
      const cVal = (cost && cost[r - 1] && cost[r - 1][c - 1] !== undefined)
                   ? cost[r - 1][c - 1] : null;
      if (cVal === null || !Number.isFinite(cVal)) return null;

      const x1 = L.rowX, y1 = rowY(r);
      const x2 = L.colX, y2 = colY(c);
      const len = Math.hypot(x2 - x1, y2 - y1);

      const line = document.createElementNS(svgNS, 'line');
      line.setAttribute('x1', x1);
      line.setAttribute('y1', y1);
      line.setAttribute('x2', x2);
      line.setAttribute('y2', y2);
      line.setAttribute('class', 'couplr-edge ' + type);
      line.setAttribute('stroke-dasharray', len);
      line.setAttribute('stroke-dashoffset', len);
      gEdges.appendChild(line);

      if (typeof line.animate === 'function') {
        const anim = line.animate(
          [{ strokeDashoffset: len }, { strokeDashoffset: 0 }],
          { duration: 320, easing: 'ease-out', fill: 'forwards' }
        );
        anim.onfinish = function () {
          line.setAttribute('stroke-dashoffset', '0');
        };
      } else {
        line.setAttribute('stroke-dashoffset', '0');
      }

      // Pulse-on-commit: a new matched edge enters at full opacity and
      // thicker stroke, then settles to the faded CSS look.
      if (type === 'matched') pulseMatch(line);

      let costEl = null;
      if (L.showCostLabels) {
        const mx = (x1 + x2) / 2;
        const my = (y1 + y2) / 2;
        costEl = document.createElementNS(svgNS, 'text');
        costEl.setAttribute('x', mx);
        costEl.setAttribute('y', my);
        costEl.setAttribute('class', 'couplr-edge-cost');
        costEl.setAttribute('text-anchor', 'middle');
        costEl.setAttribute('dominant-baseline', 'central');
        costEl.textContent = fmtNum(cVal);
        gEdgeCosts.appendChild(costEl);
      }

      return { line: line, costEl: costEl, type: type, r: r, c: c };
    }

    function removeEdge(rec) {
      if (rec.line && rec.line.parentNode) rec.line.parentNode.removeChild(rec.line);
      if (rec.costEl && rec.costEl.parentNode) rec.costEl.parentNode.removeChild(rec.costEl);
    }

    // Flash an edge at full opacity + thicker stroke, then settle. Used
    // when an edge enters or transitions into the "matched" state, so
    // the moment of commitment is visible before the edge fades into
    // the matched-backbone group.
    function pulseMatch(line) {
      if (typeof line.animate !== 'function') return;
      line.animate(
        [
          { opacity: 1.0, strokeWidth: 4.4, offset: 0 },
          { opacity: 1.0, strokeWidth: 3.4, offset: 0.45 },
          { opacity: 0.4, strokeWidth: 2.4, offset: 1.0 }
        ],
        { duration: 600, easing: 'ease-out', fill: 'forwards' }
      );
    }

    function reconcileEdges(frame) {
      const desired = new Map();
      if (Array.isArray(frame.matching)) {
        for (let i = 0; i < frame.matching.length; i++) {
          const c = frame.matching[i];
          if (c && c > 0) desired.set(edgeKey(i + 1, c), 'matched');
        }
      }
      for (const rc of normalizeEdgeList(frame.active_edges)) {
        desired.set(edgeKey(rc[0], rc[1]), 'active');
      }
      for (const rc of normalizeEdgeList(frame.path)) {
        desired.set(edgeKey(rc[0], rc[1]), 'path');
      }

      for (const [k, rec] of state.liveEdges) {
        if (!desired.has(k)) {
          removeEdge(rec);
          state.liveEdges.delete(k);
        }
      }

      for (const [k, type] of desired) {
        const existing = state.liveEdges.get(k);
        if (!existing) {
          const dash = k.indexOf('-');
          const r = +k.substring(0, dash);
          const c = +k.substring(dash + 1);
          const rec = createEdge(r, c, type);
          if (rec) state.liveEdges.set(k, rec);
        } else if (existing.type !== type) {
          existing.line.setAttribute('class', 'couplr-edge ' + type);
          if (type === 'matched') pulseMatch(existing.line);
          existing.type = type;
        }
      }
    }

    // -----------------------------------------------------------------------
    // Match emphasis: fade nodes (and labels) whose pair is in the current
    // matching. The matched edge stays bright green, but its endpoints
    // recede so the eye lands on the unmatched nodes still in play.
    // -----------------------------------------------------------------------
    function applyMatchedClass(frame) {
      const matching = Array.isArray(frame.matching) ? frame.matching : [];
      const matchedCols = new Set();
      for (let i = 1; i <= state.n; i++) {
        const c = matching[i - 1];
        const node = state.rowNodes[i];
        const label = state.rowLabels[i];
        if (c && c > 0) {
          matchedCols.add(c);
          if (node) node.setAttribute('class', 'couplr-node row matched');
          if (label) label.setAttribute('class', 'couplr-node-label matched');
        } else {
          if (node) node.setAttribute('class', 'couplr-node row');
          if (label) label.setAttribute('class', 'couplr-node-label');
        }
      }
      for (let j = 1; j <= state.m; j++) {
        const node = state.colNodes[j];
        const label = state.colLabels[j];
        if (matchedCols.has(j)) {
          if (node) node.setAttribute('class', 'couplr-node col matched');
          if (label) label.setAttribute('class', 'couplr-node-label matched');
        } else {
          if (node) node.setAttribute('class', 'couplr-node col');
          if (label) label.setAttribute('class', 'couplr-node-label');
        }
      }
    }

    // -----------------------------------------------------------------------
    // Frame rendering
    // -----------------------------------------------------------------------
    function renderFrame(idx) {
      const trace = state.trace;
      if (!trace) return;
      const nFrames = trace.frames.length;
      if (idx < 0) idx = 0;
      if (idx >= nFrames) idx = nFrames - 1;
      state.frameIdx = idx;

      const frame = trace.frames[idx];

      reconcileEdges(frame);
      applyMatchedClass(frame);

      gDuals.innerHTML = '';
      const L = state.layout;
      if (L.showDualLabels) {
        const ur = frame.dual_u, vc = frame.dual_v;
        if (Array.isArray(ur)) {
          for (let i = 1; i <= state.n && i - 1 < ur.length; i++) {
            const u = ur[i - 1];
            if (u === null || u === undefined) continue;
            const t = document.createElementNS(svgNS, 'text');
            t.setAttribute('x', L.rowX - L.nodeR - 6);
            t.setAttribute('y', rowY(i));
            t.setAttribute('class', 'couplr-dual-label');
            t.setAttribute('text-anchor', 'end');
            t.setAttribute('dominant-baseline', 'central');
            t.textContent = 'u=' + fmtNum(u);
            gDuals.appendChild(t);
          }
        }
        if (Array.isArray(vc)) {
          for (let j = 1; j <= state.m && j - 1 < vc.length; j++) {
            const v = vc[j - 1];
            if (v === null || v === undefined) continue;
            const t = document.createElementNS(svgNS, 'text');
            t.setAttribute('x', L.colX + L.nodeR + 6);
            t.setAttribute('y', colY(j));
            t.setAttribute('class', 'couplr-dual-label');
            t.setAttribute('text-anchor', 'start');
            t.setAttribute('dominant-baseline', 'central');
            t.textContent = 'v=' + fmtNum(v);
            gDuals.appendChild(t);
          }
        }
      }

      let matchedCount = 0;
      if (Array.isArray(frame.matching)) {
        for (let i = 0; i < frame.matching.length; i++) {
          if (frame.matching[i] && frame.matching[i] > 0) matchedCount++;
        }
      }

      // Algorithms whose progress metric is something other than matched-edge
      // count (e.g. GT bit-scaling, where the matched count yo-yos but
      // bit-precision is monotonic) can override the counter text per frame.
      const counterText = (typeof frame.progress_text === 'string' && frame.progress_text)
        ? frame.progress_text
        : (matchedCount + ' / ' + state.n + ' matched');

      header.querySelector('.algo-name').textContent = trace.meta.algorithm || '';
      header.querySelector('.phase-badge').textContent = frame.phase || '';
      header.querySelector('.match-counter').textContent = counterText;
      header.querySelector('.step-counter').textContent =
        'Step ' + (idx + 1) + ' / ' + nFrames;
      footer.querySelector('.couplr-description').textContent =
        frame.description || '';

      footer.querySelector('.step-prev').disabled = (idx <= 0);
      footer.querySelector('.step-next').disabled = (idx >= nFrames - 1);
    }

    // -----------------------------------------------------------------------
    // Playback
    // -----------------------------------------------------------------------
    function setPlaying(playing) {
      state.isPlaying = playing;
      footer.querySelector('.play-pause').textContent = playing ? 'Pause' : 'Play';
      if (state.timer) { clearInterval(state.timer); state.timer = null; }
      if (playing) {
        state.timer = setInterval(() => {
          if (state.frameIdx >= state.trace.frames.length - 1) {
            setPlaying(false);
            return;
          }
          renderFrame(state.frameIdx + 1);
        }, state.speedMs);
      }
    }

    footer.querySelector('.play-pause').addEventListener('click', () => {
      if (!state.trace) return;
      if (state.frameIdx >= state.trace.frames.length - 1 && !state.isPlaying) {
        renderFrame(0);
      }
      setPlaying(!state.isPlaying);
    });
    footer.querySelector('.step-prev').addEventListener('click', () => {
      setPlaying(false);
      renderFrame(state.frameIdx - 1);
    });
    footer.querySelector('.step-next').addEventListener('click', () => {
      setPlaying(false);
      renderFrame(state.frameIdx + 1);
    });
    footer.querySelector('.reset').addEventListener('click', () => {
      setPlaying(false);
      renderFrame(0);
    });
    footer.querySelector('.speed-control input').addEventListener('input', (ev) => {
      state.speedMs = +ev.target.value;
      if (state.isPlaying) setPlaying(true);
    });

    return {
      renderValue: function (x) {
        state.trace = x;
        state.n = x.meta.n_rows || 0;
        state.m = x.meta.n_cols || 0;
        state.frameIdx = 0;
        setPlaying(false);

        computeLayout();
        buildScene();
        renderFrame(0);
      },
      resize: function (w, h) {
        if (!state.trace) return;
        computeLayout();
        buildScene();
        renderFrame(state.frameIdx);
      }
    };
  }
});
