// Ranking list: drag (mouse or finger), type the position, or ↑/↓ —
// all three stay in sync. Pointer events, so it works on phones too.

import { el } from "./ui.js";

/**
 * @param {{items: {code: string, name: string, description?: string}[],
 *          order?: string[], disabled?: boolean, label: string,
 *          onChange?: (order: string[]) => void}} opts
 * @returns {{node: HTMLElement, order: () => string[]}}
 */
export function createRanker({ items, order, disabled = false, label, onChange }) {
  const byCode = new Map(items.map((i) => [i.code, i]));
  let current = order && order.length === items.length && order.every((c) => byCode.has(c)) ? [...order] : items.map((i) => i.code);
  const list = el("ol.ranker", { "aria-label": label });

  const move = (from, to, focusCode) => {
    to = Math.max(0, Math.min(current.length - 1, to));
    if (from === to) return render(focusCode);
    const [code] = current.splice(from, 1);
    current.splice(to, 0, code);
    render(focusCode);
    onChange?.([...current]);
  };

  function render(focus) {
    list.replaceChildren(...current.map((code, i) => {
      const item = byCode.get(code);
      const pos = el("input.pos", {
        type: "number", min: 1, max: current.length, value: i + 1, disabled, inputmode: "numeric",
        "aria-label": `Θέση για «${item.name}»`,
        onchange: (e) => {
          if (!e.target.isConnected) return; // list already redrawn
          const n = Number.parseInt(e.target.value, 10);
          if (Number.isInteger(n)) move(i, n - 1, `${code}:pos`);
          else e.target.value = i + 1;
        },
        // Enter applies the new position (the change event fires on blur).
        onkeydown: (e) => { if (e.key === "Enter") { e.preventDefault(); e.target.blur(); } },
      });
      const handle = el("span.handle", {
        tabindex: disabled ? -1 : 0, role: "button", "aria-label": `Μετακίνηση «${item.name}» (βελάκια πάνω/κάτω)`,
        onkeydown: (e) => {
          if (e.key === "ArrowUp") { e.preventDefault(); move(i, i - 1, `${code}:handle`); }
          if (e.key === "ArrowDown") { e.preventDefault(); move(i, i + 1, `${code}:handle`); }
        },
      }, "⠿");
      if (!disabled) handle.addEventListener("pointerdown", (e) => startDrag(e, code));
      const li = el("li", { dataset: { code } },
        handle,
        pos,
        el("span.name", {}, item.name, item.tag ? el("span.tag", {}, item.tag) : null, item.description ? el("span.desc", {}, item.description) : null),
        el("span.moves", {},
          el("button.small", { type: "button", disabled: disabled || i === 0, "aria-label": `«${item.name}» μία θέση πάνω`, onclick: () => move(i, i - 1, `${code}:up`) }, "↑"),
          el("button.small", { type: "button", disabled: disabled || i === current.length - 1, "aria-label": `«${item.name}» μία θέση κάτω`, onclick: () => move(i, i + 1, `${code}:down`) }, "↓")));
      return li;
    }));
    if (focus) {
      const [code, part] = focus.split(":");
      const li = list.querySelector(`li[data-code="${CSS.escape(code)}"]`);
      const target = { pos: "input.pos", handle: ".handle", up: ".moves button:first-child", down: ".moves button:last-child" }[part];
      const node = li?.querySelector(target);
      (node && !node.disabled ? node : li?.querySelector(".handle"))?.focus();
    }
  }

  function startDrag(e, code) {
    e.preventDefault();
    const li = list.querySelector(`li[data-code="${CSS.escape(code)}"]`);
    const handle = e.currentTarget;
    handle.setPointerCapture(e.pointerId);
    li.classList.add("dragging");
    const startY = e.clientY + window.scrollY;
    const startIndex = current.indexOf(code);
    let index = startIndex;
    let lastY = e.clientY;
    let scrolling = 0;

    // Scroll the page when the finger is near the top/bottom edge.
    const autoScroll = () => {
      const edge = 60;
      const speed = lastY < edge ? -(edge - lastY) / 4 : lastY > innerHeight - edge ? (lastY - (innerHeight - edge)) / 4 : 0;
      if (speed) {
        window.scrollBy(0, speed);
        onMove({ clientY: lastY });
      }
      scrolling = requestAnimationFrame(autoScroll);
    };
    scrolling = requestAnimationFrame(autoScroll);

    const onMove = (ev) => {
      lastY = ev.clientY;
      li.style.transform = `translateY(${ev.clientY + window.scrollY - startY}px)`;
      // Position among siblings by their vertical midpoints.
      const others = [...list.children].filter((n) => n !== li);
      let target = others.length;
      for (let k = 0; k < others.length; k++) {
        const r = others[k].getBoundingClientRect();
        if (ev.clientY < r.top + r.height / 2) { target = k; break; }
      }
      index = target;
      others.forEach((n, k) => {
        const orig = current.indexOf(n.dataset.code);
        const shift = orig > startIndex && k < target ? -1 : orig < startIndex && k >= target ? 1 : 0;
        n.style.transform = shift ? `translateY(${shift * li.offsetHeight + shift * 6}px)` : "";
        n.style.transition = "transform 120ms";
      });
    };
    const onUp = () => {
      cancelAnimationFrame(scrolling);
      handle.removeEventListener("pointermove", onMove);
      handle.removeEventListener("pointerup", onUp);
      handle.removeEventListener("pointercancel", onUp);
      [...list.children].forEach((n) => { n.style.transform = ""; n.style.transition = ""; });
      li.classList.remove("dragging");
      move(startIndex, index, `${code}:handle`);
    };
    handle.addEventListener("pointermove", onMove);
    handle.addEventListener("pointerup", onUp);
    handle.addEventListener("pointercancel", onUp);
  }

  render();
  return { node: list, order: () => [...current] };
}
