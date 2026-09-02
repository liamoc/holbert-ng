// resizable.js
(function () {
  let lastWidth = null; 
  function initResizable(el) {
    const handle = document.createElement("div");
    handle.className = "resize-handle";
    el.prepend(handle);

    const minWidth = parseInt(el.dataset.minWidth || "220", 10);
    const maxWidth = parseInt(el.dataset.maxWidth || "1000", 10);
    if (lastWidth != null) {
      el.style.width = lastWidth + "px";
    }
    let dragging = false;

    handle.addEventListener("mousedown", (e) => {
      dragging = true;
      document.body.style.userSelect = "none"; // avoid text-selection while dragging
      e.preventDefault();
    });

    window.addEventListener("mousemove", (e) => {
      if (!dragging) return;
      const rect = el.getBoundingClientRect();
      const newWidth = rect.right - e.clientX;
      const newWidth2 = Math.min(maxWidth, Math.max(minWidth, newWidth))
      el.style.width = newWidth2 + "px";
      lastWidth = newWidth2;
    });

    window.addEventListener("mouseup", () => {
      if (dragging) {
        dragging = false;
        document.body.style.userSelect = "";
      }
    });
  }

  function scan() {
    document.querySelectorAll(".resizable-panel:not([data-resizable-init])").forEach((el) => {
      el.dataset.resizableInit = "true";
      initResizable(el);
    });
  }

  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, {childList: true, subtree: true});
})();