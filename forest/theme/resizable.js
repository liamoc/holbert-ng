// resizable.js
(function () {
  function initResizable(el) {
    const handle = document.createElement("div");
    handle.className = "resize-handle";
    el.prepend(handle);

    const minWidth = parseInt(el.dataset.minWidth || "220", 10);
    const maxWidth = parseInt(el.dataset.maxWidth || "600", 10);

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
      el.style.width = Math.min(maxWidth, Math.max(minWidth, newWidth)) + "px";
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