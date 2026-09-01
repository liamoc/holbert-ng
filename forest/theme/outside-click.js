(function () {
  document.addEventListener("mousedown", (e) => {
    const groups = new Set();
    document.querySelectorAll("[class]").forEach((el) => {
      el.classList.forEach((cls) => {
        if (cls.startsWith("outside-click-group-")) groups.add(cls);
      });
    });
    groups.forEach((groupClass) => {
      const members = document.querySelectorAll(`.${groupClass}`);
      const insideAny = Array.from(members).some((el) => el.contains(e.target));
      if (!insideAny) {
        members.forEach((el) => el.dispatchEvent(new CustomEvent("outsideclick")));
      }
    });
  });
})();