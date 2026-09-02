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
      //console.log("outsideclick mousedown check", {target: e.target, insideAny});
      if (!insideAny) {
        members.forEach((el) => el.dispatchEvent(new CustomEvent("outsideclick")));
      }
    });
  });
  function focusGoalNearIndex(scopeEl, index) {
    const scope = scopeEl.closest(".theorem-instance") || document;
    const goals = Array.from(scope.querySelectorAll(".proof-goal"));
    const next = goals[Math.min(index, goals.length - 1)];
    if (next) next.focus();
  }
  window.focusGoalNearIndex = focusGoalNearIndex;
  function computeGoalIndex(el) {
    const scope = el.closest(".theorem-instance") || document;
    return Array.from(scope.querySelectorAll(".proof-goal")).indexOf(el);
  }
  window.computeGoalIndex = computeGoalIndex;

})();