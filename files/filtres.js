// Filtres par texte, année, type et mots-clés des pages Publications et Conférences.
(function () {
  const sansAccent = (t) => t.normalize("NFD").replace(/[̀-ͯ]/g, "").toLowerCase();

  document.querySelectorAll(".filtres").forEach((boite) => {
    const liste = document.querySelector(boite.dataset.cible);
    const items = Array.from(liste.querySelectorAll(".item"));
    const sections = Array.from(liste.querySelectorAll(".annee"));
    const textes = new Map(items.map((i) => [i, sansAccent(i.textContent)]));
    const texte = boite.querySelector(".f-texte");
    const annee = boite.querySelector(".f-annee");
    const compte = boite.querySelector(".f-compte");
    const cases = Array.from(boite.querySelectorAll("input[type=checkbox]"));
    const coches = (classe) =>
      Array.from(boite.querySelectorAll(`.${classe} input:checked`)).map((c) => c.value);

    function appliquer() {
      const q = sansAccent(texte.value.trim());
      const types = coches("f-types");
      const mots = coches("f-mots");
      let visibles = 0;

      items.forEach((item) => {
        const motsItem = item.dataset.mots.split("|");
        const ok =
          (!annee.value || item.dataset.annee === annee.value) &&
          (types.length === 0 || types.includes(item.dataset.type)) &&
          (mots.length === 0 || mots.some((m) => motsItem.includes(m))) &&
          (!q || textes.get(item).includes(q));
        item.hidden = !ok;
        if (ok) visibles += 1;
      });

      sections.forEach((s) => {
        s.hidden = !s.querySelector(".item:not([hidden])");
      });
      cases.forEach((c) => c.closest(".puce").classList.toggle("actif", c.checked));
      compte.textContent =
        visibles === items.length
          ? `${items.length} résultats`
          : `${visibles} résultat${visibles > 1 ? "s" : ""} sur ${items.length}`;
    }

    texte.addEventListener("input", appliquer);
    annee.addEventListener("change", appliquer);
    cases.forEach((c) => c.addEventListener("change", appliquer));
    boite.querySelector(".f-reset").addEventListener("click", () => {
      texte.value = "";
      annee.value = "";
      cases.forEach((c) => (c.checked = false));
      appliquer();
    });

    appliquer();
  });
})();
