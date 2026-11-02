// Tri par colonne et pagination des tableaux synthétiques (anciens membres, projets terminés).
(function () {
  document.querySelectorAll(".tableau-tri").forEach((table) => {
    const corps = table.tBodies[0];
    const taille = Number(table.dataset.lignes) || 10;
    const entetes = Array.from(table.tHead.rows[0].cells);
    let lignes = Array.from(corps.rows);
    let page = 0;

    const nav = document.createElement("div");
    nav.className = "tableau-nav";
    const precedent = document.createElement("button");
    const suivant = document.createElement("button");
    const info = document.createElement("span");
    precedent.textContent = "Précédent";
    suivant.textContent = "Suivant";
    [precedent, suivant].forEach((b) => {
      b.type = "button";
      b.className = "btn btn-sm btn-outline-primary";
    });
    nav.append(info, precedent, suivant);
    table.closest(".table-responsive").after(nav);

    function afficher() {
      const pages = Math.ceil(lignes.length / taille);
      lignes.forEach((l, i) => {
        l.hidden = Math.floor(i / taille) !== page;
      });
      const debut = page * taille + 1;
      const fin = Math.min((page + 1) * taille, lignes.length);
      info.textContent = `Lignes ${debut} à ${fin} sur ${lignes.length}`;
      precedent.disabled = page === 0;
      suivant.disabled = page >= pages - 1;
      nav.hidden = pages <= 1;
    }

    precedent.addEventListener("click", () => {
      page -= 1;
      afficher();
    });
    suivant.addEventListener("click", () => {
      page += 1;
      afficher();
    });

    entetes.forEach((th, c) => {
      const bouton = document.createElement("button");
      bouton.type = "button";
      bouton.className = "tri-bouton";
      bouton.textContent = th.textContent;
      th.replaceChildren(bouton);
      bouton.addEventListener("click", () => {
        const sens = th.getAttribute("aria-sort") === "ascending" ? -1 : 1;
        const num = th.dataset.tri === "num";
        const valeur = (l) => l.cells[c].textContent.trim();
        lignes.sort((a, b) => {
          const r = num
            ? Number(valeur(a)) - Number(valeur(b))
            : valeur(a).localeCompare(valeur(b), "fr");
          return r * sens;
        });
        entetes.forEach((e) => e.removeAttribute("aria-sort"));
        th.setAttribute("aria-sort", sens === 1 ? "ascending" : "descending");
        corps.append(...lignes);
        page = 0;
        afficher();
      });
    });

    afficher();
  });
})();
