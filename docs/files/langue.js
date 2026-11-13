// Le lien EN / FR du menu mène à la même page dans l'autre langue.
// Exécuté après le chargement, car Quarto réécrit les liens du menu au démarrage.
window.addEventListener("load", function () {
  const en = document.documentElement.lang === "en";
  const page = location.pathname.split("/").pop() || "index.html";
  document.querySelectorAll(".navbar a.nav-link").forEach((lien) => {
    if (lien.textContent.trim() === (en ? "FR" : "EN")) {
      lien.setAttribute("href", (en ? "../" : "en/") + page);
      lien.setAttribute("hreflang", en ? "fr" : "en");
    }
  });
});
