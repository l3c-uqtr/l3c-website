# Bloque la mise en ligne si une donnée confidentielle se retrouve dans le site compilé.
# Lancé automatiquement à la fin de `quarto render` (voir post-render dans _quarto.yml).

suppressPackageStartupMessages({
  library(readxl)
  library(yaml)
})

cfg <- read_yaml("sources.yml")
dossier <- "docs"

fichiers <- list.files(dossier, recursive = TRUE, full.names = TRUE)
interdits <- fichiers[grepl("\\.(xlsx|xls|ods|bib)$", fichiers, ignore.case = TRUE)]
if (length(interdits)) {
  stop("Fichiers sources présents dans le site compilé : ", paste(interdits, collapse = ", "))
}

if (!file.exists(cfg$sources$cvdata)) {
  message("CVdata introuvable : vérification des contenus ignorée.")
  quit(save = "no", status = 0)
}

normaliser <- function(x) tolower(trimws(gsub("[[:space:]]+", " ", x)))

articles <- read_excel(cfg$sources$cvdata, sheet = "articles", col_types = "text")
sub <- read_excel(cfg$sources$cvdata, sheet = "subvention", col_types = "text")
obtenues <- normaliser(sub$title[!is.na(sub$win) & sub$win == "1"])
refusees <- setdiff(normaliser(sub$title[is.na(sub$win) | sub$win != "1"]), obtenues)

secrets <- c(normaliser(articles$title), refusees, normaliser(articles$id))
secrets <- unique(secrets[!is.na(secrets) & nchar(secrets) >= 8])
# Les 50 premiers caractères suffisent à repérer un titre
secrets <- substr(secrets, 1, 50)

pages <- fichiers[grepl("\\.(html|json|xml|js)$", fichiers)]
pages <- pages[!grepl("site_libs", pages, fixed = TRUE)]
texte <- vapply(pages, function(p) {
  normaliser(paste(readLines(p, encoding = "UTF-8", warn = FALSE), collapse = " "))
}, "")

trouves <- character(0)
for (s in secrets) {
  ou <- names(texte)[vapply(texte, function(t) grepl(s, t, fixed = TRUE), TRUE)]
  if (length(ou)) trouves <- c(trouves, sprintf("\"%s\" dans %s", s, ou[1]))
}
if (length(trouves)) {
  stop("Données confidentielles détectées dans le site :\n", paste(trouves, collapse = "\n"))
}
message(sprintf("Confidentialité vérifiée : %d contenus recherchés dans %d pages, aucun trouvé.",
                length(secrets), length(pages)))
