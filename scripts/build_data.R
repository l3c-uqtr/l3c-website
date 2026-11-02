# Génère les contenus du site (dossier _generated/) à partir de CVdata.xlsx et des .bib.
# Lancé automatiquement par `quarto render` (voir pre-render dans _quarto.yml).
#
# CONFIDENTIALITÉ : chaque onglet est lu explicitement et seules les colonnes utilisées ici
# sont publiées. Les onglets non cités (articles, jury, expertise, selection_prof, clinique...)
# ne sont jamais lus. Les subventions non obtenues sont exclues.

suppressPackageStartupMessages({
  library(readxl)
  library(yaml)
  library(jsonlite)
})

cfg <- read_yaml("sources.yml")
dir_gen <- "_generated"
fichier_cache <- "scripts/abstracts_cache.json"
aujourd_hui <- as.integer(format(Sys.Date(), "%Y")) * 100L + as.integer(format(Sys.Date(), "%m"))

manquants <- Filter(function(p) !file.exists(p), cfg$sources)
if (length(manquants) > 0) {
  if (dir.exists(dir_gen)) {
    message("Sources introuvables, le site garde ses données actuelles : ",
            paste(unlist(manquants), collapse = ", "))
    quit(save = "no", status = 0)
  }
  stop("Sources introuvables : ", paste(unlist(manquants), collapse = ", "))
}
dir.create(dir_gen, showWarnings = FALSE)

avertissements <- character(0)
avertir <- function(...) avertissements <<- c(avertissements, paste0(...))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1]) || !nzchar(a[1])) b else a

# --- Outils texte -------------------------------------------------------------

esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

propre <- function(x) trimws(gsub("[[:space:]]+", " ", x))

cle_tri <- function(x) {
  tolower(chartr("àâäçéèêëîïôöùûüÀÂÄÇÉÈÊËÎÏÔÖÙÛÜ", "aaaceeeeiioouuuAAACEEEEIIOOUUU", x))
}

delatex <- function(x) {
  acc <- list("\\\\'e" = "é", "\\\\`e" = "è", "\\\\\\^e" = "ê", "\\\\\"e" = "ë", "\\\\'E" = "É",
              "\\\\`a" = "à", "\\\\\\^a" = "â", "\\\\\"a" = "ä", "\\\\c\\s*c" = "ç",
              "\\\\\\^o" = "ô", "\\\\\"o" = "ö", "\\\\\\^i" = "î", "\\\\\"i" = "ï",
              "\\\\`u" = "ù", "\\\\\\^u" = "û", "\\\\\"u" = "ü")
  x <- gsub("\\\\([&%_#$])", "\\1", x)
  x <- gsub("\\{\\\\(['`^\"])\\{?([a-zA-Z])\\}?\\}", "\\\\\\1\\2", x)
  for (k in names(acc)) x <- gsub(k, acc[[k]], x)
  x <- gsub("---", "—", gsub("--", "–", x, fixed = TRUE), fixed = TRUE)
  propre(gsub("[{}]", "", x))
}

# --- Lecture des .bib (analyseur tolérant : accolades équilibrées) --------------

lire_bib <- function(chemin) {
  lignes <- readLines(chemin, encoding = "UTF-8", warn = FALSE)
  lignes <- enc2utf8(sub("^﻿", "", lignes))
  ch <- strsplit(paste(lignes[!grepl("^\\s*%", lignes)], collapse = "\n"), "", fixed = TRUE)[[1]]
  n <- length(ch)
  ouvre <- ch == "{"
  ferme <- ch == "}"
  profondeur <- cumsum(ouvre) - cumsum(ferme)

  # Position de l'accolade fermant celle ouverte en i
  fermeture <- function(i) {
    cible <- profondeur[i] - 1L
    j <- which(ferme & profondeur == cible & seq_len(n) > i)
    if (length(j)) j[1] else NA_integer_
  }

  entrees <- list()
  ats <- which(ch == "@" & c(0L, head(profondeur, -1)) == 0L)
  for (a in ats) {
    debut <- a + which(ouvre[a:min(a + 40L, n)])[1] - 1L
    if (is.na(debut)) next
    type <- tolower(trimws(paste(ch[(a + 1L):(debut - 1L)], collapse = "")))
    fin <- fermeture(debut)
    if (is.na(fin) || type %in% c("comment", "string", "preamble")) next
    corps <- ch[(debut + 1L):(fin - 1L)]
    e <- lire_champs(corps)
    e$type_bib <- type
    entrees[[length(entrees) + 1L]] <- e
  }
  entrees
}

lire_champs <- function(ch) {
  n <- length(ch)
  virgule <- which(ch == ",")[1]
  if (is.na(virgule)) virgule <- n + 1L
  res <- list(cle = trimws(paste(ch[seq_len(virgule - 1L)], collapse = "")))
  i <- virgule + 1L
  while (i <= n) {
    while (i <= n && grepl("[[:space:],]", ch[i])) i <- i + 1L
    if (i > n) break
    egal <- i
    while (egal <= n && ch[egal] != "=") egal <- egal + 1L
    if (egal > n) break
    nom <- tolower(trimws(paste(ch[i:(egal - 1L)], collapse = "")))
    v <- egal + 1L
    while (v <= n && grepl("[[:space:]]", ch[v])) v <- v + 1L
    if (v > n) break
    if (ch[v] == "{") {
      prof <- 0L
      j <- v
      repeat {
        if (ch[j] == "{") prof <- prof + 1L
        if (ch[j] == "}") prof <- prof - 1L
        if (prof == 0L || j == n) break
        j <- j + 1L
      }
      valeur <- if (j > v + 1L) paste(ch[(v + 1L):(j - 1L)], collapse = "") else ""
      i <- j + 1L
    } else if (ch[v] == "\"") {
      j <- v + 1L
      while (j <= n && ch[j] != "\"") j <- j + 1L
      valeur <- if (j > v + 1L) paste(ch[(v + 1L):(j - 1L)], collapse = "") else ""
      i <- j + 1L
    } else {
      j <- v
      while (j <= n && ch[j] != ",") j <- j + 1L
      valeur <- trimws(paste(ch[v:(j - 1L)], collapse = ""))
      i <- j + 1L
    }
    res[[nom]] <- valeur
  }
  res
}

champ <- function(e, nom) {
  v <- e[[nom]]
  if (is.null(v)) return(NA_character_)
  v <- delatex(v)
  if (nzchar(v)) v else NA_character_
}

# --- Mise en forme des références ----------------------------------------------

initiales <- function(prenom) {
  mots <- strsplit(trimws(prenom), "[[:space:]]+")[[1]]
  vapply(mots, function(m) {
    parties <- strsplit(gsub("\\.", "", m), "-", fixed = TRUE)[[1]]
    parties <- parties[nzchar(parties)]
    paste0(substr(parties, 1, 1), ".", collapse = "-")
  }, "") |> paste(collapse = " ")
}

auteurs_html <- function(e) {
  brut <- e$author %||% e$editor
  if (is.null(brut)) return("")
  noms <- strsplit(propre(brut), "\\s+and\\s+")[[1]]
  notes <- list()
  if (!is.null(e$author_an)) {
    for (p in strsplit(e$author_an, "[;,]")[[1]]) {
      kv <- trimws(strsplit(p, "=", fixed = TRUE)[[1]])
      if (length(kv) == 2) notes[[kv[1]]] <- c(notes[[kv[1]]], kv[2])
    }
  }
  morceaux <- vapply(seq_along(noms), function(i) {
    nm <- delatex(noms[i])
    if (tolower(nm) == "others") return("et al.")
    if (grepl(",", nm, fixed = TRUE)) {
      p <- trimws(strsplit(nm, ",", fixed = TRUE)[[1]])
      famille <- p[1]
      prenom <- if (length(p) > 1) p[2] else ""
    } else {
      mots <- strsplit(nm, " ", fixed = TRUE)[[1]]
      famille <- mots[length(mots)]
      prenom <- paste(mots[-length(mots)], collapse = " ")
    }
    txt <- esc(if (nzchar(prenom)) paste0(famille, ", ", initiales(prenom)) else famille)
    n <- notes[[as.character(i)]]
    if ("bold" %in% n || famille == "Vallet") txt <- paste0("<strong>", txt, "</strong>")
    if ("stu" %in% n) txt <- paste0(txt, "<sup>†</sup>")
    if ("first" %in% n) txt <- paste0(txt, "<sup>*</sup>")
    txt
  }, "")
  paste(morceaux, collapse = ", ")
}

mots_cles <- function(e) {
  bruts <- trimws(unlist(strsplit(c(e$field %||% "", e$spe %||% ""), "[,;]")))
  bruts <- bruts[nzchar(bruts)]
  table_mc <- cfg$mots_cles
  noms_mc <- tolower(names(table_mc))
  res <- character(0)
  for (b in bruts) {
    k <- match(tolower(b), noms_mc)
    if (is.na(k)) {
      avertir("Mot-clé non traduit dans sources.yml : ", b)
      res <- c(res, b)
    } else if (!is.null(table_mc[[k]])) {
      res <- c(res, table_mc[[k]])
    }
  }
  unique(res)
}

MOIS_FR <- c("janvier", "février", "mars", "avril", "mai", "juin", "juillet",
             "août", "septembre", "octobre", "novembre", "décembre")
MOIS_EN <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

date_entree <- function(e) {
  d <- e$date %||% ""
  m <- regmatches(d, gregexpr("[0-9]{4}(-[0-9]{2}(-[0-9]{2})?)?", d))[[1]]
  if (length(m)) {
    annee <- as.integer(substr(m[1], 1, 4))
    mois <- if (nchar(m[1]) >= 7) as.integer(substr(m[1], 6, 7)) else NA_integer_
    jour <- if (nchar(m[1]) == 10) as.integer(substr(m[1], 9, 10)) else NA_integer_
    jour2 <- if (length(m) > 1 && substr(m[2], 1, 7) == substr(m[1], 1, 7) && nchar(m[2]) == 10) {
      as.integer(substr(m[2], 9, 10))
    } else NA_integer_
  } else {
    annee <- suppressWarnings(as.integer(regmatches(e$year %||% "", regexpr("[0-9]{4}", e$year %||% ""))))
    if (length(annee) == 0) annee <- NA_integer_
    txt <- tolower(e$month %||% "")
    mois <- which(vapply(MOIS_EN, function(k) grepl(k, txt, fixed = TRUE), TRUE))[1]
    jour <- NA_integer_
    jour2 <- NA_integer_
  }
  texte <- if (is.na(annee)) "s.d." else if (is.na(mois)) as.character(annee) else {
    j <- if (is.na(jour)) "" else if (is.na(jour2) || jour2 == jour) {
      paste0(if (jour == 1L) "1er" else jour, " ")
    } else paste0(jour, "–", jour2, " ")
    paste0(j, MOIS_FR[mois], " ", annee)
  }
  list(annee = annee, texte = texte,
       tri = (annee %||% 0L) * 10000L + (mois %||% 0L) * 100L + (jour %||% 0L))
}

# --- Résumés (récupérés sur CrossRef, conservés dans un cache) --------------------

cache_resumes <- if (file.exists(fichier_cache)) {
  fromJSON(fichier_cache, simplifyVector = FALSE)
} else list()

resume_doi <- function(doi) {
  if (is.na(doi)) return(NA_character_)
  if (doi %in% names(cache_resumes)) {
    v <- cache_resumes[[doi]]
    return(if (is.null(v)) NA_character_ else v)
  }
  options(timeout = 8)
  url <- paste0("https://api.crossref.org/works/", URLencode(doi, reserved = TRUE))
  rep <- tryCatch(fromJSON(url), error = function(e) NULL)
  if (is.null(rep)) return(NA_character_)
  brut <- rep$message$abstract
  texte <- if (is.null(brut)) NA_character_ else {
    t <- propre(gsub("<[^>]+>", " ", brut))
    gsub("&amp;", "&", gsub("&gt;", ">", gsub("&lt;", "<", t, fixed = TRUE), fixed = TRUE), fixed = TRUE)
  }
  cache_resumes[doi] <<- list(texte)
  Sys.sleep(0.2)
  texte
}

# --- Écriture des fragments -----------------------------------------------------

ecrire <- function(nom, html) {
  con <- file(file.path(dir_gen, nom), open = "wb")
  on.exit(close(con))
  writeLines(enc2utf8(c("```{=html}", html, "```")), con, sep = "\n", useBytes = TRUE)
}

puces_filtre <- function(titre, classe, valeurs) {
  comptes <- sort(table(valeurs), decreasing = TRUE)
  puces <- vapply(names(comptes), function(v) {
    sprintf("<label class=\"puce\"><input type=\"checkbox\" value=\"%s\"> %s <span class=\"puce-n\">%d</span></label>",
            esc(v), esc(v), comptes[[v]])
  }, "")
  sprintf("<fieldset class=\"f-groupe %s\"><legend>%s</legend>%s</fieldset>",
          classe, titre, paste(puces, collapse = ""))
}

liste_filtrable <- function(items, id, indice) {
  annees <- sort(unique(vapply(items, function(x) x$annee, 0L)), decreasing = TRUE)
  options_annee <- paste(sprintf("<option value=\"%d\">%d</option>", annees, annees), collapse = "")
  filtres <- c(
    sprintf("<div class=\"filtres\" data-cible=\"#%s\">", id),
    "<div class=\"f-ligne\">",
    sprintf("<input type=\"search\" class=\"f-texte form-control\" placeholder=\"%s\" aria-label=\"Rechercher\">", indice),
    sprintf("<select class=\"f-annee form-select\" aria-label=\"Année\"><option value=\"\">Toutes les années</option>%s</select>", options_annee),
    "<button type=\"button\" class=\"f-reset btn btn-outline-primary\">Réinitialiser</button>",
    "</div>",
    puces_filtre("Type", "f-types", vapply(items, function(x) x$type, "")),
    puces_filtre("Mots-clés", "f-mots", unlist(lapply(items, function(x) x$mots))),
    "<p class=\"f-compte\" aria-live=\"polite\"></p>",
    "</div>"
  )
  corps <- unlist(lapply(annees, function(a) {
    du <- Filter(function(x) x$annee == a, items)
    c(sprintf("<section class=\"annee\"><h2>%d</h2>", a),
      vapply(du, function(x) x$html, ""), "</section>")
  }))
  c(filtres, sprintf("<div id=\"%s\" class=\"liste\">", id), corps, "</div>")
}

carte_ref <- function(x, lien = NA_character_, resume = NA_character_, date_txt = NULL) {
  meta <- c(
    sprintf("<span class=\"etq etq-type\">%s</span>", esc(x$type)),
    sprintf("<span class=\"etq\">%s</span>", esc(x$mots)[seq_along(x$mots)]),
    if (!is.na(lien)) sprintf("<a class=\"lien-doi\" href=\"%s\">%s</a>", esc(lien), x$lien_txt)
  )
  if (length(x$mots) == 0) meta <- meta[!grepl("^<span class=\"etq\"></span>$", meta)]
  paste0(
    sprintf("<article class=\"item\" data-annee=\"%d\" data-type=\"%s\" data-mots=\"%s\">",
            x$annee, esc(x$type), esc(paste(x$mots, collapse = "|"))),
    sprintf("<p class=\"item-titre\">%s</p>", esc(x$titre)),
    sprintf("<p class=\"item-auteurs\">%s</p>", x$auteurs),
    sprintf("<p class=\"item-lieu\">%s</p>", x$lieu),
    sprintf("<div class=\"item-meta\">%s</div>", paste(meta, collapse = " ")),
    if (!is.na(resume)) sprintf("<details class=\"item-resume\"><summary>Résumé</summary><p>%s</p></details>", esc(resume)),
    "</article>"
  )
}

# --- Publications -----------------------------------------------------------------

TYPES_PUBLI <- c(peerjournal = "Article", book = "Livre", chapter = "Chapitre de livre", proceeding = "Actes")

publis <- lapply(lire_bib(cfg$sources$publications), function(e) {
  k <- trimws(strsplit(e$keywords %||% "", "[,;]")[[1]])
  type <- unname(TYPES_PUBLI[k[k %in% names(TYPES_PUBLI)][1]])
  titre <- champ(e, "title")
  dt <- date_entree(e)
  if (is.na(type) || is.na(titre) || is.na(dt$annee)) {
    avertir("Publication ignorée (type, titre ou année manquant) : ", e$cle)
    return(NULL)
  }
  support <- champ(e, "journal")
  if (is.na(support)) support <- champ(e, "booktitle")
  if (is.na(support)) support <- champ(e, "publisher")
  lieu <- paste0(
    if (!is.na(support)) paste0("<em>", esc(support), "</em>") else "",
    if (!is.na(champ(e, "volume"))) paste0(", ", esc(champ(e, "volume"))) else "",
    if (!is.na(champ(e, "pages"))) paste0(", ", esc(champ(e, "pages"))) else ""
  )
  doi <- champ(e, "doi")
  doi <- if (is.na(doi)) NA_character_ else sub("^https?://(dx\\.)?doi\\.org/", "", doi)
  x <- list(annee = dt$annee, type = type, mots = mots_cles(e), titre = titre,
            auteurs = auteurs_html(e), lieu = lieu, lien_txt = "DOI")
  x$html <- carte_ref(x, if (!is.na(doi)) paste0("https://doi.org/", doi) else NA_character_, resume_doi(doi))
  x
})
publis <- Filter(Negate(is.null), publis)
publis <- publis[order(-vapply(publis, function(x) x$annee, 0L))]

# --- Conférences --------------------------------------------------------------------

TYPES_CONF <- c(oral = "Communication orale", oralabst = "Communication orale",
                poster = "Affiche", posterabst = "Affiche",
                keynote = "Conférence invitée", symposium = "Symposium")

confs <- lapply(lire_bib(cfg$sources$conferences), function(e) {
  k <- tolower(trimws(strsplit(e$keywords %||% "", "[,;]")[[1]]))
  type <- unname(TYPES_CONF[k[k %in% names(TYPES_CONF)][1]])
  titre <- champ(e, "title")
  dt <- date_entree(e)
  if (is.na(type) || is.na(titre) || is.na(dt$annee)) {
    avertir("Conférence ignorée (type, titre ou année manquant) : ", e$cle)
    return(NULL)
  }
  lieu <- champ(e, "howpublished")
  if (is.na(lieu)) lieu <- champ(e, "booktitle")
  if (is.na(lieu)) lieu <- champ(e, "journal")
  if (is.na(lieu)) lieu <- champ(e, "organization")
  lieu <- if (is.na(lieu)) "" else sub("\\.$", "", sub("^\\[[^]]*\\]\\s*", "", lieu))
  x <- list(annee = dt$annee, tri = dt$tri, type = type, mots = mots_cles(e), titre = titre,
            auteurs = auteurs_html(e), lieu = esc(paste0(lieu, if (nzchar(lieu)) " · ", dt$texte)),
            lien_txt = "Lien")
  lien <- champ(e, "url")
  x$html <- carte_ref(x, if (!is.na(lien) && grepl("^https?://", lien)) lien else NA_character_)
  x
})
confs <- Filter(Negate(is.null), confs)
confs <- confs[order(-vapply(confs, function(x) x$tri, 0))]

if (file.exists(fichier_cache) || length(cache_resumes)) {
  write(toJSON(cache_resumes, auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE), fichier_cache)
}

# --- Équipe -------------------------------------------------------------------------

mmaaaa <- function(x) {
  # "09/2025" -> 202509 ; NA si absent ou illisible
  m <- regmatches(x, regexec("^([0-9]{1,2})/([0-9]{4})$", trimws(x)))
  vapply(m, function(p) if (length(p) == 3) as.integer(p[3]) * 100L + as.integer(p[2]) else NA_integer_, 0L)
}

saison <- function(x, fin) {
  # "Été 2025" -> 202505 (début) ou 202508 (fin)
  mois <- if (fin) c(hiver = 4L, ete = 8L, automne = 12L) else c(hiver = 1L, ete = 5L, automne = 9L)
  vapply(x, function(s) {
    p <- strsplit(cle_tri(trimws(s)), " ")[[1]]
    if (length(p) != 2 || !p[1] %in% names(mois)) return(NA_integer_)
    as.integer(p[2]) * 100L + mois[[p[1]]]
  }, 0L, USE.NAMES = FALSE)
}

corriger_nom <- function(nom, prenom) {
  brut <- paste0(nom, ", ", prenom)
  v <- cfg$corrections_noms[[brut]]
  if (is.null(v)) return(c(nom, prenom))
  trimws(strsplit(v, ",", fixed = TRUE)[[1]])
}

sup <- read_excel(cfg$sources$cvdata, sheet = "supervision", col_types = "text")
sup <- sup[, c("lastname", "firstname", "univ", "prog", "level", "date_start", "date_end", "cosupervision", "project", "photo")]
sup <- sup[!is.na(sup$lastname) & !is.na(sup$firstname), ]

ass <- read_excel(cfg$sources$cvdata, sheet = "assistanat", col_types = "text")
ass <- ass[, c("lastname", "firstname", "univ", "level", "date_start", "date_end", "status")]
ass <- ass[!is.na(ass$lastname) & !is.na(ass$firstname) & grepl("recherche", ass$status), ]

personnes <- rbind(
  data.frame(nom = sup$lastname, prenom = sup$firstname, univ = sup$univ, prog = sup$prog, niveau = sup$level,
             debut = mmaaaa(sup$date_start), fin = mmaaaa(sup$date_end), codir = sup$cosupervision,
             projet = sup$project, photo = sup$photo),
  data.frame(nom = ass$lastname, prenom = ass$firstname, univ = ass$univ, prog = ass$status, niveau = "Assistanat",
             debut = saison(ass$date_start, FALSE), fin = saison(ass$date_end, TRUE), codir = NA_character_,
             projet = NA_character_, photo = NA_character_)
)
noms <- t(mapply(corriger_nom, personnes$nom, personnes$prenom, USE.NAMES = FALSE))
personnes$nom <- noms[, 1]
personnes$prenom <- noms[, 2]
personnes$id <- cle_tri(paste(personnes$nom, personnes$prenom))
personnes$actif <- is.na(personnes$fin) | personnes$fin >= aujourd_hui

# Une carte par personne : son lien le plus récent, en préférant la supervision à l'assistanat
dernier_lien <- function(d) {
  d <- d[order(d$actif, d$niveau != "Assistanat", d$fin, d$debut, decreasing = TRUE), ]
  r <- d[1, ]
  r$depuis <- min(d$debut, na.rm = TRUE) %/% 100L
  r$jusqu <- max(d$fin, na.rm = TRUE) %/% 100L
  if (is.na(r$photo) && any(!is.na(d$photo))) r$photo <- d$photo[!is.na(d$photo)][1]
  r
}
membres <- do.call(rbind, lapply(split(personnes, personnes$id), dernier_lien))
membres <- membres[order(cle_tri(membres$nom), cle_tri(membres$prenom)), ]

initiales_nom <- function(prenom, nom) toupper(paste0(substr(prenom, 1, 1), substr(nom, 1, 1)))

carte_membre <- function(m) {
  photo <- if (!is.na(m$photo) && file.exists(file.path("files/profiles", m$photo))) {
    sprintf("<img class=\"avatar\" src=\"files/profiles/%s\" alt=\"Photo de %s %s\">",
            esc(m$photo), esc(m$prenom), esc(m$nom))
  } else {
    sprintf("<div class=\"avatar avatar-initiales\" aria-hidden=\"true\">%s</div>", esc(initiales_nom(m$prenom, m$nom)))
  }
  univ <- if (!is.na(m$univ)) paste0(" · ", esc(m$univ)) else ""
  paste0(
    "<article class=\"membre\">", photo, "<div class=\"membre-corps\">",
    sprintf("<h4>%s %s</h4>", esc(m$prenom), esc(m$nom)),
    sprintf("<p class=\"membre-prog\">%s%s</p>", esc(m$prog), univ),
    if (is.finite(m$depuis)) sprintf("<p class=\"membre-info\">Depuis %d</p>", m$depuis),
    if (!is.na(m$codir)) sprintf("<p class=\"membre-info\">Codirection : %s</p>", esc(m$codir)),
    if (!is.na(m$projet)) sprintf("<details><summary>Projet</summary><p>%s</p></details>", esc(propre(m$projet))),
    "</div></article>"
  )
}

GROUPES <- c(PhD = "Doctorat en recherche", `R/I` = "Doctorat recherche-intervention",
             DPsy = "Doctorat clinique (D.Psy)", Bacc = "Baccalauréat", Med = "Médecine",
             Assistanat = "Assistanat de recherche")

actuels <- membres[membres$actif, ]
html_equipe <- unlist(lapply(names(GROUPES), function(g) {
  d <- actuels[actuels$niveau == g, ]
  if (nrow(d) == 0) return(NULL)
  c(sprintf("<h3>%s</h3>", GROUPES[[g]]), "<div class=\"grille-membres\">",
    vapply(seq_len(nrow(d)), function(i) carte_membre(d[i, ]), ""), "</div>")
}))
inconnus <- setdiff(unique(actuels$niveau), names(GROUPES))
if (length(inconnus)) avertir("Niveau de supervision non affiché sur la page Équipe : ", paste(inconnus, collapse = ", "))
ecrire("equipe.md", html_equipe)

anciens <- membres[!membres$actif, ]
anciens <- anciens[order(-anciens$jusqu, cle_tri(anciens$nom)), ]
ligne_ancien <- vapply(seq_len(nrow(anciens)), function(i) {
  m <- anciens[i, ]
  periode <- if (is.finite(m$depuis) && m$depuis != m$jusqu) paste0(m$depuis, "–", m$jusqu) else as.character(m$jusqu)
  projet <- if (!is.na(m$projet)) sprintf("<br><small>%s</small>", esc(propre(m$projet))) else ""
  sprintf("<tr><td>%s %s</td><td>%s%s</td><td>%s</td><td>%s</td></tr>",
          esc(m$prenom), esc(m$nom), esc(m$prog), projet, esc(m$univ %||% ""), periode)
}, "")
ecrire("alumni.md", c(
  "<div class=\"table-responsive\"><table class=\"table table-alumni\">",
  "<thead><tr><th>Nom</th><th>Formation</th><th>Université</th><th>Période</th></tr></thead><tbody>",
  ligne_ancien, "</tbody></table></div>"
))

# Direction du laboratoire
postes <- read_excel(cfg$sources$cvdata, sheet = "now", col_types = "text")
formation <- read_excel(cfg$sources$cvdata, sheet = "education", col_types = "text")
ligne_cv <- function(d) {
  vapply(seq_len(nrow(d)), function(i) {
    lieu <- d$with[i]
    sprintf("<li><strong>%s</strong>%s <span class=\"cv-date\">%s</span></li>",
            esc(d$what[i]), if (!is.na(lieu)) paste0(", ", esc(lieu)) else "", esc(d$when[i] %||% ""))
  }, "")
}
ecrire("direction.md", c(
  "<div class=\"cv-colonnes\"><div><h3>Postes actuels</h3><ul class=\"cv-liste\">", ligne_cv(postes), "</ul></div>",
  "<div><h3>Formation et parcours</h3><ul class=\"cv-liste\">", ligne_cv(formation), "</ul></div></div>"
))

# --- Projets (subventions obtenues uniquement) -----------------------------------------

sub <- read_excel(cfg$sources$cvdata, sheet = "subvention", col_types = "text")
sub <- sub[, c("year", "code", "orga", "title", "when", "resp", "with", "where", "how", "type", "win", "status")]
sub <- sub[!is.na(sub$win) & sub$win == "1" & !is.na(sub$status) & sub$status == "O", ]
sans_titre <- is.na(sub$title)
if (any(sans_titre)) avertir("Projet sans titre ignoré (onglet subvention) : ", paste(sub$orga[sans_titre], sub$year[sans_titre], collapse = " ; "))
sub <- sub[!sans_titre, ]

fin_projet <- function(when, annee) {
  m <- regmatches(when, gregexpr("[0-9]{2}/[0-9]{4}", when))[[1]]
  if (length(m)) mmaaaa(m[length(m)]) else NA_integer_
}
sub$fin <- mapply(fin_projet, ifelse(is.na(sub$when), "", sub$when), sub$year)
# Sans date de fin connue, un projet est considéré en cours pendant trois ans
sub$fin[is.na(sub$fin)] <- (as.integer(sub$year[is.na(sub$fin)]) + 3L) * 100L + 12L
sub$en_cours <- sub$fin >= aujourd_hui
sub <- sub[order(-as.integer(sub$year), sub$type != "pi", cle_tri(sub$title)), ]

carte_projet <- function(p) {
  resp <- propre(strsplit(p$resp %||% "", "\n", fixed = TRUE)[[1]][1])
  montant <- if (isTRUE(cfg$afficher_montants) && !is.na(p$how)) {
    sprintf("<p class=\"projet-info\">Montant : %s $</p>", format(as.numeric(p$how), big.mark = " ", scientific = FALSE))
  } else ""
  paste0(
    "<article class=\"projet\">",
    sprintf("<div class=\"projet-tete\"><span class=\"etq etq-type\">%s</span><span class=\"projet-date\">%s</span></div>",
            esc(p$code %||% ""), esc(p$when %||% p$year)),
    sprintf("<h3>%s</h3>", esc(propre(p$title))),
    sprintf("<p class=\"projet-info\"><strong>Financement :</strong> %s%s</p>", esc(propre(p$orga)),
            if (!is.na(p$where)) paste0(" — ", esc(propre(p$where))) else ""),
    if (!is.na(resp) && nzchar(resp)) sprintf("<p class=\"projet-info\"><strong>Responsable :</strong> %s</p>", esc(resp)),
    if (!is.na(p$with)) sprintf("<p class=\"projet-info\"><strong>Équipe :</strong> %s</p>", esc(propre(p$with))),
    montant, "</article>"
  )
}
cartes_projets <- function(d) {
  c("<div class=\"grille-projets\">", vapply(seq_len(nrow(d)), function(i) carte_projet(d[i, ]), ""), "</div>")
}
ecrire("projets-en-cours.md", cartes_projets(sub[sub$en_cours, ]))
ecrire("projets-termines.md", cartes_projets(sub[!sub$en_cours, ]))

# --- Cours -----------------------------------------------------------------------------

cours <- read_excel(cfg$sources$cvdata, sheet = "teaching_uqtr", col_types = "text")
cours <- cours[, c("sigle", "level", "name", "start", "end", "priority", "hours")]
cours <- cours[!is.na(cours$sigle), ]
cours <- cours[order(as.integer(cours$priority)), ]
cours$actif <- is.na(cours$end) | as.integer(cours$end) >= as.integer(format(Sys.Date(), "%Y"))
cours$cycle <- ifelse(grepl("^doct", cours$level), "Doctorat", "Baccalauréat")

carte_cours <- function(k) {
  note <- regmatches(k$name, regexpr("\\[[^]]+\\]", k$name))
  nom <- trimws(gsub("\\[[^]]+\\]", "", k$name))
  periode <- if (k$actif) paste0("Depuis ", k$start) else if (k$start == k$end) k$start else paste0(k$start, "–", k$end)
  paste0(
    "<article class=\"cours\">",
    sprintf("<div class=\"cours-sigle\">%s</div><div class=\"cours-corps\"><h4>%s</h4>", esc(k$sigle), esc(nom)),
    sprintf("<p>%s · %s h · %s%s</p></div></article>", k$cycle, esc(k$hours %||% "–"), periode,
            if (length(note)) paste0(" · ", esc(gsub("[][]", "", note))) else "")
  )
}
bloc_cours <- function(d) {
  if (nrow(d) == 0) return("<p>Aucun cours pour le moment.</p>")
  c("<div class=\"liste-cours\">", vapply(seq_len(nrow(d)), function(i) carte_cours(d[i, ]), ""), "</div>")
}
ecrire("cours-actuels.md", bloc_cours(cours[cours$actif, ]))
ecrire("cours-passes.md", bloc_cours(cours[!cours$actif, ]))

passe <- read_excel(cfg$sources$cvdata, sheet = "past_teaching", col_types = "text")
ecrire("cours-experience.md", c(
  "<div class=\"cv-colonnes\">",
  unlist(lapply(names(passe), function(n) {
    v <- passe[[n]][!is.na(passe[[n]])]
    c(sprintf("<div><h3>%s</h3><ul class=\"cv-liste\">", esc(n)), sprintf("<li>%s</li>", esc(v)), "</ul></div>")
  })),
  "</div>"
))

# --- Pages Publications et Conférences, et accueil ------------------------------------------

ecrire("publications.md", liste_filtrable(publis, "liste-publications", "Titre, auteur, revue…"))
ecrire("conferences.md", liste_filtrable(confs, "liste-conferences", "Titre, auteur, congrès, ville…"))

n_articles <- sum(vapply(publis, function(x) x$type == "Article", TRUE))
chiffres <- list(
  c(length(publis), "publications", "publications.qmd"),
  c(length(confs), "présentations en congrès", "conferences.qmd"),
  c(sum(sub$en_cours), "projets financés en cours", "projets.qmd"),
  c(nrow(actuels), "personnes étudiantes et assistantes", "equipe.qmd")
)
ecrire("chiffres.md", c(
  "<div class=\"chiffres\">",
  vapply(chiffres, function(c) sprintf("<a class=\"chiffre\" href=\"%s\"><span class=\"chiffre-n\">%s</span><span>%s</span></a>",
                                       sub("qmd$", "html", c[3]), c[1], c[2]), ""),
  "</div>"
))
ecrire("recentes.md", c(
  "<div class=\"liste\">",
  vapply(publis[seq_len(min(4L, length(publis)))], function(x) gsub("<details.*</details>", "", x$html), ""),
  "</div>"
))

if (length(avertissements)) {
  message("--- À vérifier dans les données ---")
  message(paste0(" - ", unique(avertissements), collapse = "\n"))
}
message(sprintf("Site : %d publications (%d articles), %d conférences, %d projets, %d membres actuels, %d anciens, %d cours.",
                length(publis), n_articles, length(confs), nrow(sub), nrow(actuels), nrow(anciens), nrow(cours)))
