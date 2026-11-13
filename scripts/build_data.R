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


# --- Langues ----------------------------------------------------------------------
# Chaque fragment est produit en français (_generated/x.md) et en anglais (_generated/x-en.md).
# Les données elles-mêmes (titres, programmes, cours) restent dans leur langue d'origine.

LANGUES <- c("fr", "en")

TXT <- list(
  fr = list(rechercher = "Rechercher", annee = "Année", toutes = "Toutes les années", reinit = "Réinitialiser",
            type = "Type", mots = "Mots-clés", resume = "Résumé", lien = "Lien",
            indice_pub = "Titre, auteur, revue…", indice_conf = "Titre, auteur, congrès, ville…",
            depuis = "Depuis", codir = "Codirection", projet = "Projet", photo = "Photo de",
            membres = "Membres", role = "Rôle", debut = "Début", fin = "Fin",
            financement = "Financement", responsable = "Responsable", equipe = "Équipe", montant = "Montant",
            aucun_cours = "Aucun cours pour le moment.", doctorat = "Doctorat", bacc = "Baccalauréat",
            chiffres = c("publications", "présentations en congrès", "projets financés en cours",
                         "personnes étudiantes et assistantes"),
            sd = "s.d.", mois = c("janvier", "février", "mars", "avril", "mai", "juin", "juillet",
                                  "août", "septembre", "octobre", "novembre", "décembre")),
  en = list(rechercher = "Search", annee = "Year", toutes = "All years", reinit = "Reset",
            type = "Type", mots = "Keywords", resume = "Abstract", lien = "Link",
            indice_pub = "Title, author, journal…", indice_conf = "Title, author, conference, city…",
            depuis = "Since", codir = "Co-supervisor", projet = "Project", photo = "Photo of",
            membres = "Members", role = "Role", debut = "Start", fin = "End",
            financement = "Funding", responsable = "Lead", equipe = "Team", montant = "Amount",
            aucun_cours = "No courses at the moment.", doctorat = "Doctoral", bacc = "Undergraduate",
            chiffres = c("publications", "conference presentations", "funded projects in progress",
                         "students and assistants"),
            sd = "n.d.", mois = c("January", "February", "March", "April", "May", "June", "July",
                                  "August", "September", "October", "November", "December"))
)

TYPES_PUBLI <- list(peerjournal = c("Article", "Article"), book = c("Livre", "Book"),
                    chapter = c("Chapitre de livre", "Book chapter"), proceeding = c("Actes", "Proceedings"))
TYPES_CONF <- list(oral = c("Communication orale", "Oral presentation"),
                   oralabst = c("Communication orale", "Oral presentation"),
                   poster = c("Affiche", "Poster"), posterabst = c("Affiche", "Poster"),
                   keynote = c("Conférence invitée", "Invited talk"), symposium = c("Symposium", "Symposium"))
GROUPES <- list(PhD = c("Doctorat en recherche", "Research PhD"),
                `R/I` = c("Doctorat recherche-intervention", "Research-intervention doctorate"),
                DPsy = c("Doctorat clinique (D.Psy)", "Clinical doctorate (D.Psy)"),
                Bacc = c("Baccalauréat", "Undergraduate"), Med = c("Médecine", "Medicine"),
                Assistanat = c("Assistanat de recherche", "Research assistants"))

idx <- function(lg) if (lg == "fr") 1L else 2L
suffixe <- function(lg) if (lg == "fr") "" else "-en"
# Les pages anglaises sont dans docs/en/ : les fichiers partagés sont un dossier plus haut
racine <- function(lg) if (lg == "fr") "" else "../"

ROLES_EN <- c("^Étudiant-e" = "Student", "^Assistante? de recherche" = "Research assistant",
              "^PhD recherche" = "Research PhD",
              "^Doctorat clinique en neuropsychologie" = "Clinical doctorate in neuropsychology",
              "^Doctorat recherche/intervention en neuropsychologie" = "Research-intervention doctorate in neuropsychology",
              "^Doctorat en sciences de la réadaptation" = "PhD in rehabilitation sciences",
              "^Doctorat en psychologie cognitive" = "PhD in cognitive psychology",
              "^Activité de recherche, 3e année de Bac\\." = "Undergraduate research project (3rd year)")

traduire_role <- function(x, lg) {
  if (lg == "fr") return(x)
  for (motif in names(ROLES_EN)) x <- sub(motif, ROLES_EN[[motif]], x)
  x
}

# --- Mots-clés et dates -------------------------------------------------------------

# Dans sources.yml, chaque mot-clé vaut [français, anglais] ; une valeur seule sert aux deux langues
libelle_mc <- function(v) if (length(v) >= 2) unlist(v[1:2]) else rep(unlist(v), 2)
MC_EN <- list()

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
      lib <- libelle_mc(table_mc[[k]])
      MC_EN[[lib[1]]] <<- lib[2]
      res <- c(res, lib[1])
    }
  }
  unique(res)
}

mots_langue <- function(mots, lg) {
  if (lg == "fr" || length(mots) == 0) return(mots)
  vapply(mots, function(m) MC_EN[[m]] %||% m, "", USE.NAMES = FALSE)
}

MOIS_COURTS <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

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
    mois <- which(vapply(MOIS_COURTS, function(k) grepl(k, txt, fixed = TRUE), TRUE))[1]
    jour <- NA_integer_
    jour2 <- NA_integer_
  }
  list(annee = annee, mois = mois, jour = jour, jour2 = jour2,
       tri = (annee %||% 0L) * 10000L + (mois %||% 0L) * 100L + (jour %||% 0L))
}

texte_date <- function(dt, lg) {
  if (is.na(dt$annee)) return(TXT[[lg]]$sd)
  if (is.na(dt$mois)) return(as.character(dt$annee))
  nom_mois <- TXT[[lg]]$mois[dt$mois]
  jours <- if (is.na(dt$jour)) "" else if (is.na(dt$jour2) || dt$jour2 == dt$jour) {
    if (lg == "fr" && dt$jour == 1L) "1er" else as.character(dt$jour)
  } else paste0(dt$jour, "–", dt$jour2)
  if (!nzchar(jours)) return(paste(nom_mois, dt$annee))
  if (lg == "fr") paste(jours, nom_mois, dt$annee) else paste0(nom_mois, " ", jours, ", ", dt$annee)
}

# --- Résumés (récupérés sur CrossRef, conservés dans un cache) --------------------

cache_resumes <- if (file.exists(fichier_cache)) {
  fromJSON(fichier_cache, simplifyVector = FALSE)
} else list()

decoder_entites <- function(t) {
  repeat {
    avant <- t
    t <- gsub("&amp;", "&", t, fixed = TRUE)
    if (identical(t, avant)) break
  }
  gsub("&gt;", ">", gsub("&lt;", "<", t, fixed = TRUE), fixed = TRUE)
}

resume_doi <- function(doi) {
  if (is.na(doi)) return(NA_character_)
  if (doi %in% names(cache_resumes)) {
    v <- cache_resumes[[doi]]
    return(if (is.null(v)) NA_character_ else decoder_entites(v))
  }
  options(timeout = 8)
  url <- paste0("https://api.crossref.org/works/", URLencode(doi, reserved = TRUE))
  rep <- tryCatch(fromJSON(url), error = function(e) NULL)
  if (is.null(rep)) return(NA_character_)
  brut <- rep$message$abstract
  texte <- if (is.null(brut)) NA_character_ else decoder_entites(propre(gsub("<[^>]+>", " ", brut)))
  cache_resumes[doi] <<- list(texte)
  Sys.sleep(0.2)
  texte
}

# --- Écriture des fragments -----------------------------------------------------

ecrire <- function(nom, html, lg = "fr") {
  nom <- sub("\\.md$", paste0(suffixe(lg), ".md"), nom)
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

tableau_synthese <- function(colonnes, numeriques, lignes) {
  entetes <- sprintf("<th%s>%s</th>", ifelse(numeriques, " data-tri=\"num\"", ""), colonnes)
  c("<div class=\"table-responsive\"><table class=\"table table-synthese tableau-tri\" data-lignes=\"10\">",
    "<thead><tr>", entetes, "</tr></thead><tbody>", lignes, "</tbody></table></div>")
}

carte_ref <- function(x, lg) {
  t <- TXT[[lg]]
  type <- x$type[idx(lg)]
  mots <- mots_langue(x$mots, lg)
  meta <- c(
    sprintf("<span class=\"etq etq-type\">%s</span>", esc(type)),
    if (length(mots)) sprintf("<span class=\"etq\">%s</span>", esc(mots)),
    if (!is.na(x$lien)) sprintf("<a class=\"lien-doi\" href=\"%s\">%s</a>", esc(x$lien), x$lien_txt %||% t$lien)
  )
  lieu <- if (is.null(x$date)) x$lieu else esc(paste0(x$lieu, if (nzchar(x$lieu)) " · ", texte_date(x$date, lg)))
  paste0(
    sprintf("<article class=\"item\" data-annee=\"%d\" data-type=\"%s\" data-mots=\"%s\">",
            x$annee, esc(type), esc(paste(mots, collapse = "|"))),
    sprintf("<p class=\"item-titre\">%s</p>", esc(x$titre)),
    sprintf("<p class=\"item-auteurs\">%s</p>", x$auteurs),
    sprintf("<p class=\"item-lieu\">%s</p>", lieu),
    sprintf("<div class=\"item-meta\">%s</div>", paste(meta, collapse = " ")),
    if (!is.na(x$resume)) sprintf("<details class=\"item-resume\"><summary>%s</summary><p>%s</p></details>", t$resume, esc(x$resume)),
    "</article>"
  )
}

liste_filtrable <- function(items, id, indice, lg) {
  t <- TXT[[lg]]
  annees <- sort(unique(vapply(items, function(x) x$annee, 0L)), decreasing = TRUE)
  options_annee <- paste(sprintf("<option value=\"%d\">%d</option>", annees, annees), collapse = "")
  filtres <- c(
    sprintf("<div class=\"filtres\" data-cible=\"#%s\">", id),
    "<div class=\"f-ligne\">",
    sprintf("<input type=\"search\" class=\"f-texte form-control\" placeholder=\"%s\" aria-label=\"%s\">", indice, t$rechercher),
    sprintf("<select class=\"f-annee form-select\" aria-label=\"%s\"><option value=\"\">%s</option>%s</select>",
            t$annee, t$toutes, options_annee),
    sprintf("<button type=\"button\" class=\"f-reset btn btn-outline-primary\">%s</button>", t$reinit),
    "</div>",
    puces_filtre(t$type, "f-types", vapply(items, function(x) x$type[idx(lg)], "")),
    puces_filtre(t$mots, "f-mots", unlist(lapply(items, function(x) mots_langue(x$mots, lg)))),
    "<p class=\"f-compte\" aria-live=\"polite\"></p>",
    "</div>"
  )
  corps <- unlist(lapply(annees, function(a) {
    du <- Filter(function(x) x$annee == a, items)
    c(sprintf("<section class=\"annee\"><h2>%d</h2>", a),
      vapply(du, function(x) carte_ref(x, lg), ""), "</section>")
  }))
  c(filtres, sprintf("<div id=\"%s\" class=\"liste\">", id), corps, "</div>")
}

# --- Publications -----------------------------------------------------------------

publis <- lapply(lire_bib(cfg$sources$publications), function(e) {
  k <- trimws(strsplit(e$keywords %||% "", "[,;]")[[1]])
  cle_type <- k[k %in% names(TYPES_PUBLI)][1]
  titre <- champ(e, "title")
  dt <- date_entree(e)
  if (is.na(cle_type) || is.na(titre) || is.na(dt$annee)) {
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
  list(annee = dt$annee, type = TYPES_PUBLI[[cle_type]], mots = mots_cles(e), titre = titre,
       auteurs = auteurs_html(e), lieu = lieu, lien_txt = "DOI",
       lien = if (!is.na(doi)) paste0("https://doi.org/", doi) else NA_character_, resume = resume_doi(doi))
})
publis <- Filter(Negate(is.null), publis)
publis <- publis[order(-vapply(publis, function(x) x$annee, 0L))]

# --- Conférences --------------------------------------------------------------------

confs <- lapply(lire_bib(cfg$sources$conferences), function(e) {
  k <- tolower(trimws(strsplit(e$keywords %||% "", "[,;]")[[1]]))
  cle_type <- k[k %in% names(TYPES_CONF)][1]
  titre <- champ(e, "title")
  dt <- date_entree(e)
  if (is.na(cle_type) || is.na(titre) || is.na(dt$annee)) {
    avertir("Conférence ignorée (type, titre ou année manquant) : ", e$cle)
    return(NULL)
  }
  lieu <- champ(e, "howpublished")
  if (is.na(lieu)) lieu <- champ(e, "booktitle")
  if (is.na(lieu)) lieu <- champ(e, "journal")
  if (is.na(lieu)) lieu <- champ(e, "organization")
  lieu <- if (is.na(lieu)) "" else sub("\\.$", "", sub("^\\[[^]]*\\]\\s*", "", lieu))
  lien <- champ(e, "url")
  list(annee = dt$annee, tri = dt$tri, date = dt, type = TYPES_CONF[[cle_type]], mots = mots_cles(e),
       titre = titre, auteurs = auteurs_html(e), lieu = lieu, resume = NA_character_,
       lien = if (!is.na(lien) && grepl("^https?://", lien)) lien else NA_character_)
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
sup <- sup[, c("lastname", "firstname", "univ", "prog", "level", "status", "date_start", "date_end", "cosupervision", "project", "photo")]
sup <- sup[!is.na(sup$lastname) & !is.na(sup$firstname), ]

ass <- read_excel(cfg$sources$cvdata, sheet = "assistanat", col_types = "text")
ass <- ass[, c("lastname", "firstname", "univ", "level", "date_start", "date_end", "status")]
ass <- ass[!is.na(ass$lastname) & !is.na(ass$firstname) & grepl("recherche", ass$status), ]

personnes <- rbind(
  data.frame(nom = sup$lastname, prenom = sup$firstname, univ = sup$univ, prog = sup$prog, niveau = sup$level,
             debut = mmaaaa(sup$date_start), fin = mmaaaa(sup$date_end), codir = sup$cosupervision,
             projet = sup$project, photo = sup$photo, role = paste0(sup$status, " (", sup$level, ")")),
  data.frame(nom = ass$lastname, prenom = ass$firstname, univ = ass$univ, prog = ass$status, niveau = "Assistanat",
             debut = saison(ass$date_start, FALSE), fin = saison(ass$date_end, TRUE), codir = NA_character_,
             projet = NA_character_, photo = NA_character_, role = paste0(ass$status, " (", ass$level, ")"))
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
actuels <- membres[membres$actif, ]
anciens <- membres[!membres$actif, ]
anciens <- anciens[order(-anciens$jusqu, cle_tri(anciens$nom)), ]

inconnus <- setdiff(unique(actuels$niveau), names(GROUPES))
if (length(inconnus)) avertir("Niveau de supervision non affiché sur la page Équipe : ", paste(inconnus, collapse = ", "))

initiales_nom <- function(prenom, nom) toupper(paste0(substr(prenom, 1, 1), substr(nom, 1, 1)))

carte_membre <- function(m, lg) {
  t <- TXT[[lg]]
  photo <- if (!is.na(m$photo) && file.exists(file.path("files/profiles", m$photo))) {
    sprintf("<img class=\"avatar\" src=\"%sfiles/profiles/%s\" alt=\"%s %s %s\">",
            racine(lg), esc(m$photo), t$photo, esc(m$prenom), esc(m$nom))
  } else {
    sprintf("<div class=\"avatar avatar-initiales\" aria-hidden=\"true\">%s</div>", esc(initiales_nom(m$prenom, m$nom)))
  }
  univ <- if (!is.na(m$univ)) paste0(" · ", esc(m$univ)) else ""
  paste0(
    "<article class=\"membre\">", photo, "<div class=\"membre-corps\">",
    sprintf("<h4>%s %s</h4>", esc(m$prenom), esc(m$nom)),
    sprintf("<p class=\"membre-prog\">%s%s</p>", esc(traduire_role(m$prog, lg)), univ),
    if (is.finite(m$depuis)) sprintf("<p class=\"membre-info\">%s %d</p>", t$depuis, m$depuis),
    if (!is.na(m$codir)) sprintf("<p class=\"membre-info\">%s : %s</p>", t$codir, esc(m$codir)),
    if (!is.na(m$projet)) sprintf("<details><summary>%s</summary><p>%s</p></details>", t$projet, esc(propre(m$projet))),
    "</div></article>"
  )
}

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
sub$annee_debut <- mapply(function(when, annee) {
  m <- regmatches(when, regexpr("[0-9]{4}", when))
  if (length(m)) m else annee
}, ifelse(is.na(sub$when), "", sub$when), sub$year, USE.NAMES = FALSE)
sub <- sub[order(-as.integer(sub$year), sub$type != "pi", cle_tri(sub$title)), ]
termines <- sub[!sub$en_cours, ]
termines <- termines[order(-(termines$fin %/% 100L), cle_tri(termines$title)), ]

responsable <- function(p) propre(strsplit(p$resp %||% "", "\n", fixed = TRUE)[[1]][1])

carte_projet <- function(p, lg) {
  t <- TXT[[lg]]
  resp <- responsable(p)
  montant <- if (isTRUE(cfg$afficher_montants) && !is.na(p$how)) {
    sprintf("<p class=\"projet-info\">%s : %s $</p>", t$montant, format(as.numeric(p$how), big.mark = " ", scientific = FALSE))
  } else ""
  paste0(
    "<article class=\"projet\">",
    sprintf("<div class=\"projet-tete\"><span class=\"etq etq-type\">%s</span><span class=\"projet-date\">%s</span></div>",
            esc(p$code %||% ""), esc(p$when %||% p$year)),
    sprintf("<h3>%s</h3>", esc(propre(p$title))),
    sprintf("<p class=\"projet-info\"><strong>%s :</strong> %s%s</p>", t$financement, esc(propre(p$orga)),
            if (!is.na(p$where)) paste0(" — ", esc(propre(p$where))) else ""),
    if (!is.na(resp) && nzchar(resp)) sprintf("<p class=\"projet-info\"><strong>%s :</strong> %s</p>", t$responsable, esc(resp)),
    if (!is.na(p$with)) sprintf("<p class=\"projet-info\"><strong>%s :</strong> %s</p>", t$equipe, esc(propre(p$with))),
    montant, "</article>"
  )
}

# --- Cours -----------------------------------------------------------------------------

cours <- read_excel(cfg$sources$cvdata, sheet = "teaching_uqtr", col_types = "text")
cours <- cours[, c("sigle", "level", "name", "start", "end", "priority", "hours")]
cours <- cours[!is.na(cours$sigle), ]
cours <- cours[order(as.integer(cours$priority)), ]
cours$actif <- is.na(cours$end) | as.integer(cours$end) >= as.integer(format(Sys.Date(), "%Y"))
cours$doctorat <- grepl("^doct", cours$level)
passe <- read_excel(cfg$sources$cvdata, sheet = "past_teaching", col_types = "text")

carte_cours <- function(k, lg) {
  t <- TXT[[lg]]
  note <- regmatches(k$name, regexpr("\\[[^]]+\\]", k$name))
  nom <- trimws(gsub("\\[[^]]+\\]", "", k$name))
  periode <- if (k$actif) paste(t$depuis, k$start) else if (k$start == k$end) k$start else paste0(k$start, "–", k$end)
  paste0(
    "<article class=\"cours\">",
    sprintf("<div class=\"cours-sigle\">%s</div><div class=\"cours-corps\"><h4>%s</h4>", esc(k$sigle), esc(nom)),
    sprintf("<p>%s · %s h · %s%s</p></div></article>", if (k$doctorat) t$doctorat else t$bacc,
            esc(k$hours %||% "–"), periode, if (length(note)) paste0(" · ", esc(gsub("[][]", "", note))) else "")
  )
}
bloc_cours <- function(d, lg) {
  if (nrow(d) == 0) return(sprintf("<p>%s</p>", TXT[[lg]]$aucun_cours))
  c("<div class=\"liste-cours\">", vapply(seq_len(nrow(d)), function(i) carte_cours(d[i, ], lg), ""), "</div>")
}

# La page Cours n'apparaît que si afficher_cours vaut true dans sources.yml (brouillon sinon)
con <- file(file.path(dir_gen, "cours-meta.yml"), open = "wb")
writeLines(sprintf("draft: %s", if (isTRUE(cfg$afficher_cours)) "false" else "true"), con)
close(con)

# --- Génération dans chaque langue ---------------------------------------------------------

for (lg in LANGUES) {
  t <- TXT[[lg]]

  ecrire("publications.md", liste_filtrable(publis, "liste-publications", t$indice_pub, lg), lg)
  ecrire("conferences.md", liste_filtrable(confs, "liste-conferences", t$indice_conf, lg), lg)

  ecrire("equipe.md", unlist(lapply(names(GROUPES), function(g) {
    d <- actuels[actuels$niveau == g, ]
    if (nrow(d) == 0) return(NULL)
    c(sprintf("<h3>%s</h3>", GROUPES[[g]][idx(lg)]), "<div class=\"grille-membres\">",
      vapply(seq_len(nrow(d)), function(i) carte_membre(d[i, ], lg), ""), "</div>")
  })), lg)

  ecrire("alumni.md", tableau_synthese(
    c(t$membres, t$role, t$debut, t$fin), c(FALSE, FALSE, TRUE, TRUE),
    vapply(seq_len(nrow(anciens)), function(i) {
      m <- anciens[i, ]
      debut <- if (is.finite(m$depuis)) m$depuis else ""
      sprintf("<tr><td>%s, %s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
              esc(toupper(m$nom)), esc(m$prenom), esc(traduire_role(m$role, lg)), debut, m$jusqu)
    }, "")
  ), lg)

  en_cours <- sub[sub$en_cours, ]
  ecrire("projets-en-cours.md", c("<div class=\"grille-projets\">",
    vapply(seq_len(nrow(en_cours)), function(i) carte_projet(en_cours[i, ], lg), ""), "</div>"), lg)
  ecrire("projets-termines.md", tableau_synthese(
    c(t$projet, t$financement, t$responsable, t$debut, t$fin), c(FALSE, FALSE, FALSE, TRUE, TRUE),
    vapply(seq_len(nrow(termines)), function(i) {
      p <- termines[i, ]
      sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%d</td></tr>",
              esc(propre(p$title)), esc(propre(p$orga)), esc(responsable(p)), esc(p$annee_debut), p$fin %/% 100L)
    }, "")
  ), lg)

  ecrire("cours-actuels.md", bloc_cours(cours[cours$actif, ], lg), lg)
  ecrire("cours-passes.md", bloc_cours(cours[!cours$actif, ], lg), lg)
  ecrire("cours-experience.md", c(
    "<div class=\"cv-colonnes\">",
    unlist(lapply(names(passe), function(n) {
      v <- passe[[n]][!is.na(passe[[n]])]
      c(sprintf("<div><h3>%s</h3><ul class=\"cv-liste\">", esc(n)), sprintf("<li>%s</li>", esc(v)), "</ul></div>")
    })),
    "</div>"
  ), lg)

  valeurs <- c(length(publis), length(confs), sum(sub$en_cours), nrow(actuels))
  pages <- c("publications.html", "conferences.html", "projets.html", "equipe.html")
  ecrire("chiffres.md", c(
    "<div class=\"chiffres\">",
    sprintf("<a class=\"chiffre\" href=\"%s\"><span class=\"chiffre-n\">%d</span><span>%s</span></a>",
            pages, valeurs, t$chiffres),
    "</div>"
  ), lg)
  ecrire("recentes.md", c(
    "<div class=\"liste\">",
    vapply(publis[seq_len(min(4L, length(publis)))], function(x) gsub("<details.*</details>", "", carte_ref(x, lg)), ""),
    "</div>"
  ), lg)
}

unlink(file.path(dir_gen, "direction.md"))

if (length(avertissements)) {
  message("--- À vérifier dans les données ---")
  message(paste0(" - ", unique(avertissements), collapse = "\n"))
}
n_articles <- sum(vapply(publis, function(x) x$type[1] == "Article", TRUE))
message(sprintf("Site : %d publications (%d articles), %d conférences, %d projets, %d membres actuels, %d anciens, %d cours (page %s).",
                length(publis), n_articles, length(confs), nrow(sub), nrow(actuels), nrow(anciens), nrow(cours),
                if (isTRUE(cfg$afficher_cours)) "affichée" else "masquée"))
