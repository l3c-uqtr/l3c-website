# Compile la version anglaise (sous-projet en/) dans docs/en après chaque compilation complète.
# Une compilation complète de la version française vide docs/, d'où ce second passage.

if (Sys.getenv("QUARTO_PROJECT_RENDER_ALL") != "1") quit(save = "no", status = 0)

quarto <- file.path(Sys.getenv("QUARTO_BIN_PATH"), "quarto.exe")
if (!file.exists(quarto)) quarto <- Sys.which("quarto")
if (!nzchar(quarto)) quarto <- "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe"

message("Compilation de la version anglaise…")
statut <- system2(quarto, c("render", shQuote("en")))
if (statut != 0) stop("La compilation de la version anglaise a échoué.")
