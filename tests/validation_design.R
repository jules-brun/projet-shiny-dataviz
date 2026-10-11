# Structure noire de référence conservée : quatre pages, ordre et sorties.
options(sass.cache = FALSE)
Sys.setenv(TZ = "Europe/Paris")
invisible(capture.output(source("server.R")))
interface <- source("ui.R")$value
html <- as.character(interface)
pages <- c("donnees", "manquantes", "acm", "machine_learning")
positions <- vapply(pages, function(v) regexpr(paste0('data-value="', v, '"'), html, fixed = TRUE)[1], integer(1))
stopifnot(all(positions > 0), identical(order(positions), seq_along(pages)),
  !grepl('data-value="overview"|data-value="relations"|data-value="apropos"', html),
  identical(attr(interface, "lang"), "fr"),
  identical(palette_heart$bg, "#080F1B"),
  identical(theme_heart()$plot.background$fill, palette_heart$card))
ids <- c("effectifs", "distribution", "na_cumules", "na_detail",
  "perte_na_graphique", "pipeline_benchmark", "benchmark_erreurs", "choix_modeles",
  "calculer_imputation", "acm_valeurs_propres", "axe_x", "axe_y", "acm_modalites", "acm_individus")
stopifnot(all(vapply(ids, function(id) grepl(paste0('id="', id, '"'), html, fixed = TRUE), logical(1))),
  !grepl("variabilité d'imputation (95 %)", html, fixed = TRUE), !grepl('id="ml_calibration"', html, fixed = TRUE))
# La page n'ajoute aucun plotOutput/plotlyOutput à la référence noire.
sorties <- function(texte) {
  motifs <- gregexpr('(plotOutput|plotlyOutput)\\("[^"]+"', texte, perl = TRUE)
  sort(regmatches(texte, motifs)[[1]])
}
reference <- system2("git", c("show", "main:ui.R"), stdout = TRUE)
stopifnot(identical(sorties(paste(readLines("ui.R"), collapse = "\n")),
                    sorties(paste(reference, collapse = "\n"))))
# Construction complète de l'objet Shiny, y compris les sources de l'app.
stopifnot(inherits(source("app.R")$value, "shiny.appobj"))
cat("Validation design : thème noir, quatre pages dans le même ordre, sorties existantes et objet Shiny.\n")
