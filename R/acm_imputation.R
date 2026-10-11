# Apport conceptuel de Jules : cas complets / ACM régularisée / MIMCA.
# Ce module complète le moteur existant de l'application noire, sans nouveau graphe.
acm_variables_lourdes <- c("nb_vaisseaux", "test_thallium")


acm_avec_graine <- function(graine, calcul) {
  existait <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (existait) ancien <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (existait) assign(".Random.seed", ancien, envir = .GlobalEnv) else
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv))
  set.seed(graine)
  force(calcul)
}

calculer_acm_conditionnelle <- function(donnees, categories = variables_acm_app,
  mode = "complets", avec_lourdes = TRUE, nboot = 20L,
  graine = 20261011L, ncp_max = 5L, nbsim = 10L) acm_avec_graine(graine, {
  stopifnot(mode %in% c("complets", "simple", "multiple"),
    nboot %in% 3:100, ncp_max >= 1L, nbsim >= 1L)
  variables <- intersect(categories, variables_acm_app)
  if (!avec_lourdes) variables <- setdiff(variables, acm_variables_lourdes)
  if (!all(c(variables, "diagnostic", "provenance") %in% names(donnees)))
    stop("Variables requises absentes pour l’ACM.")
  # Ni diagnostic ni métadonnée technique dans les entrées cliniques.
  d <- donnees
  garder <- if (mode == "complets") complete.cases(d[variables]) else rep(TRUE, nrow(d))
  d <- d[garder, , drop = FALSE]
  if (nrow(d) < 5L) stop("Au moins cinq observations sont nécessaires.")
  x <- droplevels(as.data.frame(lapply(d[variables], factor)))
  exclues <- names(x)[vapply(x, nlevels, integer(1)) < 2L]
  variables <- setdiff(variables, exclues); x <- x[variables]
  if (length(variables) < 2L) stop("Au moins deux variables qualitatives non constantes sont nécessaires.")
  masque <- is.na(d[variables])
  rownames(masque) <- rownames(d)
  xi <- x; auxiliaires <- character()
  provenance <- droplevels(factor(d$provenance))
  if (mode != "complets" && nlevels(provenance) > 1L) {
    if (anyNA(provenance)) stop("La provenance auxiliaire doit être renseignée.")
    xi$provenance <- provenance; auxiliaires <- "provenance"
  }
  ncp <- NA_integer_; mi <- NULL; avertissements <- character()
  complet <- x
  if (mode != "complets" && any(masque)) {
    if (!requireNamespace("missMDA", quietly = TRUE)) stop("Installer missMDA pour cette imputation.")
    rang_max <- min(ncp_max, nrow(xi) - 2L,
      sum(vapply(xi, nlevels, integer(1))) - ncol(xi) - 1L)
    if (rang_max < 1L) stop("Rang insuffisant pour l’imputation ACM.")
    withCallingHandlers({
      ncp <- missMDA::estim_ncpMCA(xi, ncp.min = 0L, ncp.max = rang_max,
        nbsim = nbsim, verbose = FALSE)$ncp
      complet <- missMDA::imputeMCA(xi, ncp = ncp, method = "Regularized")$completeObs[variables]
      if (mode == "multiple") mi <- missMDA::MIMCA(xi, ncp = ncp, nboot = nboot)
    }, warning = function(w) {
      avertissements <<- c(avertissements, conditionMessage(w)); invokeRestart("muffleWarning")
    })
  }
  # Restaurer explicitement les observations : seules les cellules NA changent.
  reconstruire <- function(jeu) {
    if (nrow(jeu) != nrow(d)) stop("Jeu MIMCA mal aligné.")
    z <- d
    for (v in variables) {
      valeurs <- as.character(jeu[[v]])
      if (anyNA(valeurs) || any(!valeurs %in% levels(x[[v]])))
        stop("Modalités imputées manquantes ou inconnues.")
      z[[v]][masque[, v]] <- as.numeric(valeurs[masque[, v]])
    }
    z
  }
  simple <- reconstruire(complet)
  jeux <- if (is.null(mi)) list(simple) else lapply(mi$res.MI, reconstruire)
  imps <- list(simple = simple, jeux = jeux, masque = masque, m = length(jeux))
  # Vue individus de référence exclusivement pour les trois stratégies Jules.
  res <- calculer_acm_app(imps, variables, individus_reference = TRUE)
  res$mode <- mode; res$n <- nrow(d); res$n_total <- nrow(donnees)
  res$ncp <- ncp; res$lignes <- which(garder); res$complete <- simple[variables]
  res$masque <- masque; res$auxiliaires <- auxiliaires
  res$entrees_imputation <- names(xi); res$variables_exclues <- exclues
  res$avertissements <- unique(avertissements)
  res
})

# Cache partagé entre sessions ; stockage RDS optionnel, clé dépendante du protocole.
acm_cache_memoire <- new.env(parent = emptyenv())
acm_resultat_cache <- function(donnees, mode = "complets", avec_lourdes = TRUE,
  nboot = 20L, graine = 20261011L, ncp_max = 5L, nbsim = 10L) {
  versions <- vapply(c("FactoMineR", "missMDA"), function(p)
    if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else "absent", character(1))
  cle <- digest::digest(list(donnees, mode, avec_lourdes, nboot, graine, ncp_max, nbsim,
    versions, R.version.string, variables_acm_app, acm_variables_lourdes,
    body(calculer_acm_conditionnelle), body(calculer_acm_app), body(tableau_acm_app), body(acm_avec_graine)))
  if (exists(cle, envir = acm_cache_memoire, inherits = FALSE)) return(acm_cache_memoire[[cle]])
  dossier <- getOption("heart.acm.cache_dir", "resultats/cache_acm")
  fichier <- if (!is.null(dossier)) file.path(dossier, paste0(cle, ".rds")) else NULL
  if (!is.null(fichier) && file.exists(fichier)) res <- readRDS(fichier) else {
    res <- calculer_acm_conditionnelle(donnees, mode = mode, avec_lourdes = avec_lourdes,
      nboot = nboot, graine = graine, ncp_max = ncp_max, nbsim = nbsim)
    if (!is.null(fichier)) {
      dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
      tmp <- tempfile(tmpdir = dossier); saveRDS(res, tmp)
      if (!file.rename(tmp, fichier)) { unlink(tmp); warning("Sauvegarde du cache ACM impossible.") }
    }
  }
  acm_cache_memoire[[cle]] <- res
  res
}

acm_texte_resultat <- function(res, axes = c(1L, 2L)) {
  mode <- if (is.null(res$mode)) "Modèles choisis sur Gestion des NA" else
    names(acm_modes)[match(res$mode, unname(acm_modes))]
  texte <- paste(mode, "·", nrow(res$ref$ind$coord), "observations ·",
    length(res$variables), "variables actives ·", res$m, "jeu(x).")
  if (!is.null(res$n_total)) texte <- paste(texte,
    res$n_total - res$n, "observations exclues sur les seules variables actives.")
  if (!is.null(res$ncp) && !is.na(res$ncp)) texte <- paste(texte,
    "Rang d’imputation sélectionné par validation croisée :", res$ncp, ".")
  points <- points_acm_app(res, "modalites", axes)
  if ("diagnostic : 1" %in% points$id) {
    actifs <- !grepl("^(diagnostic|provenance) : ", points$id)
    pos <- points[points$id == "diagnostic : 1", c("x", "y")]
    distance <- rowSums(sweep(as.matrix(points[actifs, c("x", "y")]), 2, as.numeric(pos), "-")^2)
    ids <- points$id[actifs][head(order(distance), 3L)]
    texte <- paste(texte,
      "Modalités graphiquement les plus proches de la modalité 'Présence' (diagnostic 1) sur le plan factoriel affiché :",
      paste(libeller_points_acm(ids), collapse = ", "), ".",
      "Cette proximité est descriptive et ne constitue pas un test d’association.")
  }
  if (length(res$variables_exclues)) texte <- paste(texte,
    "Variables sans variation exclues :", paste(res$variables_exclues, collapse = ", "), ".")
  if (length(res$avertissements)) texte <- paste(texte,
    "Avertissements :", paste(res$avertissements, collapse = " ; "))
  texte
}
