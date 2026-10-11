source("R/graphiques.R", local = TRUE)
source("R/acm_imputation.R", local = TRUE)

# Calculs de la nouvelle application : benchmark sauvegardé et projections communes.
variables_acm_app <- c("sexe", "type_doul_thor", "glyc_jeun_elevee", "ecg_repos",
  "angine_effort", "pente_st", "nb_vaisseaux", "test_thallium")
centres_app <- c(cleveland = "Cleveland", hungarian = "Hongrie",
  switzerland = "Suisse", va = "VA Long Beach")

charger_benchmark_app <- function(dossier = "resultats/benchmark_imputation") {
  configuration <- readRDS(file.path(dossier, "configuration.rds"))
  resultats <- readRDS(file.path(dossier, "resultats.rds"))
  meilleurs <- read.csv(file.path(dossier, "meilleurs_modeles.csv"))
  types <- configuration$types
  cibles <- configuration$cibles
  choix <- setNames(lapply(cibles, function(v) {
    metrique <- if (types[[v]] == "quantitative") "RMSE" else "taux_erreur"
    r <- resultats[resultats$variable == v & resultats$metrique == metrique &
      resultats$statut == "ok" & is.finite(resultats$valeur), ]
    # Ne proposer que les modèles ayant réussi toutes les répétitions.
    n <- table(r$modele_imputation)
    names(n)[n == max(resultats$repetition)]
  }), cibles)
  defauts <- setNames(vapply(cibles, function(v) {
    m <- meilleurs$modele_imputation[meilleurs$variable == v]
    m <- intersect(m, choix[[v]])
    if (!length(m)) stop("Aucun meilleur modèle valide pour ", v)
    m[1]
  }, character(1)), cibles)
  list(configuration = configuration, resultats = resultats, types = types,
       cibles = cibles, choix = choix, defauts = defauts)
}

imputer_jeu_app <- function(d, selections, benchmark, bootstrap = FALSE, seed = 1L,
                            progression = function(...) NULL) {
  set.seed(seed)
  x <- d[names(benchmark$types)] # Exclut le diagnostic ; inclut la provenance.
  complete <- d
  for (v in benchmark$cibles) {
    progression(v)
    modele <- selections[[v]]
    if (!modele %in% benchmark$choix[[v]]) stop("Modèle indisponible pour ", v)
    manquants <- which(is.na(x[[v]]))
    if (!length(manquants)) next
    observes <- which(!is.na(x[[v]]))
    type <- benchmark$types[[v]]
    niveaux <- sort(unique(x[[v]][observes]))
    train_ids <- observes
    if (bootstrap) {
      # Bootstrap stratifié des catégories pour conserver les classes rares.
      groupes <- if (type == "quantitative") list(observes) else split(observes, x[[v]][observes])
      train_ids <- unlist(lapply(groupes, function(ids) ids[sample.int(length(ids), length(ids), TRUE)]),
                          use.names = FALSE)
    }
    train <- x[train_ids, , drop = FALSE]
    test <- x[manquants, setdiff(names(x), v), drop = FALSE]
    param <- benchmark$configuration$parametres
    # Un tirage mice par jeu, plutôt qu'une moyenne/vote de plusieurs imputations.
    if (bootstrap && modele == "mice") param$mice_m <- 1L
    p <- if (length(niveaux) == 1L) rep(niveaux, length(manquants)) else
      modeles_imputation[[modele]](train, test, v, type, benchmark$types, param, seed)
    p <- as.numeric(p)
    if (type %in% c("discrete", "ordinale"))
      p <- vapply(p, function(z) niveaux[which.min(abs(niveaux - z))], numeric(1))
    if (length(p) != length(manquants) || any(!is.finite(p)) ||
        (type != "quantitative" && any(!p %in% niveaux))) stop("Imputation invalide : ", v)
    complete[[v]][manquants] <- p
  }
  # Les prédicteurs restent ceux du benchmark, sans mise à jour séquentielle.
  complete
}

calculer_imputations_app <- function(d, selections, benchmark, m = 20L, seed = 20261010L,
                                     progression = function(...) NULL) {
  stopifnot(m >= 5L, m <= 100L)
  simple <- imputer_jeu_app(d, selections, benchmark, seed = seed)
  jeux <- lapply(seq_len(m), function(i) {
    progression(i, m)
    imputer_jeu_app(d, selections, benchmark, bootstrap = TRUE, seed = seed + i)
  })
  list(simple = simple, jeux = jeux, selections = selections, m = m, seed = seed,
       masque = is.na(d[variables_acm_app]))
}

tableau_acm_app <- function(d, niveaux = NULL, variables = variables_acm_app) {
  x <- setNames(lapply(variables, function(v) {
    valeurs <- paste0(v, " : ", d[[v]])
    if (is.null(niveaux)) factor(valeurs) else factor(valeurs, levels = niveaux[[v]])
  }), variables)
  as.data.frame(x)
}

# Moteur factoriel commun : stratégies Jules et modèles choisis de la page 2.
calculer_acm_app <- function(imputations, variables = variables_acm_app,
                             individus_reference = FALSE) {
  d <- imputations$simple
  x <- tableau_acm_app(d, variables = variables)
  if (anyNA(d[variables])) stop("L’ACM nécessite des variables actives complétées.")
  variables <- names(x)[vapply(x, nlevels, integer(1)) > 1L]
  x <- x[variables]
  if (nrow(x) < 5L || length(variables) < 2L)
    stop("Au moins cinq observations et deux variables non constantes sont nécessaires.")
  niveaux <- lapply(x, levels)
  sup <- data.frame(diagnostic = factor(d$diagnostic, levels = 0:1),
                    provenance = droplevels(factor(d$provenance)))
  utilisables <- names(sup)[vapply(sup, function(z)
    !anyNA(z) && nlevels(droplevels(z)) > 1L, logical(1))]
  base <- cbind(x, sup[utilisables])
  ncp <- min(nrow(x) - 1L, sum(vapply(x, nlevels, integer(1))) - ncol(x))
  ref <- FactoMineR::MCA(base, quali.sup = if (length(utilisables))
    match(utilisables, names(base)) else NULL, ncp = ncp, graph = FALSE)
  if (ncol(ref$ind$coord) < 2L) stop("Deux dimensions exploitables sont nécessaires.")
  # Même repère pour chaque jeu ; les lignes et les modalités restent alignées.
  coords <- lapply(imputations$jeux, function(j) {
    if (nrow(j) != nrow(d)) stop("Jeu imputé mal aligné.")
    xm <- tableau_acm_app(j, niveaux, variables)
    if (anyNA(xm)) stop("Modalités manquantes ou inconnues dans un jeu imputé.")
    as.matrix(predict(ref, newdata = xm)$coord)
  })
  eig <- ref$eig[seq_len(ncol(ref$ind$coord)), 1]
  bary <- function(coord, facteurs) {
    lignes <- lapply(names(facteurs), function(v) {
      lev <- levels(facteurs[[v]])
      lev <- lev[vapply(lev, function(l) any(facteurs[[v]] == l, na.rm = TRUE), logical(1))]
      if (!length(lev)) return(NULL)
      z <- t(vapply(lev, function(l) {
        ids <- which(!is.na(facteurs[[v]]) & facteurs[[v]] == l)
        colMeans(coord[ids, , drop = FALSE])
      }, numeric(ncol(coord))))
      z <- sweep(z, 2, sqrt(eig), "/")
      rownames(z) <- if (v %in% variables) lev else paste0(v, " : ", lev)
      z
    })
    do.call(rbind, lignes)
  }
  modalites <- lapply(seq_along(coords), function(i) {
    bary(coords[[i]], cbind(tableau_acm_app(imputations$jeux[[i]], niveaux, variables), sup))
  })
  reference_modalites <- bary(ref$ind$coord, cbind(x, sup))
  list(ref = ref, individus = if (individus_reference) list(ref$ind$coord) else coords,
       modalites = modalites, reference_modalites = reference_modalites,
       diagnostic = sup$diagnostic, provenance = sup$provenance,
       impute = rowSums(imputations$masque[, variables, drop = FALSE]) > 0, m = imputations$m,
       variables = variables, individus_reference = individus_reference)
}

ellipses_app <- function(nuage, limite = Inf) {
  groupes <- split(nuage, factor(nuage$id, levels = unique(nuage$id)))
  groupes <- head(groupes, limite)
  sorties <- lapply(groupes, function(z) {
    centre <- colMeans(z[c("x", "y")])
    s <- cov(z[c("x", "y")])
    if (any(!is.finite(s))) return(NULL)
    e <- eigen(s, symmetric = TRUE)
    if (max(e$values) < 1e-12) return(NULL) # Points invariants : ellipse nulle.
    angles <- seq(0, 2*pi, length.out = 65)
    cercle <- rbind(cos(angles), sin(angles))
    a <- sqrt(qchisq(.95, 2)) * e$vectors %*% diag(sqrt(pmax(e$values, 0)), 2) %*% cercle
    data.frame(id = z$id[1], x = a[1, ] + centre[1], y = a[2, ] + centre[2])
  })
  do.call(rbind, sorties)
}

nuage_acm_app <- function(res, vue, axes) {
  matrices <- res[[vue]]
  do.call(rbind, lapply(seq_along(matrices), function(i) {
    z <- matrices[[i]]
    data.frame(id = if (vue == "individus") as.character(seq_len(nrow(z))) else rownames(z),
               x = z[, axes[1]], y = z[, axes[2]], jeu = i)
  }))
}

graphe_acm_app <- function(res, vue, axes = c(1L, 2L), ellipses = FALSE, limite = 60L) {
  stopifnot(length(unique(axes)) == 2, all(axes >= 1), all(axes <= ncol(res$individus[[1]])))
  nuage <- nuage_acm_app(res, vue, axes)
  points <- if (isTRUE(res$individus_reference) && vue == "modalites") {
    z <- res$reference_modalites
    data.frame(id = rownames(z), x = z[, axes[1]], y = z[, axes[2]])
  } else aggregate(cbind(x, y) ~ id, nuage, mean)
  g <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, colour = palette_heart$line) +
    ggplot2::geom_vline(xintercept = 0, colour = palette_heart$line)
  if (ellipses && !(vue == "individus" && isTRUE(res$individus_reference)) && res$m >= 3L) {
    candidats <- if (vue == "individus") nuage[nuage$id %in% as.character(which(res$impute)), ] else nuage
    e <- ellipses_app(candidats, if (vue == "individus") limite else Inf)
    if (!is.null(e)) g <- g + ggplot2::geom_path(data = e, ggplot2::aes(x, y, group = id),
                                               colour = palette_heart$muted, alpha = .45, linewidth = .4)
  }
  if (vue == "individus") {
    points$diagnostic <- res$diagnostic[as.integer(points$id)]
    g <- g + ggplot2::geom_point(data = points, ggplot2::aes(x, y, colour = diagnostic), alpha = .55) +
      ggplot2::scale_colour_manual(values = c("0" = palette_heart$blue, "1" = palette_heart$red),
                                   drop = FALSE, name = "Diagnostic")
  } else {
    points$variable <- sub(" :.*$", "", points$id)
    titres_variables <- libelles_variables_acm
    points$libelle <- libeller_points_acm(points$id)
    g <- g + ggplot2::geom_point(data = points, ggplot2::aes(x, y, colour = variable,
      shape = variable %in% c("diagnostic", "provenance")), size = 3) +
      ggrepel::geom_text_repel(data = points, ggplot2::aes(x, y, label = libelle, colour = variable),
        seed = 42, max.overlaps = Inf, size = 3.7, show.legend = FALSE) +
      ggplot2::scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 18), guide = "none") +
      ggplot2::scale_colour_discrete(labels = titres_variables) +
      ggplot2::labs(colour = "Variable")
  }
  titres <- sprintf("Dimension %d (%.1f %%)", axes, res$ref$eig[axes, 2])
  g + ggplot2::labs(x = titres[1], y = titres[2],
    caption = if (isTRUE(res$individus_reference)) {
      if (vue == "individus") "Positions du jeu de référence ; aucune variabilité individuelle représentée."
      else paste0("Positions de référence · ", res$m, " jeu(x) · Diagnostic et provenance supplémentaires",
        if (ellipses && res$m >= 3L) "\nEllipses de variabilité liée aux imputations multiples ; contours descriptifs." else "")
    } else paste0(res$m, " jeux imputés · Points : positions moyennes · Diagnostic et provenance supplémentaires",
      if (ellipses) "\nEllipses de variabilité d’imputation ; contours descriptifs." else "")) +
    ggplot2::coord_equal() + theme_heart() + ggplot2::theme(legend.position = "bottom")
}

# Une barre par centre : observé + manquant = effectif nettoyé du centre.
compter_disponibilite_app <- function(d, variable) {
  stopifnot(variable %in% names(d))
  resultat <- do.call(rbind, lapply(names(centres_app), function(centre) {
    x <- d[[variable]][d$provenance == centre]
    data.frame(provenance = centre, statut = c("Valeurs observées", "NA"),
               n = c(sum(!is.na(x)), sum(is.na(x))))
  }))
  resultat$provenance <- factor(resultat$provenance, levels = names(centres_app))
  resultat$statut <- factor(resultat$statut, levels = c("Valeurs observées", "NA"))
  resultat
}
