# Benchmark conditionnel : aucun accès aux cibles du jeu de test dans les modèles.
mode_imputation <- function(x) {
  t <- table(x, useNA = "no")
  if (!length(t)) stop("Aucune valeur observée")
  names(t)[which.max(t)]
}

# Préparation ajustée exclusivement sur les lignes d'apprentissage.
preparer_predicteurs <- function(train, test, types) {
  for (v in names(train)) {
    if (types[[v]] == "quantitative") {
      remplissage <- median(train[[v]], na.rm = TRUE)
      if (!is.finite(remplissage)) remplissage <- 0
      train[[paste0(v, "_NA")]] <- as.numeric(is.na(train[[v]]))
      test[[paste0(v, "_NA")]] <- as.numeric(is.na(test[[v]]))
      train[[v]][is.na(train[[v]])] <- remplissage
      test[[v]][is.na(test[[v]])] <- remplissage
    } else {
      a <- as.character(train[[v]]); b <- as.character(test[[v]])
      remplissage <- if (all(is.na(a))) "inconnue" else mode_imputation(a)
      a[is.na(a)] <- remplissage
      niveaux <- sort(unique(a))
      b[is.na(b) | !b %in% niveaux] <- remplissage
      train[[v]] <- factor(a, levels = niveaux)
      test[[v]] <- factor(b, levels = niveaux)
    }
  }
  actifs <- vapply(train, function(x) length(unique(x)) > 1L, logical(1))
  train <- train[actifs]; test <- test[actifs]
  if (!ncol(train)) stop("Aucun prédicteur variable")
  a <- model.matrix(~ . - 1, train); b <- model.matrix(~ . - 1, test)
  actifs <- apply(a, 2, sd) > 1e-8
  a <- a[, actifs, drop = FALSE]; b <- b[, actifs, drop = FALSE]
  centre <- colMeans(a); echelle <- apply(a, 2, sd)
  list(train = train, test = test,
       a = scale(a, center = centre, scale = echelle),
       b = scale(b, center = centre, scale = echelle))
}

predire_scores <- function(a, b, y, type) {
  a <- as.data.frame(a); b <- as.data.frame(b)
  names(a) <- names(b) <- paste0("axe", seq_len(ncol(a)))
  if (type == "quantitative") {
    fit <- lm(y ~ ., data = data.frame(y = y, a))
    return(as.numeric(predict(fit, newdata = b)))
  }
  if (!requireNamespace("nnet", quietly = TRUE)) stop("Package nnet absent")
  fit <- nnet::multinom(y ~ ., data = data.frame(y = factor(y), a),
                        trace = FALSE, maxit = 300, MaxNWts = 10000)
  as.character(predict(fit, newdata = b, type = "class"))
}

# EM gaussien sur la cible et les prédicteurs quantitatifs uniquement.
# Les moments conditionnels des cellules absentes sont utilisés à chaque étape E.
predire_em <- function(train, test, y, maxit = 100L, tol = 1e-6) {
  z <- cbind(cible = y, as.matrix(train)); p <- ncol(z)
  mu <- colMeans(z, na.rm = TRUE)
  z0 <- z
  for (j in seq_len(p)) z0[is.na(z0[, j]), j] <- mu[j]
  sigma <- cov(z0) + diag(1e-6, p)
  convergee <- FALSE
  for (iteration in seq_len(maxit)) {
    s1 <- numeric(p); s2 <- matrix(0, p, p)
    for (i in seq_len(nrow(z))) {
      o <- which(!is.na(z[i, ])); m <- which(is.na(z[i, ]))
      e <- z[i, ]; v <- matrix(0, p, p)
      if (length(m)) {
        gain <- sigma[m, o, drop = FALSE] %*% solve(sigma[o, o, drop = FALSE])
        e[m] <- mu[m] + gain %*% (z[i, o] - mu[o])
        v[m, m] <- sigma[m, m, drop = FALSE] - gain %*% sigma[o, m, drop = FALSE]
      }
      s1 <- s1 + e; s2 <- s2 + tcrossprod(e) + v
    }
    mu_new <- s1 / nrow(z)
    sigma_new <- s2 / nrow(z) - tcrossprod(mu_new) + diag(1e-6, p)
    delta <- max(abs(mu_new - mu), abs(sigma_new - sigma))
    mu <- mu_new; sigma <- sigma_new
    if (delta < tol) { convergee <- TRUE; break }
  }
  if (!convergee) warning("EM : limite d'itérations atteinte")
  vapply(seq_len(nrow(test)), function(i) {
    o <- which(!is.na(test[i, ])) + 1L
    if (!length(o)) return(mu[1])
    as.numeric(mu[1] + sigma[1, o, drop = FALSE] %*%
      solve(sigma[o, o, drop = FALSE], as.numeric(test[i, o - 1L]) - mu[o]))
  }, numeric(1))
}

# Contrat d'un modèle : function(train, test, cible, type, types, parametres, seed).
# train contient une cible observée ; test ne contient jamais la cible.
modeles_imputation <- list(
  mean = function(train, test, cible, type, types, parametres, seed) {
    if (type != "quantitative") stop("non_applicable : moyenne pour cible quantitative seulement")
    rep(mean(train[[cible]]), nrow(test))
  },
  median = function(train, test, cible, type, types, parametres, seed) {
    if (!type %in% c("quantitative", "discrete", "ordinale"))
      stop("non_applicable : médiane sans sens pour cible nominale")
    rep(median(train[[cible]]), nrow(test))
  },
  mode = function(train, test, cible, type, types, parametres, seed) {
    rep(mode_imputation(train[[cible]]), nrow(test))
  },
  RandomForest = function(train, test, cible, type, types, parametres, seed) {
    if (!requireNamespace("ranger", quietly = TRUE)) stop("Package ranger absent")
    y <- train[[cible]]
    x <- preparer_predicteurs(train[setdiff(names(train), cible)], test, types)
    d <- x$train
    d$cible_benchmark <- if (type == "quantitative") y else factor(y)
    fit <- ranger::ranger(cible_benchmark ~ ., data = d, seed = seed,
      num.trees = parametres$num.trees, num.threads = 1,
      respect.unordered.factors = "order")
    as.character(predict(fit, data = x$test)$predictions)
  },
  Knn = function(train, test, cible, type, types, parametres, seed) {
    if (!requireNamespace("FNN", quietly = TRUE)) stop("Package FNN absent")
    y <- train[[cible]]
    x <- preparer_predicteurs(train[setdiff(names(train), cible)], test, types)
    ids <- FNN::get.knnx(x$a, x$b, k = min(parametres$k, nrow(train)))$nn.index
    vapply(seq_len(nrow(test)), function(i) {
      valeurs <- y[ids[i, ]]
      if (type == "quantitative") as.character(mean(valeurs)) else mode_imputation(valeurs)
    }, character(1))
  },
  ACP = function(train, test, cible, type, types, parametres, seed) {
    x <- preparer_predicteurs(train[setdiff(names(train), cible)], test, types)
    fit <- prcomp(x$a, center = FALSE, scale. = FALSE,
                  rank. = min(parametres$ncp, ncol(x$a), nrow(train) - 1L))
    predire_scores(fit$x, predict(fit, x$b), train[[cible]], type)
  },
  AFDM = function(train, test, cible, type, types, parametres, seed) {
    if (!requireNamespace("FactoMineR", quietly = TRUE)) stop("Package FactoMineR absent")
    x <- preparer_predicteurs(train[setdiff(names(train), cible)], test, types)
    fit <- FactoMineR::FAMD(x$train, ncp = min(parametres$ncp, nrow(train) - 1L), graph = FALSE)
    predire_scores(fit$ind$coord, predict(fit, newdata = x$test)$coord, train[[cible]], type)
  },
  EM = function(train, test, cible, type, types, parametres, seed) {
    if (type != "quantitative") stop("non_applicable : EM gaussien pour cible quantitative seulement")
    v <- setdiff(names(train), cible)
    v <- v[vapply(v, function(j) types[[j]] == "quantitative" &&
      sum(!is.na(train[[j]])) > 1L && sd(train[[j]], na.rm = TRUE) > 0, logical(1))]
    if (!length(v)) stop("Aucun prédicteur quantitatif pour EM")
    predire_em(train[v], test[v], train[[cible]], parametres$em_maxit)
  },
  mice = function(train, test, cible, type, types, parametres, seed) {
    if (!requireNamespace("mice", quietly = TRUE)) stop("Package mice absent")
    x <- preparer_predicteurs(train[setdiff(names(train), cible)], test, types)
    d <- rbind(x$train, x$test)
    y <- train[[cible]]
    d$cible_benchmark <- c(y, rep(NA, nrow(test)))
    methode <- "pmm"
    if (type != "quantitative") {
      d$cible_benchmark <- factor(d$cible_benchmark, levels = sort(unique(y)),
                                  ordered = type == "ordinale")
      methode <- if (nlevels(d$cible_benchmark) == 2L) "logreg" else
        if (type == "ordinale") "polr" else "polyreg"
    }
    methodes <- setNames(rep("", ncol(d)), names(d)); methodes["cible_benchmark"] <- methode
    matrice <- matrix(0L, ncol(d), ncol(d), dimnames = list(names(d), names(d)))
    matrice["cible_benchmark", names(x$train)] <- 1L
    imp <- mice::mice(d, m = parametres$mice_m, maxit = 1L, method = methodes,
                     predictorMatrix = matrice, ignore = seq_len(nrow(d)) > nrow(train),
                     seed = seed, printFlag = FALSE)
    if (!is.null(imp$loggedEvents)) warning("mice : événements d'ajustement, voir contrôle des prédicteurs")
    predictions <- lapply(seq_len(parametres$mice_m), function(i)
      as.character(mice::complete(imp, i)$cible_benchmark[(nrow(train) + 1L):nrow(d)]))
    mat <- do.call(cbind, predictions)
    if (type == "quantitative") rowMeans(matrix(as.numeric(mat), nrow = nrow(test))) else
      apply(mat, 1, mode_imputation)
  }
)

metriques_imputation <- function(vrai, predit, type) {
  if (length(predit) != length(vrai) || anyNA(predit)) stop("Prédictions absentes ou incomplètes")
  if (type == "quantitative") {
    predit <- as.numeric(predit)
    if (any(!is.finite(predit))) stop("Prédictions non finies")
    return(c(RMSE = sqrt(mean((predit - vrai)^2)), MAE = mean(abs(predit - vrai))))
  }
  erreur <- mean(as.character(predit) != as.character(vrai))
  scores <- c(taux_erreur = erreur, accuracy = 1 - erreur)
  if (type %in% c("ordinale", "discrete")) {
    scores <- c(scores, RMSE = sqrt(mean((as.numeric(predit) - vrai)^2)),
                MAE = mean(abs(as.numeric(predit) - vrai)))
  }
  scores
}

sauver_rds_atomique <- function(objet, chemin) {
  temporaire <- tempfile(tmpdir = dirname(chemin))
  on.exit(unlink(temporaire))
  saveRDS(objet, temporaire)
  if (!file.rename(temporaire, chemin)) stop("Échec de sauvegarde : ", chemin)
}

benchmark_imputation <- function(donnees, types, modeles = modeles_imputation,
  cibles = names(types)[vapply(donnees[names(types)], anyNA, logical(1))],
  repetitions = 30L, proportion = 0.10, seed = 20261007L,
  dossier = "resultats/benchmark_imputation", workers = 1L,
  parametres = list(num.trees = 200L, k = 5L, ncp = 5L, mice_m = 5L, em_maxit = 100L)) {
  stopifnot(requireNamespace("digest", quietly = TRUE), proportion > 0, proportion < 0.20,
            repetitions >= 1, repetitions == as.integer(repetitions), workers >= 1,
            !is.null(names(modeles)), !anyDuplicated(names(modeles)),
            all(cibles %in% names(types)), all(names(types) %in% names(donnees)),
            all(types %in% c("quantitative", "nominale", "ordinale", "discrete")))
  if (!length(cibles)) stop("Aucune cible contenant des NA")
  dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
  # La reprise autorise une augmentation du nombre de répétitions, pas un changement de protocole.
  configuration <- list(donnees = digest::digest(donnees), types = types, cibles = cibles,
    modeles = lapply(modeles, function(f) paste(deparse(f), collapse = "\n")),
    code = digest::digest(lapply(c("preparer_predicteurs", "predire_scores", "predire_em",
      "metriques_imputation", "mode_imputation", "benchmark_imputation"),
      function(n) paste(deparse(get(n)), collapse = "\n"))),
    proportion = proportion, seed = seed, parametres = parametres,
    versions = vapply(c("ranger", "FNN", "FactoMineR", "mice", "nnet", "digest"),
      function(p) if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else "absent",
      character(1)), R = R.version.string)
  manifeste <- file.path(dossier, "configuration.rds")
  if (file.exists(manifeste)) {
    if (!identical(readRDS(manifeste), configuration))
      stop("Configuration différente : choisir un nouveau dossier de sauvegarde.")
  } else sauver_rds_atomique(configuration, manifeste)
  graine <- function(cible, repetition, modele = "masque") {
    h <- digest::digest(list(seed, cible, repetition, modele), algo = "xxhash32")
    as.integer(strtoi(substr(h, 1, 7), base = 16L)) + 1L
  }
  executer <- function(tache) {
    cible <- tache$cible; repetition <- tache$repetition
    chemin <- file.path(dossier, sprintf("%s_rep_%05d.rds", cible, repetition))
    if (file.exists(chemin)) return(readRDS(chemin))
    observes <- which(!is.na(donnees[[cible]]))
    n_test <- floor(length(observes) * proportion)
    if (n_test < 1L || length(observes) - n_test < 5L)
      stop("Trop peu de valeurs observées pour ", cible)
    set.seed(graine(cible, repetition))
    masque <- observes[sample.int(length(observes), n_test)]
    apprentissage <- setdiff(observes, masque)
    predicteurs <- setdiff(names(donnees), cible)
    train <- donnees[apprentissage, , drop = FALSE]
    test <- donnees[masque, predicteurs, drop = FALSE]
    vrai <- donnees[[cible]][masque]
    niveaux <- sort(unique(train[[cible]])); type <- types[[cible]]
    lignes <- lapply(names(modeles), function(nom) {
      avertissements <- character(); debut <- proc.time()[[3]]
      modele_seed <- graine(cible, repetition, nom)
      resultat <- tryCatch(withCallingHandlers({
        set.seed(modele_seed)
        predit <- if (length(niveaux) == 1L) rep(niveaux, n_test) else
          modeles[[nom]](train, test, cible, type, types, parametres, modele_seed)
        if (type != "quantitative") {
          if (type %in% c("discrete", "ordinale"))
            predit <- vapply(as.numeric(predit), function(x) niveaux[which.min(abs(niveaux - x))], numeric(1))
          if (any(!as.character(predit) %in% as.character(niveaux))) stop("Classe prédite invalide")
        }
        metriques_imputation(vrai, predit, type)
      }, warning = function(w) {
        avertissements <<- c(avertissements, conditionMessage(w)); invokeRestart("muffleWarning")
      }), error = function(e) e)
      erreur <- inherits(resultat, "error")
      statut <- if (!erreur) "ok" else if (startsWith(conditionMessage(resultat), "non_applicable"))
        "non_applicable" else "echec"
      if (erreur) resultat_scores <- setNames(NA_real_, "indisponible") else resultat_scores <- resultat
      data.frame(variable = cible, modele_imputation = nom, repetition = repetition,
        metrique = names(resultat_scores), valeur = unname(resultat_scores), statut = statut,
        message = if (erreur) conditionMessage(resultat) else paste(unique(avertissements), collapse = " | "),
        n_observe = length(observes), n_train = length(apprentissage), n_masque = n_test,
        seed_masque = graine(cible, repetition), seed_modele = modele_seed,
        masque_id = digest::digest(masque), duree_secondes = proc.time()[[3]] - debut)
    })
    objet <- list(resultats = do.call(rbind, lignes), masque = masque,
                  apprentissage = apprentissage, session = sessionInfo())
    sauver_rds_atomique(objet, chemin)
    objet
  }
  # Lots bornés ; chaque tâche sauvegarde sa répétition dès qu'elle est terminée.
  taches <- expand.grid(cible = cibles, repetition = seq_len(repetitions), stringsAsFactors = FALSE)
  sorties <- vector("list", nrow(taches))
  for (debut in seq.int(1L, nrow(taches), by = workers)) {
    ids <- seq.int(debut, min(nrow(taches), debut + workers - 1L))
    lot <- lapply(ids, function(i) as.list(taches[i, ]))
    res <- if (workers > 1L && .Platform$OS.type != "windows")
      parallel::mclapply(lot, executer, mc.cores = workers, mc.set.seed = FALSE) else lapply(lot, executer)
    if (any(vapply(res, inherits, logical(1), "try-error"))) stop("Une tâche parallèle a échoué ; sauvegardes conservées")
    sorties[ids] <- res
    message("Benchmark : ", max(ids), "/", nrow(taches), " tâches terminées ou reprises")
  }
  resultats <- do.call(rbind, lapply(sorties, `[[`, "resultats")); rownames(resultats) <- NULL
  sauver_rds_atomique(resultats, file.path(dossier, "resultats.rds"))
  utils::write.csv(resultats, file.path(dossier, "resultats.csv"), row.names = FALSE)
  resultats
}

resumer_benchmark <- function(resultats) {
  valide <- resultats[resultats$statut == "ok" & is.finite(resultats$valeur), ]
  if (!nrow(valide)) return(data.frame())
  groupes <- split(valide, interaction(valide$variable, valide$modele_imputation,
                                      valide$metrique, drop = TRUE))
  do.call(rbind, lapply(groupes, function(d) {
    x <- d$valeur; n <- length(x); s <- if (n > 1L) sd(x) else NA_real_
    demi <- if (n > 1L) qt(0.975, n - 1L) * s / sqrt(n) else NA_real_
    data.frame(variable = d$variable[1], modele_imputation = d$modele_imputation[1],
      metrique = d$metrique[1], erreur_moyenne = mean(x), sd_erreur = s,
      q025 = unname(quantile(x, .025)), mediane = median(x), q975 = unname(quantile(x, .975)),
      ic_moyenne_bas = mean(x) - demi, ic_moyenne_haut = mean(x) + demi,
      nb_repetitions = n, row.names = NULL)
  }))
}
