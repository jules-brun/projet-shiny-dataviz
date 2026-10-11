# Préparation inductive : chaque paramètre est appris sur l'apprentissage seul.
# Aucune provenance, aucun diagnostic ni identifiant dans ces objets.
fit_preparation_ml <- function(x, types) {
  stopifnot(identical(names(x), names(types)), !any(c("provenance", "diagnostic", "diagnostic_initial") %in% names(x)))
  param <- lapply(names(x), function(v) {
    z <- x[[v]]
    if (types[[v]] == "quantitative") {
      med <- median(z, na.rm = TRUE)
      list(type = "quantitative", valeur = if (is.finite(med)) med else 0)
    } else {
      valeur <- if (all(is.na(z))) "inconnue" else mode_imputation(as.character(z))
      niveaux <- sort(unique(c(valeur, as.character(z[!is.na(z)]))))
      # model.matrix exige deux niveaux, même pour une catégorie constante.
      if (length(niveaux) == 1) niveaux <- c(niveaux, "__inutilisee__")
      list(type = "categorielle", valeur = valeur, niveaux = niveaux)
    }
  })
  names(param) <- names(x)
  prep <- list(param = param, variables = names(x))
  z <- appliquer_preparation_ml(prep, x, matrice = FALSE)
  a <- model.matrix(~ . - 1, z)
  actifs <- apply(a, 2, sd) > 1e-8
  prep$colonnes <- colnames(a)[actifs]
  if (!length(prep$colonnes)) stop("Aucun prédicteur variable dans l'apprentissage.")
  a <- a[, prep$colonnes, drop = FALSE]
  prep$centre <- colMeans(a)
  prep$echelle <- apply(a, 2, sd)
  prep
}

appliquer_preparation_ml <- function(prep, x, matrice = TRUE) {
  stopifnot(all(prep$variables %in% names(x)))
  z <- x[prep$variables]
  for (v in prep$variables) {
    p <- prep$param[[v]]
    if (p$type == "quantitative") {
      z[[paste0(v, "_NA")]] <- as.numeric(is.na(z[[v]]))
      z[[v]][is.na(z[[v]])] <- p$valeur
    } else {
      a <- as.character(z[[v]])
      a[is.na(a) | !a %in% p$niveaux] <- p$valeur
      z[[v]] <- factor(a, levels = p$niveaux)
    }
  }
  if (!matrice) return(z)
  a <- model.matrix(~ . - 1, z)[, prep$colonnes, drop = FALSE]
  scale(a, center = prep$centre, scale = prep$echelle)
}

# EM gaussien : mêmes moments conditionnels que le benchmark existant,
# mais séparation fit/predict pour conserver les paramètres appris.
fit_em_ml <- function(train, y, maxit = 100L) {
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
    mu2 <- s1 / nrow(z); sigma2 <- s2 / nrow(z) - tcrossprod(mu2) + diag(1e-6, p)
    delta <- max(abs(mu2 - mu), abs(sigma2 - sigma))
    mu <- mu2; sigma <- sigma2
    if (delta < 1e-6) { convergee <- TRUE; break }
  }
  if (!convergee) warning("EM : limite d'itérations atteinte")
  list(mu = mu, sigma = sigma, variables = names(train))
}

fit_imputation_cible_ml <- function(train, cible, type, types, methode, parametres, seed) {
  stopifnot(!anyNA(train[[cible]]), !any(c("provenance", "diagnostic") %in% names(train)))
  set.seed(seed)
  y <- train[[cible]]
  fit <- list(methode = methode, type = type, niveaux = sort(unique(y)), seed = seed)
  if (length(unique(y)) == 1L || methode %in% c("mean", "median", "mode")) {
    if (methode == "mean" && type != "quantitative") stop("non_applicable : moyenne")
    if (methode == "median" && !type %in% c("quantitative", "discrete", "ordinale")) stop("non_applicable : médiane")
    fit$valeur <- if (methode == "mean") mean(y) else if (methode == "median") median(y) else as.numeric(mode_imputation(y))
    return(fit)
  }
  variables <- setdiff(names(train), cible)
  if (methode == "EM") {
    if (type != "quantitative") stop("non_applicable : EM gaussien")
    v <- variables[vapply(variables, function(v) types[[v]] == "quantitative" &&
      sum(!is.na(train[[v]])) > 1 && sd(train[[v]], na.rm = TRUE) > 0, logical(1))]
    if (!length(v)) stop("Aucune mesure quantitative pour EM")
    fit$modele <- fit_em_ml(train[v], y, parametres$em_maxit)
    return(fit)
  }
  fit$prep <- fit_preparation_ml(train[variables], types[variables])
  a <- appliquer_preparation_ml(fit$prep, train[variables])
  fit$variables <- variables
  if (methode == "RandomForest") {
    fit$modele <- ranger::ranger(x = as.data.frame(a), y = if (type == "quantitative") y else factor(y),
      num.trees = parametres$num.trees, seed = seed, num.threads = 1)
  } else if (methode == "Knn") {
    fit$a <- a; fit$y <- y; fit$k <- min(parametres$k, nrow(a))
  } else if (methode %in% c("ACP", "AFDM")) {
    if (methode == "ACP") {
      fit$reduction <- prcomp(a, center = FALSE, scale. = FALSE,
        rank. = min(parametres$ncp, ncol(a), nrow(a) - 1L))
      scores <- fit$reduction$x
    } else {
      mixte <- appliquer_preparation_ml(fit$prep, train[variables], FALSE)
      mixte <- mixte[vapply(mixte, function(x) length(unique(x)) > 1L, logical(1))]
      fit$mixte <- names(mixte)
      fit$reduction <- FactoMineR::FAMD(droplevels(mixte), ncp = min(parametres$ncp, nrow(a) - 1L), graph = FALSE)
      scores <- fit$reduction$ind$coord
    }
    scores <- as.data.frame(scores); names(scores) <- paste0("axe", seq_len(ncol(scores)))
    fit$modele <- if (type == "quantitative") lm(y ~ ., data = data.frame(y = y, scores)) else
      nnet::multinom(y ~ ., data = data.frame(y = factor(y), scores), trace = FALSE,
        maxit = 300, MaxNWts = 10000)
  } else if (methode == "mice") {
    # Prédiction conditionnelle par mice : données de fit figées et assessment ignoré.
    # La sélection des colonnes et leurs niveaux sont déjà appris sur train seul.
    fit$train <- as.data.frame(a); fit$y <- y; fit$m <- parametres$mice_m
  } else stop("Méthode d'imputation inconnue : ", methode)
  fit
}

predire_imputation_cible_ml <- function(fit, test) {
  if (!nrow(test)) return(numeric())
  if (!is.null(fit$valeur)) p <- rep(fit$valeur, nrow(test)) else if (fit$methode == "EM") {
    f <- fit$modele; t <- as.matrix(test[f$variables])
    p <- vapply(seq_len(nrow(t)), function(i) {
      o <- which(!is.na(t[i, ])) + 1L
      if (!length(o)) return(f$mu[1])
      as.numeric(f$mu[1] + f$sigma[1, o, drop = FALSE] %*%
        solve(f$sigma[o, o, drop = FALSE], t[i, o - 1L] - f$mu[o]))
    }, numeric(1))
  } else {
    a <- appliquer_preparation_ml(fit$prep, test[fit$variables])
    if (fit$methode == "RandomForest") {
      p <- predict(fit$modele, data = as.data.frame(a))$predictions
      if (is.factor(p)) p <- as.character(p)
    } else if (fit$methode == "Knn") {
      ids <- FNN::get.knnx(fit$a, a, k = fit$k)$nn.index
      p <- vapply(seq_len(nrow(a)), function(i) if (fit$type == "quantitative") mean(fit$y[ids[i, ]]) else
        as.numeric(mode_imputation(fit$y[ids[i, ]])), numeric(1))
    } else if (fit$methode %in% c("ACP", "AFDM")) {
      scores <- if (fit$methode == "ACP") predict(fit$reduction, a) else {
        mixte <- appliquer_preparation_ml(fit$prep, test[fit$variables], FALSE)[fit$mixte]
        # Seuls les niveaux réellement observés du fit FAMD sont autorisés.
        for (v in names(mixte)) if (is.factor(mixte[[v]])) mixte[[v]] <- droplevels(mixte[[v]])
        predict(fit$reduction, newdata = mixte)$coord
      }
      scores <- as.data.frame(scores); names(scores) <- paste0("axe", seq_len(ncol(scores)))
      p <- predict(fit$modele, newdata = scores, type = if (fit$type == "quantitative") "response" else "class")
    } else if (fit$methode == "mice") {
      d <- rbind(fit$train, as.data.frame(a)); n <- nrow(fit$train)
      d$cible <- c(fit$y, rep(NA_real_, nrow(a)))
      method <- "pmm"
      if (fit$type != "quantitative") {
        d$cible <- factor(d$cible, levels = fit$niveaux, ordered = fit$type == "ordinale")
        method <- if (length(fit$niveaux) == 2) "logreg" else if (fit$type == "ordinale") "polr" else "polyreg"
      }
      meth <- setNames(rep("", ncol(d)), names(d)); meth["cible"] <- method
      pm <- matrix(0L, ncol(d), ncol(d), dimnames = list(names(d), names(d)))
      pm["cible", names(fit$train)] <- 1L
      imp <- mice::mice(d, m = fit$m, maxit = 1, method = meth, predictorMatrix = pm,
        ignore = seq_len(nrow(d)) > n, remove.constant = FALSE, remove.collinear = FALSE,
        seed = fit$seed, printFlag = FALSE)
      tirages <- vapply(seq_len(fit$m), function(i) as.numeric(as.character(mice::complete(imp, i)$cible[(n+1):nrow(d)])),
        numeric(nrow(a)))
      p <- if (fit$type == "quantitative") rowMeans(tirages) else apply(tirages, 1, function(x) as.numeric(mode_imputation(x)))
    }
  }
  if (is.factor(p)) p <- as.character(p)
  p <- as.numeric(p)
  if (fit$type %in% c("ordinale", "discrete")) p <- vapply(p, function(x) fit$niveaux[which.min(abs(fit$niveaux - x))], numeric(1))
  if (length(p) != nrow(test) || any(!is.finite(p)) ||
      (fit$type != "quantitative" && any(!p %in% fit$niveaux))) stop("Prédiction d'imputation invalide")
  p
}

# Adaptateurs au contrat du benchmark existant, sans accès à la provenance/cible ML.
modeles_imputation_ml <- setNames(lapply(names(modeles_imputation), function(nom) {
  force(nom)
  function(train, test, cible, type, types, parametres, seed) {
    fit <- fit_imputation_cible_ml(train, cible, type, types, nom, parametres, seed)
    predire_imputation_cible_ml(fit, test)
  }
}), names(modeles_imputation))

fit_imputations_ml <- function(x, selections, types, parametres, seed) {
  stopifnot(identical(names(x), names(types)))
  fits <- setNames(lapply(names(selections), function(v) {
    ids <- which(!is.na(x[[v]]))
    fit_imputation_cible_ml(x[ids, ], v, types[[v]], types, selections[[v]], parametres, seed)
  }), names(selections))
  list(fits = fits, variables = names(x))
}
appliquer_imputations_ml <- function(fit, x) {
  stopifnot(identical(names(x), fit$variables))
  original <- x
  for (v in names(fit$fits)) {
    ids <- which(is.na(original[[v]]))
    if (length(ids)) x[[v]][ids] <- predire_imputation_cible_ml(fit$fits[[v]], original[ids, setdiff(names(x), v), drop = FALSE])
  }
  if (anyNA(x)) stop("NA résiduels : imputation indisponible dans ce périmètre")
  x
}
