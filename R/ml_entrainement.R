# Pipeline ML : partition persistante, folds, apprentissage inductif et TEST unique.
configuration_ml <- function() list(
  seed = 20261010L, proportion_test = .2, folds = 5L,
  repetitions_imputation = 5L, proportion_masque = .1,
  # Grille fixée avant tout accès aux résultats TEST.
  seuils = seq(.15, .75, by = .025), min_specificite = .60, min_balanced_accuracy = .70,
  parametres_imputation = list(num.trees = 200L, k = 5L, ncp = 5L, mice_m = 5L, em_maxit = 100L),
  modeles = data.frame(candidat = c("logistique", "enet_ridge", "enet_mixte", "rf_3", "rf_6",
    "xgb_2", "xgb_4", "svm_05", "svm_2"),
    modele = c("Logistique", rep("Elastic net", 2), rep("Random Forest", 2), rep("XGBoost", 2), rep("SVM radial", 2))),
  cibles_imputation = c("pa_repos", "cholesterol", "glyc_jeun_elevee", "ecg_repos", "fc_max",
    "angine_effort", "depress_st", "pente_st", "nb_vaisseaux", "test_thallium"))

split_stratifie_ml <- function(d, proportion = .2, seed = 1L) {
  stopifnot(all(d$diagnostic %in% 0:1), proportion > 0, proportion < .5)
  set.seed(seed)
  groupes <- split(seq_len(nrow(d)), interaction(d$diagnostic, d$provenance, drop = TRUE))
  n <- lengths(groupes); ideal <- n*proportion
  # Toute strate de taille >=2 est présente des deux côtés. Les singletons restent TRAIN.
  minimum <- as.integer(n >= 2); maximum <- pmax(0, n-1)
  nombres <- pmin(maximum, pmax(minimum, floor(ideal)))
  cible <- max(sum(minimum), min(sum(maximum), round(nrow(d)*proportion)))
  while (sum(nombres) < cible) {
    ok <- nombres < maximum; j <- which.max(ifelse(ok, ideal-nombres, -Inf)); nombres[j] <- nombres[j]+1L
  }
  while (sum(nombres) > cible) {
    ok <- nombres > minimum; j <- which.max(ifelse(ok, nombres-ideal, -Inf)); nombres[j] <- nombres[j]-1L
  }
  test <- sort(unlist(lapply(seq_along(groupes), function(j) {
    ids <- groupes[[j]]; if (nombres[j] == 0) integer() else ids[sample.int(length(ids), nombres[j])]
  })))
  train <- setdiff(seq_len(nrow(d)), test)
  if (length(unique(d$diagnostic[train])) < 2 || length(unique(d$diagnostic[test])) < 2)
    stop("Effectifs insuffisants pour un split TRAIN/TEST avec les deux diagnostics.")
  list(train = train, test = test,
    strates = data.frame(strate = names(groupes), n = n, train = n-nombres, test = nombres),
    note = "Stratification diagnostic × provenance, allocation par plus grands restes bornée ; strates singleton conservées sur TRAIN.")
}
folds_stratifies_ml <- function(d, k = 5L, seed = 1L) {
  set.seed(seed)
  groupes <- split(seq_len(nrow(d)), interaction(d$diagnostic, d$provenance, drop = TRUE))
  affectation <- integer(nrow(d)); tailles <- integer(k)
  for (ids in groupes[order(lengths(groupes), decreasing = TRUE)]) {
    ids <- ids[sample.int(length(ids))]
    # Round-robin démarrant dans les folds les moins remplis.
    ordre <- order(tailles + runif(k, 0, .01))
    folds <- rep(ordre, length.out = length(ids))
    affectation[ids] <- folds
    tailles <- tailles + tabulate(folds, k)
  }
  for (i in seq_len(k)) if (length(unique(d$diagnostic[affectation == i])) < 2 ||
    length(unique(d$diagnostic[affectation != i])) < 2) stop("CV impossible : classe absente d'un fold.")
  affectation
}

selection_imputation_ml <- function(x, types, config, dossier, seed) {
  # Ne pas importer les anciens scores/shortlists calculés avec le futur TEST.
  # Réutiliser le protocole de masque et les mêmes familles de modèles uniquement.
  resultats <- benchmark_imputation(x, types, modeles = modeles_imputation_ml,
    cibles = config$cibles_imputation, repetitions = config$repetitions_imputation,
    proportion = config$proportion_masque, seed = seed, dossier = dossier,
    parametres = config$parametres_imputation)
  resume <- resumer_benchmark(resultats)
  selections <- setNames(character(length(config$cibles_imputation)), config$cibles_imputation)
  top5 <- list()
  for (v in config$cibles_imputation) {
    metrique <- if (types[[v]] == "quantitative") "RMSE" else "taux_erreur"
    r <- resume[resume$variable == v & resume$metrique == metrique &
      resume$nb_repetitions == config$repetitions_imputation & is.finite(resume$erreur_moyenne), ]
    r <- r[order(r$erreur_moyenne, r$sd_erreur, r$modele_imputation), ]
    r <- head(r, 5L)
    if (!nrow(r)) stop("Aucune méthode d'imputation valide : ", v)
    # Règle prédéfinie : erreur moyenne + une erreur standard (stabilité).
    r$score_stabilite <- r$erreur_moyenne + r$sd_erreur / sqrt(r$nb_repetitions)
    meilleur <- order(r$score_stabilite, r$erreur_moyenne, r$modele_imputation)[1]
    selections[[v]] <- r$modele_imputation[meilleur]
    r$retenue <- seq_len(nrow(r)) == meilleur
    top5[[v]] <- r
  }
  list(selections = selections, top5 = do.call(rbind, top5), resultats = resultats)
}

fit_classifieur_ml <- function(a, y, candidat, seed) {
  set.seed(seed)
  if (candidat == "logistique") {
    # Pas de sélection de variables ; schéma de colonnes appris sur analysis.
    fit <- glm(y ~ ., data = data.frame(y = y, a), family = binomial())
    if (!fit$converged) stop("La régression logistique n'a pas convergé")
  } else if (startsWith(candidat, "enet")) {
    alpha <- if (candidat == "enet_ridge") 0 else .5
    fit <- glmnet::glmnet(a, y, family = "binomial", alpha = alpha, lambda = .03)
  } else if (startsWith(candidat, "rf")) {
    fit <- ranger::ranger(x = as.data.frame(a), y = factor(y, levels = 0:1), probability = TRUE,
      mtry = min(ncol(a), if (candidat == "rf_3") 3L else 6L), min.node.size = 5L,
      num.trees = 400L, num.threads = 1, seed = seed)
  } else if (startsWith(candidat, "xgb")) {
    fit <- xgboost::xgb.train(params = list(objective = "binary:logistic", eval_metric = "auc",
      max_depth = if (candidat == "xgb_2") 2L else 4L, eta = .05,
      subsample = .8, colsample_bytree = .8, seed = seed, nthread = 1),
      data = xgboost::xgb.DMatrix(a, label = y), nrounds = 120L, verbose = 0)
  } else if (startsWith(candidat, "svm")) {
    fit <- e1071::svm(a, factor(y, levels = 0:1), kernel = "radial", scale = FALSE,
      cost = if (candidat == "svm_05") .5 else 2, gamma = 1/ncol(a), probability = TRUE)
  } else stop("Classifieur non disponible : ", candidat)
  list(candidat = candidat, modele = fit)
}
predire_classifieur_ml <- function(fit, a) {
  nom <- fit$candidat; m <- fit$modele
  p <- if (nom == "logistique") predict(m, newdata = as.data.frame(a), type = "response") else
    if (startsWith(nom, "enet")) predict(m, newx = a, type = "response", s = .03) else
    if (startsWith(nom, "rf")) predict(m, data = as.data.frame(a))$predictions[, "1"] else
    if (startsWith(nom, "xgb")) predict(m, xgboost::xgb.DMatrix(a)) else {
      pred <- predict(m, a, probability = TRUE)
      attr(pred, "probabilities")[, "1"]
    }
  p <- as.numeric(p)
  if (any(!is.finite(p))) stop("Probabilités de classification non finies")
  pmin(1, pmax(0, p))
}

selection_modele_seuil_ml <- function(oof, config) {
  scores <- do.call(rbind, lapply(split(oof, oof$candidat), function(z) {
    do.call(rbind, lapply(config$seuils, function(t) {
      # Évaluer les seuils sur des prédictions OOF de TRAIN, jamais sur TEST.
      metrics <- do.call(rbind, lapply(split(z, z$fold), function(f)
        metriques_prediction_ml(f$diagnostic, f$probabilite, t)))
      data.frame(candidat = z$candidat[1], modele = z$modele[1], seuil = t,
        sensitivity = mean(metrics[, "sensitivity"]), specificity = mean(metrics[, "specificity"]),
        balanced_accuracy = mean(metrics[, "balanced_accuracy"]), roc_auc = mean(metrics[, "roc_auc"]))
    }))
  }))
  scores$admissible <- scores$specificity >= config$min_specificite &
    scores$balanced_accuracy >= config$min_balanced_accuracy
  admissibles <- scores[scores$admissible, ]
  if (!nrow(admissibles)) stop("Aucun couple modèle/seuil ne respecte la règle clinique prédéfinie. TEST reste verrouillé.")
  ordonner <- function(z) z[order(-z$sensitivity, -z$balanced_accuracy, -z$roc_auc,
    -z$specificity, z$candidat, -z$seuil), ]
  meilleur <- ordonner(admissibles)[1, ]
  par_candidat <- do.call(rbind, lapply(split(scores, scores$candidat), function(z) {
    if (any(z$admissible)) ordonner(z[z$admissible, ])[1, ] else
      z[order(-z$balanced_accuracy, -z$roc_auc, -z$sensitivity), ][1, ]
  }))
  list(meilleur = meilleur, par_candidat = par_candidat, scores = scores)
}

entrainer_experience_ml <- function(d, types, config = configuration_ml(), dossier = "resultats/ml") {
  stopifnot(!"provenance" %in% names(types), !"diagnostic" %in% names(types),
    all(names(types) %in% names(d)), !anyNA(d$diagnostic))
  dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
  manifeste_path <- file.path(dossier, "manifeste.rds")
  final_path <- file.path(dossier, "evaluation_finale.rds")
  empreinte <- digest::digest(d)
  code_actuel <- tools::md5sum(c("R/ml_preparation.R", "R/ml_entrainement.R", "R/ml_evaluation.R", "R/benchmark_imputation.R"))
  if (file.exists(manifeste_path)) {
    manifeste <- readRDS(manifeste_path)
    if (!identical(manifeste$donnees, empreinte) || !identical(manifeste$config, config))
      stop("Expérience déjà enregistrée avec un autre dataset/protocole. Aucun nouvel accès à TEST autorisé.")
    if (file.exists(final_path)) return(readRDS(final_path))
    if (!identical(manifeste$code, code_actuel)) stop("Le code d'apprentissage a changé : reprise avec caches incompatibles interdite.")
    if (manifeste$test_consomme) stop("TEST déjà consulté : reprise d'évaluation interdite.")
  }
  verrou <- file.path(dossier, ".calcul_en_cours")
  if (!dir.create(verrou, showWarnings = FALSE)) stop("Un calcul est déjà en cours, ou son verrou nécessite un audit après interruption.")
  on.exit(unlink(verrou, recursive = TRUE), add = TRUE)
  journal <- function(etape, detail = "") {
    ligne <- data.frame(utc = format(Sys.time(), tz = "UTC", usetz = TRUE), etape = etape, detail = detail)
    chemin <- file.path(dossier, "registre.csv")
    write.table(ligne, chemin, append = file.exists(chemin), col.names = !file.exists(chemin),
      row.names = FALSE, sep = ",")
    message(etape, " : ", detail)
  }
  if (!file.exists(manifeste_path)) {
    split <- split_stratifie_ml(d, config$proportion_test, config$seed)
    train <- d[split$train, ]; test <- d[split$test, ]
    folds <- folds_stratifies_ml(train, config$folds, config$seed+1L)
    manifeste <- list(donnees = empreinte, config = config, split = split, folds = folds,
      test_consomme = FALSE, statut = "TEST verrouillé", types = types,
      versions = vapply(c("ranger", "glmnet", "xgboost", "e1071", "mice", "FactoMineR"),
        function(p) as.character(packageVersion(p)), character(1)),
      code = code_actuel)
    sauver_rds_atomique(manifeste, manifeste_path)
    sauver_rds_atomique(list(train = train, test_verrouille = test), file.path(dossier, "partition.rds"))
    journal("SPLIT_FIGE", paste(nrow(train), "TRAIN ;", nrow(test), "TEST"))
  }
  partition <- readRDS(file.path(dossier, "partition.rds"))
  train <- partition$train
  # Ne pas lire partition$test_verrouille avant que le pipeline soit entièrement figé.
  oof <- list(); audits <- list()
  for (fold in seq_len(config$folds)) {
    cache <- file.path(dossier, paste0("fold_", fold, ".rds"))
    if (file.exists(cache)) { objet <- readRDS(cache); oof[[fold]] <- objet$predictions; audits[[fold]] <- objet$audit; next }
    analysis_ids <- which(manifeste$folds != fold); assessment_ids <- which(manifeste$folds == fold)
    analysis <- train[analysis_ids, ]; assessment <- train[assessment_ids, ]
    journal("CV_FOLD", paste(fold, "analysis", nrow(analysis), "assessment", nrow(assessment)))
    choix <- selection_imputation_ml(analysis[names(types)], types, config,
      file.path(dossier, paste0("benchmark_fold_", fold)), config$seed+fold*100L)
    imp <- fit_imputations_ml(analysis[names(types)], choix$selections, types, config$parametres_imputation, config$seed+fold)
    ax <- appliquer_imputations_ml(imp, analysis[names(types)])
    bx <- appliquer_imputations_ml(imp, assessment[names(types)])
    prep <- fit_preparation_ml(ax, types)
    a <- appliquer_preparation_ml(prep, ax); b <- appliquer_preparation_ml(prep, bx)
    predictions <- list(); echecs <- list()
    for (j in seq_len(nrow(config$modeles))) {
      candidat <- config$modeles$candidat[j]
      res <- tryCatch({
        fit <- fit_classifieur_ml(a, analysis$diagnostic, candidat, config$seed+fold*100+j)
        predire_classifieur_ml(fit, b)
      }, error = function(e) e)
      if (inherits(res, "error")) { echecs[[candidat]] <- conditionMessage(res); next }
      predictions[[candidat]] <- data.frame(id = assessment_ids, fold = fold,
        candidat = candidat, modele = config$modeles$modele[j], diagnostic = assessment$diagnostic,
        provenance = assessment$provenance, probabilite = res)
    }
    objet <- list(predictions = do.call(rbind, predictions), audit = list(analysis_ids = analysis_ids,
      assessment_ids = assessment_ids, imputation = choix, echecs = echecs,
      predictors = names(types), preprocessing = prep))
    sauver_rds_atomique(objet, cache)
    oof[[fold]] <- objet$predictions; audits[[fold]] <- objet$audit
  }
  oof <- do.call(rbind, oof)
  stopifnot(!anyDuplicated(oof[c("candidat", "id")]))
  valides <- names(which(table(oof$candidat) == nrow(train)))
  oof <- oof[oof$candidat %in% valides, ]
  if (!nrow(oof)) stop("Aucun modèle valide dans tous les folds ; TEST reste verrouillé.")
  choix <- selection_modele_seuil_ml(oof, config)
  performances <- do.call(rbind, lapply(split(oof, interaction(oof$candidat, oof$fold, drop = TRUE)), function(z) {
    seuil <- choix$par_candidat$seuil[match(z$candidat[1], choix$par_candidat$candidat)]
    data.frame(candidat = z$candidat[1], modele = z$modele[1], fold = z$fold[1], seuil = seuil,
      as.list(metriques_prediction_ml(z$diagnostic, z$probabilite, seuil)))
  }))
  validation <- list(oof = oof, performances = performances, resume = resume_metriques_ml(performances),
    choix = choix, audits = audits, split = manifeste$split, folds = manifeste$folds, config = config)
  sauver_rds_atomique(validation, file.path(dossier, "validation_train.rds"))
  journal("SELECTION_TRAIN", paste(choix$meilleur$candidat, "seuil", choix$meilleur$seuil))
  benchmark_final <- selection_imputation_ml(train[names(types)], types, config,
    file.path(dossier, "benchmark_train_complet"), config$seed+900L)
  imp <- fit_imputations_ml(train[names(types)], benchmark_final$selections, types, config$parametres_imputation, config$seed)
  x <- appliquer_imputations_ml(imp, train[names(types)])
  prep <- fit_preparation_ml(x, types); a <- appliquer_preparation_ml(prep, x)
  modele <- fit_classifieur_ml(a, train$diagnostic, choix$meilleur$candidat, config$seed)
  pipeline <- list(imputation = imp, preprocessing = prep, modele = modele,
    seuil = choix$meilleur$seuil, predictors = names(types), benchmark = benchmark_final,
    selection = choix$meilleur, config = config)
  sauver_rds_atomique(pipeline, file.path(dossier, "pipeline_fige.rds"))
  journal("PIPELINE_FIGE", "Imputation, transformations, modèle, hyperparamètres et seuil enregistrés avant TEST")
  # Consommation marquée AVANT accès/prediction : une erreur ne permet pas de retuner.
  manifeste$test_consomme <- TRUE; manifeste$statut <- "TEST consommé une fois"
  sauver_rds_atomique(manifeste, manifeste_path)
  journal("TEST_DEVERROUILLE", "Évaluation unique du pipeline figé")
  test <- partition$test_verrouille
  xt <- appliquer_imputations_ml(pipeline$imputation, test[names(types)])
  bt <- appliquer_preparation_ml(pipeline$preprocessing, xt)
  p <- predire_classifieur_ml(pipeline$modele, bt)
  predictions <- data.frame(id = manifeste$split$test, diagnostic = test$diagnostic,
    provenance = test$provenance, probabilite = p, prediction = as.integer(p >= pipeline$seuil))
  final <- list(validation = validation, pipeline = list(selection = pipeline$selection,
      imputations = benchmark_final$selections, top5 = benchmark_final$top5, predictors = pipeline$predictors,
      seuil = pipeline$seuil),
    test = list(predictions = predictions, metrics = metriques_prediction_ml(test$diagnostic, p, pipeline$seuil),
      roc = roc_points_ml(test$diagnostic, p), calibration = calibration_ml(test$diagnostic, p),
      robustesse = robustesse_provenance_ml(predictions, pipeline$seuil)),
    manifeste = manifeste, date = format(Sys.time(), tz = "UTC", usetz = TRUE))
  sauver_rds_atomique(final, final_path)
  journal("EVALUATION_FINALE_SAUVEE", "Aucun nouveau choix autorisé sur TEST ; affichage des résultats enregistrés")
  final
}
