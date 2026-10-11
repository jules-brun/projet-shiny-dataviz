# Audit de l'expérience sauvegardée : aucun nouvel ajustement ni accès prédictif au TEST.
options(sass.cache=FALSE)
Sys.setenv(TZ="Europe/Paris")
invisible(capture.output(serveur <- source("server.R")$value))
source("R/ml_preparation.R")
source("R/ml_entrainement.R")
r <- readRDS("resultats/ml/evaluation_finale.rds")
partition <- readRDS("resultats/ml/partition.rds")
train <- partition$train
m <- r$manifeste
stopifnot(m$test_consomme, length(m$split$test)==184, length(m$split$train)==734,
  !length(intersect(m$split$train,m$split$test)),
  !any(c("provenance","diagnostic","diagnostic_initial") %in% r$pipeline$predictors),
  all(r$test$predictions$prediction == as.integer(r$test$predictions$probabilite >= r$pipeline$seuil)))
for (i in 1:5) {
  a <- r$validation$audits[[i]]
  stopifnot(!length(intersect(a$analysis_ids,a$assessment_ids)),
    length(c(a$analysis_ids,a$assessment_ids))==nrow(train),
    identical(a$assessment_ids,which(m$folds==i)))
  b <- readRDS(file.path("resultats/ml",paste0("benchmark_fold_",i),"configuration.rds"))
  stopifnot(identical(b$donnees,digest::digest(train[a$analysis_ids,names(b$types)])),
    !any(c("provenance","diagnostic") %in% names(b$types)))
  for (v in unique(a$imputation$top5$variable)) {
    z <- a$imputation$top5[a$imputation$top5$variable==v,]
    stopifnot(nrow(z)<=5, sum(z$retenue)==1, all(z$nb_repetitions==5))
  }
}
# Chaque candidat a exactement une prédiction OOF par observation TRAIN.
oof <- r$validation$oof
stopifnot(!anyDuplicated(oof[c("candidat","id")]), all(table(oof$candidat)==nrow(train)),
  all(oof$fold == m$folds[oof$id]))
registre <- read.csv("resultats/ml/registre.csv")
stopifnot(sum(registre$etape=="TEST_DEVERROUILLE")==1,
  which(registre$etape=="PIPELINE_FIGE") < which(registre$etape=="TEST_DEVERROUILLE"),
  tail(registre$etape,1)=="EVALUATION_FINALE_SAUVEE")
# Rejouer le lanceur renvoie le cache final et ne refait jamais de prédictions TEST.
fichiers <- c("resultats/ml/registre.csv","resultats/ml/evaluation_finale.rds","resultats/ml/pipeline_fige.rds")
empreintes <- tools::md5sum(fichiers)
d <- donnees_uci[c(import_uci$colonnes,"provenance")]
r2 <- entrainer_experience_ml(d,m$types,m$config)
stopifnot(identical(r,r2),identical(empreintes,tools::md5sum(fichiers)))
# Toute tentative de changer la configuration après TEST est rejetée.
config_change <- m$config; config_change$min_specificite <- .8
stopifnot(inherits(try(entrainer_experience_ml(d,m$types,config_change),silent=TRUE),"try-error"),
  identical(empreintes,tools::md5sum(fichiers)))
html <- as.character(source("ui.R")$value)
stopifnot(grepl('data-value="machine_learning"',html,fixed=TRUE))
for (g in list(graphique_cv_ml(r),graphique_confusion_ml(r),graphique_roc_ml(r),
  graphique_calibration_ml(r),graphique_centres_ml(r))) invisible(ggplot2::ggplot_build(g))
shiny::testServer(serveur,{
  stopifnot(grepl("184 TEST",output$ml_etat,fixed=TRUE))
  for (nom in c("ml_diagramme","ml_cv","ml_table_cv","ml_pipeline_retenu","ml_table_imputations",
    "ml_metriques_test","ml_confusion","ml_roc","ml_table_centres","ml_centres"))
    stopifnot(!is.null(output[[nom]]))
  stopifnot(grepl("faux négatifs",output$ml_interpretation,fixed=TRUE))
})
stopifnot(identical(empreintes,tools::md5sum(fichiers)))
cat("Validation ML : split verrouillé, apprentissage par fold, 5 candidats d'imputation, OOF, modèle/seuil figés, TEST unique, quatre pages et graphiques.\n")
