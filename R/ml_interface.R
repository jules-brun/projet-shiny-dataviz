# Page 4 : uniquement visualisation d'une expérience sauvegardée, aucun fit Shiny.
ml_ui <- function() shiny::tagList(
  # Schéma du protocole avec Mermaid (fichier local dans www/)
  shiny::tags$head(shiny::tags$script(src = "mermaid.min.js"),
    shiny::tags$script(shiny::HTML("mermaid.initialize({startOnLoad: false, theme: 'dark',
      themeVariables: {fontSize: '18px', fontFamily: 'Inter, Segoe UI, Arial',
      mainBkg: '#162438', primaryTextColor: '#EEF3FA', nodeBorder: '#78A9DF', lineColor: '#78A9DF',
      clusterBkg: '#111D2D', clusterBorder: '#E5B36C', edgeLabelBackground: '#111D2D'}});"))),
  carte_heart("Machine learning",
    shiny::uiOutput("ml_diagramme"),
    shiny::div(class = "callout", shiny::textOutput("ml_etat")),
    shiny::p(class = "note", "Le diagnostic 1 désigne la maladie. La provenance sert uniquement à stratifier et à examiner la robustesse ; elle est exclue de tous les prédicteurs, y compris ceux des imputations."), icone = "shield-lock"),
  carte_heart("Validation croisée sur TRAIN. Comparaison des modèles",
    shiny::plotOutput("ml_cv", height = "510px"), shiny::div(style="overflow-x:auto;", shiny::tableOutput("ml_table_cv")),
    shiny::p(class = "note", "Chaque point représente un fold ; le losange représente la moyenne. Le tableau donne moyenne ± écart-type. Pour chaque famille, l'hyperparamétrage et le seuil affichés ont été choisis sur les prédictions hors-fold de TRAIN. Ces scores de sélection peuvent être optimistes : seule l'évaluation TEST constitue le bilan indépendant."), icone = "bar-chart"),
  carte_heart("Pipeline final retenu. Configuration figée sur TRAIN", shiny::uiOutput("ml_pipeline_retenu"),
    shiny::tags$details(shiny::tags$summary("Sélection des imputations sur TRAIN : cinq meilleures méthodes par variable"),
      shiny::div(style="overflow-x:auto;", shiny::tableOutput("ml_table_imputations"))),
    shiny::p(class = "note", "Les classements historiques sur toute la cohorte ne sont pas réutilisés pour choisir : ils incluaient le futur TEST. Les familles existantes sont réévaluées à l'intérieur de chaque analysis fold ; les cinq meilleures méthodes valides sont retenues, puis sélectionnées par erreur moyenne + une erreur standard. Le classement final est recalculé sur TRAIN complet. Aucun choix des pages 2 ou 3 n'intervient ici."), icone = "check2-circle"),
  shiny::div(class = "callout", shiny::h3("ÉVALUATION FINALE — TEST jusque-là inédit"),
    shiny::p("TEST n'a servi ni à sélectionner les imputations, ni à choisir le modèle, les hyperparamètres ou le seuil. Les éléments ci-dessous proviennent d'une unique évaluation du pipeline figé.")),
  shiny::uiOutput("ml_metriques_test"),
  bslib::layout_columns(
    carte_heart("Matrice de confusion TEST", shiny::plotOutput("ml_confusion", height = "380px"),
      shiny::div(class = "callout", shiny::textOutput("ml_interpretation")), icone = "grid-3x3"),
    carte_heart("Courbe ROC TEST", shiny::plotOutput("ml_roc", height = "440px"), icone = "graph-up"),
    col_widths = bslib::breakpoints(xs = 12, lg = 6), fill = FALSE),
  carte_heart("Robustesse selon la provenance (après prédiction)", shiny::div(style="overflow-x:auto;", shiny::tableOutput("ml_table_centres")),
    shiny::plotOutput("ml_centres", height = "410px"),
    shiny::p(class = "note", "La sensibilité n'est présentée qu'avec au moins 10 malades, la spécificité avec au moins 10 non-malades. L'AUC nécessite ces deux effectifs et au moins 30 observations. Des cases non définies indiquent une estimation trop fragile. Les barres montrent les IC de Wilson à 95 % de la sensibilité et de la spécificité ; ce graphique ne permet pas de conclure à une homogénéité entre centres."), icone = "geo-alt")
)

# Mêmes étapes que l'ancien schéma ggplot, en Mermaid pour la lisibilité
schema_mermaid_ml <- function() 'flowchart TD
  A["Données nettoyées<br/>Diagnostic binaire"] --> B["Split stratifié<br/>TRAIN 80 % / TEST 20 %"]
  B --> C["TRAIN : CV à 5 folds"]
  C --> D["Pour chaque fold<br/>Masques → imputation → préparation"]
  D --> E["Comparaison ML<br/>Prédictions"]
  E --> F["Sélection sur TRAIN<br/>Modèle + hyperparamètres + seuil"]
  F --> G["Pipeline figé<br/>Réentraînement sur TRAIN complet"]
  G --> H["TEST déverrouillé une fois<br/>Application du pipeline figé"]
  H --> I["Évaluation finale<br/>Résultats enregistrés"]
  B --> T
  subgraph T ["TEST VERROUILLÉ"]
    T1["Aucune sélection<br/>Aucun tuning<br/>Aucun choix de seuil<br/>Provenance exclue des prédicteurs"]
  end
  T --> H'

candidats_affiches_ml <- function(r) {
  z <- r$validation$choix$par_candidat
  do.call(rbind, lapply(split(z, z$modele), function(d) {
    if (any(d$admissible)) d <- d[d$admissible, ]
    d[order(-d$sensitivity, -d$balanced_accuracy, -d$roc_auc, -d$specificity), ][1, ]
  }))$candidat
}
description_candidat_ml <- function(nom) switch(nom,
  logistique = "Régression logistique, tous les prédicteurs, sans sélection de variables",
  enet_ridge = "Elastic net : alpha = 0 ; lambda = 0,03",
  enet_mixte = "Elastic net : alpha = 0,5 ; lambda = 0,03",
  rf_3 = "Random Forest : 400 arbres ; mtry = 3 ; taille minimale de nœud = 5",
  rf_6 = "Random Forest : 400 arbres ; mtry = 6 ; taille minimale de nœud = 5",
  xgb_2 = "XGBoost : profondeur = 2 ; 120 tours ; eta = 0,05 ; sous-échantillonnage = 0,8",
  xgb_4 = "XGBoost : profondeur = 4 ; 120 tours ; eta = 0,05 ; sous-échantillonnage = 0,8",
  svm_05 = "SVM radial : coût = 0,5 ; gamma = 1 / nombre de colonnes préparées",
  svm_2 = "SVM radial : coût = 2 ; gamma = 1 / nombre de colonnes préparées", nom)

nom_metriques_ml <- c(roc_auc = "ROC AUC", sensitivity = "Sensibilité", specificity = "Spécificité",
  balanced_accuracy = "Balanced accuracy", accuracy = "Accuracy", precision = "Précision",
  f1 = "F1")
format_score_ml <- function(x) ifelse(is.finite(x), sprintf("%.3f", x), "Non défini")

graphique_cv_ml <- function(r) {
  d <- r$validation$performances
  d <- d[d$candidat %in% candidats_affiches_ml(r), ]
  vars <- c("sensitivity", "balanced_accuracy", "roc_auc")
  z <- do.call(rbind, lapply(vars, function(v) data.frame(modele=d$modele, fold=d$fold,
    metrique=nom_metriques_ml[[v]], score=d[[v]])))
  ggplot2::ggplot(z, ggplot2::aes(modele,score)) +
    ggplot2::geom_boxplot(fill=palette_heart$blue, alpha=.35, outlier.shape=NA) +
    ggplot2::geom_point(position=ggplot2::position_jitter(width=.08, height=0, seed=1),
      colour=palette_heart$text, size=2) +
    ggplot2::stat_summary(fun=mean, geom="point", shape=18, size=4, colour=palette_heart$amber) +
    ggplot2::facet_wrap(~metrique, nrow=1) + ggplot2::scale_y_continuous(limits=c(0,1)) +
    ggplot2::labs(x=NULL,y="Score sur assessment fold") + theme_heart() +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=35,hjust=1))
}

table_cv_ml <- function(r) {
  z <- r$validation$resume
  z <- z[z$candidat %in% candidats_affiches_ml(r), ]
  tab <- data.frame(Modèle=z$modele, Seuil=r$validation$choix$par_candidat$seuil[match(z$candidat,r$validation$choix$par_candidat$candidat)], check.names=FALSE)
  for (v in names(nom_metriques_ml)) tab[[nom_metriques_ml[[v]]]] <-
    paste0(format_score_ml(z[[v]]), " ± ", format_score_ml(z[[paste0(v,"_sd")]]))
  tab
}

graphique_confusion_ml <- function(r) {
  p <- r$test$predictions
  d <- as.data.frame(table(reel=factor(p$diagnostic,0:1), predit=factor(p$prediction,0:1)))
  d$type <- ifelse(d$reel=="1" & d$predit=="0","FN",ifelse(d$reel=="1","TP",ifelse(d$predit=="1","FP","TN")))
  ggplot2::ggplot(d,ggplot2::aes(predit,reel,fill=type)) +
    ggplot2::geom_tile(colour=palette_heart$card,linewidth=2) +
    ggplot2::geom_text(ggplot2::aes(label=paste0(type,"\n",Freq)),size=6,colour=palette_heart$bg,fontface="bold") +
    ggplot2::scale_fill_manual(values=c(FN=palette_heart$red,FP=palette_heart$amber,TN=palette_heart$blue,TP=palette_heart$teal),guide="none") +
    ggplot2::scale_x_discrete(labels=c("0"="0 · Non malade","1"="1 · Malade")) +
    ggplot2::scale_y_discrete(labels=c("0"="0 · Non malade","1"="1 · Malade")) +
    ggplot2::labs(x="Prédiction",y="Diagnostic réel") + ggplot2::coord_equal() + theme_heart()
}

graphique_roc_ml <- function(r) ggplot2::ggplot(r$test$roc,ggplot2::aes(fpr,tpr)) +
  ggplot2::geom_abline(slope=1,intercept=0,linetype=2,colour=palette_heart$muted) +
  ggplot2::geom_path(colour=palette_heart$red,linewidth=1) +
  ggplot2::coord_equal(xlim=c(0,1),ylim=c(0,1)) +
  ggplot2::labs(x="1 − spécificité",y="Sensibilité",subtitle=paste("AUC TEST =",format_score_ml(r$test$metrics[["roc_auc"]]))) + theme_heart()

graphique_calibration_ml <- function(r) ggplot2::ggplot(r$test$calibration,ggplot2::aes(predite,observee)) +
  ggplot2::geom_abline(slope=1,intercept=0,linetype=2,colour=palette_heart$muted) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=bas,ymax=haut),width=.025,colour=palette_heart$blue) +
  ggplot2::geom_line(colour=palette_heart$blue) + ggplot2::geom_point(colour=palette_heart$blue,size=3) +
  ggplot2::geom_text(ggplot2::aes(label=paste0("n=",n)),nudge_y=.05,colour=palette_heart$text,size=4) +
  ggplot2::coord_equal(xlim=c(0,1),ylim=c(0,1)) +
  ggplot2::labs(x="Probabilité moyenne prédite",y="Fréquence observée de diagnostic 1",
    subtitle=paste("Score de Brier =",format_score_ml(r$test$metrics[["brier"]]))) + theme_heart()

graphique_centres_ml <- function(r) {
  d <- r$test$robustesse
  z <- do.call(rbind,lapply(c("sensitivity","specificity","roc_auc"),function(v)
    data.frame(provenance=d$provenance,metrique=nom_metriques_ml[[v]],score=d[[v]])))
  z$bas <- z$haut <- NA_real_
  # IC de Wilson pour les deux proportions : ne pas masquer la faible précision.
  for (i in seq_len(nrow(z))) if (is.finite(z$score[i]) && z$metrique[i] != "ROC AUC") {
    t <- r$test$predictions[r$test$predictions$provenance == z$provenance[i], ]
    n <- sum(t$diagnostic == if (z$metrique[i] == "Sensibilité") 1 else 0)
    p <- z$score[i]; q <- qnorm(.975)
    centre <- (p+q^2/(2*n))/(1+q^2/n)
    marge <- q*sqrt(p*(1-p)/n+q^2/(4*n^2))/(1+q^2/n)
    z$bas[i] <- centre-marge; z$haut[i] <- centre+marge
  }
  ggplot2::ggplot(z,ggplot2::aes(provenance,score,colour=metrique)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=bas,ymax=haut),width=.12,
      position=ggplot2::position_dodge(width=.45),na.rm=TRUE) +
    ggplot2::geom_point(position=ggplot2::position_dodge(width=.45),size=3,na.rm=TRUE) +
    ggplot2::scale_x_discrete(labels=centres_app) + ggplot2::scale_y_continuous(limits=c(0,1)) +
    ggplot2::labs(x=NULL,y="Performance TEST",colour=NULL) + theme_heart() + ggplot2::theme(legend.position="bottom")
}

ml_serveur <- function(output, donnees) {
  path <- "resultats/ml/evaluation_finale.rds"
  r <- if (file.exists(path)) readRDS(path) else NULL
  if (!is.null(r) && !identical(r$manifeste$donnees, digest::digest(donnees))) r <- NULL
  disponible <- function() {
    shiny::validate(shiny::need(!is.null(r), "Expérience ML non disponible pour ces données. Lancer Rscript scripts/entrainer_ml.R, puis redémarrer Shiny."))
    r
  }
  output$ml_diagramme <- shiny::renderUI(shiny::tagList(
    shiny::div(class = "mermaid", style = "text-align:center;", schema_mermaid_ml()),
    shiny::tags$script("mermaid.run({querySelector: '.mermaid'});")))
  output$ml_etat <- shiny::renderText({
    if (is.null(r)) return("Calcul hors application : Rscript scripts/entrainer_ml.R. Aucun accès à TEST lors de l'affichage de cette page.")
    paste("Expérience enregistrée le",r$date,"·",length(r$manifeste$split$train),"TRAIN /",
      length(r$manifeste$split$test),"TEST · Évaluation finale unique, chargée depuis le disque.")
  })
  output$ml_cv <- shiny::renderPlot(graphique_cv_ml(disponible()),bg=palette_heart$card)
  output$ml_table_cv <- shiny::renderTable(table_cv_ml(disponible()),striped=TRUE,bordered=FALSE,spacing="s")
  output$ml_pipeline_retenu <- shiny::renderUI({
    r <- disponible(); s <- r$pipeline$selection
    b <- r$validation$resume[r$validation$resume$candidat==s$candidat,]
    cfg <- r$validation$config
    shiny::tagList(
      shiny::h4(s$modele),
      shiny::p(description_candidat_ml(s$candidat)),
      shiny::p("Seuil figé sur TRAIN : ",shiny::strong(sprintf("%.3f",r$pipeline$seuil))),
      shiny::p(paste("Règle clinique : maximiser la sensibilité, sous spécificité ≥",cfg$min_specificite,
        "et balanced accuracy ≥",cfg$min_balanced_accuracy,"; départager par balanced accuracy, AUC, puis spécificité.")),
      shiny::p(paste("Validation TRAIN : sensibilité",format_score_ml(b$sensitivity),
        "· balanced accuracy",format_score_ml(b$balanced_accuracy),"· AUC",format_score_ml(b$roc_auc))),
      shiny::tags$ul(lapply(names(r$pipeline$imputations),function(v)
        shiny::tags$li(shiny::strong(v)," → ",r$pipeline$imputations[[v]]))),
      shiny::p(class="note","Tous les réglages sont fixés avant TEST. Les valeurs observées restent intactes ; les paramètres de préparation sont appris sur l'apprentissage uniquement."))
  })
  output$ml_table_imputations <- shiny::renderTable({
    r <- disponible(); t <- r$pipeline$top5
    t[c("variable","modele_imputation","metrique","erreur_moyenne","sd_erreur","nb_repetitions","score_stabilite","retenue")]
  },digits=3,spacing="s")
  output$ml_metriques_test <- shiny::renderUI({
    r <- disponible()
    boxes <- lapply(names(nom_metriques_ml),function(v) bslib::value_box(
      title=nom_metriques_ml[[v]], value=format_score_ml(r$test$metrics[[v]]),
      shiny::p(if(v=="sensitivity") "Priorité : limiter les faux négatifs" else "Évaluation TEST indépendante"),
      theme=bslib::value_box_theme(bg=if(v=="sensitivity") "#46212C" else palette_heart$card,fg=palette_heart$text),fill=FALSE))
    do.call(bslib::layout_columns,c(boxes,list(col_widths=bslib::breakpoints(xs=12,md=6,lg=3),fill=FALSE)))
  })
  output$ml_confusion <- shiny::renderPlot(graphique_confusion_ml(disponible()),bg=palette_heart$card)
  output$ml_interpretation <- shiny::renderText({
    r <- disponible(); fn <- sum(r$test$predictions$diagnostic==1 & r$test$predictions$prediction==0)
    paste(sprintf("%.1f %%",100*r$test$metrics[["sensitivity"]]),"des patients réellement malades sont détectés ;",fn,"faux négatifs sur TEST.")
  })
  output$ml_roc <- shiny::renderPlot(graphique_roc_ml(disponible()),bg=palette_heart$card)
  output$ml_table_centres <- shiny::renderTable({
    d <- disponible()$test$robustesse
    d$provenance <- unname(centres_app[d$provenance])
    for (v in c("sensitivity","specificity","roc_auc")) d[[v]] <- format_score_ml(d[[v]])
    d
  },spacing="s")
  output$ml_centres <- shiny::renderPlot(graphique_centres_ml(disponible()),bg=palette_heart$card)
}
