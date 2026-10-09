# ACM des variables qualitatives selon la gestion des NA :
# cas complets, imputation simple (imputeMCA) ou multiple (MIMCA) avec ellipses.
# Le diagnostic n'est jamais utilisé pour imputer ; il est supplémentaire dans l'ACM.

vars_acm <- c("sexe", "type_doul_thor", "glyc_jeun_elevee", "ecg_repos",
              "angine_effort", "pente_st", "nb_vaisseaux", "test_thallium")
vars_acm_lourdes <- c("nb_vaisseaux", "test_thallium")  # plus de 50 % de NA

noms_courts_acm <- c(sexe = "Sexe", type_doul_thor = "Douleur", glyc_jeun_elevee = "Glycémie",
  ecg_repos = "ECG", angine_effort = "Angine effort", pente_st = "Pente ST",
  nb_vaisseaux = "Vaisseaux", test_thallium = "Thallium")

libelles_centres_acm <- c(cleveland = "Cleveland", hungarian = "Hongrie",
                          switzerland = "Suisse", va = "VA Long Beach")

modes_acm <- c("Cas complets" = "complets",
               "Imputation simple (ACM régularisée)" = "simple",
               "Imputation multiple (MIMCA) + ellipses" = "multiple")

# Facteurs avec des modalités lisibles et uniques ("Pente ST : Plate")
tableau_acm <- function(d, vars) {
  x <- as.data.frame(lapply(vars, function(v) {
    f <- etiqueter(d[[v]], v)
    factor(paste(noms_courts_acm[[v]], ":", f), levels = paste(noms_courts_acm[[v]], ":", levels(f)))
  }))
  names(x) <- vars
  droplevels(x)
}

# Coordonnées des modalités = barycentre des individus / racine de la valeur propre
barycentres <- function(coord, x, eig) {
  do.call(rbind, lapply(names(x), function(v) {
    do.call(rbind, lapply(levels(x[[v]]), function(m) {
      ok <- x[[v]] == m
      data.frame(modalite = m, variable = v,
                 x = mean(coord[ok, 1]) / sqrt(eig[1]), y = mean(coord[ok, 2]) / sqrt(eig[2]))
    }))
  }))
}

calculer_acm <- function(donnees, mode, avec_lourdes = FALSE, nboot = 20, graine = 20261009) {
  vars <- if (avec_lourdes) vars_acm else setdiff(vars_acm, vars_acm_lourdes)
  d <- donnees[!is.na(donnees$diagnostic), ]
  x <- tableau_acm(d, vars)
  diag <- factor(d$diagnostic, 0:1, c("Diagnostic : Absence", "Diagnostic : Présence"))
  centre <- factor(d$provenance)
  masque <- is.na(x)
  ncp <- NA

  if (mode == "complets") {
    garder <- complete.cases(x)
    x <- droplevels(x[garder, ]); diag <- diag[garder]; centre <- droplevels(centre[garder])
    masque <- masque[garder, , drop = FALSE]
  } else {
    # La provenance aide à imputer (les NA dépendent du centre), pas le diagnostic
    xi <- cbind(x, provenance = centre)
    set.seed(graine)
    ncp <- missMDA::estim_ncpMCA(xi, ncp.min = 1, ncp.max = 5, nbsim = 10, verbose = FALSE)$ncp
    x <- missMDA::imputeMCA(xi, ncp = ncp)$completeObs[vars]
  }

  ref <- FactoMineR::MCA(data.frame(x, diagnostic = diag, provenance = centre),
                         quali.sup = length(vars) + 1:2, ncp = 2, graph = FALSE)
  eig <- ref$eig[1:2, 1]
  modalites <- data.frame(modalite = rownames(ref$var$coord),
    variable = rep(vars, vapply(x, nlevels, integer(1))),
    x = ref$var$coord[, 1], y = ref$var$coord[, 2])
  sup <- data.frame(modalite = levels(diag), x = ref$quali.sup$coord[levels(diag), 1],
                    y = ref$quali.sup$coord[levels(diag), 2])

  nuages <- NULL
  if (mode == "multiple") {
    set.seed(graine)
    mi <- missMDA::MIMCA(cbind(tableau_acm(d, vars), provenance = centre), ncp = ncp, nboot = nboot)
    # Chaque jeu imputé est projeté sur l'ACM de référence
    nuages <- do.call(rbind, lapply(seq_along(mi$res.MI), function(m) {
      xm <- mi$res.MI[[m]][vars]
      xm[] <- lapply(vars, function(v) factor(xm[[v]], levels = levels(x[[v]])))
      coord <- predict(ref, newdata = xm)$coord
      b <- barycentres(coord, cbind(xm, diagnostic = diag), eig)
      b$jeu <- m
      b
    }))
    # Points = position moyenne sur les jeux imputés (centre des ellipses)
    moy <- aggregate(cbind(x, y) ~ modalite, nuages, mean)
    i <- match(modalites$modalite, moy$modalite)
    modalites[c("x", "y")] <- moy[i, c("x", "y")]
    i <- match(sup$modalite, moy$modalite)
    sup[c("x", "y")] <- moy[i, c("x", "y")]
  }

  list(mode = mode, vars = vars, n = nrow(x), n_total = nrow(d), ncp = ncp, nboot = nboot,
       masque = masque, ref = ref, modalites = modalites, sup = sup, nuages = nuages,
       individus = data.frame(x = ref$ind$coord[, 1], y = ref$ind$coord[, 2], diagnostic = diag,
                              impute = rowSums(masque) > 0),
       vars_na = vars[colSums(masque) > 0], centres = table(centre))
}


# ---- Graphes ----------------------------------------------------------------

axes_titres <- function(res) list(
  x = sprintf("Dim 1 (%s %%)", nb(res$ref$eig[1, 2])),
  y = sprintf("Dim 2 (%s %%)", nb(res$ref$eig[2, 2])))

couleurs_vars_acm <- c(sexe = "#78A9DF", type_doul_thor = "#64C4B2", glyc_jeun_elevee = "#E5B36C",
  ecg_repos = "#BC9ACB", angine_effort = "#9FB6CB", pente_st = "#D4CB9E",
  nb_vaisseaux = "#7FAB9C", test_thallium = "#C98B6B")

graphe_acm <- function(res, vue) {
  ax <- axes_titres(res)
  base <- ggplot() +
    geom_hline(yintercept = 0, colour = "#40536C") + geom_vline(xintercept = 0, colour = "#40536C")

  if (vue == "individus") {
    ind <- res$individus
    ind$forme <- ifelse(ind$impute, "Au moins une valeur imputée", "Valeurs observées")
    return(base +
      geom_point(data = ind, aes(x, y, colour = diagnostic, shape = forme), alpha = .5, size = 1.8) +
      scale_colour_manual(values = setNames(unname(couleurs_diag), levels(ind$diagnostic))) +
      scale_shape_manual(values = c("Valeurs observées" = 16, "Au moins une valeur imputée" = 4)) +
      labs(x = ax$x, y = ax$y, colour = NULL, shape = NULL) +
      coord_equal() + theme_heart(12) + theme(legend.position = "bottom", legend.box = "vertical"))
  }

  mod <- res$modalites
  mod$libelle <- sub("^.*? : ", "", mod$modalite)
  g <- base
  if (!is.null(res$nuages)) {
    nu <- res$nuages
    g <- g + stat_ellipse(data = nu[nu$variable != "diagnostic", ],
        aes(x, y, group = modalite, fill = variable, colour = variable), geom = "polygon",
        level = .95, alpha = .2, linewidth = .3) +
      stat_ellipse(data = nu[nu$variable == "diagnostic", ],
        aes(x, y, group = modalite), level = .95, colour = palette_heart$text, linetype = 2)
  }
  g +
    geom_point(data = mod, aes(x, y, colour = variable), size = 2.6) +
    ggrepel::geom_text_repel(data = mod, aes(x, y, label = libelle, colour = variable),
      size = 3.2, seed = 1, max.overlaps = Inf, show.legend = FALSE) +
    geom_point(data = res$sup, aes(x, y), shape = 23, size = 4, fill = c(couleurs_diag),
               colour = palette_heart$text) +
    ggrepel::geom_label_repel(data = res$sup, aes(x, y, label = sub("Diagnostic : ", "", modalite)),
      fill = palette_heart$panel, colour = palette_heart$text, size = 3.4, seed = 2,
      fontface = "bold", label.size = 0) +
    scale_colour_manual(values = couleurs_vars_acm, labels = noms_courts_acm, aesthetics = c("colour", "fill")) +
    labs(x = ax$x, y = ax$y, colour = NULL, fill = NULL,
         caption = if (!is.null(res$nuages)) paste0("Ellipses à 95 % sur ", res$nboot,
           " jeux imputés. Pointillés : diagnostic (supplémentaire).") else
           "Losanges : diagnostic, en supplémentaire (ne construit pas les axes).") +
    coord_equal() + theme_heart(12) + theme(legend.position = "bottom")
}


# ---- Texte ------------------------------------------------------------------

texte_acm <- function(res, vue) {
  fixe <- switch(res$mode,
    complets = "Cas complets : on garde uniquement les patients sans aucune valeur manquante sur les variables de l'ACM.",
    simple = "Imputation simple : chaque NA est remplacé par une seule valeur, prédite par une ACM régularisée (missMDA). Le graphe ne montre pas l'incertitude de ces valeurs.",
    multiple = "Imputation multiple : on crée plusieurs jeux de données imputés (MIMCA) et on les projette sur la même ACM. Plus une ellipse est grande, plus la position de la modalité dépend des valeurs imputées.")

  n_txt <- paste0(" ", res$n, " patients analysés sur ", res$n_total, ".")
  if (res$mode == "complets") {
    part <- round(100 * res$centres / sum(res$centres))
    part <- part[part > 0]
    n_txt <- paste0(n_txt, " Répartition par centre : ",
      paste0(libelles_centres_acm[names(part)], " ", part, " %", collapse = ", "), ".")
  } else {
    n_txt <- paste0(n_txt, " ", sum(res$masque), " valeurs imputées (", nb(100 * mean(res$masque)),
      " % des cellules), avec ", res$ncp, " dimensions choisies par validation croisée.")
  }

  inertie <- paste0(" Le plan résume ", nb(sum(res$ref$eig[1:2, 2])), " % de l'inertie.")

  # Modalités les plus proches de "Présence" sur le plan
  pres <- unlist(res$sup[res$sup$modalite == "Diagnostic : Présence", c("x", "y")])
  dist <- sqrt((res$modalites$x - pres[1])^2 + (res$modalites$y - pres[2])^2)
  proches <- head(res$modalites$modalite[order(dist)], 3)
  lien <- paste0(" Modalités les plus proches des malades : ", paste(proches, collapse = ", "), ".")

  ell <- ""
  if (res$mode == "multiple" && vue == "modalites") {
    nu <- res$nuages[res$nuages$variable %in% res$vars_na, ]
    aire <- vapply(split(nu, nu$modalite), function(z) sqrt(max(det(cov(z[c("x", "y")])), 0)), numeric(1))
    ell <- paste0(" Parmi les variables avec des NA, la modalité la plus incertaine est ",
      names(which.max(aire)), ". Les modalités sans NA bougent un peu aussi, car les patients se déplacent avec les valeurs imputées.")
  }
  if (vue == "individus") lien <- paste0(lien, " Les croix sont les patients avec au moins une valeur imputée.")

  paste0(fixe, n_txt, inertie, lien, ell)
}


# ---- Interface et serveur ---------------------------------------------------

acm_ui <- function() {
  shiny::tagList(
    entete_heart("05 / ACM ET IMPUTATION", "Visualiser l'effet de la gestion des NA",
      "Même ACM, trois façons de traiter les valeurs manquantes."),
    carte_heart("ACM des caractéristiques cliniques",
      bslib::layout_columns(
        shiny::selectInput("acm_mode", "Gestion des NA", choices = modes_acm, selected = "multiple"),
        shiny::radioButtons("acm_vue", "Graphe", inline = TRUE,
          choices = c("Modalités" = "modalites", "Patients" = "individus")),
        shiny::checkboxInput("acm_lourdes", "Inclure nb de vaisseaux et thallium (plus de 50 % de NA)", FALSE),
        col_widths = bslib::breakpoints(xs = 12, lg = c(4, 3, 5)), fill = FALSE),
      shiny::plotOutput("acm_graphe", height = "620px"),
      shiny::div(class = "callout", shiny::strong("Ce que montre ce graphe"), shiny::textOutput("acm_texte")),
      shiny::p(class = "note", "Le diagnostic et la provenance sont en supplémentaire : ils ne construisent pas les axes. La provenance sert à l'imputation car les NA dépendent du centre ; le diagnostic n'y participe jamais."),
      icone = "diagram-3")
  )
}

acm_serveur <- function(input, output, session, donnees) {
  resultat <- shiny::reactive({
    shiny::req(input$acm_mode %in% modes_acm)
    shiny::withProgress(message = "Calcul de l'ACM...",
      calculer_acm(donnees, input$acm_mode, isTRUE(input$acm_lourdes)))
  }) |> shiny::bindCache(input$acm_mode, isTRUE(input$acm_lourdes))
  output$acm_graphe <- shiny::renderPlot(graphe_acm(resultat(), input$acm_vue %||% "modalites"), res = 110)
  output$acm_texte <- shiny::renderText(texte_acm(resultat(), input$acm_vue %||% "modalites"))
}
