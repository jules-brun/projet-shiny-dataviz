# Premières visualisations (onglet Données) : une question = un graphe + un texte.
# Données nettoyées, sans imputation.

questions_apercu <- c(
  "Qui sont les patients ?" = "age_sexe",
  "Quelle mesure sépare malades et sains ?" = "quanti_diag",
  "Le risque augmente-t-il avec une mesure ?" = "tendance",
  "Quels profils sont les plus à risque ?" = "quali_diag",
  "Le diagnostic varie-t-il selon le centre ?" = "diagnostic_centre",
  "Quelle est la distribution d'une variable ?" = "variable")

# Libellés des codes UCI (voir dataset/heart-disease.names)
modalites_uci <- list(
  sexe = c("0" = "Femme", "1" = "Homme"),
  type_doul_thor = c("1" = "Angine typique", "2" = "Angine atypique",
                     "3" = "Douleur non angineuse", "4" = "Asymptomatique"),
  glyc_jeun_elevee = c("0" = "≤ 120 mg/dL", "1" = "> 120 mg/dL"),
  ecg_repos = c("0" = "Normal", "1" = "Anomalie ST-T", "2" = "Hypertrophie VG"),
  angine_effort = c("0" = "Non", "1" = "Oui"),
  pente_st = c("1" = "Montante", "2" = "Plate", "3" = "Descendante"),
  nb_vaisseaux = c("0" = "0", "1" = "1", "2" = "2", "3" = "3"),
  test_thallium = c("3" = "Normal", "6" = "Défaut fixe", "7" = "Défaut réversible"))

couleurs_diag <- c("Absence" = "#78A9DF", "Présence" = "#EF6473")

# Pas utilisé pour l'odds ratio (10 ans, 10 mmHg...)
pas_or <- c(age = 10, pa_repos = 10, cholesterol = 10, fc_max = 10, depress_st = 1)

etiqueter <- function(x, variable) {
  lab <- modalites_uci[[variable]]
  factor(unname(lab[as.character(x)]), levels = unname(lab))
}

groupe_diag <- function(x) factor(x, levels = 0:1, labels = c("Absence", "Présence"))

nb <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = ",")
ecrire_p <- function(p) if (is.na(p)) "p non calculable" else
  if (p < 0.001) "p < 0,001" else paste0("p = ", nb(p, 3))

# Delta de Cliff : P(X > Y) - P(X < Y), seuils de Romano et al. (2006)
delta_cliff <- function(x, y) mean(sign(outer(x, y, "-")))
taille_cliff <- function(d) cut(abs(d), c(-Inf, .147, .33, .474, Inf),
  labels = c("négligeable", "faible", "moyen", "fort"))

taille_cramer <- function(v) cut(v, c(-Inf, .1, .3, .5, Inf),
  labels = c("négligeable", "faible", "moyen", "fort"))

# Classes d'une mesure pour la question "tendance" (quintiles)
classes_mesure <- function(x, n = 5) {
  bornes <- unique(quantile(x, seq(0, 1, length.out = n + 1), na.rm = TRUE))
  cut(x, bornes, include.lowest = TRUE, dig.lab = 4)
}

alerte_effectif <- function(n) if (n < 20)
  paste0(" Attention : seulement ", n, " patients, résultats très fragiles.") else ""


# ---- Graphes ----------------------------------------------------------------

graphe_apercu <- function(d, question, var_quanti, var_quali, variable,
                          libelles, categories, sources) {
  d$groupe <- groupe_diag(d$diagnostic)

  if (question == "age_sexe") {
    d <- d[!is.na(d$age) & !is.na(d$sexe) & !is.na(d$groupe), ]
    validate(need(nrow(d) > 0, "Aucun patient dans cette sélection."))
    d$classe <- cut(d$age, seq(25, 80, 5), right = FALSE)
    b <- as.data.frame(table(classe = d$classe, sexe = etiqueter(d$sexe, "sexe"),
                             groupe = d$groupe))
    # femmes à gauche (valeurs négatives), hommes à droite
    b$n_signe <- ifelse(b$sexe == "Femme", -b$Freq, b$Freq)
    lim <- max(tapply(b$Freq, list(b$classe, b$sexe), sum), na.rm = TRUE)
    return(ggplot(b, aes(n_signe, classe, fill = groupe)) +
      geom_col(width = .85) +
      geom_vline(xintercept = 0, colour = palette_heart$muted) +
      annotate("text", x = c(-lim, lim) * .8, y = Inf, vjust = 1.5,
               label = c("Femmes", "Hommes"), colour = palette_heart$text, fontface = "bold") +
      scale_x_continuous(labels = abs, limits = c(-lim, lim) * 1.05) +
      scale_fill_manual(values = couleurs_diag) +
      labs(x = "Nombre de patients", y = "Classe d'âge (ans)", fill = "Maladie cardiaque") +
      theme_heart(12) + theme(legend.position = "bottom"))
  }

  if (question == "quanti_diag") {
    d <- d[!is.na(d[[var_quanti]]) & !is.na(d$groupe), ]
    validate(need(nrow(d) > 0, "Aucune valeur observée dans cette sélection."))
    return(ggplot(d, aes(groupe, .data[[var_quanti]], fill = groupe)) +
      geom_violin(alpha = .55, colour = NA, trim = TRUE) +
      geom_boxplot(width = .14, outlier.shape = NA, fill = palette_heart$card,
                   colour = palette_heart$text) +
      scale_fill_manual(values = couleurs_diag, guide = "none") +
      labs(x = "Maladie cardiaque", y = libelles[[var_quanti]],
           caption = "Violon = densité des valeurs ; boîte = médiane et quartiles.") +
      theme_heart(12))
  }

  if (question == "tendance") {
    d <- d[!is.na(d[[var_quanti]]) & !is.na(d$diagnostic), ]
    validate(need(nrow(d) >= 10, "Pas assez de patients pour faire des classes."))
    d$classe <- classes_mesure(d[[var_quanti]])
    b <- do.call(rbind, lapply(split(d$diagnostic, d$classe), function(y) {
      ic <- suppressWarnings(prop.test(sum(y), length(y))$conf.int)
      data.frame(n = length(y), taux = mean(y), bas = ic[1], haut = ic[2])
    }))
    b$classe <- factor(rownames(b), levels = levels(d$classe))
    return(ggplot(b, aes(classe, taux, group = 1)) +
      geom_line(colour = palette_heart$red, linewidth = .8) +
      geom_errorbar(aes(ymin = bas, ymax = haut), width = .15, colour = palette_heart$muted) +
      geom_point(size = 3, colour = palette_heart$red) +
      geom_text(aes(y = 0, label = paste0("n = ", n)), vjust = -.3,
                colour = palette_heart$muted, size = 3.2) +
      scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
      labs(x = paste(libelles[[var_quanti]], "(classes de même effectif)"),
           y = "Part de patients malades",
           caption = "Barres : intervalle de confiance à 95 %.") +
      theme_heart(12))
  }

  if (question == "quali_diag") {
    d <- d[!is.na(d[[var_quali]]) & !is.na(d$groupe), ]
    validate(need(nrow(d) > 0, "Aucune valeur observée dans cette sélection."))
    d$modalite <- etiqueter(d[[var_quali]], var_quali)
    b <- as.data.frame(table(modalite = d$modalite, groupe = d$groupe))
    b <- b[b$modalite %in% unique(d$modalite), ]
    b$pct <- b$Freq / ave(b$Freq, b$modalite, FUN = sum)
    effectifs <- aggregate(Freq ~ modalite, b, sum)
    return(ggplot(b, aes(modalite, Freq, fill = groupe)) +
      geom_col(position = "fill", width = .65) +
      geom_text(aes(label = ifelse(Freq > 0, paste0(round(100 * pct), " %"), "")),
                position = position_fill(vjust = .5), colour = palette_heart$bg, size = 3.6) +
      geom_text(data = effectifs, aes(modalite, 1, label = paste0("n = ", Freq)),
                inherit.aes = FALSE, vjust = -.6, colour = palette_heart$muted, size = 3.2) +
      scale_fill_manual(values = couleurs_diag) +
      scale_y_continuous(labels = scales::label_percent(), expand = expansion(mult = c(0, .1))) +
      labs(x = libelles[[var_quali]], y = "Part des patients", fill = "Maladie cardiaque") +
      theme_heart(12) + theme(legend.position = "bottom"))
  }

  if (question == "diagnostic_centre") {
    d <- d[!is.na(d$groupe), ]
    validate(need(nrow(d) > 0, "Aucun diagnostic renseigné dans cette sélection."))
    d$centre <- factor(d$provenance, levels = names(sources))
    b <- as.data.frame(table(centre = d$centre, groupe = d$groupe))
    b <- b[b$centre %in% unique(d$centre), ]
    b$pct <- b$Freq / ave(b$Freq, b$centre, FUN = sum)
    return(ggplot(b, aes(centre, Freq, fill = groupe)) +
      geom_col(position = "fill", width = .65) +
      geom_text(aes(label = ifelse(Freq > 0, paste0(round(100 * pct), " %"), "")),
                position = position_fill(vjust = .5), colour = palette_heart$bg, size = 3.6) +
      scale_x_discrete(labels = sources) +
      scale_fill_manual(values = couleurs_diag) +
      scale_y_continuous(labels = scales::label_percent()) +
      labs(x = NULL, y = "Part des diagnostics renseignés", fill = "Maladie cardiaque",
           caption = "Présence = code UCI original supérieur à 0.") +
      theme_heart(12) + theme(legend.position = "bottom"))
  }

  # question == "variable" : distribution simple
  x <- d[[variable]]
  x <- x[!is.na(x)]
  validate(need(length(x) > 0, "Aucune valeur observée pour cette variable dans cette sélection."))
  if (variable %in% c(categories, "diagnostic")) {
    valeurs <- if (variable == "diagnostic") groupe_diag(x) else etiqueter(x, variable)
    ggplot(data.frame(valeur = valeurs), aes(valeur)) +
      geom_bar(fill = palette_heart$blue, width = .65) +
      labs(x = libelles[[variable]], y = "Patients", caption = "Valeurs manquantes exclues.") +
      theme_heart(12)
  } else {
    ggplot(data.frame(valeur = x), aes(valeur)) +
      geom_histogram(bins = 25, fill = palette_heart$blue, colour = palette_heart$card) +
      labs(x = libelles[[variable]], y = "Patients", caption = "Valeurs manquantes exclues.") +
      theme_heart(12)
  }
}


# ---- Textes "Ce que montre ce graphe" --------------------------------------
# Une phrase fixe (ce qu'on regarde) + une phrase calculée sur la sélection.

texte_apercu <- function(d, question, var_quanti, var_quali, variable,
                         libelles, categories, sources) {

  if (question == "age_sexe") {
    d <- d[!is.na(d$age) & !is.na(d$sexe), ]
    if (!nrow(d)) return("Aucun patient avec l'âge et le sexe renseignés.")
    h <- d$age[d$sexe == 1]; f <- d$age[d$sexe == 0]
    fixe <- "La pyramide compare la structure par âge des femmes et des hommes, et la part de malades dans chaque classe."
    dyn <- paste0(" Âge et sexe renseignés pour ", nrow(d), " patients, dont ",
      nb(100 * mean(d$sexe == 1)), " % d'hommes.")
    if (length(h) && length(f)) {
      w <- suppressWarnings(wilcox.test(h, f)$p.value)
      dyn <- paste0(dyn, " Âge médian : ", median(f), " ans chez les femmes, ", median(h),
        " ans chez les hommes (Wilcoxon, ", ecrire_p(w), ").")
      dd <- d[!is.na(d$diagnostic), ]
      if (length(unique(dd$diagnostic)) == 2) {
        p <- suppressWarnings(chisq.test(table(dd$sexe, dd$diagnostic))$p.value)
        dyn <- paste0(dyn, " Malades : ", nb(100 * mean(dd$diagnostic[dd$sexe == 0])),
          " % des femmes contre ", nb(100 * mean(dd$diagnostic[dd$sexe == 1])),
          " % des hommes (χ², ", ecrire_p(p), ").")
      }
    }
    return(paste0(fixe, dyn, alerte_effectif(nrow(d))))
  }

  if (question == "quanti_diag") {
    d <- d[!is.na(d[[var_quanti]]) & !is.na(d$diagnostic), ]
    x <- d[[var_quanti]][d$diagnostic == 1]; y <- d[[var_quanti]][d$diagnostic == 0]
    fixe <- "On compare la distribution de la mesure entre patients malades et sains."
    if (!length(x) || !length(y)) return(paste(fixe, "Il faut des malades et des sains dans la sélection."))
    p <- suppressWarnings(wilcox.test(x, y)$p.value)
    dc <- delta_cliff(x, y)
    dec <- if (var_quanti == "depress_st") 1 else 0
    sens <- if (dc > 0) "plus élevées" else "plus basses"
    dyn <- paste0(" ", libelles[[var_quanti]], " : médiane ", nb(median(x), dec), " chez les malades contre ",
      nb(median(y), dec), " chez les sains (n = ", length(x), " et ", length(y), "). Wilcoxon : ", ecrire_p(p),
      ". Delta de Cliff = ", nb(dc, 2), ", effet ", taille_cliff(dc),
      if (abs(dc) >= .147) paste0(" : les valeurs sont ", sens, " chez les malades.") else ".")
    return(paste0(fixe, dyn, alerte_effectif(nrow(d))))
  }

  if (question == "tendance") {
    d <- d[!is.na(d[[var_quanti]]) & !is.na(d$diagnostic), ]
    fixe <- "On découpe la mesure en 5 classes de même effectif et on regarde la part de malades dans chacune."
    if (nrow(d) < 10 || length(unique(d$diagnostic)) < 2) return(paste(fixe, "Pas assez de patients dans la sélection."))
    d$classe <- droplevels(classes_mesure(d[[var_quanti]]))
    k <- tapply(d$diagnostic, d$classe, sum); n <- tapply(d$diagnostic, d$classe, length)
    p <- suppressWarnings(prop.trend.test(k, n)$p.value)
    m <- glm(d$diagnostic ~ d[[var_quanti]], family = binomial)
    pas <- pas_or[[var_quanti]]
    or <- exp(coef(m)[2] * pas)
    ic <- exp((coef(m)[2] + c(-1, 1) * 1.96 * sqrt(vcov(m)[2, 2])) * pas)
    dyn <- paste0(" La part de malades passe de ", nb(100 * k[1] / n[1], 0), " % dans la première classe à ",
      nb(100 * k[length(k)] / n[length(n)], 0), " % dans la dernière (Cochran-Armitage, ", ecrire_p(p),
      "). Odds ratio pour +", pas, " : ", nb(or, 2), " [", nb(ic[1], 2), " ; ", nb(ic[2], 2), "]",
      if (ic[1] > 1) ", le risque augmente." else if (ic[2] < 1) ", le risque diminue." else ", pas de tendance nette.")
    return(paste0(fixe, dyn, alerte_effectif(nrow(d))))
  }

  if (question == "quali_diag") {
    d <- d[!is.na(d[[var_quali]]) & !is.na(d$diagnostic), ]
    fixe <- "Chaque barre représente 100 % des patients d'une modalité ; on compare la part de malades."
    tab <- table(etiqueter(d[[var_quali]], var_quali), d$diagnostic)
    tab <- tab[rowSums(tab) > 0, , drop = FALSE]
    if (nrow(tab) < 2 || ncol(tab) < 2) return(paste(fixe, "Il faut au moins deux modalités et deux diagnostics."))
    khi <- suppressWarnings(chisq.test(tab, correct = FALSE))
    if (any(khi$expected < 5)) {
      p <- fisher.test(tab, simulate.p.value = nrow(tab) > 2, B = 10000)$p.value
      test <- "Fisher"
    } else {
      p <- khi$p.value
      test <- "χ²"
    }
    v <- sqrt(unname(khi$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
    taux <- tab[, "1"] / rowSums(tab)
    dyn <- paste0(" Modalité la plus touchée : ", names(which.max(taux)), " (", nb(100 * max(taux), 0),
      " % de malades), la moins touchée : ", names(which.min(taux)), " (", nb(100 * min(taux), 0),
      " %). ", test, " : ", ecrire_p(p), ", V de Cramér = ", nb(v, 2), " (lien ", taille_cramer(v), ").")
    if (var_quali == "type_doul_thor")
      dyn <- paste0(dyn, " À noter : les patients asymptomatiques sont les plus souvent malades.")
    return(paste0(fixe, dyn, alerte_effectif(nrow(d))))
  }

  if (question == "diagnostic_centre") {
    d <- d[!is.na(d$diagnostic), ]
    if (!nrow(d)) return("Aucun diagnostic renseigné dans cette sélection.")
    fixe <- "On compare la part de malades d'un centre à l'autre."
    parts <- vapply(intersect(names(sources), unique(d$provenance)), function(src) {
      x <- d$diagnostic[d$provenance == src]
      paste0(sources[[src]], " : ", nb(100 * mean(x)), " % (", sum(x), "/", length(x), ")")
    }, character(1))
    dyn <- paste0(" Parmi les ", nrow(d), " diagnostics renseignés : ", paste(parts, collapse = " ; "),
      ". Les centres ne recrutent pas les mêmes patients, il faudra en tenir compte dans les modèles.")
    return(paste0(fixe, dyn, alerte_effectif(nrow(d))))
  }

  # question == "variable"
  valeurs <- d[[variable]]
  x <- valeurs[!is.na(valeurs)]
  if (!length(x)) return(paste0("Aucune valeur observée pour ", libelles[[variable]], "."))
  debut <- paste0(libelles[[variable]], " : ", length(x), " valeurs observées et ",
                  sum(is.na(valeurs)), " manquantes.")
  if (variable %in% c(categories, "diagnostic")) {
    lab <- if (variable == "diagnostic") groupe_diag(x) else etiqueter(x, variable)
    freq <- sort(table(lab), decreasing = TRUE)
    paste0(debut, " Modalité la plus fréquente : ", names(freq)[1], " (n = ", freq[[1]], ").")
  } else {
    paste0(debut, " Médiane = ", nb(median(x)), ", intervalle interquartile = ",
           nb(quantile(x, .25)), " – ", nb(quantile(x, .75)), ".")
  }
}
