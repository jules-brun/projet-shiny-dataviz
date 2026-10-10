# Premières visualisations (onglet Données) : une question = un graphe + un texte.
# Données nettoyées, sans imputation. Graphes plotly ; libellés et couleurs dans R/graphiques.R.

questions_apercu <- c(
  "Qui sont les patients ?" = "age_sexe",
  "Quelle mesure sépare malades et sains ?" = "quanti_diag",
  "Le risque augmente-t-il avec une mesure ?" = "tendance",
  "Quels profils sont les plus à risque ?" = "quali_diag",
  "Le diagnostic varie-t-il selon le centre ?" = "diagnostic_centre",
  "Quelle est la distribution d'une variable ?" = "variable")

couleurs_diag <- couleurs_diagnostic

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

# Barres 100 % empilées : part de malades par groupe, effectif au survol.
barres_parts <- function(groupe, diagnostic, titre_x) {
  b <- as.data.frame(table(groupe = groupe, diagnostic = diagnostic))
  b <- b[b$groupe %in% unique(groupe), ]
  b$total <- ave(b$Freq, b$groupe, FUN = sum)
  b <- b[b$total > 0, ]
  b$pct <- 100 * b$Freq / b$total
  b$texte <- paste0("<b>", b$groupe, "</b> · ", b$total, " patients<br>", b$diagnostic, " : ",
                    b$Freq, " (", pct_fr(b$pct, 0), ")")
  b$etiquette <- ifelse(b$pct >= 8, pct_fr(b$pct, 0), "")
  totaux <- unique(b[c("groupe", "total")])
  p <- ggplot(b, aes(groupe, pct, fill = diagnostic, text = texte)) +
    geom_col(width = .6, colour = "white", linewidth = .4) +
    geom_text(aes(label = etiquette), position = position_stack(vjust = .5), colour = "white", size = 3.6) +
    geom_text(data = totaux, aes(groupe, 106, label = paste0("n = ", total)), inherit.aes = FALSE,
              colour = couleur_discrete, size = 3.3) +
    scale_fill_manual(values = couleurs_diag) +
    scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 25), labels = function(x) paste0(x, " %")) +
    labs(x = titre_x, y = "Part des patients") + theme_app() +
    theme(panel.grid.major.x = element_blank())
  interactif(p)
}

graphe_apercu <- function(d, question, var_quanti, var_quali, variable,
                          libelles, categories, sources) {
  d$groupe <- groupe_diag(d$diagnostic)

  if (question == "age_sexe") {
    d <- d[!is.na(d$age) & !is.na(d$sexe) & !is.na(d$groupe), ]
    validate(need(nrow(d) > 0, "Aucun patient dans cette sélection."))
    d$classe <- cut(d$age, seq(25, 80, 5), right = FALSE)
    b <- as.data.frame(table(classe = d$classe, sexe = etiqueter(d$sexe, "sexe"), groupe = d$groupe))
    # femmes à gauche (valeurs négatives), hommes à droite
    b$n_signe <- ifelse(b$sexe == "Femme", -b$Freq, b$Freq)
    b$texte <- paste0("<b>", b$sexe, "s de ", sub("\\[(\\d+),(\\d+)\\)", "\\1 à \\2", b$classe),
                      " ans</b><br>", b$groupe, " : ", b$Freq, " patients")
    lim <- max(tapply(b$Freq, list(b$classe, b$sexe), sum), na.rm = TRUE) * 1.1
    p <- ggplot(b, aes(classe, n_signe, fill = groupe, text = texte)) +
      geom_col(width = .85, colour = "white", linewidth = .2) +
      geom_hline(yintercept = 0, colour = couleur_discrete) +
      annotate("text", x = length(levels(b$classe)) + .3, y = c(-lim, lim) * .75,
               label = c("◀ Femmes", "Hommes ▶"), colour = couleur_encre, fontface = "bold") +
      scale_y_continuous(labels = abs, limits = c(-lim, lim)) +
      scale_fill_manual(values = couleurs_diag) +
      coord_flip() + labs(x = "Classe d'âge (ans)", y = "Nombre de patients") + theme_app()
    return(interactif(p))
  }

  if (question == "quanti_diag") {
    d <- d[!is.na(d[[var_quanti]]) & !is.na(d$groupe), ]
    validate(need(nrow(d) > 0, "Aucune valeur observée dans cette sélection."))
    p <- plot_ly(d, x = ~groupe, y = d[[var_quanti]], color = ~groupe, colors = couleurs_diag,
        type = "violin", box = list(visible = TRUE), meanline = list(visible = FALSE),
        points = "outliers", spanmode = "hard", hoveron = "violins+points+kde") |>
      layout(xaxis = list(title = "Maladie cardiaque"),
             yaxis = list(title = libelles[[var_quanti]], zeroline = FALSE, gridcolor = "#EDF2F8"))
    return(habiller(p, legende = FALSE))
  }

  if (question == "tendance") {
    d <- d[!is.na(d[[var_quanti]]) & !is.na(d$diagnostic), ]
    validate(need(nrow(d) >= 10, "Pas assez de patients pour faire des classes."))
    d$classe <- classes_mesure(d[[var_quanti]])
    b <- do.call(rbind, lapply(split(d$diagnostic, d$classe), function(y) {
      ic <- suppressWarnings(prop.test(sum(y), length(y))$conf.int)
      data.frame(n = length(y), k = sum(y), taux = 100 * mean(y), bas = 100 * ic[1], haut = 100 * ic[2])
    }))
    b$classe <- factor(rownames(b), levels = levels(d$classe))
    b$texte <- paste0("<b>", libelles[[var_quanti]], " ", b$classe, "</b><br>", pct_fr(b$taux, 0),
      " de malades (", b$k, "/", b$n, ")<br>IC 95 % : ", pct_fr(b$bas, 0), " – ", pct_fr(b$haut, 0))
    p <- ggplot(b, aes(classe, taux, group = 1)) +
      geom_ribbon(aes(ymin = bas, ymax = haut), fill = "#E88432", alpha = .15) +
      geom_line(colour = "#E88432", linewidth = .9) +
      geom_point(aes(text = texte), size = 3.5, colour = "#E88432") +
      geom_text(aes(y = 3, label = paste0("n = ", n)), colour = couleur_discrete, size = 3.2) +
      scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, " %")) +
      labs(x = paste(libelles[[var_quanti]], "(5 classes de même effectif)"),
           y = "Part de patients malades") + theme_app()
    return(interactif(p, legende = FALSE))
  }

  if (question == "quali_diag") {
    d <- d[!is.na(d[[var_quali]]) & !is.na(d$groupe), ]
    validate(need(nrow(d) > 0, "Aucune valeur observée dans cette sélection."))
    return(barres_parts(droplevels(etiqueter(d[[var_quali]], var_quali)), d$groupe, libelles[[var_quali]]))
  }

  if (question == "diagnostic_centre") {
    d <- d[!is.na(d$groupe), ]
    validate(need(nrow(d) > 0, "Aucun diagnostic renseigné dans cette sélection."))
    centre <- droplevels(factor(unname(sources[d$provenance]), levels = unname(sources)))
    return(barres_parts(centre, d$groupe, NULL))
  }

  # question == "variable" : distribution empilée par diagnostic
  d <- d[!is.na(d[[variable]]) & !is.na(d$groupe), ]
  validate(need(nrow(d) > 0, "Aucune valeur observée pour cette variable dans cette sélection."))
  if (variable %in% c(categories, "diagnostic")) {
    b <- as.data.frame(table(x = libeller_modalites(d[[variable]], variable), diagnostic = d$groupe))
    b <- b[b$Freq > 0, ]
    b$total <- ave(b$Freq, b$x, FUN = sum)
    b$texte <- paste0("<b>", b$x, "</b><br>", b$diagnostic, " : ", b$Freq, " patients (",
                      pct_fr(100 * b$Freq / b$total), " de la modalité)")
    p <- ggplot(b, aes(x, Freq, fill = diagnostic, text = texte)) +
      geom_col(width = .6, colour = "white", linewidth = .4)
  } else {
    bornes <- pretty(range(d[[variable]]), n = 25)
    classe <- cut(d[[variable]], bornes, include.lowest = TRUE, right = FALSE)
    b <- as.data.frame(table(classe = classe, diagnostic = d$groupe))
    i <- as.integer(b$classe)
    b$milieu <- (bornes[i] + bornes[i + 1]) / 2
    b$texte <- paste0("<b>[", bornes[i], " ; ", bornes[i + 1], "[</b><br>", b$diagnostic, " : ",
                      b$Freq, " patients")
    b <- b[b$Freq > 0, ]
    p <- ggplot(b, aes(milieu, Freq, fill = diagnostic, text = texte)) +
      geom_col(width = diff(bornes)[1] * .92, colour = "white", linewidth = .2)
  }
  p <- p + scale_fill_manual(values = couleurs_diag, drop = FALSE) +
    labs(x = libelles[[variable]], y = "Patients") + theme_app()
  interactif(p)
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
