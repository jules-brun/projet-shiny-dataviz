# Trois graphes statiques résumant le projet pour un public de cliniciens.
# Exécution depuis la racine du projet :
# Rscript graphes_statiques/graphes_statiques.R
# Sorties : graphes_statiques/1_donnees_manquantes.png, 2_signes_cliniques.png,
#           3_modele_sur_patients.png

suppressPackageStartupMessages(library(ggplot2))
invisible(capture.output(suppressWarnings(source("R/import.R", encoding = "UTF-8"))))
dossier <- "graphes_statiques"

# Palette commune : deux teintes validées (bleu = sain / pas d'alerte,
# orange = malade / alerte), encres neutres, fond clair imprimable.
couleurs <- list(bleu = "#2a78d6", orange = "#eb6834", gris = "#c9c8c2",
  texte = "#0b0b0b", second = "#52514e", grille = "#e6e5e0", fond = "#fcfcfb")

theme_rapport <- function(base_size = 15) {
  theme_minimal(base_size = base_size) + theme(
    text = element_text(colour = couleurs$texte),
    plot.background = element_rect(fill = couleurs$fond, colour = NA),
    panel.grid.major = element_line(colour = couleurs$grille, linewidth = .3),
    panel.grid.minor = element_blank(),
    axis.text = element_text(colour = couleurs$second),
    axis.title = element_text(colour = couleurs$second),
    plot.title = element_text(face = "bold", size = rel(1.25), margin = margin(b = 6)),
    plot.subtitle = element_text(colour = couleurs$second, margin = margin(b = 14), lineheight = 1.1),
    plot.caption = element_text(colour = couleurs$second, hjust = 0, size = rel(.75),
      margin = margin(t = 14), lineheight = 1.1),
    plot.title.position = "plot", plot.caption.position = "plot",
    strip.text = element_text(face = "bold", hjust = 0, size = rel(1)),
    legend.position = "top", legend.justification = "left",
    plot.margin = margin(18, 22, 14, 16))
}

centres <- c(cleveland = "Cleveland", hungarian = "Hongrie",
  switzerland = "Suisse", va = "VA Long Beach")
heart$malade <- as.integer(heart$diagnostic > 0)

# 1. Données manquantes par centre et par variable --------------------------
# Message : les examens les plus informatifs n'existent presque qu'à Cleveland,
# et les centres diffèrent aussi par leur proportion de malades.
libelles_var <- c(age = "Âge", sexe = "Sexe", type_doul_thor = "Type de douleur thoracique",
  pa_repos = "Pression artérielle au repos", cholesterol = "Cholestérol",
  glyc_jeun_elevee = "Glycémie à jeun > 120", ecg_repos = "ECG au repos",
  fc_max = "Fréquence cardiaque max.", angine_effort = "Angine à l'effort",
  depress_st = "Dépression du segment ST", pente_st = "Pente du segment ST",
  nb_vaisseaux = "Vaisseaux colorés (fluoroscopie)", test_thallium = "Test au thallium")

na <- do.call(rbind, lapply(names(centres), function(src) {
  d <- heart[heart$provenance == src, ]
  data.frame(provenance = src, variable = names(libelles_var),
    pct = vapply(names(libelles_var), function(v) mean(is.na(d[[v]])) * 100, numeric(1)))
}))
ordre_var <- names(sort(tapply(na$pct, na$variable, mean)))
na$variable <- factor(libelles_var[na$variable], levels = rev(libelles_var[ordre_var]))
entetes <- vapply(names(centres), function(src) {
  d <- heart[heart$provenance == src, ]
  sprintf("%s\n%d patients\n%d %% malades", centres[[src]], nrow(d), round(100 * mean(d$malade)))
}, character(1))
na$centre <- factor(entetes[na$provenance], levels = entetes)
na$etiquette <- ifelse(na$pct == 0, "·", ifelse(na$pct < 1, "< 1 %", sprintf("%.0f %%", na$pct)))
na$encre <- ifelse(na$pct >= 55, "clair", "fonce")

g1 <- ggplot(na, aes(centre, variable)) +
  geom_tile(aes(fill = pct), colour = couleurs$fond, linewidth = 1.5) +
  geom_text(aes(label = etiquette, colour = encre), size = 4.3) +
  scale_fill_gradient(low = "#f1f0ec", high = "#16467f", limits = c(0, 100),
    name = "Valeurs manquantes", labels = function(x) paste0(x, " %"),
    guide = guide_colourbar(barwidth = 14, barheight = .7, title.vjust = .9)) +
  scale_colour_manual(values = c(clair = "#ffffff", fonce = couleurs$texte), guide = "none") +
  scale_x_discrete(position = "top") +
  labs(x = NULL, y = NULL,
    title = "Les examens les plus informatifs n'existent presque qu'à Cleveland",
    subtitle = paste0("Part des patients sans la mesure, par centre (918 patients, 4 hôpitaux).\n",
      "Le thallium, la fluoroscopie et la pente ST manquent chez 50 à 99 % des patients hors Cleveland ;\n",
      "le cholestérol n'a jamais été mesuré en Suisse, où 93 % des patients sont malades."),
    caption = paste0("Lecture : une case foncée = un examen rarement disponible dans ce centre. ",
      "Un point = aucune valeur manquante.\nLes cholestérols et pressions égaux à 0 sont comptés comme manquants.\nSource : UCI Heart Disease (Janosi et al., 1989), après retrait de 2 doublons.")) +
  theme_rapport() +
  theme(panel.grid.major = element_blank(), axis.text.x.top = element_text(
    colour = couleurs$texte, face = "bold", lineheight = 1.1),
    axis.text.y = element_text(colour = couleurs$texte))
ggsave(file.path(dossier, "1_donnees_manquantes.png"), g1, width = 11, height = 8.5, dpi = 200, bg = couleurs$fond)

# 2. Proportion de malades selon les signes cliniques -----------------------
# Message : quelques signes simples, disponibles partout, séparent déjà fortement
# les malades des non-malades. Un point = une modalité ; la ligne = prévalence globale.
groupes <- list(
  "Douleur thoracique" = list(var = "type_doul_thor",
    mod = c("1" = "Angine typique", "2" = "Angine atypique", "3" = "Non angineuse", "4" = "Asymptomatique")),
  "Angine à l'effort" = list(var = "angine_effort", mod = c("0" = "Non", "1" = "Oui")),
  "Dépression ST à l'effort" = list(var = "depress_st_cl", mod = c(non = "Aucune (0 mm)", oui = "Présente (> 0 mm)")),
  "Fréquence cardiaque max." = list(var = "fc_max_cl",
    mod = c(bas = "< 120 bpm", moyen = "120–150 bpm", haut = "> 150 bpm")),
  "Sexe" = list(var = "sexe", mod = c("0" = "Femme", "1" = "Homme")),
  "Âge" = list(var = "age_cl", mod = c(jeune = "< 50 ans", moyen = "50–59 ans", vieux = "≥ 60 ans")))
heart$depress_st_cl <- ifelse(is.na(heart$depress_st), NA, ifelse(heart$depress_st > 0, "oui", "non"))
heart$fc_max_cl <- as.character(cut(heart$fc_max, c(-Inf, 119.5, 150, Inf), labels = c("bas", "moyen", "haut")))
heart$age_cl <- as.character(cut(heart$age, c(-Inf, 49.5, 59.5, Inf), labels = c("jeune", "moyen", "vieux")))

signes <- do.call(rbind, lapply(names(groupes), function(g) {
  v <- groupes[[g]]$var; m <- groupes[[g]]$mod
  do.call(rbind, lapply(names(m), function(k) {
    d <- heart[!is.na(heart[[v]]) & as.character(heart[[v]]) == k, ]
    data.frame(groupe = g, modalite = m[[k]], n = nrow(d), pct = 100 * mean(d$malade))
  }))
}))
prevalence <- 100 * mean(heart$malade)
ecart <- vapply(names(groupes), function(g) diff(range(signes$pct[signes$groupe == g])), numeric(1))
signes$groupe <- factor(signes$groupe, levels = names(sort(ecart, decreasing = TRUE)))
signes$modalite <- factor(signes$modalite, levels = rev(unlist(lapply(groupes, `[[`, "mod"))))
signes$sens <- ifelse(signes$pct >= prevalence, "Plus de malades que la moyenne", "Moins de malades que la moyenne")

g2 <- ggplot(signes, aes(pct, modalite)) +
  geom_vline(xintercept = prevalence, colour = couleurs$second, linewidth = .5, linetype = "22") +
  geom_segment(aes(x = prevalence, xend = pct, yend = modalite, colour = sens), linewidth = 1.1) +
  geom_point(aes(colour = sens), size = 4, stroke = 1.5, shape = 21, fill = couleurs$fond) +
  geom_point(aes(colour = sens), size = 2.6) +
  geom_text(aes(x = 103, label = sprintf("%.0f %%", pct)), hjust = 0, size = 4.1, colour = couleurs$texte) +
  geom_text(aes(x = 118, label = paste0("n = ", n)), hjust = 1, size = 3.5, colour = couleurs$second) +
  geom_text(data = data.frame(groupe = factor(levels(signes$groupe)[1], levels(signes$groupe))),
    aes(x = prevalence, y = Inf, label = sprintf("Moyenne : %.0f %%", prevalence)),
    inherit.aes = FALSE, vjust = -.6, size = 3.6, colour = couleurs$second) +
  facet_grid(groupe ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_continuous(limits = c(0, 118), breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, " %"), expand = c(0, 0)) +
  scale_colour_manual(values = c("Plus de malades que la moyenne" = couleurs$orange,
    "Moins de malades que la moyenne" = couleurs$bleu), name = NULL) +
  coord_cartesian(clip = "off") +
  labs(x = "Part de patients atteints d'une maladie coronarienne", y = NULL,
    title = "Une douleur « asymptomatique » ou une angine à l'effort : 8 patients sur 10 sont malades",
    subtitle = paste0("Proportion de malades selon des signes recueillis en consultation ou à l'épreuve d'effort,\n",
      "classés du signe le plus discriminant (en haut) au moins discriminant."),
    caption = paste0("Lecture : chez les patients sans douleur thoracique (asymptomatiques), 79 % sont malades, contre 55 % en moyenne.\n",
      "Associations descriptives, non causales : la part de malades varie aussi fortement selon le centre (de 46 % à 93 %).\n",
      "Seuls les patients pour qui le signe est connu sont comptés (n). Source : UCI Heart Disease, 918 patients.")) +
  theme_rapport() +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0, hjust = 1, vjust = 1,
      face = "bold", margin = margin(t = 6, r = 10)),
    panel.grid.major.y = element_blank(), panel.spacing.y = unit(10, "pt"),
    axis.text.y = element_text(colour = couleurs$texte))
ggsave(file.path(dossier, "2_signes_cliniques.png"), g2, width = 12, height = 9, dpi = 200, bg = couleurs$fond)

# 3. Le modèle final sur les patients jamais vus ----------------------------
# Message en fréquences naturelles : un carré = un patient du jeu TEST.
evaluation <- readRDS("resultats/ml/evaluation_finale.rds")
pred <- evaluation$test$predictions
seuil <- evaluation$pipeline$seuil
par_ligne <- 12
grille <- function(sous, titre) {
  sous <- sous[order(-sous$prediction), ]
  i <- seq_len(nrow(sous)) - 1
  data.frame(bloc = titre, x = i %% par_ligne, y = -(i %/% par_ligne),
    alerte = ifelse(sous$prediction == 1, "Alerte du modèle", "Pas d'alerte"))
}
malades <- pred[pred$diagnostic == 1, ]; sains <- pred[pred$diagnostic == 0, ]
tp <- sum(malades$prediction == 1); fp <- sum(sains$prediction == 1)
titres <- c(sprintf("%d patients malades\n→ %d signalés, %d manqués", nrow(malades), tp, nrow(malades) - tp),
  sprintf("%d patients non malades\n→ %d rassurés, %d fausses alertes", nrow(sains), nrow(sains) - fp, fp))
carres <- rbind(grille(malades, titres[1]), grille(sains, titres[2]))
carres$bloc <- factor(carres$bloc, levels = titres)
m <- evaluation$test$metrics

g3 <- ggplot(carres, aes(x, y, fill = alerte)) +
  geom_tile(width = .86, height = .86, colour = NA) +
  facet_wrap(~ bloc, nrow = 1) +
  scale_fill_manual(values = c("Alerte du modèle" = couleurs$orange, "Pas d'alerte" = couleurs$gris),
    name = NULL) +
  coord_equal() +
  labs(x = NULL, y = NULL,
    title = sprintf("Le modèle repère %d malades sur %d, mais alerte à tort plus d'un non-malade sur trois",
      tp, nrow(malades)),
    subtitle = paste0(sprintf("Prédictions sur %d patients mis de côté dès le départ et jamais utilisés pour l'apprentissage.\n", nrow(pred)),
      sprintf("Un carré = un patient. Le seuil d'alerte (probabilité ≥ %s) a été fixé pour manquer le moins de malades possible.",
        format(seuil, nsmall = 2, decimal.mark = ","))),
    caption = paste0(sprintf("Sensibilité %.0f %% · spécificité %.0f %% · AUC %s. Modèle : XGBoost sur 13 variables cliniques, données manquantes imputées.\n",
      100 * m[["sensitivity"]], 100 * m[["specificity"]],
      format(round(m[["roc_auc"]], 2), decimal.mark = ",")),
      "Usage envisagé : aide au tri avant examens complémentaires, pas un diagnostic. Performances inégales selon le centre\n",
      "(à VA Long Beach, presque tous les non-malades reçoivent une alerte).")) +
  theme_rapport() +
  theme(axis.text = element_blank(), panel.grid.major = element_blank(),
    strip.text = element_text(size = rel(1.05), lineheight = 1.15, margin = margin(b = 8)),
    panel.spacing.x = unit(36, "pt"))
ggsave(file.path(dossier, "3_modele_sur_patients.png"), g3, width = 12, height = 7.5, dpi = 200, bg = couleurs$fond)
