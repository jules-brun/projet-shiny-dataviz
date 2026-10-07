# Projet Heart Disease — étape 1 : importation et diagnostic des NA
# Source : https://archive.ics.uci.edu/dataset/45/heart+disease
# Janosi, Steinbrunn, Pfisterer & Detrano (1989), DOI: 10.24432/C52P4X.
# Données sous licence CC BY 4.0 ; documentation dans dataset/heart-disease.names.
#
# Exécution dans une session R ouverte à la racine du projet :
# source("R/import.R", encoding = "UTF-8")
# Importation et tableaux : R de base. Graphique : package ggplot2.
# À installer une seule fois si nécessaire : install.packages("ggplot2")
# Une occurrence par profil répété dans une même source ; zéros ciblés en NA.
# Aucun fichier source n’est modifié et aucune valeur n’est imputée.

# 1. Sélection explicite : une seule version par source -------------------
# Ne pas importer tous les fichiers .data : plusieurs sont des variantes
# des mêmes observations. Le fichier brut cleveland.data est signalé corrompu.
fichiers <- c(
  cleveland   = "processed.cleveland.data",
  hungarian   = "processed.hungarian.data",
  switzerland = "processed.switzerland.data",
  va          = "processed.va.data"  # VA Medical Center, Long Beach
)

# Correspondance des noms UCI vers des noms français abrégés.
# Les noms informatiques n'ont pas d'accents ; les libellés du graphique en ont.
dictionnaire <- data.frame(
  nom_uci = c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg",
              "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num"),
  nom_fr = c("age", "sexe", "type_doul_thor", "pa_repos", "cholesterol",
             "glyc_jeun_elevee", "ecg_repos", "fc_max", "angine_effort",
             "depress_st", "pente_st", "nb_vaisseaux", "test_thallium", "diagnostic"),
  libelle = c("Âge (ans)", "Sexe", "Type de douleur thoracique",
              "Pression artérielle au repos (mmHg)", "Cholestérol (mg/dL)",
              "Glycémie à jeun > 120 mg/dL", "ECG au repos",
              "Fréquence cardiaque maximale", "Angine à l'effort",
              "Dépression du segment ST", "Pente du segment ST",
              "Nombre de vaisseaux visualisés", "Résultat du test au thallium",
              "Diagnostic (code 0–4)"),
  stringsAsFactors = FALSE
)
colonnes <- dictionnaire$nom_fr

# Les valeurs et catégories UCI restent inchangées : seul le nom est traduit.
# sexe, type_doul_thor, glyc_jeun_elevee, ecg_repos, angine_effort,
# pente_st et test_thallium sont des catégories codées numériquement.
# diagnostic conserve les codes 0 à 4, sans regroupement binaire.
# nb_vaisseaux : nombre de vaisseaux majeurs visualisés par fluoroscopie.
# thal ne désigne pas ici une variable de thalassémie.

# 2. Importation de chaque fichier ---------------------------------------
importer_source <- function(provenance, dossier = "dataset") {
  chemin <- file.path(dossier, fichiers[[provenance]])
  if (!file.exists(chemin)) {
    stop("Fichier introuvable : ", chemin,
         "\nOuvrir R à la racine du projet. Dossier actuel : ", getwd())
  }

  donnees <- read.table(
    file = chemin,
    header = FALSE,              # Les fichiers n'ont pas d'en-tête.
    sep = ",", dec = ".",
    na.strings = "?",           # Marqueur manquant des fichiers processed.
    colClasses = "numeric",     # Une valeur inattendue provoque une erreur.
    strip.white = TRUE,
    comment.char = "", quote = "",
    fill = FALSE                # Ne pas compléter une ligne mal formée.
  )
  if (ncol(donnees) != length(colonnes)) {
    stop("Nombre de colonnes inattendu dans ", chemin)
  }
  names(donnees) <- colonnes
  donnees$provenance <- provenance
  donnees
}

donnees_par_source <- lapply(names(fichiers), importer_source)
names(donnees_par_source) <- names(fichiers)

# 3. Fusion verticale : empiler les lignes, pas joindre sur des mesures ---
heart <- do.call(rbind, donnees_par_source)
rownames(heart) <- NULL

# Seuls les zéros de pa_repos et cholesterol seront recodés plus bas.
# Les zéros valides des autres variables sont conservés.
# La documentation des fichiers bruts mentionne -9 comme marqueur manquant ;
# on le signale s'il apparaît ici, sans décider silencieusement d'un recodage.
if (any(as.matrix(heart[colonnes]) == -9, na.rm = TRUE)) {
  warning("Présence de -9 : vérifier leur signification avant recodage.")
}

# 3 bis. Recherche de doublons AVANT tout traitement des NA ---------------
# Une ligne identique n'est pas nécessairement le même patient : aucun
# identifiant patient fiable n'est fourni dans ces fichiers processed.
# On compare les 14 colonnes, cible comprise. Les NA aux mêmes positions
# sont considérés identiques par duplicated() : les données incomplètes
# peuvent donc produire des profils identiques sans prouver un doublon patient.

# A. Au sein de chaque provenance (répétitions après la première occurrence).
doublons_par_source <- do.call(rbind, lapply(names(fichiers), function(src) {
  d <- donnees_par_source[[src]][colonnes]
  membres <- duplicated(d) | duplicated(d, fromLast = TRUE)
  data.frame(
    provenance = src,
    n_lignes = nrow(d),
    n_repetitions = sum(duplicated(d)),
    n_lignes_concernees = sum(membres)
  )
}))
rownames(doublons_par_source) <- NULL

# B. Après fusion, exclure provenance de la comparaison : sinon deux lignes
# identiques provenant de centres différents ne seraient pas détectées.
profils <- heart[colonnes]
est_doublon <- duplicated(profils) | duplicated(profils, fromLast = TRUE)

# Clé des 14 valeurs numériques séparées par | ; NA reste un marqueur explicite.
# Sert uniquement à regrouper les lignes identiques pour les examiner.
cles <- do.call(paste, c(profils, sep = "|"))
groupes <- match(cles, unique(cles))
nb_sources <- vapply(split(heart$provenance, groupes),
                    function(x) length(unique(x)), integer(1))
est_inter_source <- unname(nb_sources[as.character(groupes)]) > 1

# Numéro de ligne dans chaque fichier source pour retrouver les observations.
lignes_source <- unlist(lapply(donnees_par_source, function(d) seq_len(nrow(d))),
                        use.names = FALSE)
details <- data.frame(
  ligne_fusion = seq_len(nrow(heart)),
  ligne_source = lignes_source,
  groupe_profil = groupes,
  inter_source = est_inter_source,
  heart
)
doublons_details <- details[est_doublon, ]
doublons_details <- doublons_details[
  order(doublons_details$groupe_profil, doublons_details$provenance), ]
doublons_inter_sources <- doublons_details[doublons_details$inter_source, ]

cat("\nProfils identiques au sein de chaque source :\n")
print(doublons_par_source, row.names = FALSE)
cat("\nRépétitions globales après la première occurrence : ",
    sum(duplicated(profils)), "\n", sep = "")
cat("Nombre de profils présents dans plusieurs sources : ",
    sum(nb_sources > 1), "\n", sep = "")
cat("\nToutes les lignes impliquées, première occurrence comprise :\n")
print(doublons_details, row.names = FALSE)
# 3 ter. Nettoyage traçable ----------------------------------------------
# Retirer les occurrences après la première dans CHAQUE source, sur les
# 14 valeurs originales. Détecter avant recodage évite de créer de faux
# doublons en confondant un zéro initial et un NA initial.
heart_brut <- heart
retirer <- duplicated(heart[c(colonnes, "provenance")])
details$decision <- ifelse(retirer, "Retirée", "Conservée")
doublons_details <- details[est_doublon, ]
doublons_details <- doublons_details[order(doublons_details$groupe_profil), ]
doublons_supprimes <- details[retirer, ]
heart <- heart[!retirer, ]
rownames(heart) <- NULL
heart_avant_recodage <- heart

# Règle analytique : zéro n'est pas une mesure exploitable de pression au
# repos ou de cholestérol total dans cette cohorte. Le traiter comme indisponible.
# Les sources physiologiques motivent ce choix, sans prouver que les auteurs
# d'UCI utilisaient systématiquement zéro comme code de donnée manquante.
# Références détaillées et liens dans l'onglet Valeurs manquantes de Shiny.
variables_zero_na <- c("pa_repos", "cholesterol")
journal_recodage <- do.call(rbind, lapply(names(fichiers), function(src) {
  d <- heart_avant_recodage[heart_avant_recodage$provenance == src, ]
  data.frame(
    provenance = src, variable = variables_zero_na, n_observations = nrow(d),
    n_na_initiaux = vapply(d[variables_zero_na], function(x) sum(is.na(x)), integer(1)),
    n_zeros_recodes = vapply(d[variables_zero_na], function(x) sum(x == 0, na.rm = TRUE), integer(1)),
    row.names = NULL
  )
}))
for (variable in variables_zero_na) {
  est_zero <- !is.na(heart[[variable]]) & heart[[variable]] == 0
  heart[[variable]][est_zero] <- NA_real_
}
journal_recodage$n_na_finaux <- journal_recodage$n_na_initiaux + journal_recodage$n_zeros_recodes
journal_recodage$pct_na_finaux <- 100 * journal_recodage$n_na_finaux / journal_recodage$n_observations

# Reconstituer la liste : tous les résumés suivants utilisent le même nettoyage.
donnees_par_source <- setNames(lapply(names(fichiers), function(src) {
  heart[heart$provenance == src, ]
}), names(fichiers))
bilan_nettoyage <- data.frame(
  n_brut = nrow(heart_brut), n_doublons_retires = sum(retirer),
  n_final = nrow(heart), n_zeros_recodes = sum(journal_recodage$n_zeros_recodes)
)
# Décomposer les NA pour visualiser la contribution du recodage.
na_origine <- rbind(
  data.frame(variable = colonnes, origine = "NA initiaux",
             nombre = unname(colSums(is.na(heart_avant_recodage[colonnes])))),
  data.frame(variable = colonnes, origine = "Zéros recodés",
             nombre = unname(colSums(is.na(heart[colonnes])) -
                             colSums(is.na(heart_avant_recodage[colonnes]))))
)
na_origine$pct <- 100 * na_origine$nombre / nrow(heart)
cat("\nBilan du nettoyage :\n")
print(bilan_nettoyage, row.names = FALSE)
print(journal_recodage, row.names = FALSE)
cat("\nAperçu après nettoyage :\n")
print(head(heart), row.names = FALSE)


# 4. Bilan global : nombre et pourcentage de NA par variable --------------
bilan_na <- function(donnees) {
  nb_na <- colSums(is.na(donnees[colonnes]))
  data.frame(
    variable = colonnes,
    n_observations = nrow(donnees),
    n_na = unname(nb_na),
    pct_na = 100 * unname(nb_na) / nrow(donnees),
    row.names = NULL
  )
}

na_global <- bilan_na(heart)

# 5. Bilan croisé variable x provenance ----------------------------------
# Pour chaque source, le dénominateur est SON nombre de lignes.
# Exemple : 10 NA / 200 observations = 5 %, et non 10 / 920.
na_par_provenance <- do.call(rbind, lapply(names(fichiers), function(src) {
  bilan <- bilan_na(donnees_par_source[[src]])
  data.frame(provenance = src, bilan, row.names = NULL)
}))
rownames(na_par_provenance) <- NULL

# Résumé complémentaire par source : distinguer cellules et lignes.
na_sources <- do.call(rbind, lapply(names(fichiers), function(src) {
  d <- donnees_par_source[[src]][colonnes]
  masque <- is.na(d)
  data.frame(
    provenance = src,
    n_observations = nrow(d),
    n_cellules = nrow(d) * ncol(d),
    n_na = sum(masque),
    pct_cellules_na = 100 * mean(masque),
    n_lignes_avec_na = sum(rowSums(masque) > 0),
    pct_lignes_avec_na = 100 * mean(rowSums(masque) > 0),
    n_lignes_completes = sum(rowSums(masque) == 0)
  )
}))
rownames(na_sources) <- NULL

# Arrondir seulement l'affichage : conserver la précision dans les objets.
afficher_bilan <- function(x) {
  colonnes_pct <- grepl("^pct_", names(x))
  x[colonnes_pct] <- lapply(x[colonnes_pct], round, digits = 2)
  print(x, row.names = FALSE)
}

cat("\nNA par variable, toutes provenances réunies :\n")
afficher_bilan(na_global)
cat("\nNA par variable ET provenance :\n")
afficher_bilan(na_par_provenance)
cat("\nRésumé des NA par provenance :\n")
afficher_bilan(na_sources)

# Objets réutilisables ensuite dans Shiny :
# heart : tableau fusionné ; donnees_par_source : liste des quatre tableaux.
# na_global, na_par_provenance, na_sources : diagnostics pour tables/graphes.
# provenance est une information de source, pas une mesure clinique.
# Les NA observés ne suffisent pas à identifier leur mécanisme (MCAR/MAR/MNAR).
# 6. Carte de chaleur : % de NA par variable et provenance ----------------
# Un pourcentage permet de comparer des centres de tailles différentes.
# Échelle commune fixe 0–100 % ; annotations pour lire les valeurs exactes.
# Fonction réutilisable dans un futur renderPlot() de Shiny.
graphique_na_provenance <- function(bilan = na_par_provenance) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop('Installer ggplot2 avec install.packages("ggplot2").')
  }
  d <- bilan
  d$variable <- factor(d$variable, levels = rev(colonnes))
  effectifs <- vapply(donnees_par_source, nrow, integer(1))
  d$provenance <- factor(d$provenance, levels = names(fichiers))
  etiquettes_sources <- setNames(
    paste0(c("Cleveland", "Hongrie", "Suisse", "VA Long Beach"),
           "\n(n = ", effectifs, ")"), names(fichiers)
  )
  d$etiquette <- sprintf("%.1f %%", d$pct_na)
  d$texte_clair <- d$pct_na >= 55

  ggplot2::ggplot(d, ggplot2::aes(x = provenance, y = variable, fill = pct_na)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = etiquette, color = texte_clair),
                       size = 3.5, show.legend = FALSE) +
    ggplot2::scale_color_manual(values = c("FALSE" = "#18212B", "TRUE" = "white")) +
    ggplot2::scale_fill_gradient(low = "#F1F5F9", high = "#08306B",
                                 limits = c(0, 100), breaks = seq(0, 100, 25),
                                 name = "% de NA") +
    ggplot2::scale_x_discrete(labels = etiquettes_sources, drop = FALSE) +
    ggplot2::scale_y_discrete(labels = setNames(dictionnaire$libelle,
                                               dictionnaire$nom_fr), drop = FALSE) +
    ggplot2::labs(
      title = "Les valeurs manquantes selon la provenance",
      subtitle = "Pourcentage calculé dans chaque source, variable par variable",
      x = NULL, y = NULL,
      caption = "Source : UCI Heart Disease. Après dédoublonnage et recodage ciblé des zéros.\nNA initiaux + zéros de pression au repos et de cholestérol ; aucune imputation."
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(color = "#18212B"))
}

if (requireNamespace("ggplot2", quietly = TRUE)) {
  graphe_na <- graphique_na_provenance()
  print(graphe_na)
} else {
  message('Tableaux disponibles. Pour le graphique : install.packages("ggplot2"), puis relancer le script.')
}
