# Importation et prétraitement des données Heart Disease (UCI)
# Source : https://archive.ics.uci.edu/dataset/45/heart+disease
# Janosi, Steinbrunn, Pfisterer & Detrano (1989), DOI: 10.24432/C52P4X, licence CC BY 4.0.
# Documentation des variables : dataset/heart-disease.names
#
# Utilisation (depuis le dossier Visu) : source("import_pretraitement.R", encoding = "UTF-8")
# Crée deux objets :
#   heart     : 918 patients x 16 colonnes, nettoyé
#   libelles  : libellés français des variables, pour les axes et légendes
# Étapes : importation des 4 centres (« ? » -> NA), fusion, retrait des doublons
# au sein d'un même centre, zéros de pression et cholestérol -> NA,
# diagnostic binaire. Aucune valeur n'est imputée.

heart <- local({
  fichiers <- c(cleveland   = "processed.cleveland.data",
                hungarian   = "processed.hungarian.data",
                switzerland = "processed.switzerland.data",
                va          = "processed.va.data")  # VA Medical Center, Long Beach
  colonnes <- c("age", "sexe", "type_doul_thor", "pa_repos", "cholesterol",
                "glyc_jeun_elevee", "ecg_repos", "fc_max", "angine_effort",
                "depress_st", "pente_st", "nb_vaisseaux", "test_thallium", "diagnostic")

  # 1. Importation : fichiers sans en-tête, « ? » = valeur manquante
  importer <- function(provenance) {
    chemin <- file.path("dataset", fichiers[[provenance]])
    if (!file.exists(chemin)) stop("Fichier introuvable : ", chemin, " (dossier actuel : ", getwd(), ")")
    d <- read.table(chemin, header = FALSE, sep = ",", na.strings = "?",
                    colClasses = "numeric", strip.white = TRUE, col.names = colonnes)
    d$provenance <- provenance
    d
  }
  d <- do.call(rbind, lapply(names(fichiers), importer))

  # 2. Doublons : on retire les lignes répétées au sein d'un même centre,
  # avant recodage pour ne pas confondre un zéro initial et un NA initial
  d <- d[!duplicated(d[c(colonnes, "provenance")]), ]

  # 3. Une pression artérielle ou un cholestérol égal à 0 n'est pas une mesure : NA
  for (v in c("pa_repos", "cholesterol")) d[[v]][d[[v]] %in% 0] <- NA

  # 4. Diagnostic binaire : 0 = pas de maladie, 1 à 4 = maladie coronarienne
  d$diagnostic_0_4 <- d$diagnostic
  d$diagnostic <- as.integer(d$diagnostic > 0)

  d$provenance <- factor(d$provenance, levels = names(fichiers))
  rownames(d) <- NULL
  d
})

libelles <- c(age = "Âge (ans)", sexe = "Sexe", type_doul_thor = "Type de douleur thoracique",
  pa_repos = "Pression artérielle au repos (mmHg)", cholesterol = "Cholestérol (mg/dL)",
  glyc_jeun_elevee = "Glycémie à jeun > 120 mg/dL", ecg_repos = "ECG au repos",
  fc_max = "Fréquence cardiaque maximale", angine_effort = "Angine à l'effort",
  depress_st = "Dépression du segment ST", pente_st = "Pente du segment ST",
  nb_vaisseaux = "Vaisseaux colorés (fluoroscopie)", test_thallium = "Test au thallium",
  diagnostic = "Maladie coronarienne (0 = non, 1 = oui)")
