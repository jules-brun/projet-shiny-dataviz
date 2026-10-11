acm_modes <- c("Modèles choisis sur Gestion des NA" = "modeles",
  "Cas complets" = "complets", "Imputation simple ACM régularisée" = "simple",
  "Imputation multiple MIMCA" = "multiple")

# Libellés UCI partagés, adaptés des helpers de Jules. Les données gardent leurs codes.
modalites_uci <- list(
  sexe = c('0' = 'Femme', '1' = 'Homme'),
  type_doul_thor = c('1' = 'Angine typique', '2' = 'Angine atypique',
    '3' = 'Douleur non angineuse', '4' = 'Asymptomatique'),
  glyc_jeun_elevee = c('0' = '≤ 120 mg/dL', '1' = '> 120 mg/dL'),
  ecg_repos = c('0' = 'Normal', '1' = 'Anomalie ST-T', '2' = 'Hypertrophie VG'),
  angine_effort = c('0' = 'Non', '1' = 'Oui'),
  pente_st = c('1' = 'Ascendante', '2' = 'Plate', '3' = 'Descendante'),
  nb_vaisseaux = c('0' = '0 vaisseau', '1' = '1 vaisseau', '2' = '2 vaisseaux', '3' = '3 vaisseaux'),
  test_thallium = c('3' = 'Normal', '6' = 'Défaut fixe', '7' = 'Défaut réversible'),
  diagnostic = c('0' = '0', '1' = '1')
)
# Diagnostic : conserver 0/1 dans les graphes pour distinguer clairement les NA.
libelles_variables_acm <- c(sexe = 'Sexe', type_doul_thor = 'Douleur thoracique',
  glyc_jeun_elevee = 'Glycémie à jeun', ecg_repos = 'ECG', angine_effort = 'Angine à l’effort',
  pente_st = 'Pente ST', nb_vaisseaux = 'Vaisseaux', test_thallium = 'Thallium',
  diagnostic = 'Diagnostic', provenance = 'Provenance')

libeller_modalites <- function(x, variable) {
  brut <- as.character(x)
  codes <- modalites_uci[[variable]]
  if (is.null(codes)) return(factor(brut))
  labels <- unname(codes[brut])
  labels[!is.na(brut) & is.na(labels)] <- brut[!is.na(brut) & is.na(labels)]
  presents <- unname(codes[names(codes) %in% brut])
  inconnus <- sort(unique(labels[!is.na(labels) & !labels %in% presents]))
  factor(labels, levels = c(presents, inconnus))
}

pct_fr <- function(x, chiffres = 1L) {
  paste0(formatC(x, format = 'f', digits = chiffres, decimal.mark = ','), ' %')
}

libeller_points_acm <- function(ids) {
  variables <- sub(' :.*$', '', ids)
  codes <- sub('^[^:]+ : ', '', ids)
  valeurs <- vapply(seq_along(ids), function(i) {
    v <- variables[i]; z <- codes[i]
    if (v == 'provenance' && z %in% names(centres_app)) return(unname(centres_app[z]))
    dictionnaire <- modalites_uci[[v]]
    if (!is.null(dictionnaire) && z %in% names(dictionnaire)) unname(dictionnaire[z]) else z
  }, character(1))
  paste0(unname(libelles_variables_acm[variables]), ' : ', valeurs)
}
