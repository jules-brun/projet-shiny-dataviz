# ============================================================
# AUDIT AVANT MODÉLISATION
# Données attendues : tableau nettoyé, avant toute imputation.
# Aucune modification de l'objet d'origine.
# ============================================================

# Copie indépendante : heart reste inchangé.
donnees <- as.data.frame(heart)

# Vérifier les colonnes nécessaires.
colonnes_profil <- c(
  "provenance", "age", "sexe", "type_doul_thor",
  "pa_repos", "cholesterol", "glyc_jeun_elevee",
  "ecg_repos", "fc_max", "angine_effort",
  "depress_st", "pente_st", "nb_vaisseaux",
  "test_thallium", "diagnostic"
)

stopifnot(all(colonnes_profil %in% names(donnees)))

# Si disponible, utiliser le diagnostic original pour identifier
# les profils identiques avant regroupement en diagnostic binaire.
if ("diagnostic_initial" %in% names(donnees)) {
  colonnes_profil[colonnes_profil == "diagnostic"] <-
    "diagnostic_initial"
}

# Appliquer la convention de dédoublonnage déjà choisie :
# même centre ET mêmes valeurs sur les 14 variables.
# Cela identifie des profils identiques, pas des identités prouvées.
doublon <- duplicated(donnees[, colonnes_profil, drop = FALSE])

cat("Profils identiques retirés :", sum(doublon), "\n")
donnees <- donnees[!doublon, , drop = FALSE]

# Appliquer uniquement les deux règles de recodage convenues.
# Aucun recodage automatique des zéros des autres variables.
for (variable in c("pa_repos", "cholesterol")) {

  x <- donnees[[variable]]

  if (!is.numeric(x)) {
    stop(variable, " doit être numérique avant le recodage.")
  }

  a_recoder <- !is.na(x) & x == 0

  cat(
    variable, ":",
    sum(a_recoder), "zéro(s) transformé(s) en NA\n"
  )

  donnees[[variable]][a_recoder] <- NA_real_
}

rownames(donnees) <- NULL

# Noms utilisés dans notre projet
variables_cliniques <- c(
  "age", "sexe", "type_doul_thor", "pa_repos",
  "cholesterol", "glyc_jeun_elevee", "ecg_repos",
  "fc_max", "angine_effort", "depress_st",
  "pente_st", "nb_vaisseaux", "test_thallium"
)

colonnes_attendues <- c(
  "provenance", "diagnostic", variables_cliniques
)

absentes <- setdiff(colonnes_attendues, names(donnees))

if (length(absentes) > 0L) {
  stop(
    "Colonnes absentes : ",
    paste(absentes, collapse = ", "),
    "\nAdapte les noms dans ce script à ceux de ton tableau."
  )
}

# Éviter as.numeric(factor), qui retournerait les codes internes.
diagnostic_num <- suppressWarnings(
  as.numeric(as.character(donnees$diagnostic))
)

if (any(!is.na(donnees$diagnostic) & is.na(diagnostic_num))) {
  stop("Le diagnostic doit être codé numériquement : 0/1 ou 0 à 4.")
}

if (any(!is.na(diagnostic_num) &
        !diagnostic_num %in% 0:4)) {
  stop("Valeurs inattendues dans le diagnostic.")
}

# Fonctionne aussi si le diagnostic est déjà binaire.
donnees$diagnostic_binaire <- ifelse(
  is.na(diagnostic_num),
  NA_integer_,
  as.integer(diagnostic_num > 0)
)

donnees$provenance <- as.character(donnees$provenance)

if (anyNA(donnees$provenance)) {
  stop("Certaines lignes n'ont pas de provenance.")
}

centres <- sort(unique(donnees$provenance))

pct <- function(n, total) {
  if (total == 0) NA_real_ else round(100 * n / total, 1)
}

# ------------------------------------------------------------
# 1. EFFECTIFS ET DIAGNOSTIC PAR CENTRE
# ------------------------------------------------------------

effectifs <- do.call(rbind, lapply(centres, function(centre) {

  d <- donnees[donnees$provenance == centre, , drop = FALSE]
  y <- d$diagnostic_binaire
  n_connus <- sum(!is.na(y))

  data.frame(
    provenance = centre,
    n = nrow(d),
    diagnostic_manquant = sum(is.na(y)),
    diagnostic_0 = sum(y == 0, na.rm = TRUE),
    diagnostic_1 = sum(y == 1, na.rm = TRUE),
    pct_diagnostic_1 = pct(sum(y == 1, na.rm = TRUE), n_connus)
  )
}))

# ------------------------------------------------------------
# 2. DISPONIBILITÉ DES VARIABLES PAR CENTRE
# n_modalites_observees permet de repérer une variable constante.
# ------------------------------------------------------------

disponibilite <- do.call(rbind, lapply(centres, function(centre) {

  d <- donnees[donnees$provenance == centre, , drop = FALSE]

  do.call(rbind, lapply(variables_cliniques, function(variable) {

    x <- d[[variable]]

    data.frame(
      provenance = centre,
      variable = variable,
      n = length(x),
      n_observes = sum(!is.na(x)),
      n_na = sum(is.na(x)),
      pct_na = pct(sum(is.na(x)), length(x)),
      n_modalites_observees = length(unique(x[!is.na(x)]))
    )
  }))
}))

# Matrice compacte : variables en lignes, centres en colonnes
matrice_na <- xtabs(
  pct_na ~ variable + provenance,
  data = disponibilite
)

# ------------------------------------------------------------
# 3. SCÉNARIOS DE CAS COMPLETS
# Ce sont des comparaisons descriptives, pas des décisions.
# Diagnostic requis dans chaque scénario.
# ------------------------------------------------------------

scenarios <- list(
  Toutes_variables = variables_cliniques,

  Sans_vaisseaux_thallium = setdiff(
    variables_cliniques,
    c("nb_vaisseaux", "test_thallium")
  ),

  Sans_vaisseaux_thallium_cholesterol = setdiff(
    variables_cliniques,
    c("nb_vaisseaux", "test_thallium", "cholesterol")
  )
)

cas_complets <- do.call(rbind, lapply(names(scenarios), function(s) {

  variables <- c("diagnostic_binaire", scenarios[[s]])

  do.call(rbind, lapply(centres, function(centre) {

    d <- donnees[donnees$provenance == centre, , drop = FALSE]
    retenu <- complete.cases(d[, variables, drop = FALSE])
    y <- d$diagnostic_binaire[retenu]

    data.frame(
      scenario = s,
      provenance = centre,
      n_initial = nrow(d),
      n_retenu = sum(retenu),
      pct_retenu = pct(sum(retenu), nrow(d)),
      diagnostic_0 = sum(y == 0),
      diagnostic_1 = sum(y == 1),
      pct_diagnostic_1 = pct(sum(y == 1), length(y))
    )
  }))
}))

# ------------------------------------------------------------
# 4. NOMBRE DE VALEURS MANQUANTES PAR PATIENT
# ------------------------------------------------------------

n_na_patient <- rowSums(
  is.na(donnees[, variables_cliniques, drop = FALSE])
)

charge_na <- do.call(rbind, lapply(centres, function(centre) {

  x <- n_na_patient[donnees$provenance == centre]

  data.frame(
    provenance = centre,
    minimum = min(x),
    mediane = median(x),
    moyenne = round(mean(x), 2),
    maximum = max(x),
    n_sans_na = sum(x == 0)
  )
}))

# ------------------------------------------------------------
# 5. PRINCIPAUX GROUPES DE VARIABLES MANQUANTES
# ------------------------------------------------------------

motif_na <- apply(
  is.na(donnees[, variables_cliniques, drop = FALSE]),
  1,
  function(x) {
    if (!any(x)) {
      "Aucun NA"
    } else {
      paste(variables_cliniques[x], collapse = " + ")
    }
  }
)

motifs_frequents <- do.call(rbind, lapply(centres, function(centre) {

  frequences <- sort(
    table(motif_na[donnees$provenance == centre]),
    decreasing = TRUE
  )

  frequences <- head(frequences, 5)

  data.frame(
    provenance = centre,
    motif = names(frequences),
    n = as.integer(frequences),
    row.names = NULL
  )
}))

# ------------------------------------------------------------
# 6. DISTRIBUTIONS DES VARIABLES QUANTITATIVES PAR CENTRE
# Aucune discrétisation : on conserve les valeurs numériques.
# ------------------------------------------------------------

variables_quantitatives <- c(
  "age", "pa_repos", "cholesterol", "fc_max", "depress_st"
)

resume_quantitatif <- do.call(rbind, lapply(centres, function(centre) {

  d <- donnees[donnees$provenance == centre, , drop = FALSE]

  do.call(rbind, lapply(variables_quantitatives, function(variable) {

    x <- d[[variable]]

    if (!is.numeric(x)) {
      stop("La variable ", variable, " doit être numérique.")
    }

    x <- x[!is.na(x)]

    quantiles <- if (length(x) > 0L) {
      quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), names = FALSE)
    } else {
      rep(NA_real_, 5)
    }

    data.frame(
      provenance = centre,
      variable = variable,
      n_observes = length(x),
      minimum = quantiles[1],
      q25 = quantiles[2],
      mediane = quantiles[3],
      q75 = quantiles[4],
      maximum = quantiles[5]
    )
  }))
}))

# ------------------------------------------------------------
# 7. RAPPORT : AFFICHAGE ET EXPORT TEXTE
# ------------------------------------------------------------

rapport <- capture.output({

  cat("=== 1. EFFECTIFS ET DIAGNOSTIC ===\n")
  print(effectifs, row.names = FALSE)

  cat("\n=== 2. POURCENTAGES DE NA PAR CENTRE ===\n")
  print(round(matrice_na, 1))

  cat("\n=== 3. CAS COMPLETS SELON LE SCENARIO ===\n")
  print(cas_complets, row.names = FALSE)

  cat("\n=== 4. NOMBRE DE NA PAR PATIENT ===\n")
  print(charge_na, row.names = FALSE)

  cat("\n=== 5. CINQ PRINCIPAUX MOTIFS PAR CENTRE ===\n")
  print(motifs_frequents, row.names = FALSE)

  cat("\n=== 6. DISTRIBUTIONS QUANTITATIVES ===\n")
  print(resume_quantitatif, row.names = FALSE)

  cat("\n=== 7. VARIABLES CONSTANTES OU ENTIEREMENT ABSENTES ===\n")
  print(
    disponibilite[
      disponibilite$n_modalites_observees <= 1,
      c("provenance", "variable", "n_observes",
        "pct_na", "n_modalites_observees")
    ],
    row.names = FALSE
  )
})

cat(rapport, sep = "\n")

dir.create("resultats", showWarnings = FALSE)

writeLines(
  rapport,
  con = "resultats/audit_avant_modelisation.txt",
  useBytes = TRUE
)

# ============================================================
# AUDIT DU PÉRIMÈTRE MULTICENTRIQUE PROPOSÉ
# Aucune suppression ni imputation appliquée.
# ============================================================

variables_principales <- c(
  "age", "sexe", "type_doul_thor", "ecg_repos",
  "pa_repos", "fc_max", "angine_effort", "depress_st"
)

# Fonction descriptive : pourcentage parmi les diagnostics connus.
pourcentage_positifs <- function(y) {
  y <- y[!is.na(y)]
  if (length(y) == 0L) return(NA_real_)
  round(100 * mean(y == 1), 1)
}

audit_principal <- do.call(
  rbind,
  lapply(sort(unique(donnees$provenance)), function(centre) {

    d <- donnees[
      donnees$provenance == centre,
      ,
      drop = FALSE
    ]

    complet <- complete.cases(
      d[, c(variables_principales, "diagnostic_binaire"),
        drop = FALSE]
    )

    n_na <- rowSums(
      is.na(d[, variables_principales, drop = FALSE])
    )

    data.frame(
      provenance = centre,
      n_total = nrow(d),
      n_complets = sum(complet),
      n_incomplets = sum(!complet),
      pct_complets = round(100 * mean(complet), 1),
      n_avec_1_na = sum(n_na == 1),
      n_avec_2_na_ou_plus = sum(n_na >= 2),
      pct_diagnostic_1_complets = pourcentage_positifs(
        d$diagnostic_binaire[complet]
      ),
      pct_diagnostic_1_incomplets = pourcentage_positifs(
        d$diagnostic_binaire[!complet]
      )
    )
  })
)

# Chevauchement des valeurs manquantes :
# diagonale = nombre de NA pour chaque variable ;
# hors diagonale = nombre de patients avec les deux variables manquantes.
indicateurs_na <- is.na(
  donnees[, variables_principales, drop = FALSE]
)

na_simultanes <- crossprod(1L * indicateurs_na)

rapport_principal <- capture.output({
  cat("=== EFFECTIFS DU PERIMETRE PRINCIPAL ===\n")
  print(audit_principal, row.names = FALSE)

  cat("\n=== NOMBRE DE NA SIMULTANES ===\n")
  print(na_simultanes)
})

cat(rapport_principal, sep = "\n")

dir.create("resultats", showWarnings = FALSE)

writeLines(
  rapport_principal,
  "resultats/audit_perimetre_principal.txt"
)