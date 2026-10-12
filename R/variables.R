# Tableau de présentation des variables (onglet Données)
variables_presentation <- data.frame(
  variable = c("age", "sexe", "type_doul_thor", "pa_repos", "cholesterol", "glyc_jeun_elevee",
    "ecg_repos", "fc_max", "angine_effort", "depress_st", "pente_st", "nb_vaisseaux",
    "test_thallium", "diagnostic"),
  description = c("Âge", "Sexe", "Type de douleur thoracique", "Pression artérielle au repos",
    "Cholestérol sérique", "Glycémie à jeun > 120 mg/dL", "Électrocardiogramme au repos",
    "Fréquence cardiaque maximale à l'effort", "Angine provoquée par l'effort",
    "Dépression du segment ST à l'effort", "Pente du segment ST au pic de l'effort",
    "Nombre de vaisseaux colorés par fluoroscopie", "Test au thallium",
    "Diagnostic (variable à prédire)"),
  type = c("Quantitative", "Qualitative", "Qualitative", "Quantitative", "Quantitative",
    "Qualitative", "Qualitative", "Quantitative", "Qualitative", "Quantitative",
    "Qualitative", "Qualitative", "Qualitative", "Qualitative"),
  valeurs = c("années", "0 = femme, 1 = homme",
    "1 = angine typique, 2 = angine atypique, 3 = douleur non angineuse, 4 = asymptomatique",
    "mmHg", "mg/dL", "0 = non, 1 = oui",
    "0 = normal, 1 = anomalie ST-T, 2 = hypertrophie ventriculaire gauche",
    "battements par minute", "0 = non, 1 = oui", "mm (par rapport au repos)",
    "1 = ascendante, 2 = plate, 3 = descendante", "0 à 3",
    "3 = normal, 6 = défaut fixe, 7 = défaut réversible",
    "0 = absence, 1 à 4 = présence (regroupés en 1)"),
  stringsAsFactors = FALSE)

tableau_variables_ui <- function(d = variables_presentation) {
  shiny::tags$table(class = "table table-variables",
    shiny::tags$thead(shiny::tags$tr(lapply(c("Variable", "Description", "Type", "Unité ou modalités"),
      shiny::tags$th))),
    shiny::tags$tbody(lapply(seq_len(nrow(d)), function(i) shiny::tags$tr(
      shiny::tags$td(shiny::tags$code(d$variable[i])), shiny::tags$td(d$description[i]),
      shiny::tags$td(d$type[i]), shiny::tags$td(d$valeurs[i])))))
}
