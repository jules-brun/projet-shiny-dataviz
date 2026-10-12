# Rscript tests/validation_factorielles_mixtes.R
options(sass.cache = FALSE, heart.acm.cache_dir = NULL)
Sys.setenv(TZ = "Europe/Paris")
invisible(capture.output(serveur <- source("server.R")$value))
original <- donnees_uci
d <- original[c(seq_len(80), 330:350, 600:620, 820:840), ]
choix <- benchmark_app$defauts
choix[intersect(variables_quantitatives_app, names(choix))] <- "mean"
set.seed(173); rng <- .Random.seed
quanti <- calculer_quantitatives_acm(d, choix, benchmark_app, m = 5L)
stopifnot(identical(rng, .Random.seed), identical(names(quanti$simple), variables_quantitatives_app),
  !anyNA(quanti$simple), identical(quanti, calculer_quantitatives_acm(d, choix, benchmark_app, m = 5L)))
for (v in variables_quantitatives_app) for (z in c(list(quanti$simple), quanti$jeux)) {
  obs <- !is.na(d[[v]])
  stopifnot(identical(z[[v]][obs], d[[v]][obs]))
}

# Fixture qualitative : seules les cellules NA changent ; les quantitatives sont
# complétées par les vrais modèles, les jeux sont ensuite projetés dans le même repère.
completer <- function(i) {
  z <- d
  for (v in variables_acm_app) {
    manquants <- which(is.na(z[[v]])); niveaux <- sort(unique(z[[v]][!is.na(z[[v]])]))
    if (length(manquants)) z[[v]][manquants] <- niveaux[(seq_along(manquants) + i) %% length(niveaux) + 1L]
  }
  z[variables_quantitatives_app] <- if (!i) quanti$simple else quanti$jeux[[i]]
  z
}
imps <- list(simple = completer(0L), jeux = lapply(1:5, completer), m = 5L, selections = choix,
  masque = is.na(d[variables_acm_app]),
  masque_complet = is.na(d[c(variables_acm_app, variables_quantitatives_app, "provenance")]))
rownames(imps$masque) <- rownames(imps$masque_complet) <- rownames(d)
reuse <- calculer_quantitatives_acm(d, choix, benchmark_app, m = 3L, imputations = imps)
stopifnot(identical(reuse$jeux, lapply(imps$jeux[1:3], function(j) j[variables_quantitatives_app])))

acm <- calculer_acm_app(imps)
sans_quanti <- calculer_acm_app(imps, quantitatives = character())
stopifnot(identical(rownames(acm$ref$quanti.sup$coord), variables_quantitatives_app),
  isTRUE(all.equal(acm$ref$ind$coord, sans_quanti$ref$ind$coord)),
  !any(variables_quantitatives_app %in% acm$variables),
  max(abs(acm$ref$quanti.sup$coord - t(cor(acm$ref$ind$coord, as.matrix(quanti$simple))))) < 1e-8)
invalide <- imps; invalide$simple$pa_repos[1] <- NA_real_
stopifnot(inherits(try(calculer_acm_app(invalide), silent = TRUE), "try-error"))
q1 <- calculer_quantitatives_acm(d, choix, benchmark_app)
cc <- calculer_acm_conditionnelle(d, mode = "complets", avec_lourdes = FALSE, quantitatives = q1)
si <- calculer_acm_conditionnelle(d, mode = "simple", avec_lourdes = FALSE, nbsim = 2, quantitatives = q1)
q3 <- calculer_quantitatives_acm(d, choix, benchmark_app, m = 3L)
mi <- calculer_acm_conditionnelle(d, mode = "multiple", avec_lourdes = FALSE, nboot = 3L,
  nbsim = 2L, quantitatives = q3)
for (r in list(cc,si,mi)) stopifnot(identical(rownames(r$ref$quanti.sup$coord), variables_quantitatives_app),
  !any(variables_quantitatives_app %in% r$variables))

afdm <- calculer_afdm_app(imps)
stopifnot(setequal(afdm$variables, c(variables_quantitatives_app, variables_acm_app, "provenance")),
  length(afdm$variables) == 14L, !any(c("diagnostic", "diagnostic_initial") %in% afdm$variables),
  names(afdm$ref$call$X)[afdm$ref$call$sup.var] == "diagnostic",
  identical(rownames(afdm$ref$quanti.var$coord), variables_quantitatives_app),
  max(abs(predict(afdm$ref, newdata = tableau_afdm_app(imps$simple,afdm$variables))$coord -
    afdm$ref$ind$coord)) < 1e-8)
base <- tableau_afdm_app(imps$simple,afdm$variables)
bary <- modalites_afdm_app(afdm$ref$ind$coord, base[vapply(base,is.factor,logical(1))])
stopifnot(max(abs(bary[rownames(afdm$ref$quali.var$coord), ] - afdm$ref$quali.var$coord)) < 1e-8,
  max(abs(associations_afdm_app(afdm$ref$ind$coord,base)[rownames(afdm$ref$var$coord), ] -
    afdm$ref$var$coord)) < 1e-8)
# Aucun effet du diagnostic sur les axes, aucun NA silencieusement remplacé.
change <- imps
change$simple$diagnostic <- 1L - change$simple$diagnostic
change$jeux <- lapply(change$jeux,function(j) {j$diagnostic <- 1L - j$diagnostic; j})
stopifnot(isTRUE(all.equal(afdm$ref$ind$coord, calculer_afdm_app(change)$ref$ind$coord)))
change$simple$diagnostic[1:5] <- NA_integer_
change$jeux <- lapply(change$jeux,function(j) {j$diagnostic[1:5] <- NA_integer_; j})
stopifnot(isTRUE(all.equal(afdm$ref$ind$coord, calculer_afdm_app(change)$ref$ind$coord)))
bad <- imps; bad$jeux[[1]]$cholesterol[1] <- NA_real_
stopifnot(inherits(try(calculer_afdm_app(bad),silent=TRUE),"try-error"))
bad <- imps; bad$jeux[[1]] <- bad$jeux[[1]][rev(seq_len(nrow(d))), ]
stopifnot(inherits(try(calculer_afdm_app(bad),silent=TRUE),"try-error"))
fixes <- which(!afdm$impute)
stopifnot(isTRUE(all.equal(afdm$individus[[1]][fixes, ],afdm$individus[[5]][fixes, ])))

# Centres et couleurs réellement rendues : individus, modalités, liens r²/η².
for (axes in list(c(1,2),c(2,3))) {
  for (vue in c("individus","modalites","associations")) {
    g <- if (vue == "associations") graphique_variables_afdm(afdm,axes,TRUE) else
      graphe_acm_app(afdm,vue,axes,TRUE,10L)
    ip <- which(vapply(g$layers,function(l) inherits(l$geom,"GeomPath"),logical(1)))
    jp <- which(vapply(g$layers,function(l) inherits(l$geom,"GeomPoint"),logical(1)))
    b <- ggplot2::ggplot_build(g); contours <- g$layers[[ip]]$data; points <- g$layers[[jp]]$data
    for (id in unique(contours$id)) {
      ids <- which(contours$id==id); z <- contours[ids,c("x","y")]
      p <- points[points$id==id,c("x","y")]
      stopifnot(max(abs(colMeans(z[-nrow(z), ]) - as.numeric(p))) < 1e-10,
        all(b$data[[ip]]$colour[ids] == b$data[[jp]]$colour[match(id,points$id)]))
    }
    sans <- if (vue == "associations") graphique_variables_afdm(afdm,axes,FALSE) else
      graphe_acm_app(afdm,vue,axes,FALSE)
    stopifnot(!any(vapply(sans$layers,function(l) inherits(l$geom,"GeomPath"),logical(1))))
  }
  for (r in list(acm,afdm)) invisible(ggplot2::ggplot_build(graphique_correlations_factorielles(r,axes)))
}

shiny::testServer(serveur, {
  do.call(session$setInputs, setNames(as.list(choix), paste0("modele_", names(choix))))
  resultat(list(imputations=imps,acm=acm,afdm=afdm))
  session$setInputs(acm_mode="modeles",acm_lourdes=TRUE,acm_nboot=20,nombre_imputations=5,
    axe_x="1",axe_y="2",axe_afdm_x="2",axe_afdm_y="3",ellipses_acm=TRUE,limite_ellipses=10,
    calculer_acm=1)
  stopifnot(is.null(erreur_acm()), !is.null(output$acm_quantitatives))
  for (id in c("afdm_valeurs_propres","afdm_variables","afdm_quantitatives","afdm_modalites","afdm_individus"))
    stopifnot(!is.null(output[[id]]))
  stopifnot(grepl("Ellipses AFDM",output$afdm_statut,fixed=TRUE))
  avant <- afdm_resultat()
  session$setInputs(ellipses_acm=FALSE,axe_afdm_x="1",axe_afdm_y="2")
  stopifnot(identical(avant,afdm_resultat()), !grepl("Ellipses AFDM",output$afdm_statut,fixed=TRUE))
})
stopifnot(identical(original,donnees_uci))
cat("Validation : cinq quantitatives supplémentaires, AFDM à 14 variables, diagnostic exclu des axes, projections communes, ellipses et Shiny.\n")
