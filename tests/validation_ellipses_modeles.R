# Rscript tests/validation_ellipses_modeles.R
options(sass.cache = FALSE)
Sys.setenv(TZ = 'Europe/Paris')
invisible(capture.output(serveur <- source('server.R')$value))
# Les premiers individus peuvent être invariants ; la limite doit être appliquée
# APRÈS avoir exclu leurs contours nuls, en privilégiant la dispersion réelle.
nuage <- rbind(data.frame(id='fixe', x=rep(0,5), y=rep(0,5)),
               data.frame(id='petit', x=c(-1,1,0,1,-1), y=c(0,1,-1,0,0)),
               data.frame(id='grand', x=c(-4,4,0,4,-4), y=c(0,4,-4,0,0)))
e <- ellipses_app(nuage, limite=1L)
stopifnot(identical(unique(e$id), 'grand'), nrow(e)==65L,
          length(unique(ellipses_app(nuage)$id)) == 2L,
          is.null(ellipses_app(nuage[nuage$id=='fixe',])),
          is.null(ellipses_app(nuage[1:2,])))
# Dispersion de rang un : segment dessiné plutôt qu'une ellipse artificielle.
segment <- ellipses_app(data.frame(id='s',x=c(-1,0,1),y=rep(0,3)))
stopifnot(!is.null(segment), all(segment$y == 0))
# Résultat factoriel synthétique aligné : la case active/désactive bien la couche.
r <- list(individus=lapply(1:5,function(i) matrix(c(0,0,i,i%%2,-i,i%%3),nrow=3,byrow=TRUE)),
          modalites=lapply(1:5,function(i) matrix(c(i,i%%2,-i,i%%3,i/2,-i%%2),
            nrow=3,byrow=TRUE,dimnames=list(c('sexe : 0','ecg_repos : 0','diagnostic : 1'),NULL))),
          diagnostic=factor(c(0,1,0),levels=0:1), impute=c(TRUE,TRUE,TRUE), m=5L,
          ref=list(eig=matrix(c(1,60,1,40),ncol=2,byrow=TRUE)))
for (vue in c('individus','modalites')) {
  g <- graphe_acm_app(r,vue,c(1,2),TRUE,limite=2L)
  ip <- which(vapply(g$layers,function(l) inherits(l$geom,'GeomPath'),logical(1)))
  jp <- which(vapply(g$layers,function(l) inherits(l$geom,'GeomPoint'),logical(1)))
  stopifnot(length(ip)==1L, length(jp)==1L)
  construit <- ggplot2::ggplot_build(g)
  paths <- g$layers[[ip]]$data; points <- g$layers[[jp]]$data
  # Le centre géométrique du contour doit coïncider avec son point, et la
  # couleur réellement rendue par ggplot doit être identique (deux diagnostics,
  # plusieurs variables, dont une modalité supplémentaire).
  for (id in unique(paths$id)) {
    lignes <- which(paths$id == id)
    contour <- paths[lignes, c('x','y')]
    centre <- colMeans(contour[-nrow(contour), ]) # Retirer le sommet de fermeture répété.
    point <- points[points$id == id, c('x','y')]
    stopifnot(max(abs(centre-as.numeric(point))) < 1e-10,
      all(construit$data[[ip]]$colour[lignes] == construit$data[[jp]]$colour[match(id,points$id)]))
  }
  sans <- graphe_acm_app(r,vue,c(1,2),FALSE)
  stopifnot(!any(vapply(sans$layers,function(l) inherits(l$geom,'GeomPath'),logical(1))))
}
# Régression MIMCA : les modalités de référence peuvent être éloignées du
# centre des tirages. Les points affichés doivent être leurs moyennes projetées.
mimca <- r; mimca$individus_reference <- TRUE
mimca$individus <- r$individus[1]
mimca$reference_modalites <- r$modalites[[1]] + 100
gm <- graphe_acm_app(mimca,'modalites',c(1,2),TRUE)
pm <- Filter(function(l) inherits(l$geom,'GeomPoint'),gm$layers)[[1]]$data
em <- Filter(function(l) inherits(l$geom,'GeomPath'),gm$layers)[[1]]$data
for (id in unique(em$id)) {
  z <- em[em$id==id,c('x','y')]
  p <- pm[pm$id==id,c('x','y')]
  stopifnot(max(abs(colMeans(z[-nrow(z),])-as.numeric(p))) < 1e-10,
    max(abs(as.numeric(p))) < 100)
}
stopifnot(!any(vapply(graphe_acm_app(mimca,'individus',c(1,2),TRUE)$layers,
  function(l) inherits(l$geom,'GeomPath'),logical(1))))
b <- bilan_ellipses_acm(r,c(1,2),1L)
stopifnot(b$individus[['disponibles']]==2L,b$individus[['affichees']]==1L)
shiny::testServer(serveur, {
  resultat(list(acm=r,imputations=list(m=5L)))
  session$setInputs(axe_x='1',axe_y='2',ellipses_acm=TRUE,limite_ellipses=1L)
  stopifnot(grepl('3 ellipses de modalités',output$statut_ellipses,fixed=TRUE),
            grepl('1 ellipses individuelles',output$statut_ellipses,fixed=TRUE))
  invariant <- r
  invariant$individus <- rep(r$individus[1],5L)
  invariant$modalites <- rep(r$modalites[1],5L)
  resultat(list(acm=invariant,imputations=list(m=5L)))
  session$flushReact()
  stopifnot(grepl('positions sont invariantes',output$statut_ellipses,fixed=TRUE))
  session$setInputs(ellipses_acm=FALSE)
  stopifnot(identical(output$statut_ellipses,''))
})
cat('Validation ellipses : centres, couleurs rendues, MIMCA, limite après filtrage, rang un, case et compteurs.\n')
