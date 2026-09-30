##' @title {Fond sz carte de la zone d'emprise du projet OpTIS}
##' @description Cette fonction produit un fond de carte de la zone
##'     d'emprise du projet OpTIS, sous la forme d'un graphique
##'     produit par la fobctiob `rasterVis::levelplot`. Ce
##'     graphique est utilisable comme fond de xarte dans les
##'     graphiques gu paxkage recommandé `lattice` grâce à la
##'     fonction `latticeExtra::as.layer`
##' @param pos vecteur numérique de longueur 2 donnant la position (x,
##'     y) en coordonnées (0, 1) de l'extrémité gauche de l'échelle de
##'     l'emprise.
##' @param scalem scalaire numérique donnant la longueur de la barre
##'     d'échelle, en mètres
##' @return {un graphique de classe `lattice`}
##' @importFrom raster extent
##' @importFrom rasterVis levelplot
##' @importFrom latticeExtra layer
##' @importFrom latticeExtra panel.scaleArrow
##' @importFrom grDevices grey
##' @keywords graphics
##' @export
getBkg <- function(pos = c(.75, .05), scalem){
    r <- data('emprise',
              package = "lavsd",
              envir = .GlobalEnv)XS
    e <- raster::extent(get(r))
    rx <- e[2] - e[1]
    x1 <- pos[1]
    x2 <- x1 + scalem/rx
    rasterVis::levelplot(
                   get(r),
                   margin = F,
                   scales = list(draw = F),
                   colorkey = F,
                   col.regions = grDevices::grey(seq(.2, .95, .005)),
                   xlab = '',
                   ylab = '') +
        latticeExtra::layer(
              latticeExtra::panel.scaleArrow(
                              x = c(x1, x2),
                              y = pos[2],
                              col = "white",
                              lwd =3,
                              angle = 0,
                              label = paste(scalem, "m"),
                              col.text = "white",
                              adj = c(.5, -.5),
                              cex = .8),
                          data = list(
                              pos = pos,
                              x1 = x1,
                              x2 = x2,
                              scalem = scalem),
                          packets = 1)

}
