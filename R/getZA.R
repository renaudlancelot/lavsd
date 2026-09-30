##' @title {Importe les zones Alizé}
##' @description {Cette fonction importe la tabke dee polygons des zones Alizé}
##' @return collection de polygones spatiaux sf
##' @importFrom sf st_read
##' @keywords import
##' @export
getZA <- function(){
    con <- getCON()
    sf::st_read(con, layer = "zone_alize")
    }
