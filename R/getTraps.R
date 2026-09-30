##' @title {Importe la table des pièges}
##' @description Cette fonction importe la table de tous les pièges enregistrés dans Laveval
##' @return un `data.frame`
##' @importFrom sf st_read
##' @importFrom dplyr mutate
##' @keywords import
##' @export
getTraps <- function(){
    con <- getCON()
    sf::st_read(con,
                layer = "pieges") |>
        dplyr::mutate(treatment = traitement)
}
