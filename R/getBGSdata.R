##' @title {Importe les captures de moustiques adultes}
##' @description{Cette fonction importe depuis le système
##'     d'information Laveval les résultats des comptages de
##'     moustiques adultes capturés avec des pièges BG Sentinel
##'     appâtés au CO~2~ gazeux, pendant le monitoring entomologique
##'     du projet OpTIS.}
##' @return {un data.frame}
##' @details{Les données de trois espèces sont disponibles dans
##'     Laveval: (i) _Aedes albopictus_, (ii)_Aedes argypti_ et
##'     (iii) _Culex quinquefasciattus_.
##' Pour chaque enregistrement, on a (liste non exhaustive):
##' * le site concerné,
##' * la zone Alizé concernée,
##' * l'identifiant du piàge,
##' * la date de pose du piège,
##' * la date de relevé du piège,
##' * le nombre de mâles sauvages _Aedes albopictus_,
##' * le nombre de mâles stériles_Aedes albopictus_
##' * le nombre de femelles sauvages _Aedes albopictus_,
##' * le nombre de femelles stériles _Aedes albopictus_,
##' * le nombre d'adultes _Aedes albopictus_ de sexe indéterminé,
##' * le nombre de mâles stériles_Aedes albopictus_ jaunes,
##' * le nombre de mâles stériles_Aedes albopictus_ orange,
##' * le nombre de mâles stériles_Aedes albopictus_ rose,
##' * le nombre de mâles stériles_Aedes albopictus_ bleu,
##' * le nombre de mâles stériles_Aedes albopictus_ vert,
##' * le nombre de mâles sauvages _Aedes aegypti_,
##' * le nombre de femelles sauvages _Aedes  aegypti_,
##' * le nombre d'adultes _Aedes  aegypti_ de sexe indéterminé,
##' * le nombre de mâles sauvages _Culex quinquefasciattus_,
##' * le nombre de femelles sauvages _Culex quinquefasciattus_,
##' * le nombre d'adultes _Culex quinquefasciattus_ de sexe indéterminé}
##' @importFrom DBI dbReadTable
##' @importFrom dplyr mutate
##' @keywords import
##' @export
getBGSdata <- function(){
    con <- getCON()
    bg_data <- DBI::dbReadTable(con, "v_bg_final") |>
        dplyr::mutate(
                   treatment = zone,
                   za = zone_alize,
                   nbmw = aedes_albopictus_m,
                   nbms = aedes_albopictus_ms,
                   nbfw = aedes_albopictus_f,
                   nbfs = aedes_albopictus_fs,
                   nbuw = aedes_albopictus_nd,
                   njaune = aedes_albopictus_jaune,
                   norange = aedes_albopictus_orange,
                   nrose = aedes_albopictus_rose,
                   nbleu = aedes_albopictus_bleu,
                   nvert = aedes_albopictus_vert,
                   ngmw = aedes_aegypti_m,
                   ngfw = aedes_aegypti_f,
                   nguw = aedes_aegypti_nd,
                   nqmw = culex_quinquefasciatus_m,
                   nqfw = culex_quinquefasciatus_f,
                   nquw = culex_quinquefasciatus_nd
               )
    class(bg_data) <- c("lvlbg", class(bg_data))
    return(bg_data)
}
