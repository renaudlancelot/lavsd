##' @title {Importe les comptages d'oeufs d'_Aedes_}
##' @description{Cette fonction importe depuis le système
##'     d'information Laveval les comptages d'oeufs d'_Aedes_
##'     collectés avec ds ovitraps pendant le monitoring entomologique
##'     du projet OpTIS.}
##' @return {un data.frame avec tout c kil fô dedans ;-}
##' @importFrom DBI dbReadTable
##' @importFrom dplyr mutate
##' @keywords import
##' @export
getOVIdata <- function(){
    con <- getCON()
    ovi_data <- DBI::dbReadTable(con, "v_ovi_final") |>
        dplyr::mutate(
               treatment = zone,
               za = zone_alize,
               n_ini = nb_oeufs_initial,
               n_tot = oeufs_total,
               n_ecl = eclos,
               n_alb = albo,
               n_aeg = aegypti,
               n_tot = as.numeric(n_tot),
               n_ecl = as.numeric(n_ecl),
               n_alb = as.numeric(n_alb),
               n_aeg = as.numeric(n_aeg)) |>
        dplyr::select(
                   piege_id, date_debut_piege,
                   date_fin_piege, treatment, za, strate,
                   date_pose, date_releve, n_ini, n_tot, n_ecl)

    class(ovi_data) <- c("lvlov", class(ovi_data))

    return(ovi_data)
}

