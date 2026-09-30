##' @title {Nettoyage des données de suppression}
##' @description Méthode générique de nettoyage des données de
##'     suppression
##' @param data objet de classe `lvlov` ou `lvlbg`; héritant
##'     de la classe `data.frame`, output des fonctions
##'     d'importation `getOVIdata` - échantillonnage des oeufs
##'     d'_Aedes_, ou `getBGSdata` - échantillonnage des
##'     moustiques adultes.
##' @param treated scalaire de type caractère indiquant le site
##'     traité: `bsit1` ou `bsit2`.
##' @param end scalaire de type date indiquant la date limite
##'     supérieure pour laquelle on veut les taux de
##'     suppression.
##' @param ... ellipse: argument(s) supplémentaires passés aux
##'     méthodes spécifiques (aucun pour le moment).
##' @return `data.frame`
##' @keywords cleaning
##' @export cleansd
cleansd <- function(data, treated, end, ...) {
  UseMethod("cleansd")
}

##' @title Nettoyage des données BGS de suppression
##' @description Méthode de nettoyage des données BGS nécessaires
##'     pour estimer la suppression des femelles adultes _Aedes
##'     albopictus_.
##' @param data objet de classe `lvlbg` output de la fonctions
##'     d'importation `getBGSdata`
##' @param treated scalaire de type caractère indiquant le site
##'     traité: `bsit1` ou `bsit2`.
##' @param end scalaire de type date indiquant la date limite
##'     supérieure pour laquelle on veut les taux de
##'     suppression. Valeur par défaut `Sys.Date()`.
##' @param ... ellipse: argument(s) supplémentaires passés aux
##'     méthodes spécifiques (aucun pour le moment).
##' @return un objet de classe `cleaned` héritant de la classe
##'     `data.frame`, avec 4 champs:
##' * `treatment` facteur: site du piège;
##' * `piege_id` facteur: identifiant du piège,
##' * `period` facteur: période de capture,
##' * `m` entier: densité apparente.
##' @importFrom dplyr select
##' @importFrom dplyr filter
##' @importFrom dplyr mutate
##' @importFrom dplyr left_join
##' @importFrom dplyr join_by
##' @importFrom missForest missForest
##' @importFrom reshape2 melt
##' @keywords cleaning
##' @export cleansd.lvlbg
##' @export
cleansd.lvlbg <- function(data, treated, end = Sys.Date(), ...){
    myd <- dplyr::select(data,
                  treatment, piege_id, date_releve, nbfw) |>
        dplyr::filter(
                   date_releve <= end &
                   (treatment == treated |
                    grepl("control", treatment))) |>
        dplyr::mutate(
                   m = nbfw,
                   nbfw = NULL,
                   treatment = factor(as.character(treatment)),
                   piege_id = factor(as.character(piege_id)),
                   period = cut(
                       date_releve,
                       breaks = seq(min(date_releve),
                                    end + 27,
                                    by = "4 weeks"),
                       include.lowest = TRUE)
               )

    ## le résultat est une matrice avec les pieges en colonnes et les
    ## périodes en lignes
    mym <- myd |>
        with(
            tapply(
                m,
                list(
                    period = period,
                    piege_id = piege_id),
                FUN = function(x)
                    mean(x, na.rm = TRUE)))

    ## on élimine les colonnes avec au moins 50% de données
    ## manquantes
    myL <- apply(
        mym,
        MARGIN = 2,
        FUN = function(x){
                if(sum(is.na(x)) >= length(x) / 2){
                    return(NULL)
                }
                else{
                    return(x)
                }
        })
    myC <- do.call(
        cbind,
        base::subset(
                  myL,
                  unlist(
                      lapply(
                          myL,
                          FUN = function(x)
                              !is.null(x))
                     ))) |>
    ## on utilise un modèle RandomForest pour imputer une valeur
    ## vraisemblable aux données manquantes
        missForest::missForest()
    myR <- myC$ximp %>%
        as.data.frame() %>%
        dplyr::mutate(.,
                      period = factor(rownames(.)), .before = 1
               ) %>%
        ## on transforme le format "wide" (matrice avec une colonne par
        ## piege) en format "long" (3 colonnes: period, piege_id, et mf)
        reshape2::melt(.,
                       id.vars = 'period',
                       measure.vars = 2:ncol(.),
                       variable.name = "piege_id",
                       value.name = "m"
                       ) |>
        ## on ajoute la colonne 'treatment' prise dans la table des
        ## pieges
        dplyr::left_join(
                   traps,
                   by = dplyr::join_by(piege_id)) |>
        dplyr::mutate(
                   treatment = factor(treatment),
                   piege_id = factor(piege_id),
                   m = round(m)) |>
        dplyr::filter(
            grepl("control", treatment) |
            treatment == treated) |>
        dplyr::select(treatment, piege_id, period, m)

    class(myR) <- c("cleaned", class(myR))
    return(myR)
}


##' @title {Nettoyage des données ovitrap de suppression}
##' @description Méthode de nettoyage des données ovitrap nécessaires
##'     pour estimer la suppression des oeufs.
##' @param data objet de classe `lvlov` output de la fonctions
##'     d'importation `getOVIdata`
##' @param treated scalaire de type caractère indiquant le site
##'     traité: `bsit1` ou `bsit2`.
##' @param end scalaire de type date indiquant la date limite
##'     supérieure pour laquelle on veut les taux de
##'     suppression. Valeur par défaut `Sys.Date()`.
##' @param ... ellipse: argument(s) supplémentaires passés aux
##'     méthodes spécifiques (aucun pour le moment).
##' @return un objet de classe `cleaned` héritant de la classe
##'     `data.frame`, avec 4 champs: * `treatment` facteur: site du
##'     piège; * `piege_id` facteur: identifiant du piège, * `period`
##'     facteur: période de capture, * `m` entier: densité apparente.
##' @importFrom dplyr select
##' @importFrom dplyr filter
##' @importFrom dplyr mutate
##' @importFrom dplyr left_join
##' @importFrom dplyr join_by
##' @importFrom missForest missForest
##' @importFrom reshape2 melt
##' @keywords data management
##' @export cleansd.lvlov
##' @export
cleansd.lvlov <- function(data, treated, end = Sys.Date(), ...){
    myd <- dplyr::select(
                      data,
                      treatment, piege_id, date_releve, n_ini) |>
        dplyr::filter(
                   !is.na(n_ini) &
                   date_releve <= end &
                   (treatment == treated |
                    grepl("control", treatment))) |>
        dplyr::mutate(
                   m = n_ini,
                   n_ini = NULL,
                   treatment = factor(as.character(treatment)),
                   piege_id = factor(as.character(piege_id)),
                   period = cut(date_releve,
                       breaks = seq(min(date_releve),
                                    end + 6,
                                    by = "1 week"),
                       include.lowest = TRUE))

    ## le résultat est une matrice avec les pieges en colonnes et les
    ## périodes en lignes
    mym <- myd |>
        with(
            tapply(
                m,
                list(
                    period = period,
                    piege_id = piege_id),
                FUN = function(x)
                    mean(x, na.rm = TRUE)))

    ## on élimine les colonnes avec au moins 50% de données
    ## manquantes
    myL <- apply(
        mym,
        MARGIN = 2,
        FUN = function(x){
                if(sum(is.na(x)) >= length(x) / 2){
                    return(NULL)
                }
                else{
                    return(x)
                }
        })
    myC <- do.call(
        cbind,
        base::subset(
                  myL,
                  unlist(
                      lapply(
                          myL,
                          FUN = function(x)
                              !is.null(x))
                     ))) |>
    ## on utilise un modèle RandomForest pour imputer une valeur
    ## vraisemblable aux données manquantes
       missForest::missForest()

    myR <- myC$ximp %>%
        as.data.frame() %>%
        dplyr::mutate(.,
                      period = factor(rownames(.)), .before = 1
               ) %>%
        ## on transforme le format "wide" (matrice avec une colonne par
        ## piege) en format "long" (3 colonnes: period, piege_id, et mf)
        reshape2::melt(.,
                       id.vars = 'period',
                       measure.vars = 2:ncol(.),
                       variable.name = "piege_id",
                       value.name = "m"
                       ) |>
        ## on ajoute la colonne 'treatment' prise dans la table des
        ## pieges
        dplyr::left_join(
                   traps,
                   by = dplyr::join_by(piege_id)) |>
        dplyr::mutate(
                   treatment = factor(treatment),
                   piege_id = factor(piege_id),
                   m = round(m)) |>
        dplyr::filter(
            grepl("control", treatment) |
            treatment == treated) |>
        dplyr::select(treatment, piege_id, period, m)
    class(myR) <- c("cleaned", class(myR))
    return(myR)
}

