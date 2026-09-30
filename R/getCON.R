##' @title {Connexion à LAVeval}
##' @description{Cette fonction ouvre une connexion à la base de
##' données Laveval}
##' @param env {nom de la variable d'environnement contenant le mot de
##' passe pour la base de données Laveval}
##' @return {une connexion est ouverte}
##' @details{Cette fonction n'a pas vocation à être utiisée directement. Elle est urilisée par les fonctions d'importation du package.}
##' @importFrom RPostgres dbConnect
##' @importFrom RPostgres Postgres
##' @keywords internal
##' @export
getCON <- function(env = "LAVEVAL_DB_PASSWORD"){
    RPostgres::dbConnect(
        RPostgres::Postgres(),
        host     = "laveval.cirad.fr",
        port     = 5432,
        dbname   = "laveval",
        user     = "readonly",
        password = Sys.getenv(env),
        options  = "-c search_path=odk,public"
        )
}
