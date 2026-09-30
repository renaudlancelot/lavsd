#' Ligne de côte de La Réunion
#'
#' Objet spatial (package `sp`) représentant la ligne de côte de La
#' Réunion. Source: La Réunion: fond de carte OpenStreetMap.
#'
#' @format Objet de classe `SpatialPolygons` (package sp), projection EPSG:2975.:
#' @source <https://data.nextgis.com/fr/region/RE/base/>
"run"

#' Emprise du projet OpTIS
#'
#' Fond de carte utilisé dans les cartes de densité relative des moustiques.
#'
#' @format Objet de classe `RasterLayer` (package raster), projection EPSG:2975. à 3 bandes (R, G, B).
#' @source CartoDB.PositronNoLabels
"emprise"

#' Zones Alisé
#'
#' Collection de Polygones spatiaux des zones Alizé.
#'
#' @format Objet de classes `sf` et `data.frame`: collection de polygonrs spatiaux.
#' @source ARS La Réunion
"za"

#' Pièges
#'
#' Table des pièges (BGS et ovitraps) utilisés pour le monitoring des populations de moustisues dans le projet OpTIS
#'
#' @format Objet de classes `sf` et `data.frame`: collection de points spatiaux..
#' @source Projet OpTIS
"traps"


#' Moustiques adultes
#'
#' Table des captures de moustiques adultes échantillonnés avec des pièges BG Sentinel.
#'
#' @format `data.frame`
#' @importFrom utils data
#' @source Projet OpTIS
"bgs"


#' Oeufs d'_Aedes_
#'
#' Table des oeufs d'_Aedes_  échantillonnés avec des ovitraps
#'
#' @format `data.frame`
#' @source Projet OpTIS
"ovi"

