#' Households distribution by  size in Grenoble urban area in 2011
#' 
#' This data set gives the households distribution by size in 
#' Grenoble urban area in 2011, including the area vectorial map at
#' municipality level.
#' 
#' \itemize{
#'   \item code: Municipality code
#'   \item name: Municipality name
#'   \item small: (1-2 persons household)
#'   \item medium: (3-4 persons household)
#'   \item big: (more then 5 persons household)
#' }
#' 
#' @docType data
#' @keywords datasets
#' @source Insee: Resultats du recensement de la population 2011, \href{https://www.insee.fr/fr/accueil}{Insee}
#' @name GreHSize
#' @usage data(GreHSize)
#' @format A Spatial object including 52 polygons corresponding to each municipality of Grenoble Urban Area (Insee definition) and following data attributes: 
"GreHSize"



#' Theoretical two groups distribution on a 10x10 grid map.
#' 
#' The theoretical examples ( Morrill 1991, Wong 1993) adapted 
#' from Hong and O'Sullivan (2015). The space is represented by 
#' a 10x10 checkboard, with different distributions of two social 
#' groups in the area.
#' 
#' \itemize{
#'    \item : spatial ID;
#'    \item : municipality name;
#'    \item : pattern A: minority distribution;
#'    \item : pattern A: majority distribution;
#'    \item : pattern B: minority distribution;
#'    \item : pattern B: majority distribution;
#'    \item : pattern C: minority distribution;
#'    \item : pattern C: majority distribution;
#'    \item : pattern D: minority distribution;
#'    \item : pattern D: majority distribution;
#'    \item : pattern E: minority distribution;
#'    \item : pattern E: majority distribution;
#'    \item : pattern F: minority distribution;
#'    \item : pattern F: majority distribution;
#'    \item : pattern G: minority distribution;
#'    \item : pattern G: majority distribution;
#'    \item : pattern H: minority distribution;
#'    \item : pattern H: majority distribution;
#'    \item : pattern I: minority distribution;
#'    \item : pattern I: majority distribution;
#' }
#' 
#' @docType data
#' @keywords datasets
#' @source Hong S-Y (2014): R package 'seg', \href{https://sites.google.com/site/hongseongyun/seg/}{Hong's page}
#' @name segdata
#' @usage data(segdata)
#' @format A 10x10 grid Spatial object and following data attributes: 
"segdata"
