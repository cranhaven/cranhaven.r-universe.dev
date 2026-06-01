#' @title Évolution du PIB français jusqu'au T1 2022
#'
#' @description
#' Ce jeu de données contient une série `ts` de l'évolution trimestrielle du
#' produit intérieur brut français.
#' Toutes les infos complémentaires sur cette série se trouve sur la page de la
#' \href{https://www.insee.fr/fr/statistiques/2830547}{publication} sur le site
#' de l'\href{https://www.insee.fr/fr/accueil}{Insee}.
#'
#' @format Un ts unidimensionnel :
#' \describe{
#' \item{start}{le ts commence au T1 1970 mais la série de PIB ne commence qu'au
#' T2 1980.}
#' \item{end}{le ts finit au T3 2022 mais la série de PIB finit au T1 2022.}
#' \item{frequency_ts}{la fréquence est trimestrielle}
#' }
#' @source \url{https://www.insee.fr/fr/statistiques/2830547}
"ev_pib"
