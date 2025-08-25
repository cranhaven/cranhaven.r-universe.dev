#' Bio-accumulation data set for \emph{Gammarus fossarum} exposed to Hg spiked water.
#'
#' Male \emph{Gammarus fossarum} exposed to Hg spiked water.
#' A single exposure concentration was tested. The duration of the accumulation
#' phase is 4 days.
#'
#' @name Male_Gammarus_Single
#' 
#' @docType data
#' 
#' @usage data(Male_Gammarus_Single)
#' 
#' @format A dataframe with 23 observations on the following four variables:
#' \describe{ 
#' \item{\code{time}}{A vector of class \code{numeric} with the time points in
#' days.}
#' \item{\code{expw}}{A vector of class \code{numeric} with Hg
#' exposure in water in \eqn{\mu g.m L^{-1}}.}
#' \item{\code{replicate}}{A vector of class \code{integer} for replicate
#' identification.}
#' \item{\code{conc}}{A vector of class \code{numeric} with Hg concentration
#' in organism in \eqn{\mu g.m L^{-1}}.}
#' }
#' 
#' @references Ciccia, T. (2019). Accumulation et devenir du mercure chez
#' l'espèce sentinelle Gammarus fossarum : de l'expérimentation au développement
#' d'un modèle toxicocinétique multi-compartiments.
#' Rapport de stage de Master 2, INRAE.
#' 
#'
#' @keywords dataset
#' 
NULL

#' Male \code{Gammarus fossarum} exposed to Hg spiked water. 
#' Three exposure concentrations were tested in triplicates. The duration of the
#' accumulation phase is 4 days for 0.0000708021 and 0.000283208
#' \eqn{\mu g.m L^{-1}} exposure concentrations, and 7 days for 0.000141604 
#' \eqn{\mu g.m L^{-1}} exposure concentration.
#'
#' @name Male_Gammarus_Merged
#' 
#' @docType data
#' 
#' @usage data(Male_Gammarus_Merged)
#' 
#' @format A dataframe with 72 observations on the following four variables:
#' \describe{ 
#' \item{\code{time}}{A vector of class \code{numeric} with the time points in
#' days.}
#' \item{\code{expw}}{A vector of class \code{numeric} with Hg
#' exposure in water in \eqn{\mu g.m L^{-1}}.}
#' \item{\code{replicate}}{A vector of class \code{integer} for replicate
#' identification.}
#' \item{\code{conc}}{A vector of class \code{numeric} with Hg concentration
#' in organism in \eqn{\mu g.m L^{-1}}.}
#' }
#' 
#' @references Ciccia, T. (2019). Accumulation et devenir du mercure chez
#' l'espèce sentinelle Gammarus fossarum : de l'expérimentation au développement
#' d'un modèle toxicocinétique multi-compartiments.
#' Rapport de stage de Master 2, INRAE.
#' 
#'
#' @keywords dataset
#' 
NULL

#' Male \code{Gammarus pulex} exposed to seanine spiked water. A single exposure
#' concentration was tested. The duration of the accumulation phase is 1.417
#' days. Three metabolites were quantified. The growth of organism was included.
#'
#' @name Male_Gammarus_seanine_growth
#' 
#' @docType data
#' 
#' @usage data(Male_Gammarus_seanine_growth)
#' 
#' @format A dataframe with 22 observations on the following four variables:
#' \describe{ 
#' \item{\code{time}}{A vector of class \code{numeric} with the time points in
#' days.}
#' \item{\code{expw}}{A vector of class \code{numeric} with seanine
#' exposure in water in \eqn{\mu g.m L^{-1}}.}
#' \item{\code{replicate}}{A vector of class \code{integer} for replicate
#' identification.}
#' \item{\code{conc}}{A vector of class \code{numeric} with concentration
#' in organism.}
#' \item{\code{concm1}}{A vector of class \code{numeric} with metabolite 
#' concentration in organism.}
#' \item{\code{concm2}}{A vector of class \code{numeric} with metabolite 
#' concentration in organism.}
#' \item{\code{concm3}}{A vector of class \code{numeric} with metabolite 
#' concentration in organism.}
#' \item{\code{growth}}{A vector of class \code{numeric} with growth of the
#' organism.}
#' }
#' 
#' @references Ashauer, R. et al. (2012). Significance of xenobiotic metabolism 
#' for bioaccumulation kinetics of organic chemicals in Gammarus pulex.
#' Environmental Science Technology, 46: 3498-3508.
#'
#' @keywords dataset
#' 
NULL


#' Data on Chironomus with several exposure routes.
#' 
#' @name Chiro_Creuzot
#' 
#' @docType data
#' 
#' @usage data(Chiro_Creuzot)
#' 
#' @format A dataframe with 24 observations on the following four variables:
#' \describe{ 
#' \item{\code{time}}{A vector of class \code{numeric} with the time points in
#' days.}
#' \item{\code{expw}}{A vector of class \code{numeric} with the exposure
#'  in water.}
#' \item{\code{expw}}{A vector of class \code{numeric} with the exposure
#'  in pore water.}
#' \item{\code{replicate}}{A vector of class \code{integer} for replicate
#' identification.}
#' \item{\code{conc}}{A vector of class \code{numeric} with concentration
#' in organism.}
#' \item{\code{concm1}}{A vector of class \code{numeric} with metabolite 
#' concentration in organism.}
#' \item{\code{concm2}}{A vector of class \code{numeric} with metabolite 
#' concentration in organism.}
#' }
#'
#' @keywords dataset
#' 
NULL


#' Data on Chironomus exposed to benzoapyrene
#' 
#' @name Chironomus_benzoapyrene
#' 
#' @docType data
#' 
#' @usage data(Chironomus_benzoapyrene)
#' 
#' @keywords dataset
#' 
NULL

#' Data on Oncorhynchus exposition
#' @name Oncorhynchus_two
#' 
#' @docType data
#' 
#' @usage data(Oncorhynchus_two)
#' 
#' @keywords dataset
#' 
NULL


#' Data on Sialis lutaria exposure time series
#' 
#' @name Exposure_Sialis_lutaria
#' 
#' @docType data
#' 
#' @usage data(Exposure_Sialis_lutaria)
#' 
#' @keywords dataset
#' 
NULL

#' Data on Sialis lutaria internal time series
#' 
#' @name Internal_Sialis_lutaria
#' 
#' @docType data
#' 
#' @usage data(Internal_Sialis_lutaria)
#' 
#' @keywords dataset
#' 
NULL

#' Data on Gammarus exposed to azoxistrobine
#' @name Gammarus_azoxistrobine_1d_Rosch2017
#' 
#' @docType data
#' 
#' @usage data(Gammarus_azoxistrobine_1d_Rosch2017)
#' 
#' @keywords dataset
#' 
NULL
