#' @title Function for organize data for Package PCPS
#'
#' @description Package \strong{PCPS} requires that the species and community sequence in
#' the data.frame or matrix must be the same for all data.frame/matrices. 
#' This function use the function \code{\link{organize.syncsa}} to organize the data.
#'
#' @details The function, as well as organize.syncsa, organizes the data for the functions of the package
#' PCPS, placing the matrices of community, phylogenetic distance and environmental varibles in the same order.
#' 
#' Essentially this function is the same as function \code{\link{organize.syncsa}}. This 
#' use as reference the community data for organize all data.frame or matrices
#' in the same order that the sampling units names and species names found in community
#' data set. For this all data sets entered must be correctly named, with rows and columns
#' named. The matrices phylodist and envir can be larger than community
#' data (more species and/or more sampling units) as long as it has at least
#' all species and/or sampling units that are in community data. The function
#' organizes the data despite the absence of one of the data.frames or matrices,
#' provided that the community data had been entered. Unspecified data will
#' appear as NULL. All arguments this funtion will be passed to organize.syncsa, 
#' see more details in \code{\link{organize.syncsa}}.
#'
#'
#' @encoding UTF-8
#' @param comm Community data, with species as columns and sampling units as
#' rows.
#' @param phylodist Matrix containing phylogenetic distance between species.
#' Must be a complete matrix (not a half diagonal matrix).This matrix can be
#' larger than community data (more species) as long as it has at least all
#' species that are in community data (Default phylodist = NULL).
#' @param envir Environmental variables for each community, with variables as
#' columns and sampling units as rows (Default envir = NULL).
#' @param check.comm Logical argument (TRUE or FALSE) to remove sampling units and
#' species with total sums equal or less than zero (Default check.comm = TRUE).
#' @param ... Other parameters for the organize.syncsa function.
#' @return A object of class metacommunity.data (also of the class list) with all result returned by organize.syncsa. Featured for:
#' \item{call}{The arguments used.}
#' \item{community}{Community data.}
#' \item{phylodist}{Phylogenetic distance.}
#' \item{environmental}{Environmental variables.}
#' @author Vanderlei Julio Debastiani <vanderleidebastiani@@yahoo.com.br>
#' @seealso \code{\link{organize.syncsa}}
#' @keywords PCPS
#' @examples
#' data(ADRS)
#' organize.pcps(ADRS$community, phylodist = ADRS$phylo)
#' @export
organize.pcps <- function (comm, phylodist = NULL, envir = NULL, check.comm = TRUE, ...)
{
  res <- SYNCSA::organize.syncsa(comm = comm, phylodist = phylodist, envir = envir, check.comm = check.comm, ...)
  res$call <- match.call()
  return(res)
}
