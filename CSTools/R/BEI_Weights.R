#'Computing the weights for SFSs using the Best Index PDFs.
#'
#'@author Eroteida Sanchez-Garcia - AEMET, \email{esanchezg@aemet.es}
#' 
#'@description This function implements the computation to obtain the
#'normalized weights for each member of each Seasonal Forecast Systems (SFS) 
#'or dataset using the Probability Density Functions (PDFs) indicated by the 
#'parameter 'pdf_weight' (for instance the Best Index estimation obtained 
#'using the 'PDFBest' function). The weight of each member is proportional to
#'the probability of its index calculated with the PDF "pdf_weight". 
#' 
#'@references Regionally improved seasonal forecast of precipitation through 
#'Best estimation of winter NAO, Sanchez-Garcia, E. et al.,
#'Adv. Sci. Res., 16, 165174, 2019, \doi{10.5194/asr-16-165-2019}
#' 
#'@param index_weight Index (e.g. NAO index) array, from a dataset of SFSs
#'  for a period of years, with at least dimensions 'member'. 
#'  Additional dimensions, for instance, a temporal dimension as 'time', 
#'  must have the same lenght in both parameters, 'index_weight' and 
#'  'pdf_weight'. 
#'@param pdf_weight Statistics array to define a Gaussian PDF with at least 
#'  dimensions 'statistic'. The firt statistic is the parameter 'mean' of the PDF 
#'  and the second statistic is the parameter 'standard deviation' of the PDF.
#'@param time_dim_name A character string indicating the name of the temporal 
#'  dimension, by default 'time'.
#' 
#'@return BEI_Weights() returns a normalized weights array with the same 
#' dimensions that index_weight.
#' 
#'@examples
#' # Example for the BEI_Weights function
#' index_weight <- 1 : (10 * 3 * 5 * 1)
#' dim(index_weight) <- c(sdate = 10, dataset = 3, member = 5, season = 1)
#' pdf_weight <- 1 : (10 * 3 * 2 * 1)
#' dim(pdf_weight) <- c(sdate = 10, dataset = 3, statistic = 2, season = 1)
#' res <- BEI_Weights(index_weight, pdf_weight)
#' dim(res)
#' # sdate   dataset    member season
#' #    10         3         5      1
#'
#'@import multiApply
#'@export
BEI_Weights <- function(index_weight, pdf_weight, time_dim_name = 'time') {
  
  if (!is.character(time_dim_name)) {
    stop("Parameter 'time_dim_name' must be a character string ",
         "indicating the name of the temporal dimension.")
  }
  if (length(time_dim_name) > 1) {
    warning("Parameter 'time_dim_name' has length greater than 1 and ",
            "only the first element will be used.")
    time_dim_name <- time_dim_name[1]
  }
  if (!is.array(index_weight)) {
    stop("Parameter 'index_weight' must be an array.")
  }
  if (!is.array(pdf_weight)) {
    stop("Parameter 'pdf_weight' must be an array.")
  }
  if (is.null(names(dim(index_weight))) || is.null(names(dim(pdf_weight)))) {
    stop("Parameters 'index_weight' and 'pdf_weight'",
         " should have dimmension names.")
  }
  if(!('member' %in% names(dim(index_weight)))) {
    stop("Parameter 'index_weight' must have dimension 'member'.")
  }
  if(!('statistic' %in% names(dim(pdf_weight)))) {
    stop("Parameter 'pdf_weight' must have dimension 'statistic'.")
  }
  if(time_dim_name %in% names(dim(index_weight)) & 
     !time_dim_name %in% names(dim(pdf_weight))) {
    stop("Parameter 'pdf_weight' must have temporal dimension.")
  }
  if(!time_dim_name %in% names(dim(index_weight)) & 
     time_dim_name %in% names(dim(pdf_weight))) {
    stop("Parameter 'index_weight' must have temporal dimension.")
  }
  if(time_dim_name %in% names(dim(index_weight)) & 
     time_dim_name %in% names(dim(pdf_weight)) & 
     dim(index_weight)[time_dim_name] != dim(pdf_weight)[time_dim_name]){
      stop("Length of temporal dimension of parameters must be equal")
  }
  if (dim(pdf_weight)['statistic'] != 2) {
    stop("Length of dimension 'statistic' ",
         "of the parameter 'pdf_weight' must be equal to 2.")
  }
  
  
  aweights <- Apply(list(index_weight, pdf_weight),
                    target_dims = list('member', 'statistic'),
                    fun = .BEI_Weights)$output1
  
  dimnames <- names(dim(index_weight))
  pos <- match(dimnames, names(dim(aweights)))
  aweights <- aperm(aweights, pos)
  names(dim(aweights)) <- dimnames
  return(aweights)
}

#'Atomic BEI_Weights
#'@param index_weight Index (e.g. NAO index) array from a SFS with dimensions
#'  (member)
#'@param pdf_weight Statistics array to define a Gaussian PDF with dimensions
#'  (statistic = 2).
#'  The firt statistic is the parameter 'mean' of the PDF and
#'  the second statistic is the parameter 'standard deviation' of the PDF.
#'@return .BEI_Weights returns an array of with dimensions (member),
#'the normalized weights for each member of a SFS using a Best NAO PDF.
#'@examples
#' # Example for the Atomic BEI_Weights function
#' index_weight <- c(1.3,3,-1)
#' dim(index_weight) <- c(member = 3)
#' pdf_weight <- c(1.5,0.8)
#' dim(pdf_weight) <- c(statistic = 2)
#' res <- .BEI_Weights(index_weight, pdf_weight)
#' dim(res)
#' # member
#' #      3
#'@noRd
.BEI_Weights <- function(index_weight, pdf_weight) {
  aweights <- apply(index_weight, 1, dnorm, mean = pdf_weight[1], sd = pdf_weight[2])
  dim(aweights) <- dim(index_weight)
  sumWeights <- sum(aweights)
  aweightsNorm <- apply(aweights, 1, NormWeight, sumWeights)
  dim(aweightsNorm) <- dim(index_weight)
  return(aweightsNorm)
}

# Auxiliar function to normalize a weight value
NormWeight <- function(weight, sumWeights) {
  return(weight/sumWeights)
}
