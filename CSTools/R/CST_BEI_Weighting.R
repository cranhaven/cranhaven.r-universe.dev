#' Weighting SFSs of a CSTools object.
#' 
#'@author Eroteida Sanchez-Garcia - AEMET, \email{esanchezg@aemet.es}
#' 
#'@description Function to apply weights to a 's2dv_cube' object.  
#'It could return a weighted ensemble mean (deterministic output) or
#'the terciles probabilities (probabilistic output) for Seasonal Forecast 
#'Systems (SFSs).
#' 
#'@references Regionally improved seasonal forecast of precipitation through
#'Best estimation of winter NAO, Sanchez-Garcia, E. et al.,
#'Adv. Sci. Res., 16, 165174, 2019, \doi{10.5194/asr-16-165-2019}
#' 
#'@param var_exp An object of the class 's2dv_cube' containing the variable
#'  (e.g. precipitation, temperature, NAO index) array.
#'  The var_exp object is expected to have an element named \code{$data} with
#'  at least a temporal dimension and a dimension named 'member'.
#'@param aweights Normalized weights array with at least dimensions 
#'  (time, member), when 'time' is the temporal dimension as default. 
#'  When 'aweights' parameter has any other dimensions (as e.g. 'lat') and 
#'  'var_exp' parameter has also the same dimension, they must be equals.  
#'@param terciles A numeric array with at least one dimension 'tercil' equal to 
#'  2, the first element is the lower tercil for a hindcast period, and the second  
#'  element is the upper tercile. By default is NULL, the terciles are computed  
#'  from var_exp data.
#'@param type A character string indicating the type of output. 
#'  If 'type' =  'probs', the function returns, in the element data from 
#'  'var_exp' parameter, an array with at least two 
#'  or four dimensions depending if the variable is spatially aggregated variable 
#'  (as e.g. NAO index), dimension (time, tercil) or it is spatial variable 
#'  (as e.g. precipitation or temperature), dimension (time, tercile, lat, lon), 
#'  containing the terciles probabilities computing with weighted members.
#'  The first tercil is the lower tercile, the second is the normal tercile and
#'  the third is the upper tercile. If 'type' =  'ensembleMean', the function 
#'  returns, in the element data from 'var_exp' parameter, an array with at 
#'  least one or three dimensions depending if the variable is a spatially 
#'  aggregated variable (as e.g. NAO index)(time) or it is spatial variable (as 
#'  e.g. precipitation or temperature) (time, lat, lon), containing the ensemble 
#'  means computing with weighted members.
#'@param time_dim_name A character string indicating the name of the 
#' temporal dimension, by default 'time'.
#'@param memb_dim A character string indicating the name of the 
#'  member dimension, by default 'member'.
#' 
#'@return CST_BEI_Weighting() returns a CSTools object (i.e., of the
#'class 's2dv_cube').
#'This object has at least an element named \code{$data}
#'with at least a temporal dimension (and dimension 'tercil' when the output 
#'are tercile probabilities), containing the ensemble means computing with 
#'weighted members or probabilities of terciles.
#' 
#'@examples
#'var_exp <- 1 : (2 * 4 * 3 * 2)
#'dim(var_exp) <- c(time = 2, member = 4, lat = 3, lon = 2)
#'aweights <- c(0.2, 0.1, 0.3, 0.4, 0.1, 0.2, 0.4, 0.3, 0.1, 0.2, 0.4, 0.4, 0.1, 
#'              0.2, 0.4, 0.2)
#'dim(aweights) <- c(time = 2, member = 4, dataset = 2)
#'var_exp <- list(data = var_exp)
#'class(var_exp) <- 's2dv_cube'
#'res_CST <- CST_BEI_Weighting(var_exp, aweights)
#'@export
CST_BEI_Weighting <- function(var_exp, aweights, terciles = NULL, 
                              type = 'ensembleMean', time_dim_name = 'time',
                              memb_dim = 'member') {
  
  # s2dv_cube
  if (!inherits(var_exp, "s2dv_cube")) {
        stop("Parameter 'var_exp' must be of the class 's2dv_cube', ",
             "as output by CSTools::CST_Load.")
  }
  # type
  if (!is.character(type)) {
    stop("Parameter 'type' must be a character string, 'probs' or ",
         "'ensembleMean', indicating the type of output.")
  }
  if (length(type) > 1) {
    warning("Parameter 'type' has length greater than 1 and ",
            "only the first element will be used.")
    type <- type[1]
  }
  
  if (type == 'ensembleMean') {
    em <- BEI_EMWeighting(var_exp$data, aweights, time_dim_name, memb_dim)
    var_exp$data <- em
  } else if (type == 'probs') {
    if (is.null(terciles)) {
      terciles <- BEI_TercilesWeighting(var_exp$data, aweights, 
                                        time_dim_name = time_dim_name, 
                                        memb_dim = memb_dim)
    }
    probs <- BEI_ProbsWeighting(var_exp$data, aweights, terciles, 
                                time_dim_name = time_dim_name, 
                                memb_dim = memb_dim)
    var_exp$data <- probs
  } else {
      stop("Parameter 'type' must be a character string ('probs' or ",
           "'ensembleMean'), indicating the type of output.")
  }
  return(var_exp)
}

#'@title Computing the weighted ensemble means for SFSs.
#'@author Eroteida Sanchez-Garcia - AEMET, \email{esanchezg@aemet.es}
#'@description This function implements the computation to obtain the weighted
#'ensemble means for SFSs using a normalized weights array,
#' 
#'@references Regionally improved seasonal forecast of precipitation through Best
#'estimation of winter NAO, Sanchez-Garcia, E. et al.,
#'Adv. Sci. Res., 16, 165174, 2019, \doi{10.5194/asr-16-165-2019}
#' 
#'@param var_exp Variable (e.g. precipitation, temperature, NAO index)
#'  array from a SFS with at least dimensions (time, member) for a spatially 
#'  aggregated variable or dimensions (time, member, lat, lon) for a spatial 
#'  variable, as 'time' the spatial dimension by default.
#'@param aweights Normalized weights array with at least dimensions 
#'  (time, member), when 'time' is the temporal dimension as default.
#'@param time_dim_name A character string indicating the name of the 
#'  temporal dimension, by default 'time'.
#'@param memb_dim A character string indicating the name of the 
#'  member dimension, by default 'member'.
#' 
#'@return BEI_EMWeighting() returns an array with at least one or three 
#'dimensions depending if the variable is spatially aggregated variable 
#'(as e.g. NAO index)(time) or it is spatial variable (as e.g. precipitation 
#'or temperature) (time, lat, lon), containing the ensemble means computing
#'with weighted members.
#' 
#'@examples
#'# Example 1 
#'var_exp <- 1 : (2 * 3 * 4)
#'dim(var_exp) <- c(time = 2, dataset = 3, member = 4)
#'aweights <- runif(24, min = 0.001, max = 0.999)
#'dim(aweights) <- c(time = 2, dataset = 3, member = 4)
#'res <- BEI_EMWeighting(var_exp, aweights)
#' 
#'# Example 2 
#'var_exp <- 1 : (2 * 4 * 2 * 3)
#'dim(var_exp) <- c(time = 2, member = 4, lat = 2, lon = 3)
#'aweights <- c(0.2, 0.1, 0.3, 0.4, 0.1, 0.2, 0.4, 0.3)
#'dim(aweights) <- c(time = 2, member = 4)
#'res <- BEI_EMWeighting(var_exp, aweights)
#'
#'@import multiApply
#'@export
BEI_EMWeighting <- function(var_exp, aweights, time_dim_name = 'time', 
                            memb_dim = 'member') {
  # var_exp
  if (!is.array(var_exp)) {
    stop("Parameter 'var_exp' must be an array.")
  }
  # aweights
  if (!is.array(aweights)) {
    stop("Parameter 'aweights' must be an array.")
  }
  # time_dim_name
  if (!is.character(time_dim_name)) {
    stop("Parameter 'time_dim_name' must be a character string indicating",
         " the name of the temporal dimension.")
  }
  if (length(time_dim_name) > 1) {
    warning("Parameter 'time_dim_name' has length greater than 1 and ",
            "only the first element will be used.")
    time_dim_name <- time_dim_name[1]
  }
  # memb_dim
  if (!is.character(memb_dim)) {
    stop("Parameter 'memb_dim' must be a character string indicating",
         " the name of the member dimension.")
  }
  # var_exp, aweights (2)
  if (is.null(names(dim(var_exp))) || is.null(names(dim(aweights)))) {
    stop("Parameters 'var_exp' and 'aweights' should have dimension names.")
  }
  if (!(time_dim_name %in% names(dim(var_exp)))) {
    stop("Parameter 'var_exp' must have temporal dimension.")
  }
  if (!(time_dim_name %in% names(dim(aweights)))) {
    stop("Parameter 'aweights' must have temporal dimension.")
  }
  if (!(memb_dim %in% names(dim(var_exp)))) {
    stop("Parameter 'var_exp' must have member dimension.")
  }
  if (!(memb_dim %in% names(dim(aweights)))) {
    stop("Parameter 'aweights' must have member dimension.")
  }
  if (dim(var_exp)[time_dim_name] != dim(aweights)[time_dim_name]) {
    stop("Length of temporal dimension ",
         "of parameter 'var_exp' and 'aweights' must be equal.")
  }
  if (dim(var_exp)[memb_dim] != dim(aweights)[memb_dim]) {
    stop("Length of member dimension ",
         "of parameter 'var_exp' and 'aweights' must be equals.")
  }
  
  res <- Apply(list(var_exp, aweights),
               target_dims = list(c(time_dim_name, memb_dim), 
                                  c(time_dim_name, memb_dim)),
              fun = .BEI_EMWeighting, time_dim_name)$output1
  return(res)
}

#'Atomic BEI_EMWeighting
#'@param var_exp Variable (e.g. precipitation, temperature, NAO index)
#'  array from a SFS with a temporal dimension, 
#'  by default 'time', and dimension 'member'.
#'@param aweights Normalized weights array with a temporal dimension, 
#'  by default 'time', and dimension 'member'
#'@param time_dim_name A character string indicating the name of the 
#'  temporal dimension, by default 'time'.
#'@return .BEI_EMWeighting returns an array of with a temporal dimension, 
#'by default 'time', containing the weighted ensemble means.
#'@examples
#'# Example for the Atomic BEI_EMWeighting function
#'var_exp <- 1 : 6
#'dim(var_exp) <- c(time = 2, member = 3)
#'aweights <- c(0.28, 0.15, 0.69, 0.64, 0.42, 0.17)
#'dim(aweights) <- c(time = 2, member = 3)
#'res <- .BEI_EMWeighting(var_exp, aweights)
#'@noRd
.BEI_EMWeighting <- function(var_exp, aweights, time_dim_name = 'time') {
  
  posTime <- match(time_dim_name, names(dim(var_exp)))
  var_exp_em <- as.array(apply(var_exp * aweights, posTime, sum))
  names(dim(var_exp_em)) <- time_dim_name
  return(var_exp_em)
}


#' Computing the weighted tercile probabilities for SFSs.
#'@author Eroteida Sanchez-Garcia - AEMET, \email{esanchezg@aemet.es}
#' 
#'@description This function implements the computation to obtain the tercile
#'probabilities for a weighted variable for SFSs using a normalized weights array,
#' 
#'@references Regionally improved seasonal forecast of precipitation through Best
#'estimation of winter NAO, Sanchez-Garcia, E. et al.,
#'Adv. Sci. Res., 16, 165174, 2019, \doi{10.5194/asr-16-165-2019}
#' 
#'@param var_exp Variable (e.g. precipitation, temperature, NAO index)
#'  array from a SFS with at least dimensions (time, member) for a spatially 
#'  aggregated variable or dimensions (time, member, lat, lon) for a spatial 
#'  variable, as 'time' the spatial dimension by default.
#'@param aweights Normalized weights array with at least dimensions 
#'  (time, member), when 'time' is the temporal dimension as default.
#'@param terciles A numeric array with at least one dimension 'tercil' equal to 
#'  2, the first element is the lower tercil for a hindcast period, and the second  
#'  element is the upper tercile. 
#'@param time_dim_name A character string indicating the name of the 
#'  temporal dimension, by default 'time'.
#'@param memb_dim A character string indicating the name of the 
#'  member dimension, by default 'member'.
#' 
#'@return BEI_ProbsWeighting() returns an array with at least two or four 
#'dimensions depending if the variable is a spatially aggregated variable 
#'(as e.g. NAO index)(time, tercil) or it is spatial variable (as e.g. 
#'precipitation or temperature)(time, tercile, lat, lon), containing the 
#'terciles probabilities computing with weighted members.
#'The first tercil is the lower tercile, the second is the normal tercile and
#'the third is the upper tercile.
#' 
#'@examples
#'# Example 1 
#'var_exp <- 1 : (2 * 4)
#'dim(var_exp) <- c(time = 2, member = 4)
#'aweights <- c(0.2, 0.1, 0.3, 0.4, 0.1, 0.2, 0.4, 0.3)
#'dim(aweights) <- c(time = 2, member = 4)
#'terciles <- c(2.5,5)
#'dim(terciles) <- c(tercil = 2)
#'res <- BEI_ProbsWeighting(var_exp, aweights, terciles)
#' 
#'# Example 2 
#'var_exp <- rnorm(48, 50, 9)
#'dim(var_exp) <- c(time = 2, member = 4, lat = 2, lon = 3)
#'aweights <- c(0.2, 0.1, 0.3, 0.4, 0.1, 0.2, 0.4, 0.3)
#'dim(aweights) <- c(time = 2, member = 4)
#'terciles <- rep(c(48,50), 2*3)
#'dim(terciles) <- c(tercil = 2, lat = 2, lon = 3)
#'res <- BEI_ProbsWeighting(var_exp, aweights, terciles)
#'@import multiApply
#'@export
BEI_ProbsWeighting <- function(var_exp, aweights, terciles, 
                               time_dim_name = 'time', memb_dim = 'member') {
  # var_exp
  if (!is.array(var_exp)) {
    stop("Parameter 'var_exp' must be an array.")
  }
  # aweights
  if (!is.array(aweights)) {
    stop("Parameter 'aweights' must be an array.")
  }
  # terciles
  if (is.null(terciles)) {
    stop("Parameter 'terciles' cannot be null.")
  }
  if (!is.array(terciles)) {
    stop("Parameter 'terciles' must be an array.")
  } 
  if (is.null(names(dim(terciles)))) {
    stop("Parameter 'terciles' should have dimension names.")
  }
  if (!('tercil' %in% names(dim(terciles)))) {
    stop("Parameter 'terciles' must have dimension 'tercil'.")
  }
  if (dim(terciles)['tercil'] != 2) {
    stop("Length of dimension 'tercil' ",
         "of parameter 'terciles' must be equal to 2.")
  }
  # time_dim_name
  if (!is.character(time_dim_name)) {
    stop("Parameter 'time_dim_name' must be a character string indicating",
         " the name of the temporal dimension.")
  }
  if (length(time_dim_name) > 1) {
    warning("Parameter 'time_dim_name' has length greater than 1 and ",
            "only the first element will be used.")
    time_dim_name <- time_dim_name[1]
  }
  # memb_dim
  if (!is.character(memb_dim)) {
    stop("Parameter 'memb_dim' must be a character string indicating",
         " the name of the member dimension.")
  }
  # var_exp, terciles, aweights (2)
  if (time_dim_name %in% names(dim(terciles))) {
    stop("Parameter 'terciles' must not have temporal dimension.")
  }
  if (memb_dim %in% names(dim(terciles))) {
    stop("Parameter 'terciles' must not have a member dimension.")
  }
  if (is.null(names(dim(var_exp))) || is.null(names(dim(aweights)))) {
    stop("Parameters 'var_exp' and 'aweights'",
         " should have dimension names.")
  }
  if (!(time_dim_name %in% names(dim(var_exp)))) {
    stop("Parameter 'var_exp' must have temporal dimension.")
  }
  if (!(time_dim_name %in% names(dim(aweights)))) {
    stop("Parameter 'aweights' must have temporal dimension.")
  }
  if (!(memb_dim %in% names(dim(var_exp)))) {
    stop("Parameter 'var_exp' must have member dimension.")
  }
  if (!(memb_dim %in% names(dim(aweights)))) {
    stop("Parameter 'aweights' must have member dimension.")
  }
  if (dim(var_exp)[time_dim_name] != dim(aweights)[time_dim_name]) {
    stop("Length of temporal dimension ",
         "of parameter 'var_exp' and 'aweights' must be equal.")
  }
  if (dim(var_exp)[memb_dim] != dim(aweights)[memb_dim]) {
    stop("Length of member dimension ",
         "of parameter 'var_exp' and 'aweights' must be equal.")
  }

  names_exp <- sort(names(dim(var_exp)))
  names_exp <- names_exp[-which(names_exp %in% c(time_dim_name, memb_dim))]
  names_tercil <- sort(names(dim(terciles)))
  names_tercil <- names_tercil[-which(names_tercil == 'tercil')]

  if (!all(dim(var_exp)[names_exp] == dim(terciles)[names_tercil])) {
    stop("Length of common dimensions ",
         "of parameter 'var_exp' and 'terciles' must be equal.")
  }
  
  res <- Apply(list(var_exp, aweights, terciles),
               target_dims = list(c(time_dim_name, memb_dim), 
                                  c(time_dim_name, memb_dim),
                                  c('tercil')),
               fun = .BEI_ProbsWeighting, time_dim_name)$output1
  return(res)
}

#'Atomic BEI_ProbsWeighting
#'@param var_exp Variable (e.g. precipitation, temperature, NAO index)
#'  array from a SFS with a temporal dimension, 
#'  by default 'time', and dimension 'member'.
#'@param aweights Normalized weights array with a temporal dimension, 
#'  by default 'time', and dimension 'member'
#'@param terciles A numeric array with one dimension 'tercil' equal to 2, 
#'  the first element is the lower tercil for a hindcast period, and the second  
#'  element is the upper tercile.
#'@param time_dim_name A character string indicating the name of the 
#'  temporal dimension, by default 'time'.
#'@param memb_dim A character string indicating the name of the 
#'  member dimension, by default 'member'.
#' 
#'@return .BEI_ProbsWeighting returns an array of with a temporal dimension,
#'as default 'time', and 'tercil' dimension, containing the probabilities 
#'for each tercile computing with weighted members.
#'The firt tercil is the lower tercile, the second is the normal tercile and
#'the third is the upper tercile.
#'@examples
#'# Example
#'var_exp <- 1 : 8
#'dim(var_exp) <- c(stime = 2, member = 4)
#'aweights <- c(0.2, 0.1, 0.3, 0.4, 0.1, 0.2, 0.4, 0.3)
#'dim(aweights) <- c(stime = 2, member = 4)
#'terciles <- quantile(1:8, probs = c(1/3, 2/3))
#'dim(terciles) <- c(tercil = 2)
#'res <- .BEI_ProbsWeighting(var_exp, aweights, terciles, time_dim_name = 'stime')
#'@noRd
.BEI_ProbsWeighting <- function(var_exp, aweights, terciles, 
                                time_dim_name = 'time', memb_dim = 'member') {
  if (any(is.na(var_exp)) || any(is.na(aweights))) {
    probTercile <- array(NA, dim = c(dim(var_exp)[time_dim_name], tercil = 3))
  } else {
    if (any(is.na(terciles))) {
      stop("Terciles are NAs")
    }
    terciles_exp <- list(lowerTercile = terciles[1], 
                         upperTercile = terciles[2])
    
    lowerTercile <- terciles_exp$lowerTercile
    upperTercile <- terciles_exp$upperTercile
    
    # Probabilities
    aTerciles <- Apply(list(var_exp), target_dims = list(memb_dim),
                       fun = Data2Tercil, lowerTercile, upperTercile)$output1
    
    pos <- match(names(dim(aTerciles)), c(time_dim_name, memb_dim))
    aTerciles <- aperm(aTerciles, pos)
    names(dim(aTerciles)) <- c(time_dim_name, memb_dim)
    
    probTercile <- array(NA, dim = c(dim(var_exp)[time_dim_name], tercil = 3))
    for (idTercil in 1:3) {
      probTercile[ ,idTercil] <- Apply(list(aTerciles, aweights),
                                      target_dims = list(memb_dim, memb_dim),
                                      fun = WeightTercil2Prob, idTercil)$output1
    }
  }
  return(probTercile)
}

#'Computing the weighted terciles for SFSs.
#'@author Eroteida Sanchez-Garcia - AEMET, \email{esanchezg@aemet.es}
#' 
#'@description This function implements the computation to obtain the terciles
#'for a weighted variable for SFSs using a normalized weights array,
#' 
#'@references Regionally improved seasonal forecast of precipitation through Best
#'estimation of winter NAO, Sanchez-Garcia, E. et al.,
#'Adv. Sci. Res., 16, 165174, 2019, \doi{10.5194/asr-16-165-2019}
#' 
#'@param var_exp Variable (e.g. precipitation, temperature, NAO index)
#'  array from a SFS with at least dimensions (time, member) for a spatially 
#'  aggregated variable or dimensions (time, member, lat, lon) for a spatial 
#'  variable, as 'time' the spatial dimension by default.
#'@param aweights Normalized weights array with at least dimensions 
#'  (time, member), when 'time' is the temporal dimension as default.
#'@param time_dim_name A character string indicating the name of the 
#'  temporal dimension, by default 'time'.
#'@param memb_dim A character string indicating the name of the 
#'  member dimension, by default 'member'.
#' 
#'@return BEI_TercilesWeighting() returns an array with at least one 
#'dimension depending if the variable is a spatially aggregated variable 
#'(as e.g. NAO index)(tercil) or it is spatial variable (as e.g. 
#'precipitation or temperature)(tercil, lat, lon), containing the 
#'terciles computing with weighted members.
#'The first tercil is the lower tercile, the second is the upper tercile.
#' 
#'@examples
#'# Example 1 
#'var_exp <- 1 : (2 * 4)
#'dim(var_exp) <- c(time = 2, member = 4)
#'aweights<- c(0.2, 0.1, 0.3, 0.4, 0.1, 0.2, 0.4, 0.3)
#'dim(aweights) <- c(time = 2, member = 4)
#'res <- BEI_TercilesWeighting(var_exp, aweights)
#' 
#'# Example 2 
#'var_exp <- rnorm(48, 50, 9)
#'dim(var_exp) <- c(time = 2, member = 4, lat = 2, lon = 3)
#'aweights<- c(0.2, 0.1, 0.3, 0.4, 0.1, 0.2, 0.4, 0.3)
#'dim(aweights) <- c(time = 2, member = 4)
#'res <- BEI_TercilesWeighting(var_exp, aweights)
#'@import multiApply
#'@export
BEI_TercilesWeighting <- function(var_exp, aweights, time_dim_name = 'time', 
                                  memb_dim = 'member') {
  
  # var_exp
  if (!is.array(var_exp)) {
    stop("Parameter 'var_exp' must be an array.")
  }
  # aweights
  if (!is.array(aweights)) {
    stop("Parameter 'aweights' must be an array.")
  }
  # time_dim_name
  if (!is.character(time_dim_name)) {
    stop("Parameter 'time_dim_name' must be a character string indicating",
         " the name of the temporal dimension.")
  }
  if (length(time_dim_name) > 1) {
    warning("Parameter 'time_dim_name' has length greater than 1 and ",
            "only the first element will be used.")
    time_dim_name <- time_dim_name[1]
  }
  # memb_dim
  if (!is.character(memb_dim)) {
    stop("Parameter 'memb_dim' must be a character string indicating",
         " the name of the member dimension.")
  }
  # var_exp, aweights (2)
  if (is.null(names(dim(var_exp))) || is.null(names(dim(aweights)))) {
    stop("Parameters 'var_exp' and 'aweights'",
         " should have dimension names.")
  }
  if (!(time_dim_name %in% names(dim(var_exp)))) {
    stop("Parameter 'var_exp' must have temporal dimension.")
  }
  if (!(time_dim_name %in% names(dim(aweights)))) {
    stop("Parameter 'aweights' must have temporal dimension.")
  }
  if (!(memb_dim %in% names(dim(var_exp)))) {
    stop("Parameter 'var_exp' must have member dimension.")
  }
  if (!(memb_dim %in% names(dim(aweights)))) {
    stop("Parameter 'aweights' must have member dimension.")
  }
  if (dim(var_exp)[time_dim_name] != dim(aweights)[time_dim_name]) {
    stop("Length of temporal dimension ",
         "of parameter 'var_exp' and 'aweights' must be equal.")
  }
  if (dim(var_exp)[memb_dim] != dim(aweights)[memb_dim]) {
    stop("Length of member dimension ",
         "of parameter 'var_exp' and 'aweights' must be equal.")
  }
  
  res <- Apply(list(var_exp, aweights),
               target_dims = list(c(time_dim_name, memb_dim), 
                                  c(time_dim_name, memb_dim)),
               fun = .BEI_TercilesWeighting, time_dim_name)$output1
  return(res)
}

#'Atomic BEI_TercilesWeighting
#'@param var_exp Variable (e.g. precipitation, temperature, NAO index)
#'  array from a SFS with a temporal dimension, 
#'  by default 'time', and dimension 'member'.
#'@param aweights Normalized weights array with a temporal dimension, 
#'  by default 'time', and dimension 'member'
#'@param time_dim_name A character string indicating the name of the 
#'  temporal dimension, by default 'time'.
#'@return .BEI_TercilesWeighting returns a numeric array with dimension tercil
#'equal to 2, the first is the lower tercil and the second the upper tercile, 
#'computing with weighted members considering all members and all period.
#'If any member value for any period is NA , the terciles are not computed, and
#'the function return NA value as tercile upper and lower.
#'@examples
#'# Example
#'var_exp <- 1 : 8
#'dim(var_exp) <- c(stime = 2, member = 4)
#'aweights <- c(0.2, 0.1, 0.3, 0.4, 0.1, 0.2, 0.4, 0.3)
#'dim(aweights) <- c(stime = 2, member = 4)
#'res <- .BEI_TercilesWeighting(var_exp, aweights, time_dim_name = 'stime')
#'@noRd
.BEI_TercilesWeighting <- function(var_exp, aweights, time_dim_name = 'time') {

  if (any(is.na(var_exp)) || any(is.na(aweights))) {
    terciles_exp <- array(c(NA, NA), dim = c(tercil = 2))
  } else {
    l_terciles_exp <- WeightTerciles(var_exp, aweights, time_dim_name)
    terciles_exp <- array(c(l_terciles_exp$lowerTercile, 
                            l_terciles_exp$upperTercile), dim = c(tercil = 2))
  }
  return(terciles_exp)
}

# Auxiliar function to compute in which tercile is a data value
Data2Tercil_old <- function(x, lt, ut) {
  if (is.na(lt) || is.na(ut)) {
    y <- rep(NA, length(x))
  } else {
    y <- rep(2, length(x))
    y[x <= lt] <- 1
    y[x >= ut] <- 3
    if (lt == ut) {
      warning("The upper and lower terciles are equals")
    }
  }
  dim(y) <- c(member = length(x))
  return (y)
}
# Auxiliar function to compute in which tercile is a data value
Data2Tercil <- function(x, lt, ut) {
  if (is.na(lt) || is.na(ut)) {
    y <- rep(NA, length(x))
  } else {
    y <- rep(2, length(x))
    y[x <= lt] <- 1
    y[x >= ut] <- 3
    if (lt == ut) {
      y <- rep(NA, length(x))
    }
  }
  dim(y) <- c(member = length(x))
  y[which(is.na(x))] <- NA
  return (y)
}
# Auxiliar function to convers weighted terciles to probabilities
WeightTercil2Prob <- function(aTerciles, aWeights, idTercil) {
  return(sum(aWeights[which(aTerciles == idTercil)]))
}

# Auxiliar function to compute weighted terciles
WeightTerciles <- function(data, aweights, time_dim_name = 'time') {
  namesdimdata <- names(dim(data))
  namesdimaweights <- names(dim(aweights))
  pos <- match(namesdimdata, namesdimaweights)
  aweights <- aperm(aweights, pos)
  names(dim(aweights)) <- namesdimdata
  vectorData <- as.vector(data)
  vectorWeights <- as.vector(aweights/dim(aweights)[time_dim_name]) # normalized
  #lSortData <- sort(vectorData,index.return=TRUE)
  indSort <- order(vectorData) # index asociated to weight
  # indSort <- lSortData$ix # index asociated to weight
  # corresponding for this data
  dataSort <- vectorData[indSort]
  # dataSort <- lSortData$x
  # Adding normalized weights. When 1/3 is reached, the data value
  # is lower tercile and when 2/3 is reached, it is the upper tercile.
  sumWeights <- 0
  ilowerTercile <- 0
  while ((sumWeights < 1/3) & (ilowerTercile < length(aweights))) {
    ilowerTercile <- ilowerTercile + 1
    sumWeights <- sumWeights + vectorWeights[indSort[ilowerTercile]]
  }
  if (ilowerTercile  == 1) {
    lowerTercile <- dataSort[ilowerTercile]
  } else {
    lowerTercile <- (dataSort[ilowerTercile] +
                     dataSort[ilowerTercile - 1]) / 2
  }
  sumWeights <- 0
  iupperTercile <- 0
  while ((sumWeights < 2/3) & (iupperTercile < length(aweights))) {
    iupperTercile <- iupperTercile + 1
    sumWeights <- sumWeights + vectorWeights[indSort[iupperTercile]]
  }
  if (iupperTercile == 1) {
    upperTercile <- dataSort[iupperTercile]
  } else {
    upperTercile <- (dataSort[iupperTercile]+
                     dataSort[iupperTercile - 1]) / 2
  }
  return(list(lowerTercile = lowerTercile, upperTercile = upperTercile))
}
