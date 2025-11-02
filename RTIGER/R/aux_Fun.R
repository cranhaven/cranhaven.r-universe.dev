#' Extract the parameters for the R-optimization step
#' @param params list of parameters for the HMM.
#' @param states names of the states.
#'
#' @keywords internal
#' @noRd
#'

extract_emissions = function(params, states = c("mat","het","pat")){
  res = lapply(states, function(state){
    vec = c(params[["paraBetaAlpha"]][state,1], params[["paraBetaBeta"]][state,1])
    names(vec) = c("alpha", "beta")
    return(vec)
  })
  names(res) = states
  return(res)


}


#' Generate random parameters
#' @param params list of parameters for the HMM.
#' @param randomize logical variable wether the initial values should be taken randomly.
#'
#' @keywords internal
#' @noRd
#'

generate_params = function(params=list(), rigidity, nstates,randomize = TRUE){
  # vstates = c("pat","het","mat")
  # mstates = c("+","badP","badM")
  # estates = c("pat","het","mat","badP","badM")
  if(is.null(params$nstates)) params$nstates = as.integer(nstates)
  # transition matrix
  if (is.null(params$a)){
    # transition matrix a
    vstates = ifelse(rep(nstates == 3, nstates),c("pat","het","mat"), paste0("state_", 1:nstates))
    a = matrix(0.1,ncol=nstates,nrow=nstates) + diag(nstates)*10
    # if (randomize) a = a + matrix(runif(9,min = 0, max = 0.5),ncol=3)
    if (randomize) a = a + matrix(runif(nstates*nstates,min = 0, max = 0.01),ncol=nstates)
    a = a / rowSums(a)
    dimnames(a) = list(vstates,vstates)
    # a["pat", "mat"] = a["mat", "pat"] = 1e-16
    # a = a/rowSums(a)
    # a["het", "het"] = mean(a["pat", "pat"], a["mat", "mat"])
    # a = a/rowSums(a)
    params$logtransition = log(a)
  }

  # beta-binomial emission distributions
  if (is.null(params$alpha)){
    psi = vector("list", nstates)
    priorstrength = 1 + randomize*runif(1,min = -0.5, max = 0.5)
    alphavec = ifelse(rep(nstates == 3, nstates), as.vector(c(20,20,1)), sample(c(20,1), nstates, replace = TRUE))
    alphavec = alphavec +  randomize*runif(nstates,min = -.5, max = .5)
    alpha_s = alphavec * priorstrength
    names(alpha_s) = vstates
    betavec = ifelse(rep(nstates == 3, nstates), as.vector(c(1,20,20)), sample(c(20,1), nstates, replace = TRUE))
    betavec = betavec +  randomize*runif(nstates ,min = -.5, max = .5)
    beta_s = betavec * priorstrength
    names(beta_s) = vstates

    params$paraBetaAlpha = alpha_s
    params$paraBetaBeta = beta_s
  }
  params$rigidity = as.integer(rigidity)
  # starting distribution for hidden states
  if (is.null(params$piv)){
    piv = rep(1, nstates) + randomize * runif(nstates,min=-1,max=3)
    piv = piv / sum(piv)
    names(piv) = vstates
    params$logpi = piv
  }

  # # probabilities for the marker states
  # if (is.null(params$pim)){
  #   pim = c(8,1,1) + randomize * runif(3,min=-1,max=3)
  #   pim = pim / sum(pim)
  #   names(pim) = mstates
  #   params$pim = pim
  # }

  return(params)
} # end generate_params

#' Checks if the txt files from input have the correct format and removes observations with 0 observations
#'
#' @keywords internal
#' @noRd
#'
checkfileColumns = function(f,samp){
  if(!is(f$V2, "integer")){
    stop(cat(paste("Second column in file ", samp, " is not an integer.\nGenomic positions must be integers!" )))
  }

  if(!is(f$V4, "integer") | !is(f$V6, "integer")){
    stop(cat(paste("Either one or both of the allele counts in file ", samp, " are not integers.\nAllele counts must be integers!")))
  } # If values are integers

  f <- f[rowSums(f[,c(4,6)]) != 0, ]
  d = paste(f$V1, f$V2, sep = "_")
  f <- f[!duplicated(d),]

  return(f)
}

#' Checks if the txt files from input have the correct format for raw data
#'
#' @keywords internal
#' @noRd
#'
checkfileColumnsRaw = function(f,samp){
  if(!is(f$V2, "integer")){
    stop(cat(paste("Second column in file ", samp, " is not an integer.\nGenomic positions must be integers!" )))
  }

  if(!is(f$V4, "integer") | !is(f$V6, "integer")){
    stop(cat(paste("Either one or both of the allele counts in file ", samp, " are not integers.\nAllele counts must be integers!")))
  } # If values are integers

  f$V4[rowSums(f[,c(4,6)]) == 0] = NA
  f$V6[rowSums(f[,c(4,6)], na.rm = TRUE) == 0] = NA

  f <- f[!duplicated(f$V2),]

  return(f)
}

#' Create info list for observations
#'
#' @keywords internal
#' @noRd
#'

create_info <- function(obs){
  info = list()

  info$sample_names = names(obs)
  info$part_names = names(obs[[1]])
  info$marker_names = lapply(obs[[1]], colnames)
  info$sample_nr = length(obs)
  info$part_nr = length(obs[[1]])
  info$part_lengths = sapply(obs, function(samp){
    dims = sapply(samp, ncol)
    return(dims)
  })
  info$NAs_matrices = lapply(obs, function(x) lapply(x, function(y){
    res = apply(y, 2, function(r) ifelse(is.na(r), NA, 1))
  }))
  return(info)
}




