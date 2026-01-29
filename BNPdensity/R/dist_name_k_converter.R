dist_name_k_index_dict <- setNames(object = 1:10, nm = c("normal", "gamma", "beta", "exponential", "lognormal", "half-Cauchy", "half-normal", "half-student", "uniform", "truncated normal"))
dist_name_k_index_dict["norm"] <- 1
dist_name_k_index_dict["exp"] <- 4
dist_name_k_index_dict["double exponential"] <- 4
dist_name_k_index_dict["lnorm"] <- 5
dist_name_k_index_dict["halfcauchy"] <- 6
dist_name_k_index_dict["halfnorm"] <- 7
dist_name_k_index_dict["halft"] <- 8
dist_name_k_index_dict["unif"] <- 9

#' Convert distribution names to indices
#'
#' @param distname a character representing the distribution name. Allowed names are "normal", "gamma", "beta", "exponential", "double exponential", "lognormal", "half-Cauchy", "half-normal", "half-student", "uniform" and "truncated normal", or their common abbreviations "norm", "exp", "lnorm", "halfcauchy", "halfnorm", "halft" and "unif".
#'
#' @return an index describing the distribution. 1 = Normal; 2
#' = Gamma; 3 = Beta; 4 = Double Exponential; 5 = Lognormal, 6 = Half-Cauchy, 7 = Half-normal, 8 = Half-Student, 9 = Uniform, 10 = Truncated normal
#'
dist_name_k_index_converter <- function(distname) {
  if (length(distname) > 1) stop("Please provide a single distribution name.")
  if (!is.character(distname)) {
    stop("distname is not a character, please provide a proper distribution name as a character.")
  } else {
    if (!(distname %in% names(dist_name_k_index_dict))) {
      stop("The distribution name was not recognised as one of the available distributions.")
    } else {
      return(dist_name_k_index_dict[distname])
    }
  }
}

#' Process the distribution name argument into a distribution index
#'
#' This function is intended to help with compatibility with the previous versions of the package.
#'
#' @param distname Can be an integer or a distribution name. Allowed names are "normal", "gamma", "beta", "exponential", "lognormal", "half-Cauchy", "half-normal", "half-student", "uniform" and "truncated normal", or their common abbreviations "norm", "exp", "halfcauchy", "halfnorm", "halft" and "unif".
#'
#' @return an integer both if distname is an integer or a character
process_dist_name <- function(distname) {
  if (distname %in% 1:10) {
    return(distname)
  } else {
    return(dist_name_k_index_converter(distname = distname))
  }
}
