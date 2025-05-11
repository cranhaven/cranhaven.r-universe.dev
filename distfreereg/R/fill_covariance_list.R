fill_covariance_list <- function(need, covariance_list, matsqrt_tol, solve_tol){
  # This function is used by compare.default() to fill in required but missing
  # elements from the covariance list (that is, the list that contains up to
  # four equivalent specifications of a covariance structure). The function that
  # generates the errors can have any of these as an argument, and this function
  # fills in any gaps.
  #
  # The order of the conditional clauses is determined by the quickest way to
  # "get" to a needed matrix given matrices already present in covariance_list.
  # (Note that matrices are added to that list as they are computed.) Three
  # operations are possibly required, listed in ascending order of speed:
  # 1. squaring; e.g., P = Q %*% Q. (This is actually computed using
  #    crossprod() within matsqsym, which is faster that A %*% A, but equivalent
  #    since all matrices here are symmetric.)
  # 2. inverting; e.g., P = matinv(Sigma).
  # 3. square root; e.g., Q = matsqrt(P).
  #
  # Preference is given to paths through Q, which leads to the code blocks' not
  # being quite parallel.
  
  stopifnot(all(need %in% c("Sigma", "SqrtSigma", "P", "Q")))

  # First, calculate Sigma if necessary.
  if("Sigma" %in% need && is.null(covariance_list[["Sigma"]])){
    if(!is.null(covariance_list[["SqrtSigma"]])){
      covariance_list[["Sigma"]] <- matsqsym(covariance_list[["SqrtSigma"]])
    } else {
      if(is.null(covariance_list[["P"]])){
        covariance_list[["P"]] <- matsqsym(covariance_list[["Q"]])
      }
      covariance_list[["Sigma"]] <- matinv(covariance_list[["P"]], tol = solve_tol)
    }
  }
  
  # Next, SqrtSigma.
  if("SqrtSigma" %in% need && is.null(covariance_list[["SqrtSigma"]])){
    if(!is.null(covariance_list[["Q"]])){
      covariance_list[["SqrtSigma"]] <- matinv(covariance_list[["Q"]], tol = solve_tol)
    } else {
      if(is.null(covariance_list[["Sigma"]])){
        covariance_list[["Q"]] <- matsqrt(covariance_list[["P"]], tol = matsqrt_tol)
        covariance_list[["SqrtSigma"]] <- matinv(covariance_list[["Q"]], tol = solve_tol)
      } else {
        covariance_list[["SqrtSigma"]] <- matsqrt(covariance_list[["Sigma"]], tol = matsqrt_tol)
      }
    }
  }
  
  # Now P.
  if("P" %in% need && is.null(covariance_list[["P"]])){
    if(!is.null(covariance_list[["Q"]])){
      covariance_list[["P"]] <- matsqsym(covariance_list[["Q"]])
    } else {
      if(is.null(covariance_list[["Sigma"]])){
        covariance_list[["Q"]] <- matinv(covariance_list[["SqrtSigma"]], tol = solve_tol)
        covariance_list[["P"]] <- matsqsym(covariance_list[["Q"]])
      } else {
        covariance_list[["P"]] <- matinv(covariance_list[["Sigma"]], tol = solve_tol)
      }
    }
  }
  
  # Finally, Q.
  if("Q" %in% need && is.null(covariance_list[["Q"]])){
    if(!is.null(covariance_list[["SqrtSigma"]])){
      covariance_list[["Q"]] <- matinv(covariance_list[["SqrtSigma"]], tol = solve_tol)
    } else {
      if(is.null(covariance_list[["P"]])){
        covariance_list[["P"]] <- matinv(covariance_list[["Sigma"]], tol = solve_tol)
      }
      covariance_list[["Q"]] <- matsqrt(covariance_list[["P"]], tol = matsqrt_tol)
    }
  }
  
  return(covariance_list)
}
