#' Bivariate Rank-Based Variable Selection
#'
#' The `rbvs.user` function ranks covariates based on their importance using a copula-based approach.
#' It is designed to work with survival or time-to-event data.
#'
#' @param x A data frame containing the covariates.
#' @param y A data frame containing the time to events and censoring information.
#' @param ny The number of rows in the `y` data frame, representing the number of events.
#' @param m The number of bootstrap iterations to perform.
#' @param copula A character string specifying the type of copula to use in the fitting procedure.
#'               Valid options include "N", "C0", "C90", "C180", "C270", "GAL0", "GAL90", "GAL180", "GAL270",
#'               "J0", "J90", "J180", "J270", "G0", "G90", "G180", "G270", "F", "AMH", "FGM", "T", "PL", "HO".
#' @param margins A character vector specifying the link functions to use in the fitting procedure.
#'                Options are "PH" (Proportional Hazards), "PO" (Proportional Odds), and "probit" (Probit).
#' @param n.rep The number of replications to use in the bootstrap procedure.
#' @param metric Character, specifies the metric used for ranking the variables.
#'        Must be one of 'CE', 'FIM', 'Abs'. Default is 'FIM'.
#'
#'
#'
#' @return A list containing rankings for the criteria selected. The options for criteria are based on the `measure` function
#'         and can include 'FIM' (Fisher Information Measure), 'Abs' (Absolute Value) and 'CE' (Copula Entropy).
#'
#' @noRd
#'
rbvs.user=function(x, y, ny, m, copula, margins,n.rep, metric){






  m.rankE1<- vector(mode='list', n.rep)
  m.rankE2<- vector(mode='list', n.rep)
  m.rank_ABSBeta1E<- vector(mode='list', n.rep)
  m.rank_ABSBeta2E<- vector(mode='list', n.rep)

  for(rk in 1:n.rep){
    subs=subsample(n=ny, m, B=1) # Generate a subsample without replacement
    xs <- x[subs[, 1],]
    xs=x[subs[,1],]
    ys=y[subs[,1],]
    out.rbvs=measure(xs,ys, copula, margins,  metric)
    m.rankE1[[rk]]=out.rbvs$RankBeta1E
    m.rankE2[[rk]]=out.rbvs$RankBeta2E


    #m.rank_ABSBeta1E[[rk]]=out.rbvs$RankABSBeta1E
    #m.rank_ABSBeta2E[[rk]]=out.rbvs$RankABSBeta2E

    # Overwrite the previous message with the new message
  #  msg <- paste("Bootstrap:", rk, "of:", n.rep) # Extra spaces to ensure overwriting
  #  cat("\r", msg, sep = "")
    #flush.console()
  }



  # Combine rankings into a list
  m.rank<- list(CE1=m.rankE1,
                CE2=m.rankE2 )

  return(m.rank)
}



