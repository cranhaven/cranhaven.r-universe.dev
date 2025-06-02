#' @title Mutual information for any copula in the package
#' @description Returns the mutual information of a copula.
#' @param cop Copula name.
#' @param theta Copula parameters.

mi.cop <- function(theta,cop){
  switch (cop,
          "frank" = mi(frankCopula(param = theta, dim = 2)),
          "gaussian" = mi.gaussian(theta),
          "clayton" = mi(claytonCopula(param = theta, dim = 2)),
          "joe" = mi(joeCopula(param = theta, dim = 2)),
          "gumbel" = mi(gumbelCopula(param = theta, dim = 2)),
          "amh" = mi(amhCopula(param = theta, dim = 2)),
          "grid" = measures.grid(gc = theta, measures = c("mi"))$mi 
  )
}

