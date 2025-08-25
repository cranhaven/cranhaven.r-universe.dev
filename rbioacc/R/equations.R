#' Equations of the mathematical model used for the fit
#' 
#' @param fit An object of class \code{fitTK}
#' @param object The data.frame used as the base as the fit object
#' 
#' @return A vector of strings each containing an equation
#' 
#' @export

equations <- function(fit, object){
  
  nexp <- fit$stanTKdata$n_exp
  nmet <- fit$stanTKdata$n_met
  elim <- ifelse(is.infinite(fit$stanTKdata$elim_rate), TRUE, FALSE)
  growth <- (fit$stanTKdata$n_out == 2)
  
  whichexpeq <- exposure_names(object)
  parkexpeq <- NULL
  for (i in 1:nexp) {
    exp <- unlist(whichexpeq[i])
    parkexpeq[i] <- paste0("ku", exp, "\\times c", exp)
  }

  Ueq <- stringr::str_c(parkexpeq, collapse = "+")
  
  parkm <- paste0(rep("km", nmet), 1:nmet)
  parkem <- paste0(rep("kem", nmet), 1:nmet)
  varmeteq <- paste0(rep("Cm"), 1:nmet)
  M <- stringr::str_c(parkm, collapse = "+")
  
  if (elim) {
    E <- "k_{ee}"
  }
  if (growth) {
    E <- paste(E, "k_{eg}", sep = "+")
  }
  
  if (elim) {
    ifelse(nmet != 0,  B <- paste(E, M, sep = "+"),  B <- paste0(E))
  } else {
    ifelse(nmet != 0,  B <- paste0(M),  B <- "0")
  }
  
  if(elim) {
    Eqacc <- paste0("$\\frac{dCp(t)}{dt} = ", Ueq, " - (", B, ") \\times Cp(t),\\ for\\ 0 \\leq t \\leq t_{c}$")
  } else {
    Eqacc <- paste0("$\\frac{dCp(t)}{dt} = ", Ueq, ",\\ for\\ 0 \\leq t \\leq t_{c}")
  }
  
  if (elim) {
    Eqdep <- paste0("$\\frac{dCp(t)}{dt} = -(", B, ") \\times Cp(t),\\ for\\ t > t_{c}$")
  }
  
  if(nmet != 0) {
    Eqmet <- c()
    for(l in 1:nmet) {
      tmp <- paste0("$\\frac{d", varmeteq[l], "(t)}{dt} = ")
      tmp <- paste0(tmp, parkm[l], "\\times Cp(t)-", parkem[l], "\\times ", varmeteq[l], "(t)$")
      Eqmet <- c(Eqmet, tmp)
    }
  }
  
  # Growth
  if (growth) {
    EqGrowth <- paste0("$\\frac{dG(t)}{dt} = k_{eg} \\times (g_{max} - G(t))$")

  }
  
  diffeq <- c(Eqacc)
  if (elim){
    diffeq <- c(diffeq, Eqdep)
  }
  if (nmet != 0) {
    diffeq <- c(diffeq, Eqmet)
  }
  if (growth) {
    diffeq <- c(diffeq, EqGrowth)
  }
  
  return(.exprToLaTeX(diffeq))
  
}

# convert variable names
.exprToLaTeX <- function(equations) {
  for (i in 1:length(equations)) {
    s <- equations[i]
    s <- stringr::str_replace_all(s, "ku(\\w+)", "k_{u\\1}")
    s <- stringr::str_replace_all(s, "ke(\\w+)", "k_{e\\1}")
    s <- stringr::str_replace_all(s, "km(\\w+)", "k_{m\\1}")
    s <- stringr::str_replace_all(s, "C(\\w+)", "C_{\\1}")
    s <- stringr::str_replace_all(s, "c(\\w+)", "c_{\\1}")
    
    equations[i] <- s
  }
  
  return(equations)
}