.error.handler <- function(x) {

  names.x <- names(x)
  if(any(!names.x %in% c("", "n", "J", "K", "L", "n0", "J0", "K0", "L0", "g1", "g2", "g3", "gm3", "g4",
                         "r21","r22","r23","r24", "r2t2", "r2t3", "r2t4",
                         "rel1", "rho2", "rho3", "rho4", "rhom2", "rhom3",
                         "omega2", "omega3", "omega4", "omegam2", "omegam3",
                         "omega2tm", "omega2t", "esv2", "esv3", "esv4",
                         "r2m1", "r2m2", "r2m3",
                         "q", "p", "alpha", "power", "mdes", "es", "escp", "esa", "esb", "esb1", "esb2", "esB",
                         "esab", "esab1", "esaB", "esa0", "esb0", "esb10", "esB0", "powera", "powerb",
                         "powerb1", "powerB", "maxiter", "abstol", "tol", "two.tailed",
                         "mc", "nsims", "ndraws",
                         # partially nested designs
                         "rho_ic", "rho2_trt", "rho3_trt", "ic_size", "ratio_tc_var", "z.test", "df",
                         # partially nested designs probing mediation
                         "rhom_ctrl", "rhom2_ctrl", "rhom3_trt", "rhom2_trt", "rhom_trt", "rho_trt",
                         "r2m1_ctrl", "r2m2_ctrl", "r2m1_trt", "r2m2_trt", "r2m3_trt",
                         "r2y1z_trt", "r2y2z_trt", "r2y3z_trt",
                         "r2y1_trt", "r2y2_trt", "r2y3_trt",
                         "g1_ctrl", "g2_ctrl", "g2_trt", "g3_trt",
                         "n_ctrl", "n_trt", "J_ctrl", "J_trt",  "K_trt"))) {
    stop("Unused arguments", call. = FALSE)
  }

  # exclude NULL arguments and redefine the check list
  idx.notnull <- match(names(lapply(x, is.null)[!lapply(x, is.null) == TRUE]),
                       names.x)
  parms.notnull <- x[idx.notnull]
  names.x <- names(parms.notnull)
  x <- lapply(parms.notnull, eval)

  # validity check for sample sizes
  idx.n <- intersect(c("n","J","K","L","df","ic_size",
                       # partially nested designs probing mediation
                       "n_ctrl", "n_trt", "J_ctrl", "J_trt",  "K_trt"),  names.x)
  length.unlist.n <- length(unlist(x[idx.n]))
  length.list.n <- length(x[idx.n])
  if(length.list.n == length.unlist.n){
    if(any(x[idx.n] <= 0) ||
       any(lapply(x[idx.n], function(x)!is.numeric(x)) == TRUE) ||
       any(lapply(x[idx.n], length) > 1)) {
      stop("Incorrect sample size", call.=FALSE)
    }
  } else {
    stop("Incorrect sample size", call.=FALSE)
  }

  # validity check for number of covariates
  idx.g <- intersect(c("g1", "g2", "g3", "gm3", "g4",
                       # partially nested designs probing mediation
                       "g1_ctrl", "g2_ctrl", "g2_trt", "g3_trt"),  names.x)
  if(length(idx.g) > 0) {
    if(!is.numeric(unlist(x[idx.g])) |
       any(lapply(x[idx.g], length) > 1) |
       any(x[idx.g] < 0)) {
      stop("Incorrect number of covariates", call.=FALSE)
    }
  }

  # validity check for variance parameters, proportions, and probabilities
  idx.var <- intersect(c("r21", "r22", "r23","r24", "r2t2", "r2t3", "r2t4",
                         "r2m1", "r2m2", "r2m3","rhom2", "rhom3",
                         "omega2tm", "omega2t", "omegam2", "omegam3",
                         "esv2", "esv3", "esv4",
                         "rel1", "rho2", "rho3", "rho4", "omega2", "omega3", "omega4",
                         "q", "q", "p", "p", "alpha", "power", "powera",
                         "powerb", "powerb1", "powerB",
                         "rho_ic", "rho2_trt", "rho3_trt",
                         # partially nested designs probing mediation
                         "rhom_ctrl", "rhom3_trt", "rhom2_trt", "rhom_trt", "rho_trt",
                         "r2m1_ctrl", "r2m2_ctrl", "r2m1_trt", "r2m2_trt", "r2m3_trt",
                         "r2y1z_trt", "r2y2z_trt", "r2y3z_trt",
                         "r2y1_trt", "r2y2_trt", "r2y3_trt"),  names.x)
  if(any(lapply(x[idx.var], function(x)!is.numeric(x)) == TRUE) |
     any(lapply(x[idx.var], length) > 1) |
     any(x[idx.var] < 0) |
     any(x[idx.var] > 1)) {
    stop("Incorrect value for [0, 1] bounded argument(s)", call.=FALSE)
  }

  # validity check for R-squared value and number of covariate consistency
  # not checked for partially nested designs probing mediation
  idx.r2 <- intersect(c("r21", "r22", "r23", "r24", "r2t2",
                        "r2t3", "r2t4", "r2m1", "r2m2", "r2m3"), names.x)
  if(length(idx.g) != 0 & length(idx.r2) != 0) {
    if (any(x[idx.r2] > 0) & any(x[idx.g] == 0)) {
      x.r2 <- x[idx.r2]
      x.g <- x[idx.g]
      err.r2 <- names(x.r2[x.r2 > 0])
      err.g <- names(x.g[x.g == 0])
      if (any(substr(err.r2, nchar(err.r2), nchar(err.r2))== substr(err.g, 2, 2))){
        warning("R-squared value for a level may not be greater than zero",
                call. = FALSE)
      }
    } else if (any(x[idx.r2] == 0) & any(x[idx.g] > 0)) {
      x.r2 <- x[idx.r2]
      x.g <- x[idx.g]
      err.r2 <- names(x.r2[x.r2 == 0])
      err.g <- names(x.g[x.g > 0])
      if (any(substr(err.r2, nchar(err.r2), nchar(err.r2)) == substr(err.g, 2, 2))) {
        warning("R-squared value for a level may not be zero",
                call. = FALSE)
      }
    }
  }

  # validity check for effect size
  idx.es <- intersect(c("escp", "es", "esa", "esb", "esb1", "esb2", "esB",
                        "esa0", "esb0", "esb10", "esB0"),  names.x)
  if(any(lapply(x[idx.es], function(x)!is.numeric(x)) == TRUE) |
     any(lapply(x[idx.es], length) > 1) |
     any(x[idx.es] < 0)) {
    stop("Incorrect value for effect size", call.=FALSE)
  }
  if(any(x[idx.es] > 5)) {
    stop("Extreme value for effect size", call.=FALSE)
  }

  # validity check for two-tailed test
  if("two.tailed" %in% names.x){
    if(!is.logical(x$two.tailed) | length(x$two.tailed) > 1 ){
      stop("Non-logical value for argument 'two.tailed'", call.=FALSE)
    }
  }

    # validity check for z-test
  if("z.test" %in% names.x){
    if(!is.logical(x$z.test) | length(x$z.test) > 1 ){
      stop("Non-logical value for argument 'z.test'", call.=FALSE)
    }
  }

  # validity check for maxiter
  if("maxiter" %in% names.x){
    if(length(x$maxiter) > 1 |
       !is.numeric(x$maxiter) |
       x$maxiter < 10 |
       x$maxiter > 5000){
      stop("Incorrect value for argument 'maxiter'", call.=FALSE)
    }
  }

  # validity check for ratio_tc_var
  if("ratio_tc_var" %in% names.x){
    if(length(x$ratio_tc_var) > 1 |
       !is.numeric(x$ratio_tc_var) |
       x$ratio_tc_var < .50 |
       x$ratio_tc_var > 5){
      stop("Incorrect value for argument 'ratio_tc_var'", call.=FALSE)
    }
  }

  # validity check for mc
  if("mc" %in% names.x){
    if(length(x$mc) > 1 |
       !is.logical(x$mc)){
      stop("Incorrect value for argument 'mc'", call.=FALSE)
    }
  }

  # validity check for nsims
  if("nsims" %in% names.x){
    if(length(x$nsims) > 1 |
       !is.numeric(x$nsims) |
       x$nsims < 10 |
       x$nsims > 1e6){
      stop("Incorrect value for argument 'nsims'", call.=FALSE)
    }
  }

  # validity check for ndraws
  if("ndraws" %in% names.x){
    if(length(x$ndraws) > 1 |
       !is.numeric(x$ndraws) |
       x$ndraws < 10 |
       x$ndraws > 1e6){
      stop("Incorrect value for argument 'ndraws'", call.=FALSE)
    }
  }

  # warn user specifying both esv and omega
  if(all(c("esv2", "omega2") %in% names.x)){
    message("Ignoring any specification to 'esv2'.\n Note that 'omega2 = esv2 / rho2'.\n Use either 'omega2' or 'esv2")
  }
  if(all(c("esv3", "omega3") %in% names.x)){
    message("Ignoring any specification to 'esv3'.\n Note that 'omega3 = esv3 / rho3'.\n Use either 'omega3' or 'esv3")
  }
   if(all(c("esv4", "omega4") %in% names.x)){
    message("Ignoring any specification to 'esv4'.\n Note that 'omega4 = esv4 / rho4'. \n Use either 'omega4' or 'esv4")
  }

}#.error.handler
