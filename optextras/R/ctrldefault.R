##################################################################
ctrldefault <- function(npar) { 
# THIS IS FULL VERSION FOR optimx
#
     ## These are DEFAULTS. They may be nonsense in some contexts.

      allmeth <- c("BFGS", "CG", "Nelder-Mead", "L-BFGS-B", "nlm", "nlminb", 
                "lbfgsb3c", "Rcgmin", "Rtnmin", "Rvmmin", "snewton", "snewtonm",
                 "spg", "ucminf", "newuoa", "bobyqa", "nmkb", "hjkb", "hjn", 
                 "lbfgs", "subplex")

#  allpkg has package where element of allmeth is found
      allpkg <-  c("stats", "stats", "stats", "stats", "stats", "stats",
                "lbfgsb3c", "optimx", "optimx", "optimx", "optimx", "optimx",
                "BB", "ucminf", "minqa", "minqa", "dfoptim", "dfoptim", 
                "optimx", "lbfgs", "subplex")

     # 160628: uobyqa removed as it fails hobbs from 1,1,1 unscaled

      bdmeth <- c("L-BFGS-B", "nlminb", "lbfgsb3c", "Rcgmin", "Rtnmin", "Rvmmin",  
                "bobyqa", "nmkb", "hjkb", "hjn")

      maskmeth <- c("Rcgmin", "Rvmmin", "hjn")
 
# offset changed from 100 to 1000 on 180710
      ctrl.default <- list(
        acctol = 0.0001, # used for acceptable point test in backtrack linesearch
        all.methods = FALSE, # we do NOT want all methods to be the default
        allmeth = allmeth, # to define the set of all methods
        allpkg = allpkg, # to list all the packages required
      	badval = (0.5)*.Machine$double.xmax, # use this value as a flag of non-computable item
        bdmeth = bdmeth, # list of bounds-constrained methods
        bigval = .Machine$double.xmax*0.01, # a very large number (note smaller than badval)
        defgrapprox = "grfwd", # use forward approximation as default. Could argue for grcentral
        defmethod = "Nelder-Mead", # same default as optim(). Do we want this?
        defstep=1, # default stepsize is 1 (Newton stepsize)
        dowarn = TRUE,  # generally want to turn on warnings
        eps = 1e-07, # a tolerance for a small quantity (about single precision level)
        epstol = .Machine$double.eps, # but this is the machine epsilon
        fnscale = 1.0, # Normally scale function by 1. -1 maximizes functions
        grtesttol=(.Machine$double.eps)^(1/3), # a test tolerance for equality for numeric and 
        # analytic gradients
        have.bounds = FALSE, # normally have UNCONSTRAINED function
        hessasymtol = 0.0001, # tolerance for testing Hessian asymmetry. Note that it 
        # cannot be too small or we get too many false positives
        hesstesttol=(.Machine$double.eps)^(1/3), # See grtesttol. This is for Hessian approx.
        keepinputpar = FALSE, # When TRUE do NOT allow bounds check to change parameter values
        kkt = TRUE, # Normally test KKT conditions. May take a LONG time.
        kkttol = 0.001, # tolerance for testing KKT small gradient condition 
        kkt2tol = 1.0E-6, # tolerance for testing KKT curvature condition
        maskmeth = maskmeth, # list of methods that allow masks (fixed parameters)
        maximize = FALSE, # normally MINIMIZE (see fnscale)
        maxit = 500*round(sqrt(npar+1)), # limit on number of iterations or gradient evaluations
        maxfeval = 5000*round(sqrt(npar+1)), # limit on function evaluations
        offset = 1000.0, # used for equality test (a + offset) == (b + offset)
        parchanged = FALSE, # set TRUE when bounds check has changed parameter values
        parscale = rep(1, npar), # vector of scaling factors for parameters. Try to get
        # scaled parameters to have magnitude in range (1, 10)
        reltest = 100.0,
        save.failures = TRUE,
      	scaletol = 3, 
        stepdec = 0.2, 
        steplen0 = 0.75, 
        stepmax = 5,
        stepmin = 0,
        stepredn = 0.2,
        stopbadupdate = FALSE,
        tol = 0, 
        trace = 0,
        watch = FALSE
      )
}
##################################################################

dispdefault <- function(ctrl) { # Display the control vector using cat and print
  cat("Control vector (ctrl) multiple element items first:\n")
  cat("allmeth:")
  print(ctrl$allmeth)
  cat("allpkg:")
  print(ctrl$allpkg)
  cat("bdmeth:")
  print(ctrl$bdmeth)
  cat("maskmeth:")
  print(ctrl$maskmeth)
  cat("parscale:")
  print(ctrl$parscale)
  cat("acctol=",ctrl$acctol,"  all.methods=",ctrl$all.methods,"  badval=",ctrl$badval,
    "  bigval=",ctrl$bigval,"\n")
  cat("defgrapprox=",ctrl$defgrapprox,"  defmethod=",ctrl$defmethod,"  defstep=",ctrl$defstep,"\n")
  cat("dowarn=",ctrl$dowarn,"  eps=",ctrl$eps,"  epstol=",ctrl$epstol,"  fnscale=",ctrl$fnscale,"\n")
  cat("grtesttol=",ctrl$grtesttol,"  have.bounds=",ctrl$have.bounds,"  hessasymptol=",ctrl$hessasymptol,"\n")
  cat("hesttesttol=",ctrl$hesstesttol,"  keepinputpar=",ctrl$keepinputpar,"  kkt=",ctrl$kkt,"\n")
  cat("kkttol=",ctrl$kkttol,"  kkt2tol=",ctrl$kkt2tol,"  maximize=",ctrl$maximize,"\n")
  cat("maxit=",ctrl$maxit,"  maxfeval=",ctrl$maxfeval,"  offset=",ctrl$offset,
     "  parchanged=",ctrl$parchanged,"\n")
  cat("reltest=",ctrl$reltest,"  save.failures=",ctrl$save.failures,"  scaletol=",ctrl$scaletol,"\n")
  cat("stepdec=",ctrl$stepdec,"  steplen0=",ctrl$steplen0,"  stepmax=",ctrl$stepmax,
     "  stepmin=",ctrl$stepmin,"\n")
  cat("stepredn=",ctrl$stepredn,"  stopbadupdate=",ctrl$stopbadupdate,"  tol=",ctrl$tol,"\n")
  cat("trace=",ctrl$trace,"  watch=",ctrl$watch,"\n")  
  cat("=========================================\n")  
}

