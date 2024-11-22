# CHECK FOR ERRORS IN SYNTAX
#------------------------------------------------------------------------------
merlin_error_check <- function(model,data,timevar,family,link,intmethod,
                               covariance,levels,sweights) {

  Nlevels = length(levels)
  Nmodels = length(family)

  # check specified familes
  #-------------------------
  if (length(model) != Nmodels) {
    stop("The number of families specified must equal the number of models",call. = FALSE)
  }
  for (f in 1:Nmodels) {
    if (!(family[f] == "gaussian" |  family[f] == "weibull" | family[f] == "exponential" |
          family[f] == "gompertz" | family[f] == "bernoulli" | family[f] == "rp" |
          family[f] == "user" | family[f] == "loghazard" | family[f] == "null" |
          family[f] == "poisson" | family[f] == "beta" | family[f] == "negbinomial")) {
      errorpaste <- paste0(family[f]," is not a valid family")
      stop(errorpaste,call. = FALSE)
    }
  }

  # check the ys
  #--------------
  y <- list()
  for (k in 1:length(model)) {
      modk <- strsplit(as.character(model[[k]]),split='~', fixed=TRUE)
      if (family[k] == "gaussian" | family[k] == "bernoulli"
          | family[k] == "poisson" | family[k] == "user"
          | family[k] == "beta" | family[k] == "negbinomial") {
        y[[k]] <- merlin_trim(unlist(modk)[2])
        if (!(y[[k]] %in% names(data))) stop(paste0("Outcome variable ",y[[k]]," is not present in data set"))
        if (!is.numeric(data[,y[[k]]])) stop("Outcome must be numeric")
        }
      else {  #survival outcomes
        y[[k]] <- c(as.character(model[[k]][[2]][[2]]),as.character(model[[k]][[2]][[3]]))
        if (!(y[[k]][1] %in% names(data))) stop(paste0("Outcome variable ",y[[k]][1]," is not present in data set"))
        if (!(y[[k]][2] %in% names(data))) stop(paste0("Outcome variable ",y[[k]][2]," is not present in data set"))
        if (!is.numeric(data[,y[[k]][1]])) stop("Survival times must be numeric")
        if (!is.numeric(data[,y[[k]][2]])) stop("Event indicator must be numeric")
        if (!(identical(as.integer(data[,y[[k]][2]]),as.integer(round(data[,y[[k]][2]]))) & max(data[,y[[k]][2]],na.rm=T)<2 & min(data[,y[[k]][2]],na.rm=T)>-1)) stop("Event indicator must be 0 or 1")
      }
  }

   # check random-effects
  #----------------------
  if (Nlevels > 0) {
    for (l in 1:Nlevels) {
      if (!(levels[l] %in% colnames(data)))
        stop("The specified levels must refer to a column in the dataset",call. = FALSE)
    }
  }

  # check integration methods
  #---------------------------
  Nim = length(intmethod)
  if (Nim>0) {
      for (i in 1:Nim) {
          if (intmethod[i]!="ghermite" & intmethod[i]!="halton" & intmethod[i]!="sobol"
              & intmethod[i]!="mc") {
            stop(paste0(intmethod[i]," is not a valid intmethod"),call. = FALSE)
          }
      }
      if (Nim!=1 & Nim!=Nlevels) {
        stop("Incorrect intmethod specification",call. = FALSE)
      }
  }

  # check sampling weights syntax
  #-------------------------------
  if (length(sweights) > 0) {
    for (w in 1:length(sweights)) {
      if (!(sweights[w] %in% colnames(data)))
        stop("The specified weights must refer to a column in the dataset",call. = FALSE)
    }
  }

  # check covariance structure
  #----------------------------
  if (!(covariance) %in% c("unstructured","diagonal","exchangeable","identity")) {
    stop(paste0('"',covariance,'"'," is not a valid covariance structure"),call. = FALSE)
  }

  # go through elements of linear predictor
  #-----------------------------------------
  timevar_needed = rep(0,length(model))
  # using same structure as merlin_setup_function (line 666)
  for (m in 1:length(model)) { # loop through models
    rhs <- merlin_setup_get_rhs(model[[m]])
    cmps <- merlin_trim(unlist(strsplit(rhs, split='+', fixed=TRUE)))
    for (c in 1:length(cmps)) {
      els <- unlist(strsplit(cmps,split=":",fixed=TRUE))
      for (e in 1:length(els)) {
        el <- unlist(strsplit(els[e],split="*",fixed=TRUE))[1]
        el <- merlin_trim(el)
        hassquareb	= grepl("[",el,fixed=T)
        hasroundb	  = grepl("(",el,fixed=T)

        if (grepl("EV",el,fixed=T)) { # EV elements
          timevar_needed[m] <- 1
          if (!hassquareb) stop("Using EV requires square brackets")
          evvar <- unlist(strsplit(unlist(strsplit(el,split="[",fixed=TRUE))[2],split="]",fixed=TRUE))[1]
          if (!evvar %in% unlist(y)) stop(paste0(evvar," is not a specified model outcome"))
          if (evvar %in% unlist(y[[m]])) stop(paste0("The expected value of an outcome variable cannot be used in the model for that outcome"))
        }

        if (grepl("M",el,fixed=T) & hassquareb) { # random-effects
          revar <- unlist(strsplit(unlist(strsplit(el,split="[",fixed=TRUE))[2],split="]",fixed=TRUE))[1]
          if (length(levels)==0) stop("If random-effects are specified the levels option must be used")
          if (!revar %in% levels) stop(paste0(revar," has not been included in the levels option"))
        }
      }
    }
  }

  # check timevar specification
  #-----------------------------
  # timevar - if only one is specified assume it's the same throughout
  Ntimevars = length(timevar)
  if (Ntimevars > 0) {
    if (Ntimevars < Nmodels) {
      if (Ntimevars != Nmodels)
        stop("Length of timevar must be 1 or equal to the number of models specified")
    }
  }
  if (sum(timevar_needed) > 0 & length(timevar) == 0) stop("timevar option must be specified for this model")
  for (tv in seq_along(timevar)) {
    if (!(timevar[tv] %in% colnames(data))) stop(paste0("timevar '", timevar[tv], "' is not a column of the input dataset"), call. = FALSE)
  }
}
