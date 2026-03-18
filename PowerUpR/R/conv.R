# define object conversion functions
mrss.to.mdes <- function(object) {

  if(any(c("med211", "med221", "med311", "med321", "med331", "med_pn21", "med_pn31", "med_pn32") %in% class(object))) {
    stop("Indirect effects are currently not supported in object conversion functions", call. = FALSE)
  }

  if(inherits(object, "mrss")) {
    idx.par <- match(c("n","J","K","L"), names(object$parms))
    nlevels <- sum(!is.na(idx.par)) + 1
    parms <- object$parms

    fun.parsed <- scan(text = object$fun, what = "character", sep=".", quiet = TRUE)
    fixed <- substr(fun.parsed[2], nchar(fun.parsed[2])-1, nchar(fun.parsed[2])-1)

    if(nlevels == 1) {
      parms$n <- object$n
    }else if(nlevels == 2) {
      ifelse(fixed == "f",
             parms$n <- object$n,
             parms$J <- object$J)
    }else if(nlevels == 3) {
      ifelse(fixed == "f",
             parms$J <- object$J,
             parms$K <- object$K)
    }else if(nlevels == 4) {
      ifelse(fixed == "f",
             parms$K <- object$K,
             parms$L <- object$L)
    }

    if(length(fun.parsed) == 2){
      fun <- paste0("mdes", ".", fun.parsed[2])
    }else{
      fun <- paste0("mdes", ".", fun.parsed[2], ".",  fun.parsed[3])
    }
    parms <- parms[intersect(names(parms), names(formals(fun)))]
    return(invisible(do.call(fun, parms)))
  } else {
    stop("'object' should have class 'mrss' and returned from MRSS functions", call.=FALSE)
  }
}


mrss.to.power <- function(object) {

  if(any(c("med211", "med221", "med311", "med321", "med331", "med_pn21", "med_pn31", "med_pn32") %in% class(object))) {
    stop("Indirect effects are currently not supported in object conversion functions", call. = FALSE)
  }

  fun.parsed <- scan(text = object$fun, what = "character", sep=".", quiet = TRUE)
  fixed <- substr(fun.parsed[2], nchar(fun.parsed[2])-1, nchar(fun.parsed[2])-1)

  if(inherits(object, "mrss")) {
    idx.par <- intersect(c("n","J","K","L"), names(object$parms))
    nlevels <- length(idx.par) + 1
    parms <- object$parms

    if(nlevels == 1) {
      parms$n <- object$n
    }else if(nlevels == 2) {
      ifelse(fixed == "f",
             parms$n <- object$n,
             parms$J <- object$J)
    }else if(nlevels == 3) {
      ifelse(fixed == "f",
             parms$J <- object$J,
             parms$K <- object$K)
    }else if(nlevels == 4) {
      ifelse(fixed == "f",
             parms$K <- object$K,
             parms$L <- object$L)
    }

    if(length(fun.parsed) == 2){
      fun <- paste0("power", ".", fun.parsed[2])
    }else{
      fun <- paste0("power", ".", fun.parsed[2], ".",  fun.parsed[3])
    }
    parms <- parms[intersect(names(parms), names(formals(fun)))]
    return(invisible(do.call(fun, parms)))
  } else {
    stop("'object' should have class 'mrss' and returned from MRSS functions", call.=FALSE)
  }
}


power.to.mdes <- function(object) {

  if(any(c("med211", "med221", "med311", "med321", "med331", "med_pn21", "med_pn31", "med_pn32") %in% class(object))) {
    stop("Indirect effects are currently not supported in object conversion functions", call. = FALSE)
  }

  if(inherits(object, "power")) {
    idx.par <- intersect(c("n","J","K","L"), names(object$parms))
    nlevels <- length(idx.par)
    parms <- object$parms
    parms$power <- object$power
    fun.parsed <- scan(text = object$fun, what = "character", sep=".", quiet = TRUE)
    if(length(fun.parsed) == 2){
      fun <- paste0("mdes", ".", fun.parsed[2])
    }else{
      fun <- paste0("mdes", ".", fun.parsed[2], ".",  fun.parsed[3])
    }
    parms <- parms[intersect(names(parms), names(formals(fun)))]
    return(invisible(do.call(fun, parms)))
  } else {
    stop("'object' should have class 'power' and returned from power functions", call.=FALSE)
  }
}

mdes.to.power <- function(object){

  if(any(c("med211", "med221", "med311", "med321", "med331", "med_pn21", "med_pn31", "med_pn32") %in% class(object))) {
    stop("Indirect effects are currently not supported in object conversion functions", call. = FALSE)
  }

  if(inherits(object, "mdes")) {
    idx.par <- intersect(c("n","J","K","L"), names(object$parms))
    nlevels <- length(idx.par)
    parms <- object$parms
    parms$es <- object$mdes[1]
    fun.parsed <- scan(text = object$fun, what = "character", sep=".", quiet = TRUE)
    if(length(fun.parsed) == 2){
      fun <- paste0("power", ".", fun.parsed[2])
    }else{
      fun <- paste0("power", ".", fun.parsed[2], ".",  fun.parsed[3])
    }
    parms <- parms[intersect(names(parms), names(formals(fun)))]
    return(invisible(do.call(fun, parms)))
  } else {
    stop("'object' should have class 'mdes' and returned from MDES functions", call.=FALSE)
  }
}

mdes.to.pctl <- function(object){

  if(any(c("med211", "med221", "med311", "med321", "med331", "med_pn21", "med_pn31", "med_pn32") %in% class(object))) {
    stop("Indirect effects are currently not supported in object conversion functions", call. = FALSE)
  }

  if(inherits(object, "mdes")){
    pctl <- pnorm(object$mdes) * 100
    mdes.pctl <- rbind(round(object$mdes,3),round(pctl,3))
    rownames(mdes.pctl) <- c("mdes","pctl")
    colnames(mdes.pctl) <- c(".", paste0(100 * (1-object$parms$alpha),"% lcl"), paste0(100 * (1-object$parms$alpha),"% ucl"))
    print(mdes.pctl)
    cat("----------------------------------- \n")
    cat("Note: An MDES of zero is equivalent to 50th percentile \n")
    return(invisible(mdes.pctl))
  }else if(is.numeric(object)){
    pctl <- pnorm(object) * 100
    mdes.pctl <- rbind(round(object,3),round(pctl,3))
    rownames(mdes.pctl) <- c("mdes","pctl")
    colnames(mdes.pctl) <- paste0(".", 1:length(object))
    print(mdes.pctl)
    cat("----------------------------------- \n")
    cat("Note: An MDES of zero is equivalent to 50th percentile \n")
    return(invisible(mdes.pctl))
  } else{
    stop("'object' should have class 'mdes' and returned from MDES functions", call.=FALSE)
  }
}
