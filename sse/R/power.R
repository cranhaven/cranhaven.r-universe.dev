### ------------------------------------------------------------------ CLASSES
### FIXME description in
###   - man/powPar-class.Rd,
###   - man/powCalc-class.Rd, and
###   - man/power-class.Rd
setClass("powPar",
         representation(list = "list",
                        theta = "numeric",
                        theta.name = "character",
                        theta.act = "numeric",
                        xi = "numeric",
                        xi.name = "character",
                        xi.act = "numeric",
                        n = "integer",
                        n.act = "integer"))
##
setClass("powEx",
         representation(theta.example = "numeric",
                        xi.example = "numeric",
                        endpoint.example = "character",
                        power.example = "numeric",
                        iter.example = "integer",
                        method = "character",
                        lm.range = "numeric",
                        drop = "numeric",
                        forceDivisor = "logical",
                        divisor = "integer"))
##
setClass("powFun",
         representation(statistic = "function",
                        return.power = "logical",
                        return.list = "logical",
                        return.n = "integer",
                        return.names = "character"))
##
setClass("powCalc",
         representation(core = "array",
                        size = "array",
                        iter = "integer",
                        cluster = "list"),
         contains = c("powFun", "powPar"))

##
setClass("SampleSize",
         representation(estimate = "integer"))

##
setClass("power",
         contains = c("powCalc", "powEx"),
         validity = function(object) {
           ## theta of powEx should be in the range of theta from powCalc
           if (object@theta.example > max(object@theta)
               | object@theta.example < min(object@theta)){
             stop(strwrap(prefix = " ", initial = "",
                          "argument 'theta' that is chosen for the example
                          'powEx()' must be within the range of evaluated values
                          definded in the object generated with 'powPar'"))
           }
           return(TRUE)
         }
         )


### -----------------------------------------------------FUNCTIONS / CONSTRUCTORS
### ---------------------------------
powPar <- function(n, theta = NA, xi = NA, ...) {
  ## -----
  ## testing and preparing the input we get
  ## - n: should be anything that can be converted to numeric
  ##          (logical is allowed but dangerous)
  ##      is not allowed to contain dublicates
  ## - theta: default NA is only for backward compatibility
  ##          (use of any name together with theta.name)
  ##          is not allowed to contain dublicates
  ## - xi: almost the same as for theta
  ## ---
  dots.eval <- list(...)
  ## ---
  ## handling of n
  if (length(unique(round(n, 10))) != length(round(n, 10))){
    stop(strwrap(
        "It is not allowed to have dublicated elements in 'n'",
        prefix = " ", initial = ""))
  }
  
  ## ---
  ## handling of theta
  ## FIXME: how to handle theta.name (if provided)? Add it to plot? etc.
  if (all(is.na(theta))) {
    if (any(names(dots.eval) == "theta.name")) {
      theta.name <- dots.eval[["theta.name"]]
      if (any(names(dots.eval) == theta.name)) {
        theta <- dots.eval[[theta.name]]
      } else {
        stop(strwrap(gettextf(
            "You said that the 'theta.name' is % s but there is no argument %s",
            dQuote(theta.name), dQuote(theta.name)),
                  prefix = " ", initial = ""))
      }
      dots.eval[["theta.name"]] <- NULL
    } else {
      stop(strwrap("If the argument 'theta' is NA, you should provide a vector
                   of values to be evaluated OR the argument theta.name with the
                   name of the argument that provides the vector of values.",
                  prefix = " ", initial = ""))
    }
  } else {
    if (any(names(dots.eval) == "theta.name")) {
      theta.name <- dots.eval[["theta.name"]]
      dots.eval[["theta.name"]] <- NULL
      warning(strwrap(
          "I am in trouble here: You provide a 'theta', as well as a
           'theta.name'.
           The elemets provided to 'theta' will be used for further
           calculations.",
          prefix = " ", initial = ""))
    } else {
      theta.name <- as.character(NA)
    }
  }
  if (length(unique(round(theta, 10))) != length(round(theta, 10))){
    stop(strwrap(
            "It is not allowed to have dublicated elements in 'theta'",
            prefix = " ", initial = ""))
    }

  ## ---
  ## handling of xi
  if (all(is.na(xi))) {
    if (any(names(dots.eval) == "xi.name")) {
      xi.name <- dots.eval[["xi.name"]]
      if (any(names(dots.eval) == xi.name)) {
        xi <- dots.eval[[xi.name]]
      } else {
        stop(strwrap(gettextf(
            "You said that the 'xi.name' is % s but there is no argument %s",
            dQuote(xi.name), dQuote(xi.name)),
            prefix = " ", initial = ""))
      }
      dots.eval[["xi.name"]] <- NULL
    } else {
      xi.name <- as.character(NA)
      xi <- NA
    }
  } else {
    if (any(names(dots.eval) == "xi.name")) {
      xi.name <- dots.eval[["xi.name"]]
      dots.eval[["xi.name"]] <- NULL
      warning(strwrap(
          "I am in trouble here: You provide a 'xi', as well as a 'xi.name'.
           The elemets provided to 'xi' will be used for further
           calculations.",
          prefix = " ", initial = ""))
    } else {
      xi.name <- as.character(NA)
    }
  }
  if (length(unique(round(xi, 10))) != length(round(xi, 10))){
    stop(strwrap(
        "It is not allowed to have dublicated elements in 'xi'",
        prefix = " ", initial = ""))
  }
  
  ## ---
  ## generate a new powPar object
  powPar <- new("powPar",
                list = dots.eval,
                theta = round(theta, 10), # there is for sure a nicer solution
                                          # than rounding here
                                          # ... the same is true for xi
                theta.name = theta.name,
                xi = round(xi, 10),
                xi.name = xi.name,
                n = as.integer(n),
                n.act = as.integer(NA),
                theta.act = as.numeric(NA),
                xi.act = as.numeric(NA))

  ## ---
  ## set the actual value to the first value in the sequence
  ## (because rounding is done in the previous step this step is separated)
  powPar@theta.act <- powPar@theta[1]
  powPar@xi.act <- powPar@xi[1]
  powPar@n.act <- powPar@n[1]

  ## ---
  return(powPar)
}








### --------------------------------------------------------------------- METHODS
### for users usage (and internal usage)

setMethod("powEx",
          signature = c(x = "powCalc"),
          definition = function(x,
                                theta,
                                xi = NA,
                                endpoint = NA,
                                power = 0.9,
                                drop = 0,
                                method = c("default", "lm", "step"),
                                lm.range = NA,
                                forceDivisor = FALSE) {
            ## -----
            ## This method contructs a power-object from a powCalc-object and
            ## a powEx-object.
            ## The powEx-object is not visible for the user (this is different
            ## from older


            ## -----
            ## generating an object of class powEx
            y <- construct.powEx(theta,
                                 xi,
                                 endpoint,
                                 power,
                                 drop,
                                 method,
                                 lm.range,
                                 forceDivisor)


            ## -----
            ## testing if powCalc and powEx fit together.
            ## ---
            ## if powCalc has no xi, powEx should neither
            if (all(is.na(x@xi)) & all(!is.na(y@xi.example))){
              warning(strwrap(
                  "powCalc-object does not make use of 'xi', but you provide
                  an example for 'xi'. The example for 'xi' will be ignored",
                  prefix = " ", initial = ""))
              y@xi.example <- as.numeric(NA)
            }
            ## ---
            ## do powCalc has the same endpoint names as powEx?
            if (!is.na(y@endpoint.example)
                && all(x@return.names != y@endpoint.example)) {
              stop(strwrap(gettextf(
                  "You have chosen an 'endpoint' for the example that is not
                   available, please select one of the following list: %s",
                  dQuote(x@return.names),
                  prefix = " ", initial = ""))
                , call. = FALSE)
            }
            ## ---
            ## if forceDivisor is TRUE we guess what a good divisor might be
            ## this needs to be done here because we need the objects
            ## powEx and powCalc
            if (forceDivisor && is.na(y@divisor)){
              divisor <- 1
              for ( i in 2:min(x@n) ) {
                divisor <- ifelse(all(x@n %% i == 0), i, divisor)
              }
              y@divisor <- as.integer(divisor)
              message(strwrap(gettextf(
                  "If 'forceDivisor' is set to TRUE, I guess on a good divisor
                   to be used: If %s is not a good choice, please indicate
                   what divisor to be used.",
                  dQuote(y@divisor),
                  prefix = " ", initial = "")))
            }
            ## -----
            ## generating a new power object
            return(new("power", x, y))
          })



setMethod("tex",
          signature(x = "power", type = "character"),
          definition = function(x,
                                type = c("drop", "nRec", "nEval", "sampling",
                                         "theta", "xi", "n.iter", "power"),
                                ...) {
            type <- match.arg(type)
            switch(type,
                   sampling = {
                     paste("$n_{i=1,...,",
                           length(x@n),
                           "} = ",
                           min(x@n),
                           ", ..., ",
                           max(x@n),
                           "$",
                           sep = "")
                   },
                   theta = {
                     x@theta.example
                   },
                   xi = {
                     x@xi.example
                   },
                   n.iter = {
                     ifelse(length(x@iter.example) == 0
                            || x@iter > x@iter.example,
                            x@iter,
                            x@iter.example)
                   },
                   power = {
                     x@power.example
                   },
                   drop = {
                     paste(round(100 * x@drop), "~\\\\%", sep = "")
                   },
                   nRec = {
                     ceiling(sampleSize(x)@estimate / (1 - x@drop))
                   },
                   nEval = {
                     ceiling(sampleSize(x)@estimate)
                   })
          })



setMethod("pp",
          signature(x = "powPar"),
          function(x, name) {
            ##
            if (name %in% slotNames("power")) {
              slot(x, name)
            } else {
              eval(x@list[[name]])
            }
          })



setMethod("n", signature(x = "powPar"), function(x){
  ##
  x@n.act
  ##
})



setMethod("theta", signature(x = "powPar"), function(x){
  ##
  x@theta.act
  ##
})



setMethod("xi", signature(x = "powPar"), function(x){
  ##
  x@xi.act
  ##
})



setMethod("dim", signature(x = "powPar"), function(x){
  c(n = length(x@n),
    theta = length(x@theta),
    xi = length(x@xi),
    endpoint = NA)
})



setMethod("dim", signature(x = "powCalc"), function(x){
  c(n = length(x@n),
    theta = length(x@theta),
    xi = length(x@xi),
    endpoint = length(x@return.names))
})



setMethod("plot",
          signature(x = "power", y = "missing"),
          definition = function(x, ...) {
            plot.power(x, ...)
            })

## ...FIXME
## not sure if in the future it would make sense to draw the calc-obj already
## Problems: - for which xi do we draw if there are several?
## technically it would meen: method exDat for powCalc-objects is needed 
## setMethod("plot",
##           signature(x = "powCalc", y = "missing"),
##           definition = function(x, ...) {
##             plot.power(x, example = FALSE, ...)
##             })


setMethod("inspect",
          signature = c(object = "power"),
          definition = function(object){
            invisible(sampleSize(x = object, inspect = TRUE))
          })


setMethod("refine",
          signature(object = "power"),
          definition = function(object, factor = 10){

            ## -----
            ## only useful if power object was generated with resampling
            if (object@return.power) {
              stop(strwrap(prefix = " ", initial = "",
                  "Additional iterations for the chosen example are only
                   meaningful if the object was created using resampling."))
            }


            ## -----
            ## prepare n.iter
            if (!is.numeric(factor) || is.na(factor) || factor <= 1) {
              stop(strwrap(
                  "The argument 'factor' must be >1
                  (is used for multiplying the number of iterations."),
                  prefix = " ", initial = "")
            }
            n.iter <- object@iter
            iter.example <- as.integer(n.iter * factor)


            ## -----
            if (is.na(object@xi.example)) {
              refinedObj <- workhorse(
                  object,
                  theta = object@theta == object@theta.example,
                  n.iter = iter.example)
            } else {
              refinedObj <- workhorse(
                  object,
                  theta = object@theta == object@theta.example,
                  xi = object@xi == object@xi.example,
                  n.iter = iter.example)
            }
            ## Because only the iterations of the example are increased
            ## during refinement this needs to be handled here correctly
            refinedObj@iter.example <- iter.example
            refinedObj@iter <- n.iter

            ## -----
            return(refinedObj)
          })

setMethod("update",
          signature(object = "powCalc"),
          definition = function(object, ...){
            ## -----
            ## This method evaluates parts or the whole core.
            ## Some changes ask for a complete new evaluation without keeping
            ## anything from the past.
            ## If several things are to be changed the sequence of the steps is
            ## very important:
            ## 1. first n with old n.iter
            ## 2. then theta with old n.iter
            ## 3. then xi with old n.iter
            ## 4. then all to new n.iter
            ## 5.
            ## - if any of the steps 1.-3. ask for complete new evaluation (sets
            ## new.calc to TRUE) then we skip to step 5.
            ## - if changes to n, theta, xi are provided but nothing needs to be
            ## done then any.change is kept to FALSE
            ##   step 4

            ## -----
            ## testing and preparing the input we get
            ## - object: not tested because it is in the signature
            ## - n, theta, xi: - only one at the time should be used!
            ##                 - all three must be a vector of logicals
            ##                 - indicating which elements of n, theta and xi
            ## should be evaluated
            ## - n.iter: - needs special attention because can be changed but
            ## there is no slot
            ##             the corresponding slot is called iter and holds
            ## something different
            ##           - is the total number of iterations that should be
            ## accieved AFTER running workhorse or NA
            ##           - must be integer of length 1

            dots <- list(...)
            dots.names <- names(dots)
            ## ## -----
            ## ## handle keep
            ## keep <- FALSE
            ## if (any(dots.names == "keep")) {
            ##   keep <- dots[["keep"]]
            ##   ## removing keep from dots and dots.names
            ##   dots <- dots[which(dots.names != "keep")]
            ##   dots.names <- dots.names[-which(dots.names == "keep")]
            ##   if (!is.logical(keep)) {
            ##     warning(strwrap(
            ##         "The argument 'keep' should be a logical,
            ##         it will be set to FALSE",
            ##         prefix = " ", initial = ""))
            ##     keep <- FALSE
            ##   }
            ## }

            # notAlloud <- c("core", "size", "iter")
            
            ## this list might be longer but other usages are not tested and
            ## documented!
            allowedSlots <- c("n", "theta", "xi", "statistic", "n.iter")
            if (any(!dots.names %in% allowedSlots)){
              stop(strwrap(gettextf(
                  "It is only allowed to update the following elements: %s .",
                  dQuote(paste(allowedSlots, collapse = ", "))),
                  prefix = " ", initial = ""))
            }


            ## -----
            ## preparing a new object containing all slots from the historic and
            ## all changes from the user
            ## the historic object will be used later again!
            newObj <- object
            for (i in seq_along(dots.names)) {
              if (.hasSlot(newObj, dots.names[i])) {
                if (dots.names[i] == "n") {
                  slot(newObj, dots.names[i]) <- as.integer(
                      round(dots[[i]], 10))
                  ##
                  if (length(unique(round(as.integer(dots[[i]]), 10)))
                      != length(round(as.integer(dots[[i]]), 10))){
                    stop(strwrap(
                        "It is not allowed to have dublicated elements in 'n'",
                        prefix = " ", initial = ""))
                  }
                } else if (dots.names[i] == "theta"){ ## could be done generic
                  slot(newObj, dots.names[i]) <- round(
                      as.numeric(dots[[i]]), 10)
                  ##
                  if (length(unique(round(dots[[i]], 10)))
                      != length(round(dots[[i]], 10))){
                    stop(strwrap(
                        "It is not allowed to have dublicated elements in
                        'theta'",
                        prefix = " ", initial = ""))
                  }
                } else if (dots.names[i] == "xi"){
                  slot(newObj, dots.names[i]) <- round(
                      as.numeric(dots[[i]]), 10)
                  ##
                  if (length(unique(round(dots[[i]], 10)))
                      != length(round(dots[[i]], 10))){
                    stop(strwrap(
                        "It is not allowed to have dublicated elements in 'xi'",
                        prefix = " ", initial = ""))
                  }
                } else {
                  slot(newObj, dots.names[i]) <- dots[[i]]
                }
              }
            }


            ## -----
            ## preparing n.iter to be used by the workhorse,
            ## see there what is expected:
            if (hasArg("n.iter")) {
              if (object@return.power) {
                warning(strwrap(
                    "The argument 'n.iter' is use only if the object was
                     created using resampling.
                     The provided value will not be used.",
                    prefix = " ", initial = ""))  
                n.iter <- as.integer(NA)
              } else {
                ## ans else statement is used here because only further
                ## warnings or errors should be given if n.iter is used
                n.iter <- dots[["n.iter"]]
                if (length(n.iter) > 1){
                  warning("Only the first element of 'n.iter' is used")
                  n.iter <- n.iter[1]
                }
                n.iter <- as.integer(n.iter)
                ## n.iter must be at least as in previous runs
                if (!is.na(n.iter) & n.iter < object@iter) {
                  stop(strwrap(gettextf(
                      "The number of iterations must be equal >= %s.",
                      (object@iter)),
                      prefix = " ", initial = ""),
                      call. = FALSE)
                }
              }
            } else {
              ## that does not meen that no iterations are done
              n.iter <- as.integer(NA)
            }


            ## -----
            ## a helper if any step switches this to TRUE then the workhorse
            ## should do a complete new evaluation
            new.calc <- FALSE
            any.change <- FALSE

### new statistic
            if ("statistic" %in% dots.names) {
              message(strwrap("A new 'statistic' is provided.
                              All calculations will be done",
                            prefix = " ", initial = ""))
              new.calc <- TRUE
              ## updates all elements that are in the class powFun in the
              ## existing newObj by running the new statistic
              as(newObj, "powFun") <- powFunGen(newObj, newObj@statistic)
              ## newObj@core <- array(NA,
              ##                     dim = c(dim(newObj)[c("n", "theta", "xi")],
              ##                             newObj@return.n))
              ## newObj <- workhorse(newObj,
              ##                     n.iter = n.iter)
### new n, theta and/or xi
            } else if (any(c("n", "theta", "xi") %in% dots.names)) {
              takeObj <- object # is always the actual one
              giveObj <- object
              increase.n <- FALSE
              increase.theta <- FALSE
              increase.xi <- FALSE
              ## new n
              if ("n" %in% dots.names) {
                message("A new <n> is provided.")
                ## we introduce s that takes now "n" (later "theta" and "xi")
                s <- "n"
                if (identical(slot(newObj, s), slot(object, s))) {
                  warning("But it is the same -> No changes will be done.")
                } else if (!any(slot(newObj, s) %in% slot(object, s))) {
                  message(strwrap("It is entirly different ->
                                   All evaluations will be done.",
                                  prefix = " ", initial = ""))
                  new.calc <- TRUE
                  any.change <- TRUE
                }else if (!new.calc
                          & any(!(slot(newObj, s) %in% slot(object, s)))) {
                  message(strwrap(
                      "It contains some available elements ->
                       Only the new elements will be evaluated.",
                      prefix = " ", initial = ""))
                  ## --- increase n
                  ## the increasing itself is posponed because first we
                  ## check if other dimensions can be reduced
                  increase.n <- TRUE
                } else if (!new.calc
                           & all(round(slot(newObj, s), 10)
                                 %in% round(slot(object, s), 10))
                           & length(slot(newObj, s) < length(
                                                          slot(object, s)))) {
                  message(strwrap(
                      "It contains only available elements ->
                       No new elements will be evaluated.",
                      prefix = " ", initial = ""))
                  ## --- shrink n
                  takeElements <-
                    round(slot(newObj, s), 10) %in% round(slot(object, s), 10)
                  giveElements <-
                    round(slot(object, s), 10) %in% round(slot(newObj, s), 10)
                  takeObj@core <- array(NA,
                                        dim = c(length(newObj@n),
                                                length(object@theta),
                                                length(object@xi),
                                                newObj@return.n))
                  slot(takeObj, s) <- slot(newObj, s)
                  takeObj@core[takeElements, , , ] <-
                    giveObj@core[giveElements, , , ]
                  giveObj <- takeObj
                  any.change <- TRUE
                }
              }
              ## new theta
              if (!new.calc & "theta" %in% dots.names) {
                message("A new <theta> is provided.")
                s <- "theta"
                if (identical(slot(newObj, s), slot(object, s))) {
                  warning("But it is the same -> No changes will be done.")
                } else if (!any(slot(newObj, s) %in% slot(object, s))) {
                  message(strwrap("It is entirly different ->
                                   All evaluations will be done.",
                                  prefix = " ", initial = ""))
                  new.calc <- TRUE
                  any.change <- TRUE
                } else if (!new.calc
                           & any(!(round(slot(newObj, s), 10)
                             %in% round(slot(object, s), 10)))) {
                  message(strwrap(
                      "It contains some available elements ->
                       Only the new elements will be evaluated.",
                      prefix = " ", initial = ""))
                  ## --- increase theta
                  increase.theta <- TRUE
                } else if (!new.calc
                           & all(round(slot(newObj, s), 10)
                                 %in% round(slot(object, s), 10))
                           & length(slot(newObj, s)
                                    < length(slot(object, s)))) {
                  message(strwrap(
                      "It contains only available elements ->
                       No new elements will be evaluated.",
                      prefix = " ", initial = ""))
                  ## --- shrink theta
                    takeElements <-
                    round(slot(newObj, s), 10) %in% round(slot(object, s), 10)
                  giveElements <-
                    round(slot(object, s), 10) %in% round(slot(newObj, s), 10)
                  takeObj@core <- array(NA,
                                        dim = c(length(takeObj@n),
                                                length(newObj@theta),
                                                length(object@xi),
                                                newObj@return.n))
                  slot(takeObj, s) <- slot(newObj, s)
                  takeObj@core[, takeElements, , ] <-
                    giveObj@core[, giveElements, , ]
                  giveObj <- takeObj
                  any.change <- TRUE
                }
              }
              ## new xi
              if (!new.calc & "xi" %in% dots.names) {
                message("A new <xi> is provided.")
                s <- "xi"
                if (identical(slot(newObj, s), slot(object, s))) {
                  warning("But it is the same -> No changes will be done.")
                } else if (!any(slot(newObj, s) %in% slot(object, s))) {
                  message(strwrap(
                      "It is entirly different ->
                       All evaluations will be done.",
                      prefix = " ", initial = ""))
                  new.calc <- TRUE
                  any.change <- TRUE
                } else if (!new.calc
                           & any(!(slot(newObj, s) %in% slot(object, s)))) {
                  message(strwrap(
                      "It contains some available elements ->
                       Only the new elements will be evaluated.",
                      prefix = " ", initial = ""))
                  increase.xi <- TRUE
                } else if (!new.calc
                           & all(round(slot(newObj, s), 10)
                                 %in% round(slot(object, s), 10))
                           & length(slot(newObj, s)
                                    < length(slot(object, s)))) {
                  message(strwrap(
                      "It contains only available elements ->
                       No new elements will be evaluated.",
                      prefix = " ", initial = ""))
                  ## --- shrink xi
                  takeElements <- (round(slot(newObj, s), 10)
                    %in% round(slot(object, s), 10))
                  giveElements <- (round(slot(object, s), 10)
                    %in% round(slot(newObj, s), 10))
                  takeObj@core <- array(NA,
                                        dim = c(length(takeObj@n),
                                                length(takeObj@theta),
                                                length(newObj@xi),
                                                newObj@return.n))
                  slot(takeObj, s) <- slot(newObj, s)
                  takeObj@core[, , takeElements, ] <-
                    giveObj@core[, , giveElements, ]
                  giveObj <- takeObj
                  any.change <- TRUE
                }
              }
              if (!new.calc & increase.n) {
                s <- "n"
                message("New elements for 'n' are evaluated.")
                ## generating a object and a vector for giving (existing
                ## elements) and taking (existing elements)
                ## |                          | example | length | class   |
                ## | old elements (e.g. of n) | xxxxx   |      5 |         |
                ## | new elements             | ___xxx  |      3 |         |
                ## in this example the new n does not have the first tree
                ## elements but one additional
                ## | giveElements             | FFFTT   |      5 | logical |
                ## | takeElements             |    TTF  |      3 | logical |
                ## we stepwise increase and therefore do not directly work on
                ## the newObj
                ## if theta of xi are available we will "rotate", takeObj
                ## will then become giveObj,
                ## and newObj will become takeObj again etc
                takeElements <-
                  round(slot(newObj, s), 10) %in% round(slot(object, s), 10)
                giveElements <-
                  round(slot(object, s), 10) %in% round(slot(newObj, s), 10)
                ##
                ## generating a core (with NA) of the right dimensions for the
                  ## new obj (new in this step only!)
                  takeObj@core <- array(NA,
                                        dim = c(length(newObj@n),
                                                length(takeObj@theta),
                                                length(takeObj@xi),
                                                newObj@return.n))
                  slot(takeObj, s) <- slot(newObj, s)
                  ## filling in the elements that we keep from the past
                  takeObj@core[takeElements, , , ] <-
                    giveObj@core[giveElements, , , ]
                  ##
                  takeObj <- workhorse(takeObj,
                                       ## those elements that we could take from
                                       ## the past do not need to be changed
                                       ## again:
                                       n = !takeElements)
                  giveObj <- takeObj
                  any.change <- TRUE
                }
                if (!new.calc & increase.theta) {
                  message("New elements for 'theta' are evaluated.")
                  s <- "theta"
                  takeElements <-
                    round(slot(newObj, s), 10) %in% round(slot(object, s), 10)
                  giveElements <-
                    round(slot(object, s), 10) %in% round(slot(newObj, s), 10)
                  ##
                  takeObj@core <- array(NA,
                                        ## ATTENTION not the same as in above
                                        dim = c(length(takeObj@n),
                                                length(newObj@theta),
                                                length(takeObj@xi),
                                                newObj@return.n))
                  slot(takeObj, s) <- slot(newObj, s)
                  takeObj@core[, takeElements, , ] <-
                    giveObj@core[, giveElements, , ]
                  ##
                  takeObj <- workhorse(takeObj,
                                       theta = !takeElements)
                  giveObj <- takeObj
                  any.change <- TRUE
                }
                if (!new.calc & increase.xi) {
                  message("New elements for 'xi' are evaluated.")
                  s <- "xi"
                  takeElements <-
                    round(slot(newObj, s), 10) %in% round(slot(object, s), 10)
                  giveElements <-
                    round(slot(object, s), 10) %in% round(slot(newObj, s), 10)
                  ##
                  takeObj@core <- array(NA,
                                        ## ATTENTION not the same as in "n"
                                        ## and "theta" above:
                                        dim = c(length(takeObj@n),
                                                length(takeObj@theta),
                                                length(newObj@xi),
                                                newObj@return.n))
                  slot(takeObj, s) <- slot(newObj, s)
                  takeObj@core[, , takeElements, ] <-
                    giveObj@core[, , giveElements, ]
                  ##
                  takeObj <- workhorse(takeObj,
                                       xi = !takeElements)
                  giveObj <- takeObj
                  any.change <- TRUE
                }
              
              
              
              ## ---
              ## Now that takeObj has grown across "n", "theta", and "xi" but 
              ## ONLY if it did not step over a new.calc "request" and
              ## ONLY if it did step over any change
              ## otherwise there is no takeObj and we do not want to
              if (!new.calc & any.change) {
                ## print("route A")
                newObj <- takeObj
              }
            }
            
### new calc
            ## it is possible to update "n", "theta", "xi" AND do a new.calc
            ## but if we do a new.calc
            ## we do not need to do it also for n.iter again.
            if (new.calc) {
              ## print("route C, complete new evaluations")
              ## empty the historic core and fill it again
              newObj@core <- array(NA,
                                   dim = c(dim(newObj)[c("n", "theta", "xi")],
                                           newObj@return.n))
              newObj <- workhorse(newObj,
                                  n.iter = n.iter)
  
### new n.iter
            } else if (!is.na(n.iter)) {
            ##  print("route D, increasing n.iter")
              newObj <- workhorse(newObj,
                                  n.iter = n.iter)
            }
            
            return(newObj)
          })



setMethod("powCalc",
          signature(object = "powPar"),
          definition = function(object,
                                statistic,
                                n.iter = NA,
                                cluster = FALSE) {


            ## -----
            ## testing and preparing the input we get
            ## - object: not tested because it is in the signature
            ## - statistic: tested by powFunGen()
            ## - n.iter
            ## - cluster
            powFun.info <- powFunGen(object, statistic)

            if (length(n.iter) > 1){
              warning("Only the first element of 'n.iter' is used")
              n.iter <- n.iter[1]
            }
            n.iter <- as.integer(n.iter)


            ## ----- FIXME
            ## handling the argument cluster
            ## FIXME zumbrunnt: Why not allow cluster computing if n.iter is
            ## not specified?
            if (powFun.info@return.power || (is.logical(cluster) && !cluster)){
              cl <- FALSE
            }
            ##
            if (!powFun.info@return.power
                && (is.integer(cluster) || is.numeric(cluster))) {
              if (length(cluster) > 1){
                warning("Only the first element of cluster will be used.")
                cl <- cluster[1]
              }else{
                cl <- cluster
              }
            }

            ## if a user specifies cluster = TRUE
            if (!powFun.info@return.power && is.logical(cluster) && cluster){
              cl <- 8
            }

            ## if 'cluster' is an object of class 'cluster'
            ## THIS does NOT WORK: FIXME
            if (!powFun.info@return.power && inherits(cluster, "cluster")) {
              cl <- TRUE
            }



            ## -----
            ## constructing an powCalc object with empty core, n.iter, and size
            size.array <- array(NA,
                                dim = c(dim(object)[c("n", "theta", "xi")],
                                        powFun.info@return.n))
            power.array <- array(NA,
                                 dim = c(dim(object)[c("n", "theta", "xi")],
                                         powFun.info@return.n))
            ##
            newCalc <- new("powCalc",
                           size = size.array,
                           core = power.array,
                           iter = as.integer(NA),
                           cluster = list("cluster" = cl), # FIXME
                           powFun.info,
                           object)


            ## -----
            ## populate the core of the new object
            filledCalc <- workhorse(newCalc, n.iter = n.iter)


            ## -----
            return(filledCalc)
          })




setMethod("show",
          signature(object = "power"),
          definition = function(object) {
            cat("*---objcet of class power------------------------*\n")

            ## parameters that were used for calcualaton
            if (length(object@list) == 0) {
              ## cat("No additional parameters defined.\n")
            }else{
              cat("Additional parameters defined:\n")
              for (i in names(object@list)) {
                if (is.na(object@theta.name)) {
                  cat(paste("   ", i ,# ": ", eval(pp(object, i)),
                            "\n", sep = ""))
                }
              }
            }
            cat("\n")
            ## power array
            cat("Range and dimensions of the power array:\n")
            cat(paste("       n:  (dim: ", dim(object)[1], ") from ",
                      range(object@n)[1], " to ", range(object@n)[2],
                      "\n", sep = ""))
            cat(paste("   theta:  (dim: ", dim(object)[2], ") from ",
                      range(object@theta)[1], " to ", range(object@theta)[2],
                      "\n", sep = ""))
            cat(paste("      xi:  (dim: ", dim(object)[3], ") from ",
                      range(object@xi)[1], " to ", range(object@xi)[2],
                      "\n", sep = ""))
            cat(paste("endpoint: ",
                      paste(object@return.names, collapse = ", ")))
            ## if (all(!is.na(object@xi))){
            ##   cat(paste("      xi: from ", range(object@xi)[1], " to ",
            ##             range(object@xi)[2], " (dim: ", dim(object)[3],
            ##             ")\n", sep = ""))
            ## }
            cat("\n")


            ## infos about calculations
            if (object@return.power) {
              cat("No iterations used.\n")
            } else {
              cat("Number of iterations:\n")
              cat(paste("  n.iter: ", object@iter, "\n", sep = ""))
              cat("\n")
            }

            
            ## range of power observed
            cat("Range of power observed:\n")
            cat(paste("     Min: ",
                      round(range(object@core, na.rm = TRUE)[1],2),
                      "\n     Max: ",
                      round(range(object@core, na.rm = TRUE)[2],2), "\n"))
            cat("\n")


            ## example
            cat("Example: Sample size evaluation for:\n")
            if ( !is.na(object@endpoint.example)) {
              cat(paste("        endpoint: ", object@endpoint.example,
                        "\n", sep = ""))
            }
              cat(paste("           theta: ", object@theta.example,
                        "\n", sep = ""))
              if ( !is.na(object@xi.example)) {
                  cat(paste("            xi: ", object@xi.example,
                            "\n", sep = ""))
              }
              cat(paste("           power: ", object@power.example,
                        "\n", sep = ""))
              cat(paste("   drop out rate: ", object@drop,
                        "\n", sep = ""))
            cat("*------------------------------------------------*\n")
          })







### ------------------------------------------------------------------ METHODS
### that are used only internally - therefore not exported


setMethod("powFunGen",
          signature(object = "powPar"),
          definition = function(object, statistic){
            ## -----
            ## This function is only used internally (therefore not exported)
            ## to generate objects of class powFun.


            ## -----
            ## testing and preparing the input we get
            ## - object: not tested because it is in the signature
            ## - statistic: tested by new("powFun"... if is class
            ##              function the rest is done here
            

            ## -----
            ## generating an empty object to be populated later
            x <- new("powFun",
                     statistic = statistic)
            

            ## -----
            ## calling the function to test the output, called return
            statistic.return <- statistic(object)
            
            
            ## -----
            ## testing the return
            ## - class - logical (in resampling use)
            ##         - atomic or list (in power use) this is tested by the
            ##           comparison that can only be done for this classes
            ##           by testing if between 0 and 1
            if (is.logical(statistic.return)
                | is.double(statistic.return)
                && (all(statistic.return <= 1)
                  & all(statistic.return >= 0))) {
              if (is.logical(statistic.return)) {
                x@return.power <- FALSE
              } else {
                x@return.power <- TRUE
              }
            } else { # everything else is not allowed
              stop(strwrap(
                  "The function statistic does not return a logical or the
                   power (which should always be numeric and between 0 and 1.",
                  prefix = " ", initial = ""))
            }


            ## -----
            ## preparing data and populating the powFun object
            ## FIXME: implementation of list as return!
            ## if ( is.list(statistic.return) ){
            ##   warning(strwrap(
            ##       "The implementation that allows the statistic to return a
            ##        list is under construction!",
            ##       prefix = " ", initial = "")) #FIXME
            ##   x@return.list <- TRUE
            ##   x@return.n <- length(statistic.return$power)
            ##   x@return.names <- names(statistic.return$power)
            ##   ## n.size <- length(statistic.return$size)
            ##   ## size.name <- names(statistic.return$size)
            ##   ## if (is.null(size.name)) {
            ##   ## size.name <- paste("n", seq(1, n.size), sep = "")}
            ##   ## ##
            ##   ## if (!is.logical(statistic.return$power)
            ##   ## & !powFun.info@return.power) {
            ##   ##   warning("The function statistic does not return a logical
            ##   ##            vector for the list element power. I assume it is
            ##   ##            the power and will ignore your n.iter.")
            ##   ##   n.iter <- as.integer(NA)
            ##   ## }
            ##   ## ##
            ##   ## if (powFun.info@return.power){
            ##   ##   size.array <- array(NA, dim = c(dim(object), n.size))
            ##   ## } else {
            ##   ##   size.array <- array(NA,
            ##                            dim = c(dim(object), n.size, n.iter))
            ##   ## }
 ##           } else {
              x@return.list <- FALSE
              x@return.n <- length(statistic.return)
              if (is.null(names(statistic.return))) {
                ## we give a name to the endpoints if not provided
                x@return.names <- paste("ep", seq(1, x@return.n), sep = "")
              } else {
                x@return.names <- names(statistic.return)
              }
 ##           }
            

### -----
            return(x)
          })



setMethod("workhorse",
          signature(object = "powCalc"),
          definition = function(object,
                                n = NULL,
                                theta = NULL,
                                xi = NULL,
                                n.iter = NA){
            ## -----
            ## This method is only used internally to populate the core and
            ## n.iter of powCalc objects


            ## -----
            ## no test! expecting the correct input !
            ## - object: not tested because it is in the signature
            ## - n, theta, xi: - only one at the time should be used!
            ##                 - all three must be a vector of logicals
            ##                 - indicating which elements of n, theta and xi
            ##                   should be evaluated
            ## - n.iter: - is the total number of iterations that should be
            ##             acchieved AFTER running workhorse
            ##           - must be integer of length 1



            ## -----
            ## preparing input
            ## same three lines of code as in update function
            if (is.null(n)) {
              n <- rep(TRUE, length(object@n))
            }
            if (is.null(theta)) {
              theta <- rep(TRUE, length(object@theta))
            }
            if (is.null(xi)) {
              xi <- rep(TRUE, length(object@xi))
            }


            ## -----
            ## preparing n.iter and relatives
            ## ... is done at this stage (and not in preparation, because
            ## available.iter is needed here)
            ## before this preparation:
            ##    n.iter: as provided to workhorse
            ##    object@iter: number of iterations in previous runs
            ##    available.iter: not existing
            if (!object@return.power) {
              if (!is.na(n.iter)) {
                n.iter <- as.integer(n.iter)
                if ( !is.na(object@iter)) {
                  ## this is the case if object is updated only
                  ## how many iterations were done already:
                  available.iter <- object@iter
                   ## how many iterations are we doing in this run:
                  n.iter <- n.iter - available.iter
                   ## how many after this run:
                  object@iter <- as.integer(available.iter + n.iter)
                } else {
                  ## this is the case for new objects only
                  available.iter <- as.integer(0)
                  object@iter <- n.iter
                }
              } else {
                ## n.iter is not provided for this run but available in object
                ## this is the case for an update where n.iter is not changed
                ## in this case we use the same n.iter for the new objcects
                if ( !is.na(object@iter)) {
                  n.iter <- object@iter
                  available.iter <- as.integer(0)
                }
              }
            }
            ## after this preparation:
            ##    n.iter: the number of iterations for this run!
            ##    object@iter: how many iterations will be available after this
            ##                 run
            ##    available.iter: number of iterations available that are
            ##                    extended to n.iter


            ## -----
            ## the preparation is done in powCalc
            ## only using cluster if several n are needed
            with.cluster <- FALSE
            if ( sum(n) > 1 ){
              if ( is.numeric(object@cluster[[1]])
                  || (is.logical(object@cluster[[1]]) && object@cluster[[1]])) {
                cl <- makeCluster(detectCores())
                clusterEvalQ(cl, {
                  library(parallel)
                  library(sse)
                })
                with.cluster <- TRUE
              }
            }
            ## -----
            ## some extractions for simpler use (does this speed up?)
            power.array <- object@core
            statistic <- object@statistic

            ## -----
            ##
            ## FIXME size.array <-


            ## -----
            ##            with.cluster <- TRUE # FIXME

            
### --- cluster
### --- resampling (automatically if with.cluster)
            if (with.cluster) {
              message(paste("using cluster"))
              
              ## -----
              ## this step is not needed but speeds up a lot compared to
              ## applying the parSapply directly
              power.fun.resample <- function(statistic, object, n.iter){
                sig <- replicate(n.iter,
                                 statistic(object)
                                 )
                if (is.vector(sig)) {
                  ## this is the case if there is only one endpoint
                  sig <- matrix(sig,
                                ncol = length(sig),
                                dimnames = list(names(sig[1])))
                }
                return(apply(sig, 1, function(x){
                  sum(x, na.rm = TRUE) / length(x[!is.na(x)])
                }))
              }

              ## pos. :integer used for indexing within the core array
              pos.n <- seq(along = object@n)
              pos.theta <- seq(along = object@theta)
              pos.xi <- seq(along = object@xi)
              ## taking into account n, theta and xi we loop through all
              ## positions in the array that should be evaluated in this run
              for (theta.i in pos.theta[theta]) {
                for (xi.i in pos.xi[xi]) {
                  object@theta.act <- object@theta[theta.i]
                  object@xi.act <- object@xi[xi.i]
                  objects <- list()
                  i <- 1
                  for (n.i in pos.n[n]) {
                    object@n.act <- object@n[n.i]
                    objects[[i]] <- object
                    i <- i + 1
                  }
                  power.array[pos.n[n], pos.theta[theta.i], pos.xi[xi.i], ] <-
                    t(parSapply(cl,
                                objects,
                                function(x){
                                  power.fun.resample(statistic, x, n.iter)
                                }))
                  size.array <- array(NA)
                  message(gettextf("theta: %s %s",
                                   object@theta.act, Sys.time()))
                }
              }
              if (!all(is.na(object@core)) & available.iter != 0 ) {
                ## if iterations available from older runs they are use
                ## ?? is this loop still needed, when?
                for (ep in object@return.n) {
                  power.array[, , , ep] <-
                    (object@core[, , , ep, drop = FALSE]
                      * available.iter
                      + power.array[, , , ep, drop = FALSE] * n.iter) /
                    object@iter
                } 
              }
              stopCluster(cl)
            } else { # with.cluster
### --- no-cluster
              message(paste("not using cluster"))


### --- simple
            if (!object@return.list) {


### --- resampling
              if ( !object@return.power ){
                ## pos. :integer used for indexing within the core array
                pos.n <- seq(along = object@n)
                pos.theta <- seq(along = object@theta)
                pos.xi <- seq(along = object@xi)
                ## taking into account n, theta and xi we loop through all
                ## positions in the array
                ## that should be evaluated in this run
                for (n.i in pos.n[n]){
                  for (theta.i in pos.theta[theta]){
                    for (xi.i in pos.xi[xi]){
                      object@n.act <- object@n[n.i]
                      object@theta.act <- object@theta[theta.i]
                      object@xi.act <- object@xi[xi.i]
                      stat.eval <- vapply(X = seq(length.out = n.iter),
                                          FUN = function(x) {
                                            statistic(object)
                                          },
                                          FUN.VALUE = logical(object@return.n))
                      if (object@return.n == 1) {
                        power.array[pos.n[n.i],
                                    pos.theta[theta.i],
                                    pos.xi[xi.i], ] <-
                          sum(stat.eval, na.rm = TRUE) /
                          length(stat.eval[!is.na(stat.eval)])
                      } else {
                        power.array[pos.n[n.i],
                                    pos.theta[theta.i],
                                    pos.xi[xi.i], ] <-
                          apply(stat.eval, 1, function(x){
                            sum(x, na.rm = TRUE) / length(x[!is.na(x)])
                          })
                      }
                      size.array <- array(NA) ## FIXME
                    } # xi.loop
                  } # theta.loop
                  message(paste("n:", object@n.act, Sys.time()))
                } # n.loop
                if (!all(is.na(object@core)) & available.iter != 0 ) {
                  ## if iterations available from older runs they are use
                  ## ?? is this loop still needed, when?
                  for (ep in object@return.n) {
                    power.array[, , , ep] <-
                      (object@core[, , , ep, drop = FALSE]
                        * available.iter
                        +  power.array[, , , ep, drop = FALSE] * n.iter) /
                      object@iter
                  }
                }
                
### --- power
              } else {
                pos.n <- seq(along = object@n)
                pos.theta <- seq(along = object@theta)
                pos.xi <- seq(along = object@xi)
                for (n.i in pos.n[n]){
                  for (theta.i in pos.theta){
                    for (xi.i in pos.xi){
                      object@n.act <- object@n[n.i]
                      object@theta.act <- object@theta[theta.i]
                      object@xi.act <- object@xi[xi.i]
                      ##
                      ## for debugging it may help to change to:
                      ## stat.eval <<- statistic(object)
                      stat.eval <- statistic(object)
                      power.array[pos.n[n.i],
                                  pos.theta[theta.i],
                                  pos.xi[xi.i], ] <- stat.eval
                      size.array <- array(NA) ## FIXME
                    } # xi.loop
                  } # theta.loop
                  message(paste("n:", object@n.act, Sys.time()))
                } # n.loop
              }
            } # return.list
            } # with.cluster


            ## -----
            ## populate core and return object
            object@core <- power.array
            return(object)
          })

setMethod("exDat",
          signature = c(x = "power"),
          definition = function(x, y, ...){

            ## determin the index for xi
            xi.example <- x@xi.example
            if (is.na(xi.example)) {
              xi.example.integer <- 1
            } else {
              xi.example.integer <- which(xi.example == x@xi)
            }

            ## determin the index for endpoint
            ## - existence of endpoint.example in enpoint.names
            ##   is done by powEx() earlier
            endpoint.example <- x@endpoint.example
            if (is.na(endpoint.example)) {
              endpoint.example.integer <-  1
            } else {
              endpoint.example.integer <-
                which(endpoint.example == x@return.names)
            }
            
            ## taking the subset of core
            dat <- data.frame(sample.size = rep(pp(x, "n"), times = dim(x)[2]),
                              theta = rep(pp(x, "theta"), each = dim(x)[1]),
                              power = c(x@core[,
                                              ,
                                               xi.example.integer,
                                               endpoint.example.integer]))
            return(dat)
          })


### ---------------------------------
setMethod("sampleSize",
          signature = c(x = "power"),
          definition = function(x, inspect = FALSE, ...){

            ## -----
            ## extracting slots and making the elements easily available
            power.example <- x@power.example
            theta.example <- x@theta.example
            method <- x@method
            lm.range <- x@lm.range
            n.vec <- x@n
            return.power <- x@return.power
            forceDivisor <- x@forceDivisor
            divisor <- x@divisor


            ## -----
            ## set the default method
            if (method == "default") {
              if (return.power) {
                method <- "step"
              } else {
                method <- "lm"
              }
            }


            ## -----
            ## extracting the data from object of class power and
            ## providing a data.set with
            ## - sample.size
            ## - power
            ## - theta
            dat <- exDat(x)


            ## -----
            ## testing if parts from class powEx and powCalc (that were merged
            ## to class power) fit together.
            ## ?? should this be moved to example()?
            if (max(dat$power, na.rm = TRUE) < min(power.example, na.rm = TRUE)
                | min(dat$power,
                      na.rm = TRUE) > max(power.example,
                      na.rm = TRUE)) {
              stop(strwrap(gettextf(
                  "The power of the example is outside of the observed range
                   for the whole parameter space. The observed range is:
                   %s to %s.
                   There will be no example",
                  round(min(dat$power, na.rm = TRUE), 2),
                  round(max(dat$power, na.rm = TRUE), 2)),
                  prefix = " ", initial = ""),
                  call. = FALSE)
            }



            ## -----
            ## generating subset for the example
            dat.example <- dat[dat$theta == theta.example
                               & dat$power > 0
                               & dat$power < 1, ]

            ##
            if (nrow(dat.example) == 0) {
              stop(strwrap(
                  "The observed power of the example does not
                   include values > 0 or < 1.",
                  prefix = " ", initial = ""),
                  call. = FALSE)
              }
            
            ##
            if (max(dat.example$power, na.rm = TRUE)
                == min(power.example, na.rm = TRUE)) {
              stop(strwrap(gettextf(
                  "The observed power of the example is constant.
                   The value is: %s.
                   It is not meaningful do further calculations.",
                  min(dat.example$power),
                  prefix = " ", initial = "")),
                  call. = FALSE)
              }
            
            ##
            if (max(dat.example$power, na.rm = TRUE)
                < min(power.example, na.rm = TRUE)
                | min(dat.example$power, na.rm = TRUE)
                > max(power.example, na.rm = TRUE)) {
              warning(strwrap(gettextf(
                  "The power of the example is outside of the observed range.
                   The observed range is: %s to %s. There will be no example.",
                  round(min(dat.example$power, na.rm = TRUE), 2),
                  round(max(dat.example$power, na.rm = TRUE), 2),
                  prefix = " ", initial = "")),
                  call. = FALSE)
              ## by setting the method to na sample size is not estimated but
              ## set to NA:
              method <- "na" 
            }


            ## -----
            transform <- function(x) {
              ##older versions: 0.5 * log((1 + x) / (1 - x))
              (qnorm(x) + qnorm(1 - 0.05)) ^ 2 
            }
#            untransform <- function(y) (exp(2 * y) -1)/(1 + exp(2 * y))
                                        # FIXME look for backtransformation


            ## -----
            m.lm <- lm(sample.size ~ transform(power), data = dat.example)
            ##
### it is a problem if all data available is used for calculating the sample
### size (trade-off, taking only neighbours or taking all...)
            switch(method,
                   ## linear model
                   "lm" = {
                     power.borders <- (1 + c(-1, 1) * lm.range) * power.example
                     ## to allow transform transformation on power.borders:
                     power.borders[power.borders > 1] <- 0.99
                     power.borders[power.borders < 0] <- 0
                     ## a vector of length to indicating the border for the
                     ## power range used for fitting a linear model and
                     ## estimating the sample size
                     dat.example.range <-
                       dat.example[dat.example$power > power.borders[1] &
                                   dat.example$power < power.borders[2], ]
                     ##
                     m.lm.range <-
                       lm(sample.size ~ transform(power),
                          data = dat.example.range)
                     p.lm <- predict(m.lm.range,
                                     interval = "confidence",
                                     newdata = data.frame(
                                         power = c(power.example,
                                                   power.borders)))
                     sample.size <- ceiling(p.lm[1, 1])
                     message(strwrap(gettextf(
                         "estimator: %s \n95-CI: [%s]\n",
                         sample.size,
                         paste(round(p.lm[1, c("lwr", "upr")]),
                               collapse = "; ")),
                         prefix = " ", initial = ""))
                     ## forceDivisor handling
                     if (forceDivisor) {
                       if (sample.size %% divisor) {
                         sample.size <- sample.size +
                           (divisor - sample.size %% divisor)
                         message(strwrap(gettextf(
                             "Returning %s instead of the estimator to achieve
                              a divisibility with the divisor %s.",
                             sample.size,
                             divisor),
                             prefix = " ", initial = ""))
                       }
                     }
                     mypanel <- function(x, y, ...) {
                       panel.xyplot(x, y, col = "blue", ...)
                       suppressWarnings(panel.loess(x,
                                                    y,
                                                    span = 0.75,
                                                    degree = 2,
                                                    family = "gaussian",
                                                    col = "blue",
                                                    ...))
                       ## panel.abline(m.lm.range$coef, col = "red")
                       panel.lines(x = c(transform(power.borders)),
                                   y = p.lm[2:3, 1],
                                   col = "red",
                                   lty = 1,
                                   lwd = 1.5)
                       panel.abline(m.lm$coef, col = "blue", lty = 2)
                       ## showing the chosen power
                       panel.text(x = min(transform(power.example)),
                                  y = min(dat.example$sample.size),
                                  labels = paste(" power = ",
                                                 round(power.example, 2),
                                                 sep = ""),
                                  adj = c(-0.05, -0.05),
                                  srt = 90,
                                  col = "grey")
                       panel.abline(v = transform(power.example), col = "gray")
                       ## showing the chosen sample size
                       panel.abline(h = sample.size, col = "gray")
                       panel.text(x = min(transform(dat.example$power)),
                                  y = sample.size,
                                  labels = paste(" sample size = ",
                                                 sample.size, sep = ""),
                                  adj = c(-0.05, -0.05),
                                  col = "grey")
                       ## showing the method
                       panel.text(x = min(transform(dat.example$power)),
                                  y = max(y),
                                  labels = paste(" method: ", method,
                                                 " (lm.range = ", lm.range,
                                                 ")", sep = ""),
                                  adj = c(-0.05, 1.1),
                                  col = "grey")
                     }
                   },
                   "step" = {
                     element <- tail(which(dat.example$power < power.example),
                                     1) + 1
                     if (element > nrow(dat.example)) {
                         stop(strwrap(
                              "The observed power for the example is to small
                               for the step method.",
                             prefix = " ", initial = ""),
                             call. = FALSE)
                         }
                     
                     sample.size <- dat.example$sample.size[element]
                     ## for the inspection plot we need a line (based on
                     ## all data)
                     m.lm <- lm(sample.size ~ transform(power),
                                data = dat.example)
                     ## forceDivisor handling
                     if (forceDivisor) {
                       if (sample.size %% divisor) {
                         sample.size <- sample.size +
                           (divisor - sample.size %% divisor)
                         message(strwrap(gettextf(
                             "Returning %s instead of the estimator to achieve
                              a divisibility with the divisor %s.",
                             sample.size,
                             divisor),
                             prefix = " ", initial = ""))
                       }
                     }
                     mypanel <- function(x, y, ...) {
                       panel.xyplot(x, y, col = "blue", ...)
                       ## suppressWarnings because this is for visualisation
                       ## only.
                       suppressWarnings(panel.loess(x,
                                                    y,
                                                    span = 0.75,
                                                    degree = 2,
                                                    family = "gaussian",
                                                    col = "blue",
                                                    ...))
                       panel.abline(m.lm$coef, col = "blue", lty = 2)
                       panel.abline(v = transform(power.example), col = "gray")
#                      grid.text(label = paste("power = ",
#                                round(power.example, 2), sep = ""),
#                       x = transform(power.example),
#                       y = unit(0.15, "npc"), just = c(-0.01, 0.5))
                       ## showing the chosen power
                       panel.text(x = min(transform(power.example)),
                                  y = min(dat.example$sample.size),
                                  labels = paste(" power: ",
                                                 round(power.example, 2),
                                                 sep = ""),
                                  adj = c(-0.05, -0.05),
                                  srt = 90,
                                  col = "grey")
                       panel.points(x = transform(dat.example$power[element]),
                                    y = dat.example$sample.size[element],
                                    col = "red",
                                    pch = "*",
                                    cex = 4)
                       ## showing the chosen sample size
                       panel.abline(h = sample.size, col = "gray")
                       panel.text(x = min(transform(dat.example$power)),
                                  y = sample.size,
                                  labels = paste(" sample size: ",
                                                 sample.size,
                                                 sep = ""),
                                  adj = c(-0.05, -0.05),
                                  col = "grey")
                       ## showing the method
                       panel.text(x = min(transform(dat.example$power)),
                                  y = max(y),
                                  labels = paste(" method: ",
                                                 method,
                                                 sep = ""),
                                  adj = c(-0.05, 1.1),
                                  col = "grey")
                     }
                   },
                   "na" = {
                     sample.size <- NA
                      mypanel <- function(x, y, ...) {
                       panel.xyplot(x, y, col = "blue", ...)
                     }
                   }
                   )
### loess
###############
### this part is not used and only here fore historical reasons
            ## if (type == "loess"){
            ##   span = 0.05
            ##   m.loess <- loess(sample.size ~ transform(power), span = span,
            ##                    data = dat.example)
            ##   p.loess <- predict(m.loess,
          ##                      newdata = data.frame(power = power.example))
            ##   sample.size <- ceiling(p.loess)
            ##   cat(paste("estimator: ", sample.size,  "\n"))
            ## }
            if (inspect) {
              print(xyplot(sample.size ~ transform(power), data = dat.example,
                           xlab = "power (transformed)",
                           ylab = "sample size",
                           panel = mypanel
                           ))
            }
            return(new("SampleSize", estimate = as.integer(sample.size)))
          })




### ------------------------------------------------------------------ FUNCTIONS
### only for internal usage - not exported

### ---------------------------------
construct.powEx <- function(theta,
                            xi = NA,
                            endpoint = NA,
                            power = 0.9,
                            drop = 0,
                            method = c("default", "lm", "step"),
                            lm.range = NA,
                            forceDivisor = FALSE) {
  ## -----
  ## This function generats an object of class powEx.


  ## -----
  ## testing and preparing the input we get
  ## - theta: is not tested
  ## - xi:
  ## - endpoint:
  ## - power:
  ## - drop:
  ## - method: not tested but handled by match.arg (other tests depend on
  ##           method -> handled first)
  ## - lm.range:
  ## - forceDivisor:


  ## --- method
  method <- match.arg(method)
  ## --- theta
  if (length(theta) > 1){
    warning(strwrap(
        "The argument 'theta' should have a length of one.
         Only the first element is used",
        prefix = " ", initial = ""))
    theta <- theta[1]
  }
  ## --- xi
  if (length(xi) > 1){
    warning(strwrap(
        "The argument 'xi' should have a length of one.
         Only the first element is used",
        prefix = " ", initial = ""))
    xi <- xi[1]
  }
  ## --- endpoint
  if (length(endpoint) > 1){
    warning(strwrap(
        "The argument 'endpoint' should have a length of one.
         Only the first element is used",
        prefix = " ", initial = ""))
    endpoint <- endpoint[1]
  }
  ## --- power
  if (length(power) > 1){
    warning(strwrap(
        "The argument 'power' should have a length of one.
         Only the first element is used",
        prefix = " ", initial = ""))
    power <- power[1]
  }
  if (power <= 0 | power >= 1){
    stop("The argument 'power' should be between 0 and 1")
  }
  ## --- drop
  if (length(drop) > 1){
    warning(strwrap(
        "The argument 'drop' should have a length of one.
         Only the first element is used",
        prefix = " ", initial = ""))
    drop <- drop[1]
  }
  if (drop < 0 | drop >= 1){
    stop("The argument 'drop' should be between 0 and 1")
  }
  ## --- lm.range
  if (method == "step"){
    if (!is.na(lm.range)){
      warning(strwrap(
          "If the method 'step' is selecte, the argument 'lm.range'
           will not be use. ",
          prefix = " ", initial = ""))
    }
    lm.range <- as.numeric(NA)
  } else {
    ## for method "default" or "lm" we set the lm.range to 0.2 or if provided
    ## test it.
    if (length(lm.range) > 1) {
        warning(strwrap(
            "The argument 'lm.range' should have a length of one.
             Only the first element is used",
            prefix = " ", initial = ""))
        lm.range <- lm.range[1]
    }
    if (is.na(lm.range)) {
      lm.range <- 0.2
    } 
    if (lm.range < 0 | lm.range > 1){
      stop("The argument 'lm.range' should be in the range [0; 1]")
    }
  }
  ## --- forceDivisor
  ## handle forceDivisor and generate divisor
  divisor <- as.integer(NA)
  if (!is.logical(forceDivisor)) {
    ## if forceDivisor is numeric we set forceDivisor to TRUE
    if (is.numeric(forceDivisor) && length(forceDivisor) == 1) {
      if (forceDivisor <= 0) {
        stop(strwrap("If a divisor should be used, 
                     the argument 'forceDivisor' should get a positive number
                     or should be TRUE (which uses the default divisor of 2).",
                     prefix = " ", initial = ""))
      }
      divisor <- as.integer(forceDivisor)
      if (forceDivisor != divisor) {
        warning(gettextf(
            "argument 'forceDivisor' got %s but %s (integer) will be used",
            dQuote(forceDivisor), dQuote(divisor)))
      }
      forceDivisor <- TRUE
    } else {
      stop("The arguemnt 'forceDivisor' should get an integer or logical.")
    }
  }


  ## -----
  ## populating a new powEx object
  new("powEx",
      theta.example = theta,
      xi.example = as.numeric(xi),
      endpoint.example = as.character(endpoint),
      power.example = power,
      drop = drop,
      method = method,
      lm.range = lm.range,
      forceDivisor = forceDivisor,
      divisor = divisor)
}



### --------------------------------- FIXME (no changes done when 
plot.power <- function(x,
                       at = c(0.8, 0.85, 0.9, 0.95),
                       smooth = FALSE,
                       example = TRUE,
                       label.pos = 0.75,  ## the argument label.pos is
                                          ## not used yet
                       reflines = TRUE,
                       symbol = bquote(theta),
                       ...){
  object <-  x

  ## -----
  ## some tests
  ## only plot if dimenstion are ok
  if (any(dim(object)[1:2] < 2)) {
    ## this allows the programmer to see the estimated sample size
    ## sample.size <- sampleSize(object) 
    stop(strwrap(
        "length of 'theta' and 'n' (defined in the powPar-object)
         must be >= 2"),
         call. = FALSE)
  }


  ##
  dat <- exDat(object)


  # ---
  ## handling the smooth-argument
  if (length(smooth) > 1) {
    warning("Only the first element of <smooth> is used.")
    smooth <- smooth[1]
  }
  if (is.logical(smooth) & smooth) {
    span <- 0.75
    dat[!is.na(dat$power) & dat$power > 0 & dat$power < 1, "power"] <- fitted(
        loess(power ~ theta + sample.size,
              data = dat[!is.na(dat$power) & dat$power > 0 & dat$power < 1, ],
              span = span))
  }
  if (is.numeric(smooth)) {
    if (smooth <= 0) {
      stop(strwrap(
          "The argument 'smooth' has to be > 0,
           have a look at the argument 'span' in ?loess"))
    }
    span <- smooth
    transform <- function(x) {
      (qnorm(x) + qnorm(1 - 0.05)) ^ 2
    }
    untransform <- function(y) {
      pnorm(sqrt(y) - qnorm(1 - 0.05))
    } #(exp(2 * y) -1)/(1 + exp(2 * y))
    fitted.transform <- fitted(loess(transform(power) ~ theta + sample.size,
                                  data = dat[!is.na(dat$power)
                                             & dat$power > 0
                                             & dat$power < 1, ],
                                  span = span))
    dat[!is.na(dat$power) & dat$power > 0 & dat$power < 1, "power"] <-
      untransform(fitted.transform)
  }


  ## ---
  ## handling the example
  if (example) {
    sample.size <- sampleSize(object)@estimate
    if (is.na(sample.size)) {
      ## this is TRUE if the range of power for the theta.example does not
      ## include power.example
      example <- FALSE
    }
    theta.example <- object@theta.example
  }


  ## ---
  ## handling the at-argument
  if (!is.numeric(at)) {
    stop("The argument 'at' has to be numeric")
  }
  if (any(at <= 0) | any(at >= 1)) {
    stop("The argument 'at' has to be > 0 and < 1")
  }
  if (example){
    ## sorting at that the power.example is the first element
    at <- at[c(which(round(at, 10) == object@power.example),
               which(round(at, 10) != object@power.example))]
  }
  at.main <- c(at[1], at[1] * 1.00000000001)
  at.second <- at[-1]
  if (length(at.second == 1)) {
    at.second  <-  c(at.second, at.second * 1.0000000001)
  }
  rm(at)


  ## ---
  ## handling the xlim and ylim-argument
  ## (the default handling does not allow a nice control of the labeling and
  ## of the example)
  dots.eval <- list(...)
  if (any(names(dots.eval) == "xlim")) {
    xlim <- dots.eval[["xlim"]]
    dat <- dat[dat$theta >= xlim[1] & dat$theta <= xlim[2], ]
    if (example) {
      if (theta.example < xlim[1] | theta.example > xlim[2]) {
        warning(strwrap(
            "The example is outsite the ploting range and will not be used.",
            prefix = " ", initial = ""))
        example <- FALSE
      }
    }
  }
 if (any(names(dots.eval) == "ylim")) {
    ylim <- dots.eval[["ylim"]]
    dat <- dat[dat$sample.size >= ylim[1] & dat$sample.size <= ylim[2], ]
    if (example) {
      if (sample.size < ylim[1] | sample.size > ylim[2]) {
        warning(strwrap(
            "The example is outsite the range the ploting range and
             will not be used.",
            prefix = " ", initial = ""))
        example <- FALSE
      }
    }
  }



  ##
  ## xscale.components.theta <- function(lim, ...){
  ##     ans <- xscale.components.default(lim = lim, ...)
  ##     tick.at <- round(ans$bottom$ticks$at,10)
  ##     cat(tick.at)
  ##     cat("\n")
  ##     theta.tick.at <- round(object@theta,10)
  ##     cat(theta.tick.at)
  ##           cat("\n")
  ##     major <- theta.tick.at %in%  tick.at
  ##     cat(major)
  ##           cat("\n")
  ##     ans$bottom$ticks$at <- theta.tick.at
  ##     tck <- rep(0.75, length.out = length(theta.tick.at))
  ##     tck[major] <- 1.5
  ##     ans$bottom$ticks$tck <- tck# ifelse(major, 1.5, 0.75)
  ##     ans$bottom$labels$at <- tick.at
  ##     ans$bottom$labels$labels <- as.character(tick.at)
  ##     print(ans)
  ##     ans
  ## }

  ## -----
  ## creating the power plot
  cp <- contourplot(power ~ theta + sample.size,
                    data = dat,
                    ...,
                    ## xscale.components = xscale.components.theta,
                    ## scales = list(x = list(tick.number = 10)),
                    ##
                    panel = function(x, y, z, at, labels, ...){
                        ##
                        if (reflines){
                            panel.fill(col = grey(0.9))
                            ##
                            panel.refline(v = object@theta, col = grey(1))
                            panel.refline(h = object@n, col = grey(1))
                        }
                      ## drawing the thick line
                      panel.contourplot(x, y, z,
                                        at = at.main,
                                        cut = 2,
                                        lwd = 4,
                                        col = grey(0.6),
                                        label.style = "align",
                                        # perhaps for the next version instead
                                        # of previous line
                                        # label.style = "free",
                                        # label.pos = label.pos,
                                        labels = list(cex = 1,
                                                      labels = paste(
                                                          "Power = ",
                                                          round(at.main, 3),
                                                          sep = ""),
                                                      col = grey(0),
                                                      adj = c(0.5, -0.5)),
                                        ...)
                      ## label.style causes often a conflict with the arrows ...
                      ## experience shows that "align" here and "mixed" for the
                      ## others is probably
                      ## best, perhaps the user choice would make sense
                      if (length(at.second) >= 1) {
                        ## drawing the thin line(s)
                        panel.contourplot(x, y, z,
                                          at = at.second,
                                          cut = 2,
                                          lwd = 1,
                                          col = grey(0.4),
                                          label.style = "align",
                                          ##  label.style = "free",
                                          ## label.pos = label.pos,
                                          labels = list(labels = as.character(
                                                           round(at.second, 3)),
                                                        col = grey(0),
                                                        adj = c(0.5, -0.25)),
                                          ...)
                      }
                      ## adding the example
                      if (example){
                        grid.lines(x = unit(c(theta.example, theta.example),
                                            "native"),
                                   y = unit(c(0, sample.size),
                                            c("npc", "native")) +
                                     unit(c(0.02, -0.02),
                                          c("npc", "npc")),
                                   arrow = arrow(length = unit(0.02, "npc")))
                        grid.lines(x = unit(c(theta.example, 0),
                                            c("native", "npc")) +
                                     unit(c(-0.02, 0.02),
                                          c("npc", "npc")),
                                   y = unit(c(sample.size, sample.size),
                                            "native"),
                                   arrow = arrow(length = unit(0.02, "npc")))
                        grid.text(label = paste("N=", sample.size, sep = ""),
                                  x = unit(0.05, "npc"),
                                  y = unit(sample.size, "native"),
                                  just = c(0, -0.2))
                        grid.text(label = bquote(paste(.(symbol), "=",
                                                       .(theta.example),
                                                       sep = "")),
                                  y = unit(0.1, "npc"),
                                  x = unit(theta.example, "native"),
                                  just = c(0.5, -0.2),
                                  rot = 90)
                        grid.points(x = unit(theta.example, "native"),
                                    y = unit(sample.size, "native"),
                                    pch = 20,
                                    size = unit(0.7, "char"))
                      }
                    }
                    )

  ##
  class(cp) <- "trellis"
  print(cp)
}
