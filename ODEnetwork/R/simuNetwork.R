# FIXME: Converting polar to cartesian coordinates missing

#' Simulation of the Differential Equations
#' 
#' Simulates the given \code{\link{ODEnetwork}} over a time range.
#'
#' @param odenet [\code{ODEnetwork}]\cr
#'    List of class \code{\link{ODEnetwork}}.
#' @param times [\code{numeric}]\cr
#'    Time sequence to calculate or simulate the ode network.
#' @param origin.min.time [\code{logical(1L)}]\cr
#'    Define origin of ode time.
#'    \code{FALSE} sets it to 0, \code{TRUE} to the minimum of \code{times}.
#'    Default is \code{FALSE}.
#' @param ... Additional arguments.
#' @return an extended list of class [\code{\link{ODEnetwork}}].
#' @export
#' @examples
#' masses <- 4:6
#' dampers <- diag(1:3)
#' springs <- diag(7:9)
#' odenet <- ODEnetwork(masses, dampers, springs)
#' position <- rep(10, 3)
#' velocity <- rep(0, 3)
#' odenet <- setState(odenet, position, velocity)
#' odenet <- simuNetwork(odenet = odenet, times = seq(0, 20))
simuNetwork <- function(odenet, times, origin.min.time = FALSE, ...) {
  UseMethod("simuNetwork")
}

#' @method simuNetwork ODEnetwork
#' @export
simuNetwork.ODEnetwork <- function(odenet, times, origin.min.time = FALSE, ...) {
  assertNumeric(times, any.missing = FALSE)
  assertFlag(origin.min.time)
  
  # calc time origin
  if (origin.min.time) {
    time.origin <- min(times)
    times = times - time.origin
  } else {
    time.origin <- 0
  }
  
  # create events structure from events data
  odenet <- createEvents(odenet)
  # read events and update them if necessary
  if (is.null(odenet$events)) {
    eventdat <- NULL
  } else if (odenet$events$type == "dirac" || odenet$events$type == "constant") {
    eventdat <- odenet$events$data
    if (odenet$coordtype == "polar") {
      # replace "m" with "x" and "a" with "v"
      levels(eventdat$var) <- gsub("m", "x", levels(eventdat$var))
      levels(eventdat$var) <- gsub("a", "v", levels(eventdat$var))
    }
    eventdat <- list(data = eventdat)
  } else if (odenet$events$type == "linear") {
    # get maximum from eventdata, to generate an event that sets the state to the correct value
    # when forcing ends
    eventdat <- odenet$events$data
    # we need only label, and with Martin Morgan trick sorting over label is not needed
    cLab <- eventdat$var[cOrder <- order(eventdat$time)]
    blnLast <- !duplicated(cLab, fromLast = TRUE) # it finds last (max) occurrence of label
    eventdat <- eventdat[seq_len(nrow(eventdat))[cOrder][blnLast], ]
    # convert to cartesian
    if (odenet$coordtype == "polar") {
      for (i in 1:length(odenet$masses)) {
        strSubs <- paste(c("m", "a"), i, sep = ".")
        if (prod(strSubs %in% levels(eventdat$var))) {
          eventdat$value[eventdat$var == strSubs] <- convertCoordinates(matrix(eventdat$value[eventdat$var == strSubs], ncol = 2))
        }
        # replace "m" with "x" and "a" with "v"
        levels(eventdat$var) <- gsub("m", "x", levels(eventdat$var))
        levels(eventdat$var) <- gsub("a", "v", levels(eventdat$var))
      }
    }
    # switch to format for events in ode
    eventdat <- list(data = eventdat)
  }
  
  if (is.null(eventdat)) {
    # solve ODEs analytically
    cN <- length(odenet$masses)
    # derive 0 = My'' + Dy' + Ky + b
    # convert to y'' = -M^-1*D*y' - M^-1*K*y - M^-1*b
    mMinv <- diag(1/odenet$masses, cN)
    # get parameters and convert the matricies in the correct form
    mD <- odenet$dampers
    diag(mD) <- -rowSums(mD)
    mD <- -mD
    mK <- odenet$springs
    diag(mK) <- -rowSums(mK)
    mK <- -mK
    # switch to ODEs of first order with x' = C * x
    # C = rbind( (0, I), (-M^-1*K,-M^-1*D) )
    mC <- rbind(cbind(diag(0, cN), diag(1, cN)), cbind(-mMinv%*%mK, -mMinv%*%mD))
    if (sum(abs(odenet$distances)) > 0) {
      # get distances and convert to correct form
      mR <- odenet$distances
      diag(mR) <- -diag(mR)
      mR[lower.tri(mR)] <- -mR[lower.tri(mR)]
      # calculate vector b with b_i = sum(k_ij*r_ij, j=1..n)
      b <- diag(odenet$springs %*% t(mR))
      # calculate vector for inhomogeneous solution
      # (chol2inv(chol(M)) possible because of symmetric posdev square matrix)
      inhomo <- c(chol2inv(chol(mK)) %*% b, rep(0, cN))
    } else {
      inhomo <- rep(0, 2*cN)
    }
    # starting vector y0, with adjusting position by the inhomogeneous part
    y0 <- matrix(odenet$state)
    # eigenvalues and -vectors of C
    lstEigen <- eigen(mC)
    # constants from starting values: solve [y0 + inv(C) %*% h = V %*% c] for c
    cConstants <- solve(lstEigen$vectors, y0 + inhomo)
    # create solution y = exp(t*c)*y0 by eigenvalues etc
    # empty function
    funODEs <- function() {}
    # time as parameter
    formals(funODEs) <- alist(t = 0)
    # create function text
    # sum(c_i * exp(t*lambda_i) * eigenv_i)
    strFun <- paste("exp(t*(", lstEigen$values, "))", sep = "")
    strFun <- paste("(", cConstants, ")*", strFun, sep = "")
    strTemp <- apply(lstEigen$vectors, 2, paste, collapse = ", ")
    strFun <- paste(strFun, "*c(", strTemp, ")", sep = "")
    strFun <- paste(strFun, collapse = " + ")
    # parse string and add to body
    # function returns a vector of length 2n,
    # the first n values are the positions, followed by the velocities
    body(funODEs) <- as.call(c(as.name("{"), parse(text = strFun)))
    mResult <- vapply(times, funODEs, rep(1i, 2*cN))
    # sort to alternating position and velocity:
    # (time, x.1, v.1, x.2, v.2, ..., x.n, v.n)
    mResOde <- cbind(time = times + time.origin, t(Re(mResult[rep(1:cN, each = 2) + c(0, cN), ])))
    # set correct names
    colnames(mResOde)[2:ncol(mResOde)] <- paste(c("x", "v"), rep(1:cN, each=2), sep = ".")
    # reorder inhomogeneous vector to alternating position and velocity
    inhomo <- inhomo[rep(1:cN, each = 2) + c(0, cN)]
    # apply inhomogeneous part to positions
    mResOde[, -1] <- t(t(mResOde[, -1]) - inhomo)
    # extend the ODEnetwork object
    odenet$simulation$method <- "analytic"
    # add necessary deSolve class attributes
    attr(mResOde, "class") <- c("deSolve", "matrix")
  } else {
    # solve ODEs nummerically
    mResOde <- ode(  y = createState(odenet)		# starting state
                   , times = times     # time vector
                   , func = createOscillators(odenet) # function of all differential equations
                   , parms = createParamVec(odenet)  # create parmeter vector from masses, springs and dampers
                   , events = eventdat
                   , ...
    )
    mResOde[, "time"] = mResOde[, "time"] + time.origin
    # extend the ODEnetwork object
    odenet$simulation$method <- attr(mResOde, "type")
    # overwrite calculations with forcings
    if (!is.null(odenet$events) && odenet$events$type == "linear") {
      # add return of forcings
      for (i in 1:length(odenet$masses)) {
        for (strVar in c("x.", "v.")) {
          if (!is.null(odenet$events$linear[[paste(strVar, i, sep = "")]])) {
            state <- paste(strVar, i, sep = "")
            stateF <- paste(strVar, i, "Force", sep = "")
            # replace non-forcing with forcing, where forcings are not NA
            mResOde[!is.na(mResOde[, stateF]), state] <- mResOde[!is.na(mResOde[, stateF]), stateF]
            mResOde <- mResOde[, !(colnames(mResOde) %in% stateF)]
          }
        }
      }
      # add necessary deSolve class attributes
      attr(mResOde, "class") <- c("deSolve", "matrix")
    }
  }
  # convert to polar coordinates
  if (odenet$coordtype == "polar") {
    # m(agnitude) and a(ngle) instead of x and y
    strNames <- c("m", "a")
    n <- length(odenet$masses)
    for (i in 1:n) {
      mResOde[, c(2*i, 2*i+1)] <- convertCoordinates(mResOde[, c(2*i, 2*i+1)], "polar")
    }
    colnames(mResOde) <- c(colnames(mResOde)[1], paste(strNames, rep(1:n, each=2), sep="."))
  }
  
  # extend the ODEnetwork object
  odenet$simulation$results <- mResOde
  return(odenet)
}
