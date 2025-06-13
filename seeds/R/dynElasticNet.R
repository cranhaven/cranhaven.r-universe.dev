#' estimating the optimal control using the dynamic elastic net
#'
#' @param alphaStep starting value of the stepsize for the gradient descent, will be calculate to minimize the cost function by backtracking algorithm
#' @param armijoBeta  scaling of the alphaStep to find a approximately optimal value for the stepsize
#' @param x0 initial state of the ode system
#' @param parameters parameters of the ODE-system
#' @param alpha1 L1 cost term scalar
#' @param alpha2 L2 cost term scalar
#' @param measData measured values of the experiment
#' @param SD standard deviation of the experiment; leave empty if unknown; matrix should contain the timesteps in the first column
#' @param modelFunc function that describes the ODE-system of the model
#' @param measFunc function that maps the states to the outputs
#' @param optW vector that indicated at which knots of the network the algorithm should estimate the hidden inputs
#' @param origAUC AUCs of the first optimization; only used by the algorithm
#' @param plotEsti boolean that controls of the current estimates should be plotted
#' @param modelInput an dataset that describes the external input of the system
#' @param conjGrad boolean that indicates the usage of conjugate gradient method over the normal steepest descent
#' @param constStr  a string that represents constrains, can be used to calculate a hidden input for a component that gradient is zero
#' @param maxIteration a upper bound for the maximal number of iterations
#' @param eps citeria for stopping the algorithm
#' @param nnStates a bit vector indicating the states that should be non negative
#' @param verbose Boolean indicating if an output in the console should be created to display the gradient descent steps
#'
#' @return A list containing the estimated hidden inputs, the AUCs, the estimated states and resulting measurements and the cost function
optimal_control_gradient_descent <- function(alphaStep, armijoBeta, x0, parameters, alpha1, alpha2, measData, constStr,
                          SD, modelFunc, measFunc, modelInput, optW, origAUC, maxIteration, plotEsti, conjGrad, eps, nnStates, verbose) {
  
  if (.Platform$OS.type != "windows"){
    temp_costate_path <- paste0(tempdir(),'/','costate.R')
    temp_hidden_input_path <- paste0(tempdir(),'/','stateHiddenInput.R')
  } else {
    temp_costate_path <- paste0(tempdir(),'\\','costate.R')
    temp_hidden_input_path <- paste0(tempdir(),'\\','stateHiddenInput.R')
  }
  
  e <- new.env()
  
  source(temp_hidden_input_path, local = e)
  source(temp_costate_path, local = e)

  costate <- get('costate', envir = e)
  hiddenInputState <- get('hiddenInputState', envir = e)

  #### initialization ####
  # optW 
  # startOptW     vectors that indicates states that should be optimized
  #         
  # N             number of equidistant points the numerical solver evaluetes the model function
  # t0, tf        limits of the interval the model is evaluated on
  #               saved in tInt
  # measureTimes  time values of the given measurements
  # measureData   values of the measured data
  startOptW <- optW
  if (missing(optW)) {
    optW <- rep(1, length(x0))
  }

  if (missing(plotEsti)) {
    plotEsti <- FALSE
  }

  if (missing(nnStates)) {
    nnStates <- rep(0, length(x0))
  }

  times <- measData[, 1]
  # set the number of additional timesteps the function should be evaluated at
  N <- 10 * length(times)
  t0 <- times[1]
  tf <- utils::tail(times, n = 1)
  times <- seq(from = t0, to = tf, length.out = N)
  tInt <- c(t0, tf)

  measureTimes <- measData[, 1]
  measureData <- measData[, -1]

  alphaDynNet <- list(a1 = alpha1, a2 = alpha2)


  ##### Create the neccessary function for event based ode solving
  # RootFunc  - Function for finding the root of a given vector (x -> 0, tI < t < tF)
  # EventFunc - Function that the output of the states after being triggered by the root function
  eventTol <- 0.0
  resetValue <- 0.0001


  RootFunc <- eval(parse(text = createRoot(rootStates = nnStates)))
  EventFunc <- eval(parse(text = createEvent(tolerance = eventTol, value = resetValue)))

  #### interpolation of the data ####
  # Q   matrix of weights based on a given standard deviation
  #     Two cases
  #     1. No standard deviation is given:
  #         weights scales the measurements in order to eliminate bias from
  #         extrem differences in measurements
  #         (as suggested by Dominik Kahl)
  #
  #     2. Weights are calculated on given standard deviation (sd)
  #         measurementpoints with high sd are given lower weights to include the
  #         uncertainty of the measurement when estimating the trajectories
  #

  if (is.null(SD)) {
    # Case 1
    measureData <- as.matrix(measureData)
    Q <- matrix(data = rep(1, length(measureData)), ncol = ncol(measureData))
    Q = Q / abs(measureData)
    Q[is.infinite(Q)] = 0
    Q[is.na(Q)] = 0

    interpQ <- apply(X = Q, MARGIN = 2, FUN = function(t) stats::approx(x = measureTimes, y = t, xout = times))
    interpQ = do.call(cbind, lapply(interpQ, FUN = function(t) cbind(t$y)))
    Q <- interpQ
  }
  else {
    # Case 2
    interpSD <- apply(X = SD[,-1], MARGIN = 2, FUN = function(t) stats::approx(x = SD[, 1], y = t, xout = times))
    interpSD = do.call(cbind, lapply(interpSD, FUN = function(t) cbind(t$y)))
    Q <- apply(X = interpSD, MARGIN = 2, FUN = function(t)(1 / t ^ 2) / length(t))
  }

  if (all(is.null(names(x0)))) {
    names(x0) <- paste0(rep("x", length(x0)), 1:length(x0))
  }

  if (!is.null(modelInput)) {
    # with external input
    inputInterp <- list()
    inputInterp <- apply(X = modelInput[, -1, drop = F], MARGIN = 2, FUN = function(x) stats::approxfun(x = modelInput[, 1], y = x, rule = 2, method = 'linear'))
  }

  #### solve the nominal model ####
  #   Case
  #   1   use compiled code
  #   2   use standard deSolve ode function
  if (grepl("Rtools", Sys.getenv('PATH')) || (.Platform$OS.type != "windows")) {
    if (!is.null(modelInput)) {
      inputApprox <- apply(X = modelInput[, -1, drop = F], MARGIN = 2, FUN = function(x) stats::approx(x = modelInput[, 1], y = x, xout = times, rule = 2))
      inputApprox = list(cbind(times, inputApprox$u$y))
    } else {
      inputApprox <- list(cbind(times, rep(0, length(times))))
    }

    w <- matrix(rep(0, length(x0) * length(times)), ncol = length(x0))
    wSplit <- split(w, rep(1:ncol(w), each = nrow(w)))
    wList <- lapply(wSplit, FUN = function(x) cbind(times, x))
    forcings <- c(inputApprox, wList)

    if (sum(nnStates) == 0) {
      solNominal = deSolve::ode(y = x0, times, func = "derivsc",
                              parms = parameters, dllname = "model", initforc = "forcc",
                              forcings = forcings, initfunc = "parmsc")
    } else {
      solNominal = deSolve::lsoda(y = x0, times, func = "derivsc",
                              parms = parameters, dllname = "model", initforc = "forcc",
                              forcings = forcings, initfunc = "parmsc", nroot = sum(nnStates),
                              rootfunc = "myroot", events = list(func = EventFunc, root = TRUE))

    }
  } else {
    if (!is.null(modelInput)) {
      # with external input
      if (sum(nnStates) == 0) {
        solNominal <- as.data.frame(deSolve::ode(y = x0,
                                                  times = times,
                                                  func = modelFunc,
                                                  parms = parameters,
                                                  input = inputInterp))
      } else {
        solNominal <- as.data.frame(deSolve::ode(y = x0,
                                                  times = times,
                                                  func = modelFunc,
                                                  parms = parameters,
                                                  input = inputInterp,
                                                  events = list(func = EventFunc, root = TRUE),
                                                  rootfun = RootFunc))
      }
    } else {
      if (sum(nnStates) == 0) {
        solNominal <- as.data.frame(deSolve::ode(y = x0,
                                                  times = times,
                                                  func = modelFunc,
                                                  parms = parameters))
      } else {
        solNominal <- as.data.frame(deSolve::ode(y = x0,
                                                  times = times,
                                                  func = modelFunc,
                                                  parms = parameters,
                                                  events = list(func = EventFunc, root = TRUE),
                                                  rootfun = RootFunc))
      }
    }
  }

  Tx <- solNominal[, 1]
  x <- solNominal[, -1, drop = FALSE]

  # initialize the hidden inputs as matrix with columns representing the inputs for each state
  w = matrix(rep(0, nrow(x) * ncol(x)), nrow = nrow(x))
  colnames(w) <- paste0(rep("w", ncol(w)), 1:ncol(w))

  # calculate the measurements
  #   Arguments
  #   x         nummerical solution to the system calculated by deSolver
  #   measFunc  a measurement function specified by the user
  #             Cases
  #             1     a measurement function is given
  #             2     if no function is given all states are considered observable
  getMeassures <- function(x, measFunc) {
    if (isTRUE(all.equal(measFunc, function(x) { }))) {
      message('No meassurement function defined. Assuming all states are observable.')
      y = x[, -1, drop = FALSE]
    } else {
      y = measFunc(x[, -1, drop = FALSE])
    }
    
    y = as.data.frame(cbind(x[, 1], y))
    names(y)[1] <- 't'
    names(y)[-1] <- paste0(rep("y", ncol(y) - 1), 1:(ncol(y) - 1))
    return(y)
  }

  # calculate the stepsize of the gradient decent
  #   based on cubic interpolation of the last two values of the costfunction
  #   of the backtracking line search
  #   
  getAlphaBacktracking <- function(oldW, W, Q, y, gradStep, J, currIter, alphaDynNet, alphaS, stepBeta, optW, para, tInt, Tp, measFunc, input, measureTimes, nnStates, rootFunc, eventFunc) {


    iter = 500
    alpha = alphaS
    arrayJ = rep(0, iter)

    time <- seq(from = tInt[1], to = tInt[2], length.out = 100)
    solX <- matrix(rep(0, length(time) * (length(optW) + 1)), ncol = length(optW) + 1)
    Tx <- rep(0, length(time))
    x <- matrix(0, length(optW) * length(time), ncol = length(optW))
    yHat <- matrix(rep(0, length(measData)), ncol = ncol(measData))


    for (i in 1:iter) {
      flagNext <- FALSE
      newW = oldW + alpha * gradStep
      input$w = apply(X = newW, MARGIN = 2, FUN = function(x) stats::approxfun(x = Tp, y = x, method = 'linear', rule = 2))

      if (grepl("Rtools", Sys.getenv('PATH')) || (.Platform$OS.type != "windows")) {
        wSplit <- split(newW, rep(1:ncol(newW), each = nrow(newW)))
        wList <- lapply(wSplit, FUN = function(x) cbind(times, x))
        forcings <- c(inputApprox, wList)

        tryCatch({

          if (sum(nnStates) == 0) {
              solX <- deSolve::ode(y = x0, times = time, func = "derivsc",
                                            parms = parameters, dllname = "model", initforc = "forcc",
                                            forcings = forcings, initfunc = "parmsc")
          } else {
              solX <- deSolve::lsoda(y = x0, times = time, func = "derivsc",
                                     parms = parameters, dllname = "model", initforc = "forcc",
                                     forcings = forcings, initfunc = "parmsc", nroot = sum(nnStates),
                                     rootfunc = "myroot", events = list(func = eventFunc, root = TRUE))
          }
        },
          error = function(cond) {
            # message('Error during calculating the stepsize of the gradient decent:')
            # message('Error while solving the ode system.')
            # message('Try to decrease alphaStep and see if the ode can then be solved.')
            flagNext <<- TRUE
          },
          warning = function(cond) {
            # message('Warning while solving the ode system with the deSolve-package:')
            # message('Original error message:')
            # message(cond)
          },
          finally = {

        }
        )
      } else {
        input$optW = optW
        tryCatch({

          if (sum(nnStates) == 0) {
            R.utils::captureOutput(
              solX <- deSolve::ode(y = x0, times = time, func = hiddenInputState, parms = parameters, input = input)
            )
          } else {

            R.utils::captureOutput(
                solX <- deSolve::ode(y = x0,
                                     times = time,
                                     func = hiddenInputState,
                                     parms = parameters,
                                     input = input,
                                     events = list(func = eventFunc, root = TRUE),
                                     rootfun = rootFunc))
          }
        },
          error = function(cond) {
            message('Error during calculating the stepsize of the gradient decent:')
            message('Error while solving the ode system.')
            message('Try to decrease alphaStep and see if the ode can then be solved.')
          },
          warning = function(cond) {
            message('Warning while solving the ode system with the deSolve-package:')
            message('Original error message:')
            message(cond)
          },
          finally = {

        }
        )
      }

      if (flagNext) {
        next
      }



      Tx = solX[, 1]
      x = solX[, -1, drop = FALSE]

      yHat = getMeassures(solX, measFunc)
      if (sum(is.nan(colSums(x))) > 0 || sum(colSums(x)) == 0) {
        stop('\nThe numerical solution of the ode system failed. See above message.')
      }
      input$interpX = apply(X = x, MARGIN = 2, FUN = function(x) stats::approxfun(x = Tx, y = x, rule = 2, method = 'linear'))
      input$interpyHat = apply(X = yHat[, -1, drop = FALSE], MARGIN = 2, FUN = function(x) stats::approxfun(x = yHat[, 1], y = x, rule = 2, method = 'linear'))

      arrayJ[i] = costFunction(measureTimes, input, alphaDynNet)

      if (i > 1 && (arrayJ[i] > arrayJ[i - 1]) && (arrayJ[i] < J[currIter])) {
        alpha = alphaS * stepBeta ^ (i - 2)
        break
      }
      alpha = alpha * stepBeta
    }


    # cubic interpolation to find the minimum of the costfunction given the 
    # calculated gradient
    intAlpha1 <- alpha
    intAlpha2 <- alpha * stepBeta
    costAlpha1 <- arrayJ[i - 1]
    costAlpha2 <- arrayJ[i]

    # recursive function to find the minimum
    cubicInterpolMin <- function(alphaA, alphaB, jA, jB, nnStates, rootFunc, eventFunc) {
      alpha3 <- 0.5 * (alphaA + alphaB)
      newW = oldW + alpha3 * gradStep

      if (grepl("Rtools", Sys.getenv('PATH')) || (.Platform$OS.type != "windows")) {
        wSplit <- split(newW, rep(1:ncol(newW), each = nrow(newW)))
        wList <- lapply(wSplit, FUN = function(x) cbind(times, x))
        forcings <- c(inputApprox, wList)

        if (sum(nnStates) == 0) {
          solX <- deSolve::ode(y = x0, time, func = "derivsc",
                            parms = parameters, dllname = "model", initforc = "forcc",
                            forcings = forcings, initfunc = "parmsc")
        } else {
          solX <- deSolve::lsoda(y = x0, times = time, func = "derivsc",
                                  parms = parameters, dllname = "model", initforc = "forcc",
                                  forcings = forcings, initfunc = "parmsc", nroot = sum(nnStates),
                                  rootfunc = "myroot", events = list(func = eventFunc, root = TRUE))
        }
      } else {
        input$optW <- optW
        input$w <- apply(X = newW, MARGIN = 2, FUN = function(x) stats::approxfun(x = Tp, y = x, method = 'linear', rule = 2))
        time <- seq(from = tInt[1], to = tInt[2], length.out = 300)

        if (sum(nnStates) == 0) {
          solX <- deSolve::ode(y = x0,
                              times = time,
                              func = hiddenInputState,
                              parms = parameters,
                              input = input)
        } else {
          solX <- deSolve::ode(y = x0,
                              times = time,
                              func = hiddenInputState,
                              parms = parameters,
                              input = input,
                              events = list(func = eventFunc, root = TRUE),
                              rootfun = rootFunc)

        }
      }




      Tx <- solX[, 1]
      x <- solX[, -1, drop = FALSE]

      yHat <- getMeassures(solX, measFunc)

      if (sum(is.nan(colSums(x))) > 0) {
        warning('Numeric Integration failed. Returning last working step.')
        return(alphaA)
      }

      input$interpX <- apply(X = x, MARGIN = 2, FUN = function(x) stats::approxfun(x = Tx, y = x, rule = 2, method = 'linear'))
      input$interpyHat <- apply(X = yHat[, -1, drop = FALSE], MARGIN = 2, FUN = function(x) stats::approxfun(x = yHat[, 1], y = x, rule = 2, method = 'linear'))

      j3 = costFunction(measureTimes, input, alphaDynNet)

      alphaT = alpha3 - (alphaB - alphaA) / 4 * (jB - jA) / (jB - 2 * j3 + jA)

      if (is.nan(alphaT) || (length(alphaT) == 0)) {
        return(alphaA)
      } else {
        if (alphaT > 0) {
          alpha = alphaT
          return(alpha)
        } else {
          alpha <- cubicInterpolMin(alphaA = alphaA, alphaB = alpha3, jA = jA, jB = j3, nnStates = nnStates, rootFunc = rootFunc, eventFunc = eventFunc)
          return(alpha)
        }
      }
    }


    #check if the cubicInterpolation gives a lower value as the last iteration
    #   cubic interpolation can result in an rise of the cost function
    #   check if the calculated alpha is an descent

    alphaTemp <- cubicInterpolMin(alphaA = intAlpha1, alphaB = intAlpha2, jA = costAlpha1, jB = costAlpha2, nnStates = nnStates, rootFunc = rootFunc, eventFunc = eventFunc)

    return(alphaTemp)

  }

  # function that plots the current estimates for each iteration
  showEstimates <- function(measureTimes, AUCs, input, alpha2, J, nomSol, SD) {
    tPlot <- seq(from = measureTimes[1], to = measureTimes[length(measureTimes)], length.out = 50)
    
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    
    
    SD = SD[,-1]

    y <- sapply(input$interpY, mapply, measureTimes)
    yhat <- sapply(input$interpyHat, mapply, tPlot)
    w <- sapply(input$w, mapply, tPlot)
    yNom <- sapply(nomSol, mapply, tPlot)

    J <- unlist(J)
    J = J[J != 0]

    width = 2
    numMeas <- ncol(y)
    if ((numMeas + 3) %% 3 == 0) {
      n <- (numMeas + 3) %/% 3
    } else {
      n <- (numMeas + 3) %/% 3 + 1
    }
    m <- 3
    par(mfrow = c(n, m), ask = F)
    barplot(unlist(AUCs[1,]), col = 'red', xlab = 'hidden inputs', main = 'AUC (a.u)')
    for (i in 1:numMeas) {
      yLab <- paste0('y', as.character(i))
      yMax <- max(max(y[, i]), max(yhat[, i]), max(yNom[, i]))
      yMin <- min(min(y[, i]), min(yhat[, i]), min(yNom[, i]))
      if (is.null(SD)) {
        plot(x = measureTimes, y = y[, i], type = 'p', pch = 20, col = 'black', xlab = 't', ylab = yLab, ylim = c(yMin, yMax), lwd = width)
      } else {
        Hmisc::errbar(x = measureTimes, y = y[, i], yplus = y[, i] + SD[, i], yminus = y[, i] - SD[, i], ylab = yLab, xlab = 't', ylim = c(yMin, yMax), add = FALSE)
      }
      par(new = T)
      plot(x = tPlot, y = yhat[, i], type = 'l', col = 'red', xlab = 't', ylab = yLab, ylim = c(yMin, yMax), lwd = width)
      par(new = T)
      plot(x = tPlot, y = yNom[, i], type = 'l', col = 'blue', xlab = 't', ylab = yLab, ylim = c(yMin, yMax), lwd = width)
    }
    plot(J, type = 'l', xlab = 'iteration', ylab = 'J[w]', lwd = width)
    matplot(x = tPlot, y = w, type = 'l', col = 'red', xlab = 't', lwd = width)
  }


  # cost function that is to be optimized
  costFunction <- function(measureTimes, input, alphaDynNet) {
    y <- sapply(input$interpY, mapply, measureTimes)
    yhat <- sapply(input$interpyHat, mapply, measureTimes)
    q <- sapply(input$q, mapply, measureTimes)
    w <- sapply(input$w, mapply, measureTimes)

    yCost <- list()
    # cost of the deviation of the calculated measurements to the given data
    for (i in 1:ncol(yhat)) {
      yCost$Start[[i]] = sum((yhat[1, i] - y[1, i]) * q[1, i] * (yhat[1, i] - y[1, i]))
      yCost$Middle[[i]] = sum((yhat[, i] - y[, i]) * q[, i] * (yhat[, i] - y[, i]))
      yCost$End[[i]] = sum((yhat[nrow(yhat), i] - y[nrow(y), i]) * q[nrow(q), i] * (yhat[nrow(yhat), i] - y[nrow(y), i]))
    }

    # cost for the inputs
    wCost <- list(L1 = 0, L2 = 0)
    for (i in 1:ncol(w)) {
      wCost$L1 = wCost$L1 + sum(abs(w[, i]))
      wCost$L2 = wCost$L2 + sum(abs(w[, i] ^ 2))
    }
    
    #combining the costs
    cost = sum(unlist(yCost$Start)) + sum(unlist(yCost$Middle)) + sum(unlist(yCost$End)) + alphaDynNet$a1 * wCost$L1 + alphaDynNet$a2 * wCost$L2
    return(cost)
  }
  
  # print(solNominal)
  # print(measFunc)

  yHat <- getMeassures(solNominal, measFunc)
  yNominal <- apply(X = yHat[, -1, drop = FALSE], MARGIN = 2, FUN = function(x) stats::approxfun(x = yHat[, 1], y = x, rule = 2, method = 'linear'))

  #### constraint based calculations ####
  createConst <- function(constString, needGrad) {
    trim <- function(x) gsub(pattern = '\\s', replacement = "", x = x)
    cont = strsplit(x = trim(constString), split = "==")[[1]][2]

    eqs <- character(length = length(needGrad))
    for (i in 1:length(needGrad)) {
      str <- paste0('Solve(', trim(constString), ',x', needGrad[i], ')')
      eqs[i] <- Ryacas::yac_str(str)
    }
    eq <- trim(gsub(pattern = 'list\\(||\\)\\)', replacement = "", x = eqs))
    eq = gsub(pattern = '==', replacement = '=', x = eq)
    eq = gsub(pattern = "(x)([0-9]*)", replacement = 'P[,\\2]', x = eq)
    eq = gsub(pattern = paste0("[*/]*[+-]*", cont), replacement = '', x = eq)


    return(eq)
  }

  evalGrad <- function(constStr, gradM, optW) {
    gradzero <- which(colSums(gradM) == 0)
    optCur <- which(optW > 0)

    nG <- optCur[optCur %in% gradzero]

    if (length(nG) > 0) {
      cP <- createConst(constString = constStr, needGrad = nG)
    }
    return(cP)
  }


  #### interpolation and initiation of main loop ####
  #   initializing the object save time in contrast to letting R manage the memory
  #   possible because to format of all calculated object stays the same
  xInterp <- apply(X = x, MARGIN = 2,
                  FUN = function(x) stats::approxfun(x = Tx, y = x, rule = 2, method = 'linear'))

  yInterp <- apply(X = measData[, -1, drop = FALSE], MARGIN = 2,
                  FUN = function(x) stats::approxfun(x = measData[, 1], y = x, rule = 2, method = 'linear'))

  yHatInterp <- apply(X = yHat[, -1, drop = FALSE], MARGIN = 2,
                  FUN = function(x) stats::approxfun(x = yHat[, 1], y = x, rule = 2, method = 'linear'))

  qInterp <- apply(X = Q, MARGIN = 2,
                  FUN = function(x) stats::approxfun(x = times, y = x, rule = 2, method = 'linear'))

  wInterp <- apply(X = w, MARGIN = 2,
                  FUN = function(x) stats::approxfun(x = Tx, y = x, rule = 2, method = 'linear'))


  # all data that have to be interpolated are stored in a list with name 'input"
  if (is.null(modelInput)) {
    input <- list(optW = optW, interpX = xInterp, interpY = yInterp,
                  interpyHat = yHatInterp, q = qInterp, w = wInterp)
  } else {
    uInterp <- inputInterp
    input <- list(optW = optW, interpX = xInterp, interpY = yInterp,
                  interpyHat = yHatInterp, q = qInterp, w = wInterp, u = uInterp)
  }

  cP <- NULL

  if (missing(maxIteration)) {
    maxIter <- 100
  } else {
    maxIter <- maxIteration
  }

  J <- rep(0, maxIter)
  # cost of the nominal model is stores in vector J as starting value 
  # of the gradient descent
  J[1] = costFunction(measureTimes, input, alphaDynNet)
  if (verbose) {
    cat('\n')
    cat(paste0('Cost nominal model J[w]= ', J[1], '\n'))
  }


  lT <- rep(0., ncol(x))
  timesCostate <- seq(from = tf, to = t0, length.out = N)
  solCostate <- matrix(rep(0., (length(optW) + 1) * length(timesCostate)), ncol = length(optW) + 1)
  Tp <- rep(0., length(timesCostate))
  P <- matrix(rep(0., length(timesCostate) * length(optW)), ncol = length(optW))
  oldW <- matrix(rep(0., length(optW) * length(timesCostate)), ncol = length(optW))
  inputState <- list()
  inputState$optW <- optW
  solX <- matrix(rep(0., length(optW) * length(times)))
  alphaS = alphaStep

  wApprox <- matrix(rep(0., length(times) * 2), ncol = 2)
  wApprox[, 1] = times

  #### MAIN LOOP ####
  for (i in 1:maxIter) {
    tryCatch({
      R.utils::captureOutput(
          solCostate <- deSolve::ode(y = lT, times = timesCostate, func = costate, parms = parameters, input = input)
        )
    },
      error = function(cond) {
        message('Error while solving the costate equation of the model:')
        message('Original error message')
        message(cond)
      },
      warning = function(cond) {
        message('Warning while solving the costate ode system with the deSolve-package:')
        message('Original error message:')
        message(cond)
      },
      finally = { }
    )


    solCostate = solCostate[nrow(solCostate):1,]
    Tp = solCostate[, 1]
    P = solCostate[, -1, drop = FALSE]

    if (!is.null(constStr) && i == 1) {
      cP = evalGrad(gradM = P, optW = optW, constStr = constStr)
    }
    if (!is.null(cP)) {
      eval(parse(text = cP))
    }

    oldW = w

    if (conjGrad) {
      gNeg = P + alpha2 * w
      if (i == 1) {
        oldGrad = -gNeg
        step = gNeg
      }
      else {

        newGrad <- gNeg * (gNeg + oldGrad)
        newInt <- apply(X = newGrad, MARGIN = 2, FUN = function(x) pracma::trapz(Tp, x))

        oldInt <- apply(X = oldGrad, MARGIN = 2, FUN = function(x) pracma::trapz(Tp, x ^ 2))

        newInt[is.nan(newInt)] <- 0
        oldInt[is.nan(oldInt)] <- 0
        betaTest <- sum(newInt) / sum(oldInt)
        step = gNeg + betaTest * step

        oldGrad = -gNeg

      }
    } else {
      step = P + alpha2 * w
    }



    alpha = getAlphaBacktracking(oldW = oldW,
                                 W = w,
                                 Q = Q,
                                 y = measData,
                                 gradStep = step,
                                 J = J,
                                 currIter = i,
                                 alphaDynNet = alphaDynNet,
                                 alphaS = alphaS,
                                 stepBeta = armijoBeta,
                                 optW = optW,
                                 para = parameters,
                                 tInt = tInt,
                                 Tp = Tp,
                                 measFunc = measFunc,
                                 input = input,
                                 measureTimes = measureTimes,
                                 nnStates = nnStates,
                                 rootFunc = RootFunc,
                                 eventFunc = EventFunc)

    # calculate the new hidden inputs
    w = oldW + alpha * step

    if (sum(is.na(colSums(w))) > 0) {
      warning("WARNING: Numerical solution of the ode system failed.\n\t This could result out of the observability of the system.\n")
      break
    }
    inputState$wInterp <- apply(X = w, MARGIN = 2, FUN = function(x) stats::approxfun(x = Tp, y = x, method = 'linear', rule = 2))

    if (grepl("Rtools", Sys.getenv('PATH')) || (.Platform$OS.type != "windows")) {
      ### c solver
      wSplit <- split(w, rep(1:ncol(w), each = nrow(w)))
      wList <- lapply(wSplit, FUN = function(x) cbind(times, x))

      forcings <- c(inputApprox, wList)
      tryCatch({

        if (sum(nnStates) == 0) {
          R.utils::captureOutput(
            solX <- deSolve::ode(y = x0, times, func = "derivsc",
                                parms = parameters, dllname = "model", initforc = "forcc",
                                forcings = forcings, initfunc = "parmsc")
          )

        } else {
          solX <- deSolve::lsoda(y = x0, times, func = "derivsc",
                                parms = parameters, dllname = "model", initforc = "forcc",
                                forcings = forcings, initfunc = "parmsc", nroot = sum(nnStates),
                                rootfunc = "myroot", events = list(func = EventFunc, root = TRUE))
        }
      },
        error = function(cond) {
          message('Error while solving the ode system of the model with the calculated hidden inputs:')
          message('Original error message:\n')
          message(cond)
        },
        warning = function(cond) {
          message('Warning while solving the ode system with the deSolve-package:')
          message('Original error message:')
          message(cond)
        },
        finally = { }
      )

    } else {
      if (!is.null(modelInput)) {
        inputState$u <- inputInterp
      }
      tryCatch({
        if (sum(nnStates) == 0) {
          R.utils::captureOutput(
            solX <- deSolve::ode(y = x0,
                                  times = times,
                                  func = hiddenInputState,
                                  parms = parameters,
                                  input = inputState)
          )
        } else {
          R.utils::captureOutput(
            solX <- deSolve::ode(y = x0,
                                  times = times,
                                  func = hiddenInputState,
                                  parms = parameters,
                                  input = inputState,
                                  events = list(func = EventFunc, root = TRUE),
                                  rootfun = RootFunc)
          )
        }
      },
        error = function(cond) {
          message('Error while solving the ode system of the model with the calculated hidden inputs:')
          message('Original error message:\n')
          message(cond)
        },
        warning = function(cond) {
          message('Warning while solving the ode system with the deSolve-package:')
          message('Original error message:')
          message(cond)
        },
        finally = { }
      )

    }
    ####
    Tx = solX[, 1]
    x = solX[, -1, drop = FALSE]


    yHat <- getMeassures(solX, measFunc)
    input$interpX <- apply(X = x, MARGIN = 2, FUN = function(x) stats::approxfun(x = Tx, y = x, rule = 2, method = 'linear'))
    input$interpyHat <- apply(X = yHat[, -1, drop = FALSE], MARGIN = 2, FUN = function(x) stats::approxfun(x = yHat[, 1], y = x, rule = 2, method = 'linear'))
    input$w <- inputState$wInterp

    J[i + 1] = costFunction(measureTimes, input, alphaDynNet)

    tAUC <- measureTimes
    absW <- abs(sapply(input$w, mapply, tAUC))
    interpAbsW <- apply(X = absW, MARGIN = 2, FUN = function(x) stats::approxfun(x = tAUC, y = x, rule = 2, method = 'linear'))

    AUCs <- sapply(X = interpAbsW, FUN = function(x) pracma::trapzfun(f = x, a = t0, b = tf))
    
    if (verbose) {
      cat(sprintf("Iteration %3d: \t J(w)= %006.4f   change J(w): %6.2f%%  \t alpha = %10.5f \n", i, J[i + 1], (1 - abs(J[i + 1] / J[i])) * 100, alpha))
    }
      
    if (plotEsti == TRUE) {
      showEstimates(measureTimes, AUCs, input, alpha2, J, yNominal, SD)
    }
    # breaking condition of the loop
    #   if the percent wise change of the cost function is smaller than the given
    #   epsilon the alogithm will stop
    if ((abs(J[i + 1] / J[i]) > 1 - (eps / 100))) {
      break
    }
  }

  if (missing(origAUC)) {
    origAUC <- AUCs
  }

  #### Selection of AUC and return ####
  greedySelection <- function(AUC, optW, origAUC) {
    orderAUCs <- order(-do.call(cbind, as.list(origAUC[1,])))
    tempOptW = rep(0, ncol(AUC))

    if (sum(unlist(origAUC[1, ])) == sum(unlist(AUC[1, ]))) {
      # if no aucs are given (first optimisation is running) -> select bigges AUCs
      tempOptW[orderAUCs[1]] = 1
    }
    else {
      tempOptW[orderAUCs[1:(sum(optW) + 1)]] = 1
    }
    return(tempOptW)
  }

  # root mean squared error as measure of the fit
  rmse <- function(measureTimes, input) {
    y <- sapply(input$interpY, mapply, measureTimes)
    yhat <- sapply(input$interpyHat, mapply, measureTimes)

    # feature scaling of the data
    y = apply(y, MARGIN = 2, FUN = function(X)(X - min(X)) / diff(range(X)))
    yhat = apply(yhat, MARGIN = 2, FUN = function(X)(X - min(X)) / diff(range(X)))

    y[is.nan(y)] <- 0
    yhat[is.nan(yhat)] <- 0

    return((colSums((yhat - y) ^ 2)) / nrow(yhat))
  }

  colnames(yHat) <- append('t', paste0('y', 1:(ncol(yHat) - 1)))

  results <- list()
  results$w <- cbind(Tp, w)
  results$AUC <- do.call(cbind, AUCs[1,])
  results$optW <- greedySelection(AUCs, optW, origAUC)
  results$nomX <- solNominal
  results$x <- solX
  results$y <- yHat
  results$rmse <- rmse(measureTimes, input)
  lastJ = J[J > 0]
  lastJ = lastJ[length(lastJ)]
  results$J <- lastJ
  results$totalJ <- J


  return(results)

}
