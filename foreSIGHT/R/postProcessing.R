ppInfoList <- list()

ppInfoList[["annVar"]] <- list(
  npars = 1,
  parNam = c("annVarFac"),
  minBound = c(0.1),
  maxBound = c(5)
)

ppInfoList[["scaleExtremesAll"]] <- list(
  npars = 0,
  scaleExtremesProb = 0.99
)

ppInfoList[["scaleExtremesSeas"]] <- list(
  npars = 0,
  scaleExtremesProb = 0.99
)

ppInfoList[["annCor"]] <- list(
  npars = 1,
  parNam = c("annAR1coeff"),
  minBound = c(-0.5),
  maxBound = c(0.99)
)

#################################

runPP <- function(sim, obs, PPname, parS, datInd, randomTerm = NULL) {
  if (PPname == "annVar") {
    sim <- pp.annVar(sim = sim, annVarFac = parS["annVarFac"], datInd = datInd)
  } else if (PPname == "scaleExtremesAll") {
    sim <- pp.scaleExtremes(sim = sim, obs = obs, prob = ppInfoList[[PPname]]$scaleExtremesProb, strat = "all", datInd = datInd)
  } else if (PPname == "scaleExtremesSeas") {
    sim <- pp.scaleExtremes(sim = sim, obs = obs, prob = ppInfoList[[PPname]]$scaleExtremesProb, strat = "seas", datInd = datInd)
  } else if (PPname == "annCor") {
    sim <- pp.annShuffle(
      P = sim, times = datInd$times,
      annAR1coeff = parS["annAR1coeff"],
      seed = randomTerm$seed
    )
  }

  return(sim)
}

#################################

pp.annVar <- function(sim, annVarFac, datInd) {
  Pann <- rep(NA, datInd$nyr)
  Pdaily <- sim
  for (iy in 1:datInd$nyr) {
    ind <- datInd$i.yy[[iy]]
    Pann[iy] <- sum(Pdaily[ind])
  }

  Pann[Pann == 0] <- 0.00001

  meanPann <- mean(Pann)
  Pann_new <- meanPann + annVarFac * (Pann - meanPann)
  Pann_fac <- Pann_new / Pann

  multSim <- rep(NA, datInd$nTimes)
  for (iy in 1:datInd$nyr) {
    ind <- datInd$i.yy[[iy]]
    multSim[ind] <- Pann_fac[iy]
  }
  multSim <- pmax(multSim, 0)
  sim <- Pdaily * multSim

  if (any(is.na(sim))) {
    browser()
  }

  return(sim)
}

#################################

pp.scaleExtremes <- function(sim, obs, prob, strat, datInd) {
  if (length(sim) != length(obs)) {
    browser()
  }

  if (strat == "all") {
    N <- 1
  } else if (strat == "seas") {
    N <- 4
  }

  for (s in 1:N) {
    if (strat == "all") {
      keep <- 1:datInd$nTimes
    } else if (strat == "seas") {
      keep <- datInd$i.ss[[s]]
    }
    if (any(is.na(sim))) {
      print("sim has na")
    }
    if (any(is.na(sim[keep]))) {
      print("sim[keep] has na")
    }
    fac <- stats::quantile(sim[keep], prob) / stats::quantile(obs[keep], prob)
    nTop <- floor(length(sim[keep]) * (1 - prob))
    sortSim <- sort(sim[keep], decreasing = T, index.return = T)
    i <- keep[sortSim$ix[1:nTop]]
    sortObs <- sort(obs[keep], decreasing = T)[1:nTop]
    sim[i] <- fac * sortObs
  }

  return(sim)
}

###################
# calculate lag-1 autocorrelation
calcAC <- function(x) {
  nx <- length(x)
  AC <- stats::cor(x[1:(nx - 1)], x[2:nx])
  return(AC)
}

###################
# shuffle time series, so that rank of new time series matches rank of simulated AR1 time series

shuffle <- function(annAR1coeff, sort.P.ann, index.P.ann, seed = 1) {
  # simulate AR1 time series with AR1 coefficient annAR1coeff
  set.seed(seed)
  ar1 <- suppressWarnings(as.numeric(stats::arima.sim(n = length(sort.P.ann), list(ar = annAR1coeff))))

  # determine rank of simulate AR1 time series
  rankAR1 <- rank(ar1)

  # re-order P and year based on rakns of AR1 series
  P.ann.new <- sort.P.ann[rankAR1]
  year.new <- index.P.ann[rankAR1]

  # calculate lag-1 correlation for new P
  P.ann.cor <- calcAC(P.ann.new)

  out <- list(
    P.ann.cor = P.ann.cor,
    P.new = P.ann.new,
    year.new = year.new
  )

  return(out)
}

###################

pp.annShuffle <- function(P, times, annAR1coeff, seed = 1, iyy = NULL, return.indices = F) {
  years.all <- as.integer(format(times, "%Y"))
  years <- unique(years.all)

  P.ann <- c()
  for (y in 1:length(years)) {
    year <- years[y]
    keep <- which(years.all == year)
    P.ann[y] <- sum(P[keep])
  }

  s <- sort(P.ann, index.return = T)
  sort.P.ann <- s$x
  index.P.ann <- s$ix

  o <- shuffle(
    annAR1coeff = annAR1coeff,
    sort.P.ann = sort.P.ann,
    index.P.ann = index.P.ann,
    seed = seed
  )

  P.ann.cor <- o$P.ann.cor
  P.new <- o$P.new
  year.new <- o$year.new

  #################################

  P.new.daily <- c()
  keepList <- c()
  for (y in 1:length(years)) {
    year <- years[1] + (year.new[y] - 1)
    if (is.null(iyy)) {
      keep <- which(years.all == year)
    } else {
      keep <- iyy[[year + 1]]
    }
    keepList <- c(keepList, keep)
  }

  P.new.daily <- P[keepList]

  if (!return.indices) {
    return(P.new.daily)
  } else {
    return(list(
      P.new = P.new.daily,
      indices = keepList
    ))
  }
}

######################################################

#' Post-processing to apply changes in temporal structure of annual precipitation.
#'
#' \code{shuffle_sim} changes the temporal structure of annual climate to match
#' a target change in a perturbed attribute
#' @param sim a list; simulated climate object from \code{generateScenarios}
#' @param clim a list; reference climate
#' @param attPerturb a string; name of attribute to be perturbed
#' @param targetVals a vector of numbers; targets for perturbed attributes
#' @param targetType a vector of strings; type of change in target attributes (either 'frac' or 'diff')
#' @param seed an integer; random number seed
#' @param annAR1coeffList a vector of numbers; the range of values for annual AR(1) parameters used in grid search
#' @param cSel an integer or string; for multi-site data, can either cvalculate perturbed attributes on a single site (integer) or the mean climate over all sites (\code{cSel='mean'}).
#' @return A list with simulated perturbed climates. Same format as output from \code{generateScenarios}.
#' @examples
#' \dontrun{
#' ###############
#' # load dates, precip, PET and streamflow data for Scott Creek
#' data('data_A5030502')
#' # create reference data - won't include PET here since not modelled 
#' clim_ref = list(times = data_A5030502$times,
#'                 P = data_A5030502$P)  
#' ###############
#' # create exposure space for baseline climate - no perturbations
#' 
#' attPerturbType = "regGrid"
#' attPerturb = c('P_day_all_tot_m')
#' attPerturbSamp = c(1)
#' attPerturbMin = c(1)
#' attPerturbMax = c(1)
#' # will hold a number of attributes to historical values
#' # note: normP = P99/avg rainfall.  
#' attHold = c('P_day_all_P99','P_day_all_avgDSD','P_day_all_nWet_m',
#'             'P_day_DJF_tot_m','P_day_DJF_normP99','P_day_DJF_avgDSD','P_day_DJF_nWet_m',
#'             'P_day_MAM_tot_m','P_day_MAM_normP99','P_day_MAM_avgDSD','P_day_MAM_nWet_m',
#'             'P_day_JJA_tot_m','P_day_JJA_normP99','P_day_JJA_avgDSD','P_day_JJA_nWet_m',
#'             'P_day_SON_tot_m','P_day_SON_normP99','P_day_SON_avgDSD','P_day_SON_nWet_m')
#' expSpace = createExpSpace(attPerturb = attPerturb,
#'                           attPerturbSamp = attPerturbSamp,
#'                           attPerturbMin = attPerturbMin,
#'                           attPerturbMax = attPerturbMax,
#'                           attPerturbType = attPerturbType,
#'                           attHold = attHold)
#' 
#' ###############
#' # setup model settings
#' 
#' modelSelection = list()
#' 
#' modelSelection$modelType = list()
#' modelSelection$modelType$P = "latent" # latent variable model
#' 
#' modelSelection$modelParameterVariation = list()
#' modelSelection$modelParameterVariation$P = "seas" # parameters vary with season
#' 
#' modelSelection[["optimisationArguments"]] = list()
#' modelSelection[["optimisationArguments"]][["OFtol"]] = 0.1 # stop optimization when OF < OFtol
#' 
#' # set penalty weights. More weights to total, and attributes for 'all' data)
#' modelSelection[["penaltyAttributes"]]=c('P_day_all_tot_m','P_day_all_P99',
#'                                         'P_day_all_avgDSD','P_day_all_nWet_m')
#' modelSelection[["penaltyWeights"]] = c(3,2,2,2)
#' 
#' # write to JSON file
#' modelSelectionJSON = jsonlite::toJSON(modelSelection, pretty = TRUE, auto_unbox = TRUE)
#' controlFile = paste0(tempdir(), "\\eg_controlFile.json")
#' write(modelSelectionJSON, file = controlFile)
#' 
#' ###############
#' # generate baseline climate scenarios
#' 
#' time.1 = Sys.time()
#' sim.base = generateScenarios(reference = clim_ref,
#'                              expSpace = expSpace,
#'                              controlFile = controlFile,
#'                              seedID = 1,
#'                              numReplicates = 1)
#' time.2 = Sys.time()
#' print(time.2-time.1)
#' 
#' ###############
#' # shuffle baseline climate to introduce temporal structure at annual scale
#' 
#' sim.shuffle = shuffle_sim(sim=sim.base,
#'                           clim=clim_ref,
#'                           attPerturb = 'P_day_all_tot_dwellTime',
#'                           targetVals = c(1,1.5,2,2.5),
#'                           targetType = 'frac')
#' 
#' ###############
#' # evaluate how changes in 'P_day_all_tot_dwellTime' affect other attributes
#' 
#' attSel = colnames(sim.base$expSpace$targetMat)
#' # other attributes
#' attSel = c(attSel,'P_day_all_tot_dwellTime','P_day_all_avgWSD',
#'            'P_day_all_P99.9','P_day_JJA_P99.9','P_day_SON_P99.9',
#'            'P_day_DJF_P99.9','P_day_MAM_P99.9')
#' 
#' att = 'P_day_all_tot_dwellTime'
#' par(mfrow=c(5,3),mar=c(4,7,2,1))
#' # plot changes in a single attribute with respect to perturbed attributes
#' plotPerformanceAttributesOAT(clim=clim_ref,
#'                              sim=sim.shuffle,
#'                              attPerturb=att,
#'                              attEval=attSel,
#'                              cex.main = 1.5,cex.xaxis = 1,cex.yaxis = 1)  
#' }
#' @export
shuffle_sim <- function(sim,
                        clim,
                        attPerturb = "P_day_all_tot_dwellTime",
                        targetVals,
                        targetType = "frac",
                        seed = 1,
                        annAR1coeffList = seq(-0.2, 0.9, 0.01),
                        cSel = "mean") {
  perturb.varname <- get.attribute.varType(attPerturb)

  varNames <- unlist(lapply(colnames(sim$expSpace$targetMat), get.attribute.varType))
  varNames <- unique(unlist(strsplit(varNames, "/")))
  other.varnames <- varNames[varNames != perturb.varname]

  expSpace <- sim$expSpace
  expSpace.new <- list()
  expSpace.new$attPerturb <- c(expSpace$attPerturb, attPerturb)
  expSpace.new$attHold <- expSpace$attHold
  expSpace.new$targetType <- c(expSpace$targetType, targetType)
  expSpace.new$attTied <- expSpace$attTied

  targetMat.orig <- expSpace$targetMat

  tar.1 <- 1:nrow(targetMat.orig)
  tar.2 <- 1:length(targetVals)
  nTar.total <- length(tar.1) * length(tar.2)

  atts.1 <- colnames(targetMat.orig)
  atts.total <- c(atts.1, attPerturb)
  nAtt.Total <- length(atts.total)

  targetMat <- matrix(nrow = nTar.total, ncol = nAtt.Total)
  tar.index.1 <- tar.index.2 <- rep(NA, nrow(targetMat))
  tar <- 0
  for (t.2 in tar.2) {
    for (t.1 in tar.1) {
      tar <- tar + 1
      tar.index.1[tar] <- t.1
      tar.index.2[tar] <- t.2
      targetMat[tar, ] <- c(unlist(targetMat.orig[t.1, ]), targetVals[t.2])
    }
  }
  targetMat <- data.frame(targetMat)
  colnames(targetMat) <- atts.total
  expSpace.new$targetMat <- targetMat

  nReps <- length(which(grepl("Rep", names(sim))))

  seed_list <- seed + 0:(nReps - 1)

  d <- dim(clim[[perturb.varname]])
  multisite <- FALSE
  if (!is.null(d)) {
    if (d[2] > 1) {
      multisite <- TRUE
    }
  }

  if (multisite) {
    if (!is.null(cSel)) {
      if (cSel == "mean") {
        clim_ref[[perturb.varname]] <- apply(clim_ref[[perturb.varname]], 1, mean)
      } else {
        clim_ref[[perturb.varname]] <- clim_ref[[perturb.varname]][, cSel]
      }
    }
  }

  att.clim <- calculateAttributes(clim_ref, attPerturb)

  times <- sim$simDates
  att.sim.multi <- list()
  sim.new <- list()
  for (r in 1:nReps) {
    repName <- paste0("Rep", r)
    print(repName)
    sim.new[[repName]] <- list()
    att.sim.multi[[repName]] <- list()
    for (t in 1:nTar.total) {
      tarName <- paste0("Target", t)
      print(tarName)
      tarName.1 <- paste0("Target", tar.index.1[t])
      var.sim <- sim[[repName]][[tarName.1]][[perturb.varname]]$sim
      if (!is.matrix(var.sim)) {
        var.sim <- as.matrix(var.sim, ncol = 1)
      }
      if (is.null(att.sim.multi[[repName]][[tarName.1]])) {
        att.sim.multi[[repName]][[tarName.1]] <- list()
        if (multisite) {
          if (!is.null(cSel)) {
            if (cSel == "mean") {
              var.sim.single <- apply(var.sim, 1, mean)
            } else {
              var.sim.single <- var.sim[, cSel]
            }
          }
        } else {
          var.sim.single = var.sim
        }
        att.sim.multi[[repName]][[tarName.1]] <- c()
        clim_sim_save <- var.sim.new.list <- list()
        for (i in 1:length(annAR1coeffList)) {
          annAR1coeff <- annAR1coeffList[i]
          var.sim.new <- pp.annShuffle(
            P = var.sim.single, times = times, annAR1coeff = annAR1coeff,
            seed = seed_list[r]
          )
          var.sim.new.list[[i]] <- var.sim.new
          clim_sim <- list(times = times)
          clim_sim[[perturb.varname]] <- var.sim.new
          clim_sim_save[[i]] <- clim_sim
          att.sim.multi[[repName]][[tarName.1]][i] <- calculateAttributes(clim_sim, attPerturb)
        }
      }

      # sim.new[[repName]][[tarName]][[perturb.varname]] = sim[[repName]][[tarName.1]][[perturb.varname]]

      att.target <- targetMat[t, attPerturb] * att.clim

      abs_diff <- abs(att.sim.multi[[repName]][[tarName.1]] - att.target)
      i <- min(which(abs_diff == min(abs_diff)))
      # med <- median(i)  # Interpolated median
      # Find the actual value in x closest to it
      # i <- i[which.min(abs(i - med))]

      # par(mfrow=c(1,1))
      # ylim = c(min(att.sim.multi[[repName]][[tarName.1]],att.clim,att.sim),
      #          max(att.sim.multi[[repName]][[tarName.1]],att.clim,att.sim))
      # plot(annAR1coeffList,att.sim.multi,ylim=ylim)
      # abline(h=att.clim,col='blue')
      # abline(h=att.sim,col='red')
      # abline(h=att.target,col='green')

      print(paste0(att.target, " ", att.sim.multi[[repName]][[tarName.1]][i]))

      annAR1coeff <- annAR1coeffList[i]
      pp.annShuffle.out <- pp.annShuffle(
        P = var.sim.single, times = times, annAR1coeff = annAR1coeff,
        seed = seed_list[r], return.indices = T
      )
      #      print(i)
      #      print(var.sim.new.list[[i]][1:20])
      #      print(pp.annShuffle.out$P.new[1:20])

      sim.new[[repName]][[tarName]][[perturb.varname]] <- list(sim = var.sim[pp.annShuffle.out$indices, ])
      for (other.varname in other.varnames) {
        other.sim <- sim[[repName]][[tarName.1]][[other.varname]]$sim
        if (!is.matrix(other.sim)) {
          other.sim <- as.matrix(other.sim, ncol = 1)
        }
        other.sim.targ <- other.sim[pp.annShuffle.out$indices, ]
        sim.new[[repName]][[tarName]][[other.varname]] <- list(sim = other.sim.targ)
      }

      clim_sim_shuffle <- list(times = times)
      if (multisite) {
        if (!is.null(cSel)) {
          if (cSel == "mean") {
            clim_sim_shuffle[[perturb.varname]] <- apply(sim.new[[repName]][[tarName]][[perturb.varname]]$sim, 1, mean)
          } else {
            clim_sim_shuffle[[perturb.varname]] <- sim.new[[repName]][[tarName]][[perturb.varname]]$sim[, cSel]
          }
        } 
      } else {
        clim_sim_shuffle[[perturb.varname]] = sim.new[[repName]][[tarName]][[perturb.varname]]$sim
      }
      print(calculateAttributes(clim_sim_shuffle, attPerturb))

      if (att.sim.multi[[repName]][[tarName.1]][i] != calculateAttributes(clim_sim_shuffle, attPerturb)) {
        browser()
      }
    }
  }

  sim.new$expSpace <- expSpace.new
  sim.new$simDates <- sim$simDates

  return(sim.new)
}

######################################################
