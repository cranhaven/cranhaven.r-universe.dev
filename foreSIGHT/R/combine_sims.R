######################################################
# combine sim objects from generateScenarios()
# useful when we want to combine sim objects that have different variables

combine_sims <- function(sim.1, var.1, # 1st sim file, and variable name
                         sim.2, var.2, # 2nd sim file, and different variable name
                         combination.reps = T) # whether we consider combinations of replicates, or just match each replicate
{
  sim.new <- list()

  # check dates same for both sims
  if (identical(sim.1$simDates, sim.2$simDates)) {
    sim.new$simDates <- sim.1$simDates
  } else {
    print("simDates differ")
    return()
  }

  # record controlFile info
  sim.new$controlFile.1 <- sim.1$controlFile
  sim.new$controlFile.2 <- sim.2$controlFile

  # combine expSpace info from both sims
  expSpace.1 <- sim.1$expSpace
  expSpace.2 <- sim.2$expSpace
  expSpace.new <- list()
  expSpace.new$attPerturb <- c(expSpace.1$attPerturb, expSpace.2$attPerturb)
  expSpace.new$attPerturbSamp <- c(expSpace.1$attPerturbSamp, expSpace.2$attPerturbSamp)
  expSpace.new$attPerturbMin <- c(expSpace.1$attPerturbMin, expSpace.2$attPerturbMin)
  expSpace.new$attPerturbMax <- c(expSpace.1$attPerturbMax, expSpace.2$attPerturbMax)
  expSpace.new$targetType <- c(expSpace.1$targetType, expSpace.2$targetType)

  # target numbers
  tar.1 <- 1:nrow(expSpace.1$targetMat)
  tar.2 <- 1:nrow(expSpace.2$targetMat)

  # attribute names
  atts.1 <- colnames(expSpace.1$targetMat)
  atts.2 <- colnames(expSpace.2$targetMat)

  atts.total <- c(atts.1, atts.2)

  common <- intersect(atts.1, atts.2)

  if (length(common) > 0) {
    print("sim1 and sim2 cannot have common attributes in expSpaces")
    return()
  }

  # create new targetMat, and indices from each original targetMat
  nTar.total <- length(tar.1) * length(tar.2)
  targetMat <- matrix(nrow = nTar.total, ncol = length(atts.total))
  tar.index.1 <- tar.index.2 <- rep(NA, nrow(targetMat))
  tar <- 0
  for (t.2 in tar.2) {
    for (t.1 in tar.1) {
      tar <- tar + 1
      tar.index.1[tar] <- t.1
      tar.index.2[tar] <- t.2
      targetMat[tar, ] <- c(
        unlist(expSpace.1$targetMat[t.1, ]),
        unlist(expSpace.2$targetMat[t.2, ])
      )
    }
  }
  # format targetMat and add to expSpace.new
  targetMat <- data.frame(targetMat)
  colnames(targetMat) <- atts.total
  expSpace.new$targetMat <- targetMat
  sim.new$expSpace <- expSpace.new

  nReps.1 <- length(which(grepl("Rep", names(sim.1))))
  nReps.2 <- length(which(grepl("Rep", names(sim.2))))
  # calculate all combinations of replicates
  if (combination.reps) {
    nReps.total <- nReps.1 * nReps.2
    rep.index.1 <- rep.index.2 <- rep(NA, nReps.total)
    rep <- 0
    for (r.2 in 1:nReps.2) {
      for (r.1 in 1:nReps.1) {
        rep <- rep + 1
        rep.index.1[rep] <- r.1
        rep.index.2[rep] <- r.2
      }
    }
    # combine individual replicates
  } else {
    if (!nReps.1 == nReps.2) {
      print("require nReps1==nReps2")
      return()
    }
    nReps.total <- nReps.1
    rep.index.1 <- rep.index.2 <- rep(NA, nReps.total)
    for (r in 1:nReps.1) {
      rep.index.1[r] <- r
      rep.index.2[r] <- r
    }
  }


  for (r in 1:nReps.total) {
    repName <- paste0("Rep", r)
    repName.1 <- paste0("Rep", rep.index.1[r])
    repName.2 <- paste0("Rep", rep.index.2[r])
    sim.new[[repName]] <- list()
    for (t in 1:nTar.total) {
      tarName <- paste0("Target", t)
      tarName.1 <- paste0("Target", tar.index.1[t])
      tarName.2 <- paste0("Target", tar.index.2[t])
      sim.new[[repName]][[tarName]][[var.1]] <- sim.1[[repName.1]][[tarName.1]][[var.1]]
      sim.new[[repName]][[tarName]][[var.2]] <- sim.2[[repName.2]][[tarName.2]][[var.2]]
    }
  }

  return(sim.new)
}

######################################################
# add observed climate timeseries to a sim object.
# useful when system model requires input climate data not available in sim
# e.g. for rainfall-runoff model, if we simulate P only, we can add oberved PET

add_obs_var_to_sim <- function(sim, var, data) {
  sim.new <- sim

  nReps <- length(which(grepl("Rep", names(sim))))
  nTar <- length(sim[[1]])

  for (r in 1:nReps) {
    repName <- paste0("Rep", r)
    for (t in 1:nTar) {
      tarName <- paste0("Target", t)
      sim.new[[repName]][[tarName]][[var]] <- list(sim = data)
    }
  }

  sim.new$mergeInfo <- paste0("added ", var)

  return(sim.new)
}
