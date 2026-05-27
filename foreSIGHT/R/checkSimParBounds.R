# checks for calibrated parameters lying on bounds
checkParsBounds <- function(sim) {
  nRep <- length(which(grepl("Rep", names(sim))))
  nTar <- length(sim[[1]])

  varNames <- names(sim[[1]][[1]])
  varNames <- varNames[!varNames %in% c("attSim", "targetSim", "parS", "score")]

  # currently only setup for single var
  if (length(varNames) > 1) {
    stop("only setup for single var")
  }

  parNames <- names(sim[[1]][[1]][[1]]$par)

  countOnBounds <- matrix(data = 0, nrow = nTar, ncol = length(parNames))
  colnames(countOnBounds) <- parNames

  for (iTar in 1:nTar) {
    tarName <- paste0("Target", iTar)
    # print(tarName)

    for (iRep in 1:nRep) {
      repName <- paste0("Rep", iRep)
      # print(repName)

      for (var in varNames) {
        i <- which(sim[[repName]][[tarName]][[var]]$onBounds)
        parNames[i]
        if (any(sim[[repName]][[tarName]][[var]]$onBounds)) {
          cat(
            "var=", var,
            ", target=", iTar,
            ", rep=", iRep,
            ": parameters ", parNames[i],
            " on bounds\n"
          )
        }
        countOnBounds[iTar, i] <- countOnBounds[iTar, i] + 1
      }
    }
  }

  countOnBounds <- countOnBounds / nRep * 100
  countOnBounds <- round(countOnBounds, digits = 2)
  return(countOnBounds)
}
