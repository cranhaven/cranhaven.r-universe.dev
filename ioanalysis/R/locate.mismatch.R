locate.mismatch <- function(io){
  # Preliminaries
  if(!"InputOutput" %in% class(io)) stop('io should be of "InputOutput" class. See ?as.inputoutput')
  RS_label <- io$RS_label
  regions <- unique(RS_label[, 1])
  sectors <- unique(RS_label[, 2])
  # Let the games begin!
  # A place to store results
  mismatch <- list(NULL)
  for(s in 1:length(sectors)){
    regionswithout <- NULL
    for(r in 1:length(regions)){
      i <- which(RS_label[, 1] == regions[r] & RS_label[, 2] == sectors[s])
      if(length(i) == 0){
        regionswithout <- c(regionswithout, regions[r])
      }
      if(length(i) > 1){
        cat("\n  WARNING:\n")
        cat("\n ", sectors[s], "occurs more than once in", regions[r], "at", i)
        cat("\n  Consider using agg.sector(",deparse(substitute(io)),", sectors = ", s, ", regions = ",r ,', newname = "', sectors[s],'") to combine these sectors into one', sep = "")
        cat("\n  or renaming sector.")
      }
    }
    if(length(regionswithout) > 0){
      regionswith <- setdiff(regions, regionswithout)
      temp <- vector("list", 3)
      names(temp) <- c("location", "regionswith", "regionswithout")
      i <- which(RS_label[, 2] == sectors[s])
      temp[[1]] <- i
      temp[[2]] <- regionswith
      temp[[3]] <- regionswithout
      mismatch[[sectors[s]]] <- temp
    }
  }
  if(length(mismatch) == 1){
    cat("\n    All regions have the same sectors. There are no mismatches.\n\n")
  }
  if(length(mismatch) > 1){
    mismatch[[1]] <- NULL
    mismatch
  }
}
