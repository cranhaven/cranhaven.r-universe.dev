#' @title \code{testCoAccessibilityChromVar}
#'
#' @description
#'   `r lifecycle::badge("deprecated")`
#'   This function is deprecated due as we no longer recommend using a chromVAR 
#'   background set.
#'   \code{testCoAccessibilityChromVar} takes an input set of tile
#'   pairs and tests whether they are significantly different compared to a
#'   background set found via ChromVAR
#'
#' @param SampleTileObj The SummarizedExperiment object output from
#'   getSampleTileMatrix containing your sample-tile matrices
#' @param tile1 vector of indices or tile names (chrX:100-2000) for tile pairs
#'   to test (first tile in each pair)
#' @param tile2 vector of indices or tile names (chrX:100-2000) for tile pairs
#'   to test (second tile in each pair)
#' @param backNumber number of ChromVAR-matched background pairs. Default is
#'   1000.
#' @param returnBackGround Boolean, if TRUE return the background correlations
#'   as well as foreground. Default is FALSE.
#' @param highMem Boolean to control memory usage. Default is FALSE. Only set
#'   highMem to TRUE if you have plenty of memory and want to run this function
#'   faster.s
#' @param numCores Optional, the number of cores to use with multiprocessing.
#'   Default is 1.
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#' @param ZI boolean flag that enables zero-inflated (ZI) Spearman correlations
#'   to be used. Default is TRUE. If FALSE, skip zero-inflation and calculate
#'   the normal Spearman.
#'
#' @return foreGround A data.frame with Tile1, Tile2, Correlation, and p-value
#'   for that correlation compared to the background
#'
#' @export
#' @keywords internal
testCoAccessibilityChromVar <- function(SampleTileObj,
                                        tile1,
                                        tile2,
                                        numCores = 1,
                                        ZI = TRUE,
                                        backNumber = 1000,
                                        returnBackGround = FALSE,
                                        highMem = FALSE,
                                        verbose = TRUE) {
  lifecycle::deprecate_warn("1.0.1", 
                            "testCoAccessibilityChromVar()", 
                            "MOCHA::testCoAccessibility()")
  . <- NULL

  if (!requireNamespace("chromVAR", quietly = TRUE)) {
    stop(
      "Package 'chromVAR' is required for combineSampleTileMatrix. ",
      "Please install 'chromVAR' to proceed."
    )
  }

  if (length(tile1) != length(tile2)) {
    stop("tile1 and tile2 must be the same length.")
  }

  fullObj <- combineSampleTileMatrix(SampleTileObj)

  backPeaks <- chromVAR::getBackgroundPeaks(fullObj)

  if (is.character(tile1) && is.character(tile2)) {
    nTile1 <- match(tile1, rownames(fullObj))
    nTile2 <- match(tile2, rownames(fullObj))
  } else if (is.numeric(tile1) && is.numeric(tile2)) {
    nTile1 <- tile1
    nTile2 <- tile2

    tile1 <- rownames(fullObj)[nTile1]
    tile2 <- rownames(fullObj)[nTile2]
  } else {
    stop("tile1 and tile 2 must both be either numbers (indices) or strings")
  }

  if (backNumber > 2450) {
    backNumber <- 2450
    if (verbose) {
      warning("backNumber too high, setting to maximum of 2450.")
    }
  } else if (backNumber <= 10) {
    stop("backNumber too low (<=10). We recommend at least 100.")
  }

  cl <- parallel::makeCluster(numCores)
  parallel::clusterExport(cl, varlist = c("nTile1", "nTile2", "backPeaks"), envir = environment())

  if (verbose) {
    message("Finding background peak pairs")
  }

  backgroundCombos <- pbapply::pblapply(seq_along(nTile1), function(x) {
    tmpMat <- expand.grid(backPeaks[nTile1[x], ], backPeaks[nTile2[x], ])
    tmpMat <- tmpMat[tmpMat[, 1] != tmpMat[, 2], ]
    tmpMat[sample.int(dim(tmpMat)[1], backNumber), ]
  }, cl = cl)


  gc()

  parallel::stopCluster(cl)
  cl <- parallel::makeCluster(numCores)

  accMat <- SummarizedExperiment::assays(fullObj)[[1]]

  ## Test original pairs of locations
  combPairs <- data.frame(tile1, tile2)
  subAccMat <- accMat[unique(c(nTile1, nTile2)), ]

  if (verbose) {
    message("Identifying foreground")
  }

  parallel::clusterExport(cl, varlist = c("subAccMat", "combPairs"), envir = environment())

  foreGround <- runCoAccessibility(subAccMat, combPairs, ZI, verbose, cl)
  if (any(is.na(foreGround$Correlation))) {
    if (verbose) {
      warning("All foreground correlations are undefined")
    }
  }
  parallel::stopCluster(cl)

  gc()

  ## Now we need to test the background set

  if (verbose) {
    message("Identifying background correlations.")
  }

  if (highMem) {
    allBackCombos <- do.call("rbind", backgroundCombos)

    uniqueBackCombos <- unique(allBackCombos)

    combPairs <- data.frame(
      Tile1 = rownames(accMat)[uniqueBackCombos[, 1]],
      Tile2 = rownames(accMat)[uniqueBackCombos[, 2]]
    )

    subAccMat <- accMat[unique(c(uniqueBackCombos[, 1], uniqueBackCombos[, 2])), ]

    cl <- parallel::makeCluster(numCores)

    if (verbose) {
      message(
        paste("Generating Background correlations for all",
          length(tile1), "pairs",
          sep = " "
        )
      )
    }

    parallel::clusterExport(cl, varlist = c("subAccMat", "combPairs"), envir = environment())

    uniquebackGround <- runCoAccessibility(subAccMat, combPairs, ZI, verbose, cl)

    allBackCombosDF <- data.frame(
      Tile1 = rownames(accMat)[allBackCombos[, 1]],
      Tile2 = rownames(accMat)[allBackCombos[, 2]]
    )

    backGround <- dplyr::left_join(allBackCombosDF, uniquebackGround, by = c("Tile1" = "Tile1", "Tile2" = "Tile2")) %>%
      dplyr::group_by(dplyr::row_number() %/% (backNumber + 1)) %>%
      dplyr::group_map(~.x)

    parallel::stopCluster(cl)
  } else {
    ### low memory implementation

    backGround <- list()

    for (i in seq_along(backgroundCombos)) {
      combPairs <- data.frame(
        Tile1 = rownames(accMat)[backgroundCombos[[i]][, 1]],
        Tile2 = rownames(accMat)[backgroundCombos[[i]][, 2]]
      )

      subAccMat <- accMat[unique(c(backgroundCombos[[i]][, 1], backgroundCombos[[i]][, 2])), ]

      cl <- parallel::makeCluster(numCores)
      if (verbose) {
        message(paste("Generating Background correlations for", i, "of", length(tile1), "pairs", sep = " "))
      }
      parallel::clusterExport(cl, varlist = c("subAccMat", "combPairs"), envir = environment())

      tmp_background <- runCoAccessibility(subAccMat, combPairs, ZI, verbose, cl)

      parallel::stopCluster(cl)

      gc()

      backGround <- append(backGround, list(tmp_background))
    }
  }


  cl <- parallel::makeCluster(numCores)
  parallel::clusterExport(cl, varlist = c("foreGround", "backGround"), envir = environment())

  pValues <- pbapply::pblapply(seq_along(backGround), function(x) {
    cor1 <- foreGround$Correlation[x]

    if (is.na(cor1)) {
      NA
    } else if (cor1 >= 0) {
      sum(cor1 > backGround[[x]]$Correlation) / length(backGround[[x]]$Correlation)
    } else if (cor1 < 0) {
      sum(cor1 < backGround[[x]]$Correlation) / length(backGround[[x]]$Correlation)
    }
  }, cl = cl) %>% unlist()
  parallel::stopCluster(cl)

  gc()

  foreGround$pValues <- pValues

  if (returnBackGround) {
    return(list("Foreground" = foreGround, "Background" = backGround))
  } else {
    return(foreGround)
  }
}

#' @title \code{testCoAccessibilityRandom}
#'
#' @description 
#'   `r lifecycle::badge("deprecated")`
#'   Renamed to `testCoAccessibility` to remove unnecessary specificity around
#'   the background set, and for a shorter function name.
#' 
#'   \code{testCoAccessibilityRandom} takes an input set of tile
#'   pairs and tests whether they are significantly different compared to
#'   random, non-overlapping background set.
#'   
#' @param SampleTileObj The SummarizedExperiment object output from
#'   getSampleTileMatrix containing your sample-tile matrices
#' @param tile1 vector of indices or tile names (chrX:100-2000) for tile pairs
#'   to test (first tile in each pair)
#' @param tile2 vector of indices or tile names (chrX:100-2000) for tile pairs
#'   to test (second tile in each pair)
#' @param backNumber number of background pairs. Default is 1000.
#' @param calcPValue Boolean, if TRUE calculate p-values. Default is TRUE.
#' @param returnBackGround Boolean, if TRUE return the background correlations
#'   as well as foreground. Default is FALSE.
#' @param numCores Optional, the number of cores to use with multiprocessing.
#'   Default is 1.
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#' @param ZI boolean flag that enables zero-inflated (ZI) Spearman correlations
#'   to be used. Default is TRUE. If FALSE, skip zero-inflation and calculate
#'   the normal Spearman.
#'
#' @return foreGround A data.frame with Tile1, Tile2, Correlation, and p-value
#'   for that correlation compared to the background
#'
#' @keywords internal
#' @export
#'
testCoAccessibilityRandom <- function(SampleTileObj,
                                      tile1,
                                      tile2,
                                      numCores = 1,
                                      ZI = TRUE,
                                      backNumber = 1000,
                                      calcPValue = TRUE,
                                      returnBackGround = FALSE,
                                      verbose = TRUE) {
  . <- NULL
  lifecycle::deprecate_warn("1.0.1", 
                            "testCoAccessibilityRandom()", 
                            "MOCHA::testCoAccessibility()")
  testCoAccessibility(SampleTileObj,
                      tile1,
                      tile2,
                      numCores,
                      ZI,
                      backNumber,
                      calcPValue,
                      returnBackGround,
                      verbose)
}

#' @title \code{testCoAccessibility}
#'
#' @description 
#'   \code{testCoAccessibility} takes an input set of tile
#'   pairs and tests whether they are significantly different compared to
#'   random, non-overlapping background set.
#'   
#' @param SampleTileObj The SummarizedExperiment object output from
#'   getSampleTileMatrix containing your sample-tile matrices
#' @param tile1 vector of indices or tile names (chrX:100-2000) for tile pairs
#'   to test (first tile in each pair)
#' @param tile2 vector of indices or tile names (chrX:100-2000) for tile pairs
#'   to test (second tile in each pair)
#' @param backNumber number of background pairs. Default is 1000.
#' @param calcPValue Boolean, if TRUE calculate p-values. Default is TRUE.
#' @param returnBackGround Boolean, if TRUE return the background correlations
#'   as well as foreground. Default is FALSE.
#' @param numCores Optional, the number of cores to use with multiprocessing.
#'   Default is 1.
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#' @param ZI boolean flag that enables zero-inflated (ZI) Spearman correlations
#'   to be used. Default is TRUE. If FALSE, skip zero-inflation and calculate
#'   the normal Spearman.
#'
#' @return foreGround A data.frame with Tile1, Tile2, Correlation, and p-value
#'   for that correlation compared to the background
#'
#' @export
#'
testCoAccessibility <- function(SampleTileObj,
                                tile1,
                                tile2,
                                numCores = 1,
                                ZI = TRUE,
                                backNumber = 1000,
                                calcPValue = TRUE,
                                returnBackGround = FALSE,
                                verbose = TRUE) {
  . <- NULL
  
  if (length(tile1) != length(tile2)) {
    stop("tile1 and tile2 must be the same length.")
  }
  
  fullObj <- combineSampleTileMatrix(SampleTileObj)
  
  if (is.character(tile1) && is.character(tile2)) {
    nTile1 <- match(tile1, rownames(fullObj))
    nTile2 <- match(tile2, rownames(fullObj))
  } else if (is.numeric(tile1) && is.numeric(tile2)) {
    nTile1 <- tile1
    nTile2 <- tile2
    
    tile1 <- rownames(fullObj)[nTile1]
    tile2 <- rownames(fullObj)[nTile2]
  } else {
    stop("tile1 and tile 2 must both be either numbers (indices) or strings")
  }
  
  # Only run this if the backNumber is an actual number. If an actually background set is prepare, skip it.
  if (is.null(dim(backNumber))) {
    if (backNumber >= length(rownames(fullObj)) - length(unique(c(tile1, tile2)))) {
      backNumber <- length(rownames(fullObj)) - length(unique(c(tile1, tile2)))
      if (verbose) {
        warning("backNumber too high. Reset to all background combinations.")
      }
    } else if (backNumber <= 10) {
      stop("backNumber too low (<=10). We recommend 1000.")
    }
  }
  
  accMat <- SummarizedExperiment::assays(fullObj)[[1]]
  
  ## Test original pairs of locations
  combPairs <- data.frame(tile1, tile2)
  
  if (verbose) {
    message("Identifying foreground")
  }
  
  cl <- parallel::makeCluster(numCores)
  foreGround <- runCoAccessibility(accMat, combPairs, ZI, verbose, cl)
  parallel::stopCluster(cl)
  gc()
  
  if (any(is.na(foreGround$Correlation))) {
    if (verbose) {
      warning("All foreground correlations are undefined")
    }
  }
  
  if (is.null(dim(backNumber))) {
    if (verbose) {
      message("Finding background peak pairs")
    }
    
    backGroundTiles <- rownames(accMat)[!rownames(accMat) %in% c(tile1, tile2)]
    
    backgroundCombos <- data.frame(
      Tile1 = sample(backGroundTiles, backNumber),
      Tile2 = sample(backGroundTiles, backNumber)
    )
    
    backgroundCombos <- backgroundCombos[backgroundCombos[, 1] != backgroundCombos[, 2], ]
  } else if (dim(backNumber)[2] > 1) {
    if (verbose) {
      message("Using user-defined background pairs")
    }
    
    backNumber <- as.data.frame(backNumber)
    if (!all(c("Tile1", "Tile2") %in% colnames(backNumber))) {
      stop("User-defined background pairs requires a column for Tile1 and Tile2")
    } else if (!all(grepl(":", c(backNumber[, "Tile1"], backNumber[, "Tile2"])) & grepl("-", backNumber[, "Tile1"], backNumber[, "Tile2"]))) {
      stop("User-defined background pairs must be in the form ChrX:100-2000")
    } else if (!all(c(backNumber[, "Tile1"], backNumber[, "Tile2"]) %in% rownames(fullObj))) {
      stop("User-defined background pairs includes regions not found within the sample tile accessibility matrix.")
    }
    
    backgroundCombos <- as.data.frame(backNumber)[, c("Tile1", "Tile2")]
    
    if (sum(backgroundCombos[, "Tile1"] != backgroundCombos[, "Tile2"]) < 10) {
      stop("User-defined background pairs are fewer than 10. Please provide a larger background.")
    } else {
      backgroundCombos <- backgroundCombos[backgroundCombos[, "Tile1"] != backgroundCombos[, "Tile2"], ]
    }
  } else {
    stop("Incorrect backNumber provided. Please provider either a number, or a data.frame with columns entitled Tile1 and Tile2, describing pairs to test. The tile names should be in the format ChrX:100-2000.")
  }
  rm(combPairs)
  
  ## Now we need to test the background set
  
  if (verbose) {
    message("Identifying background correlations.")
  }
  cl <- parallel::makeCluster(numCores)
  backGround <- runCoAccessibility(
    accMat = accMat,
    pairs = backgroundCombos, ZI = ZI, verbose = verbose,
    numCores = cl
  )
  
  
  parallel::stopCluster(cl)
  gc()
  
  rm(accMat)
  rm(backgroundCombos)
  rm(fullObj)
  
  if (calcPValue) {
    if (verbose) {
      message("Generating p-values.")
    }
    
    greatList <- unlist(pbapply::pblapply(foreGround$Correlation[which(foreGround$Correlation > 0)],
                                          function(x) {
                                            return(sum(x > backGround$Correlation))
                                          },
                                          cl = 1
    )) / length(backGround$Correlation)
    
    lesserList <- unlist(pbapply::pblapply(foreGround$Correlation[which(foreGround$Correlation < 0)],
                                           function(x) {
                                             return(sum(x < backGround$Correlation))
                                           },
                                           cl = 1
    )) / length(backGround$Correlation)
    
    
    foreGround$pValues <- rep(NA, length(foreGround$Correlation))
    foreGround$pValues[which(foreGround$Correlation > 0)] <- 1 - greatList
    foreGround$pValues[which(foreGround$Correlation < 0)] <- 1 - lesserList
  }
  
  
  if (returnBackGround) {
    return(list("Foreground" = foreGround, "Background" = backGround))
  } else {
    return(foreGround)
  }
}


#' @title \code{runCoAccessibility}
#'
#' @description \code{runCoAccessibility}
#'
#' @param SampleTileObj The SummarizedExperiment object output from
#'   getSampleTileMatrix containing your sample-tile matrices
#' @param accMat accessibility matrix to use for correlations
#' @param pairs data.frame for pairs of tiles to test. Must be tileNames
#'   (chrX:100-200).
#' @param numCores Optional, the number of cores to use with multiprocessing.
#'   Default is 1.
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#' @param ZI boolean flag that enables zero-inflated (ZI) Spearman correlations
#'   to be used. Default is TRUE. If FALSE, skip zero-inflation and calculate
#'   the normal Spearman.
#'
#' @return zi_spear_mat_tmp a data.table of tile pairs with associated
#'   correlations.
#'
#' @examples
#' runCoAccessibility(
#'   assays(SampleTileObj)[[1]],
#'   pairs = data.frame(
#'     Tile1 = c("chrX:1000-1499", "chr21:1000-1499"),
#'     Tile2 = c("chrX:1500-1999", "chr21:1500-1999")
#'   )
#' )
#' @noRd
#'
runCoAccessibility <- function(accMat, pairs, ZI = TRUE, verbose = TRUE, numCores = 1) {

  # Generate matrix for just Tile1, and just Tile2, then combined by column.
  combinedMat <- cbind(accMat[pairs[, 1], ], accMat[pairs[, 2], ])
  matList <- as.list(as.data.frame(t(combinedMat)))
  # Remember how many samples you have, so you can split the list later
  if (verbose) {
    message("Data.frame wrangled for co-accessibility")
  }

  rm(combinedMat)
  rm(accMat)

  ## split combinedMat into blocks that represent the number of cores we are using.
  ## Use split command for that.
  ## Then test each pair, but only within each block.

  if (ZI) {
    correlation_tmp <- unlist(pbapply::pblapply(
      X = matList,
      FUN = ZISpearman,
      cl = numCores
    ))
  } else {
    correlation_tmp <- unlist(pbapply::pblapply(
      X = matList,
      FUN = Spearman,
      cl = numCores
    ))
  }

  # Create zero-inflated correlation matrix from correlation values
  zi_spear_mat_tmp <- data.table::data.table(
    Correlation = correlation_tmp,
    Tile1 = pairs[, 1],
    Tile2 = pairs[, 2]
  )

  return(zi_spear_mat_tmp)
}
