#' @title Run Zero-inflated Generalized Linear Mixed Modeling on pseudobulked
#'   scATAC data
#'
#' @description 
#'   `r lifecycle::badge("deprecated")`
#'   This function is deprecated - improved modeling functions can be found in 
#'   the package "ChAI" at https://github.com/aifimmunology/ChAI
#'   \code{runZIGLMM} Runs linear mixed-effects modeling for
#'   zero-inflated data using \code{\link[glmmTMB]{glmmTMB}}.
#'
#' @param TSAM_Object A SummarizedExperiment object generated from
#'   getSampleTileMatrix.
#' @param cellPopulation Name of a cell type(s), or 'all'. The function will
#'   combine the cell types mentioned into one matrix before running the model.
#' @param continuousFormula The formula for the continuous data that should be
#'   used within glmmTMB. It should be in the format (exp ~ factors). All
#'   factors must be found in column names of the TSAM_Object metadata, except
#'   for CellType, FragNumber and CellCount, which will be extracted from the
#'   TSAM_Object. modelFormula must start with 'exp' as the response. See
#'   \link[glmmTMB]{glmmTMB}.
#' @param ziformula The formula for the zero-inflated data that should be used
#'   within glmmTMB. It should be in the format ( ~ factors). All factors must
#'   be found in column names of the TSAM_Object colData metadata, except for
#'   CellType, FragNumber and CellCount, which will be extracted from the
#'   TSAM_Object.
#' @param zi_threshold Zero-inflated threshold ( range = 0-1), representing the
#'   fraction of samples with zeros. When the percentage of zeros in the tile is
#'   between 0 and zi_threshold, samples with zeroes are dropped and only the
#'   continous formula is used. Use this parameter at your own risk. Default is
#'   0.
#' @param initialSampling Size of data to use for pilot
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#' @param numCores integer. Number of cores to parallelize across.
#'
#' @return results a SummarizedExperiment containing LMEM results
#'
#'
#'
#' @examples
#' \dontrun{
#' modelList <- runZIGLMM(STM[c(1:1000), ],
#'   cellPopulation = "CD16 Mono",
#'   continuousFormula = exp ~ Age + Sex + days_since_symptoms + (1 | PTID),
#'   ziformula = ~ FragNumber + Age,
#'   verbose = TRUE,
#'   numCores = 35
#' )
#' }
#'
#' @export
#'
runZIGLMM <- function(TSAM_Object,
                      cellPopulation = "all",
                      continuousFormula = NULL,
                      ziformula = NULL,
                      zi_threshold = 0,
                      initialSampling = 5,
                      verbose = FALSE,
                      numCores = 1) {
  
  lifecycle::deprecate_warn(
    when="1.1.0", 
    what="runZIGLMM()", 
    details = "Please use improved modeling functions in the package `ChAI` at https://github.com/aifimmunology/ChAI"
  )
  
  Sample <- NULL
  if (any(c(class(continuousFormula), class(ziformula)) != "formula")) {
    stop("continuousFormula and/or ziformula was not provided as a formula.")
  }

  if (zi_threshold < 0 | zi_threshold > 1 | !is.numeric(zi_threshold)) {
    stop("zi_threshold must be between 0 and 1.")
  }

  if (is.null(cellPopulation)) {
    stop("No cell type name was provided.")
  } else if (all(tolower(cellPopulation) == "all")) {

    # Merge all together.
    newObj <- combineSampleTileMatrix(TSAM_Object)
  } else if (all(cellPopulation %in% names(SummarizedExperiment::assays(TSAM_Object)))) {

    # Subset down to just those
    newObj <- combineSampleTileMatrix(subsetMOCHAObject(TSAM_Object, subsetBy = "celltype", groupList = cellPopulation, subsetPeaks = TRUE))
  } else {
    stop("Error around cell type name. Some or all were not found within TSAM_Object.")
  }

  modelingData <- log2(SummarizedExperiment::assays(newObj)[["counts"]] + 1)
  MetaDF <- as.data.frame(SummarizedExperiment::colData(newObj))

  if (!all(all.vars(continuousFormula) %in% c("exp", colnames(MetaDF)))) {
    stop("Model formula is not in the correct format (exp ~ factors) or model factors are not found in column names of metadata within the TSAM_Object.")
  }

  if (!all(all.vars(ziformula) %in% colnames(MetaDF))) {
    stop("factors from the ziformula were not found in the metadata.")
  }

  variableList <- c(all.vars(continuousFormula)[all.vars(continuousFormula) != "exp"], all.vars(ziformula))

  MetaDF <- dplyr::filter(MetaDF, Sample %in% colnames(modelingData))
  modelingData <- modelingData[, match(colnames(modelingData), MetaDF$Sample)]


  # Subset metadata to just the variables needed. This minimizes overhead for parallelization
  MetaDF <- MetaDF[, colnames(MetaDF) %in% c("Sample", variableList)]


  if (verbose) {
    message("Running a quick test.")
  }


  # Generate pilot data for the null data.frame
  pilotIndices <- sample(x = 1:dim(modelingData)[1], size = initialSampling, replace = FALSE)
  modelList <- pbapply::pblapply(X = pilotIndices, function(x) {
    df <- data.frame(
      exp = as.numeric(modelingData[x, ]),
      MetaDF, stringsAsFactors = FALSE
    )

    tryCatch(
      {
        glmmTMB::glmmTMB(continuousFormula,
          ziformula = ziformula,
          data = df,
          family = stats::gaussian(),
          REML = TRUE
        )
      },
      error = function(e) {
        NA
      }
    )
  }, cl = NULL)

  if (all(is.na(unlist(modelList)))) {
    stop("For the initial sampling, every test model failed. Reconsider modelFormula or increase initial sampling size.")
  } else {
    idx <- which(!is.na(unlist(modelList)))
    coeffList <- lapply(1:length(idx), function(x) {
      tryCatch(
        {
          summary(modelList[[x]])$coefficients
        },
        error = function(e) {
          NA
        }
      )
    })
    if (all(is.na(unlist(coeffList)))) {
      stop("For the initial sampling, every test model failed. Reconsider modelFormula or increase initial sampling size.")
    }
    coeff2 <- coeffList[!is.na(unlist(coeffList))]
    nullDF <- lapply(coeff2[[1]], as.data.frame)
    nullDF$cond[!is.na(nullDF$cond)] <- NA
    nullDF$zi[!is.na(nullDF$zi)] <- NA
    rm(modelList)
  }

  if (verbose) {
    message("Modeling results.")
  }


  # Make your clusters for efficient parallelization
  cl <- parallel::makeCluster(numCores)
  iterList <- lapply(rownames(modelingData), function(x) {
    list(
      x, continuousFormula, modelingData, MetaDF, nullDF,
      zi_threshold, ziformula
    )
  })
  parallel::clusterExport(
    cl = cl, varlist = c(iterList),
    envir = environment()
  )
  parallel::clusterEvalQ(cl, {
    library(glmmTMB)
  })

  coeffList <- pbapply::pblapply(
    cl = cl,
    X = iterList,
    individualZIGLMM
  )
  parallel::stopCluster(cl)

  output_list <- lapply(list("cond", "zi"), function(varType) {
    if (verbose) {
      message(stringr::str_interp("Extracting coefficients for the ${varType} component"))
    }
    valuesToExtract <- c("Estimate", "Pr(>|z|)", "Std. Error")

    dfList <- lapply(valuesToExtract, function(valType) {
      tmpDF <- do.call("rbind", pbapply::pblapply(X = coeffList, function(x) {
        extractVariable(x, varType, valType, nullDF)
      }, cl = NULL))
      if (!is.null(tmpDF)) {
        rownames(tmpDF) <- rownames(modelingData)
      }
      tmpDF
    })

    names(dfList) <- c("Slopes", "Significance", "StdError")
    dfList
  })

  names(output_list) <- c("cond", "zi")

  combinedList <- lapply(c("Slopes", "Significance", "StdError"), function(x) {
    cond1 <- output_list[["cond"]][[x]]
    zi1 <- output_list[["zi"]][[x]]

    colnames(zi1) <- paste("ZI_", colnames(zi1), sep = "")
    cbind(cond1, zi1)
  })

  names(combinedList) <- c("Slopes", "Significance", "StdError")

  newMetadata <- newObj@metadata
  newMetadata$History <- append(newMetadata$History, paste("runZIGLMM", utils::packageVersion("MOCHA")))

  results <- SummarizedExperiment::SummarizedExperiment(
    combinedList,
    rowRanges = SummarizedExperiment::rowRanges(newObj),
    metadata = newMetadata
  )

  return(results)
}


extractVariable <- function(varList, varType, variable, nullDF) {
  varDF <- varList[[varType]]
  if (dim(varDF)[2] > 0) {
    val_tmp <- unlist(varDF[, variable])
    names(val_tmp) <- rownames(varDF)
    val_tmp
  } else {
    varDF <- nullDF[[varType]]
    val_tmp <- unlist(varDF[, variable])
    names(val_tmp) <- rownames(varDF)
    val_tmp
  }
}

#' @title \code{IndividualZIGLMM}
#'
#' @description 
#'   `r lifecycle::badge("deprecated")`
#'   This function is deprecated - improved modeling functions can be found in 
#'   the package "ChAI" at https://github.com/aifimmunology/ChAI
#'   \code{IndividualZIGLMM} Runs zero-inflated linear modeling on
#'   data provided. Written for efficient parallelization.
#'
#' @param iterList A list where the first index is a data.frame to use for
#'   modeling, and the second is the formula for modeling.
#'
#' @return A linear model
#' @noRd
#'
individualZIGLMM <- function(iterList) {

  lifecycle::deprecate_soft(
    when="1.1.0", 
    what="individualZIGLMM()", 
    details = "Please use improved modeling functions in the package `ChAI` at https://github.com/aifimmunology/ChAI"
  )

  x <- iterList[[1]]
  continuousFormula <- iterList[[2]]
  modelingData <- iterList[[3]]
  MetaDF <- iterList[[4]]
  nullDF <- iterList[[5]]
  zi_threshold <- iterList[[6]]
  ziformula <- iterList[[7]]

  df <- data.frame(
    exp = as.numeric(modelingData[x, ]),
    MetaDF, stringsAsFactors = FALSE
  )

  output_vector <- tryCatch(
    {
      if (sum(df$exp == 0) / length(df$exp) == 0) {
        modelRes <- glmmTMB::glmmTMB(continuousFormula,
          ziformula = ~0,
          data = df,
          family = stats::gaussian(),
          REML = TRUE
        )
      } else if (sum(df$exp == 0) / length(df$exp) <= zi_threshold) {
        df <- df[df$exp != 0, ]
        modelRes <- glmmTMB::glmmTMB(continuousFormula,
          ziformula = ~0,
          data = df,
          family = stats::gaussian(),
          REML = TRUE
        )
      } else {
        modelRes <- glmmTMB::glmmTMB(continuousFormula,
          ziformula = ziformula,
          data = df,
          family = stats::gaussian(),
          REML = TRUE
        )
      }

      lapply(summary(modelRes)$coefficients, as.data.frame)
    },
    error = function(e) {
      nullDF
    }
  )
  return(output_vector)
}



#' @title Execute a pilot run of model on a subset of data
#'
#' @description 
#'   `r lifecycle::badge("deprecated")`
#'   This function is deprecated - improved modeling functions can be found in 
#'   the package "ChAI" at https://github.com/aifimmunology/ChAI
#'   \code{pilotLMEM} Runs linear mixed-effects modeling for zero
#'   inflated data using \code{\link[glmmTMB]{glmmTMB}}. TryCatch will catch
#'   errors, and return the error and dataframe for troubleshooting.
#'
#' @param TSAM_Object A SummarizedExperiment object generated from
#'   getSampleTileMatrix, chromVAR, or other.
#' @param cellPopulation A single cell population on which to run this pilot
#'   model
#' @param continuousFormula The formula, see \code{\link[glmmTMB]{glmmTMB}}.
#'   Combined fixed and random effects formula, following lme4 syntax.
#' @param ziformula The zero-inflated formula, see
#'   \code{\link[glmmTMB]{glmmTMB}}. a one-sided (i.e., no response variable)
#'   formula for zero-inflation combining fixed and random effects: the default
#'   ~0 specifies no zero-inflation. Specifying ~. sets the zero-inflation
#'   formula identical to the right-hand side of formula (i.e., the conditional
#'   effects formula); terms can also be added or subtracted. When using ~. as
#'   the zero-inflation formula in models where the conditional effects formula
#'   contains an offset term, the offset term will automatically be dropped. The
#'   zero-inflation model uses a logit link.
#' @param zi_threshold Zero-inflated threshold (range = 0-1), representing the
#'   fraction of samples with zeros. When the percentage of zeros in the tile is
#'   between 0 and zi_threshold, samples with zeroes are dropped and only the
#'   continous formula is used. Use this parameter at your own risk. Default is
#'   0.
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#' @param pilotIndices A vector of integers defining the subset of the
#'   ExperimentObj matrix. Default is 1:10.
#'
#' @return modelList a list of outputs from glmmTMB::glmmTMB
#'
#'
#' @export
pilotZIGLMM <- function(TSAM_Object,
                        cellPopulation = NULL,
                        continuousFormula = NULL,
                        ziformula = NULL,
                        zi_threshold = 0,
                        verbose = FALSE,
                        pilotIndices = 1:10) {
  lifecycle::deprecate_warn(
    when="1.1.0", 
    what="pilotZIGLMM()", 
    details = "Please use improved modeling functions in the package `ChAI` at https://github.com/aifimmunology/ChAI"
  )
  Sample <- NULL
  if (any(c(class(continuousFormula), class(ziformula)) != "formula")) {
    stop("continuousFormula and/or ziformula was not provided as a formula.")
  }

  if (zi_threshold < 0 | zi_threshold > 1 | !is.numeric(zi_threshold)) {
    stop("zi_threshold must be between 0 and 1.")
  }

  if (is.null(cellPopulation)) {
    stop("No cell type name was provided.")
  } else if (all(tolower(cellPopulation) == "all")) {

    # Merge all together.
    newObj <- combineSampleTileMatrix(TSAM_Object)
  } else if (all(cellPopulation %in% names(SummarizedExperiment::assays(TSAM_Object)))) {

    # Subset down to just those
    newObj <- combineSampleTileMatrix(subsetMOCHAObject(TSAM_Object, subsetBy = "celltype", groupList = cellPopulation, subsetPeaks = TRUE))
  } else {
    stop("Error around cell type name. Some or all were not found within TSAM_Object.")
  }


  modelingData <- log2(SummarizedExperiment::assays(newObj)[["counts"]] + 1)
  MetaDF <- as.data.frame(SummarizedExperiment::colData(newObj))

  if (!all(all.vars(continuousFormula) %in% c("exp", colnames(MetaDF), "FragNumber", "CellCount", "CellType"))) {
    stop("Model formula is not in the correct format (exp ~ factors) or model factors are not found in column names of metadata within the TSAM_Object.")
  }

  if (!all(all.vars(ziformula) %in% c(colnames(MetaDF), "FragNumber", "CellCount", "CellType")) & length(all.vars(ziformula)) > 0) {
    stop("factors from the ziformula were not found in the metadata.")
  }

  variableList <- c(all.vars(continuousFormula)[all.vars(continuousFormula) != "exp"], all.vars(ziformula))

  MetaDF <- dplyr::filter(MetaDF, Sample %in% colnames(modelingData))
  modelingData <- modelingData[pilotIndices, match(colnames(modelingData), MetaDF$Sample)]

  # Subset metadata to just the variables needed. This minimizes overhead for parallelization
  MetaDF <- MetaDF[, colnames(MetaDF) %in% c("Sample", variableList)]

  if (verbose) {
    message("Modeling results.")
  }

  # Make your clusters for efficient parallelization

  modelList <- pbapply::pblapply(X = pilotIndices, function(x) {
    df <- data.frame(
      exp = as.numeric(modelingData[x, ]),
      MetaDF, stringsAsFactors = FALSE
    )

    # tryCatch to catch error, return dataframe for troubleshooting
    tryCatch(
      {
        if (sum(df$exp == 0) / length(df$exp) == 0) {
          modelRes <- glmmTMB::glmmTMB(continuousFormula,
            ziformula = ~0,
            data = df,
            family = stats::gaussian(),
            REML = TRUE
          )
        } else if (sum(df$exp == 0) / length(df$exp) <= zi_threshold) {
          df <- df[df$exp != 0, ]
          modelRes <- glmmTMB::glmmTMB(continuousFormula,
            ziformula = ~0,
            data = df,
            family = stats::gaussian(),
            REML = TRUE
          )
        } else {
          modelRes <- glmmTMB::glmmTMB(continuousFormula,
            ziformula = ziformula,
            data = df,
            family = stats::gaussian(),
            REML = TRUE
          )
        }
      },
      error = function(e) {
        warning("Hit modeling error.")
        list(e, rownames(modelingData)[x], df)
      }
    )
  }, cl = NULL)

  return(modelList)
}


#' @title getModelValues from runZIGLMM output.
#'
#' @description 
#'   `r lifecycle::badge("deprecated")`
#'   This function is deprecated - improved modeling functions can be found in 
#'   the package "ChAI" at https://github.com/aifimmunology/ChAI
#'   \code{getModelValues} Pull out a data.frame of model values (slope, significance, and std.error) for a given factor from the SummarizedExperiment output of runZIGLMM.
#' @param object A SummarizedExperiment object generated from runZIGLMM.
#' @param specificVariable A string, describing the factor of influence.
#'
#' @return A data.frame of slopes, significance, and standard error for one factor.
#'
#'
#'
#' @examples
#' \dontrun{
#' age_df <- getModelValues(runZIGLMM_output, "Age")
#' }
#'
#' @export
#'
getModelValues <- function(object, specificVariable) {
  lifecycle::deprecate_warn(
    when="1.1.0", 
    what="getModelValues()", 
    details = "Please use improved modeling functions in the package `ChAI` at https://github.com/aifimmunology/ChAI"
  )
  slopes <- SummarizedExperiment::assays(object)[["Slopes"]]
  significance <- SummarizedExperiment::assays(object)[["Significance"]]
  if (length(specificVariable) > 1) {
    stop("Cannot provide more than one value to specificVariable.")
  }

  df <- data.frame(
    "Element" = rownames(slopes),
    "Estimate" = slopes[, specificVariable],
    "PValue" = significance[, specificVariable]
  )
  return(df)
}
