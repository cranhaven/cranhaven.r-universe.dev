#' @title Run Linear Mixed-Effects Modeling for continuous, non-zero inflated
#'   data
#'
#' @description \code{runLMEM} Runs linear mixed-effects modeling for
#'   continuous, non-zero inflated data using \code{\link[lmerTest]{lmer}}
#'
#' @param ExperimentObj A SummarizedExperiment object generated from
#'   getSampleTileMatrix, chromVAR, or other. It is expected to contain only one
#'   assay, or only the first assay will be used for the model. Data should not
#'   be zero-inflated.
#' @param assayName The name of the assay to model within the
#'   SummarizedExperiment.
#' @param modelFormula The formula to use with lmerTest::lmer, in the format
#'   (exp ~ factors). All factors must be found in column names of the
#'   ExperimentObj metadata. modelFormula must start with 'exp' as the response.
#'   See \link[lmerTest]{lmer}.
#' @param initialSampling Size of data to use for pilot
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#' @param numCores integer. Number of cores to parallelize across.
#'
#' @return results a SummarizedExperiment containing LMEM results. Assays are
#'   metrics related to the model coefficients, including the Estimate,
#'   Std_Error, df, t_value, p_value. Within each assay, each row corresponds to
#'   each row of the SummarizedExperiment and columns correspond to each fixed
#'   effect variable within the model. Any row metadata from the
#'   ExperimentObject (see rowData(ExperimentObj)) is preserved in the output.
#'   The Residual matrix and the variance of the random effects are saved in the
#'   metadata slot of the output.
#'
#'
#'
#' @examples
#' \dontrun{
#' modelList <- runLMEM(ExperimentObj,
#'   assayName = names(ExperimentObj)[[1]]
#'   modelFormula = NULL,
#'   initialSampling = 5,
#'   verbose = FALSE,
#'   numCores = 1
#' )
#' }
#'
#' @export
runLMEM <- function(ExperimentObj,
                    assayName,
                    modelFormula,
                    initialSampling = 5,
                    verbose = FALSE,
                    numCores = 2) {
  lifecycle::deprecate_warn(
    when="1.1.0", 
    what="runLMEM()", 
    details = "Please use improved modeling functions in the package `ChAI` at https://github.com/aifimmunology/ChAI"
  )
  Sample <- NULL

  if (!requireNamespace("lmerTest", quietly = TRUE)) {
    stop(
      "Package 'lmerTest' is required for runLMEM ",
      "Please install 'lmerTest' to proceed."
    )
  }
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop(
      "Package 'lme4' is required for runLMEM. ",
      "Please install 'lme4' to proceed."
    )
  }

  if (!any(names(SummarizedExperiment::assays(ExperimentObj)) %in% assayName)) {
    stop("ExperimentObj does not contain an assay that matches the assayName input variable.")
  }

  modelingData <- as.data.frame(
    SummarizedExperiment::assays(ExperimentObj)[[assayName]]
  )
  MetaDF <- as.data.frame(SummarizedExperiment::colData(ExperimentObj))


  if (!methods::is(modelFormula, "character")) {
    stop(
      "modelFormula is not a  string. modelFormula must be a character string ",
      "describing a formula in the format (exp ~ factors)"
    )
  } else if (!methods::is(modelFormula, "formula")) {
    modelFormula <- as.character(modelFormula)
  }

  if (!"exp" %in% all.vars(stats::formula(paste(modelFormula, collapse = " ")))) {
    stop(
      "modelFormula is not in the format (exp ~ factors). ",
      "modelFormula must start with 'exp' as the response."
    )
  }

  if (
    !all(all.vars(stats::formula(paste(modelFormula, collapse = " "))) %in% c("~", "exp", colnames(MetaDF)))
  ) {
    stop(
      "Model factors are not found ",
      "in the 'colData' of the ExperimentObj, or ",
      "modelFormula is not in the format ",
      "(exp ~ factors)."
    )
  }
  variableList <- all.vars(stats::formula(paste(modelFormula, collapse = " ")))[all.vars(stats::formula(paste(modelFormula, collapse = " "))) != "exp"]

  MetaDF <- dplyr::filter(MetaDF, Sample %in% colnames(modelingData))
  modelingData <- modelingData[
    , match(colnames(modelingData), MetaDF$Sample),
    drop = FALSE
  ]

  # Subset metadata to just the variables in modelFormula
  MetaDF <- MetaDF[, colnames(MetaDF) %in% c("Sample", variableList)]

  if (verbose) {
    message("Running an initial model on sample of data.")
  }

  # Generate pilot data for the null data.frame
  pilotIndices <- sample(
    x = seq_len(dim(modelingData)[1]),
    size = initialSampling,
    replace = FALSE
  )
  modelList <- pbapply::pblapply(X = pilotIndices, function(x) {
    df <- data.frame(
      exp = as.numeric(modelingData[x, ]),
      MetaDF, stringsAsFactors = FALSE
    )

    tryCatch(
      {
        lmerTest::lmer(formula = stats::formula(paste(modelFormula, collapse = " ")), data = df)
      },
      error = function(e) {
        NA
      }
    )
  }, cl = NULL)
  # Initial sampling: When the model fails to converge, we
  # return a data.frame with NAs that is the same size.
  if (all(is.na(unlist(modelList)))) {
    stop(
      "For the initial sampling, every test model failed to converge. ",
      "Reconsider modelFormula or increase 'initialSampling'."
    )
  } else {
    idx <- which(!is.na(unlist(modelList)))
    nullDF <- as.data.frame(summary(modelList[[idx[1]]])$coefficients)

    nullDF[!is.na(nullDF)] <- NA
    nullResidual <- stats::resid(modelList[[idx[1]]])
    nullResidual[!is.na(nullResidual)] <- NA

    # Add in missing residuals
    if (!all(MetaDF$Sample %in% names(nullResidual))) {
      NA_samples <- rep(NA, sum(!MetaDF$Sample %in% names(nullResidual)))
      names(NA_samples) <- MetaDF$Sample[!MetaDF$Sample %in% names(nullResidual)]
      nullResidual <- c(nullResidual, NA_samples)
    }
    nullResidual <- nullResidual[match(names(nullResidual), MetaDF$Sample)]
    nullVcov <- as.data.frame(lme4::VarCorr(modelList[[idx[1]]]))$vcov
    names(nullVcov) <- as.data.frame(lme4::VarCorr(modelList[[idx[1]]]))$grp
    nullVcov[!is.na(nullVcov)] <- NA

    nullDFList <- list("Coeff" = nullDF, "Resid" = nullResidual, "VCov" = nullVcov)

    rm(modelList)
    # Why do we make and then export to cluster this nullDFList if it is not used?
    # It's used as a dummy variable in cases where the model fails. NAs are returned from individualLMEM instead of the function breaking.
  }
  
  # Nested internal helper function used in parallelization
  individualLMEM <- function(iterList) {
    x <- iterList[[1]]
    modelFormula <- iterList[[2]]
    modelingData <- iterList[[3]]
    MetaDF <- iterList[[4]]
    nullDF <- iterList[[5]]
    
    df <- data.frame(
      exp = as.numeric(modelingData[x, ]),
      MetaDF, stringsAsFactors = FALSE
    )
    
    output_vector <- tryCatch(
      {
        modelRes <- lmerTest::lmer(formula = as.character(modelFormula), data = df)
        Coeff <- as.data.frame(summary(modelRes)$coefficients)
        Resid <- stats::resid(modelRes)
        
        if (!all(MetaDF$Sample %in% names(Resid))) {
          NA_samples <- rep(NA, sum(!MetaDF$Sample %in% names(Resid)))
          names(NA_samples) <- MetaDF$Sample[!MetaDF$Sample %in% names(Resid)]
          Resid <- c(Resid, NA_samples)
        }
        Resid <- Resid[match(names(Resid), MetaDF$Sample)]
        vcov <- as.data.frame(lme4::VarCorr(modelRes))$vcov
        names(vcov) <- as.data.frame(lme4::VarCorr(modelRes))$grp
        list("Coeff" = Coeff, "Resid" = Resid, "VCov" = vcov)
      },
      error = function(e) {
        nullDFList # is not defined here
      }
    )
    return(output_vector)
  }
  
  if (numCores > 1) {
    cl <- parallel::makeCluster(numCores)
    parallel::clusterExport(
      cl = cl, varlist = c(
        "modelFormula", "modelingData",
        "MetaDF", "individualLMEM", "nullDFList"
      ),
      envir = environment()
    )
    
    parallel::clusterEvalQ(cl, {
      library(lmerTest)
    })
    coeffList <- pbapply::pblapply(
      cl = cl,
      X = rownames(modelingData),
      individualLMEM
    )
    parallel::stopCluster(cl)
  } else {
    coeffList <- pbapply::pblapply(
      cl = NULL,
      X = rownames(modelingData),
      individualLMEM
    )
  }


  if (verbose) {
    message("Reorganizing coefficients.")
  }

  processedOuts <- processModelOutputs(
    modelOutputList = coeffList,
    nullDFList = nullDFList,
    rownamesList = rownames(modelingData),
    SummarizedExperimentObj = ExperimentObj
  )

  return(processedOuts)
}


# #' @title Internal function to run linear modeling
# #'
# #' @description \code{IndividualLMEM} Runs linear modeling
# #'   with lmerTest::lmer
# #' @param iterList A list where the first index is a data.frame
# #'   to use for modeling, and the second is the formula for modeling.
# #'   list(x, modelFormula, modelingData, MetaDF, nullDF)
# #'
# #' @return output_vector A linear model
# #'
# #' @noRd
# individualLMEM <- function(iterList) {
#   x <- iterList[[1]]
#   modelFormula <- iterList[[2]]
#   modelingData <- iterList[[3]]
#   MetaDF <- iterList[[4]]
#   nullDF <- iterList[[5]]
# 
#   df <- data.frame(
#     exp = as.numeric(modelingData[x, ]),
#     MetaDF, stringsAsFactors = FALSE
#   )
# 
#   output_vector <- tryCatch(
#     {
#       modelRes <- lmerTest::lmer(formula = as.character(modelFormula), data = df)
#       Coeff <- as.data.frame(summary(modelRes)$coefficients)
#       Resid <- stats::resid(modelRes)
# 
#       if (!all(MetaDF$Sample %in% names(Resid))) {
#         NA_samples <- rep(NA, sum(!MetaDF$Sample %in% names(Resid)))
#         names(NA_samples) <- MetaDF$Sample[!MetaDF$Sample %in% names(Resid)]
#         Resid <- c(Resid, NA_samples)
#       }
#       Resid <- Resid[match(names(Resid), MetaDF$Sample)]
#       vcov <- as.data.frame(lme4::VarCorr(modelRes))$vcov
#       names(vcov) <- as.data.frame(lme4::VarCorr(modelRes))$grp
#       list("Coeff" = Coeff, "Resid" = Resid, "VCov" = vcov)
#     },
#     error = function(e) {
#       nullDFList # is not defined here
#     }
#   )
#   return(output_vector)
# }

#' @title Internal function to processing model outputs
#'
#' @description 
#'  `r lifecycle::badge("deprecated")`
#'   This function is deprecated - improved modeling functions can be found in 
#'   the package "ChAI" at https://github.com/aifimmunology/ChAI
#'   \code{processModelOutputs}
#' @param modelOutputList. A list of modeloutputs, processed by either individualLMEM or individualZIGLMM.
#'        The first output is the coefficient data.frame, then the residuals, and the Variance.
#' @param nullDFList A null templates for the model outputs
#' @param rownamesList a name of all the rows that were interate over.
#' @param SummarizedExperiment SummarizedExperiment object used for modeling. From this, rowData and colData will be preserved with the residuals.
#' @return A SummarizedExperiment object, that captures the models performance. Each fixed effect will be one assay, columns will be the
#'        statistics for the fixed effect and measurements (Estimate, Error, p-value, etc..). Residuals and Variance will be saved in the object's metadata.
#'
#' @noRd
processModelOutputs <- function(modelOutputList, nullDFList, rownamesList, ranged = FALSE,
                                SummarizedExperimentObj, returnList = FALSE) {
  lifecycle::deprecate_warn(
    when="1.1.0", 
    what="processModelOutputs()", 
    details = "Please use improved modeling functions in the package `ChAI` at https://github.com/aifimmunology/ChAI"
  )
  coeffNames <- rownames(nullDFList$Coeff)
  newColumnNames <- gsub("Pr\\(>\\|.\\|)", "p_value", gsub(" |\\. ", "_", colnames(nullDFList$Coeff)))
  output_list <- lapply(coeffNames, function(z) {
    tmpCoef <- do.call("rbind", pbapply::pblapply(X = modelOutputList, function(x) {
      tmpDf <- x[["Coeff"]][z, ]
      colnames(tmpDf) <- newColumnNames
      tmpDf$FDR <- stats::p.adjust(tmpDf$p_value, "fdr")
      tmpDf
    }, cl = NULL))
    rownames(tmpCoef) <- rownamesList
    tmpCoef
  })
  names(output_list) <- gsub("Pr\\(>\\|.\\|)", "p_value", gsub(" |\\. ", "_", coeffNames))

  residual_tmp <- do.call(
    "rbind", pbapply::pblapply(X = modelOutputList, function(x) {
      x[["Resid"]]
    }, cl = NULL)
  )
  vcov_tmp <- do.call(
    "rbind", pbapply::pblapply(X = modelOutputList, function(x) {
      x[["VCov"]]
    }, cl = NULL)
  )
  rownames(residual_tmp) <- rownames(vcov_tmp) <- rownamesList

  residual_tmp <- residual_tmp[, match(rownames(SummarizedExperiment::colData(SummarizedExperimentObj)), colnames(residual_tmp))]

  if (returnList) {
    return(list("output" = output_list, "Resid" = residual_tmp, "Variance" = vcov_tmp))
  }
  # Repackage Residuals into a SummarizedExperiment
  if (ranged) {
    ResidualSE <- SummarizedExperiment::SummarizedExperiment(
      list("Residual" = residual_tmp),
      colData = SummarizedExperiment::colData(SummarizedExperimentObj),
      rowRanges = SummarizedExperiment::rowRanges(SummarizedExperimentObj),
      metadata = S4Vectors::metadata(SummarizedExperimentObj)
    )
  } else {
    ResidualSE <- SummarizedExperiment::SummarizedExperiment(
      list("Residual" = residual_tmp),
      colData = SummarizedExperiment::colData(SummarizedExperimentObj),
      rowData = SummarizedExperiment::rowData(SummarizedExperimentObj),
      metadata = S4Vectors::metadata(SummarizedExperimentObj)
    )
  }


  # Package up the metadata list.
  metaDataList <- list(
    "Residuals" = ResidualSE,
    "RandomEffectVariance" = vcov_tmp
  )

  results <- SummarizedExperiment::SummarizedExperiment(
    output_list,
    rowData = SummarizedExperiment::rowData(SummarizedExperimentObj),
    metadata = metaDataList
  )

  return(results)
}

#' @title Execute a pilot run of single linear model on a subset of data
#'
#' @description
#'   `r lifecycle::badge("deprecated")`
#'   This function is deprecated - improved modeling functions can be found in 
#'   the package "ChAI" at https://github.com/aifimmunology/ChAI
#'   \code{pilotLMEM} Runs linear mixed-effects modeling for
#'   continuous, non-zero inflated data using \code{\link[lmerTest]{lmer}}
#'
#' @param ExperimentObj A SummarizedExperiment-type object generated from
#'   chromVAR, makePseudobulkRNA, or other. Objects from getSampleTileMatrix can work,
#'   but we recommend runZIGLMM for those objects, not runLMEM>
#' @param assayName a character string, matching the name of an assay within the SummarizedExperiment.
#'   The assay named will be used for modeling.
#' @param modelFormula The formula to use with lmerTest::lmer, in the
#'   format (exp ~ factors). All factors must be found in column names
#'   of the ExperimentObj metadata.
#' @param pilotIndices A vector of integers defining the subset of
#'   the ExperimentObj matrix. Default is 1:10.
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#'
#' @return modelList a list of outputs from lmerTest::lmer
#'
#'
#' @export
pilotLMEM <- function(ExperimentObj,
                      assayName,
                      modelFormula,
                      pilotIndices = 1:10,
                      verbose = FALSE) {
  lifecycle::deprecate_warn(
    when="1.1.0", 
    what="pilotLMEM()", 
    details = "Please use improved modeling functions in the package `ChAI` at https://github.com/aifimmunology/ChAI"
  )
  Sample <- NULL
  if (length(assayName) > 1) {
    stop(
      "More than one assay was provided. ",
      "assayName must be length 1. To run over multiple assays/cell types, ",
      "combine the matrices into one and wrap them in a SummarizedExperiment. Then set assayName to ",
      "the name of that matrix in the obejct."
    )
  } else if (
    !assayName %in% names(SummarizedExperiment::assays(ExperimentObj))
  ) {
    stop("assayName was not found within ExperimentObj.")
  }

  modelingData <- SummarizedExperiment::assays(ExperimentObj)[[assayName]]
  MetaDF <- as.data.frame(SummarizedExperiment::colData(ExperimentObj))

  if (!methods::is(modelFormula, "formula") & !methods::is(modelFormula, "character")) {
    stop(
      "modelFormula is not a formula or string. modelFormula must be a formula or character string in the format ",
      "(exp ~ factors)"
    )
  } else if (methods::is(modelFormula, "formula")) {
    modelFormula <- as.character(modelFormula)
  }

  if (!"exp" %in% all.vars(stats::formula(paste(modelFormula, collapse = " ")))) {
    stop(
      "modelFormula is not in the format (exp ~ factors). ",
      "modelFormula must start with 'exp' as the response."
    )
  }

  if (
    !all(all.vars(stats::formula(paste(modelFormula, collapse = " "))) %in% c("exp", colnames(MetaDF)))
  ) {
    stop(
      "Model factors are not found ",
      "in the 'colData' of the ExperimentObj, or ",
      "modelFormula is not in the format ",
      "(exp ~ factors)."
    )
  }

  variableList <- all.vars(stats::formula(paste(modelFormula, collapse = " ")))[all.vars(stats::formula(paste(modelFormula, collapse = " "))) != "exp"]

  MetaDF <- dplyr::filter(MetaDF, Sample %in% colnames(modelingData))
  modelingData <- modelingData[
    pilotIndices, match(colnames(modelingData), MetaDF$Sample),
    drop = FALSE
  ]

  # Subset metadata to just the variables in modelFormula
  MetaDF <- MetaDF[
    , colnames(MetaDF) %in% c("Sample", variableList),
    drop = FALSE
  ]

  modelList <- pbapply::pblapply(pilotIndices, function(x) {
    df <- data.frame(
      exp = as.numeric(modelingData[x, ]),
      MetaDF, stringsAsFactors = FALSE
    )

    lmerTest::lmer(formula = modelFormula, data = df)
  }, cl = NULL)

  return(modelList)
}
