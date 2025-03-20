#' @title Zero-inflated Variance Decomposition for pseudobulked scATAC data
#'
#' @description 
#'   `r lifecycle::badge("deprecated")`
#'   This function is deprecated - improved modeling functions can be found in 
#'   the package "ChAI" at https://github.com/aifimmunology/ChAI
#'   \code{varZIGLMM} Identified variance decomposition on a given
#'   cell type across both zero-inflated and continuous space using a
#'   zero-inflated general linear mixed model \code{\link[glmmTMB]{glmmTMB}}
#'
#' @param TSAM_Object A SummarizedExperiment object generated from
#'   getSampleTileMatrix.
#' @param cellPopulation Name of a cell type(s), or 'all'. The function will
#'   combine the cell types mentioned into one matrix before running the model.
#' @param continuousRandom Random effects to test in the continuous portion. All
#'   factors must be found in column names of the TSAM_Object metadata, except
#'   for FragNumber and CellCount, which will be extracted from the
#'   TSAM_Object's metadata.
#' @param ziRandom Random effects to test in the zero-inflated portion. All
#'   factors must be found in column names of the TSAM_Object colData metadata,
#'   except for FragNumber and CellCount, which will be extracted from the
#'   TSAM_Object's metadata.
#' @param zi_threshold Zero-inflated threshold ( range = 0-1), representing the
#'   fraction of samples with zeros. When the percentage of zeros in the tile is
#'   between 0 and zi_threshold, samples with zeroes are dropped and only the
#'   continous formula is used. Use this parameter at your own risk. Default is
#'   0.
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#' @param numCores integer. Number of cores to parallelize across.
#'
#' @return results a SummarizedExperiment containing results from ZIGLMM (Fixed
#'   effect estiamtes, P-values, and Std Error)
#'
#' @examples
#' \dontrun{
#' modelList <- runZIGLMM(STM[c(1:1000), ],
#'   cellPopulation = "CD16 Mono",
#'   continuousRandom = c("Age", "Sex", "Days"),
#'   ziRandom = c("FragNumber", "Days"),
#'   verbose = TRUE,
#'   numCores = 35
#' )
#' }
#'
#' @export
#' 
varZIGLMM <- function(TSAM_Object,
                      cellPopulation = NULL,
                      continuousRandom = NULL,
                      ziRandom = NULL,
                      zi_threshold = 0.1,
                      verbose = FALSE,
                      numCores = 1) {

  lifecycle::deprecate_warn(
    when="1.1.0", 
    what="varZIGLMM()", 
    details = "Please use improved modeling functions in the package `ChAI` at https://github.com/aifimmunology/ChAI"
  )
  Sample <- NULL
  
  if (length(cellPopulation) > 1) {
    stop(
      "More than one cell population was provided. ",
      "cellPopulation must be length 1. To run over multiple cell types, ",
      "run combineSampleTileMatrix() to produce a new combined TSAM_Object and set ",
      "cellPopulation = 'counts'."
    )
  } else if (
    (!cellPopulation %in% names(SummarizedExperiment::assays(TSAM_Object)))
  ) {
    stop("cellPopulation was not found within TSAM_Object.")
  } else if (cellPopulation == "counts") {
    newObj <- TSAM_Object
  } else {
    newObj <- combineSampleTileMatrix(subsetMOCHAObject(TSAM_Object, subsetBy = "celltype", groupList = cellPopulation, subsetPeaks = TRUE))
  }


  modelingData <- log2(SummarizedExperiment::assays(newObj)[["counts"]] + 1)
  MetaDF <- as.data.frame(SummarizedExperiment::colData(newObj))

  if (!all(continuousRandom %in% c("exp", colnames(MetaDF)))) {
    stop("Random continuous effects are not found in metadata.")
  }

  if (!all(ziRandom %in% colnames(MetaDF) | all(ziRandom == 0))) {
    stop("Random Zero-inflated effects are not found in metadata.")
  }

  CondvarForm <- paste0(unlist(lapply(continuousRandom, function(x) paste("(1|", x, ")", sep = ""))), collapse = " + ")
  continuousFormula <- paste("exp ~ ", CondvarForm, sep = "")

  if (all(ziRandom != 0)) {
    zi_form <- paste0(unlist(lapply(ziRandom, function(x) paste("(1|", x, ")", sep = ""))), collapse = " + ")
    variableList <- c(continuousRandom, ziRandom)
  } else {
    zi_form <- ziRandom
    variableList <- continuousRandom
  }
  ziformula <- paste("~ ", zi_form)

  MetaDF <- dplyr::filter(MetaDF, Sample %in% colnames(modelingData))
  modelingData <- modelingData[, match(colnames(modelingData), MetaDF$Sample)]

  # Subset metadata to just the variables needed. This minimizes overhead for parallelization
  MetaDF <- MetaDF[, colnames(MetaDF) %in% c("Sample", variableList)]

  if (any(is.na(MetaDF))) {
    stop("NAs are included in the MetaDF. Please remove them and try again.")
  }

  # Subset metadata to just the variables needed. This minimizes overhead for parallelization
  MetaDF <- MetaDF[, colnames(MetaDF) %in% c("Sample", variableList)]

  ## Log transform the FragmentNumbers so as to stabilize the model. But only if FragNumber is in the model. Same for CellCounts.
  if (any(colnames(MetaDF) %in% c("FragNumber"))) {
    MetaDF$rawFragNumber <- MetaDF$FragNumber
    MetaDF$FragNumber <- log10(MetaDF$FragNumber)
  }
  if (any(colnames(MetaDF) %in% c("CellCounts"))) {
    MetaDF$rawCellCounts <- MetaDF$CellCounts
    MetaDF$CellCounts <- log10(MetaDF$CellCounts)
  }



  # Generate null results.
  if (all(ziRandom != 0)) {
    nullDF <- rep(NA, length(continuousRandom) + length(ziRandom) + 1)
    names(nullDF) <- c(paste("Cond", continuousRandom, sep = "_"), paste("ZI", ziRandom, sep = "_"), "Residual")
  } else {
    nullDF <- rep(NA, length(continuousRandom) + 1)
    names(nullDF) <- c(paste("Cond", continuousRandom, sep = "_"), "Residual")
  }

  # Subset metadata to just the variables needed. This minimizes overhead for parallelization
  MetaDF <- MetaDF[, colnames(MetaDF) %in% c("Sample", variableList)]

  if (any(is.na(MetaDF))) {
    stop("NAs are included in the MetaDF. Please remove them and try again.")
  }
  
  # Nested internal helper function used in parallelization
  individualVarZIGLMM <- function(x) {
    df <- data.frame(
      exp = as.numeric(modelingData[x, ]),
      MetaDF, stringsAsFactors = FALSE
    )
    
    output_vector <- tryCatch(
      {
        if (all(df$exp != 0, na.rm = T)) {
          modelRes <- glmmTMB::glmmTMB(stats::as.formula(continuousFormula),
                                       ziformula = ~0,
                                       data = df,
                                       family = stats::gaussian(),
                                       REML = TRUE
          )
        } else if (sum(df$exp == 0, na.rm = T) / length(df$exp) < zi_threshold) {
          df$exp[df$exp == 0] <- NA
          modelRes <- glmmTMB::glmmTMB(stats::as.formula(continuousFormula),
                                       ziformula = ~0,
                                       data = df,
                                       family = stats::gaussian(),
                                       REML = TRUE
          )
        } else {
          modelRes <- glmmTMB::glmmTMB(stats::as.formula(continuousFormula),
                                       ziformula = stats::as.formula(ziformula),
                                       data = df,
                                       family = stats::gaussian(),
                                       REML = TRUE
          )
        }
        
        if (!modelRes$sdr$pdHess) {
          return(nullDF)
        }
        
        cond_other <- unlist(glmmTMB::VarCorr(modelRes)$cond)
        names(cond_other) <- paste("Cond", names(cond_other), sep = "_")
        residual <- as.vector(attr(glmmTMB::VarCorr(modelRes)$cond, "sc")^2)
        names(residual) <- "Residual"
        
        if (all(df$exp != 0, na.rm = T)) {
          subNull <- nullDF[grepl("ZI_", names(nullDF))]
          zi_other <- rep(0, length(subNull))
          names(zi_other) <- names(subNull)
          varcor_df <- c(cond_other, zi_other, residual)
        } else if (length(all.vars(stats::as.formula(ziformula))) != 0) {
          zi_other <- unlist(glmmTMB::VarCorr(modelRes)$zi)
          names(zi_other) <- paste("ZI", names(zi_other), sep = "_")
          varcor_df <- c(cond_other, zi_other, residual)
        } else {
          varcor_df <- c(cond_other, residual)
        }
        
        varDecomp <- varcor_df / sum(varcor_df)
        return(varDecomp)
      },
      error = function(e) {
        nullDF
      }
    )
    return(output_vector)
  }
  
  # Make your clusters for efficient parallelization
  if (numCores > 1) {
    cl <- parallel::makeCluster(numCores)
    parallel::clusterEvalQ(cl, {
      library(glmmTMB)
    })

    parallel::clusterExport(
      cl = cl, varlist = c("continuousFormula", "ziformula", "zi_threshold", "modelingData", "MetaDF", "individualVarZIGLMM", "nullDF"),
      envir = environment()
    )

    varDecompList <- pbapply::pblapply(cl = cl, X = rownames(modelingData), individualVarZIGLMM)
  } else {
    varDecompList <- pbapply::pblapply(cl = NULL, X = rownames(modelingData), function(x) {
      individualVarZIGLMM(x)
    })
  }


  if (!is.null(cl)) {
    parallel::stopCluster(cl)
  }

  results <- do.call("rbind", varDecompList)
  rownames(results) <- rownames(modelingData)

  return(results)
}


# #' @title \code{individualVarZIGLMM }
# #' 
# #' @description \code{individualVarZIGLMM } Runs ZI-GLMM on data provided and returns variance decomposition across random effects. Wirtten for parallelization.
# #' 
# #' @param refList. A list where the first index is a data.frame to use for modeling, and the second is the formula for modeling.
# #' 
# #' @return A linear model
# #' @noRd
# #' 
# individualVarZIGLMM <- function(x) {
#   df <- data.frame(
#     exp = as.numeric(modelingData[x, ]),
#     MetaDF, stringsAsFactors = FALSE
#   )
# 
# 
#   output_vector <- tryCatch(
#     {
#       if (all(df$exp != 0, na.rm = T)) {
#         modelRes <- glmmTMB::glmmTMB(stats::as.formula(continuousFormula),
#           ziformula = ~0,
#           data = df,
#           family = stats::gaussian(),
#           REML = TRUE
#         )
#       } else if (sum(df$exp == 0, na.rm = T) / length(df$exp) < zi_threshold) {
#         df$exp[df$exp == 0] <- NA
#         modelRes <- glmmTMB::glmmTMB(stats::as.formula(continuousFormula),
#           ziformula = ~0,
#           data = df,
#           family = stats::gaussian(),
#           REML = TRUE
#         )
#       } else {
#         modelRes <- glmmTMB::glmmTMB(stats::as.formula(continuousFormula),
#           ziformula = stats::as.formula(ziformula),
#           data = df,
#           family = stats::gaussian(),
#           REML = TRUE
#         )
#       }
# 
#       if (!modelRes$sdr$pdHess) {
#         return(nullDF)
#       }
# 
# 
#       cond_other <- unlist(glmmTMB::VarCorr(modelRes)$cond)
#       names(cond_other) <- paste("Cond", names(cond_other), sep = "_")
#       residual <- as.vector(attr(glmmTMB::VarCorr(modelRes)$cond, "sc")^2)
#       names(residual) <- "Residual"
# 
#       if (all(df$exp != 0, na.rm = T)) {
#         subNull <- nullDF[grepl("ZI_", names(nullDF))]
#         zi_other <- rep(0, length(subNull))
#         names(zi_other) <- names(subNull)
#         varcor_df <- c(cond_other, zi_other, residual)
#       } else if (length(all.vars(stats::as.formula(ziformula))) != 0) {
#         zi_other <- unlist(glmmTMB::VarCorr(modelRes)$zi)
#         names(zi_other) <- paste("ZI", names(zi_other), sep = "_")
#         varcor_df <- c(cond_other, zi_other, residual)
#       } else {
#         varcor_df <- c(cond_other, residual)
#       }
# 
#       varDecomp <- varcor_df / sum(varcor_df)
#       return(varDecomp)
#     },
#     error = function(e) {
#       nullDF
#     }
#   )
#   return(output_vector)
# }

