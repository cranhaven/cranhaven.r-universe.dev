# -------------------------------------------------------------------------------
#   This file is part of 'diversityForest'.
#
# 'diversityForest' is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# 'diversityForest' is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with 'diversityForest'. If not, see <http://www.gnu.org/licenses/>.
#
#  NOTE: 'diversityForest' is a fork of the popular R package 'ranger', written by Marvin N. Wright.
#  Most R and C++ code is identical with that of 'ranger'. The package 'diversityForest'
#  was written by taking the original 'ranger' code and making any
#  changes necessary to implement diversity forests.
#
# -------------------------------------------------------------------------------

##' Implements the most basic form of diversity forests that uses univariable, binary splitting.
##' Currently, categorical, metric, and survival outcomes are supported.
##' 
##' @title Construct a basic diversity forest prediction rule that uses univariable, binary splitting.
##' @param formula Object of class \code{formula} or \code{character} describing the model to fit. Interaction terms supported only for numerical variables.
##' @param data Training data of class \code{data.frame}, \code{matrix}, \code{dgCMatrix} (Matrix) or \code{gwaa.data} (GenABEL).
##' @param num.trees Number of trees. Default is 500.
##' @param mtry Artefact from 'ranger'. NOT needed for diversity forests. 
##' @param importance Variable importance mode, one of 'none', 'impurity', 'impurity_corrected', 'permutation'. The 'impurity' measure is the Gini index for classification, the variance of the responses for regression and the sum of test statistics (see \code{splitrule}) for survival. NOTE: Currently, only "permutation" (and "none") work for diversity forests.
##' @param write.forest Save \code{divfor.forest} object, required for prediction. Set to \code{FALSE} to reduce memory usage if no prediction intended.
##' @param probability Grow a probability forest as in Malley et al. (2012). NOTE: Not yet implemented for diversity forests!
##' @param min.node.size Minimal node size. Default 1 for classification, 5 for regression, 3 for survival, and 5 for probability.
##' @param max.depth Maximal tree depth. A value of NULL or 0 (the default) corresponds to unlimited depth, 1 to tree stumps (1 split per tree).
##' @param replace Sample with replacement. 
##' @param sample.fraction Fraction of observations to sample. Default is 1 for sampling with replacement and 0.632 for sampling without replacement. For classification, this can be a vector of class-specific values. 
##' @param case.weights Weights for sampling of training observations. Observations with larger weights will be selected with higher probability in the bootstrap (or subsampled) samples for the trees.
##' @param class.weights Weights for the outcome classes (in order of the factor levels) in the splitting rule (cost sensitive learning). Classification and probability prediction only. For classification the weights are also applied in the majority vote in terminal nodes.
##' @param splitrule Splitting rule. For classification and probability estimation "gini" or "extratrees" with default "gini". For regression "variance", "extratrees" or "maxstat" with default "variance". For survival "logrank", "extratrees", "C" or "maxstat" with default "logrank". NOTE: For diversity forests currently only the default splitting rules are supported.
##' @param num.random.splits Artefact from 'ranger'. NOT needed for diversity forests.
##' @param alpha For "maxstat" splitrule: Significance threshold to allow splitting. NOT needed for diversity forests.
##' @param minprop For "maxstat" splitrule: Lower quantile of covariate distribution to be considered for splitting. NOT needed for diversity forests.
##' @param split.select.weights Numeric vector with weights between 0 and 1, representing the probability to select variables for splitting. Alternatively, a list of size num.trees, containing split select weight vectors for each tree can be used.  
##' @param always.split.variables Currently not useable. Character vector with variable names to be always selected.
##' @param respect.unordered.factors Handling of unordered factor covariates. One of 'ignore' and 'order' (the option 'partition' possible in 'ranger' is not (yet) possible with diversity forests). Default is 'ignore'. Alternatively TRUE (='order') or FALSE (='ignore') can be used. 
##' @param scale.permutation.importance Scale permutation importance by standard error as in (Breiman 2001). Only applicable if permutation variable importance mode selected.
##' @param keep.inbag Save how often observations are in-bag in each tree. 
##' @param inbag Manually set observations per tree. List of size num.trees, containing inbag counts for each observation. Can be used for stratified sampling.
##' @param holdout Hold-out mode. Hold-out all samples with case weight 0 and use these for variable importance and prediction error.
##' @param quantreg Prepare quantile prediction as in quantile regression forests (Meinshausen 2006). Regression only. Set \code{keep.inbag = TRUE} to prepare out-of-bag quantile prediction.
##' @param oob.error Compute OOB prediction error. Set to \code{FALSE} to save computation time, e.g. for large survival forests.
##' @param num.threads Number of threads. Default is number of CPUs available.
##' @param save.memory Use memory saving (but slower) splitting mode. No effect for survival and GWAS data. Warning: This option slows down the tree growing, use only if you encounter memory problems. NOT needed for diversity forests.
##' @param verbose Show computation status and estimated runtime.
##' @param seed Random seed. Default is \code{NULL}, which generates the seed from \code{R}. Set to \code{0} to ignore the \code{R} seed. 
##' @param dependent.variable.name Name of outcome variable, needed if no formula given. For survival forests this is the time variable.
##' @param status.variable.name Name of status variable, only applicable to survival data and needed if no formula given. Use 1 for event and 0 for censoring.
##' @param classification Only needed if data is a matrix. Set to \code{TRUE} to grow a classification forest.
##' @param nsplits Number of candidate splits to sample for each split. Default is 30.
##' @param proptry Parameter that restricts the number of candidate splits considered for small nodes. If \code{nsplits} is larger than \code{proptry} times the number of all possible splits, the number of candidate splits to draw is reduced to the largest integer smaller than \code{proptry} times the number of all possible splits. Default is 1, which corresponds to always using \code{nsplits} candidate splits.
##' @return Object of class \code{divfor} with elements
##'   \item{\code{forest}}{Saved forest (If write.forest set to TRUE). Note that the variable IDs in the \code{split.varIDs} object do not necessarily represent the column number in R.}
##'   \item{\code{predictions}}{Predicted classes/values, based on out-of-bag samples (classification and regression only).}
##'   \item{\code{variable.importance}}{Variable importance for each independent variable.}
##'   \item{\code{prediction.error}}{Overall out-of-bag prediction error. For classification this is the fraction of missclassified samples, for probability estimation the Brier score, for regression the mean squared error and for survival one minus Harrell's C-index.}
##'   \item{\code{r.squared}}{R squared. Also called explained variance or coefficient of determination (regression only). Computed on out-of-bag data.}
##'   \item{\code{confusion.matrix}}{Contingency table for classes and predictions based on out-of-bag samples (classification only).}
##'   \item{\code{unique.death.times}}{Unique death times (survival only).}
##'   \item{\code{chf}}{Estimated cumulative hazard function for each sample (survival only).}
##'   \item{\code{survival}}{Estimated survival function for each sample (survival only).}
##'   \item{\code{call}}{Function call.}
##'   \item{\code{num.trees}}{Number of trees.}
##'   \item{\code{num.independent.variables}}{Number of independent variables.}
##'   \item{\code{min.node.size}}{Value of minimal node size used.}
##'   \item{\code{treetype}}{Type of forest/tree. classification, regression or survival.}
##'   \item{\code{importance.mode}}{Importance mode used.}
##'   \item{\code{num.samples}}{Number of samples.}
##'   \item{\code{splitrule}}{Splitting rule.}
##'   \item{\code{replace}}{Sample with replacement.}
##'   \item{\code{nsplits}}{Value of \code{nsplits} used.}
##'   \item{\code{proptry}}{Value of \code{proptry} used.}
##' @examples
##' \dontrun{
##' 
##' ## Load package:
##' library("diversityForest")
##' 
##' ## Set seed to obtain reproducible results:
##' set.seed(1234)
##'
##' ## Diversity forest with default settings (NOT recommended)
##' # Classification:
##' divfor(Species ~ ., data = iris, num.trees = 20)
##' # Regression:
##' iris2 <- iris; iris2$Species <- NULL; iris2$Y <- rnorm(nrow(iris2))
##' divfor(Y ~ ., data = iris2, num.trees = 20)
##' # Survival:
##' library("survival")
##' divfor(Surv(time, status) ~ ., data = veteran, num.trees = 20, respect.unordered.factors = "order")
##' # NOTE: num.trees = 20 is specified too small for practical 
##' # purposes - the prediction performance of the resulting 
##' # forest will be suboptimal!!
##' # In practice, num.trees = 500 (default value) or a 
##' # larger number should be used.
##' 
##' ## Diversity forest with specified values for nsplits and proptry (NOT recommended)
##' divfor(Species ~ ., data = iris, nsplits = 10, proptry = 0.4, num.trees = 20)
##' # NOTE again: num.trees = 20 is specified too small for practical purposes.
##' 
##' ## Applying diversity forest after optimizing the values of nsplits and proptry (recommended)
##' tuneres <- tunedivfor(formula = Species ~ ., data = iris, num.trees.pre = 20)
##' # NOTE: num.trees.pre = 20 is specified too small for practical 
##' # purposes - the out-of-bag error estimates of the forests 
##' # constructed during optimization will be much too variable!!
##' # In practice, num.trees.pre = 500 (default value) or a 
##' # larger number should be used.
##' divfor(Species ~ ., data = iris, nsplits = tuneres$nsplitsopt, 
##'   proptry = tuneres$proptryopt, num.trees = 20)
##' # NOTE again: num.trees = 20 is specified too small for practical purposes.
##' 
##' ## Prediction
##' train.idx <- sample(nrow(iris), 2/3 * nrow(iris))
##' iris.train <- iris[train.idx, ]
##' iris.test <- iris[-train.idx, ]
##' tuneres <- tunedivfor(formula = Species ~ ., data = iris.train, num.trees.pre = 20)
##' # NOTE again: num.trees.pre = 20 is specified too small for practical purposes.
##' rg.iris <- divfor(Species ~ ., data = iris.train, nsplits = tuneres$nsplitsopt, 
##'   proptry = tuneres$proptryopt, num.trees = 20)
##' # NOTE again: num.trees = 20 is specified too small for practical purposes.
##' pred.iris <- predict(rg.iris, data = iris.test)
##' table(iris.test$Species, pred.iris$predictions)
##' 
##' ## Variable importance
##' rg.iris <- divfor(Species ~ ., data = iris, importance = "permutation", num.trees = 20)
##' # NOTE again: num.trees = 20 is specified too small for practical purposes.
##' rg.iris$variable.importance
##' }
##'
##' @author Roman Hornung, Marvin N. Wright
##' @references
##' \itemize{
##'   \item Hornung, R. (2022). Diversity forests: Using split sampling to enable innovative complex split procedures in random forests. SN Computer Science 3(2):1, <\doi{10.1007/s42979-021-00920-1}>.
##'   \item Wright, M. N., Ziegler, A. (2017). ranger: A fast implementation of random forests for high dimensional data in C++ and R. Journal of Statistical Software 77:1-17, <\doi{10.18637/jss.v077.i01}>.
##'   \item Breiman, L. (2001). Random forests. Machine Learning 45:5-32, <\doi{10.1023/A:1010933404324}>.
##'   \item Malley, J. D., Kruppa, J., Dasgupta, A., Malley, K. G., & Ziegler, A. (2012). Probability machines: consistent probability estimation using nonparametric learning machines. Methods of Information in Medicine 51:74-81, <\doi{10.3414/ME00-01-0052}>.
##'   \item Meinshausen (2006). Quantile Regression Forests. Journal of Machine Learning Research 7:983-999.
##'   }
##' @seealso \code{\link{predict.divfor}}
##' @encoding UTF-8
##' @useDynLib diversityForest, .registration = TRUE
##' @importFrom Rcpp evalCpp
##' @import stats 
##' @import utils
##' @importFrom Matrix Matrix
##' @export
divfor <- function(formula = NULL, data = NULL, num.trees = 500, mtry = NULL,
                   importance = "none", write.forest = TRUE, probability = FALSE,
                   min.node.size = NULL, max.depth = NULL, replace = TRUE, 
                   sample.fraction = ifelse(replace, 1, 0.632), 
                   case.weights = NULL, class.weights = NULL, splitrule = NULL, 
                   num.random.splits = 1, alpha = 0.5, minprop = 0.1,
                   split.select.weights = NULL, always.split.variables = NULL,
                   respect.unordered.factors = NULL,
                   scale.permutation.importance = FALSE,
                   keep.inbag = FALSE, inbag = NULL, holdout = FALSE,
                   quantreg = FALSE, oob.error = TRUE,
                   num.threads = NULL, save.memory = FALSE,
                   verbose = TRUE, seed = NULL, 
                   dependent.variable.name = NULL, status.variable.name = NULL, 
                   classification = NULL, nsplits = 30, proptry = 1) { # asdf
  
  ## GenABEL GWA data
  if ("gwaa.data" %in% class(data)) {
    snp.names <- data@gtdata@snpnames
    snp.data <- data@gtdata@gtps@.Data
    data <- data@phdata
    if ("id" %in% names(data)) {
      data$"id" <- NULL
    }
    gwa.mode <- TRUE
    save.memory <- FALSE
  } else {
    snp.data <- as.matrix(0)
    gwa.mode <- FALSE
  }
  
  ## Stop, if option "respect.unordered.factors = 'partition'" (cf. ranger) was specified:
  if(!is.null(respect.unordered.factors) && respect.unordered.factors == "partition") {
    stop("Error: Option 'respect.unordered.factors = 'partition'' not (yet) supported for diversity forests.")
  }
 
  ## Sparse matrix data
  if (inherits(data, "Matrix")) {
    if (!("dgCMatrix" %in% class(data))) {
      stop("Error: Currently only sparse data of class 'dgCMatrix' supported.")
    }
  
    if (!is.null(formula)) {
      stop("Error: Sparse matrices only supported with alternative interface. Use dependent.variable.name instead of formula.")
    }
  }
    
  ## Formula interface. Use whole data frame is no formula provided and depvarname given
  if (is.null(formula)) {
    if (is.null(dependent.variable.name)) {
      stop("Error: Please give formula or outcome variable name.")
    }
    if (is.null(status.variable.name)) {
      status.variable.name <- ""
      response <- data[, dependent.variable.name, drop = TRUE]
    } else {
      response <- survival::Surv(data[, dependent.variable.name], data[, status.variable.name]) #data[, c(dependent.variable.name, status.variable.name)]
    }
    data.selected <- data
  } else {
    formula <- formula(formula)
    if (!inherits(formula, "formula")) {
      stop("Error: Invalid formula.")
    }
    data.selected <- parse.formula(formula, data, env = parent.frame())
    response <- data.selected[, 1]
  }
  
  ## Check missing values
  if (any(is.na(data.selected))) {
    offending_columns <- colnames(data.selected)[colSums(is.na(data.selected)) > 0]
    stop("Missing data in columns: ",
         paste0(offending_columns, collapse = ", "), ".", call. = FALSE)
  }
  
  ## Check response levels
  if (is.factor(response)) {
    if (nlevels(response) != nlevels(droplevels(response))) {
      dropped_levels <- setdiff(levels(response), levels(droplevels(response)))
      warning("Dropped unused factor level(s) in outcome variable: ",
              paste0(dropped_levels, collapse = ", "), ".", call. = FALSE)
    }
  }
  
  ## Treetype
  if (is.factor(response)) {
    if (probability) {
      treetype <- 9
    } else {
      treetype <- 1
    }
  } else if (is.numeric(response) && (is.null(ncol(response)) || ncol(response) == 1)) {
    if (!is.null(classification) && classification && !probability) {
      treetype <- 1
    } else if (probability) {
      treetype <- 9
    } else {
      treetype <- 3
    }
  } else if (inherits(response, "Surv") || is.data.frame(response) || is.matrix(response)) {
    treetype <- 5
  } else {
    stop("Error: Unsupported type of outcome variable.")
  }
  
  ## Quantile prediction only for regression
  if (quantreg && treetype != 3) {
    stop("Error: Quantile prediction implemented only for regression outcomes.")
  }
  
  ## Dependent and status variable name. For non-survival dummy status variable name.
  if (!is.null(formula)) {
    if (treetype == 5) {
      dependent.variable.name <- dimnames(response)[[2]][1]
      status.variable.name <- dimnames(response)[[2]][2]
    } else {
      dependent.variable.name <- names(data.selected)[1]
      status.variable.name <- ""
    }
    independent.variable.names <- names(data.selected)[-1]
  } else {
    independent.variable.names <- colnames(data.selected)[colnames(data.selected) != dependent.variable.name &
                                                          colnames(data.selected) != status.variable.name]
  }
  
  ## respect.unordered.factors
  if (is.null(respect.unordered.factors)) {
    #if (!is.null(splitrule) && splitrule == "extratrees") {
    #  respect.unordered.factors <- "partition"
    #} else {
      respect.unordered.factors <- "ignore"
    #}
  }

  ## Old version of respect.unordered.factors
  if (respect.unordered.factors == TRUE) {
    respect.unordered.factors <- "order"
  } else if (respect.unordered.factors == FALSE) {
    respect.unordered.factors <- "ignore"
  }
  
  ## Recode characters as factors and recode factors if 'order' mode
  if (!is.matrix(data.selected) && !inherits(data.selected, "Matrix")) {
    character.idx <- sapply(data.selected, is.character)
    
    if (respect.unordered.factors == "order") {
      ## Recode characters and unordered factors
      names.selected <- names(data.selected)
      ordered.idx <- sapply(data.selected, is.ordered)
      factor.idx <- sapply(data.selected, is.factor)
      independent.idx <- names.selected != dependent.variable.name & 
        names.selected != status.variable.name & 
        names.selected != paste0("Surv(", dependent.variable.name, ", ", status.variable.name, ")")
      recode.idx <- independent.idx & (character.idx | (factor.idx & !ordered.idx))

      if (any(recode.idx) & (importance == "impurity_corrected" || importance == "impurity_unbiased")) {
        warning("Corrected impurity importance may not be unbiased for re-ordered factor levels. Consider setting respect.unordered.factors to 'ignore' or 'partition' or manually compute corrected importance.")
      }
      
      ## Numeric response
      if (is.factor(response)) {
        num.response <- as.numeric(response)
      } else {
        num.response <- response
      }

      ## Recode each column
      data.selected[recode.idx] <- lapply(data.selected[recode.idx], function(x) {
        if (!is.factor(x)) {
          x <- as.factor(x)
        } 
        
        if ("Surv" %in% class(response)) {
          ## Use median survival if available or largest quantile available in all strata if median not available
          levels.ordered <- largest.quantile(response ~ x)
          
          ## Get all levels not in node
          levels.missing <- setdiff(levels(x), levels.ordered)
          levels.ordered <- c(levels.missing, levels.ordered)
        } else if (is.factor(response) & nlevels(response) > 2) {
          levels.ordered <- pca.order(y = response, x = x)
        } else {
          ## Order factor levels by mean response
          means <- sapply(levels(x), function(y) {
            mean(num.response[x == y])
          })
          levels.ordered <- as.character(levels(x)[order(means)])
        }
        
        ## Return reordered factor
        factor(x, levels = levels.ordered, ordered = TRUE, exclude = NULL)
      })
      
      ## Save levels
      covariate.levels <- lapply(data.selected[independent.idx], levels)
    } else {
      ## Recode characters only
      data.selected[character.idx] <- lapply(data.selected[character.idx], factor)
    }
  }
  
  ## Input data and variable names, create final data matrix
  if (!is.null(formula) && treetype == 5) {
    data.final <- data.matrix(cbind(response[, 1], response[, 2],
                              data.selected[-1]))
    colnames(data.final) <- c(dependent.variable.name, status.variable.name,
                              independent.variable.names)
  } else if (is.matrix(data.selected) || inherits(data.selected, "Matrix")) {
    data.final <- data.selected
  } else {
    data.final <- data.matrix(data.selected)
  }
  variable.names <- colnames(data.final)
  
  ## If gwa mode, add snp variable names
  if (gwa.mode) {
    variable.names <- c(variable.names, snp.names)
    all.independent.variable.names <- c(independent.variable.names, snp.names)
  } else {
    all.independent.variable.names <- independent.variable.names
  }
  
  ## Error if no covariates
  if (length(all.independent.variable.names) < 1) {
    stop("Error: No covariates found.")
  }
  
  ## Number of trees
  if (!is.numeric(num.trees) || num.trees < 1) {
    stop("Error: Invalid value for num.trees.")
  }
  
  ## mtry
  if (is.null(mtry)) {
    mtry <- 0
  } else if (!is.numeric(mtry) || mtry < 0) {
    stop("Error: Invalid value for mtry")
  }

    ## proptry ## asdf
  if (is.null(proptry)) { ## asdf
    proptry <- 0 ## asdf
  } else if (!is.numeric(proptry) || !((proptry > 0) & (proptry <= 1))) { ## asdf
    stop("Error: Invalid value for proptry") ## asdf
  } ## asdf
  
  ## Seed
  if (is.null(seed)) {
    seed <- runif(1 , 0, .Machine$integer.max)
  }
  
  ## Keep inbag
  if (!is.logical(keep.inbag)) {
    stop("Error: Invalid value for keep.inbag")
  }
  
  ## Num threads
  ## Default 0 -> detect from system in C++.
  if (is.null(num.threads)) {
    num.threads = 0
  } else if (!is.numeric(num.threads) || num.threads < 0) {
    stop("Error: Invalid value for num.threads")
  }
  
  ## Minumum node size
  if (is.null(min.node.size)) {
    min.node.size <- 0
  } else if (!is.numeric(min.node.size) || min.node.size < 0) {
    stop("Error: Invalid value for min.node.size")
  }
  
  ## Tree depth
  if (is.null(max.depth)) {
    max.depth <- 0
  } else if (!is.numeric(max.depth) || max.depth < 0) {
    stop("Error: Invalid value for max.depth. Please give a positive integer.")
  }
  
  ## Sample fraction
  if (!is.numeric(sample.fraction)) {
    stop("Error: Invalid value for sample.fraction. Please give a value in (0,1] or a vector of values in [0,1].")
  }
  if (length(sample.fraction) > 1) {
    if (!(treetype %in% c(1, 9))) {
      stop("Error: Invalid value for sample.fraction. Vector values only valid for classification forests.")
    }
    if (any(sample.fraction < 0) || any(sample.fraction > 1)) {
      stop("Error: Invalid value for sample.fraction. Please give a value in (0,1] or a vector of values in [0,1].")
    }
    if (sum(sample.fraction) <= 0) {
      stop("Error: Invalid value for sample.fraction. Sum of values must be >0.")
    }
    if (length(sample.fraction) != nlevels(response)) {
      stop("Error: Invalid value for sample.fraction. Expecting ", nlevels(response), " values, provided ", length(sample.fraction), ".")
    }
    if (!replace & any(sample.fraction * length(response) > table(response))) {
      idx <- which(sample.fraction * length(response) > table(response))[1]
      stop("Error: Not enough samples in class ", names(idx), 
           "; available: ", table(response)[idx], 
           ", requested: ", (sample.fraction * length(response))[idx], ".")
    }
    if (!is.null(case.weights)) {
      stop("Error: Combination of case.weights and class-wise sampling not supported.")
    }
  } else {
    if (sample.fraction <= 0 || sample.fraction > 1) {
      stop("Error: Invalid value for sample.fraction. Please give a value in (0,1] or a vector of values in [0,1].")
    }
  }
  
  ## Importance mode
  if (is.null(importance) || importance == "none") {
    importance.mode <- 0
  } else if (importance == "impurity") {
    importance.mode <- 1
  } else if (importance == "impurity_corrected" || importance == "impurity_unbiased") {
    importance.mode <- 5
  } else if (importance == "permutation") {
    if (scale.permutation.importance) {
      importance.mode <- 2
    } else {
      importance.mode <- 3
    }
  } else {
    stop("Error: Unknown importance mode.")
  }
  
  ## Case weights: NULL for no weights
  if (is.null(case.weights)) {
    case.weights <- c(0,0)
    use.case.weights <- FALSE
    if (holdout) {
      stop("Error: Case weights required to use holdout mode.")
    }
  } else {
    use.case.weights <- TRUE
    
    ## Sample from non-zero weights in holdout mode
    if (holdout) {
      sample.fraction <- sample.fraction * mean(case.weights > 0)
    }
    
    if (!replace && sum(case.weights > 0) < sample.fraction * nrow(data.final)) {
      stop("Error: Fewer non-zero case weights than observations to sample.")
    }
  }
  
  ## Manual inbag selection
  if (is.null(inbag)) {
    inbag <- list(c(0,0))
    use.inbag <- FALSE
  } else if (is.list(inbag)) {
    use.inbag <- TRUE
    if (use.case.weights) {
      stop("Error: Combination of case.weights and inbag not supported.")
    }
    if (length(sample.fraction) > 1) {
      stop("Error: Combination of class-wise sampling and inbag not supported.")
    }
    if (length(inbag) != num.trees) {
      stop("Error: Size of inbag list not equal to number of trees.")
    }
  } else {
    stop("Error: Invalid inbag, expects list of vectors of size num.trees.")
  }
  
  ## Class weights: NULL for no weights (all 1)
  if (is.null(class.weights)) {
    class.weights <- rep(1, nlevels(response))
  } else {
    if (!(treetype %in% c(1, 9))) {
      stop("Error: Argument class.weights only valid for classification forests.")
    }
    if (!is.numeric(class.weights) || any(class.weights < 0)) {
      stop("Error: Invalid value for class.weights. Please give a vector of non-negative values.")
    }
    if (length(class.weights) != nlevels(response)) {
      stop("Error: Number of class weights not equal to number of classes.")
    }

    ## Reorder (C++ expects order as appearing in the data)
    class.weights <- class.weights[unique(as.numeric(response))]
  }
  
  ## Split select weights: NULL for no weights
  if (is.null(split.select.weights)) {
    split.select.weights <- list(c(0,0))
    use.split.select.weights <- FALSE
  } else if (is.numeric(split.select.weights)) {
    if (length(split.select.weights) != length(all.independent.variable.names)) {
      stop("Error: Number of split select weights not equal to number of independent variables.")
    }
    split.select.weights <- list(split.select.weights)
    use.split.select.weights <- TRUE
  } else if (is.list(split.select.weights)) {
    if (length(split.select.weights) != num.trees) {
      stop("Error: Size of split select weights list not equal to number of trees.")
    }
    use.split.select.weights <- TRUE
  } else {
    stop("Error: Invalid split select weights.")
  }
  
  ## Always split variables: NULL for no variables
  if (is.null(always.split.variables)) {
    always.split.variables <- c("0", "0")
    use.always.split.variables <- FALSE
  } else {
    use.always.split.variables <- TRUE
  }
  
  if (use.split.select.weights && use.always.split.variables) {
    stop("Error: Please use only one option of split.select.weights and always.split.variables.")
  }

  ## Currently, for diversity forests, only the default 
  ## splitting rules are supported:  ## asdf
    if (!is.null(splitrule)) {
    if (treetype == 5 && splitrule != "logrank") {
      splitrule <- "logrank"
	  warning("'splitrule' changed to 'logrank' as currently only this option is available for diversity forests for survival.")
    } else if (treetype == 3 && splitrule != "variance") {
      splitrule <- "variance"
	  	  warning("'splitrule' changed to 'variance' as currently only this option is available for diversity forests for regression.")
    } else if (treetype %in% c(1, 9) && splitrule != "gini") {
      splitrule <- "gini"
	  	  	  warning("'splitrule' changed to 'gini' as currently only this option is available for diversity forests for classification.")
    }
  }
  
  ## Splitting rule
  if (is.null(splitrule)) {
    if (treetype == 5) {
      splitrule <- "logrank"
    } else if (treetype == 3) {
      splitrule <- "variance"
    } else if (treetype %in% c(1, 9)) {
      splitrule <- "gini"
    }
    splitrule.num <- 1
  } else if (splitrule == "logrank") {
    if (treetype == 5) {
      splitrule.num <- 1
    } else {
      stop("Error: logrank splitrule applicable to survival data only.")
    }
  } else if (splitrule == "gini") {
    if (treetype %in% c(1, 9)) {
      splitrule.num <- 1
    } else {
      stop("Error: Gini splitrule applicable to classification data only.")
    }
  } else if (splitrule == "variance") {
    if (treetype == 3) {
      splitrule.num <- 1
    } else {
      stop("Error: variance splitrule applicable to regression data only.")
    }
  } else if (splitrule == "auc" || splitrule == "C") {
    if (treetype == 5) {
      splitrule.num <- 2
    } else {
      stop("Error: C index splitrule applicable to survival data only.")
    }
  } else if (splitrule == "auc_ignore_ties" || splitrule == "C_ignore_ties") {
    if (treetype == 5) {
      splitrule.num <- 3
    } else {
      stop("Error: C index splitrule applicable to survival data only.")
    }
  } else if (splitrule == "maxstat") {
    if (treetype == 5 || treetype == 3) {
      splitrule.num <- 4
    } else {
      stop("Error: maxstat splitrule applicable to regression or survival data only.")
    }
  } else if (splitrule == "extratrees") {
    splitrule.num <- 5
  } else {
    stop("Error: Unknown splitrule.")
  }
  
  ## Maxstat splitting
  if (alpha < 0 || alpha > 1) {
    stop("Error: Invalid value for alpha, please give a value between 0 and 1.")
  }
  if (minprop < 0 || minprop > 0.5) {
    stop("Error: Invalid value for minprop, please give a value between 0 and 0.5.")
  }

  ## Extra trees
  if (!is.numeric(num.random.splits) || num.random.splits < 1) {
    stop("Error: Invalid value for num.random.splits, please give a positive integer.")
  }
  if (splitrule.num == 5 && save.memory && respect.unordered.factors == "partition") {
    stop("Error: save.memory option not possible in extraTrees mode with unordered predictors.")
  }

  ## Unordered factors  
  if (respect.unordered.factors == "partition") {
    names.selected <- names(data.selected)
    ordered.idx <- sapply(data.selected, is.ordered)
    factor.idx <- sapply(data.selected, is.factor)
    independent.idx <- names.selected != dependent.variable.name & names.selected != status.variable.name
    unordered.factor.variables <- names.selected[factor.idx & !ordered.idx & independent.idx]
    
    if (length(unordered.factor.variables) > 0) {
      use.unordered.factor.variables <- TRUE
      ## Check level count
      num.levels <- sapply(data.selected[, factor.idx & !ordered.idx & independent.idx, drop = FALSE], nlevels)
      max.level.count <- .Machine$double.digits
      if (max(num.levels) > max.level.count) {
        stop(paste("Too many levels in unordered categorical variable ", unordered.factor.variables[which.max(num.levels)], 
                   ". Only ", max.level.count, " levels allowed on this system. Consider using the 'order' option.", sep = ""))
      } 
    } else {
      unordered.factor.variables <- c("0", "0")
      use.unordered.factor.variables <- FALSE
    } 
  } else if (respect.unordered.factors == "ignore" || respect.unordered.factors == "order") {
    ## Ordering for "order" is handled above
    unordered.factor.variables <- c("0", "0")
    use.unordered.factor.variables <- FALSE
  } else {
    stop("Error: Invalid value for respect.unordered.factors, please use 'order', 'partition' or 'ignore'.")
  }

  ## Unordered maxstat splitting not possible
  if (use.unordered.factor.variables && !is.null(splitrule)) {
    if (splitrule == "maxstat") {
      stop("Error: Unordered factor splitting not implemented for 'maxstat' splitting rule.")
    } else if (splitrule %in% c("C", "auc", "C_ignore_ties", "auc_ignore_ties")) {
      stop("Error: Unordered factor splitting not implemented for 'C' splitting rule.")
    }
  }
  
  ## Warning for experimental 'order' splitting 
  if (respect.unordered.factors == "order") {
    if (treetype == 3 && splitrule == "maxstat") {
      warning("Warning: The 'order' mode for unordered factor handling with the 'maxstat' splitrule is experimental.")
    }
    if (gwa.mode & ((treetype %in% c(1,9) & nlevels(response) > 2) | treetype == 5)) {
      stop("Error: Ordering of SNPs currently only implemented for regression and binary outcomes.")
    }
  }
  

  ## Prediction mode always false. Use predict.divfor() method.
  prediction.mode <- FALSE
  predict.all <- FALSE
  prediction.type <- 1
  
  ## No loaded forest object
  loaded.forest <- list()
  
  ## Use sparse matrix
  if ("dgCMatrix" %in% class(data.final)) {
    sparse.data <- data.final
    data.final <- matrix(c(0, 0))
    use.sparse.data <- TRUE
  } else {
    sparse.data <- Matrix(matrix(c(0, 0)))
    use.sparse.data <- FALSE
  }
  
  if (respect.unordered.factors == "order"){
    order.snps <- TRUE
  } else {
    order.snps <- FALSE
  }
  
  
  ## Clean up
  rm("data.selected")

  ## Call divfor
  
  result <- divforCpp(treetype, dependent.variable.name, data.final, variable.names, mtry,
                      num.trees, verbose, seed, num.threads, write.forest, importance.mode,
                      min.node.size, split.select.weights, use.split.select.weights,
                      always.split.variables, use.always.split.variables,
                      status.variable.name, prediction.mode, loaded.forest, snp.data,
                      replace, probability, unordered.factor.variables, use.unordered.factor.variables, 
                      save.memory, splitrule.num, case.weights, use.case.weights, class.weights, 
                      predict.all, keep.inbag, sample.fraction, alpha, minprop, holdout, prediction.type, 
                      num.random.splits, sparse.data, use.sparse.data, order.snps, oob.error, max.depth, 
                      inbag, use.inbag, nsplits, npairs=0, proptry, divfortype=1, promispairs=list(0,0), eim_mode=0)
  
  if (length(result) == 0) {
    stop("User interrupt or internal error.")
  }
  
  ## Prepare results
  if (importance.mode != 0) {
    names(result$variable.importance) <- all.independent.variable.names
  }

  ## Set predictions
  if (treetype == 1 && is.factor(response) && oob.error) {
    result$predictions <- integer.to.factor(result$predictions,
                                            levels(response))
    true.values <- integer.to.factor(unlist(data.final[, dependent.variable.name]),
                                     levels(response))
    result$confusion.matrix <- table(true.values, result$predictions, 
                                     dnn = c("true", "predicted"), useNA = "ifany")
  } else if (treetype == 5 && oob.error) {
    if (is.list(result$predictions)) {
      result$predictions <- do.call(rbind, result$predictions)
    } 
    if (is.vector(result$predictions)) {
      result$predictions <- matrix(result$predictions, nrow = 1)
    }
    result$chf <- result$predictions
    result$predictions <- NULL
    result$survival <- exp(-result$chf)
  } else if (treetype == 9 && !is.matrix(data) && oob.error) {
    if (is.list(result$predictions)) {
      result$predictions <- do.call(rbind, result$predictions)
    } 
    if (is.vector(result$predictions)) {
      result$predictions <- matrix(result$predictions, nrow = 1)
    }
    
    ## Set colnames and sort by levels
    colnames(result$predictions) <- unique(response)
    if (is.factor(response)) {
      result$predictions <- result$predictions[, levels(droplevels(response)), drop = FALSE]
    }
  }
  
  ## Splitrule
  result$splitrule <- splitrule
  
  ## Set treetype
  if (treetype == 1) {
    result$treetype <- "Classification"
  } else if (treetype == 3) {
    result$treetype <- "Regression"
  } else if (treetype == 5) {
    result$treetype <- "Survival"
  } else if (treetype == 9) {
    result$treetype <- "Probability estimation"
  }
  if (treetype == 3) {
    result$r.squared <- 1 - result$prediction.error / var(response)
  }
  result$call <- sys.call()
  result$importance.mode <- importance
  result$num.samples <- nrow(data.final)
  result$replace <- replace
  
  ## Write forest object
  if (write.forest) {
    if (is.factor(response)) {
      result$forest$levels <- levels(response)
    }
    result$forest$independent.variable.names <- independent.variable.names
    result$forest$treetype <- result$treetype
    class(result$forest) <- "divfor.forest"
    
    ## In 'ordered' mode, save covariate levels
    if (respect.unordered.factors == "order" && !is.matrix(data)) {
      result$forest$covariate.levels <- covariate.levels
    }
  }
  
  class(result) <- "divfor"
  
  ## Prepare quantile prediction
  if (quantreg) {
    terminal.nodes <- predict(result, data, type = "terminalNodes")$predictions + 1
    n <- result$num.samples
    result$random.node.values <- matrix(nrow = max(terminal.nodes), ncol = num.trees)
    
    ## Select one random obs per node and tree
    for (tree in 1:num.trees){
      idx <- sample(1:n, n)
      result$random.node.values[terminal.nodes[idx, tree], tree] <- response[idx]
    }
    
    ## Prepare out-of-bag quantile regression
    if(!is.null(result$inbag.counts)) {
      inbag.counts <- simplify2array(result$inbag.counts)
      random.node.values.oob <- 0 * terminal.nodes
      random.node.values.oob[inbag.counts > 0] <- NA
      
      ## For each tree and observation select one random obs in the same node (not the same obs)
      for (tree in 1:num.trees){
        is.oob <- inbag.counts[, tree] == 0
        num.oob <- sum(is.oob)
        
        if (num.oob != 0) {
          oob.obs <- which(is.oob)
          oob.nodes <- terminal.nodes[oob.obs, tree]
          for (j in 1:num.oob) {
            idx <- terminal.nodes[, tree] == oob.nodes[j]
            idx[oob.obs[j]] <- FALSE
            random.node.values.oob[oob.obs[j], tree] <- save.sample(response[idx], size = 1)
          }
        }
      }
      
      ## Check num.trees
      minoob <- min(rowSums(inbag.counts == 0))
      if (minoob < 10) {
        stop("Error: Too few trees for out-of-bag quantile regression.")
      }
      
      ## Use the same number of values for all obs, select randomly
      result$random.node.values.oob <- t(apply(random.node.values.oob, 1, function(x) {
        sample(x[!is.na(x)], minoob)
      }))
    }
  }

  result$mtry <- NULL
  
  return(result)
}
