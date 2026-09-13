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

##' Constructs a random forest for multi-class outcomes and calculates the class-focused variable importance measure (VIM) and the discriminatory VIM.\cr
##' The class-focused VIM ranks the covariates with respect to their ability to distinguish individual outcome classes from all others, which can be important in multi-class prediction tasks (see "Details" below). The discriminatory VIM, in contrast, similarly to conventional VIMs, measures the overall influence of covariates on classification performance, regardless of their relevance to individual classes.
##' 
##' Covariates targeted by the \emph{class-focused VIM}, which specifically help distinguish individual outcome classes from the others are hereafter referred to as "class-related covariates". The primary motivation for identifying class-related covariates is frequently the interpretation of covariate effects.\cr
##' Potential example applications include cancer subtyping (identifying biomarkers predictive of specific subtypes, e.g., luminal A, HER2-positive, rather than broad groups, e.g., hormone-driven vs. non-hormone-driven cancers), voting studies (covariates specifically associated with support for individual parties rather than general ideological orientation), and forensic science (detecting covariates specific to crime types like burglary or cybercrime rather than broadly violent vs. non-violent offenses).\cr
##' In contrast to the class-focused VIM, conventional VIMs, 
##' such as the permutation VIM or the Gini importance, and the \emph{discriminatory VIM} measure the overall influence 
##' of variables regardless of their class-relatedness Therefore, these measures 
##' rank not only class-related variables high, but also variables that only 
##' discriminate well between groups of classes. This is problematic, if only 
##' class-related variables are to be identified.\cr
##' NOTE: To learn about the shapes of the influences of the variables with the largest 
##' class-focused VIM values on the multi-class outcome, it is crucial to apply the 
##' \code{\link{plot.multifor}} function to the \code{multifor} object. Two further related
##' plot functions are \code{\link{plotMcl}} and \code{\link{plotVar}}.\cr
##' NOTE ALSO: This methodology is based on work currently under peer review. A reference will be added once the corresponding paper is published.\cr\cr
##' The class-focused VIM requires that all variables are ordered. For this reason, before constructing the random forest,
##' the categories of unordered categorical variables are ordered using an approach
##' by Coppersmith et al. (1999), which ensures that close categories feature similar 
##' outcome class distributions. This approach is also used in the \code{ranger} R package,
##' when using the option \code{respect.unordered.factors="order"}.
##' 
##' @title Construct a random forest prediction rule and calculate class-focused and discriminatory variable importance scores.
##' @param formula Object of class \code{formula} or \code{character} describing the model to fit. Interaction terms supported only for numerical variables.
##' @param data Training data of class \code{data.frame}, or \code{matrix}, \code{dgCMatrix} (Matrix).
##' @param num.trees Number of trees. Default is 5000 for datasets with a maximum of 5000 observations and 1000 for datasets with more than 5000 observations.
##' @param importance Variable importance mode, one of the following: "both" (the default), "class-focused", "discriminatory", "none". If "class-focused", class-focused VIM values are computed, if "discriminatory", discriminatory VIM values are computed, and if "both", both class-focused and discriminatory VIM values are computed. See the 'Details' section below for details.
##' @param write.forest Save \code{multifor.forest} object, required for prediction. Set to \code{FALSE} to reduce memory usage if no prediction intended.
##' @param probability Grow a probability forest as in Malley et al. (2012). Using this option (default is \code{TRUE}), class probability predictions are obtained.
##' @param min.node.size Minimal node size. Default 5 for probability and 1 for classification.
##' @param max.depth Maximal tree depth. A value of NULL or 0 (the default) corresponds to unlimited depth, 1 to tree stumps (1 split per tree).
##' @param replace Sample with replacement. Default is \code{FALSE}.
##' @param sample.fraction Fraction of observations to sample. Default is 1 for sampling with replacement and 0.7 for sampling without replacement. This can be a vector of class-specific values. 
##' @param case.weights Weights for sampling of training observations. Observations with larger weights will be selected with higher probability in the bootstrap (or subsampled) samples for the trees.
##' @param keep.inbag Save how often observations are in-bag in each tree. 
##' @param inbag Manually set observations per tree. List of size num.trees, containing inbag counts for each observation. Can be used for stratified sampling.
##' @param holdout Hold-out mode. Hold-out all samples with case weight 0 and use these for variable importance and prediction error.
##' @param oob.error Compute OOB prediction error. Default is \code{TRUE}.
##' @param num.threads Number of threads. Default is number of CPUs available.
##' @param verbose Show computation status and estimated runtime.
##' @param seed Random seed. Default is \code{NULL}, which generates the seed from \code{R}. Set to \code{0} to ignore the \code{R} seed. 
##' @param dependent.variable.name Name of outcome variable, needed if no formula given.
##' @param mtry Number of candidate variables to sample for each split. Default is the (rounded down) square root of the number variables.
##' @param npervar Number of splits to sample per candidate variable. Default is 5.
##' @return Object of class \code{multifor} with elements
##'   \item{\code{predictions}}{Predicted classes (for \code{probability=FALSE}) or class probabilities (for \code{probability=TRUE}), based on out-of-bag samples.}
##'   \item{\code{num.trees}}{Number of trees.}
##'   \item{\code{num.independent.variables}}{Number of independent variables.}
##'   \item{\code{min.node.size}}{Value of minimal node size used.}
##'   \item{\code{mtry}}{Number of candidate variables sampled for each split.}
##'   \item{\code{class_foc_vim}}{class-focused VIM values. Only computed for independent variables that feature at least as many unique values as the outcome variable has classes. For other variables, the entries in the vector \code{var.imp.multiclass} will be \code{NA}.}
##'   \item{\code{discr_vim}}{Discriminatory VIM values for all independent variables.}
##'   \item{\code{prediction.error}}{Overall out-of-bag prediction error. For classification this is the fraction of missclassified samples and for probability estimation the Brier score.}
##'   \item{\code{confusion.matrix}}{Contingency table for classes and predictions based on out-of-bag samples (classification only).}
##'   \item{\code{forest}}{Saved forest (If write.forest set to TRUE). Note that the variable IDs in the \code{split.varIDs} object do not necessarily represent the column number in R.}
##'   \item{\code{treetype}}{Type of forest/tree. Classification or probability.}
##'   \item{\code{call}}{Function call.}
##'   \item{\code{importance.mode}}{Importance mode used.}
##'   \item{\code{num.samples}}{Number of samples.}
##'   \item{\code{replace}}{Sample with replacement.}
##'   \item{\code{plotres}}{List ob objects needed by the plot functions: \code{data} contains the data; \code{yvarname} is the name of the outcome variable.}
##' @examples
##' \dontrun{
##'
##' ## Load package:
##' 
##' library("diversityForest")
##' 
##' 
##' 
##' ## Set seed to make results reproducible:
##' 
##' set.seed(1234)
##' 
##' 
##' 
##' ## Load the "ctg" data set:
##' 
##' data(ctg)
##' 
##' 
##' 
##' ## Construct a random forest:
##' 
##' model <- multifor(dependent.variable.name = "CLASS", data = ctg, 
##'                   num.trees = 20)
##' 
##' # NOTE: num.trees = 20 (in the above) would be much too small for practical 
##' # purposes. This small number of trees was simply used to keep the
##' # runtime of the example short.
##' # The default number of trees is num.trees = 5000 for datasets with a maximum of
##' # 5000 observations and num.trees = 1000 for datasets larger than that.
##' 
##' 
##' 
##' ## The out-of-bag estimated Brier score (note that by default
##' ## 'probability = TRUE' is used in 'multifor'):
##' 
##' model$prediction.error
##' 
##' 
##' 
##' ## Inspect the class-focused and the discriminatory VIM values:
##' 
##' model$class_foc_vim
##' 
##' # --> Note that there are no class-focused VIM values for some of the variables.
##' # These are those for which there are fewer unique values than outcome classes.
##' # See the "Details" section above.
##' 
##' model$discr_vim
##' 
##' 
##' ## Inspect the 5 variables with the largest class-focused VIM values and the
##' ## 5 variables with the largest discriminatory VIM values:
##' 
##' sort(model$class_foc_vim, decreasing = TRUE)[1:5]
##' 
##' sort(model$discr_vim, decreasing = TRUE)[1:5]
##' 
##' 
##' 
##' ## Instead of passing the name of the outcome variable through the 
##' ## 'dependent.variable.name' argument as above, the formula interface can also 
##' ## be used. Below, we fit a random forest with only the first five variables 
##' ## from the 'ctg' data set:
##' 
##' model <- multifor(CLASS ~ b + e + LBE + LB + AC, data=ctg, num.trees = 20)
##' 
##' 
##' ## As expected, the out-of-bag estimated prediction error is much larger
##' ## for this model:
##' 
##' model$prediction.error
##' 
##' 
##' 
##' ## NOTE: Visual exploration of the results of the class-focused VIM analysis
##' ## is crucial.
##' ## Therefore, in practice the next step would be to apply the
##' ## 'plot.multifor' function to the object 'model'.
##' 
##' # plot(model)
##' 
##' 
##' 
##' 
##' 
##' ## Prediction:
##' 
##' 
##' # Separate 'ctg' data set randomly in training
##' # and test data:
##' 
##' data(ctg)
##' train.idx <- sample(nrow(ctg), 2/3 * nrow(ctg))
##' ctg.train <- ctg[train.idx, ]
##' ctg.test <- ctg[-train.idx, ]
##' 
##' # Construct random forest on training data:
##' # NOTE again: num.trees = 20 is specified too small for practical purposes.
##' model_train <- multifor(dependent.variable.name = "CLASS", data = ctg.train, 
##'                         importance = "none", probability = FALSE, 
##'                         num.trees = 20)
##' # NOTE: Because we are only interested in prediction here, we do not
##' # calculate VIM values (by setting importance = "none"), because this
##' # speeds up calculations.
##' # NOTE also: Because we are interested in class label prediction here
##' # rather than class probability prediction we specified 'probability = FALSE'
##' # above.
##' 
##' # Predict class values of the test data:
##' pred.ctg <- predict(model_train, data = ctg.test)
##' 
##' # Compare predicted and true class values of the test data:
##' table(ctg.test$CLASS, pred.ctg$predictions)
##' 
##' 
##' 
##' ## Repeat the analysis for class probability prediction
##' ## (default 'probability = TRUE'):
##' 
##' model_train <- multifor(dependent.variable.name = "CLASS", data = ctg.train, 
##'                         importance = "none", num.trees = 20)
##' 
##' # Predict class probabilities in the test data:
##' pred.ctg <- predict(model_train, data = ctg.test)
##' 
##' # The predictions are now a matrix of class probabilities:
##' head(pred.ctg$predictions)
##' 
##' # Obtain class predictions by choosing the classes with the maximum predicted
##' # probabilities (the function 'which.is.max' chooses one class randomly if
##' # there are several classes with maximum probability):
##' library("nnet")
##' classes <- levels(ctg.train$CLASS)
##' pred_classes <- factor(classes[apply(pred.ctg$predictions, 1, which.is.max)], 
##'                        levels=classes)
##' 
##' # Compare predicted and true class values of the test data:
##' table(ctg.test$CLASS, pred_classes)
##' 
##' }
##'
##' @author Roman Hornung, Marvin N. Wright
##' @references
##' \itemize{
##'   \item Hornung, R. (2022). Diversity forests: Using split sampling to enable innovative complex split procedures in random forests. SN Computer Science 3(2):1, <\doi{10.1007/s42979-021-00920-1}>.
##'   \item Wright, M. N., Ziegler, A. (2017). ranger: A fast implementation of random forests for high dimensional data in C++ and R. Journal of Statistical Software 77:1-17, <\doi{10.18637/jss.v077.i01}>.
##'   \item Breiman, L. (2001). Random forests. Machine Learning 45:5-32, <\doi{10.1023/A:1010933404324}>.
##'   \item Malley, J. D., Kruppa, J., Dasgupta, A., Malley, K. G., & Ziegler, A. (2012). Probability machines: consistent probability estimation using nonparametric learning machines. Methods of Information in Medicine 51:74-81, <\doi{10.3414/ME00-01-0052}>.
##'   \item Coppersmith, D., Hong, S. J., Hosking, J. R. (1999). Partitioning nominal attributes in decision trees. Data Mining and Knowledge Discovery 3:197-217, <\doi{10.1023/A:1009869804967}>.
##'   }
##' @seealso \code{\link{predict.multifor}}
##' @encoding UTF-8
##' @useDynLib diversityForest, .registration = TRUE
##' @importFrom Rcpp evalCpp
##' @import stats 
##' @import utils
##' @importFrom Matrix Matrix
##' @export
multifor <- function(formula = NULL, data = NULL, num.trees = ifelse(nrow(data) <= 5000, 5000, 1000),
                   importance = "both", write.forest = TRUE, probability = TRUE,
                   min.node.size = NULL, max.depth = NULL, replace = FALSE, 
                   sample.fraction = ifelse(replace, 1, 0.7), 
                   case.weights = NULL,
                   keep.inbag = FALSE, inbag = NULL, holdout = FALSE,
                   oob.error = TRUE,
                   num.threads = NULL,
                   verbose = TRUE, seed = NULL, 
                   dependent.variable.name = NULL, 
                   mtry = NULL, npervar = 5) {

  ## We always order the categories of categorical variables:
  respect.unordered.factors <- "order"
  save.memory <- FALSE
  
  ## GenABEL GWA data
  if ("gwaa.data" %in% class(data)) {
    stop("Error: Ordering of SNPs currently not implemented.")
  }
  
    snp.data <- as.matrix(0)

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
      response <- data[, dependent.variable.name, drop = TRUE]
    data.selected <- data
  } else {
    formula <- formula(formula)
    if (!inherits(formula, "formula")) {
      stop("Error: Invalid formula.")
    }
    data.selected <- parse.formula(formula, data, env = parent.frame())
    response <- data.selected[, 1]
  }
    
  if(is.null(mtry)) {
      mtry <- floor(sqrt(ncol(data.selected) - 1))
  }
  
  ## Check missing values
  if (any(is.na(data.selected))) {
    offending_columns <- colnames(data.selected)[colSums(is.na(data.selected)) > 0]
    stop("Missing data in columns: ",
         paste0(offending_columns, collapse = ", "), ".", call. = FALSE)
  }
  
  ## Outcome must be factor:
  if (!is.factor(response)) {
    stop("Error: Outcome variable needs to be a factor.")
  }
  
  ## The outcome must have a maximum of 20 classes:
  maxlevels <- 20
  if (nlevels(droplevels(response)) > maxlevels)
    stop(paste0("Error: The outcome must have at most ", maxlevels, " classes (categories)."))
  
  ## Check response levels
    if (nlevels(response) != nlevels(droplevels(response))) {
      dropped_levels <- setdiff(levels(response), levels(droplevels(response)))
      warning("Dropped unused factor level(s) in outcome variable: ",
              paste0(dropped_levels, collapse = ", "), ".", call. = FALSE)
    }

  ## Treetype
    if (probability) {
      treetype <- 9
    } else {
      treetype <- 1
    }

  ## Dependent and status variable name. For non-survival dummy status variable name.
  if (!is.null(formula)) {
      dependent.variable.name <- names(data.selected)[1]
    independent.variable.names <- names(data.selected)[-1]
  } else {
    independent.variable.names <- colnames(data.selected)[colnames(data.selected) != dependent.variable.name]
  }
  
  ## Recode characters as factors and recode factors if 'order' mode
  if (!is.matrix(data.selected) && !inherits(data.selected, "Matrix")) {
    character.idx <- sapply(data.selected, is.character)

      ## Recode characters and unordered factors
      names.selected <- names(data.selected)
      ordered.idx <- sapply(data.selected, is.ordered)
      factor.idx <- sapply(data.selected, is.factor)
      independent.idx <- names.selected != dependent.variable.name
      recode.idx <- independent.idx & (character.idx | (factor.idx & !ordered.idx))
      
      ## Numeric response
        num.response <- as.numeric(response)

      ## Recode each column
      data.selected[recode.idx] <- lapply(data.selected[recode.idx], function(x) {
        if (!is.factor(x)) {
          x <- as.factor(x)
        } 
        
        if (nlevels(response) > 2) {
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
  }
  
  ## Input data and variable names, create final data matrix
  if (is.matrix(data.selected) || inherits(data.selected, "Matrix")) {
    data.final <- data.selected
  } else {
    data.final <- data.matrix(data.selected)
  }
  variable.names <- colnames(data.final)
  
    all.independent.variable.names <- independent.variable.names

  ## Error if no covariates
  if (length(all.independent.variable.names) < 1) {
    stop("Error: No covariates found.")
  }
  
  ## Number of trees
  if (!is.numeric(num.trees) || num.trees < 1) {
    stop("Error: Invalid value for num.trees.")
  }
 
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
  
  ## Importance mode To Do:
  if (is.null(importance) || importance == "none") {
    importance.mode <- 0
  } else if (importance == "both") {
    importance.mode <- 6
  } else if (importance == "class-focused") {
    importance.mode <- 7
  } else if (importance == "discriminatory") {
    importance.mode <- 8
  } else {
    stop("Error: Importance mode not supported.")
  }
  
  ## Case weights: NULL for no weights asdf
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
  
  ## Splitting rule
      splitrule <- "gini"
  
  ## Prediction mode always false. Use predict.multifor() method.
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
  
    order.snps <- TRUE
  
  ## Clean up
  # rm("data.selected")

  ## Call C++:
  result <- divforCpp(treetype, dependent.variable.name, data.final, variable.names, mtry=0,
                      num.trees, verbose, seed, num.threads, write.forest, importance.mode,
                      min.node.size, split_select_weights=list(c(0,0)), use_split_select_weights=FALSE,
                      always_split_variable_names=c("0", "0"), use_always_split_variable_names=FALSE,
                      status_variable_name="", prediction.mode, loaded.forest, snp.data,
                      replace, probability, unordered_variable_names=c("0", "0"), use_unordered_variable_names=FALSE, 
                      save.memory, splitrule_r=1, case.weights, use.case.weights, class_weights=rep(1, nlevels(response)), 
                      predict.all, keep.inbag, sample.fraction, alpha=0.5, minprop=0.1, holdout, prediction.type, 
                      num_random_splits=npervar, sparse.data, use.sparse.data, order.snps, oob.error, max.depth, 
                      inbag, use.inbag, nsplits=mtry, npairs=0, proptry=0, divfortype=3, promispairs=list(0,0), 
                      eim_mode=0)
  
  if (length(result) == 0) {
    stop("User interrupt or internal error.")
  }
  
  ## Prepare results
  if (importance.mode != 0) {
    if (importance.mode == 6 || importance.mode == 7) {
	names(result$class_foc_vim) <- all.independent.variable.names
    }
    if (importance.mode == 6 || importance.mode == 8)
    names(result$discr_vim) <- all.independent.variable.names
  }

  ## Set predictions
  if (treetype == 1 && is.factor(response) && oob.error) {
    result$predictions <- integer.to.factor(result$predictions,
                                            levels(response))
    true.values <- integer.to.factor(unlist(data.final[, dependent.variable.name]),
                                     levels(response))
    result$confusion.matrix <- table(true.values, result$predictions, 
                                     dnn = c("true", "predicted"), useNA = "ifany")
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
  } else if (treetype == 9) {
    result$treetype <- "Probability estimation"
  }

  result$call <- sys.call()
  result$importance.mode <- importance
  result$num.samples <- nrow(data.final)
  result$replace <- replace
  
  ## Write forest object
  if (write.forest) {
      result$forest$levels <- levels(response)

    result$forest$independent.variable.names <- independent.variable.names
    result$forest$treetype <- result$treetype
    class(result$forest) <- "multifor.forest"
    
    ## In 'ordered' mode, save covariate levels
    if (respect.unordered.factors == "order" && !is.matrix(data)) {
      result$forest$covariate.levels <- covariate.levels
    }
  }
  
  plotres <- list()
  plotres$data <- data.selected
  plotres$yvarname <- dependent.variable.name
  
  result$plotres <- plotres
  
  # Delete some artefacts which are not needed:
  result$proptry <- NULL
  result$npairs <- NULL
  result$splitrule <- NULL
  
  # "nsplits" is the same as "mtry", remove "mtry" and rename "nsplits" to "mtry:
  result$mtry <- NULL
  names(result)[names(result)=="nsplits"] <- "mtry"
  
  # For classification, there is an element "confusion.matrix", which
  # appears in the resulting list at the wrong place.
  # --> Change the order of the list elements:
  if (result$treetype=="Classification") {
    names_list <- names(result)
    index1 <- which(names_list == "forest")
    index2 <- which(names_list == "confusion.matrix")
    names_list[c(index1, index2)] <- names_list[c(index2, index1)]
    result <- result[names_list]
  }
  
  class(result) <- "multifor"

  
  return(result)
}
