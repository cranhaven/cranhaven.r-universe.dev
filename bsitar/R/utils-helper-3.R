

#' An internal function to set up data for \code{bsitar} model
#'
#' @param data An input data frame.
#'
#' @param x The predictor (typically age) variables.
#'
#' @param y The outcome variables. Must be a single name except when fitting a
#'   multivariate model.
#'
#' @param id The group identifier.
#'
#' @param uvarby An optional (default \code{NA}) to specify the indicator
#'   variable for fitting univariate-by-subgroup model. See \code{univariate_by}
#'   argument in the [bsitar::bsitar()] function. If not \code{NA}, then it should
#'   be a valid factor variable present in the \code{data}.
#'
#' @param mvar A logical (default \code{FALSE}) to specify the the multivariate
#'   model. See \code{multivariate} argument in the [bsitar::bsitar()] function.
#'
#' @param xfuns Optional name(s) of the transformation function(s) applied to
#'   the predictor variable (typically age). Default \code{NULL}.
#'
#' @param yfuns Optional name(s) of the transformation function(s) applied to
#'   the outcome variable. Default \code{NULL}.
#'
#' @param outliers An optional (default \code{NULL}) to remove velocity
#'   outliers. The argument should be a named list to pass options to the
#'   [bsitar::outliers()] function. See [bsitar::outliers()] for details.
#'   
#' @param subset A logical (default \code{TRUE}) to indicate whether to create
#'   data for each level of the \code{univariate_by} variable, or only for a
#'   subset of levels. The \code{subset = TRUE} is typically used during model
#'   fit and \code{subset = FALSE} during post processing of each sub model. The
#'   argument \code{subset} is ignored when \code{univariate_by} is \code{NA} or
#'   \code{NULL}.
#' 
#' @return A data frame with necessary information added a attributes.
#' 
#' @author Satpal Sandhu  \email{satpal.sandhu@bristol.ac.uk}
#'
#' @keywords internal
#' @noRd
#' 
prepare_data <- function(data,
                         x,
                         y,
                         id,
                         uvarby = NA,
                         mvar = FALSE,
                         xfuns = NULL,
                         yfuns = NULL,
                         outliers = NULL,
                         subset = TRUE) {
  
  data <- data %>% droplevels()
  
  if (!is.null(outliers)) {
    remove_ <- outliers$remove
    icode_ <- outliers$icode
    icode_ <- deparse(substitute(icode_))
    limit_ <- outliers$limit
    velpower_ <- outliers$velpower
    lag_ <- outliers$lag
    linearise_ <- outliers$linearise
    verbose_ <- outliers$verbose
    
    for (yi in 1:length(y)) {
      if (!y[yi] %in% colnames(data)) {
        stop(
          "When model is fit with argument outliers (i.e., outliers not NULL), ",
          "\n",
          "  then outcome variable should be part of the newdata specified.",
          "\n",
          "  please check the missing outcome varibale: ",
          y[yi]
        )
      }
      if (!x[yi] %in% colnames(data)) {
        stop(
          "When model is fit with argument outliers (i.e., outliers not NULL),",
          " \n ",
          "  then predictor variable should be part of the newdata specified.",
          "\n",
          "  please check the missing predictor varibale: ",
          x[yi]
        )
      }
      if (!id[yi] %in% colnames(data)) {
        stop(
          "When model is fit with argument outliers 
          (i.e., outliers not NULL), ",
          "\n",
          "  then group identifier variable should be 
          part of the newdata specified.",
          "\n",
          "  please check the missing group identifier varibale: ",
          id[yi]
        )
      }
      data <-
        outliers(
          x = x[yi],
          y =  y[yi],
          id = id[yi],
          data = data,
          icode = icode_,
          lag = lag_,
          velpower = velpower_,
          limit = limit_,
          linearise = linearise_,
          remove = remove_,
          verbose = verbose_
        )
      
    }
  } # if(!is.null(outliers)) {
  
  org.data <- data
  
  # Note that x tarnsformation is done within the prepare_function
  transform_y <- function(y, yfuns) {
    for (myfunsi in 1:length(y)) {
      mysi <- y[[myfunsi]]
      myfunsi <- yfuns[[myfunsi]]
      if (grepl('.Primitive', myfunsi, fixed = T) &
          grepl('log', myfunsi, fixed = T)) {
        myfunsi <- 'log'
      }
      if (grepl('.Primitive', myfunsi, fixed = T) &
          grepl('sqrt', myfunsi, fixed = T)) {
        myfunsi <- 'sqrt'
      }
      if (myfunsi == 'log')
        if(!is.null(data[[mysi]])) data[[mysi]] <- log(data[[mysi]])
      if (myfunsi == 'sqrt')
        if(!is.null(data[[mysi]])) data[[mysi]] <- sqrt(data[[mysi]])
    }
    return(data)
  }
  
  if (!(is.na(uvarby) | uvarby == "NA")) {
    if (!uvarby %in% colnames(data)) {
      stop(paste(
        "\nvariable",
        uvarby,
        "used for setting univariate submodels is missing"
      ))
    }
    if (!is.factor(data[[uvarby]])) {
      stop("subset by variable '",
           uvarby,
           "' should be a factor variable")
    }
    for (l in levels(data[[uvarby]])) {
      if(!subset) data[[l]] <- data[[y[1]]]
      if(subset) data[[l]]  <- data[[l]]
    }
    unibyimat <-
      model.matrix(~ 0 + eval(parse(text = uvarby)), data)
    subindicators <- paste0(uvarby, levels(data[[uvarby]]))
    colnames(unibyimat) <- subindicators
    y <- levels(data[[uvarby]])
    data <- as.data.frame(cbind(data, unibyimat))
    data <- transform_y(y, yfuns)
    attr(data, "ys") <- y
    attr(data, "multivariate") <- FALSE
    attr(data, "uvarby") <- uvarby
    attr(data, "subindicators") <- subindicators
    # data_out <- data
  } else if (mvar) {
    data <- org.data
    data <- transform_y(y, yfuns)
    attr(data, "ys") <- y
    attr(data, "multivariate") <- TRUE
    attr(data, "uvarby") <- NULL
    attr(data, "subindicators") <- NULL
  } else {
    # data_out <- org.data
    data <- org.data
    data <- transform_y(y, yfuns)
    attr(data, "ys") <- y
    attr(data, "multivariate") <- FALSE
    attr(data, "uvarby") <- NULL
    attr(data, "subindicators") <- NULL
  }
  return(data)
}



