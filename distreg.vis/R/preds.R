#' Predict parameters of a distreg models' target distribution
#'
#' This function takes a fitted model and a dataframe with explanatory variables
#' and a column for the intercept to compute predicted parameters for the
#' specified distribution. Without worrying about class-specific function
#' arguments, \code{preds()} offers a consistent way of obtaining predictions
#' based on specific covariate combinations.
#'
#' @param model A fitted distributional regression model object. Check supported
#'   classes at \link{distreg_checker}.
#' @param newdata A data.frame with explanatory variables as columns, and rows
#'   with the combinations you want to do predictions for. Furthermore, whether
#'   or not to include the intercept has to be specified via a logical variable
#'   \code{intercept}. If omitted, the average of the explanatory variables is
#'   used (see \link{set_mean}).
#' @param what One of \code{"mean"} or \code{"samples"}. The default for bamlss
#'   models is "samples", while the default for gamlss models is "mean". This
#'   argument changes how the mean of the parameter is calculated. See details
#'   for details.
#' @param vary_by Variable name in character form over which to vary the
#'   mean/reference values of explanatory variables. It is passed to
#'   \link{set_mean}. See that documentation for further details.
#' @examples
#' # Generating data
#' data_fam <- model_fam_data(fam_name = "BE")
#'
#' # Fit model
#' library("gamlss")
#' beta_model <- gamlss(BE ~ norm2 + binomial1,
#'   data = data_fam, family = BE())
#'
#' # Get 3 predictions
#' ndata <- data_fam[sample(1:nrow(data_fam), 3), c("binomial1", "norm2")]
#' preds(model = beta_model, newdata = ndata)
#'
#' # If newdata argument is omitted preds uses the means of the explanatory variables
#' preds(model = beta_model, newdata = NULL) # this gives the same results as ...
#' preds(model = beta_model, newdata = set_mean(model_data(beta_model))) # ...this
#'
#' @return A data.frame with one column for every distributional parameter and a
#'   row for every covariate combination that should be predicted.
#' @importFrom stats na.omit predict
#' @importFrom gamlss predictAll
#' @export

preds <- function(model, newdata = NULL, what = "mean", vary_by = NULL) {

  # Check and convert to data.frame (necessary for tibble-like datasets)
  if (is(newdata, "data.frame"))
    newdata <- as.data.frame(newdata)
  if (!is.null(newdata) && !is(newdata, "data.frame"))
    stop("Newdata has to be in a data.frame format")

  # If newdata is NULL set model data to mean (like in plot_moments)
  if (is.null(newdata))
    newdata <- set_mean(
      model_data(model),
      vary_by = vary_by
    )

  # Omit NA's in prediction data
  newdata <- na.omit(newdata)

  # Rownames
  rnames <- row.names(newdata)

  if (is(model, "gamlss")) {

    # Only mean possible to be calculated
    if (what != "mean")
      stop("In gamlss models there are no samples")

    # Predicted parameters - gamlss
    pred_par <-
      as.data.frame(predictAll(model, newdata = newdata, type = "response",
                               data = model_data(model, incl_dep = TRUE)),
                               row.names = rnames)
    pred_par <- pred_par[, !colnames(pred_par) %in% "y", drop = FALSE] # goddamn

  } else if (is(model, "bamlss")) {

    if (what == "mean") {
      pred_par <-
        as.data.frame(predict(model, newdata = newdata, drop = FALSE,
                              type = "parameter", intercept = TRUE),
                      row.names = rnames)
    }

    if (what == "samples") {
      # Predicted parameters - bamlss - mean
      samples_raw <-
        predict(model, newdata = newdata, drop = FALSE,
                type = "parameter", intercept = TRUE,
                FUN = function(x) return(x))
      pred_par <- preds_transformer(samples_raw, newdata = newdata)
    }

  } else if (is(model, "betareg") | is(model, "betatree")) {
    if (what == "mean") {
      pred_par <- data.frame(
        mu = stats::predict(model, newdata = newdata, type = "response"),
        phi = stats::predict(model, newdata = newdata, type = "precision"))
    } else if (what != "mean") {
      stop("betareg uses ML, so no samples available.")
    }


  } else {
    stop("Model class is not supported, so can't make predictions. \n
         See ?distreg_checker for supported models")
  }

  # Return it here
  return(pred_par)
}

#' Sample transformer
#'
#' This functions transforms the bamlss samples from a list for every parameter
#' to a list for every prediction. This makes it easier to use the moments()
#' function.
#'
#' @keywords internal

preds_transformer <- function(samp_list, newdata) {

  # Names of parameters
  params <- names(samp_list)

  # Transpose matrices
  samp_list <- lapply(samp_list, FUN = t)

  # Make colnames
  samp_list <- lapply(samp_list, function(x) {
    colnames(x) <- row.names(newdata)
    x <- as.data.frame(x)
    return(x)
  })

  # Reshape
  resh <- lapply(seq_len(nrow(newdata)), FUN = function(num) {
    sapply(samp_list, function(df) {
      return(df[, num])
    })
  })
  names(resh) <- row.names(newdata)

  # Give back result
  return(resh)
}

