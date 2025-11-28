#' Plot function: Display the influence of a covariate
#'
#' This function takes a dataframe of predictions with one row per prediction
#' and one column for every explanatory variable. Then, those predictions are
#' held constant while one specific variable is varied over it's whole range
#' (min-max). Then, the constant variables with the varied interest variables
#' are predicted and plotted against the expected value and the variance of the
#' underlying distribution.
#'
#' The target of this function is to display the influence of a selected effect
#' on the predicted moments of the modeled distribution. The motivation for
#' computing influences on the moments of a distribution is its
#' interpretability: In most cases, the parameters of a distribution do not
#' equate the moments and as such are only indirectly location, scale or shape
#' properties, making the computed effects hard to understand.
#'
#' Navigating through the disarray of link functions, non-parametric effects and
#' transformations to moments, \code{plot_moments()} supports a wide range of
#' target distributions. See \link{dists} for details.
#'
#' Whether a distribution is supported or not depends on whether the underlying
#' \code{R} object possesses functions to calculate the moments of the
#' distribution from the predicted parameters. To achieve this for as many
#' distributional families as possible, we worked together with both the authors
#' of \link{gamlss} (Rigby and Stasinopoulos 2005) and \link{bamlss} (Umlauf et
#' al. 2018) and implemented the moment functions for almost all available
#' distributions in the respective packages. The \link{betareg} family was
#' implemented in \link{distreg.vis} as well.
#' @references Rigby RA, Stasinopoulos DM (2005). "Generalized Additive Models
#'   for Location, Scale and Shape." Journal of the Royal Statistical Society C,
#'   54(3), 507-554.
#'
#'   Umlauf, N, Klein N, Zeileis A (2018). "BAMLSS: Bayesian
#'   Additive Models for Location, Scale and Shape (and Beyond)." Journal of
#'   Computational and Graphical Statistics, 27(3), 612-627.
#' @param int_var The variable for which influences of the moments shall be
#'   graphically displayed. Has to be in character form.
#' @param pred_data Combinations of covariate data, sometimes also known as
#'   "newdata", including the variable of interest, which will be ignored in
#'   later processing.
#' @param model A fitted model on which the plots are based.
#' @param palette See \code{\link{plot_dist}}.
#' @param ex_fun An external function \code{function(par) {...}} which
#'   calculates a measure, whose dependency from a certain variable is of
#'   interest. Has to be specified in character form. See examples for an
#'   example.
#' @param rug Should the resulting plot be a rug plot?
#' @param samples If the provided model is a bamlss model, should the moment
#'   values be "correctly" calculated, using the transformed samples? See
#'   details for details.
#' @param uncertainty If \code{TRUE}, displays uncertainty measures about the
#'   covariate influences. Can only be \code{TRUE} if samples is also
#'   \code{TRUE}.
#' @param vary_by Variable name in character form over which to vary the
#'   mean/reference values of explanatory variables. It is passed to
#'   \link{set_mean}. See that documentation for further details.
#' @importFrom magrittr %>% extract inset set_colnames set_rownames
#' @import ggplot2
#' @examples
#'
#' # Generating some data
#' dat <- model_fam_data(fam_name = "LOGNO")
#'
#' # Estimating the model
#' library("gamlss")
#' model <- gamlss(LOGNO ~ ps(norm2) + binomial1,
#'                 ~ ps(norm2) + binomial1,
#'                 data = dat, family = "LOGNO")
#'
#' # Get newdata by either specifying an own data.frame, or using set_mean()
#' # for obtaining mean vals of explanatory variables
#' ndata_user <- dat[1:5, c("norm2", "binomial1")]
#' ndata_auto <- set_mean(model_data(model))
#'
#' # Influence graphs
#' plot_moments(model, int_var = "norm2", pred_data = ndata_user) # cont. var
#' plot_moments(model, int_var = "binomial1", pred_data = ndata_user) # discrete var
#' plot_moments(model, int_var = "norm2", pred_data = ndata_auto) # with new ndata
#'
#' # If pred_data argument is omitted plot_moments uses mean explanatory
#' # variables for prediction (using set_mean)
#' plot_moments(model, int_var = "norm2")
#'
#' # Rug Plot
#' plot_moments(model, int_var = "norm2", rug = TRUE)
#'
#' # Different colour palette
#' plot_moments(model, int_var = "binomial1", palette = "Dark2")
#'
#' # Using an external function
#' ineq <- function(par) {
#'   2 * pnorm((par[["sigma"]] / 2) * sqrt(2)) - 1
#' }
#' plot_moments(model, int_var = "norm2", pred_data = ndata_user, ex_fun = "ineq")
#'
#' @export

plot_moments <- function(model, int_var, pred_data = NULL,
                         rug = FALSE, samples = FALSE, uncertainty = FALSE,
                         ex_fun = NULL, palette = "viridis", vary_by = NULL) {

  # Are the moments even implemented?
  if (!has.moments(fam_obtainer(model)))
    stop("The modeled distribution does not have implemented moment functions.")

  # Convert NULL to FALSE
  if (is.null(rug))
    rug <- FALSE
  if (is.null(uncertainty))
    uncertainty <- FALSE
  if (is.null(samples))
    samples <- FALSE

  # Get model data
  m_data <- model_data(model)

  # Ignore interested variable
  pred_data[[int_var]] <- NULL

  # Stop if int_var is not one of colnames of model frame
  if (!int_var %in% colnames(m_data))
    stop("int_var not one of explanatory variables")

  # What type does the variable have?
  if (is.numeric(m_data[[int_var]])) {
    coltype <- "num" # numeric
  } else {
    coltype <- "cat" # categorical
  }

  ## Set the original variables to their mean/reference category ##
  ## if pred_data is not provided ##
  if (is.null(pred_data))
    pred_data <- set_mean(model_data(model), vary_by = vary_by)

  # Make a range of the variable
  if (coltype == "num") {
    vals_seq <- seq(min(m_data[[int_var]]), max(m_data[[int_var]]),
                    length.out = 100)
  } else {
    vals_seq <- unique(m_data[[int_var]])
  }

  # Now vary over all possible vals of interest variable
  pred_data <- pred_data %>%
    inset("prediction", value = rownames(.)) %>%
    extract(rep(row.names(.), each = length(vals_seq)), ) %>%
    inset(int_var, value = vals_seq) %>%
    inset("id", value = row.names(.))

  # Use another function if you have multinomial family
  if (fam_obtainer(model) == "multinomial")
    return(plot_multinom_exp(model, int_var, pred_data, m_data, palette, coltype))

  # Now make predictions and find out expected value and variance
  to_predict <- pred_data[, !colnames(pred_data) %in% c("id", "prediction"), drop = FALSE]

  # Calculate predicted parameters with/without samples
  if (!samples)
    preds <- preds(model, newdata = to_predict, what = "mean")
  if (samples)
    preds <- preds(model, newdata = to_predict, what = "samples")

  # Compute moments
  preds_mean <- moments(
    par = preds,
    fam_name = fam_obtainer(model),
    ex_fun = ex_fun
  )

  # Which params are interesting?
  int_params <- colnames(preds_mean)[colnames(preds_mean) != "id"]

  # Compute empirical quantiles of moments
  if (samples && uncertainty) {
    preds_lowlim <- moments(par = preds, fam_name = fam_obtainer(model),
                            what = "lowerlimit", ex_fun = ex_fun)
    preds_upperlim <- moments(par = preds, fam_name = fam_obtainer(model),
                              what = "upperlimit", ex_fun = ex_fun)
  }

  # Reshape into long
  preds_reshaped_mean <- reshape_into_long(preds_mean, pred_data, int_var,
                                           int_params, samples, coltype)

  # Upper and lower limits for samples
  if (samples && uncertainty) {

    # Lower Limit
    preds_reshaped_lowlim <- reshape_into_long(preds_lowlim, pred_data,
                                               int_var, int_params, samples,
                                               coltype)

    # Upper Limit
    preds_reshaped_upperlim <- reshape_into_long(preds_upperlim, pred_data,
                                                 int_var, int_params, samples,
                                                 coltype)

    # Merge it with both
    preds_reshaped_mean$lowerlim <- preds_reshaped_lowlim$value
    preds_reshaped_mean$upperlim <- preds_reshaped_upperlim$value
  }

  # Rename factor of moments
  preds_reshaped_mean$moment <- factor(preds_reshaped_mean$moment,
                                       levels = c("Expected_Value",
                                                  "Variance",
                                                  ex_fun))

  # Now make plot
  ground <- ggplot(preds_reshaped_mean,
                   aes_string(x = int_var, y = "value", col = "prediction")) +
    facet_wrap(~moment, scales = "free") +
    theme_bw() +
    labs(y = "Moment values") +
    ggtitle(paste("Influence of", int_var,
                  "on parameters of modeled distribution"))

  # Palettes
  if (palette == "viridis") {
    ground <- ground +
      scale_fill_viridis_d() +
      scale_colour_viridis_d()
  } else if (palette != "default") {
    ground <- ground +
      scale_fill_brewer(palette = palette) +
      scale_colour_brewer(palette = palette)
  }

  # Line if numeric
  if (coltype == "num") {
    plot <- ground +
      geom_line()

    # Add uncertainty measure
    if (samples && uncertainty)
      plot <- plot + geom_ribbon(data = preds_reshaped_mean,
                                 aes_string(ymin = "lowerlim",
                                            ymax = "upperlim",
                                            fill = "prediction"),
                                 alpha = 0.15, linetype = 2, col = NA)

  } else if (coltype == "cat") {
    plot <- ground +
      geom_bar(aes_string(fill = "prediction"),
               stat = "identity",
               position = position_dodge(),
               col = "black")

    # Add uncertainty measure
    if (samples && uncertainty)
      plot <- plot +
        geom_errorbar(aes_string(ymin = "lowerlim", ymax = "upperlim"),
                      alpha = 0.6,
                      position = position_dodge2(.05),
                      col = "black")
  }

  # Add rug if wanted
  if (rug && coltype == "num") {
    var <- model_data(model, varname = int_var)
    plot <- plot +
      geom_rug(data =
                 data.frame(var,
                            prediction =
                              unique(preds_reshaped_mean$prediction)[1],
                            value = 0),
               aes_string(x = "var"),
               sides = "b", alpha = 0.5, color = "black")
  }

  # Don't display rug in discrete cases
  if (rug && coltype == "cat") {
    stop("Cannot display a rug plot in discrete cases")
  }

  # Return plot here
  return(plot)
}


#' Internal: Plot function as sub-case to plot_moments for
#'   multinomial family
#'
#' @import ggplot2
#' @importFrom stats model.frame
#' @importFrom magrittr %>% inset extract set_rownames set_colnames
#' @keywords internal

plot_multinom_exp <- function(model, int_var, pred_data, m_data, palette, coltype) {

  # Special case of multinomial requires m_data attached with dependent variable
  m_data <- model.frame(model)

  # Get predictions for each class dep on int_var
  preds <- pred_data[, !colnames(pred_data) %in% c("id", "prediction")] %>%
    preds(model, newdata = .) %>%
    mult_trans(., model) %>%
    inset("id", value = row.names(.))
  classes <- as.character(unique(m_data[, 1]))
  preds <- preds %>%
    merge(y = pred_data, by.x = "id") %>%
    extract(, c(int_var, classes, "prediction")) %>%
    set_colnames(c(int_var, paste0("c.", classes), "prediction")) %>%
    reshape(., direction = "long",
            varying = seq_len(length(classes)) + 1,
            idvar = c(int_var, "prediction")) %>% # because classes start with second column
    set_colnames(c(int_var, "prediction", "class", "value")) %>%
    set_rownames(seq_len(nrow(.)))
  preds$class <- factor(preds$class, levels = levels(m_data[, 1]))

  # Numerical influence plot
  if (coltype == "num") {
    ground <- ggplot(preds, aes_string(x = int_var, y = "value", fill = "class")) +
      geom_area() +
      facet_wrap(~prediction) +
      labs(y = "Expected value of class") +
      ggtitle(paste("Influence of", int_var,
                    "on expected values of every class' pi_i")) +
      theme_bw()
  }
  # Categorical influence plot
  if (coltype == "cat") {
    ground <- ggplot(preds, aes_string(x = int_var, y = "value", fill = "class")) +
      geom_bar(stat = "identity", position = position_dodge()) +
      facet_wrap(~prediction) +
      labs(y = "Expected value of class") +
      ggtitle(paste("Influence of", int_var,
                    "on expected values of every class' pi_i")) +
      theme_bw()
  }

  # Palettes
  if (palette == "viridis") {
    ground <- ground +
      scale_fill_viridis_d() +
      scale_colour_viridis_d()
  } else if (palette != "default") {
    ground <- ground +
      scale_fill_brewer(palette = palette) +
      scale_colour_brewer(palette = palette)
  }
  return(ground)
}

#' Internal: Reshape into Long Format
#'
#' @param preds_intvar A data.frame with the moments as columns and the splitted
#'   \code{int_var} as rows (default 100 values from min to max). There are
#'   three ways which this data.frame can look like. First, as the mean of the
#'   moments. secondly, as the upper and thirdly as the lower quantiles of the
#'   moments.
#' @keywords internal

reshape_into_long <- function(preds_intvar, pred_data, int_var, int_params, samples, coltype) {

  # Put id as extra var
  preds_intvar$id <- row.names(preds_intvar)

  # Merge predictions with pred_data, transform into long and to numerics
  preds_reshaped <- preds_intvar %>%
    merge(y = pred_data, by.x = "id") %>%
    extract(, c(int_var, "prediction", int_params)) %>%
    set_colnames(c(int_var, "prediction", paste0("mom.", int_params))) %>%
    reshape(., direction = "long",
            varying = seq_along(int_params) + 2, # because moments start after 2
            idvar = c(int_var, "prediction")) %>%
    set_rownames(seq_along(rownames(.))) %>%
    set_colnames(c(int_var, "prediction", "moment", "value"))

  if (coltype == "num") { # if we have a numeric column trans to num
    preds_reshaped[[int_var]] <- as.numeric(preds_reshaped[[int_var]])
  } else if (coltype == "cat") { # if not then make character
    preds_reshaped[[int_var]] <- as.character(preds_reshaped[[int_var]])
  }

  # Return here
  return(preds_reshaped)
}
