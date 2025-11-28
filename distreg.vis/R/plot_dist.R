#' Plot predicted distributional regression models
#'
#' This function plots the parameters of a predicted distribution (e.g. obtained
#' through \code{\link{preds}}) with ggplot2. You can use all supported
#' distributional regression model classes (check details of
#' \link{distreg_checker}) as well as all supported distributional families
#' (available at \link{dists}).
#'
#' @details To get a feel for the predicted distributions and their differences,
#'   it is best to visualize them. In combination with the obtained parameters
#'   from \link{preds}, the function \code{plot_dist()} looks for the necessary
#'   distribution functions (probability density function or cumulative
#'   distribution function) from the respective packages and then displays them
#'   graphically.
#'
#'   After \code{plot_dist()} has received all necessary arguments, it executes
#'   validity checks to ensure the argument's correct specification. This
#'   includes controlling for the correct \code{model} class, checking whether
#'   the distributional family can be used safely and whether cdf or pdf
#'   functions for the modeled distribution are present and ready to be
#'   graphically displayed. If this is the case, the internal
#'   \link{fam_fun_getter} is used to create a list with two functions pointing
#'   to the correct pdf and cdf functions in either the \link{gamlss} or
#'   \link{bamlss} namespace. The functions for \link{betareg} are stored in
#'   \link{distreg.vis}.
#'
#'   Following a successful calculation of the plot limits, the graph itself can
#'   be created. Internally, \link{distreg.vis} divides between continuous,
#'   discrete and categorical distributions. Continuous distributions are
#'   displayed as filled line plots, while discrete and categorical
#'   distributions take bar graph shapes.
#'
#'   For plotting, \link{distreg.vis} relies on the \link{ggplot2} package
#'   (Wickham 2016). After an empty graph is constructed, the previously
#'   obtained cdf or pdf functions are evaluated for each predicted parameter
#'   combination and all values inside the calculated plot limits.
#'
#' @references Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis.
#'   Springer-Verlag New York. ISBN 978-3-319-24277-4.
#'   \url{https://ggplot2.tidyverse.org}.
#'
#' @param model A fitted distributional regression model object. Check
#'   \link{distreg_checker} to see which classes are supported.
#' @param pred_params A data.frame with rows for every model prediction and
#'   columns for every predicted parameter of the distribution. Is easily
#'   obtained with the \code{distreg.vis} function \code{\link{preds}}.
#' @param palette The colour palette used for colouring the plot. You can use
#'   any of the ones supplied in \code{\link[ggplot2]{scale_fill_brewer}} though
#'   I suggest you use one of the qualitative ones: Accent, Dark2, etc. Since
#'   0.5.0 \code{"viridis"} is included, to account for colour blindness.
#' @param type Do you want the probability distribution function ("pdf") or the
#'   cumulative distribution function ("cdf")?
#' @param rug If TRUE, creates a rug plot
#' @param vary_by Variable name in character form over which to vary the
#'   mean/reference values of explanatory variables. It is passed to
#'   \link{set_mean}. See that documentation for further details.
#' @param newdata A data.frame object being passed onto \link{preds}. You can do
#'   this if you don't want to specify the argument \code{pred_params} directly.
#'   If you specify \code{newdata}, then \code{preds(model, newdata = newdata)}
#'   is going to be executed to be used as \code{pred_params}.
#' @return A ggplot2 object.
#' @examples
#' # Generating data
#' data_fam <- model_fam_data(fam_name = "BE")
#'
#' # Fit model
#' library("gamlss")
#' beta_model <- gamlss(BE ~ norm2 + binomial1,
#'   data = data_fam, family = BE())
#'
#' # Obtains all explanatory variables and set them to the mean, varying by binomial1
#' # (do this if you do not want to specify ndata of preds by yourself)
#' ndata <- set_mean(model_data(beta_model), vary_by = "binomial1")
#'
#' # Obtain predicted parameters
#' param_preds <- preds(beta_model, newdata = ndata)
#'
#' # Create pdf, cdf plots
#' plot_dist(beta_model, param_preds, rug = TRUE)
#' plot_dist(beta_model, param_preds, type = "cdf")
#' plot_dist(beta_model, param_preds, palette = 'default')
#'
#' # You can also let plot_dist do the step of predicting parameters of the mean explanatory variables:
#' plot_dist(beta_model, pred_params = NULL, vary_by = 'binomial1')
#' @export

plot_dist <- function(model, pred_params = NULL, palette = "viridis",
                      type = "pdf", rug = FALSE, vary_by = NULL,
                      newdata = NULL) {

  # Check whether the function is even applied to the right classes
  if (!distreg_checker(model))
    stop("This tool only works for model certain classes. \n Execute ?distreg_checker to find out which ones are currently supported")

  # Compute mean values of expl variables if no pred_params is provided
  if (is.null(pred_params) && is.null(newdata))
    pred_params <- preds(model, vary_by = vary_by)

  if (is.null(pred_params) && !is.null(newdata))
    pred_params <- preds(model, newdata = newdata, vary_by = vary_by)

  # Get right family
  fam_name <- fam_obtainer(model)

  # Obtain dependent variable
  if (rug)
    depvar <- model_data(model, dep = TRUE)
  else
    depvar <- NULL

  # Check here whether distribution is even implemented
  if (!is.implemented(fam_name))
    stop("Family not implemented")

  # Check here whether pred_params is in correct form
  if (!is(pred_params, "data.frame"))
    stop("Argument pred_params has to be in data.frame form")

  # Get correct pdf and cdf functions - fam_fun_getter should also check whether a cdf is even available
  if (is.continuous(fam_name))
    funs_list <- list(pdf = fam_fun_getter(fam_name, "d"),
                      cdf = fam_fun_getter(fam_name, "p"))

  # Get correct limits
  lims <- limits(fam_name, pred_params)

  # Different plots depending on type of distribution
  if (is.continuous(fam_name))
    plot <- pdfcdf_continuous(lims, funs_list, type, pred_params, palette, depvar)
  else if (!is.continuous(fam_name))
    plot <- pdfcdf_discrete(pred_params, palette, fam_name, type, model, lims, depvar)

  # Return it
  return(plot)
}

#' Internal: Create the pdf/cdf for continuous covariates
#'
#' Returns a plot
#' @import ggplot2
#' @keywords internal

pdfcdf_continuous <- function(lims, funs, type, p_m, palette, depvar) {

  if (type == "cdf") {
    # Assemble plot
    ground <- ggplot(data = data.frame(y = c(0, 1), x = lims),
                     aes_string(x = "x", y = "y")) +
      ggtitle("Predicted distribution(s)") +
      labs(x = "y", y = "F(y)")

    # Add functions
    for (i in seq_len(nrow(p_m))) {
      args <- p_m[i, , drop = FALSE]
      ground <- ground +
        stat_function(fun = funs$cdf, args = list(par = as.list(args)),
                      geom = "line", aes_(col = row.names(args)))
    }
  } else if (type == "pdf") {
    # Assemble plot
    ground <- ggplot(data = data.frame(x = lims), aes_string(x = "x")) +
      ggtitle("Predicted distribution(s)") +
      labs(x = "y", y = "f(y)")

    # Add functions
    for (i in seq_len(nrow(p_m))) {
      args <- p_m[i, , drop = FALSE]
      ground <- ground +
        stat_function(fun = funs$pdf, args = list(par = as.list(args)),
                      geom = "area", aes_(fill = row.names(args)), alpha = 0.7)
    }
  }

  # Different theme
  ground <- ground + theme_classic()

  # Colour Palettes - if not default or viridis
  if (palette == "viridis") {
    ground <- ground +
      scale_fill_viridis_d() +
      scale_colour_viridis_d()
  } else if (palette != "default") {
    ground <- ground +
      scale_fill_brewer(palette = palette) +
      scale_colour_brewer(palette = palette)
  }


  # Make legend title
  if (type == "pdf") {
    ground$labels$fill <- "Predictions"
  } else if (type == "cdf") {
    ground$labels$colour <- "Predictions"
  }

  # This rug really ties the room together
  if (!is.null(depvar)) {
    ground <- ground +
      geom_rug(data = data.frame(depvar),
               aes_string(y = "0",
                          x = "depvar"),
               sides = "b")
  }

  # Return plot
  return(ground)
}

#' Internal: Create the pdf/cdf for discrete covariates
#'
#' Returns a plot
#' @import ggplot2
#' @keywords internal

pdfcdf_discrete <- function(pred_params, palette, fam_name, type, model, lims, depvar) {

  # Transform discrete predictions
  pred_df <- disc_trans(pred_params, fam_name, type, model, lims)

  if (type == "pdf") {
    # Assemble plot
    ground <- ggplot(pred_df, aes_string(x = "xvals", y = "value", fill = "rownames")) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(x = "y", y = "f(y)") +
      ggtitle("Predicted distributions(s)")

    # Classic theme
    ground <- ground + theme_classic()

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

    # Legend label
    ground$labels$fill <- "Predictions"

    # This rug really ties the room together
    if (!is.null(depvar) & fam_name != "multinomial") {
      suppressWarnings({
        ground <- ground +
          geom_rug(data = data.frame(depvar, fill = unique(pred_df$rownames)[1]), # weird bug with rownames not specified
                   aes_string(y = "1L",
                              x = "depvar",
                              fill = "fill"),
                   sides = "b", position = "jitter",
                   alpha = 0.5)
      })
    }

  } else if (type == "cdf") {

    if (fam_name == "multinomial")
      stop("CDF for multinomial dist not feasible")

    # Assemble plot
    ground <- ggplot(pred_df, aes_string("xvals", "value", col = "rownames")) +
      geom_step(linetype = 2) +
      labs(x = "y", y = "F(y)") +
      ggtitle("Predicted distribution(s)")

    # Classic theme
    ground <- ground + theme_classic()

    # Palette
    if (palette == "viridis") {
      ground <- ground +
        scale_fill_viridis_d() +
        scale_colour_viridis_d()
    } else if (palette != "default") {
      ground <- ground +
        scale_fill_brewer(palette = palette) +
        scale_colour_brewer(palette = palette)
    }

    # Legend label
    ground$labels$colour <- "Predictions"

    # This rug really ties the room together
    if (!is.null(depvar)) {
      suppressWarnings({
        ground <- ground +
          geom_rug(data = data.frame(depvar, colour = unique(pred_df$rownames)[1]), # weird bug with rownames not specified
                   aes_string(y = "1L",
                              x = "depvar",
                              colour = "colour"),
                   sides = "b", position = "jitter",
                   alpha = 0.5, colour = "black")
      })
    }
  }

  # Return plot
  return(ground)
}
