#' @title Plot the histogram or density of the Conditional Average Treatment Effect
#' @description Plot the conditional average treatment effect (CATE) of a 'bartCause' model.
#' The conditional average treatment effect is derived from taking the difference between
#' predictions for each individual under the control condition and under the treatment condition averaged over the population.
#' Means of the CATE distribution will resemble SATE and PATE but the CATE distribution accounts for more uncertainty than SATE and less uncertainty than PATE.
#'
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param type histogram or density
#' @param ci_80 TRUE/FALSE. Show the 80\% credible interval?
#' @param ci_95 TRUE/FALSE. Show the 95\% credible interval?
#' @param reference numeric. Show a vertical reference line at this value
#' @param .mean TRUE/FALSE. Show the mean reference line
#' @param .median TRUE/FALSE. Show the median reference line
#'
#'
#' @author George Perrett, Joseph Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 bartCause
#' @examples
#' \donttest{
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSup.rule = 'none'
#' )
#' plot_CATE(model_results)
#' }
plot_CATE <- function(.model, type = c('histogram', 'density'), ci_80 = FALSE, ci_95 = FALSE, reference = NULL, .mean = FALSE, .median = FALSE){

  validate_model_(.model)
  type <- tolower(type[1])
  if (type %notin% c('histogram', 'density')) stop("type must be 'histogram' or 'density'")

  # set title
  .title <- switch(
    .model$estimand,
    ate = "Posterior of Average Treatment Effect",
    att = "Posterior of Average Treatment Effect of the Treated",
    atc = "Posterior of Average Treatment Effect of the Control"
  )

  # calculate stats
  pate <- bartCause::extract(.model, 'cate')
  pate <- as.data.frame(pate)
  ub <- quantile(pate$pate, 0.9)
  lb <- quantile(pate$pate, 0.1)
  ub.95 <- quantile(pate$pate, 0.975)
  lb.95 <- quantile(pate$pate, 0.025)
  dd <- density(pate$pate)
  dd <- with(dd, data.frame(x, y))

  # build base plot
  p <- ggplot(pate, aes(pate)) +
    scale_linetype_manual(values = c(2, 3)) +
    theme(legend.title = element_blank()) +
    labs(title = .title,
         x = toupper(.model$estimand))

  # histogram
  if (type == 'histogram'){
    p <- p +
      geom_histogram(fill = 'grey60') +
      labs(y = 'Frequency')

    # add credible intervals
    if (isTRUE(ci_95)) p <- p + geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 3, color = 'grey10')
    if (isTRUE(ci_80)) p <- p + geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 1.5, color = 'grey25')
  }

  # density
  if (type == 'density'){
    p <- p +
      geom_density() +
      labs(y = 'Density',
           linetype = NULL)

    # add credible intervals
    if (isTRUE(ci_95)){
      p <- p +
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(x = x, y = y, ymax = y),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8)
    }
    if (isTRUE(ci_80)){
      p <- p +
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(x = x, y = y, ymax = y),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8)
    }
  }

  # add reference lines
  if (isTRUE(.mean)) p <- p + geom_vline(data = pate, aes(xintercept = mean(pate), linetype = 'mean'))
  if (isTRUE(.median)) p <- p + geom_vline(data = pate, aes(xintercept = median(pate), linetype = 'median'))
  if (!is.null(reference)) p <- p + geom_vline(xintercept = reference)

  return(p)
}


#' @title Plot Individual Conditional Average Treatment effects
#' @description Plots a histogram of Individual Conditional Average Treatment effects (ICATE).
#' ICATEs are the difference in each individual's predicted outcome under the treatment and predicted outcome under the control averaged over the individual.
#' Plots of ICATEs are useful to identify potential heterogeneous treatment effects between different individuals. ICATE plots can be grouped by discrete variables.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param .group_by a grouping variable as a vector
#' @param n_bins number of bins
#' @param .alpha transparency of histograms
#'
#' @author George Perrett
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr bartCause
#'
#' @examples
#' \donttest{
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSup.rule = 'none'
#' )
#' plot_ICATE(model_results, lalonde$married)
#' }
plot_ICATE <- function(.model, .group_by = NULL, n_bins = 30, .alpha = .7){

  validate_model_(.model)
  if (!is.null(.group_by)) is_discrete_(.group_by)

  posterior <- bartCause::extract(.model, 'icate')
  icates <- as_tibble(apply(posterior, 2, mean))

  # adjust value based on estimand
  .group_by <- adjust_for_estimand_(.model, .group_by)

  # create base plot
  p <- ggplot(icates, aes(x = value)) +
    geom_histogram(bins = n_bins)

  # add grouping
  if(!is.null(.group_by)){
    p <- ggplot(data = icates,
                aes(x = value, fill = as.factor(.group_by))) +
      geom_histogram(position = 'identity', bins = n_bins, alpha = .alpha)
  }

  # add labels
  p <- p +
    labs(title = NULL,
         x = NULL,
         y = 'Count',
         fill = NULL)

  return(p)
}

#' @title Plot histogram or density of Population Average Treatment Effect
#' @description Plot shows the Population Average Treatment Effect which is derived from the posterior predictive distribution of the difference between \eqn{y | z=1, X} and \eqn{y | z=0, X}.
#' Mean of PATE will resemble CATE and SATE but PATE will account for more uncertainty and is recommended for informing inferences on the average treatment effect.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param type histogram or density
#' @param ci_80 TRUE/FALSE. Show the 80\% credible interval?
#' @param ci_95 TRUE/FALSE. Show the 95\% credible interval?
#' @param reference numeric. Show a vertical reference line at this value
#' @param .mean TRUE/FALSE. Show the mean reference line
#' @param .median TRUE/FALSE. Show the median reference line
#'
#' @author George Perrett, Joseph Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 bartCause
#' @examples
#' \donttest{
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSup.rule = 'none'
#' )
#' plot_PATE(model_results)
#' }
plot_PATE <- function(.model, type = c('histogram', 'density'), ci_80 = FALSE, ci_95 = FALSE, reference = NULL, .mean = FALSE, .median = FALSE){

  validate_model_(.model)
  type <- tolower(type[1])
  if (type %notin% c('histogram', 'density')) stop("type must be 'histogram' or 'density'")

  # set title
  .title <- switch(
    .model$estimand,
    ate = "Posterior of Average Treatment Effect",
    att = "Posterior of Average Treatment Effect of the Treated",
    atc = "Posterior of Average Treatment Effect of the Control"
  )

  # calculate stats
  pate <- bartCause::extract(.model)
  pate <- as.data.frame(pate)
  ub <- quantile(pate$pate, .9)
  lb <- quantile(pate$pate, .1)
  ub.95 <- quantile(pate$pate, .975)
  lb.95 <- quantile(pate$pate, .025)
  dd <- density(pate$pate)
  dd <- with(dd, data.frame(x, y))

  # build base plot
  p <- ggplot(pate, aes(pate)) +
    scale_linetype_manual(values = c(2, 3)) +
    theme(legend.title = element_blank()) +
    labs(title = .title,
         x = toupper(.model$estimand))

  # histogram
  if (type == 'histogram'){
    p <- p +
      geom_histogram(fill = 'grey60') +
      labs(y = 'Frequency')

    # add credible intervals
    if (isTRUE(ci_95)) p <- p + geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 3, color = 'grey10')
    if (isTRUE(ci_80)) p <- p + geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 1.5, color = 'grey25')
  }

  # density
  if (type == 'density'){
    p <- p +
      geom_density() +
      labs(y = 'Density')

    # add credible intervals
    if (isTRUE(ci_95)){
      p <- p +
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(x = x, y = y, ymax = y),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8)
    }
    if (isTRUE(ci_80)){
      p <- p +
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(x = x, y = y, ymax = y),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8)
    }
  }

  # add reference lines
  if (isTRUE(.mean)) p <- p + geom_vline(data = pate, aes(xintercept = mean(pate), linetype = 'mean'))
  if (isTRUE(.median)) p <- p + geom_vline(data = pate, aes(xintercept = median(pate), linetype = 'median'))
  if (!is.null(reference)) p <- p + geom_vline(xintercept = reference)

  return(p)
}

#' @title Plot histogram or density of Sample Average Treatment Effects
#' @description Plot a histogram or density of the Sample Average Treatment Effect (SATE). The Sample Average Treatment Effect is derived from taking the difference of each individual's observed outcome and a predicted counterfactual outcome from a BART model averaged over the population.
#' The mean of SATE will resemble means of CATE and PATE but will account for the least uncertainty.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param type histogram or density
#' @param ci_80 TRUE/FALSE. Show the 80\% credible interval?
#' @param ci_95 TRUE/FALSE. Show the 95\% credible interval?
#' @param reference numeric. Show a vertical reference line at this x-axis value
#' @param .mean TRUE/FALSE. Show the mean reference line
#' @param .median TRUE/FALSE. Show the median reference line
#'
#' @author George Perrett, Joseph Marlo
#'
#' @return ggplot object
#' @export
#' @import ggplot2 bartCause
#' @examples
#' \donttest{
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSup.rule = 'none'
#' )
#' plot_SATE(model_results)
#' }
plot_SATE <- function(.model, type = c('histogram', 'density'), ci_80 = FALSE, ci_95 = FALSE, reference = NULL, .mean = FALSE, .median = FALSE){

  validate_model_(.model)
  type <- tolower(type[1])
  if (type %notin% c('histogram', 'density')) stop("type must be 'histogram' or 'density'")

  # set title
  .title <- switch(
    .model$estimand,
    ate = "Posterior of Average Treatment Effect",
    att = "Posterior of Average Treatment Effect of the Treated",
    atc = "Posterior of Average Treatment Effect of the Control"
  )

  # calculate stats
  pate <- bartCause::extract(.model, 'sate')
  pate <- as.data.frame(pate)
  ub <- quantile(pate$pate, .9)
  lb <- quantile(pate$pate, .1)
  ub.95 <- quantile(pate$pate, .975)
  lb.95 <- quantile(pate$pate, .025)
  dd <- density(pate$pate)
  dd <- with(dd, data.frame(x, y))

  # build base plot
  p <- ggplot(pate, aes(pate)) +
    scale_linetype_manual(values = c(2, 3)) +
    theme(legend.title = element_blank()) +
    labs(title = .title,
         x = toupper(.model$estimand))

  # histogram
  if (type == 'histogram'){
    p <- p +
      geom_histogram(fill = 'grey60') +
      labs(y = 'Frequency')

    # add credible intervals
    if (isTRUE(ci_95)) p <- p + geom_segment(x = lb, xend = ub, y = 0, yend = 0, size = 3, color = 'grey10')
    if (isTRUE(ci_80)) p <- p + geom_segment(x = lb.95, xend = ub.95, y = 0, yend = 0, size = 1.5, color = 'grey25')
  }

  # density
  if (type == 'density'){
    p <- p +
      geom_density() +
      labs(y = 'Density')

    # add credible intervals
    if (isTRUE(ci_95)){
      p <- p +
        geom_ribbon(data = subset(dd, x > lb.95 & x < ub.95),
                    aes(x = x, y = y, ymax = y),
                    ymin = 0, fill = "grey40", colour = NA, alpha = 0.8)
    }
    if (isTRUE(ci_80)){
      p <- p +
        geom_ribbon(data = subset(dd, x > lb & x < ub),
                    aes(x = x, y = y, ymax = y),
                    ymin = 0, fill = "grey30", colour = NA, alpha = 0.8)
    }
  }

  # add reference lines
  if (isTRUE(.mean)) p <- p + geom_vline(data = pate, aes(xintercept = mean(pate), linetype = 'mean'))
  if (isTRUE(.median)) p <- p + geom_vline(data = pate, aes(xintercept = median(pate), linetype = 'median'))
  if (!is.null(reference)) p <- p + geom_vline(xintercept = reference)

  return(p)
}
