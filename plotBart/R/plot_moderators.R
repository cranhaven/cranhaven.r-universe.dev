#' @title Partial dependency plot of a continuous moderating variable
#' @description Plot a partial dependency plot with a continuous covariate from a 'bartCause' model. Identify treatment effect variation predicted across levels of a continuous variable.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param moderator the moderator as a vector
#' @param n_bins number of bins to cut the moderator with. Defaults to the lesser of 15 and number of distinct levels of the moderator
#' @details Partial dependency plots are one way to evaluate heterogeneous treatment effects that vary by values of a continuous covariate. For more information on partial dependency plots from BART causal inference models see Green and Kern 2012.
#' @author George Perrett, Joseph Marlo
#'
#' @references
#' Green, D. P., & Kern, H. L. (2012).
#' Modeling heterogeneous treatment effects in survey experiments with Bayesian additive regression trees.
#' Public opinion quarterly, 76(3), 491-511.
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#' @importFrom stats density median na.omit predict quantile sd
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
#'  commonSuprule = 'none',
#'  keepTrees = TRUE
#' )
#' plot_moderator_c_pd(model_results, lalonde$age)
#' }
plot_moderator_c_pd <- function(.model, moderator, n_bins = NULL){

  validate_model_(.model)

  # validate n_bins argument
  n_mod_levels <- n_distinct(moderator)
  if (n_mod_levels <= 1) stop('dplyr::n_distinct(moderator) must be at least 2')
  if (is.null(n_bins)) n_bins <- pclamp_(15, 2, n_mod_levels)
  if (!(n_bins > 1 & n_bins <= n_mod_levels)) stop("n_bins must be greater than 1 and less than or equal to dplyr::n_distinct(moderator)")

  # extract data from model
  new_data <- as_tibble(.model$data.rsp@x)

  # create dataframe where all observations are treated
  new_data_z1 <- new_data
  name_trt <- .model$name.trt
  new_data_z1[, name_trt] <- 1
  new_data_z1 <- cbind.data.frame(y_response = .model$fit.rsp$y, new_data_z1)

  # create dataframe where all observations are control
  new_data_z0 <- new_data
  new_data_z0[, name_trt] <- 0
  new_data_z0 <- cbind.data.frame(y_response = .model$fit.rsp$y, new_data_z0)

  # locate the moderator in bartc data
  search_moderator <- function(x) sum(moderator - x)
  index <- which(lapply(new_data_z0, search_moderator) == 0)
  if (!isTRUE(index > 0)) stop('Cannot find moderator in original data. Is moderator within the original dataframe used to fit the .model?')

  # get range for predictions
  cut <- n_bins-1
  p <- seq(min(moderator), max(moderator), (max(moderator) - min(moderator))/cut)
  if (length(p) < n_distinct(moderator)) {
    .range <- p
  } else{
    .range <- unique(moderator)[order(unique(moderator))]
  }

  # predict new data with overridden treatment columns
  cates <- lapply(.range, fit_pd_, z1 = new_data_z1, z0 = new_data_z0, index = index, .model = .model)
  names(cates) <- seq_along(cates)
  cates <- bind_cols(cates)
  cates.m <- apply(cates, MARGIN = 2, FUN = mean)
  cates.m <- bind_cols(cates.m = cates.m, .range = .range)

  # get credible intervals
  ci_range <- c(0.025, 0.1, 0.9, 0.975)
  cates.ci <- as_tibble(t(apply(cates, MARGIN = 2, FUN = quantile, probs = ci_range)))
  cates_plot <- bind_cols(cates.m, cates.ci)
  indices <- 2 + seq_along(ci_range)
  colnames(cates_plot)[indices] <- paste0('ci_', as.character(ci_range * 100))

  # plot it
  p <- ggplot(cates_plot) +
    geom_ribbon(aes(x = .range, y = cates.m, ymin = ci_2.5, ymax = ci_97.5, fill = '95% ci')) +
    geom_ribbon(aes(x = .range, y = cates.m, ymin = ci_10, ymax = ci_90, fill = '80% ci')) +
    scale_fill_manual(values = c('grey40', 'grey60')) +
    geom_point(aes(x = .range, y = cates.m), size = 2) +
    geom_line(aes(x = .range, y = cates.m)) +
    labs(title = NULL,
         x = NULL,
         y = 'CATE') +
    theme(legend.position = "bottom")

  return(p)
}


#' @title LOESS plot of a continuous moderating variable
#' @description Plot the LOESS prediction of ICATEs by a continuous covariate. This is an alternative to partial dependency plots to assess treatment effect heterogeneity by a continuous covariate. See Carnegie, Dorie and Hill 2019.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param moderator the moderator as a vector
#' @param line_color the color of the loess line
#'
#' @author George Perrett, Joseph Marlo
#'
#' @references
#' Carnegie, N., Dorie, V., & Hill, J. L. (2019).
#' Examining treatment effect heterogeneity using BART.
#' Observational Studies, 5(2), 52-70.
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#' @importFrom bartCause extract
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
#'  commonSuprule = 'none'
#' )
#' plot_moderator_c_loess(model_results, lalonde$age)
#' }
plot_moderator_c_loess <- function(.model, moderator, line_color = 'blue'){

  validate_model_(.model)
  is_numeric_vector_(moderator)

  # adjust moderator to match estimand
  moderator <- adjust_for_estimand_(.model, moderator)

  # extract and rotate posterior
  posterior <- bartCause::extract(.model, 'icate')
  posterior <- posterior %>%
    t() %>%
    as.data.frame() %>%
    as_tibble()

  # split posterior into list of dfs by each level of moderator
  split_posterior <- split(posterior, moderator)
  posterior_means <- lapply(split_posterior, rowMeans)

  # unlist into a data.frame for plotting
  dat <- data.frame(value = unlist(posterior_means))
  dat$moderator <- moderator[order(moderator)]
  rownames(dat) <- seq_len(nrow(dat))

  # plot it
  p <- ggplot(dat, aes(moderator, value)) +
    geom_point() +
    geom_smooth(method = 'loess',
                formula = y ~ x,
                se = TRUE,
                size = 1.5,
                color = line_color) +
    labs(title = NULL,
         x = NULL,
         y = 'icate')

  return(p)
}

#' @title Plot the Conditional Average Treatment Effect conditional on a discrete moderator
#' @description Plot the Conditional Average Treatment Effect split by a discrete moderating variable. This plot will provide a visual test of moderation by discrete variables.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param moderator the moderator as a vector
#' @param .alpha transparency value [0, 1]
#' @param facet TRUE/FALSE. Create panel plots of each moderator level?
#' @param .ncol number of columns to use when faceting
#'
#' @author George Perrett
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#' @importFrom bartCause extract
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
#'  commonSuprule = 'none'
#' )
#' plot_moderator_d_density(model_results, lalonde$educ)
#' }
plot_moderator_d_density <- function(.model, moderator, .alpha = 0.7, facet = FALSE, .ncol = 1){

  validate_model_(.model)
  is_discrete_(moderator)

  # adjust moderator to match estimand
  moderator <- adjust_for_estimand_(.model, moderator)

  # extract and rotate posterior
  posterior <- bartCause::extract(.model, 'icate')
  posterior <- posterior %>%
    t() %>%
    as.data.frame() %>%
    as_tibble()

  # split posterior into list of dfs by each level of moderator
  split_posterior <- split(posterior, moderator)
  posterior_means <- lapply(split_posterior, colMeans)

  # unlist into a data.frame for plotting
  dat <- data.frame(value = unlist(posterior_means))
  dat$moderator <- sub("\\..*", '', rownames(dat))
  rownames(dat) <- seq_len(nrow(dat))

  # plot it
  p <- ggplot(dat, aes(value, fill = moderator)) +
    geom_density(alpha = .alpha) +
    labs(title = NULL,
         x = 'CATE',
         y = NULL) +
    theme(legend.position = 'bottom')

  # add faceting
  if(isTRUE(facet)){
    p <- p + facet_wrap(~moderator, ncol = .ncol)
  }

  return(p)
}



#' @title Plot the posterior interval of the Conditional Average Treatment Effect grouped by a discrete variable
#' @description Plots the range of the Conditional Average Treatment Effect grouped by a discrete variable. This is analogous to plot_moderator_d_density but is preferable for moderators with many categories. Rather than plotting the full density, the posterior range is shown.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param moderator the moderator as a vector
#' @param .alpha transparency value [0, 1]
#' @param horizontal flip the plot horizontal?
#'
#' @author George Perrett, Joseph Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#' @importFrom bartCause extract
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
#'  commonSuprule = 'none'
#' )
#' plot_moderator_d_linerange(model_results, lalonde$educ)
#' }
plot_moderator_d_linerange <- function(.model, moderator, .alpha = 0.7, horizontal = FALSE){

  validate_model_(.model)
  is_discrete_(moderator)

  # adjust moderator to match estimand
  moderator <- adjust_for_estimand_(.model, moderator)

  # extract and rotate posterior
  posterior <- bartCause::extract(.model, 'icate')
  posterior <- posterior  %>%
    t() %>%
    as.data.frame() %>%
    as_tibble()

  # split posterior into list of dfs by each level of moderator
  split_posterior <- split(posterior, moderator)
  posterior_means <- lapply(split_posterior, colMeans)

  # unlist into a data.frame for plotting
  dat <- data.frame(value = unlist(posterior_means))
  dat$moderator <- sub("\\..*", '', rownames(dat))
  rownames(dat) <- seq_len(nrow(dat))

  # tidy up the data
  dat <- dat %>%
    group_by(moderator) %>%
    mutate(.min = quantile(value, .025),
           .max = quantile(value, .975),
           point = mean(value)) %>%
    dplyr::select(-value) %>%
    arrange(desc(point)) %>%
    ungroup() %>%
    distinct()

  # plot it
  p <- ggplot(dat, aes(x = moderator, y = point, color = moderator)) +
    geom_point(size = 2) +
    geom_linerange(aes(ymin = .min, ymax = .max), alpha = .alpha) +
    labs(title = NULL,
         x = element_blank(),
         y = 'CATE') +
    theme(legend.position = 'bottom')

  if (horizontal) p <- p + coord_flip()

  return(p)
}

#' @title Plot a single regression tree of covariates on ICATEs
#' @description Plot a single regression tree for exploratory heterogeneous effects. Fit single regression tree on bartc() ICATEs to produce variable importance plot. This plot is useful for identifying potential moderating variables.
#' Tree depth may be set to depths 1, 2 or 3. Terminal nodes signal the Conditional Average Treatment effect within levels of moderation variables. Trees with different values across terminal nodes suggest strong treatment effect moderation.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param max_depth one of c(1, 2, 3). Maximum number of node levels within the tree. 2 is recommended
#'
#' @author George Perrett, Joseph Marlo
#'
#' @import ggplot2 dplyr
#' @importFrom ggdendro dendro_data
#' @importFrom rpart rpart
#' @importFrom bartCause extract
#'
#' @return ggplot object
#' @export
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
#'  commonSuprule = 'none'
#' )
#' plot_moderator_search(model_results)
#' }
plot_moderator_search <- function(.model, max_depth = c(2, 1, 3)){

  validate_model_(.model)

  max_depth <- max_depth[[1]]
  if (max_depth %notin% c(2, 1, 3)) stop('max_depth must be one of c(1, 2, 3)')

  icate <- bartCause::extract(.model , 'icate')
  icate.m <- apply(icate, 2, mean)

  # pull data from model and create a matrix of confounders
  .data <- as.data.frame(.model$data.rsp@x)

  # adjust data for estimand
  if (.model$estimand == 'ate') {
    confounders <- as.matrix(.data[, c(-1,-(length(.data)))])
  } else if (.model$estimand == 'att') {
    .data <- .data[.model$trt == 1,]
    confounders <- as.matrix(.data[, c(-1,-(length(.data)))])
  } else{
    .data <- .data[.model$trt == 0,]
    confounders <- as.matrix(.data[, c(-1,-(length(.data)))])
  }

  # fit regression tree
  cart <- rpart::rpart(icate.m ~ ., data = as.data.frame(confounders), maxdepth = max_depth)
  # p <- rpart.plot::rpart.plot(cart, type = 2, branch = 1, box.palette = 0)

  # create dendrogram
  p_gg <- rpart_ggplot_(cart)

  return(p_gg)
}


rpart_ggplot_ <- function(.model){

  # remove depth information from model so resulting plot is easy to read
  .model$frame$dev <- 1

  # extract data to construct dendrogram
  fitr <- ggdendro::dendro_data(.model)
  n_leaf <- .model$frame$n[.model$frame$var == '<leaf>']
  n_split <- .model$frame$n[.model$frame$var != '<leaf>']
  pred_split <- round(.model$frame$yval[.model$frame$var != '<leaf>'], 1)
  terminal_leaf_y <- 0.1
  leaf_labels <- tibble(
    x = fitr$leaf_labels$x,
    y = terminal_leaf_y,
    label = paste0(
      'y = ', fitr$leaf_labels$label,
      '\nn = ', n_leaf)
  )
  yes_no_offset <- c(0.75, 1.25)
  yes_no <- tibble(
    x = c(fitr$labels$x[[1]] * yes_no_offset[1],
          fitr$labels$x[[1]] * yes_no_offset[2]),
    y = rep(fitr$labels$y[[1]], 2),
    label = c("yes", "no")
  )
  split_labels <- tibble(
    x = fitr$labels$x,
    y = fitr$labels$y + 0.07,
    label = paste0(
      'y = ', pred_split,
      '\nn = ', n_split
    )
  )

  # set terminal segments to y = terminal_leaf_y
  initial_node_y <- fitr$labels$y[[1]]
  fitr$segments <- fitr$segments %>%
    mutate(y_new = ifelse(y > yend, y, yend),
           yend_new = ifelse(yend < y, yend, y)) %>%
    select(n, x, y = y_new, xend, yend = yend_new) %>%
    mutate(y = ifelse(y > initial_node_y, terminal_leaf_y, y),
           yend = ifelse(x == xend & x == round(x) & y > yend, terminal_leaf_y, yend))

  # set plot constants
  label_text_size <- 3
  x_limits <- c(0.5, nrow(fitr$leaf_labels) + 0.5)
  y_limits <- c(min(fitr$segments$y) - 0.05,
                max(fitr$segments$y) + 0.15)

  # plot it
  p <- ggplot() +
    geom_segment(data = fitr$segments,
                 aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_label(data = yes_no,
               aes(x = x, y = y, label = label),
               size = label_text_size) +
    geom_label(data = leaf_labels,
               aes(x = x, y = y, label = label),
               size = label_text_size) +
    geom_label(data = split_labels,
               aes(x = x, y = y, label = label),
               size = label_text_size) +
    geom_label(data = fitr$labels,
               aes(x = x, y = y, label = label),
               label.size = NA, fontface = 'bold') +
    expand_limits(x = x_limits,
                  y = y_limits) +
    scale_x_continuous(labels = NULL, breaks = NULL) +
    scale_y_continuous(labels = NULL, breaks = NULL) +
    labs(title = 'Exploratory heterogeneous effects',
         x = NULL,
         y = NULL) +
    theme(panel.background = element_blank())

  return(p)
}
