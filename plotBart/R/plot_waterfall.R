#' @title Plot a waterfall of the ICATEs
#' @description Plots the point and posterior intervals of each individual's ICATE ordered by the ICATE or a continuous variable. Points can be colored by a discrete variable.
#' Waterfall plots are a useful visual diagnostic of possible treatment effect heterogeneity.
#' A flat line implies little treatment effect heterogeneity while a steeper curve implies that the treatment effect varies across individuals in the sample. Ordering points by a continuous variable or coloring points by a discrete variable can be helpful to identify potential moderators of the treatment effect.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @param descending order the ICATEs by value?
#' @param .order a vector representing a custom order
#' @param .color a vector representing colors
#' @param .alpha transparency value [0, 1]
#'
#' @author George Perrett
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
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
#' plot_waterfall(model_results)
#' }
plot_waterfall <- function(.model, descending = TRUE, .order = NULL, .color = NULL, .alpha = 0.5){

  validate_model_(.model)
  if(!is.null(.color)){
    if (!is.vector(.color)) stop(".color must be a vector")
    if (nrow(.model$data.rsp@x) != length(.color)) stop(paste(".color must be a vector of length", nrow(.model$data.rsp@x)))
  }

  # calculate stats
  posterior <- bartCause::extract(.model, 'icate')
  posterior <- posterior %>%
    t() %>%
    as.data.frame() %>%
    as_tibble()

  icate.m <- apply(posterior, 1, mean)
  icate.sd <- apply(posterior, 1, sd)
  icate.uci <- icate.m + icate.sd * 1.96
  icate.lci <- icate.m - icate.sd * 1.96

  dat <- tibble(icate.m, icate.lci, icate.uci)

  if(!is.null(.color)){
    .color <- adjust_for_estimand_(.model, .color)
    dat$.color <- .color
  }
  # specify order of icates on x axis
  if(isTRUE(descending)){
    dat <- dat %>% arrange(desc(icate.m))
  } else if(!is.null(.order)){
    if(isTRUE(descending)){
      dat <- arrange(dat, desc(.order))
    } else{
      dat <- arrange(dat, .order)
    }
  } else{
    dat <- arrange(dat, icate.m)
  }

  dat <- mutate(dat, icate.o = row_number())

  # create base plot
  p <- ggplot(dat, aes(x = icate.o, y = icate.m)) +
    geom_linerange(aes(ymin = icate.lci, ymax = icate.uci),
                   alpha = .alpha) +
    geom_point() +
    labs(title = NULL,
         x = 'Ordered icates',
         y = 'icate') +
    theme(legend.position = 'top')

  # add color
  if(!is.null(.color)){
    p <- p +
      aes(color = as.character(.color)) +
      labs(title = NULL,
           x = 'Ordered icates',
           y = 'icate')
  }

  # apply custom order
  if(!is.null(.order)){
    .order <- adjust_for_estimand_(.model, .order)
    p <- p +
      aes(x = .order, y = icate.m) +
      labs(title = NULL,
           x = 'ordered icates',
           y = 'icate')
  }

  return(p)
}
