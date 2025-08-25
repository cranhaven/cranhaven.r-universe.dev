#' @export
#' 
#' @rdname ppc
#' 
df_ppc <- function(fit, ...){
  UseMethod("df_ppc")
}

#' PPC data.frame
#' 
#' @rdname ppc
#' 
#' @param fit An object returned by fitTK
#' @param \dots additional arguments
#' 
#' @return A data frame with median and 95\% credible interval
#' 
#' @export
#' 
df_ppc.fitTK <- function(fit, ...){
  df <- .df_for_plot(fit)
  df$col_range <- ifelse(df$qinf95 > df$observation | df$qsup95 < df$observation, "out", "in")
  return(df)
}

.percentage_ppc <- function(df_ppc){
  sum(df_ppc$col_range == "in") / nrow(df_ppc) * 100
}


#' Posterior predictive check
#'
#' This is the generic \code{ppc} S3 method for plots of the predicted
#' values along with 95\% credible intervals
#' versus the observed values for \code{fitTK} objects.
#'
#' The black points show the observed number of survivors (pooled
#' replicates, on \eqn{X}-axis) against the corresponding predicted
#' number (\eqn{Y}-axis). Predictions come along with 95\% prediction
#' intervals, which are depicted in green when they contain the
#' observed value and in red otherwise. Samples with equal observed
#' value are shifted on the \eqn{X}-axis. For that reason, the
#' bisecting line (y = x), is represented by steps when observed
#' values are low. That way we ensure green intervals do intersect the
#' bisecting line.
#' 
#' @rdname ppc
#' 
#' @export
#' 
ppc <- function(fit, ...){
  UseMethod("ppc")
}


#' PPC plot
#' 
#' @rdname ppc
#' 
#' @param fit And object returned by fitTK
#' @param \dots Additional arguments
#' 
#' @return a plot of class \code{ggplot}
#' 
#' @export
#' 
ppc.fitTK <- function(fit, ...){

  df <- df_ppc(fit)
  
  percent_in <- round(.percentage_ppc(df), digits = 2)
  
  plt <- ggplot(data = df) + 
    theme_classic() +
    theme(legend.position="none") +
    labs(x = "Observation", y = "Prediction",
         subtitle = paste("PPC=", percent_in, "%")) +
    scale_colour_manual(values = c("green", "red")) +
    geom_abline(slope = 1) +
    geom_linerange(
      aes_string(x = 'observation',
          ymin = 'qinf95',
          ymax = 'qsup95',
          group = 'replicate',
          color = 'col_range')#,
      # position = position_dodge(width=0.5)
      ) +
    geom_point(
      aes_string(x = 'observation',
          y = 'q50',
          group = 'replicate')#,
      # position = position_dodge(width=0.5)
      ) + 
    facet_wrap(~variable, scales = "free")

  return(plt)
}
