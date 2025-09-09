#' Plots the coefficients of FERGM and ERGM objects.
#'
#' This function allows the users to visualize FERGM estimates or to compare the coefficients of ERGMs and FERGMs.
#' @param fergm.fit A model object returned by the \code{fergm} function.  Must be specified.
#' @param ergm.fit A model object returned by the \code{ergm} function.  May be specified when comparing ERGM and FERGM coefficients.
#' @param custom_var_names A vector of custom variable names used in presentation that match the order of the \code{form} object passed to \code{fergm}.  If not provided, defaults to names inherited by \code{fergm.fit}.
#' @return This function produces a coefficient rope-ladder plot containing 95\% confidence intervals using ggplot2.  The function either takes \code{fergm} model output or \code{fergm} and \code{ergm} model output.  The former is effective in summarizing \code{fergm} model output while the latter is effective in comparing FERGM and ERGM estimates.
#' @keywords FERGM interpret summary
#' @references Box-Steffensmeier, Janet M., Dino P. Christenson, and Jason W. Morgan. 2018. ``Modeling Unobserved Heterogeneity in Social Networks with the Frailty Exponential Random Graph Model." \emph{Political Analysis}. (26)1:3-19.references Box-Steffensmeier, Janet M., Dino P. Christenson, and Jason W. Morgan. 2017. ``Modeling Unobserved Heterogeneity in Social Networks with the Frailty Exponential Random Graph Model." \emph{Political Analysis}.
#' @references Stan Development Team (2016). RStan: the R interface to Stan. R package version 2.14.1. \url{http://mc-stan.org/}.
#' @examples
#' # load example data
#' data("ergm.fit")
#' data("fergm.fit")
#' data("mesa")
#' # Compare substantive implications via coef plot, these are with 95% credible intervals
#' coef_plot(fergm.fit = fergm.fit, ergm.fit = ergm.fit,
#' custom_var_names =  c("Edges", "Sex Homophily", "Grade Homophily",
#' "Race Homophily", "GWESP", "Alternating K-Stars"))
#' coef_plot(fergm.fit = fergm.fit,
#' custom_var_names =  c("Edges", "Sex Homophily", "Grade Homophily",
#' "Race Homophily", "GWESP", "Alternating K-Stars"))
#' @export

coef_plot <- function(fergm.fit = NULL, ergm.fit = NULL, custom_var_names = NULL){
  its <- rstan::extract(fergm.fit$stan.fit)$beta

  if(is.null(custom_var_names)){
    custom_var_names <- colnames(fergm.fit$stan.dta$x)
  }

  fergm_df <- cbind(as.data.frame(colMeans(its)), as.data.frame(matrixStats::colQuantiles(its, probs = c(0.025, 0.975))))
  colnames(fergm_df)[1] <- "mean"

  if(!is.null(ergm.fit)){
    est <- cbind(round(cbind(stats::coef(ergm.fit),
                             stats::confint(ergm.fit)), 3),
                 round(fergm_df, 3))
    colnames(est) <- c("ergm", "ergm.low", "ergm.high", "fergm", "fergm.low",
                       "fergm.high")
    est$var <- custom_var_names

    coef_df <- data.frame()
    for(i in 1:length(custom_var_names)){
      temp <- data.frame(
        var = rbind(paste0("ERGM: ", custom_var_names[i]), paste0("FERGM: ", custom_var_names[i])),
        coef = rbind(est$ergm[i], est$fergm[i]),
        low = rbind(est$ergm.low[i], est$fergm.low[i]),
        high = rbind(est$ergm.high[i], est$fergm.high[i]))
      coef_df <- rbind(coef_df, temp)
    }
    coef_df$var <- factor(coef_df$var, levels = unique(coef_df$var))

    p = ggplot2::ggplot(coef_df, aes(x=coef_df$var)) +
      geom_pointrange(aes(y = coef_df$coef, ymin = coef_df$low, ymax = coef_df$high), colour=ifelse(coef_df$low < 0 & coef_df$high > 0, "firebrick4", "dodgerblue4")) +
      theme_bw()  +
      coord_flip() +
      geom_hline(yintercept = 0, lty=2) +
      xlab('Variable') +
      ylab('Coefficient') +
      theme_bw() +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))
  } else {
    est <- round(fergm_df, 3)
    colnames(est) <- c("fergm", "fergm.low",
                       "fergm.high")
    est$var <- custom_var_names

    coef_df <- data.frame()
    for(i in 1:length(custom_var_names)){
      temp <- data.frame(
        var = rbind(paste0("FERGM: ", custom_var_names[i])),
        coef = rbind(est$fergm[i]),
        low = rbind(est$fergm.low[i]),
        high = rbind(est$fergm.high[i]))
      coef_df <- rbind(coef_df, temp)
    }
    coef_df$var <- factor(coef_df$var, levels = unique(coef_df$var))

    p = ggplot2::ggplot(coef_df, aes(x=coef_df$var)) +
      geom_pointrange(aes(y = coef_df$coef, ymin = coef_df$low, ymax = coef_df$high), colour=ifelse(coef_df$low < 0 & coef_df$high > 0, "firebrick4", "dodgerblue4")) +
      theme_bw()  +
      coord_flip() +
      geom_hline(yintercept = 0, lty=2) +
      xlab('Variable') +
      ylab('Coefficient') +
      theme_bw() +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))
  }

  return(p)
}
