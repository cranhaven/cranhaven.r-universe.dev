#' Plots the posterior density for FERGM model terms.
#'
#' This function allows the users to examine the posterior density of FERGM model terms.
#' @param fergm.fit A model object returned by the \code{fergm} function.  Must be specified.
#' @param custom_var_names A vector of custom variable names used in presentation that match the order of the \code{form} object passed to \code{fergm}.  If not provided, defaults to names inherited by \code{fergm.fit}.
#' @return This prints a list of posterior density plots produced using ggplot2.
#' @references Box-Steffensmeier, Janet M., Dino P. Christenson, and Jason W. Morgan. 2018. ``Modeling Unobserved Heterogeneity in Social Networks with the Frailty Exponential Random Graph Model." \emph{Political Analysis}. (26)1:3-19.
#' @references Stan Development Team (2016). RStan: the R interface to Stan. R package version 2.14.1. \url{http://mc-stan.org/}.
#' @keywords FERGM interpret summary
#' @examples
#' # load example data
#' data("ergm.fit")
#' data("fergm.fit")
#' data("mesa")
#'
#' # rstan functions
#' # Histogram of the posterior
#' rstan::stan_hist(fergm.fit$stan.fit, par = "beta")
#' # Density of the posteriors
#' rstan::stan_dens(fergm.fit$stan.fit, par = "beta")
#'
#' # We have a cleaner function to look at the posterior densities
#' densities <- coef_posterior_density(fergm.fit = fergm.fit,
#' custom_var_names = c("Edges", "Sex Homophily", "Grade Homophily", "Race Homophily",
#' "GWESP", "Alternating K-Stars"))
#' densities[[1]]
#' densities[[2]]
#' @export

coef_posterior_density <- function(fergm.fit = NULL, custom_var_names = NULL){
  its <- rstan::extract(fergm.fit$stan.fit)$beta

  if(is.null(custom_var_names)){
    custom_var_names <- colnames(fergm.fit$stan.dta$x)
  }

  plot_list <- list()

  for(i in 1:ncol(its))
    local({
      i <- i
    ts <- its[,i]
    plot_df <- data.frame(values = ts)
    plot_df <- reshape2::melt(plot_df)
    var <- custom_var_names[i]

    pl <- ggplot2::ggplot(data = plot_df, aes(x = plot_df$value)) +
      geom_density(alpha = 0.5, fill = "firebrick4", color = "firebrick4") +
      xlab("Effect Value") +
      ylab("Density") +
      ggtitle(var) +
      #cale_fill_manual(values=c("firebrick4", "dodgerblue4"),
                        #name="Model",
                       # breaks=c("pct_correct_ergm", "pct_correct_fergm"),
                        #labels=c("ERGM", "FERGM")) +
     # scale_color_manual(values=c("firebrick4", "dodgerblue4"),
                        # name="Model",
                        # breaks=c("pct_correct_ergm", "pct_correct_fergm"),
                        # labels=c("ERGM", "FERGM")) +
    #  geom_vline(xintercept=0) +
      theme_bw() +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))
    plot_list[[i]] <<- pl
  })
  return(plot_list)
}
