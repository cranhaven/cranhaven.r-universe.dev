#' Bayesian Assurance Computation in the Beta-Binomial Setting
#'
#' Returns the Bayesian assurance corresponding to a hypothesis test for
#' difference in two independent proportions.
#' @importFrom ggplot2 ggplot aes geom_hline geom_vline aes theme element_text 
#' @importFrom ggplot2 xlab ylab ggtitle scale_x_continuous scale_y_continuous
#' @importFrom rlang .data
#' @importFrom stats rbinom rbeta qnorm
#' @importFrom pbapply pbsapply
#' @param n1 sample size of first group
#' @param n2 sample size of second group
#' @param p1 proportion of successes in first group. Takes on a NULL (default) 
#' assignment if unknown.
#' @param p2 proportion of successes in second group. Takes on a NULL 
#' (default) assignment if unknown.
#' @param alpha_1,beta_1 shape parameters for the distribution of `p1` 
#' if `p1` is unknown: \eqn{p1 ~ Beta(\alpha_1, \beta_1)}
#' @param alpha_2,beta_2 shape parameters for the distribution of p2 if 
#' p2 is unknown: \eqn{p2 ~ Beta(\alpha_2, \beta_2)}
#' @param sig_level significance level
#' @param alt a character string specifying the alternative hypothesis, 
#' must select one of following choices: `"two.sided"` (default), 
#' `"greater"` or `"less"`.
#' @param mc_iter number of MC samples evaluated under the analysis objective
#' @returns approximate Bayesian assurance of independent two-sample proportion 
#' test
#' @examples
#'
#' #########################################################
#' # alpha1 = 0.5, beta1 = 0.5, alpha2 = 0.5, beta2 = 0.5 ##
#' #########################################################
#' n <- seq(200, 1000, 10)
#' assur_vals <- bayesassurance::bayes_sim_betabin(n1 = n, n2 = n, 
#' p1 = 0.25, p2 = 0.2, alpha_1 = 0.5, beta_1 = 0.5, alpha_2 = 0.5, 
#' beta_2 = 0.5, sig_level = 0.05, alt = "greater", mc_iter = 1000)
#'
#' assur_vals$assurance_table
#' assur_vals$assurance_plot
#' 
#' @export
#'

bayes_sim_betabin <- function(n1, n2, p1, p2, alpha_1, alpha_2, beta_1, beta_2, 
                              sig_level, alt, mc_iter){
    
  is.scalar <- function(x) is.atomic(x) && length(x) == 1L
  
  if(dim(as.matrix(n1))[1] != dim(as.matrix(n2))[1]){
    stop("n1 and n2 must be of equal length.")
  }
  
  if(is.scalar(p1) == FALSE | is.scalar(p2) == FALSE){
    stop("p1 and p2 must be scalar values.")
  }
  
  if(is.scalar(alpha_1) == FALSE | is.scalar(alpha_2) == FALSE | 
     is.scalar(beta_1) == FALSE | 
     is.scalar(beta_2) == FALSE){
    stop("Shape parameters must be scalar values.")
  }
  
  if(sig_level < 0 | sig_level > 1){
    stop("Not a valid significance level, must be between 0 and 1.")
  }
  
  if((alt %in% c("greater", "less", "two.sided")) == FALSE | is.null(alt)){
    stop("Please specify one of the three options for alternative test case: 
         greater, less, two.sided.")
  }
  
  count <- 0

    # set.seed(1)
    if(is.null(p1) == TRUE & is.null(p2) == TRUE){
      p1 <- stats::rbeta(n=1, alpha_1, beta_1)
      p2 <- stats::rbeta(n=1, alpha_2, beta_2)
    }else if(is.null(p1) == TRUE & is.null(p2) == FALSE){
      p1 <- stats::rbeta(n=1, alpha_1, beta_1)
    }else if(is.null(p1) == FALSE & is.null(p2) == TRUE){
      p2 <- stats::rbeta(n=1, alpha_2, beta_2)
    }

    MC_samp <- function(n1 = n1, n2 = n2){
      for(i in 1:mc_iter){

        # x1 and x2 are observed frequency values, simulate by drawing 
        # from the binomial distribution
        x1 <- stats::rbinom(n=1, size=n1, prob=p1)
        x2 <- stats::rbinom(n=1, size=n2, prob=p2)

        # estimating difference of proportions: p = p1 - p2
        p_post_mean <- ((alpha_1 + x1) / (alpha_1 + beta_1 + n1)) - 
          ((alpha_2 + x2) / (alpha_2 + beta_2 + n2))
        p_post_var <- (((alpha_1 + x1) * (beta_1 + n1 - x1)) / 
          ((alpha_1 + beta_1 + n1)^2 * (alpha_1 + beta_1 + n1 + 1))) +
          (((alpha_2 + x2) * (beta_2 + n2 - x2)) / ((alpha_2 + beta_2 + n2)^2 * 
          (alpha_2 + beta_2 + n2 + 1)))

        # critical z-value of normal distribution
        if(alt == "two.sided"){
          z <- stats::qnorm(1 - sig_level / 2)
        }else if(alt == "greater" | alt == "less"){
          z <- stats::qnorm(1 - sig_level)
        }

        # Condition to check if 0 falls in 100(1-sig_level)% Credible Interval
        if(alt == "two.sided"){
          # lower two-sided confidence bound
          lb <- p_post_mean - z * (sqrt(p_post_var))
          # upper two-sided confidence bound
          ub <- p_post_mean + z * (sqrt(p_post_var))
          if(0 < lb | 0 > ub){
            count <- count + 1
          }else{
            count <- count
          }
        }else if(alt == "less"){ 
          # Condition to check if 0 falls above UB of 
          # 100(1-sig_level)% Credible Interval
          # lower two-sided confidence bound
          lb <- -Inf
          # upper two-sided confidence bound
          ub <- p_post_mean + z * (sqrt(p_post_var))
          if(0 > ub){
            count <- count + 1
          }else{
            count <- count
          }
        }else if(alt == "greater"){
          # lower two-sided confidence bound
          lb <- p_post_mean - z * (sqrt(p_post_var))
          # upper two-sided confidence bound
          ub <- Inf
          if(0 < lb){
            count <- count + 1
          }else{
            count <- count
          }
        }
      }
      assurance <- count / mc_iter
      return(assurance)
    }

    # objects returned if n1 and n2 are vectors
    if(length(n1) > 1 & length(n2) > 1){
      assurance <- pbapply::pbmapply(MC_samp, n1 = n1, n2 = n2)
      assur_tab <- as.data.frame(cbind(n1, n2, assurance))
      colnames(assur_tab) <- c("n1", "n2", "Assurance")

      # returns an assurance plot only if vectors n1 and n2 are identical, 
      # enabling the plot to focus on a single n on the x-axis
      if(identical(n1, n2)){
        assur_plot <- ggplot2::ggplot(assur_tab, alpha = 0.5, 
          aes(x = .data$`n1`, y = .data$Assurance)) +
          ggplot2::geom_point(aes(x = .data$`n1`, 
                                  y = .data$Assurance), lwd = 1.2) +
          ggplot2::ggtitle("Assurance Values") +
          ggplot2::xlab("Sample Size n = n1 = n2") + ggplot2::ylab("Assurance")
        assur_plot <- structure(assur_plot, class = "ggplot")
        return(list(assurance_table = assur_tab, assurance_plot = assur_plot, 
                    mc_samples = mc_iter))
      }else{
        return(list(assurance_table = assur_tab, mc_samples = mc_iter))
      }
    }else{
      # objects returned when a scalar is passed in for n1 and n2
      assurance <- MC_samp(n1 = n1, n2 = n2)
      return(list(assur_val = paste0("Assurance: ", round(assurance, 3)), 
                  mc_samples = mc_iter))
    }
}





