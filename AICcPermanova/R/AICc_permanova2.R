#' Calculate AICc for a permutational multivariate analysis of variance (PERMANOVA)
#'
#' @description #' This function calculates the Akaike's Information Criterion (AICc) for a permutational multivariate analysis of variance (PERMANOVA) model. The AICc is a modified version of the Akaike Information Criterion (AIC) that is more appropriate for small sample sizes and high-dimensional models.
#'
#' @param adonis2_model An object of class adonis2 from the vegan package
#'
#' @return A data frame with the AICc, the number of parameters (k) and the number of observations (N).
#'
#' @examples
#'
#' library(vegan)
#' data(dune)
#' data(dune.env)
#'
#' # Run PERMANOVA using adonis2
#'
#' Model <- adonis2(dune ~ Management*A1, data = dune.env)
#'
#' # Calculate AICc
#' AICc_permanova2(Model)
#'
#' @details
#' The AICc calculation for a PERMANOVA model is:
#'
#' \deqn{AICc = AIC + \frac{2k(k+1)}{n-k-1}}{AICc = AIC + (2k(k+1))/(n-k-1)}
#'
#' where AIC is the Akaike Information Criterion, k is the number of parameters in the model (excluding the intercept), and n is the number of observations.
#'
#' @export
#'
#' @import vegan
#'
#' @references
#' Zuur, A. F., Ieno, E. N., Walker, N. J., Saveliev, A. A., & Smith, G. M. (2009). Mixed effects models and extensions in ecology with R. Springer Science & Business Media.
#'
#' @seealso \code{\link{adonis2}}
#'
#' @keywords models


AICc_permanova2 <- function(adonis2_model) {

    # Ok, now extract appropriate terms from the adonis model Calculating AICc
    # using residual sum of squares (RSS or SSE) since I don't think that adonis
    # returns something I can use as a likelihood function... maximum likelihood
    # and MSE estimates are the same when distribution is gaussian See e.g.
    # https://www.jessicayung.com/mse-as-maximum-likelihood/;
    # https://towardsdatascience.com/probability-concepts-explained-maximum-likelihood-estimation-c7b4342fdbb1
    # So using RSS or MSE estimates is fine as long as the residuals are
    # Gaussian https://robjhyndman.com/hyndsight/aic/ If models have different
    # conditional likelihoods then AIC is not valid. However, comparing models
    # with different error distributions is ok (above link).


    RSS <- adonis2_model$SumOfSqs[ length(adonis2_model$SumOfSqs) - 1 ]
    MSE <- RSS / adonis2_model$Df[ length(adonis2_model$Df) - 1 ]

    nn <- adonis2_model$Df[ length(adonis2_model$Df) ] + 1

    k <- nn - adonis2_model$Df[ length(adonis2_model$Df) - 1 ]


    # AIC : 2*k + n*ln(RSS/n)
    # AICc: AIC + [2k(k+1)]/(n-k-1)

    # based on https://en.wikipedia.org/wiki/Akaike_information_criterion;
    # https://www.statisticshowto.datasciencecentral.com/akaikes-information-criterion/ ;
    # https://www.researchgate.net/post/What_is_the_AIC_formula;
    # http://avesbiodiv.mncn.csic.es/estadistica/ejemploaic.pdf;
    # https://medium.com/better-programming/data-science-modeling-how-to-use-linear-regression-with-python-fdf6ca5481be


    AIC <- 2*k + nn*log(RSS/nn)
    AICc <- AIC + (2*k*(k + 1))/(nn - k - 1)

    output <- data.frame(AICc = AICc, k = k, N = nn)

    return(output)

  }
