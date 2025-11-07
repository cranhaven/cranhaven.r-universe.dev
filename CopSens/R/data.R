#' Dataset with Gaussian Treatments and Outcomes
#'
#' A dataset containing Gaussian treatments and outcomes of 10,000 observations.
#'
#' @format A data frame with eleven variables: one Gaussian outcome, \code{y}, and ten Gaussian
#' treatments, \code{t1}, \code{t2}, ..., \code{t10}.
#'
#' @source For data generating process, see \code{data-raw/Data_Generation.R}.
"GaussianT_GaussianY"



#' Dataset with Gaussian Treatments and Binary Outcomes
#'
#' A dataset containing Gaussian treatments and binary outcomes of 10,000 observations.
#'
#' @format A data frame with eleven variables: one binary outcome, \code{y}, and ten Gaussian
#' treatments, \code{t1}, \code{t2}, ..., \code{t10}.
#'
#' @source For data generating process, see \code{data-raw/Data_Generation.R}.
"GaussianT_BinaryY"



#' Body weight and gene expressions of 287 mice
#'
#' A dataset are collected from 287 mice, including the body weight, 37 gene expressions,
#' and 5 single nucleotide polymorphisms.
#'
#' @format A data frame with forty-three variables: the mice body weight, \code{y},
#' 5 single nucleotide polymorphisms, \code{rs3663003}, \code{rs4136518}, \code{rs3694833},
#' \code{rs4231406}, \code{rs3661189}, and the rest are thirty-seven genes.
#'
#' @source \url{https://arxiv.org/abs/2011.04504}
"micedata"


#' Estimates of genes' effects on mice body weight using null treatments approach from Miao et al. (2020)
#'
#' The dataset consists of estimates of treatment effects of 17 genes, which are likely to affect mouse weight,
#' by using the null treatments approach from Miao et al. (2020),
#' assuming that at least half of the confounded treatments have no causal effect on the outcome.
#'
#' @format A data frame with 17 rows and 6 variables:
#' \describe{
#'   \item{esti}{mean estimates of genes' treatment effects on mouse body weight}
#'   \item{X2.5.}{2.5\% percentile of the estimates of genes' treatment effects on mouse body weight}
#'   \item{X97.5.}{97.5\% percentile of the estimates of genes' treatment effects on mouse body weight}
#'   \item{X5.}{5\% percentile of the estimates of genes' treatment effects on mouse body weight}
#'   \item{X95.}{95\% percentile of the estimates of genes' treatment effects on mouse body weight}
#'   \item{signif}{significance}
#'   }
#'
#' @source \url{https://arxiv.org/abs/2011.04504}
"mice_est_nulltr"


















