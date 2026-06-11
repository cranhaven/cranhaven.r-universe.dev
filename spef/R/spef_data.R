#' Bladder Tumors Cancer Recurrences
#'
#' A data frame contains data on recurrences of bladder cancer,
#' used by many people to demonstrate methodology for recurrent event modeling.
#' The data was obtained by courtesy of Ying Zhang, containing records of 118 patients
#' from three treatment arms: 48 are from the placebo arm, 37 are from the thiotepa arm,
#' and the rest 33 are from the pyridoxine arm.
#'
#' @format A \code{data.frame} contains the following columns:
#' \describe{
#'   \item{\code{subject}}{patient id}
#'   \item{\code{time}}{observation time}
#'   \item{\code{count}}{cumulative number of tumors}
#'   \item{\code{count2}}{number of new tumors since last observation time}
#'   \item{\code{number}}{initial number of tumors (8=8 or more)}
#'   \item{\code{size}}{size (cm) of largest initial tumor}
#'   \item{\code{pyridoxine}}{dummy variable for pyridoxine arm}
#'   \item{\code{thiotepa}}{dummy variable for thiotepa arm}
#' }
#'
#' @references Byar, D. P. (1980). The Veterans administration study of chemoprophylaxis
#' for recurrent stage I bladder tumors: Comparisons of placebo, pyridoxine, and topical thiotepa.
#' \emph{Bladder Tumors and Other Topics in Urological Oncology}, pp. 363--370. New York: Plenum.
#' @references Wellner, J. A. and Zhang, Y. (2007) Two likelihood-based semiparametric estimation
#' methods for panel count data with covariates. \emph{Annals of Statistics}, \bold{35}(5), 2106--2142.
#' @references Lu, M., Zhang, Y. and Huang, J. (2009) Semiparametric estimation methods for panel
#' count data using monotone B-Splines. \emph{Journal of the American Statistical Association}
#' \bold{104}(487), 1060--1070.
#' 
#' @docType data
#' @usage data(bladTumor)
#' @name bladTumor
#'
#' @note To our surprise, the two-treatment (placebo and thiotepa) subset of
#'   the full version \code{bladTumor} do not match the two-treatment
#'   version \code{blaTum}.
#'
#' @seealso \code{\link{blaTum}}
#' @example inst/examples/ex_bladTumor.R
NULL

#' Bladder Tumors Cancer Recurrences
#'
#' A data frame contains data on recurrences of bladder cancer,
#' used by many people to demonstrate methodology for recurrent event modelling.
#' This data set organized differently from \code{bladTumor}.
## and is adapted from \url{http://onlinelibrary.wiley.com/journal/10.1111/(ISSN)1467-9868/homepage/62_2.html}.
#' The data contains records of 85 patients from two treatment arms:
#' 48 are from the placebo arm, and the rest 37 are from the thiotepa arm.
#'
#' @format A \code{data.frame} contains the following columns:
#' \describe{
#'   \item{\code{id}}{patient id}
#'   \item{\code{treatment}}{placebo = 0, thiotepa = 1}
#'   \item{\code{size}}{size (cm) of largest initial tumor}
#'   \item{\code{num}}{initial number of tumors (8 = 8 or more)}
#'   \item{\code{time}}{observation time}
#'   \item{\code{count}}{number of new tumors since last observation time}
#' }
#'
#' @references Byar, D. P. (1980). The Veterans administration study of chemoprophylaxis for
#' recurrent stage I bladder tumors: comparisons of placebo, pyridoxine, and topical thiotepa.
#' \emph{Bladder Tumors and Other Topics in Urological Oncology}, pp. 363--370. New York: Plenum.
#' @references Sun, J. and Wei, L. J. (2000) Regression analysis of panel count data
#'  with covariate dependent observation and censoring times.
#'  \emph{Journal of the Royal Statistical Society, Series B: Statistical Methodology},
#' \bold{62}(2), 293--302.
#' @references Huang, C. Y., Wang, M. C. and Zhang, Y. (2006).
#' Analyzing panel count data with informative observation times.
#' \emph{Biometrika}, \bold{93}(4): 763--776.
#'
#' @note To our surprise, the two-treatment (placebo and thiotepa) subset of
#' the full version \code{bladTumor} do not match the two-treatment version \code{blaTum}.
#'
#' @docType data
#' @name blaTum
#' @usage data(blaTum)
#' 
#' @seealso \code{\link{bladTumor}}
#' @example inst/examples/ex_blaTum.R
NULL

#' Skin cancer chemoprevention trial
#'
#' A data frame contains data on the recurrence of skin tumor.
#' The original data is available in Table A.3. of Sun and Zhao (2013).
#' This dataset contains records of 290 patients.
#'
#' @format This data frame contains the following columns:
#' \describe{
#'   \item{\code{id}}{: patient id (repeated for each recurrence).}
#'   \item{\code{time}}{: observation time.}
#'   \item{\code{age}}{: patient's age at enrollment.}
#'   \item{\code{male}}{: gender; male = 1, female = 0.}
#'   \item{\code{dfmo}}{: treatment (DFMO) group = 1; placebo = 0.}
#'   \item{\code{priorTumor}}{: number of prior tumor from diagnosis to randomization.}
#'   \item{\code{countBC}}{: number of newly developed basal cell carcinomas tumors since
#'   last observation time.}
#'   \item{\code{countSC}}{: number of newly developed squamous cell carcinomas
#'   tumors since last observation time.}
#'   \item{\code{count}}{: number of newly developed non-melanoma tumors
#'   since last observation time; this is equal to \code{countBC + countSC}.}
#' }
#'
#' @references Chiou, S., Xu, G., Yan, J., and Huang, C.-Y. (2017).
#' Semiparametric estimation of the accelerated mean model with panel count data under
#' informative examination times. \emph{Biometrics}, to appear. <doi: 10.1111/biom.12840>.
#' @references Sun, J. and Zhao, X. (2013). Statistical Analysis of Panel Count
#'  Data. New York: Springer.
#'
#' @docType data
#' @name skinTumor
#' @usage data(skinTumor)
#' @seealso \code{skiTum}
#' 
#' @example inst/examples/ex_skinTumor.R
NULL

#' A Simulated Data Mimicking a Skin Cancer Chemoprevention Trial
#'
#' A data frame contains data on the recurrence of skin tumor.
#' This simulated data is created for illustrative purpose and it mimic
#' the Skin Cancer Chemoprevention Trial used in Chiou et al. (2017).
#' This data set contains records of 100 simulated patients.
#'
#' @format This data frame contains the following columns:
#' \describe{
#'     \item{\code{id}}{: patient id (repeated for each recurrence).}
#'     \item{\code{time}}{: observation time.}
#'     \item{\code{age}}{: patient's age at enrollment; age = 1 if greater
#'       than 65, age = 0 otherwise.}
#'     \item{\code{male}}{: gender; male = 1, female = 0.}
#'     \item{\code{dfmo}}{: treatment (DFMO) group = 1; placebo = 0.}
#'     \item{\code{priorTumor}}{: number of prior tumor from diagnosis to randomization.}
#'     \item{\code{count}}{: number of new tumors since last observation time.}
#'   }
#'
#' @docType data
#' @name skiTum
#' @usage data(skiTum)
#' @seealso \code{skinTumor}
#' 
#' @example inst/examples/ex_skiTum.R
NULL
