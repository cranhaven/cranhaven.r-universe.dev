#' Simulated Dataset 87
# Date: July 20, 2018
#'
#' @docType data
#' @name simdata87
#' @usage data(simdata87)
#' @keywords datasets
#'
#' @description  The 87th dataset from the simulation study with 10 percent of observations were below the detection limit (BDL) based of a real epidemiological dataset. Out of 1000 subjects, fourteen correlated chemicals are completely observed (in X.true). In this simulation design, each chemical was simulated from independent normal distributions.
#'
#' BDLs were created using the bottom 10th percentile of the true data. Three covariates are considered:  the child’s age, the child’s sex (Male/Female), and the child’s ethnicity/race (White, Non-Hispanic White, and Other). After creating a model matrix, male white newborns (age = 0) serves as the  reference. The age is simulated from a normal with mean of 3.78 and standard deviation of 1.85 truncated between 0 and 8. The categorical variables are simulated from independent binomial distributions.  The outcome will be simulated using a logistic WQS model with complete data:
#' \deqn{logit(\mu_{i}) = -1.25 + log(1.75)*WQS_{i} + 0.032*z_{age}+ -0.0285*z_{sex} + 0.540*z_{His} + 0.120*z_{other} }
#' where \deqn{ WQS_{i}= \Sigma_{j=1}^{c}(w_{j}*q_{ij}) }
#'
#'  with four of the 14 weights \eqn{w_j}'s being 0.25 and the rest 0. The \eqn{q_ij} refers to the quantile score of the \emph{jth} chemical in the \emph{ith} subject.
#'
#' @format  A list that contains: \itemize{
#'   \item y.scenario: A binary outcome (1 = case, 0 = control)
#'   \item X.true: 14 chemicals; complete data.
#'   \item X.bdl: 14 chemicals with NA's subbed for the bottom 10th percentile of the true values.
#'   \item DL: The detection limit. Here, found to be the 10th percentile of X.true
#'   \item n0: A vector of length 14 indicating the number of non-detects.
#'   \item delta: A vector of length 14 indicating whether the chemical is observed (1) or not (0)
#'   \item Z.sim: A data-frame of covariates consisting of: \itemize{
#'          \item Age: A continuous covariate of child's age , simulated using normal with mean of 3.78 and sd of 1.85, truncated between 0 and 8, the maximum age of the case.
#'          \item Female: Binary variable child's sex, simulated using the proportion of females (0.42) by binomial distribution.
#'          \item Hispanic, Non-Hispanic_Others: Two indicator variables of child's race/ethnicity, sampled from independent binomial distributions  (proportion of Hispanic: 0.33; proportion of Other: 0.23).
#'          }
#'   \item time: The time required to simulate the data.
#'   }
#'
#' @references
#'  Ward, M. H., Colt, J. S., Metayer, C., Gunier, R. B., Lubin, J., Crouse, V., … Buffler, P. A. (2009). Residential Exposure
#'  to Polychlorinated Biphenyls and Organochlorine Pesticides and Risk of Childhood Leukemia. Environmental Health Perspectives, 117(6), 1007–1013.
#'  https://doi.org/10.1289/ehp.0900583

#  <both my papers here>
# APA format
#'
#' @examples
#' simdata87 <- data(simdata87)
NULL

# l.data previously


# Params_Scenario is a list that consists of scenario parameters used to generate simdata87: \itemize{
# \item Simulation Settings \itemize{
# \item Scenario W10
# \item Proportion of BDL: 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1
# \item Strength of WQS Association (beta1):  0.5596158
# \item Odds Ratio Simulated: 0.2865048 1.7500000
# }
# \item
# \item Parameters for all Scenarios \itemize{
# \item Sample Size (n): 1000
# \item Number of chemicals (c):  14
# \item Realizations of each scenario (M):  100
# \item Number of imputations in MI (K): 0 1 5
# \item Data Generation Seed: 47336
# \item More Information printed to screen? FALSE
# \item Chemical Weights (w)
#       x1 x2 x3 x4 x5   x6    x7    x8  x9   x10  x11   x12    x13  x14
#       0  0  0  0  0    0.25  0.25  0   0    0    0.25  0.25   0    0
