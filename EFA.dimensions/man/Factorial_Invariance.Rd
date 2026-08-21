\name{Factorial_Invariance}
\alias{Factorial_Invariance}
\title{Factor Model Invariance}
\description{Conducts tests for configural, metric, scalar, and strict invariance 
of factor models across groups.}
\usage{
Factorial_Invariance(model, data, group, estimator = 'ML', verbose = TRUE)
}
\arguments{
  \item{model}{
  \code{}A CFA model in lavaan package format. Example: 
  \cr\cr model_RSE <- '
  \cr pos_items =~ Q1 + Q2 + Q4 + Q6 + Q7
  \cr neg_items =~ Q3_R + Q5_R + Q8_R + Q9_R + Q10_R '}

  \item{data}{
  \code{}A dataframe with the model variables and the group factor.
  \cr Example: data = data_RSE_sex}

  \item{group}{
  \code{}The name of the group variable in data.
  \cr Example: group = 'gender'}

 \item{estimator}{
  \code{}The name of the lavaan estimator to be used in the analyses.
  \cr The options for basic estimators (continuous data) are 
  ML, GLS, WLS, DWLS, ULS, DLS, and PML.
  \cr The options for robust estimators are MLM, MLMVS, MLMV, MLF, MLR, 
  WLSM, WLSMVS, WLSMV, ULSM, ULSMVS, and ULSMV.
  \cr Example: estimator = 'ML'}

  \item{verbose}{
  \code{}(optional) Should detailed results be displayed in console? 
  \cr TRUE (default) or FALSE}
}
\details{
  This is a wrapper function that uses functions from the lavaan (Rosseel, 2012) 
  and semTools (Jorgensen et al., 2026) packages.
  
  The analyses can be run for regular CFAs or for ESEM measurement models. See the Examples below.
}
\value{A list containing the following components:
\item{fitcoefs_by_group}{The model fit coefficients for each group separately}
\item{model_Configural}{The parameter estimates for the configural invariance model}
\item{model_Metric}{The parameter estimates for the metric invariance model}
\item{model_Scalar}{The parameter estimates for the scalar invariance model}
\item{model_Strict}{The parameter estimates for the strict invariance model}
\item{inv_model_fits}{The invariance model fit coefficients}
\item{inv_model_fit_diffs}{Differences in the invariance model fit coefficients}
\item{chisq_diffs}{Chi-Squared model differences tests}
}
\references{
       {Jorgensen, T. D., Pornprasertmanit, S., Schoemann, A. M., & 
       Rosseel, Y. (2026). semTools: Useful tools for structural equation 
       modeling. R package version 0.5-8. 
       Retrieved from https://CRAN.R-project.org/package=semTools}
\cr\cr {Milfont, T. L., & Fischer, R. (2015). Testing measurement invariance across 
        groups: Applications in cross-cultural research. 
        \emph{International Journal of Psychological Research, 3,} 111130.}
\cr\cr {Rosseel, Y. (2012). lavaan: An R package for structural equation modeling. 
        \emph{Journal of Statistical Software, 48,} 136.}
\cr\cr {Stark, S., Chernyshenko, O. S., & Drasgow, F. (2006). Detecting differential 
        item functioning with confirmatory factor analysis and item response theory: 
        Toward a unified strategy. \emph{Journal of Applied Psychology, 91(6),} 12921306.}
\cr\cr {Stein, J. A., Lee, J. W., & Jones, P. S. (2006). Assessing cross-cultural 
        differences through use of multiple-group invariance analyses. 
        \emph{Journal of Personality Assessment, 87(3),} 249258.}
\cr\cr {Tan, T. (2024) Frequentist and Bayesian factorial invariance using R. 
        \emph{Practical Assessment, Research, and Evaluation. 29(8),} 1-34.}
}
\author{Brian P. O'Connor}
\examples{
model_RSE <- '
   pos_items =~ Q1 + Q2 + Q4 + Q6 + Q7
   neg_items =~ Q3_R + Q5_R + Q8_R + Q9_R + Q10_R '

Factorial_Invariance(model = model_RSE, data = data_RSE_sex, 
                     group = 'gender', estimator = 'ML')
\donttest{
# run Factorial_Invariance for an ESEM model
# first, use the ESEM function to obtain the esem_model_syntax (without the group variable, gender)
esem_output <- ESEM(data = subset(data_RSE_sex, select = -c(gender)), Nfactors = 2)

Factorial_Invariance(model = esem_output$esem_model_syntax, data = data_RSE_sex, 
                     group = 'gender', estimator = 'ML')
}
}
