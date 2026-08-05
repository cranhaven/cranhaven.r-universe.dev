\name{default_varying}
\alias{default_param}
\alias{default_save}
\alias{default_tuning}
\alias{default_varying}
\title{
Setting Default Values in \code{clustGLMM}
}
\description{
Sets default values for the hyperparameters of the priors \code{default_param}, indicators of group-specificity \code{default_varying}, integer and double valued tuning parameters \code{default_tuning} and indicators of what variables should be saved \code{default_save}.
}
\usage{
default_param(sparse = TRUE, ...)
default_save(...)
default_tuning(experience = FALSE, ...)
default_varying(...)
}
\arguments{
  \item{sparse}{a logical, if \code{TRUE} the rate and shape hyperparameters of \code{e0} are set to values encouraging sparsity in the weights}
  \item{experience}{a logical, indicating if the default values are
    changed to values proven suitable from empirical experience}
  \item{...}{named scalars to override default values}
}
\details{
If arguments \code{param}, \code{save}, \code{tuning} and \code{varying} of \code{\link{clustGLMM}} are not specified, these functions are used to create the default values. They can also be used to create prototypical \code{param}, \code{varying}, \code{tuning} and \code{save} argument specifications and then adjust them.
}
\value{
The output of \code{default_varying} is a named vector that contains a Boolean for each model parameter that may be considered group-specific. The default values are:
\describe{
  \item{\code{varying["prec_num"] = TRUE}}{group-specific precisions for numeric outcomes,}
  \item{\code{varying["c_ord"] = TRUE}}{group-specific ordered intercepts for ordinal outcomes,}
  \item{\code{varying["InvSigma"] = FALSE}}{group-invariant combined precision matrix for all random effects,}
  \item{\code{varying["InvQ"] = FALSE}}{group-invariant precision matrix used in prior for \code{InvSigma},}
  \item{\code{varying["naY"] = FALSE}}{group-invariant imputed missing outcome values.}
}

The output of \code{default_save} is a named vector that contains a Boolean for each model parameter including some transformed parameters indicating if they are saved and included in the return object. The default values are:
\describe{
  \item{\code{save["beta_num_fix"] = TRUE}}{fixed effects for numeric outcomes,}
  \item{\code{save["beta_num"] = TRUE}}{group-specific effects for numeric outcomes,}
  \item{\code{save["prec_num"] = TRUE}}{precisions for numeric outcomes,} 
  \item{\code{save["sd_num"] = TRUE}}{standard deviations for numeric outcomes (transformed from \code{prec_num}),}
  \item{\code{save["var_num"] = TRUE}}{variances for numeric outcomes (transformed from \code{prec_num}),}
  \item{\code{save["beta_poi_fix"] = TRUE}}{fixed effects for count outcomes,}
  \item{\code{save["beta_poi"] = TRUE}}{group-specific effects for count outcomes,}
  \item{\code{save["beta_bin_fix"] = TRUE}}{fixed effects for binary outcomes,}
  \item{\code{save["beta_bin"] = TRUE}}{group-specific effects for binary outcomes,}
  \item{\code{save["beta_ord_fix"] = TRUE}}{fixed effects for ordinal outcomes,}
  \item{\code{save["beta_ord"] = TRUE}}{group-specific effects for ordinal outcomes,}
  \item{\code{save["c_ord"] = TRUE}}{ordered intercepts for ordinal outcomes,}
  \item{\code{save["a_ord"] = FALSE}}{transformed ordered intercepts for ordinal outcomes into unrestricted variables,}
  \item{\code{save["pi_ord"] = TRUE}}{transformed \code{a_ord} into probabilities,}
  \item{\code{save["beta_cat_fix"] = TRUE}}{fixed effects for general categorical outcomes,} 
  \item{\code{save["beta_cat"] = TRUE}}{group-specific effects for general categorical outcomes,}
  \item{\code{save["InvSigma"] = TRUE}}{combined precision matrix for all random effects,}
  \item{\code{save["Sigma"] = TRUE}}{combined covariance matrix for all random effects (transformed from \code{InvSigma}),}
  \item{\code{save["sdSigma"] = TRUE}}{standard deviations for all random effects (transformed from \code{InvSigma}),} 
  \item{\code{save["corSigma"] = TRUE}}{correlations between all random effects (transformed from \code{InvSigma}),}
  \item{\code{save["detInvSigma"] = FALSE}}{determinant of \code{InvSigma} matrix,}
  \item{\code{save["InvQ"] = FALSE}}{precision matrix used in prior for \code{InvSigma},}
  \item{\code{save["Q"] = FALSE}}{covariance matrix used in prior for \code{InvSigma} (transformed from \code{InvQ}),}
  \item{\code{save["detInvQ"] = FALSE}}{determinant of \code{InvQ} matrix,}
  \item{\code{save["b"] = FALSE}}{a matrix of random effects for all outcomes and units,}
  \item{\code{save["w"] = TRUE}}{mixture weights of length \code{G} that sum up to 1,}
  \item{\code{save["ng"] = TRUE}}{cluster occupancies of length \code{G} that sum up to number of units,}
  \item{\code{save["loglik"] = TRUE}}{NOT a log-likelihood but a very crude approximation to it created as an easy to evaluate byproduct of full-conditional cluster allocation probabilities,}
  \item{\code{save["pUig"] = FALSE}}{a matrix of full-conditional cluster allocation probabilities,}
  \item{\code{save["U"] = TRUE}}{a vector of current allocations of units into clusters (values in \code{1, ..., G}),}
  \item{\code{save["Gplus"] = TRUE}}{a number of non-empty components,}
  \item{\code{save["e0"] = TRUE}}{precision parameter for prior of \code{w},}
  \item{\code{save["naY"] = FALSE}}{imputed missing outcome values.}
}

The output of \code{default_param} is a named list that contains values of the hyperparameters needed to set up the priors. The default values are:
\describe{
\item{\code{param[["InvSigma_df"]] = 1}}{prior degrees of freedom for Wishart prior for \code{InvSigma} (dimension of this matrix is automatically added to this value),}
\item{\code{param[["InvQ_df"]] = 1}}{prior degrees of freedom for Wishart prior for \code{InvQ} (dimension of this matrix is automatically added to this value),}
\item{\code{param[["prec_num_shp"]] = 1}}{prior shape for precision parameter \code{prec_num} for numeric outcomes,}
\item{\code{param[["prec_num_rte"]] = 10}}{prior rate for precision parameter \code{prec_num} for numeric outcomes,}
\item{\code{param[["beta_num_fix_sd"]] = 1}}{standard deviation for fixed effects for numeric outcomes,}
\item{\code{param[["beta_num_sd"]] = 1}}{standard deviation for group-specific effects for numeric outcomes,}
\item{\code{param[["beta_poi_fix_sd"]] = 1}}{standard deviation for fixed effects for count outcomes,}
\item{\code{param[["beta_poi_sd"]] = 1}}{standard deviation for group-specific effects for count outcomes,}
\item{\code{param[["beta_bin_fix_sd"]] = 1}}{standard deviation for fixed effects for binary outcomes,}
\item{\code{param[["beta_bin_sd"]] = 1}}{standard deviation for group-specific effects for binary outcomes,}
\item{\code{param[["beta_ord_fix_sd"]] = 1}}{standard deviation for fixed effects for ordinal outcomes,}
\item{\code{param[["beta_ord_sd"]] = 1}}{standard deviation for group-specific effects for ordinal outcomes,}
\item{\code{param[["api_prior"]] = 1}}{prior parameter for Dirichlet distribution of \code{pi_ord} (same for all levels),}
\item{\code{param[["beta_cat_fix_sd"]] = 1}}{standard deviation for fixed effects for general categorical outcomes,}
\item{\code{param[["beta_cat_sd"]] = 1}}{standard deviation for group-specific effects for general categorical outcomes,}
\item{\code{param[["init_b_sd"]] = 0.01}}{standard deviation for sampling initial values of random effects \code{b},}
\item{\code{param[["e0_shp"]] = 1}}{prior shape of \code{e0},}
\item{\code{param[["e0_rte"]] = 100}}{prior rate of \code{e0},}
\item{\code{param[["InvV"]] = 0.01}}{diagonal element of the prior inverse scale matrix for \code{InvQ}.}
}
By adjusting the values of \code{e0_shp} and \code{e0_rte} different levels of sparsity can be achieved. 
The lower \code{e0_shp/e0_rte} is, the more sparse results are expected.  
With \code{sparse = TRUE} the prior distribution is a gamma distribution with \code{e0_shp} = 1 and \code{e0_rte} = 100.
Otherwise \code{e0_shp} = 4 and \code{e0_rte} = 1 will be used, which prefers non-sparse solutions. 

The output of \code{default_tuning} is a named list of parameters that tune the sampling algorithm. For clarity, the parameters are split into two sublists depending on the expected type (\code{integer} or \code{double}). The default values are:
\describe{
\item{\code{$integer$freq_proposal_update = 10}}{every \code{freq_proposal_update} iterations change the proposal distributions for Metropolis steps,}
\item{\code{$integer$times_proposal = 1}}{how many times a new state is proposed within one Metropolis step,}
\item{\code{$integer$maxiter = 25}}{maximum iterations allowed during Newton-Raphson update of proposal distribution,}
\item{\code{$integer$maxnrep = 100}}{maximum number of repetitions of Newton-Raphson with a different start location,}
\item{\code{$integer$kspec_bi_cat = 0}}{Boolean as integer, if \code{TRUE} then each categorical outcome has a different set of random effects for each of \code{Kcat-1} levels,}
\item{\code{$double$const_proposal_beta_poi_fix = 1.0}}{constant by which the proposal direction for \code{beta_poi_fix} is multiplied (const_proposal^2 to variance),}
\item{\code{$double$const_proposal_beta_poi = 1.0}}{constant by which the proposal direction for \code{beta_poi} is multiplied (const_proposal^2 to variance),}
\item{\code{$double$const_proposal_beta_bin_fix = 1.0}}{constant by which the proposal direction for \code{beta_bin_fix} is multiplied (const_proposal^2 to variance),}
\item{\code{$double$const_proposal_beta_bin = 1.0}}{constant by which the proposal direction for \code{beta_bin} is multiplied (const_proposal^2 to variance),}
\item{\code{$double$const_proposal_beta_ord_fix = 1.0}}{constant by which the proposal direction for \code{beta_ord_fix} is multiplied (const_proposal^2 to variance),}
\item{\code{$double$const_proposal_beta_ord = 1.0}}{constant by which the proposal direction for \code{beta_ord} is multiplied (const_proposal^2 to variance),}
\item{\code{$double$const_proposal_beta_cat_fix = 1.0}}{constant by which the proposal direction for \code{beta_cat_fix} is multiplied (const_proposal^2 to variance),}
\item{\code{$double$const_proposal_beta_cat = 1.0}}{constant by which the proposal direction for \code{beta_cat} is multiplied (const_proposal^2 to variance),}
\item{\code{$double$const_proposal_a_ord = 1.0}}{constant by which the proposal direction for \code{a_ord} is multiplied (const_proposal^2 to variance),}
\item{\code{$double$const_proposal_b = 1.0}}{constant by which the proposal direction for \code{b} is multiplied (const_proposal^2 to variance),}
\item{\code{$double$const_proposal_e0 = 1.0}}{constant by which the proposal direction for \code{e0} is multiplied (const_proposal^2 to variance),}
\item{\code{$double$tolerance = 1e-07}}{tolerance when computing the norm of the shift within Newton-Raphson.}
}
With \code{experience = TRUE} the chosen values will be set to those proven suitable from empirical experience.


}
\references{
Vávra, J., Komárek, A., Grün, B., Malsiner-Walli, G. Clusterwise multivariate regression of mixed-type panel data. Stat Comput 34, 46 (2024). https://doi.org/10.1007/s11222-023-10304-5
}
\author{
Jan Vávra
}
\seealso{
\code{\link{clustGLMM}}
}
\examples{
(param <- default_param())
(param <- default_param(sparse = FALSE))
(save <- default_save())
(tuning <- default_tuning())
(tuning <- default_tuning(experience = TRUE))
(varying <- default_varying())

}
