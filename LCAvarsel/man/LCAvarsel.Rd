\name{LCAvarsel}
\alias{LCAvarsel}
\alias{print.LCAvarsel}
\title{Variable selection for latent class analysis}

\description{
Perform variable selection for latent class analysis for multivariate categorical data clustering. The function allows to find the set of variables with relevant clustering information and discard those that are redundant and/or not informative. Different searching methods can be used: stepwise backward or forward, swap-stepwise backward or forward, and stochastic evolutionary search via genetic algorithm. Concomitant covariates can be also included in the estimation of the latent class analysis model.}

\usage{
LCAvarsel(Y, G = 1:3, X = NULL,
          search = c("backward", "forward", "ga"),
          independence = FALSE, 
          swap = FALSE,
          bicDiff = 0,
          ctrlLCA = controlLCA(), 
          ctrlReg = controlReg(), 
          ctrlGA = controlGA(),
          start = NULL,
          checkG = TRUE,
          parallel = FALSE, 
          verbose = interactive())
}


\arguments{
\item{Y}{A dataframe with (response) categorical variables. The categorical variables used to fit the latent class analysis model are converted to \code{factor}. Missing values are not allowed and observations with \code{NA} entries are automatically removed.}

\item{G}{An integer vector specifying the numbers of latent classes for which the BIC is to be calculated.}

\item{X}{A vector or dataframe of concomitant covariates to be used to predict the class membership probabilities. If supplied, the number of observations of \code{X} must match the number of \code{Y} and missing values are automatically removed. If \code{NULL}, a model with no predictor variables is estimated. Note that the variable selection procedure does NOT perform selection of the concomitant covariates.}

\item{search}{A character vector indicating the type of search: \code{"backward"} starts from a model with all the available variables and at each step of the algorithm removes/adds a variable until no further change to the clustering set; \code{"forward"} starts from a minimum identifiable model and at each step of the algorithm adds/removes a variable until no further change to the clustering set; \code{"ga"} performs a stochastic search via a genetic algorithm.}

\item{independence}{A logical value indicating if, at each step of the selection algorithm, the proposed/non-clustering variables must be assumed independent from the current set of clustering variables.}

\item{swap}{A logical value indicating wheter or not a swap-stepwise search must be performed. If \code{TRUE}, a swap move is executed after each add and removal step. Only used when \code{search} is set to \code{"backward"} or \code{"forward"}.}

\item{bicDiff}{A numerical value indicating the minimum absolute BIC difference between clustering model and no clustering model used to accept the inclusion/removal of a variable into/from the set of clustering
variables in the stepwise and swap-stepwise search algorithms.}

\item{ctrlLCA}{A list of control parameters for estimation of the latent class analysis model via EM algorithm; see also \code{\link{controlLCA}}.}

\item{ctrlReg}{A list of control parameters for the multinomial logistic regression step used to model the conditional distribution of the proposed/non-clustering variables. Only used when \code{independence = FALSE}; see also \code{\link{controlReg}}.}

\item{ctrlGA}{A list of control parameters for the genetic algorithm employed for the variable selection procedure when \code{search = "ga"}; see also \code{\link{controlGA}}.}

\item{start}{A character vector or a numeric binary matrix of initial clustering variables. When \code{search} is set to \code{"backward"} or \code{"forward"}, if supplied, it must be a character vector of variable names to be used as the initial clustering set. When \code{search = "ga"}, if provided, it must be a binary matrix of solutions indicating the set(s) of clustering variables included in the initial population of the genetic algorithm.}

\item{checkG}{A logical argument indicating if the identifiability of the latent class analysis model has to be checked for the values of \code{G} given in input. When \code{TRUE} (by default) only identifiable models according to the criterion described in \code{\link{maxG}} are estimated. If \code{FALSE}, also non identifiable models are estimated during the variable selection procedure; in this last case, \emph{use it at your own risk!}}

\item{parallel}{A logical argument indicating if parallel computation should be used. If \code{TRUE}, all the available cores are used. The argument could also be set to a numeric integer value specifying the number of cores to be employed.}

\item{verbose}{A logical argument specifying wether the iterations of the variable selection procedure need to be shown or not. By default is \code{TRUE} if the session is interactive, \code{FALSE} otherwise.}
}

\value{
An object of class \code{'LCAvarsel'} containing the following components:
\item{variables}{A character vector containing the set of selected relevant clustering variables.}
\item{model}{An object of class \code{'fitLCA'} corresponding to the latent class analysis model fitted on the selected variables. See \code{\link{fitLCA}}.}
\item{info}{A dataframe or a matrix containing information about the iterations of the variable selection procedure. If \code{search} is \code{"backward"} or \code{"forward"}, \code{info} is a dataframe with a row for each step of the algorithm and provides information regarding the type of step (Remove/Add), the name of the proposed variable, the BIC difference between the clustering model and the no clustering model for the proposed variable and the decision (Accepted/Rejected). When \code{search = "ga"}, \code{info} is a matrix containing summary statistics of the fitness function for the last 10 iterations of the genetic algorithm.}
\item{search}{A character string indicating the type of search used to perform the variable selection.}
\item{swap}{A logical value indicating if the swap move was used in the selection procedure. If \code{search = "ga"}, the value is \code{NULL}.}
\item{independence}{A logical value indicating if the proposed/non-clustering variables have been assumed independent from the current set of clustering variables during the search.}
\item{GA}{An object of class \code{'ga-class'} with information about the genetic algorithm. Only present when \code{search = "ga"}. See \code{\link[GA]{ga-class}}.}
\item{na}{A numeric vector which contains the row indices of the observations removed because of missing values. Only present when the provided data matrix \code{X} contains \code{NA}s.}
}

\details{
This function implements variable selection methods for latent class analysis for model-based clustering of multivariate categorical data. The general framework is based on a model-selection approach where the usefulness for clustering of a variable is assessed by comparing different models: a model where the variable contains relevant clustering information versus a model where it does not and it is redundant or not informative.

The model selection task corresponds to a combinatorial optimization problem and to conduct the search over the models space the following methods are available:
\itemize{
\item \emph{Stepwise backward/forward}. Enabled when \code{search = "backward"}. The algorithm starts from a model with all the variables included in the clustering set, then at each step a variable is removed/added until there is no further modification to the set of selected variables. At the start of the variable selection procedure, two consecutive removal steps are performed if \code{start = NULL}.
\item \emph{Stepwise forward/backward}. Enabled when \code{search = "forward"}. The algorithm  starts from the minimum subset of variables that allows a latent class analysis model to be identified, then the variables are added/removed in turn to/from the set of clustering variables until no further change to the set of selected ones. The initial set of clustering variables is chosen by default using the strategy described in Dean and Raftery (2010); however, argument \code{start} can be used to provide an alternative set of initial clustering variables.
\item \emph{Swap-stepwise backward/forward}. Enabled when \code{search = "backward"} and \code{swap = TRUE}. In this case, an additional swap move is performed after each removal and addition step.
\item \emph{Swap-stepwise forward/backward}. Enabled when \code{search = "forward"} and \code{swap = TRUE}. In this case, an extra swap move is performed after each addition and removal step.
\item \emph{Stochastic evolutionary search}. Enabled when \code{search = "ga"}. A genetic algorithm with binary encoding is employed to search for the optimal set of clustering variables. The algorithm stops when the maximum number of iterations specified by \code{maxiter} has been reached or there are no further improvement in the fitness function after \code{run} iterations; see \code{controlGA}.
}

In the swapping step, a non-clustering variable is switched with a clustering one. The couple of variables to be swapped is selected according to their evidence of being or not being useful for clustering. This step can prevent the algorithm from getting trapped into a local sub-optimum when many correlated variables are present; however, it increases the computational cost of the variable selection procedure. 

By default, at each step the variable selection procedure considers only latent class analysis models for which the identifiability condition described in \code{\link{maxG}} holds. When performing stepwise or swap-stepwise selection, for some combinations of clustering variables and number of classes, it could happen that a step of the variable selection procedure could not be performed because no latent class model is identifiable on any of the possible clustering sets. In such case, the step is not performed and a NA is returned. In the case of evolutionary search, non identifiable models are automatically discarded. When \code{checkG = FALSE}, also non identifiable models are estimated and considered during the variable selection process. Note that in this case the final output could be unreliable.

The stochastic evolutionary search implemented via the genetic algorithm allows for a better exploration of the model space. During the search, multiple sets of clustering variables are considered at the same time; then, for each set, a latent class analysis model is estimated on the clustering variables and a regression/independence model is estimated on the non-clustering ones. Different sets are generated by various genetic operators and the fittest individuals are selected. The fitness function is defined as the BIC of the joint distribution of both clustering and non-clustering variables, where clustering variables are modeled via a latent class analysis model and non-clustering variables are modeled via multinomial logistic regression or simple independent multinomial distributions in the case \code{independence = TRUE}. The nature of the genetic algorithm leads to a more exhaustive search, however with a larger computational cost than standard stepwise selection methods. The use of the \code{parallel} option allows for the estimation of multiple models in parallel and can speed up the computations.

If provided, the vector/matrix of concomitant covariates given in input in \code{X} is included in the latent class analysis model for the clustering variables at each step of the variable selection process. Thus, formally, a "latent class regression" model is estimated on the clustering variables (see \code{\link{fitLCA}}). Note that these covariates are only used to predict the class membership probabilities and no selection is performed on them.
}

%\author{Michael Fop, Thomas Brendan Murphy}

\examples{
\dontrun{
# few simple examples
data(carcinoma, package = "poLCA")
sel1 <- LCAvarsel(carcinoma)                  # Fop et al. (2017) method with no swap step
sel2 <- LCAvarsel(carcinoma, swap = TRUE)     # Fop et al. (2017) method with swap step
sel3 <- LCAvarsel(carcinoma, search = "forward", 
                  independence = TRUE)        # Dean and Raftery(2010) method
sel4 <- LCAvarsel(carcinoma, search = "ga")   # stochastic evolutionary search

# an example with a concomitant covariate 
data(election, package = "poLCA")
elec <- election[, cbind("MORALG", "CARESG", "KNOWG", "LEADG", "DISHONG", "INTELG",
                         "MORALB", "CARESB", "KNOWB", "LEADB", "DISHONB", "INTELB")]
party <- election$PARTY
fit <- fitLCA(elec, G = 3, X = party)
sel <- LCAvarsel(elec, G = 3, X = party, parallel = TRUE)
pidmat <- cbind(1, 1:7)
exb1 <- exp(pidmat \%*\% fit$coeff)
exb2 <- exp(pidmat \%*\% sel$model$coeff)
par(mfrow = c(1,2))
matplot(1:7, ( cbind(1, exb1)/(1 + rowSums(exb1)) ),
        ylim = c(0,1), type = "l",
        main = "Party ID as a predictor of candidate affinity class",
        xlab = "Party ID: strong Democratic (1) to strong Republican (7)",
        ylab = "Probability of latent class membership", 
        lwd = 2 , col = 1)
matplot(1:7, ( cbind(1, exb2)/(1 + rowSums(exb2)) ),
        ylim = c(0,1), type = "l",
        main = "Party ID as a predictor of candidate affinity class",
        xlab = "Party ID: strong Democratic (1) to strong Republican (7)",
        ylab = "Probability of latent class membership", 
        lwd = 2 , col = 1)
# compare
compareCluster(fit$class, sel$model$class)
}
}

\references{
Fop, M., and Smart, K. M. and Murphy, T. B. (2017). Variable selection for latent class analysis with application to low back pain diagnosis. \emph{Annals of Applied Statistics}, 11(4), 2085-2115.

Dean, N. and Raftery, A. E. (2010). Latent class analysis variable selection. \emph{Annals of the Institute of Statistical Mathematics}, 62:11-35.

Scrucca, L. (2017). On some extensions to GA package: Hybrid optimisation, parallelisation and islands evolution. \emph{The R Journal}, 9(1), 187-206.

Scrucca, L. (2013). GA: A package for genetic algorithms in R. \emph{Journal of Statistical Software}, 53(4), 1-3.
}

\seealso{
\code{\link{fitLCA}}, \code{\link{maxG}}
}
