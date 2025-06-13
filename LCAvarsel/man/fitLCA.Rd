\name{fitLCA}
\alias{fitLCA}
\alias{print.fitLCA}
\title{Latent class analysis model}

\description{
Estimation and model selection for latent class analysis and latent class regression model for clustering multivariate categorical data. The best model is automatically selected using BIC.
}

\usage{
fitLCA(Y, G = 1:3, X = NULL, ctrlLCA = controlLCA())
}

\arguments{
\item{Y}{A dataframe with (response) categorical variables. The categorical variables used to fit the latent class analysis model are converted to \code{factor}.}

\item{G}{An integer vector specifying the numbers of latent classes for which the BIC is to be calculated.}

\item{X}{A vector or dataframe of concomitant covariates used to predict the class-membership probability. If supplied, the number of observations of \code{X} must match the number of \code{Y}. If \code{NULL}, a model with no predictor variables is estimated.} 

\item{ctrlLCA}{A list of control parameters for the EM algorithm used to fit the model.}
}

\value{
An object of class \code{'fitLCA'} providing the optimal latent class model selected by BIC. 

The ouptut is a list containing:
\item{G}{The best number of latent classes according to BIC.}
\item{parameters}{A list with the following components:
  \describe{
        \item{\code{tau}}{The estimated mixing proportions.}
        \item{\code{theta}}{The estimated class conditional probabilities.}
  }
}
\item{coeff}{Multinomial logit coefficient estimates on the covariates (when provided). \code{coeff} is a matrix with G-1 columns, and one row for each covariate. All logit coefficients are calculated for each class with respect to class 1, assumed as reference by default.}
\item{loglik}{Value of the maximized Log-likelihood.}
\item{BIC}{All BIC values computed for the range of values of \eqn{G} provided.}
\item{bic}{The optimal BIC value.}
\item{npar}{Number of estimated parameters.}
\item{resDf}{Number of residual degrees of freedom.}
\item{z}{A matrix whose \code{[i,g]} entry is the probability that observation \code{i} belongs to the \code{g}th class.}
\item{class}{Classification corresponding to the maximum a posteriori of matrix \code{z}.}
\item{iter}{Number of iterations.}
}

\details{
The function is a simple wrapper around the function \code{\link{poLCA}} in the homonymous package and returns less information about the estimated model. The selection of the number of latent classes is performed automatically by means of the Bayesian information criterion (BIC).

When included, covariates are used to predict the probability of class membership. In this case the model is termed as "latent class regression", or, alternatively "concomitant-variable latent class analysis". See \code{\link{poLCA}} for details.
}

\examples{
data(gss82, package = "poLCA")
maxG(gss82, 1:7)      # not all latent class models can be fitted
fit <- fitLCA(gss82, G = 1:4)

\dontrun{
# diminish tolerance and increase number of replicates
fit2 <- fitLCA(gss82, G = 1:4, ctrlLCA = controlLCA(tol = 1e-06, nrep = 10))
}

# the example with a single covariate as in ?poLCA
data(election, package = "poLCA")
elec <- election[, cbind("MORALG", "CARESG", "KNOWG", "LEADG", "DISHONG", "INTELG",
                         "MORALB", "CARESB", "KNOWB", "LEADB", "DISHONB", "INTELB")]
party <- election$PARTY
fit <- fitLCA(elec, G = 3, X = party)
pidmat <- cbind(1, 1:7)
exb <- exp(pidmat \%*\% fit$coeff)
matplot(1:7, ( cbind(1, exb)/(1 + rowSums(exb)) ),
        ylim = c(0,1), type = "l",
        main = "Party ID as a predictor of candidate affinity class",
        xlab = "Party ID: strong Democratic (1) to strong Republican (7)",
        ylab = "Probability of latent class membership", 
        lwd = 2 , col = 1)
}

\references{
Linzer, D. A. and Lewis, J. B. (2011). poLCA: An R package for polytomous variable
latent class analysis. \emph{Journal of Statistical Software} 42 1-29.
}

\seealso{
\code{\link[poLCA]{poLCA}}
}
