\name{assign.folds}
\alias{assign.folds}
\title{Assign observations to folds in a balanced way}
\description{Function to assign observations to folds, ensuring a similar distribution across folds (and sites).}
\usage{assign.folds(y, family = c("binomial", "cox", "gaussian"), site = NULL, nfolds = 10)}
\arguments{
  \item{y}{response to be predicted. A binary vector for \code{"binomial"}, an object of class \code{"Surv"} for \code{"cox"}, or a numeric vector for \code{"gaussian"}.}
  \item{family}{distribution of y: \code{"binomial"}, \code{"cox"}, or \code{"gaussian"}.}
  \item{site}{vector with the sites' names, or NULL for studies conducted in a single site.}
  \item{nfolds}{number of folds.}
}
\details{If \code{family} is \code{"binomial"}, the function randomly assigns the folds separately for the two outcomes. If \code{family} is \code{"gaussian"}, the function randomly assigns the folds separately for ranges of the outcome. If \code{family} is \code{"gaussian"}, the function randomly assigns the folds separately for ranges of time and censorship. If \code{site} is not null, the function randomly assigns the folds separately for each site.}
\value{A numeric vector with the fold assigned to each observation}
\references{
  Solanes, A., Mezquida, G., Janssen, J., Amoretti, S., Lobo, A., Gonzalez-Pinto, A., Arango, C., Vieta, E., Castro-Fornieles, J., Berge, D., Albacete, A., Gine, E., Parellada, M., Bernardo, M.; PEPs group (collaborators); Pomarol-Clotet, E., Radua, J. (2022)
  Combining MRI and clinical data to detect high relapse risk after the first episode of psychosis.
  \emph{Schizophrenia}, \bold{8}, 100, doi:10.1038/s41537-022-00309-w.
}
\author{Joaquim Radua and Aleix Solanes}
\seealso{
  \code{\link{cv}} for conducting a cross-validation.
}
\examples{
# Create random y (numeric)
y = rnorm(200, sample(c(1, 10), 200, replace = TRUE))

# Assign folds
fold = assign.folds(y, "gaussian", nfolds = 4)

# Check that the distribution of y is similar across folds
oldpar = par(mfrow = c(2, 2))
for (i in 1:4) {
  hist(y[which(fold == i)], main = paste("Fold", i), xlab = "y")
}
par(oldpar)
}
