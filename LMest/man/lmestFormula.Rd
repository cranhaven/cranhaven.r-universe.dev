\name{lmestFormula}
\alias{lmestFormula}
\title{Formulas for \code{LMest} functions}
\description{Bulding formulas for \code{\link{lmest}}, \code{\link{lmestCont}}, \code{\link{lmestMixed}}, and \code{\link{lmestMc}}.}
\usage{
lmestFormula(data,
              response, manifest = NULL,
              LatentInitial = NULL, LatentTransition = NULL,
              AddInterceptManifest = FALSE,
              AddInterceptInitial = TRUE,
              AddInterceptTransition = TRUE, responseStart = TRUE,
              manifestStart = TRUE, LatentInitialStart = TRUE,
              LatentTransitionStart = TRUE)
}
\arguments{
  \item{data}{a data.frame or a matrix of data}
  \item{response}{a numeric or character vector indicating the column indices or the names for the response variables}
  \item{manifest}{a numeric or character vector indicating the column indices or the names for the covariates affecting the measurement model}
  \item{LatentInitial}{a numeric or character vector indicating the column indices or the names for the covariates affecting the initial probabilities}
  \item{LatentTransition}{a numeric or character vector indicating the column indices or the names for the covariates affecting the transition probabilities}
  \item{AddInterceptManifest}{a logical value indicating whether the intercept is added to the covariates affecting the measurement model}
  \item{AddInterceptInitial}{a logical value indicating whether the intercept is added to covariates affecting the initial probabilities}
  \item{AddInterceptTransition}{a logical value indicating whether the intercept is added to covariates affecting the transition probabilities}
  \item{responseStart}{a logical value indicating whether the response variables names start with \code{response} argument}
  \item{manifestStart}{a logical value indicating whether the covariates names start with \code{manifest} argument}
  \item{LatentInitialStart}{a logical value indicating whether the covariates names start with \code{LatentInitial} argument}
  \item{LatentTransitionStart}{a logical value indicating whether the covariates names start with \code{LatentTransition} argument}
}
\details{
Generates formulas for \code{responsesFormula} and \code{latentFormula} to use in \code{\link{lmest}}, \code{\link{lmestCont}}, \code{\link{lmestMixed}}, and \code{\link{lmestMc}}.
}
\value{
Returns a list with \code{responsesFormula} and \code{latentFormula} objects.
}


\author{
Francesco Bartolucci,
  Silvia Pandolfi,
  Fulvia Pennoni,
  Alessio Farcomeni,
  Alessio Serafini}

\examples{
data(data_SRHS_long)
names(data_SRHS_long)

# Formula with response srhs and covariates for both initail and transition: 
# gender,race,educational,age.

## LM model with covariates on the latent model
# and with intercepts on the initial and transition probabilities

fm <- lmestFormula(data = data_SRHS_long,
                   response = "srhs",
                   LatentInitial = 3:6, LatentTransition = 3:6)
fm

## LM model with covariates on the latent model
# and without intercepts on the initial and transition probabilities

fm <- lmestFormula(data = data_SRHS_long,
                   response = "srhs",
                   LatentInitial = 3:6, LatentTransition = 3:6,
                   AddInterceptInitial = FALSE,AddInterceptTransition = FALSE)
fm

######

data(data_criminal_sim)
str(data_criminal_sim)

# Formula with only the responses from y1 to y10

fm <- lmestFormula(data = data_criminal_sim,response = "y")$responsesFormula
fm

# Formula with only the responses from y1 to y10 and intercept for manifest

fm <- lmestFormula(data = data_criminal_sim,
                   response = "y",AddInterceptManifest = TRUE)$responsesFormula
fm


## LM model for continous responses

data(data_long_cont)
names(data_long_cont)

# Formula with response Y1, Y2, no covariate for manifest,
# X1 covariates for initail and X2 covariate for transition

fm <- lmestFormula(data = data_long_cont,
                   response = c("Y"),
                   LatentInitial = "X",
                   LatentTransition = "X2")
fm

## Wrong model specification since two variable start with X.
# Check the starts arguments. 

# For the right model:

fm <- lmestFormula(data = data_long_cont,
                   response = c("Y"),
                   LatentInitial = "X1",LatentTransition = "X2")
fm

## or

fm <- lmestFormula(data = data_long_cont,
                   response = c("Y"),
                   LatentInitial = 6,LatentTransition = "X2",
                   LatentInitialStart = FALSE)
fm

\dontrun{

data(data_criminal_sim)
data_criminal_sim <- data.frame(data_criminal_sim)

# Mixed LM model for females

responsesFormula <- lmestFormula(data = data_criminal_sim,
                                 response = "y")$responsesFormula

out <- lmest(responsesFormula = responsesFormula,
             index = c("id","time"),
             data = data_criminal_sim,
             k = 2)
}
}
