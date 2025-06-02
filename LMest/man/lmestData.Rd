\name{lmestData}
\alias{lmestData}

\title{Data for \code{LMest} functions}
\description{An object of class \code{lmestData} containing data in long format, some necessary information on the data structure and objects for the estimation functions.}
\usage{
lmestData(data, id = NULL, time = NULL,
          idAsFactor = TRUE, timeAsFactor = TRUE,
          responsesFormula = NULL, latentFormula = NULL,
          na.rm = FALSE, check.names = FALSE)
}
\arguments{
  \item{data}{a matrix or data frame in long format of observation}
  \item{id}{a numeric vector or a string indicating the column with the unit identifier. If NULL, the first column is considered}
  \item{time}{a numeric vector or a string indicating the column with the time occasions. If NULL, the second column is considered, and if the \code{id} is not NULL, the function will automatically add the column with the time occasions}
  \item{idAsFactor}{a logical value indicating whether or not the column with the ids is converted to a factor. (By default is set to TRUE)}
  \item{timeAsFactor}{a logical value indicating whether or not the column with the time occasions is converted in a factor. (By default is set to TRUE)}
  \item{responsesFormula}{A detailed description is given in \code{\link{lmest},\link{lmestCont}}}
  \item{latentFormula}{A detailed description is given in \code{\link{lmest},\link{lmestCont}}}
  \item{na.rm}{a logical value indicating whether or not the observation with at least a missing value is removed (By default is set to FALSE)}
  \item{check.names}{a logical value indicating whether or not the names of the variables are syntactically valid, and adjusted if necessary. (By default is set to FALSE)}
}

\value{
 An object of class \code{'lmestData'} with the following objects:

\item{data}{a data.frame object to use in the estimation functions}
\item{id}{a integer vector with the unit identifier }
\item{time}{a integer vector with the time occasions}
\item{n}{the number of observation}
\item{TT}{an integer value indicating number of time occasions}
\item{d}{an interger value indicating the number of variables (columns except id and time)}
\item{Y}{the response variables}
\item{Xmanifest}{the variables affecting the measurement model if specified in \code{responsesFormula}}
\item{Xinitial}{the variables affecting the initial probabilities of the latent model if specified in \code{latentFormula}}
\item{Xtrans}{the variables affecting the transition probabilities of the latent model if specified in \code{latentFormula}}
}

\author{Francesco Bartolucci,
  Silvia Pandolfi,
  Fulvia Pennoni,
  Alessio Farcomeni,
  Alessio Serafini}

\examples{

data(data_long_cont)
str(data_long_cont)

## Data with continous resposes

dt <- lmestData(data = data_long_cont, id = "id",time="time")
str(dt)

## Summary of each variable and for each time

summary(dt)

## Summary of each variable

summary(dt, type = "cross")

## Summary of each variable by time

summary(dt, type = "year")

plot(dt)
plot(dt, typePlot = "sh")

#######################

\dontrun{

data("data_criminal_sim")

dt1 <- lmestData(data = data_criminal_sim, id = "id", time = "time")
str(dt1)

summary(dt1, varType = rep("d",ncol(dt1$Y)))

dt2 <- lmestData(data = data_criminal_sim, id = "id", time = "time",
                 responsesFormula = y1 + y2 ~ y3, latentFormula = ~ y7 + y8 | y9 + y10)
str(dt2)

## Summary for responses, covariates on the manifest distribution,
## covariates on intial and transition probabilities

summary(dt2, dataSummary = "responses",varType = rep("d",ncol(dt2$Y)))
summary(dt2, dataSummary = "manifest",varType = rep("d",ncol(dt2$Xmanifest)))
summary(dt2, dataSummary = "initial",varType = rep("d",ncol(dt2$Xinitial)))
summary(dt2, dataSummary = "transition",varType = rep("d",ncol(dt2$Xtrans)))

}

}


