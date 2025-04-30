\name{MonoInc}
\alias{MonoInc}


\title{
Monotonic Increasing
}
\description{
Combines many of the functions in the MonoInc package. Given a data range, weights, and imputation methods of choice, MonoInc will impute flagged values using either one or a combination of two imputation methods. It can also perform all single imputation methods for comparison.
}
\usage{
MonoInc(data, id.col, x.col, y.col, data.r = NULL, tol = 0, direction = "inc", w1 = 0.5, 
  min, max, impType1 = "nn", impType2 = "reg", sum = FALSE)
}

\arguments{
  \item{data}{
a data.frame or matrix of measurement data
}
  \item{id.col}{
column where the id's are stored
}
  \item{x.col}{
column where x values, or time variable is stored
}
  \item{y.col}{
column where y values, or measurements are stored
}
  \item{data.r}{
range for y values; must have three columns: 1 - must match values in xcol, 2 - lower range values, 3 - upper range values
}
  \item{tol}{
tolerance; how much outside of the range (data.r) is acceptable; same units as data in ycol 
}
\item{direction}{
the direction of the function a choice between increasing 'inc', and decreasing 'dec'
}
\item{w1}{
weight of imputation type 1 (impType1); default is 0.50
}
\item{min}{
lowest acceptable value for measurement; does not have to be a number in ycol
}
  \item{max}{
highest acceptable value for measurement; does not have to be a number in ycol
}
  \item{impType1}{
imputation method 1, a choice between Nearest Neighbor "nn", Regression "reg", Fractional Regression "fr", Last Observation Carried Forward "locf", or Last & Next "ln";  default is "nn"
}
  \item{impType2}{
imputation method 2;  default is "reg"
}
  \item{sum}{
if true the function will return a matrix of all imputation methods in the package
}
}
\details{
If two imputation methods are chosen, MonoInc will take a weighted average of the output of the imputed values. User must chose one or two imputation methods or sum=TRUE for a comparison. If there are not enough values available to impute missing or erroneous values, MonoInc will return an NA. Advice: Do NOT overwrite original data using this function! Use parallel processing if available on your device.
}

\value{
Returns the data matrix with additional columns for the selected imputation method.  If sum=TRUE, it will return a column for each single imputation method.  The Y column will have NAs, indicating that this observation was flagged and imputed, for summary only. Duplicate rows are removed. 
}

\author{Michele Josey  \href{mailto:mjosey@nccu.edu}{mjosey@nccu.edu} 
Melyssa Minto  \href{mailto:mminto@nccu.edu}{mminto@nccu.edu}
}

\examples{
data(simulated_data)
simulated_data <- simulated_data[1:1000,]
data(data.r)
library(sitar)

## Run MonoInc
sum <- MonoInc(simulated_data, 1,2,3, data.r,5,direction='inc', w1=0.3, min=30, max=175, 
    impType1=NULL, impType2=NULL, sum=TRUE)
head(sum)
test <- MonoInc(simulated_data, 1,2,3, data.r,5,direction='inc', w1=0.3, min=30, max=175, 
    impType1="nn", impType2="fr")
head(test)

## plot longitudinal height for each id
mplot(x=X, y=Nn.Fr, data=test)
tol <- 5
lines(data.r[,1], data.r[,2]-tol, col=2, lty=2)
lines(data.r[,1], data.r[,3]+tol, col=2, lty=2)
}

\keyword{monotonic}
\keyword{mono}
