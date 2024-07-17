\name{aiData}
\alias{aiData}
\alias{print.aidata}
\docType{data}
\title{Transforming Raw Data for Static AIDS Model}
\description{This function transforms import values and quantities into a data format that are needed for a static AIDS model. }

\usage{aiData(x, label, label.tot = "WD", prefix.value = "v",
  prefix.quant = "q", start = NULL, end = NULL, stone = TRUE, 
  dummy = NULL, season = c("none", "m", "q"), ...)}
\arguments{
  \item{x }{ raw time series data such as \code{daBedRaw}.}
  \item{label }{ names of supplying countries; this can be as long as needed.}
  \item{label.tot }{ names of the world total (default label is "WD").}
  \item{prefix.value }{ prefix for value variables.}
  \item{prefix.quant }{ prefix for quantity varibles.}
  \item{start }{ start date for the transformed time series; this can be used to select a smaller window; the default is the start date of the raw data \code{x}.}
  \item{end }{ end date for the transformed time series.}
  \item{stone }{ whether the Stond Price Index is constructed (default TRUE); if FALSE, the version of log-linear analog to the Paasche index with lagged budget shares in Moschini (1995) is used. See references for detail.}
  \item{dummy }{ adding dummy variables if date ranges are provided as a list.}
  \item{season }{ adding seasonality variables or not; if yes, either monthly dummy or quarterly dummy; this is prepared mainly for monthly data.}      
  \item{\dots }{ additional arguments to be passed.}
}
\details{This transforms raw import data into a format needed for a static AIDS model. This separation of data prepraration from model fitting allows greater flexibility in using \code{aiStaFit} in estimating a static AIDS model. In addition, when the raw data contain zero, a small number is substituted to avoid NA when the price variable (value/quantity) is calculated. The values for the residual supplier will change when the \code{label} is different.}

\value{
Return a list object with two components:
  \item{out }{ a time series object ready for static AIDS models. }
  \item{share }{ a time series object of the share data.}
  \item{price }{ a time series object of the price data.}
  \item{quantity }{ a time series object of the quantity data.}
  \item{value }{ a time series object of the value data.}    
  \item{m }{ a vector of the total expenditure.}
  \item{call }{ a record of the system call; this allows \code{update.default} to be used. }
}

\section{Methods}{
  One method is defined as follows:
  \describe{
    \item{\code{print}:}{print the first several observations of the final data needed for the AIDS model.}
  }
}

\references{  
Moschini, G. 1995. Units of measurement and the Stone index in demand system estimation. American Journal of Agricultural Economics 77(1):63-68.

Singh, K., M.M. Dey, and G. Thapa. 2011. An error corrected almost ideal demand system for crustaceans in the United States. Journal of International Food & Agribusiness Marketing 23(3):271-284.

Wan, Y., C. Sun, and D.L. Grebner. 2010. Analysis of import demand for wooden beds in the United States. Journal of Agricultural and Applied Economics 42(4):643-658.
}


\author{Changyou Sun (\email{cs258@msstate.edu})}
\seealso{\code{\link{aiStaFit}}; \code{\link{daBedRaw}}; \code{\link{daBed}}.}

\examples{

data(daBedRaw)
dumm <- list(dum1 = c(2003, 10, 2003, 10), dum2 = c(2004, 7, 2004, 7),
  dum3 = c(2005, 1, 2005, 1))
imp8 <- aiData(x = daBedRaw,
  label = c("CN", "VN", "ID", "MY", "CA", "BR", "IT"),
  label.tot = "WD", prefix.value = "v", prefix.quant = "q",
  start = c(2001, 1), end = c(2008, 12), dummy = dumm)
imp4 <- update(imp8, label = c("CN", "VN", "ID"))
imp5 <- update(imp4, label = c("CN", "VN", "ID", "MY"))
imp8; imp4; imp5
daTest <- imp8$out
colnames(daTest)[18:20] <- c("dum1", "dum2", "dum3")
head(daTest)

data(daBed)
head(daTest); head(daBed)
identical(daBed, daTest)
}
\keyword{manip}