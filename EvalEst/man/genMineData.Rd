\name{genMineData}
\alias{genMineData}
\alias{build.diagonal.model}
\alias{build.input.models}

\title{Generate Data}
\description{Generate data for Monte Carlo experiments}
\usage{
    genMineData(umodel, ymodel, uinput=NULL, sampleT=100, 
      unoise=NULL, usd=1,ynoise=NULL, ysd=1, rng=NULL)
    build.input.models(data, max.lag=NULL)
    build.diagonal.model(multi.models)
}
\arguments{
    \item{umodel}{Model for input data.}
    \item{ymodel}{Model for output data.}
    \item{sampleT}{Number of periods of data to generate.}
    \item{unoise}{Input noise.}
    \item{usd}{Standard deviationof input noise.}
    \item{ynoise}{Output noise.}
    \item{ysd}{Standard deviation of output noise.}
    \item{rng}{RNG setting.}
    \item{multi.models}{A list of \code{TSestModel}s.}
    \item{data}{data from which to build models.}
    \item{max.lag}{number of lags in the estimated models.}
    \item{uinput}{Input data to \code{umodel}.}
}
\value{A TSdata object.}
\details{
    This function generates test data using specified models.
    \code{umodel} is used to generate data corresponding to input data and
    \code{ymodel} is used to generate data corresponding to output data.
    The result of \code{umodel} is used as input to \code{ymodel} so the 
    input dimension of \code{ymodel} should be the output dimension of 
    \code{umodel}. Typically the \code{ymodel} would be degenerate 
    in some of the input variables so the effective inputs are a subset.
    If \code{umodel} requires input data it should be specified in \code{uinput} 
    If \code{noise} is \code{NULL} then an normal noise will be generated 
    by \code{simulate}.
    This will be iid N(0,I).
    The RNG will be set first to \code{rng} if it is specified. 
    If \code{unoise} or \code{ynoise} are specified they should be as 
    expected by \code{simulate}
    for the specified \code{umodel} and \code{ymodel}.
    
    \code{genMineData} uses \code{build.input.models}, which makes a list of 
    univariate \code{TSestModel}s, one for each series in \code{inputData(data)}
    estimated by \code{estVARXls} with \code{max.lag} lags. 
    \code{genMineData} then uses 
    \code{build.diagonal.model} which builds one diagonal model from a list 
    of models returned  by \code{build.input.models}. It uses the AR part only.
}

\seealso{
    \code{\link[dse]{simulate}}
}

\examples{
    data("eg1.DSE.data.diff", package="dse")
    umodel <- build.diagonal.model(
            build.input.models(eg1.DSE.data.diff, max.lag=2))
    z  <- TSdata(output=outputData(eg1.DSE.data.diff), 
                 input = inputData(eg1.DSE.data.diff))
    ymodel <- TSmodel(estVARXls(z, max.lag=3))   
    sim.data <- genMineData(umodel, ymodel)
}
\concept{DSE}
\keyword{ts}
