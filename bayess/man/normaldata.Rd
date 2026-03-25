\name{normaldata}
\alias{normaldata}
\docType{data}
\title{
Normal dataset%%   ~~ data name/kind ... ~~
}
\description{
This dataset is used as "the" normal dataset in Chapter 2. 
It is linked with the famous Michelson-Morley experiment that
opened the way to Einstein's relativity theory in 1887. It corresponds
to the more precise experiment of Illingworth in 1927. The datapoints
are measurment of differences in the speeds of
two light beams travelling the same distance in two orthogonal directions.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(normaldata)}
\format{
  A data frame with 64 observations on the following 2 variables.
  \describe{
    \item{\code{x1}}{index of the experiment}
    \item{\code{x2}}{averaged fringe displacement in the experiment}
  }
}
\details{
The 64 data points in this dataset are associated with session numbers, corresponding to two different times of
the day, and they represent the averaged fringe displacement due to orientation taken over ten measurements 
made by Illingworth, who assumed a normal error model. 
}
\seealso{
\cite{\link{morley}}%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(normaldata)
shift=matrix(normaldata,ncol=2,byrow=TRUE)[,2]
hist(shift[[1]],nclass=10,col="steelblue",prob=TRUE,main="")
}
\keyword{datasets}
\keyword{Michelson-Morley experiment}
