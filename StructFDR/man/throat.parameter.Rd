\name{throat.parameter}
\alias{throat.parameter}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Dirichlet parameters used for generating the artificial data sets.
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
These parameters were derived based on a microbiome data set for studying the effect of
smoking on  the upper respiratory tract microbiome. The left throat microbiome 
was used to estimate the relative proportions of 778 most abundant OTUs. The dispersion parameter was selected
to achieve medium level of zeroinflation (around 50\% zeros).
}
\usage{data(throat.parameter)}
\format{
  The format is:
 chr "throat.parameter"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
A list containing the proportion vector (\code{throat.p.est}) and the dispersion (\code{throat.theta}).
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
Charlson ES, Chen J, Custers-Allen R, Bittinger K, Li H, et al. (2010) 
Disordered Microbial Communities in the Upper Respiratory Tract of 
Cigarette Smokers. PLoS ONE 5(12): e15216. 
}
\examples{
data(throat.parameter)
}
\keyword{datasets}
