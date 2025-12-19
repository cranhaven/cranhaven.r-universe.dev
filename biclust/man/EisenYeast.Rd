\name{EisenYeast}
\alias{EisenYeast}
\alias{yeast microarray data matrix}
\alias{Saccharomices Cerevisiae data matrix}

\docType{data}

\title{Eisen Yeast}
\description{Microarray data matrix for 80 experiments with Saccharomyces Cerevisiae organism
  by Eisen Lab.}
\usage{
  data(EisenYeast)
  }


%\details{}
%\value{}
%\references{Si acaso Eisen1998}

\format{
  Data frame with information about the expression levels of 6221 genes over 80 conditions.
  Missing values have been imputed using k-nearest neighbor averaging implemented in
  impute.knn() from library 'impute' (using default k=10).
  Gene names follow ORF (Open Reading Format) notation.
  }
\source{Eisen Lab at http://rana.lbl.gov/EisenData.htm}
%\seealso{
  %parallelCoordinates for alternative representation of biclusters.
%  \item{\code{\link{bubbleplot}}}{ for simultaneous representation of biclusters.}
%  }
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datasets}
