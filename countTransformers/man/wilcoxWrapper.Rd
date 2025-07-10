\name{wilcoxWrapper}
\alias{wilcoxWrapper}
\title{
Wrapper Function for Wilcoxon Rank Sum Test
}
\description{
Wrapper function for wilcoxon rank sum test.
}
\usage{
wilcoxWrapper(mat, grp)
}
\arguments{
  \item{mat}{
G x n data matrix, where G is the number of genes and n is the number of subjects
}
  \item{grp}{
	  n x 1 vector of subject group info
}
}
\details{
	For each row of \code{mat}, we perform Wilcoxon rank sum test.
}
\value{
	A G x 1 vector of p-values.
}
\references{
  Zhang Z, Yu D, Seo M, Hersh CP, Weiss ST, Qiu W. 
	Novel Data Transformations for RNA-seq Differential Expression Analysis. 
	(2019) 9:4820 https://rdcu.be/brDe5
}
\author{
	Zeyu Zhang, Danyang Yu, Minseok Seo, Craig P. Hersh, Scott T. Weiss, Weiliang Qiu
}
%\note{
%%  ~~further notes~~
%}

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ method }% use one of  RShowDoc("KEYWORDS")

