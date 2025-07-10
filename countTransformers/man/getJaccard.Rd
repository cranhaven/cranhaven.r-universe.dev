\name{getJaccard}
\alias{getJaccard}
\title{
	Calculate Jaccard Index for Two Binary Vectors
}
\description{
	Calculate Jaccard index for two binary vectors.
}
\usage{
getJaccard(cl1, cl2)
}
\arguments{
  \item{cl1}{
	  n by 1 binary vector of classification 1 for the n subjects
}
  \item{cl2}{
	  n by 1 binary vector of classification 2 for the n subjects
}
}
\details{
	Jaccard Index is defined as the ratio \deqn{d/(b+c+d}, where
	\eqn{d} is the number of subjects who were classified to group 1 by both classification rules,
	\eqn{b} is the number of subjects who were classified to group 1 by classification rule 1 and were classified to group 0 by classification rule 2,
	\eqn{c} is the number of subjects who were classified to group 0 by classification rule 1 and were classified to group 1 by classification rule 2.
}
\value{
	The Jaccard Index
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
\examples{
  n = 10
  set.seed(1234567)

  # generate two random binary vector of size n
  cl1 = sample(c(1,0), size = n, prob = c(0.5, 0.5), replace = TRUE)
  cl2 = sample(c(1,0), size = n, prob = c(0.5, 0.5), replace = TRUE)
  cat("\n2x2 contingency table >>\n")
  print(table(cl1, cl2))

  JI = getJaccard(cl1, cl2)
  cat("Jaccard index = ", JI, "\n")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ method }% use one of  RShowDoc("KEYWORDS")

