\name{l2Transformer}
\alias{l2Transformer}
\title{
Log Based Count Transformation Minimizing Sum of Sample-Specific Squared Difference
}
\description{
Log based count transformation minimizing sum of sample-specific squared difference.
}
\usage{
l2Transformer(mat, low = 1e-04, upp = 1000)
}
\arguments{
\item{mat}{
	  G x n data matrix, where G is the number of genes and n is the number of subjects
}
  \item{low}{
	  lower bound for the model parameter
}
  \item{upp}{
	  upper bound for the model parameter
}
}
\details{
Denote \eqn{x_{gi}} as the expression level of the \eqn{g}-th gene for
the \eqn{i}-th subject.
We perform the log transformation
\deqn{y_{gi}=\log_2\left(x_{gi} + \frac{1}{\delta}\right)}. The optimal value for the parameter \eqn{\delta} is to minimize the sum of the squared 
difference between the sample mean and the sample median across \eqn{n} subjects \deqn{\sum_{i=1}^{n}\left(\bar{y}_i - \tilde{y}_i\right)^2},
\eqn{\bar{y}_i=\sum_{g=1}^{G}y_{gi}/G} and 
\eqn{\tilde{y}_i} is the median of \eqn{y_{1i}, \ldots, y_{Gi}}, and
where \eqn{G} is the number of genes and \eqn{n} is the number of subjects.
}
\value{
A list with 3 elements:	   
\item{res.delta }{An object returned by \code{optimize} function}
\item{delta }{model parameter}
\item{mat2 }{transformed data matrix having the same dimension as \code{mat}}
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
library(Biobase)

data(es)
print(es)

# expression set
ex = exprs(es)
print(dim(ex))
print(ex[1:3,1:2])

# mean-median before transformation
vec = c(ex)
m = mean(vec)
md = median(vec)
diff = m - md
cat("m=", m, ", md=", md, ", diff=", diff, "\n")

res = l2Transformer(mat = ex)

# estimated model parameter
print(res$delta)

# mean-median after transformation
vec2 = c(res$mat2)
m2 = mean(vec2)
md2 = median(vec2)
diff2 = m2 - md2
cat("m2=", m2, ", md2=", md2, ", diff2=", diff2, "\n")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ method }% use one of  RShowDoc("KEYWORDS")
