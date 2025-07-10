\name{lvTransformer}
\alias{lvTransformer}
\title{
	Log and VOOM Transformation
}
\description{
	Log and VOOM Transformation.
}
\usage{
lvTransformer(mat, lib.size=NULL, low=0.001, upp=1000)
}
\arguments{
	  \item{mat}{
	  G x n data matrix, where G is the number of genes and n is the number of subjects
}
  \item{lib.size}{
	  By default, \code{lib.size} is a vector of column sums of \code{mat}
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
\deqn{y_{gi}=\log_2\left(t_{gi} + \frac{1}{\delta}\right)}, where
\deqn{t_{gi}=\frac{\left(x_{gi}+0.5\right)}{X_i+1}\times 10^6} and
\eqn{X_i=\sum_{g=1}^{G} x_{gi}} is the column sum for the \eqn{i}-th column
of the matrix \code{mat}. The optimal value for the parameter \eqn{\delta} is to minimize the squared 
difference between the sample mean and the sample median of the pooled data \eqn{y_{gi}}, \eqn{g=1, \ldots, G}, \eqn{i=1, \ldots, n},
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

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

res = lvTransformer(mat = ex)

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
