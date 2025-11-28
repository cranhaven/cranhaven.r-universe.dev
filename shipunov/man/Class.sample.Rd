\name{Class.sample}
\alias{Class.sample}
\title{Samples along the class labels}
\description{Stratified sampling: sample separately within each class}

\usage{Class.sample(lbls, nsam=NULL, prop=NULL, uniform=FALSE)}

\arguments{
\item{lbls}{Vector of labels convertable into factor}
\item{nsam}{Number of samples to take from each class}
\item{prop}{Proportion of samples to take from each class}
\item{uniform}{Uniform instead of random?}
}

\details{
'Class.sample()' splits labels into groups in accordance with classes,
and samples each of them separately. If 'prop' is specified, then number
of samples in each class calculated separately from this value. Of both
'nsam' and 'prop' specified, preference is given to 'prop'.

Uniform method samples each n-th member of the class to reach the desired
sample size.

If sample size is bigger then class size, the whole class will be sampled.

Class.sample() uses the ave() internally, and can be easily extended, for
example, to make k-fold sampling, like:

ave(seq_along(lbls), lbls,
 FUN=function(.x) cut(sample(length(.x)), breaks=k, labels=FALSE))
}

\value{Logical vector of length equal to 'vector'}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{

(sam <- Class.sample(iris$Species, nsam=5))
iris.trn <- iris[sam, ]
iris.tst <- iris[!sam, ]

(sample1 <- Class.sample(iris$Species, nsam=10))
table(iris$Species, sample1)
(sample2 <- Class.sample(iris$Species, prop=0.2))
table(iris$Species, sample2)
(sample3 <- Class.sample(iris$Species, nsam=10, uniform=TRUE))
table(iris$Species, sample3)
(sample4 <- Class.sample(iris$Species, prop=0.2, uniform=TRUE))
table(iris$Species, sample4)
}

\keyword{manip}
