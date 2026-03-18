\name{rbs}
\alias{rbs}
\title{Simulating from Birnbaum-Saunders (BS) distribution.}
\description{Simulating from BS distribution whose density function is given by
\deqn{
f_{{BS}}(t|\alpha,\beta)=\frac{0.5t +0.5 \beta}{\sqrt{2\pi}\alpha \sqrt{\beta}t^{\frac{3}{2}}} \exp\left\{-\frac{(t-\beta)^2}{2\alpha^2\beta t}\right\},
}
where \eqn{t}>0. The parameters of GBS distribution are \eqn{\alpha}>0 and \eqn{\beta}>0.
}
\usage{rbs(n, alpha, beta)}
\arguments{
\item{n}{Size of required realizations.}
\item{alpha}{Parameter \eqn{alpha}.}
\item{beta}{Parameter \eqn{beta}.}
%\item{nu}{Parameter \eqn{nu}.}
}
\value{A vector of \eqn{n} realizations from distribution.}

\author{Mahdi Teimouri}
\examples{
rbs(n = 100, alpha = 1, beta = 2)
}
