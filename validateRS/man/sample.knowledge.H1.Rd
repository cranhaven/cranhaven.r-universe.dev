\name{sample.knowledge.H1}
\alias{sample.knowledge.H1}


\title{
Draw a random sample from assumed distribution of H1 values. This function can be used to compute alternative hypothesis H1C from the ECB Working paper. 
}
\description{
A sample of a certain size is drawn from a chosen distribution.  The parameters for the distribution are obtained through a call to the function \emph{par.dist.default}. The parameters may be changed, although the structure should not be changed. 

For alternative hypothesis H1C one should use the truncated normal distribution for the parameter ''par''. 
}
\usage{
sample.knowledge.H1(n, par, p.0=NULL)
}

\arguments{
  \item{n}{The size of the sample to be drawn
}
  \item{par}{The distribution and its parameters. 
}
  \item{p.0}{A vector with the PD values under H0.  The default value is NULLL. If a vector is supplied, then, for each draw, only the p.1 rows where at least one pd is higher than the corresponding p.0 pd are kept.  
}

}
\details{
A sample of size n is drawn from the distribution. 
}
\value{
A list with elements:
\item{dist }{the name of the distribution that was used}
\item{par }{the parameters of the distribution that was used}
\item{p.1 }{the sample drawn}
\item{stats.wrong.pds}{The p.1 are randomly drawn from the distribution given in the parameter par. By the random drawing it might be that the outcome does not fulfill H1. In that case the draw is excluded from the list-element p.1 and added to the list element stats.wrong.pds}
}

\seealso{
\code{\link{par.dist.default}}, 
\code{\link{simul.scenario.rs}}
}

\examples{
\dontrun{
s.nrm<-sample.knowledge.H1(n=100, par=par.dist.default(dist="tr.normal", 
                         p.0=c(0.001, 0.004)))
}
}

