\name{simul.scenario.rs}
\alias{simul.scenario.rs}

\title{
Determine cardinality and power for a statistical test. 
}

\description{
For a sample drawn with \emph{sample.knowledge.H1} and for a statistical test, the acceptance region is determined and the power for each value of H1 in the sample is computed. 
}
\usage{
simul.scenario.rs(hypo.test, p.0, sampleH1, sizes, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hypo.test}{The name of the hypothesis test: \dQuote{minP} for the multiple test , \dQuote{minPp} for the enhanced multiple test or \dQuote{sterneHull} for the envelope test. 
}
  \item{p.0}{A vector containing the PDs under H0

}
  \item{sampleH1}{The sample drawn using the function \emph{sample.knowledge.H1}. 
}
  \item{sizes}{A matrix with different scenario's for the sizes of the rating classes. There should be at least two rows and the number of columns must equal the length of the parameter p.0. 
}
  \item{alpha}{The significance level
}
}

\value{
A list with elements%%  \item{comp1 }{Description of 'comp1'}
\item{par }{The parameters passed to the function}
\item{power }{Power for each value of H1, the average power and the standard error on the average}
\item{cardinality.AR }{The cardinality of the acceptance region}
\item{executionTime }{The time it took to compute the acceptance region and to compute the power}
\item{knowledgeH1 }{The characteristics ot the distribution and the sample drawn}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sample.knowledge.H1}},
}
\examples{
p.0<-c(0.001, 0.004)
sizes<-rbind( c(500, 500), 
              c(1000, 5000))
alpha<-0.05
\dontrun{
s.nrm<-sample.knowledge.H1(n=10, 
                        par=par.dist.default(dist="tr.normal", p.0=c(0.001, 0.004)))
scen.nrm.mp<-simul.scenario.rs(hypo.test="minP", p.0=p.0, 
                        sampleH1=s.nrm, sizes=sizes, alpha=alpha)
scen.nrm.mpp<-simul.scenario.rs(hypo.test="minPp", p.0=p.0, 
                        sampleH1=s.nrm, sizes=sizes, alpha=alpha)

par.ownsample<-par.dist.default(dist="usersupplied", p.0=c(0.001, 0.004))
par.ownsample$param<-rbind(c(0.001, 0.01), c(0.004, 0.004), c(0.004, 0.01))
s.own<-sample.knowledge.H1(n=10, 
                        par=par.ownsample)

scen.ownsample.mp<-simul.scenario.rs(hypo.test="minP", p.0=p.0, 
                        sampleH1=s.own, sizes=sizes, alpha=alpha)
}
}
