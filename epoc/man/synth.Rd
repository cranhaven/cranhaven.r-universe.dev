\name{synth}
\alias{synth}
\title{Blinded cancer mRNA, CNA and survival data}
\encoding{utf-8}
\description{
  This dataset contains blinded mRNA, CNA and survival data of 186 cancer tumors modified for demonstration usage.
  Some genes are randomly selected from 10672 probes, others are chosen for their characteristics.

  mRNA is standardized to sd=1 and mean=0.
  CNA is centered to mean=0.
  survival is in days.
}
\usage{data(synth)}
\format{
  The \code{synth} data set is a list containing mRNA \code{y}, CNA \code{u} and \code{surv} survival data.
}
\examples{
\dontrun{  
  data(synth)
  y <- synth$y
  # standardize u
  u <- apply(synth$u, 2, function(x) x/sd(x))
  G <- epocG(Y=y, U=u)
  summary(G)
  plot(G)
}
}
\keyword{datasets}
\keyword{cancer}
\keyword{epoc}

