\name{Rosenwald dataset}
\alias{rosenwald}
\alias{rosenwald.expr}
\alias{rosenwald.cli}
\docType{data}

\title{
  Rosenwald et al. Lymphochip data
}

\description{
  This dataset contains 60 Diffuse Large B-Cell Lymphomas analysed on Lymphochip microarrays, as published by Rosenwald et al. The "Germinal Center B-cell like" and "Activated B-Cell like" subtypes, as determined by hierarchical clustering, were predicted by a LPS approach in Wright et al.
  
  To minimize package size, values were rounded at 3 decimals and only 60 DLBCL from the 240 series were randomly selected (40 from the "Training" set, 20 from the "Validation" set), excluding "Type III" sub-types.
}

\usage{data(rosenwald)}

\format{
  \code{rosenwald.expr} is a numeric matrix of expression values, with probes in rows and samples in columns. Both dimensions are named, probes by there "UNIQID" and samples by there "LYM numbers". Many \code{NA} values are present.

  \code{rosenwald.cli} is a data.frame with a row for each sample, and 4 \code{factor} columns described below. Rows are named by samples "LYM numbers", in the same order than \code{rosenwald.expr}.
  \describe{
    \item{set}{the "Training" or "Validation" set the sample comes from.}
    \item{group}{the DLBCL sub-type that is to be predicted ("GCB" or "ABC").}
    \item{follow.up}{follow-up of the patient, in years.}
    \item{status}{status of the patient at the end of the follow-up ("Dead" or "Alive").}
  }  
}

\source{
  \url{http://llmpp.nih.gov/DLBCL/}
}

\references{
  Rosenwald A et al. \cite{The use of molecular profiling to predict survival after chemotherapy for diffuse large-B-cell lymphoma.} N Engl J Med. 2002 Jun 20;346(25):1937-47.
  
  Wright G et al. \cite{A gene expression-based method to diagnose clinically distinct subgroups of diffuse large B cell lymphoma.} Proc Natl Acad Sci U S A. 2003 Aug 19;100(17):9991-6.
}

\keyword{datasets}
