\name{interactions}

\alias{interactions}

\docType{data}

\title{
  Dataset containging interactions of the Kinase-Phosphatase Network
  (Abd-Rabbo et al. 2017)
}

\description{
  This dataset contains a data frame called interactions containing the
  binary interactions assembled in the kinase-phosphatase network.
  Interactions were obtained from the Kinase Interaction
  Database (Sharifpoor et al.) and from other data curation efforts
  (Abd-Rabbo et al. 2017).
}

\usage{data("interactions")}
\format{
  This data frame contains 1087 rows and the following columns:
  \describe{
    \item{\code{kp_orf}}{standard name (ORF) of the kinase/phosphatase
      that phosphorylates/dephosphorylates the substrate}
    \item{\code{substrate_orf}}{standard name (ORF) of the protein
      substrate phosphorylated/dephosphorylated by the kinase/phosphatase}
  }
}

\details{
  These interactions represent binary interactions between kinases/phosphatases
  and their substrates.  They were used in (Abd-Rabbo et al. 2017) to form the
  kinase-phosphatase network used with the Vertex Sort algorithm to
  study the hierarchical structure of the signaling regulatory network
  in the budding yeast.
}

\source{
  Abd-Rabbo, D. and Michnick, S.W. 2017 \emph{BMC Syst Biol} \bold{11}.
}

\references{
  Sharifpoor, S., Nguyen, Ba. A.N., Young, J.Y. et al. 2011 \emph{Genome
  Biol} \bold{12}, R39.
}

\examples{
  ## load the VertexSort library
  library(VertexSort)

  ## load interactions of the kinase-phosphatase network (kp-net)
  data(interactions)

  ## view the dimentions of the interactions data frame
  dim(interactions)

  ## view the top of the interactions data frame
  head(interactions)

  ## The interactions data frame could be loaded into an R session
  ## and used as an example to apply on it the vertex sort algorithm
  ## or generate radom networks. See ?vertex.sort, ?dpr, ?sdpr, ?dnpr,
  ## ?idpr and ?odpr
}

\keyword{datasets}
