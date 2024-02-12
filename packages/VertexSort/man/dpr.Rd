\name{dpr, sdpr, dnpr, idpr, odpr}

\alias{dpr}
\alias{sdpr}
\alias{dnpr}
\alias{idpr}
\alias{odpr}

\title{Functions for Network Randomization using different algorithms}

\description{
  These functions can be run in the parallel mode to reduce the time of
  excution.  They randomize a network according to the following five
  algorithms:\cr
  \enumerate{
    \item degree preserving randomization (dpr).\cr
    \item similar degree preserving randomization (sdpr).\cr
    \item degree non-preserving randomization (dnpr).\cr
    \item in-degree preserving randomization (idpr).\cr
    \item out-degree preserving randomization (odpr).
  }
}

\usage{
  dpr(vgraph, viteration_no, vparallel = FALSE, vcpus = 1)
  sdpr(vgraph, viteration_no, vparallel = FALSE, vcpus = 1)
  dnpr(vgraph, viteration_no, vparallel = FALSE, vcpus = 1)
  idpr(vgraph, viteration_no, vparallel = FALSE, vcpus = 1)
  odpr(vgraph, viteration_no, vparallel = FALSE, vcpus = 1)
}

\arguments{
  \item{vgraph}{the graph to be randomized. The graph should be of class igraph.}
  \item{viteration_no}{an integer scalar indicating the desired number of random
  graphs to be generated.}
  \item{vparallel}{a logical scalar indicating whether to use the parallel
    programming feature to reduce the process time. The default value is
    FALSE.}
  \item{vcpus}{an integer scalar determining the number of CPUs to be
  used when parallel is TRUE. The default value is 1.}
}

\details{
  The \code{dpr()} function randomizes the input graph by randomly
  selecting two of its edges and exchanging their ends.  Multiple edges
  having the same direction between two nodes are then removed by
  switching each of them with randomly selected edges.  These steps are
  repeated 10 times the number of edges of the input graph.  For more
  information see references (Abd-Rabbo et al. 2017).

  The \code{sdpr()} function randomizes the input graph using the
  matching algorithm, see references (Milo et al. 2004) and examples.

  The \code{dnpr()} function randomly selects two nodes with replacement
  to create edges.

  The \code{idpr()} function randomizes the \dQuote{actor_id} column of
  the edges data frame by randomly selecting nodes with replacement from
  this column.  The edges data frame contains the edges making the network
  and contains two columns: \dQuote{actor_id} and \dQuote{target_id}.

  The \code{odpr()} function randomizes the \dQuote{target_id} column of
  the edges data frame by randomly selecting nodes with replacement from
  this column.  The edges data frame contains the edges making the network
  and contains two columns: \dQuote{actor_id} and \dQuote{target_id}.

  A log file having the name of each of these functions
  (e.g. dpr_log.txt, idpr_log.txt) will be created in the working
  directory and could be opened in order to follow the progression of
  long randomizations.  This log file will be deleted at the end of the
  task.
}

\value{
  \item{graph}{list of the randomized graphs of class igraph.}
}

\references{
  Abd-Rabbo, D. and Michnick, S.W. 2017 \emph{BMC Syst Biol} \bold{11}.

  Milo, R., Kashtan, N., Itzkovitz, S. 2004 \emph{cond-mat.stat-mech}
  arXiv:cond-mat/0312028v2.
}

\author{
  Diala Abd-Rabbo \email{diala.abd.rabbo@gmail.com}
}


\seealso{
  \code{\link{igraph}}, \code{\link{snowfall}}.
}

\examples{
  ## generate a random graph of the kinase-phosphatase network by using
  ## each of the five algorithms

  ## load the VertexSort library
  library(VertexSort)

  ## load interactions of the kinase-phosphatase network (kp-net)
  data("interactions")
  vs_kp_net <- vertex.sort(interactions)
  kp_net    <- vs_kp_net$graph

  ## dpr function: randomize a network with preserving its node degrees
  ## notice the difference in execution time when using and
  ## not using the parallel programning mode
  ptm <- proc.time()
  rand_g <- dpr(kp_net, 4)            # without parallel mode
  proc.time() - ptm

  ptm <- proc.time()
  rand_g <- dpr(kp_net, 4, TRUE, 4)   # with parallel mode
  proc.time() - ptm

  ## verify that rand_g have the same in- and out-degrees as those
  ## of kp_net should obtain TRUE in both commands.
  all(degree(kp_net, V(kp_net), "in")==degree(rand_g[[1]], V(rand_g[[1]]), "in"))
  all(degree(kp_net, V(kp_net), "out")==degree(rand_g[[1]], V(rand_g[[1]]), "out"))

  ## sdpr function: randomize a network with preserving similar node degrees
  rand_g <- sdpr(kp_net, 1)
  ## verify that rand_g have similar in- and out-degrees to those of
  ## kp_net. Should be -1, 0 or 1
  sort(unique(degree(kp_net, V(kp_net), "in")-degree(rand_g[[1]], V(rand_g[[1]]), "in")))
  sort(unique(degree(kp_net, V(kp_net), "out")-degree(rand_g[[1]], V(rand_g[[1]]), "out")))

  ## dnpr function: randomize a network without preserving its node degrees
  rand_g <- dnpr(kp_net, 1)
  ## verify that rand_g have different in- and out-degrees of those of
  ## kp_net. Should get FALSE in both commands
  all(degree(kp_net, V(kp_net), "in")==degree(rand_g[[1]], V(rand_g[[1]]), "in"))
  all(degree(kp_net, V(kp_net), "out")==degree(rand_g[[1]], V(rand_g[[1]]), "out"))

  ## idpr function: randomize a network with preserving its node in-degrees
  rand_g <- idpr(kp_net, 1)
  ## verify that rand_g have same in-degrees and different out-degrees as
  ## those of kp_net. Should get TRUE and FALSE respectively.
  all(degree(kp_net, V(kp_net), "in")==degree(rand_g[[1]], V(rand_g[[1]]), "in"))
  all(degree(kp_net, V(kp_net), "out")==degree(rand_g[[1]], V(rand_g[[1]]), "out"))

  ## odpr function: randomize a network with preserving its node out-degrees
  rand_g <- odpr(kp_net, 1)
  ## verify that rand_g have same out-degrees and different in-degrees as
  ## those of kp_net. Should get FALSE and TRUE respectively.
  all(degree(kp_net, V(kp_net), "in")==degree(rand_g[[1]], V(rand_g[[1]]), "in"))
  all(degree(kp_net, V(kp_net), "out")==degree(rand_g[[1]], V(rand_g[[1]]), "out"))
}

\keyword{Graphs}
\keyword{Optimization}
