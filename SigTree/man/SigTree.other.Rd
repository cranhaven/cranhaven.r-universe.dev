\name{SigTree.other}
\alias{num.edges}
\alias{num.tips}
\alias{num.internal.nodes}
\alias{num.total.nodes}
\alias{srt.pvalues}
\alias{stouffers}
\alias{fishers}
\alias{index.matrix}
\alias{p.p2.ADJ.p1}
\alias{result}
\alias{tip.colors}
\alias{edge.colors}
\alias{plotphylo2}
\alias{circularplot2}
\alias{hartung}
\alias{node_depthSigTree}
\alias{node_depth_edgelengthSigTree}
\alias{node_heightSigTree}
\alias{node_height_cladoSigTree}
\alias{R_init_SigTree}

\title{
SigTree Other Functions
}
\description{
Internal functions used by the main functions of \code{SigTree} (\code{plotSigTree}, \code{export.figtree}, and \code{export.inherit}):
\tabular{ll}{
\tab \cr
\code{num.edges}  \tab determine the number of edges in \code{tree} \cr
\code{num.tips}  \tab determine the number of tips in \code{tree} \cr
\code{num.internal.nodes}  \tab determine the number of nodes in \code{tree} \cr
\code{num.total.nodes}  \tab determine the number of total nodes (internal + tips) in \code{tree} \cr
\code{srt.pvalues}  \tab sort \code{unsorted.pvalues} by tip labels (column 1) to be in same order as tip labels in \code{tree} \cr
\code{stouffers}  \tab perform Stouffer's Method on a vector of p-values; return one p-value \cr
\code{fishers}  \tab perform Fisher's Method on a vector of p-values; return one p-value \cr
\code{index.matrix}  \tab create matrix to identify the descendants/tips (rows) belonging to each node/family (column) \cr 
\code{p.p2.ADJ.p1}  \tab convert 1-sided p-values to 2-sided, perform p-value adjustment (for multiple-hypothesis \cr
                    \tab testing), and convert back to 1-sided \cr
\code{result}  \tab calculate p-values for each node/edge branch \cr
\code{tip.colors}  \tab determine coloring of each tip \cr
\code{edge.colors}  \tab determine coloring of each edge \cr
\code{plotphylo2}   \tab (based on \code{ape} package's \code{plot.phylo} function); plots \code{tree} while allowing for different \cr
                    \tab edge coloring (root edge when \code{type="fan"}, and different colors for each half of the \cr
                    \tab "perpendicular-to-the-root" edges). Prior to package version 1.2, \code{plot.phylo} was used instead. \cr
                    \tab Beginning in package version 1.3 (to attain CRAN compatibility), includes .C calls to copies of four \cr
                    \tab \code{ape} .C functions (copied with credit under \code{ape}'s GPL license).\cr
\code{circularplot2} \tab (based on \code{ape} package's \code{circular.plot} function) called by \code{phyloplot2} when \code{type="fan"} \cr
\code{hartung} \tab perform Hartung's Method on a vector of p-values; return one p-value \cr
\tab \cr
  }
}

\details{
It is assumed that each internal node has exactly two descendants.  It is also assumed that each internal node has a lower number than each of its ancestors (excluding tips).

To access the tutorial document for this package (including this function), type in R: \code{vignette("SigTree")}

}

\references{
Stevens J.R., Jones T.R., Lefevre M., Ganesan B., and Weimer B.C. (2017) "SigTree: A Microbial Community Analysis Tool to Identify and Visualize Significantly Responsive Branches in a Phylogenetic Tree."  Computational and Structural Biotechnology Journal 15:372-378.

Jones T.R. (2012) "SigTree: An Automated Meta-Analytic Approach to Find Significant Branches in a Phylogenetic Tree"  MS Thesis, Utah State University, Department of Mathematics and Statistics.  http://digitalcommons.usu.edu/etd/1314

}
\author{
John R. Stevens and Todd R. Jones
}

\note{
Extensive discussion of methods developed for this package are available in Jones (2012).  
In that reference, (and prior to package version number 1.1), the \code{srt.pvalues} function was named \code{sort.pvalues} (the name change was made to resolve S3 class issues), and \code{plotphylo2} was not available.
}


% Add one or more standard keywords, see file "KEYWORDS" in the
% R documentation directory.
\keyword{ tree }

