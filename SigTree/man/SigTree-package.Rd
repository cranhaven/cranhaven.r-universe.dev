\name{SigTree-package}
\alias{SigTree-package}
\alias{SigTree}
\docType{package}
\title{
Determine significantly responsive branches in phylogenetic trees
}
\description{
\code{SigTree} is a package of functions to determine significant response of branches of phylogenetic trees and produce colored plots both in R and (via exported .tre file) FigTree.
\code{\link{plotSigTree}} takes a phylogenetic tree (of class phylo) and a data frame (or matrix) of corresponding tip (OTU) labels and p-values and determines the significance of the branches (as families of p-values) and plots the tree with colored branches (corresponding to families) according to the level of significance of the branch.  \code{\link{export.inherit}} produces a CSV file (or data frame) with the p-values for all branches as well as which tips belong to which branches. \code{\link{export.figtree}} exports a .tre file that can be opened in FigTree that produces a colored plot (with colors according to the significance of corresponding branches) with p-value annotations.
}
\details{
\tabular{ll}{
Package: \tab SigTree\cr
Type: \tab Package\cr
Version: \tab 1.10.6\cr
Date: \tab 2017-09-29\cr
License: \tab GPL-3\cr
}
For more information, see the documentation for 
\code{\link{plotSigTree}}, \code{\link{export.inherit}}, and \code{\link{export.figtree}}.

To access the tutorial document for this package, type in R: \code{vignette("SigTree")}


}
\author{
John R. Stevens and Todd R. Jones

Maintainer:
John R. Stevens <john.r.stevens@usu.edu>
}
\references{

Stevens J.R., Jones T.R., Lefevre M., Ganesan B., and Weimer B.C. (2017) "SigTree: A Microbial Community Analysis Tool to Identify and Visualize Significantly Responsive Branches in a Phylogenetic Tree."  Computational and Structural Biotechnology Journal 15:372-378.

Jones T.R. (2012) "SigTree: An Automated Meta-Analytic Approach to Find Significant Branches in a Phylogenetic Tree" (2012). MS Thesis, Utah State University, Department of Mathematics and Statistics.  http://digitalcommons.usu.edu/etd/1314

\code{FigTree} is available at \code{http://tree.bio.ed.ac.uk/software/figtree/}. 
}
\keyword{ tree }
\keyword{ dplot }
