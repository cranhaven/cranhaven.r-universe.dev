\name{export.inherit}
\alias{export.inherit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to produce a CSV file with the p-value for each branch as well as a list of all of the tips in each branch's family
}
\description{
\code{export.inherit} takes \code{tree} and \code{unsorted.pvalues} and produces a \code{CSV} file (or data frame) with p-values for each branch (including 
tips) as well as a list of all of the tips that belong to each branch's family (i.e., all of the tips that are descendants of the branch).  The
p-values are computed based on arguments involving p-value adjustment (for multiple hypothesis testing) and either Stouffer's or Fisher's p-value combination method.
}
\usage{
export.inherit(tree, unsorted.pvalues, adjust = TRUE, side = 1, method = "hommel",
file = "", test = "Stouffer", frame = FALSE, branch="edge")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{
a phylogenetic tree of class \code{phylo}.
}
  \item{unsorted.pvalues}{
a data frame (or matrix) with tip labels in column 1 and p-values in column 2.  The tip labels must correspond to the tip labels in \code{tree}.
}
  \item{adjust}{
a logical argument that controls whether there is p-value adjustment performed (\code{TRUE}) or not (\code{FALSE}).
}
  \item{side}{
a numerical argument that takes values \code{1} and \code{2}, depending on whether the p-values in \code{unsorted.pvalues} are 1-sided or 2-sided, respectively.  Only used in p-value adjustment if \code{adjust = TRUE}.
}
  \item{method}{
one of the p-value adjustment methods (used for multiple-hypothesis testing) found in \code{p.adjust.methods} (\code{"holm"}, \code{"hochberg"}, \code{"hommel"}, \code{"BH"}, \code{"bonferroni"}, \code{"BY"}, \code{"fdr"}, and \code{"none"}).  See help for \code{p.adjust} for more details on these methods.  \code{method} is only used if \code{adjust = TRUE}.
}
  \item{file}{
the file path for the \code{CSV} file to be written to.  If \code{frame=TRUE}, no CSV file is created.  If \code{frame=FALSE}, a CSV file will only be created if \code{file} is specified. 
}
  \item{test}{
a character string taking on \code{"Hartung"}, \code{"Stouffer"}, or \code{"Fisher"}.  This is the p-value combination method that will be used.  In most cases, "Stouffer" will be most appropriate, unless \code{adonis.tree} indicates significant evidence of dependence among p-values, in which case "Hartung" is preferred.
}
  \item{frame}{
a logical argument that controls whether or not to return (in R) the resulting \code{data.frame} object. If FALSE, \code{file} must be specified.  
}

  \item{branch}{
  a character controlling branch definition: \code{"edge"} and \code{"node"} are the only options.  This does not affect statistical methods, only the colors used in edge coloring. Prior to package version 1.2, only \code{branch="node"} was implemented.  The edge and node numbering is internal to class \code{phylo}, and are not necessarily sequential numbers. 
}

}
\details{
The tip labels of \code{tree} (accessed via \code{tree$tip.label}) must have the same names (and the same length) as the tip labels in \code{unsorted.pvalues}, but may be in a different order.  The p-values in column 2 of \code{unsorted.pvalues} obviously must be in the [0, 1] range.  \code{export.inherit} assumes that each internal node has exactly two descendants.  It also assumes that each internal node has a lower number than each of its ancestors (excluding tips).

To access the tutorial document for this package (including this function), type in R: \code{vignette("SigTree")}

}
\value{
This function produces a \code{CSV} file; alternatively, if \code{frame=TRUE}, this function will return a \code{data.frame} object.
}
\references{
Stevens J.R., Jones T.R., Lefevre M., Ganesan B., and Weimer B.C. (2017) "SigTree: A Microbial Community Analysis Tool to Identify and Visualize Significantly Responsive Branches in a Phylogenetic Tree."  Computational and Structural Biotechnology Journal 15:372-378.

Jones T.R. (2012) "SigTree: An Automated Meta-Analytic Approach to Find Significant Branches in a Phylogenetic Tree" (2012). MS Thesis, Utah State University, Department of Mathematics and Statistics.  http://digitalcommons.usu.edu/etd/1314

}
\author{
John R. Stevens and Todd R. Jones
}


\examples{
### To access the tutorial document for this package, type in R (not run here): 
# vignette("SigTree")

### Create tree, then data frame, then use plotSigTree to plot the tree
### Code for random tree and data frame
node.size <- 10
seed <- 109
# Create tree
set.seed(seed);
library(ape)
r.tree <- rtree(node.size)
# Create p-values data frame
set.seed(seed)
r.pval <- rbeta(node.size, .1, .1)
# Randomize the order of the tip labels
# (just to emphasize that labels need not be sorted)
set.seed(seed)
r.tip.label <- sample(r.tree$tip.label, size=length(r.tree$tip.label))
r.pvalues <- data.frame(label=r.tip.label, pval=r.pval)

# Check for dependence among p-values; lack of significance here
# indicates default test="Stouffer" is appropriate; 
# otherwise, test="Hartung" would be more appropriate.
adonis.tree(r.tree,r.pvalues)

# Create CSV file called "ExportInherit1.csv"
export.inherit(r.tree, r.pvalues, test="Stouffers", file="ExportInherit1.csv")

# Look at resulting file in R -- see package vignette
f <- export.inherit(r.tree, r.pvalues, test="Stouffers", frame=TRUE)
f
}

\keyword{ tree }

