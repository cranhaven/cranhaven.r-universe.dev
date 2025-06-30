\name{export.figtree}
\alias{export.figtree}
\title{
Function to export a \code{NEXUS} file that can be opened in \code{FigTree} to produce a plot of the phylogenetic tree with branches colored according to significance of families of p-values
}
\description{
\code{export.figtree} takes \code{tree} and \code{unsorted.pvalues} and produces
a \code{NEXUS} file that can \code{FigTree} can subsequently open.  The p-values for each branch (family of tips) are
computed and the branches are colored accordingly.  It computes the p-values based on arguments involving p-value adjustment (for multiple hypothesis testing) and either Stouffer's or Fisher's p-value combination method. 
There are arguments that allow for the customization of the p-value cutoff ranges as well as the colors to be used
 in the coloring of the branches.  There is also an option to include annotations for each edge that contain the p-value 
for the corresponding branch.
}
\usage{
export.figtree(tree, unsorted.pvalues, adjust=TRUE, side=1, 
	method="hommel", p.cutoffs=ifelse(rep(side==1, ifelse(side==1, 6, 3)),
	c(.01, .05, .1, .9, .95, .99), c(.01, .05, .1)), file="", 
	pal=ifelse(rep(side==1, ifelse(side==1, 1, length(p.cutoffs)+1)),
	"RdBu", rev(brewer.pal(length(p.cutoffs)+1,"Reds"))),
	test = "Stouffer", edge.label=TRUE, ignore.edge.length=FALSE,
	branch="edge")
}
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
  \item{p.cutoffs}{
a vector of increasing p-value cutoffs (excluding 0 and 1) to determine the ranges of p-values used in the coloring of the branches.
}
  \item{file}{
the file path that the \code{NEXUS} file that \code{FigTree} can open is written to.
}
  \item{pal}{
one of the palettes from the RColorBrewer package (see \code{brewer.pal.info} for a list) or a vector of hexadecimal colors.  These are the colors that are used to color the branches.  The first color corresponds to the first range in p.cutoffs and so on.
}
  \item{test}{
a character string taking on \code{"Hartung"}, \code{"Stouffer"}, or \code{"Fisher"}.  This is the p-value combination method that will be used.  In most cases, "Stouffer" will be most appropriate, unless \code{adonis.tree} indicates significant evidence of dependence among p-values, in which case "Hartung" is preferred.
}
  \item{edge.label}{
a logical argument that, when \code{TRUE}, produces edge label annotations in \code{FigTree}.  These annotations are the p-values of the edge's parent node when \code{branch="node"}.  They are accessible via highlighting an edge, hitting the \code{Annotate} button, and selecting \code{P-value}.
}
  \item{ignore.edge.length}{
a logical parameter.  When \code{TRUE}, the edge lengths from \code{tree} are ignored and \code{FigTree} creates its own edge lengths.  \code{ignore.edge.length} has no effect if the edges in \code{tree} have no defined length.  
}

 \item{branch}{
  a character controlling branch definition: \code{"edge"} and \code{"node"} are the only options.  This does not affect statistical methods, only the colors used in edge coloring. Prior to package version 1.2, only \code{branch="node"} was implemented.
}

}
\details{
The tip labels of \code{tree} (accessed via \code{tree$tip.label}) must have the same names (and the same length) as the tip labels in \code{unsorted.pvalues}, but may be in a different order.  The p-values in column 2 of \code{unsorted.pvalues} obviously must be in the [0, 1] range.  \code{p.cutoffs} takes values in the (0, 1) range.  The default value for \code{p.cutoffs} is \code{c(0.01, 0.05, 0.1, 0.9, 0.95, 0.99)} if side is \code{1} and \code{c(0.01, 0.05, 0.1)} if side is \code{2}.  Thus, the ranges (when side is \code{1}) are: [0, .01], (.01, .05], ..., (.99, 1].  These ranges correspond to the colors specified in \code{pal}.  P-values in the [0, .01] range correspond to the left-most color if \code{pal} is a palette (view this via \code{display.brewer.pal(x, pal)} - where \code{x} is the number of colors to be used) or the first value in the vector if \code{pal} is a vector of colors.  If \code{pal} is a vector of colors, then the length of \code{pal} should be one greater than the length of \code{p.cutoffs}.  In other words, its length must be the same as the number of p-value ranges.  In addition, each color in this vector of colors needs to be in hexadecimal format, for example, \code{"#B2182B"}.  Formats of colors other than hexadecimal will likely give unwanted results in the edges of the tree produced in \emph{FigTree}, such as all-black edges or the edges being colored in a meaningless way.  This is because the color conversion assumes hexadecimal colors.  The default value of \code{pal} is \code{"RdBu"} (a divergent palette of reds and blues, with reds corresponding to small p-values) if \code{side} is \code{1} and the reverse of \code{"Reds"} (a sequential palette) if \code{side} is {2}.   The sequential palettes in \code{RColorBrewer} go from light to dark, so \code{"Reds"} is reversed so that the dark red corresponds to small p-values.    It probably makes more sense to use a divergent palette when using 1-sided p-values and a sequential palette (reversed) when using 2-sided p-values.  To create a vector of reversed colors from a palette with \code{x} number of colors and \code{"PaletteName"} as the name of the palette, use \code{rev(brewer.pal(x, "PaletteName"))}.  \code{ignore.edge.length} may be useful to get a more uniformly-shaped tree. \code{export.figtree} assumes that each internal node has exactly two descendants.  It also assumes that each internal node has a lower number than each of its ancestors (excluding tips). 

The \code{branch} argument controls whether edge coloring corresponds to the combined p-value of the tips below the edge (\code{"edge"}) or of the tips below the edge's leading (away from the tips) node (\code{"node"}).  Note that if \code{branch="node"} is used, then both edges leaving a node will necessarily be colored the same.

To access the tutorial document for this package (including this function), type in R: \code{vignette("SigTree")}

}
\value{
This function creates a \code{NEXUS} file that can be opened by the program \code{FigTree}.
}
\references{
Stevens J.R., Jones T.R., Lefevre M., Ganesan B., and Weimer B.C. (2017) "SigTree: A Microbial Community Analysis Tool to Identify and Visualize Significantly Responsive Branches in a Phylogenetic Tree."  Computational and Structural Biotechnology Journal 15:372-378.

Jones T.R. (2012) "SigTree: An Automated Meta-Analytic Approach to Find Significant Branches in a Phylogenetic Tree" (2012). MS Thesis, Utah State University, Department of Mathematics and Statistics.  http://digitalcommons.usu.edu/etd/1314

\code{FigTree} is available at \code{http://tree.bio.ed.ac.uk/software/figtree/}.  
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
set.seed(seed)
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

# Export "ExportFigtree1.tre" file that can be opened in FigTree
library(phyext2)
export.figtree(r.tree, r.pvalues, test="Stouffer", file="ExportFigtree1.tre")

}
\keyword{ tree }

