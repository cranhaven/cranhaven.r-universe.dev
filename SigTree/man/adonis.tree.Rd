\name{adonis.tree}
\alias{adonis.tree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to perform adonis test of independence on p-values from tests of multiple OTUs. 
}
\description{
\code{adonis.tree} takes \code{tree} and \code{unsorted.pvalues} and computes a p-value corresponding to a test for significant differences among the p-values in \code{unsorted.pvalues} based on the between-OTU distances in the phylogenetic tree \code{tree}.
}
\usage{
adonis.tree(tree, unsorted.pvalues, seed=1234, perms=10000, z=TRUE, make2sided=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{a phylogenetic tree of class \code{phylo}.
}
  \item{unsorted.pvalues}{a data frame (or matrix) with tip labels in column 1 and p-values in column 2.  The tip labels must correspond to the tip labels in \code{tree}.
}
  \item{seed}{positive integer seed value, to force reproducibility of permutations.
}
  \item{perms}{number of permutations to employ for adonis test
}
  \item{z}{logical argument (TRUE or FALSE) indicating whether or not to convert p-values to corresponding standard normal (Z) variates, on which scale the adonis test would subsequently be performed.
}
  \item{make2sided}{logical argument (TRUE or FLASE) indicating whether or not to convert p-values to two-sided; this should be TRUE whenever \code{unsorted.pvalues} are one-sided p-values.
}
}
\details{
After converting p-values to corresponding standard normal (Z) variates (when \code{make2sided=TRUE}), and obtaining the distance matrix of between-OTU distances, this function employs the \code{adonis} function of the package \code{vegan}.  This effectively results in a test of whether the OTU p-values are independent (the null hypothesis here), or whether differences among the OTU p-values are associated with between-OTU distances.

The "adonis" method was apparently originally called "anodis", for "analysis of dissimilarities".  To more easily distinguish this method from ANOSIM ("analysis of similarities", which also handles dissimilarities), it was re-named "anodis".  According to the help file for \code{adonis}, "Most anosim models could be analyzed with adonis, which seems to be a more robust alternative" because it is less sensitive to dispersion effects (Warton et al., 2012).


To access the tutorial document for this package (including this function), type in R: \code{vignette("SigTree")}


}
\value{
This function returns a single numeric value, corresponding to a p-value of null: "p-values for OTUs are independent" vs. alternative: "OTU p-value differences are associated with pairwise OTU distances".
}
\references{

Stevens J.R., Jones T.R., Lefevre M., Ganesan B., and Weimer B.C. (2017) "SigTree: A Microbial Community Analysis Tool to Identify and Visualize Significantly Responsive Branches in a Phylogenetic Tree."  Computational and Structural Biotechnology Journal 15:372-378.

Jones T.R. (2012) "SigTree: An Automated Meta-Analytic Approach to Find Significant Branches in a Phylogenetic Tree" (2012). MS Thesis, Utah State University, Department of Mathematics and Statistics.  http://digitalcommons.usu.edu/etd/1314

Anderson, M.J. (2001) "A new method for non-parametric multivariate analysis of variance." Austral Ecology, 26: 32-46.

Reiss P.T., Stevens M.H.H., Shehzad Z., Petkova E., and Milham M.P. (2010) "On Distance-Based Permutation Tests for Between-Group Comparisons." Biometrics 66:636-643.

Warton, D.I., Wright, T.W., Wang, Y. (2012)  "Distance-based multivariate analyses confound location and dispersion effects." Methods in Ecology and Evolution, 3, 89-101. 


}
\author{
John R. Stevens
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
### To access the tutorial document for this package, type in R (not run here): 
# vignette('SigTree')

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
# indicates default test="Stouffer" would be appropriate in other 
# main SigTree package functions (plotSigTree, export.figtree, 
# and export.inherit); otherwise, test="Hartung" would be more 
# appropriate.
adonis.tree(r.tree,r.pvalues)

}

\keyword{ tree }
\keyword{ adonis }
