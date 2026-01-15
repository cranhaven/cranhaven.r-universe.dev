\docType{package}
\name{BcDiag-package}
\alias{BcDiag-package}
\title{The BCDiag package}
\description{Bicluster Diagnostics plots}
\section{Introduction}{
  The Bicluster Diagnostics plots(BcDiag) package is a visualization technique, 
  for profiling and summarizing Bicluster data, particularly for gene expression level data.
  Target data matrix are bicluster genes(rows) and conditions(columns) versus clustered genes or conditions.
  }
\section{Main task}{
  A BicDiag is a package of visualaization bicluster data, which is a 
  subset matrix that have similar characterstics in terms of row(genes) 
  and columns(conditions).
  
  It has used three different types of bicluster algorithms to extract the biculsterd data; 'biclust','isa2' and 'fabia'. plots such as boxplot,histogram, line plot,3D plot  are some of the plots that have used to visualize the data. 
  
  Major taskes of the package can be categorized in to three sections;
\enumerate{
\item profiling and summarizing the biclustered vs. the clustered simultaneously
\item profiling and summarizing only the biclusterd data.
\item exploring the biclusterd data using anova and median polish techniques.
}
}
\author{Mengsteab Aregay \email{mycs.zab@gmail.com}}
\references{
  
  Hochreiter, S., Bodenhofer, U., Heusel, M.\emph{et al.} (2010).FABIA: factor analysis for bicluster acquisition. \emph{Bioinformatices}, 26, 1520-1527.
  
  Kaiser S. and Leisch F. (2008). A Toolbox for Bicluster Analysis in R. \emph{Ludwigstrasse.} 33.
 
 Csardi G., Kutalik Z., and Bergmann S.(2010). Modular analysis of gene expression data with R. \emph{Bioinformatics,} 26, 1376-7
}
\seealso{
  The Bicluster algorithms in the packages \pkg{biclust},\pkg{fabia} and  \pkg{isa2}.
}