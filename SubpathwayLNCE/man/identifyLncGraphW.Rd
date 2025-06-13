\name{identifyLncGraphW}
\alias{identifyLncGraphW}
\title{Annotate and identify subpathways}
\description{
Annotate user-interested molecules to pathways and identify significantly enriched subpathways.
}
\usage{
  identifyLncGraphW(moleculeList,graphList,type="gene_lncRNA",
             background=getBackgroundLnc(type),
             order="pvalue",decreasing=FALSE,bet=1)
}
\arguments{
  \item{moleculeList}{A character vector. Such as user-interested lncRNAs and/or genes under disease phenotypes.}
  \item{graphList}{A graph list. There nodes must be represented by genes or lncRNAs and genes.} 
  \item{type}{A character string. Should be one of "gene", "lncRNA" or "gene_lncRNA".}   
  \item{background}{A character vector of molecules.}
  \item{order}{A character string. Should be one of "pvalue", "fdr".}
  \item{decreasing}{A logical. Should the sort be ordered by increasing or decreasing?}
  \item{bet}{A number. A parameter. }
}
\details{

The function can support the annotation and identification of metabolic subpathways based on genes, lncRNAs or gene_lncRNAs sets. The argument \code{moleculeList} supports three kinds of molecular sets: "genes", "lncRNAs" or "gene_lncRNAs". 

The argument \code{type} represent the type of input molecules, including one of "genes", "lncRNAs" or "gene_lncRNA". 

Detailed \code{background} information is provided in the function \code{\link{getBackgroundLnc}}.

When many correlated subpathways are considered, the parameter \code{order} is used to order the pathways on the basis of "pvalue" or "fdr". 

The parameter \code{decreasing} is set TRUE that represent the order would be performed by decreasing.

}

\value{
A list. It include: 'pathwayId', 'pathwayName', 'annMoleculeList', 'annMoleculeNumber', 'annBgMoleculeList', 'annBgNumber', 'MoleculeNumber', 'bgNumber','annWeight', 'pvalue', and 'fdr',  corresponding to pathway identifier, pathway name, the submitted molecules annotated to a pathway, the number of submitted molecules annotated to a pathway, the background molecules annotated to a pathway, the number of background molecules annotated to a pathway, the number of submitted molecules, the number of background molecules, the weight of lncRNA competitively regulated subpathway p-value of the hypergeometric test, and Benjamini-Hochberg fdr values.

The background molecules annotated to a pathway are equal to all molecules in the pathway. For example, if the submitted molecules are human genes, the background molecules annotated to a pathway are equal to all human genes in the pathway.

The number of background molecules is the number of all molecules. For example, if the submitted molecules are human genes, the number of background molecules is equal to all human genes.

To visualize and save the results, the \code{list} can be converted to the \code{data.frame} by the function \code{\link{printGraphW}}. 

Note that \code{moleculeList} must be a 'character' vector. The genes must be represented by NCBI gene ids, and lncRNAs must be represented by mature lncRNA Ensemble name.
}
\author{ Xinrui Shi, Chunquan Li and Xia Li}
\seealso{\code{\link{printGraphW}}, \code{\link{getBackgroundLnc}}, \code{\link{GetExampleData}}}

\examples{
\dontrun{

### Integrate lncRNAs of competitive regulation into KEGG pathway graphs ###
  #LncGenePairs<-GetExampleData(exampleData="LncGenePairs")
  #inteUMGraph<-getInteUMGraph(LncGenePairs)
### get user-interested lncRNAs and genes sets.
  geneLnc<-GetExampleData(exampleData="geneLnc")
# get locate subpathways.
  #sub<-getLocSubGraphLnc(geneLnc,interUMGraph,type="gene_lncRNA",n=1,s=8)
   sub<-GetExampleData(exampleData="sub")
  SubcodeLncResult<-identifyLncGraphW(geneLnc,sub,type="gene_lncRNA",bet=1)
}
}
\keyword{ file }

