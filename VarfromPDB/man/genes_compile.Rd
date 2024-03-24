\name{genes_compile}
\alias{genes_compile}
\title{
Compile the disease-related genes from multiple public databases
}
\description{
To compile a gene set related to a disease especially for a rare disease from 
multiple databases, including HPO, orphanet, omim, clinvar and uniprot.
}
\usage{
genes_compile(HPO, orphanet, omim, clinvar, uniprot,
       localPDB.path = paste(getwd(),"localPDB",sep="/"))
}
\arguments{
  \item{HPO}{
the object from \emph{pheno.extract.HPO} function.
}
  \item{orphanet}{
the object from \emph{extract.genes.orphanet} function.
}
  \item{omim}{
the object from \emph{extract.omim} function.The default value is NULL.
}
  \item{clinvar}{
the object from \emph{extract.clinvar} function.
}
  \item{uniprot}{
the object from \emph{extract.uniprot} function.
}
  \item{localPDB.path}{
the path of localized public databases.
}
}
\details{
The relationships between genes and a phenotype in different databases can be 
intergrated automatically. 
}
\value{
A matrix containing the following information
   \item{GeneSymbol }{gene symbols from HGNC.}
   \item{chr }{chorosomes of the genes.}
   \item{strand }{strands of the genes.}
   \item{start }{start positions (hg19) of the genes.}
   \item{end }{end positions (hg19) of the genes.}
   \item{EntrezGeneID }{Entrez GeneID}
   \item{ApprovedName }{Approved gene name from HGNC.}
   \item{Synonyms }{gene Synonyms.}
   \item{HPO }{the phenotypes from HPO.}
   \item{Orphanet }{the phenotypes from orphanet.}
   \item{OMIM }{the phenotypes from OMIM.}
   \item{ClinVar }{the phenotypes from ClinVar.}
   \item{Uniprot }{the phenotypes from Uniprot.}
}
\author{
Zongfu Cao (caozongfu@nrifp.org.cn)
}
\seealso{
 \code{\link{pheno_extract_HPO}}, \code{\link{extract_omim}}, 
 \code{\link{extract_genes_orphanet}}, \code{\link{extract_clinvar}}, 
 \code{\link{extract_uniprot}}
}
\examples{
## compile the gene-disease relationship from multiple databases
#genesPDB <- genes_compile(HPO = HPO.Joubert, orphanet = orphanet.joubert,
#                          omim = genes.omim,
#                          clinvar = genes.clinvar,
#                          uniprot = genes.uniprot)
}                          

\keyword{ phenotype }
\keyword{ gene }
