\name{genes_add_pubmed}
\alias{genes_add_pubmed}
\title{
Compile the disease-related genes from PubMed abstracts into the gene set from 
the public databases
}
\description{
To compile the genes related to a disease especially for a rare disease from 
PubMed abstracts into the gene set  from the public databases, 
including HPO, orphanet, omim, clinvar and uniprot.
}
\usage{
genes_add_pubmed(keyword,genepdb, pubmed, 
             localPDB.path = paste(getwd(), "localPDB", sep = "/"))
}
\arguments{
  \item{keyword}{
character string: keyword, to search the disease, clinical feature, or phenotype.
}
  \item{genepdb}{
the object from function \emph{genes_compiled}.
}
  \item{pubmed}{
the object from function \emph{extract_pubmed}. The object need to be 
checked mannually.
}
  \item{localPDB.path}{
the path of localized public databases.
}
}
\details{
The relationships between genes and a phenotype are compared with those from 
public databases, then the additional relationships can be merged together. 
For the object from function \emph{extract_pubmed} maybe have noise, we 
strongly recommend that the additional relationships between genes and 
phenotypes should be pay more attention and checked mannually.   
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
   \item{pubmed}{the phenotypes from PubMed.}   
}
\author{
Zongfu Cao (caozongfu@nrifp.org.cn)
}

\seealso{
\code{\link{extract_pubmed}}, \code{\link{genes_compile}}
}
\examples{
## add the relationships between genes and phenotypes into those from 
## the public databases. 
#geneAll <- genes_add_pubmed(genepdb= genesPDB, pubmed=genes.pubmed)
}
\keyword{ phenotype }
\keyword{ gene }
\keyword{ PubMed }
