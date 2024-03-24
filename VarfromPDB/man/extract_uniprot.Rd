\name{extract_uniprot}
\alias{extract_uniprot}
\title{
Extract the genes and variants related to a genetic disorder from UniprotKB
}
\description{
\emph{extract_uniprot} extracts the genes and variants associated to a known 
genetic disorder or a clinical feature from the UniProt Knowledgebase (UniprotKB). 
The alias of a disease/phenotye are caputured from HPO database. Furtherly, 
the gene mutations on a gene list can also be captured at the same time.
}
\usage{
extract_uniprot(keyword, localPDB.path = paste(getwd(),"localPDB",sep="/"), 
                HPO.disease = NULL, genelist = NULL)
}
\arguments{
  \item{keyword}{
character string: keyword, to search the disease, clinical feature, or phenotype.
}
  \item{localPDB.path}{
the path of localized public data bases. The default value is set in the 
working directory.
}
  \item{HPO.disease}{
MIM number of the disease. The default value is NULL, which means that all the 
MIM number of the disease in HPO are added.
}
  \item{genelist}{
the gene(s) associated to the disease, or the genes you are interested.
}
}
\details{
\emph{extract_uniprot} extracts the genes and variants from Uniprot, which 
focus on amino acid altering variants, 
and manually curated Human polymorphisms and disease mutations from 
UniProtKB/Swiss-Prot.

The Uniprot file \emph{humsavar} can be downloaded automatically. However, the 
speed may depend on the network environment. 
So, we recommend to make the file ready locally before the jobs using 
\emph{localPDB()}.
}
\value{
A list containing two components:
 \item{genes.extr }{genes captured from Uniprot. }
 \item{dat.extr }{variants captured from Uniprot. }
}
\references{
1.The UniProt Consortium UniProt: a hub for protein information. 
Nucleic Acids Res. 43: D204-D212 (2015).
}
\author{
Zongfu Cao (caozongfu@nrifp.org.cn)
}


\seealso{
\code{\link{pheno_extract_HPO}}, 
\code{\link{extract_omim}}, 
\code{\link{extract_genes_orphanet}}, 
\code{\link{extract_clinvar}}
}
\examples{
## extract the genes and variants associated to a known mendelian 
## disorder from uniprot
#    uniprot.phenotype = extract_uniprot(keyword="retinoblastoma")
}
\keyword{ Uniprot }
\keyword{ gene }
