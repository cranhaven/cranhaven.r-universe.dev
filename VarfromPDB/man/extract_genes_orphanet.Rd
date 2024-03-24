\name{extract_genes_orphanet}
\alias{extract_genes_orphanet}
\title{
Extract the genes related to a genetic disorder from Orphanet
}
\description{
\emph{extract_genes_orphanet} extracts the genes associated to a known genetic 
disorder or a clinical feature from Orphanet database. 
The alias of a disease/phenotye are considered based on HPO database and then 
capture the information in Orphanet.
}
\usage{
extract_genes_orphanet(keyword, 
          localPDB = paste(getwd(),"localPDB",sep="/"), HPO.disease = NULL)
}

\arguments{
  \item{keyword}{
character string: keyword, to search the disease, clinical feature, or phenotype.
}
  \item{localPDB}{
the path of localized public data bases. The default value is set in the 
working directory.
}
 \item{HPO.disease}{
Orpha Number of the disease. The default value is NULL, which means that 
all the Orpha Numbers of the disease in HPO are added.
}
}
\details{
The function extracts the genes associated to a genetic disease especial 
rare disease, or a clinical feature or phenotype from Orphanet database. 
The keyword is searched not only in Ophanet, but also in HPO considering the 
alias of the diesease. More details about Ophanet can be seen in 
\url{http://www.orpha.net/consor/cgi-bin/index.php}. 
}
\value{
a matrix will be returned including

1.OrphaNumber

2.Phenotype

3.GeneSymbol

4.GeneName

5.GeneType

6.AssociationType

7.AssociationStatus
}
\references{
1.Orphanet: an online rare disease and orphan drug data base. INSERM 1997.
Available on http://www.orpha.net. Accessed [date accessed].

2.Orphadata: Free access data from Orphanet. INSERM 1997.
Available on http://www.orphadata.org. Data version [XML]
}
\author{
Zongfu Cao (caozongfu@nrifp.org.cn)
}

\seealso{
\code{\link{pheno_extract_HPO}},
\code{\link{extract_omim}},
\code{\link{extract_uniprot}},
\code{\link{extract_clinvar}}
}
\examples{
    ## extract genes from Orphanet
#    orphanet.phenotype = extract_genes_orphanet(keyword = "retinoblastoma")
 }

\keyword{ Ophanet }
\keyword{ gene }
\keyword{ rare disease }
