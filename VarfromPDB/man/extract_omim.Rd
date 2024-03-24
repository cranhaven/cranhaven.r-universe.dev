\name{extract_omim}
\alias{extract_omim}
\title{
Extract the genes and variants related to a genetic disorder from OMIM
}
\description{
\emph{extract_omim} extracts the genes and variants related to a known genetic 
disorder or a clinical feature from NCBI OMIM database.The alias of a 
disease/phenotype are caputured from HPO database and searched in OMIM. 
Furtherly, the variants on a use-defined gene list can be captured meanwhile.
}
\usage{
extract_omim(keyword, omim.apiKey,  
             localPDB.path = paste(getwd(),"localPDB",sep="/"), 
             type = "both", HPO.disease = NULL, genelist = NULL)
}
\arguments{
  \item{keyword}{
character string: keyword, to search the disease, clinical feature, or phenotype.
}
  \item{omim.apiKey}{
the API key of OMIM.
}
  \item{localPDB.path}{
the path of localized public data bases. The default value is set in the 
working directory.
}
  \item{type}{
the type of the information to extract, must be one of "gene", "variant", 
"both"(default).
}
  \item{HPO.disease}{
MIM number of the disease. The default value is NULL, which means that all the 
MIM number of the disease in HPO are added.
}
  \item{genelist}{
the gene(s) related to the disease, or the genes you are interested.
}
}
\details{
\emph{extract_omim} extracts the genes from OMIM first, and then translate to 
appoved gene symbol by HGNC. Then the variants are captured for each gene 
from OMIM API. However, you should apply for an account and an API key from OMIM.

We recommend to make the files ready locally before a job, in order to avoid 
a possible failure by the bad network environment.
}
\value{
A list containing two components:
 \item{morbidmap }{the subset of the file \emph{morbidmap}, 
 which include all the information about genes and phenotypes in OMIM. }
 \item{mutations }{ all the mutations in the genes in OMIM. }
}
\references{
1.OMIM:http://www.omim.org/

2.Amberger JS, Bocchini CA, Schiettecatte F, Scott AF, Hamosh A. OMIM.org:
Online Mendelian Inheritance in Man (OMIM), an online catalog of human genes and
genetic disorders. Nucleic Acids Res. 2015 Jan;43(Database issue):D789-98. doi:
10.1093/nar/gku1205. Epub 2014 Nov 26. PubMed PMID: 25428349; PubMed Central
PMCID: PMC4383985.
}
\author{
Zongfu Cao (caozongfu@nrifp.org.cn)
}

\seealso{
\code{\link{pheno_extract_HPO}},
\code{\link{extract_uniprot}},
\code{\link{extract_genes_orphanet}},
\code{\link{extract_clinvar}}
}
\keyword{OMIM}
\keyword{ rare disease }
\keyword{ genetic disease }
\keyword{ gene }
\keyword{ variant }
