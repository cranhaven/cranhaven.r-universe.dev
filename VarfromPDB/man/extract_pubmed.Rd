\name{extract_pubmed}
\alias{extract_pubmed}
\title{
Extract the genes and variants related to a genetic disorder from PubMed
}
\description{
\emph{extract_pubmed} extracts the genes and variants related to a known genetic 
disorder or a clinical feature from NCBI PubMed.
}
\usage{
extract_pubmed(query, keyword, localPDB.path = paste(getwd(), "localPDB",
              sep = "/"))
}
\arguments{
  \item{query}{
searching strategy in PubMed, such as 
"pubmed AND gene AND mutation AND chinese NOT meta analysis".
}
  \item{keyword}{
character string: keyword, to search the disease, clinical feature, 
or phenotype.
}
  \item{localPDB.path}{
the path of localized public data bases. The default value is set in the 
working directory.
}
}
\details{
\emph{extract_pubmed} extracts the phenotypes, genes and mutations from 
PubMed abstracts, and check the gene names to appoved symbol by HGNC. 
We recommend to check the searching strategy and the results carefully.
}
\value{
A list containing two components:
 \item{pubmed_captures }{the relationships among phenotypes, genes, and 
       mutations captured from PubMed}
 \item{abstracts }{ all the abstracts captured from PubMed. }
}
\author{
Zongfu Cao (caozongfu@nrifp.org.cn)
}

\examples{
## capture the genes and mutations related to cataract in Chinese populations 
## from PubMed
#cataract_pubmed <- extract_pubmed(
#    query = "cataract AND gene AND mutation AND chinese NOT meta analysis",
#               keyword="cataract",
#               localPDB="/public/home/czf/project/rare.disease/localPDB")
 }
\keyword{PubMed}
\keyword{ rare disease }
\keyword{ genetic disease }
\keyword{ gene }
\keyword{ variant }
