\name{pheno_extract_HPO}
\alias{pheno_extract_HPO}
\title{
Extract the genes related to a disease or disease alias from HPO database.
}
\description{
Extract the genes associated to a disease or disease alias from the 
Human Phenotype Ontology (HPO) database. 
The keyword can also be a clinical feature. All the genes and alias of 
a disease here can be considered in other databases, 
including Ophanet, OMIM, ClinVar and Uniprot. 
}
\usage{
pheno_extract_HPO(keyword, localPDB.path = paste(getwd(),"localPDB",sep="/"))
}
\arguments{
  \item{keyword}{
character string: keyword, to search a disease, a clinical feature, 
or a phenotype.
}
  \item{localPDB.path}{
the path of localized public data bases. The default value is set in 
the working directory.
}
}
\details{
Many genetic diseases have multiple aliases, and for a clinical feature, 
there are many different disease names too. 
All he information can be gotten from HPO. More details about HPO, please 
see \url{http://www.human-phenotype-ontology.org/}.

The HPO files include phenotype_annotation.tab and diseases_to_genes, 
which can be downloaded automatically. 
However, the speed may depend on the network environment. So, we recommend to 
make the files ready locally before the jobs using \emph{localPDB()}.
}
\value{
A list contains two complements
 \item{HPO}{subset of HPO}
 \item{diseases_to_genes}{extract the genes and alias for a disease(phenotype), 
 or a clinical feature.}
}
\references{
1.Sebastian K?hler, Sandra C Doelken, Christopher J. Mungall, Sebastian Bauer, 
Helen V. Firth, et al. The Human Phenotype Ontology project: linking molecular 
biology and disease through phenotype data Nucl. Acids Res. 
(1 January 2014) 42 (D1): D966-D974 doi:10.1093/nar/gkt1026
}
\author{
Zongfu Cao (caozongfu@nrifp.org.cn)
}

\seealso{
\code{\link{extract_omim}}, 
\code{\link{extract_uniprot}}, 
\code{\link{extract_genes_orphanet}}, 
\code{\link{extract_clinvar}}
}
\examples{
#extract the phenotypes and genes from HPO
#    HPO.phenotype = pheno_extract_HPO("retinoblastoma")
}
\keyword{HPO}
\keyword{disease}
\keyword{gene}
