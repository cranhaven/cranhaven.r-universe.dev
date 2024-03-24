\name{variants_compile}
\alias{variants_compile}
\title{
Compile the disease-related variants from multiple public databases
}
\description{
To get a variant set related to a disease especially for a rare disease from 
multiple database, including omim, clinvar and uniprot.
}
\usage{
variants_compile(omim, clinvar, uniprot, 
     localPDB.path = paste(getwd(), "localPDB", sep = "/"))
}
\arguments{
  \item{omim}{
the object from \emph{extract.omim} function.
}
  \item{clinvar}{
the object from \emph{extract.clinvar} function.
}
  \item{uniprot}{
the object from \emph{extract.uniprot} function.
}
  \item{localPDB.path}{
the path of localized public data bases.
}
}
\details{
A variant maybe have different names in different databases. 
All the variants in OMIM and Uniprot are compared with ClinVar, 
then the additional variants are appended the clinVar-like 
summary variant set.
}
\value{
A matrix containing all the variant-gene-phenotypes relationships.
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
#variantsPDB <- variants_compile(omim = variants.omim, 
#                                clinvar = variants.clinvar,
#                                uniprot = variants.uniprot)
}
\keyword{ variants }
\keyword{ phenotype }
