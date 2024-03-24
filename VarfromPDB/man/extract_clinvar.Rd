\name{extract_clinvar}
\alias{extract_clinvar}
\title{
Extract the genes and variants related to a genetic disorder from ClinVar
}
\description{
\emph{extract_clinvar} extracts the genes and variants associated to a known 
genetic disorder or a clinical feature from NCBI ClinVar database. 
It annotates the phenotypes from GeneReview, MedGen, and OMIM. 
The alias of a disease/phenotye are considered in HPO database. 
Furtherly, the variants on a use-defined gene list can be captured 
at the same time.
}
\usage{
extract_clinvar(keyword, localPDB.path = paste(getwd(),"localPDB",sep="/"), 
           type = "both", HPO.disease = NULL, genelist = NULL, OMIM = NULL)
}
\arguments{
  \item{keyword}{
character string: keyword, to describe a disease, clinical feature, or phenotype.
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
the gene(s) associated to the disease, or the genes you are interested.
}
  \item{OMIM}{
whether use the information from OMIM database. The default value is NULL.
It can be set 'yes' when you make sue you have a OMIM API key.
}
}
\details{
The function extracts the genes and variants associated to a disease, 
clinical feature or phenotype from ClinVar database. 
The keyword is searched not only in ClinVar, 
but also in HPO to considered the different alias of a diesease. 
You can prepare the files from OMIM, ClinVar, Orhanet, Uniprot, HPO, 
MedGen, and GeneReview using \emph{localPDB()} before you start the 
job, which maybe more efficient. More details ablout ClinVar can be 
seen from \url{http://www.ncbi.nlm.nih.gov/clinvar/}.
}
\value{
A list containing two components:
 \item{gene2dis }{ subset of the file gene_condition_source_id, which include 
 all the information about genes and phenotypes in ClinVar. }
 \item{variants }{ subset of the file variant_summary.txt, but added sevetal 
 colomns which describe the phenotype from GeneReview, MedGen, 
 and OMIM databases. }
}
\references{
1.Landrum MJ, Lee JM, Riley GR, Jang W, Rubinstein WS, Church DM, Maglott DR. 
ClinVar: public archive of relationships among sequence variation and human 
phenotype. Nucleic Acids Res. 2014 Jan 1;42(1):D980-5. 
doi: 10.1093/nar/gkt1113. PubMed PMID: 24234437

2.Melissa Landrum, PhD, Jennifer Lee, PhD, George Riley, PhD, Wonhee Jang, PhD, 
Wendy Rubinstein, MD, PhD, Deanna Church, PhD, and Donna Maglott, PhD.             
ClinVar. http://www.ncbi.nlm.nih.gov/books/NBK174587/

3.Sebastian K?hler, Sandra C Doelken, Christopher J. Mungall, Sebastian Bauer, 
Helen V. Firth, et al. The Human Phenotype Ontology project: linking molecular 
biology and disease through phenotype data Nucl. Acids Res. 
(1 January 2014) 42 (D1): D966-D974 doi:10.1093/nar/gkt1026
}
\author{
Zongfu Cao (caozongfu@nrifp.org.cn)
}
\seealso{
\code{\link{pheno_extract_HPO}},
\code{\link{extract_omim}},
\code{\link{extract_uniprot}},
\code{\link{extract_genes_orphanet}}
}
\examples{
#    library(VarfromPDB)
#    clinvar.phenotype = extract_clinvar(keyword="retinoblastoma")
#    genes.clinvar = clinvar.phenotype[[1]]
#    print(dim(genes.clinvar))
#    variants.clinvar = clinvar.phenotype[[2]]
#    print(dim(variants.clinvar))
}

\concept{ClinVar}
\concept{Variant}
\concept{Gene}
