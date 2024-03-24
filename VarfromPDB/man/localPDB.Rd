\name{localPDB}
\alias{localPDB}
\title{
Localize the public databases including HPO, MedGen, GeneReview, HGNC, Orphanet, 
ClinVar and Uniprot.
}
\description{
\emph{localPDB} downloads the necessary files from the public databases including 
HPO, MedGen, GeneReview, HGNC, Orphanet, ClinVar and Uniprot.
}
\usage{
localPDB(localPDB.path = paste(getwd(), "localPDB", sep = "/"), PDB = "all", 
         omim.url = NULL, download.method = "curl_fetch_disk")
}
\arguments{
  \item{localPDB.path}{
the path to localize the public databases.
}
  \item{PDB}{
which database to localize. The value must be one of "all"(default), "HPO", 
"MedGen", "GeneReview", "HGNC", "Orphanet", "ClinVar" or "Uniprot". 
}
  \item{omim.url}{
the FTP URL of OMIM. 
}
  \item{download.method}{
the method for downloading files, including "curl_fetch_disk", "curl_download",
"download.file".  
}
}

\details{
The function gets the necessary files from the public databases including HPO, 
MedGen, GeneReview, HGNC, OMIM, Orphanet, ClinVar and Uniprot.

For the \emph{omim.url}, you shoul apply for an OMIM account from 
\url{http://omim.org/downloads} and get the FTP URL. 
}

\references{
1.Sebastian Kohler, Sandra C Doelken, Christopher J. Mungall, Sebastian Bauer, 
Helen V. Firth, et al. The Human Phenotype Ontology project: linking molecular 
biology and disease through phenotype data Nucl. Acids Res. 
(1 January 2014) 42 (D1): D966-D974 doi:10.1093/nar/gkt1026

2.Orphanet: an online rare disease and orphan drug data base. INSERM 1997.
Available on http://www.orpha.net. Accessed [date accessed].

3.Orphadata: Free access data from Orphanet. INSERM 1997.
Available on http://www.orphadata.org. Data version [XML]


4.Landrum MJ, Lee JM, Riley GR, Jang W, Rubinstein WS, Church DM, Maglott DR. 
ClinVar: public archive of relationships among sequence variation and human 
phenotype. Nucleic Acids Res. 2014 Jan 1;42(1):D980-5. 
doi: 10.1093/nar/gkt1113. PubMed PMID: 24234437

5.The UniProt Consortium UniProt: a hub for protein information. 
Nucleic Acids Res. 43: D204-D212 (2015).

6.Amberger JS, Bocchini CA, Schiettecatte F, Scott AF, Hamosh A. OMIM.org:
Online Mendelian Inheritance in Man (OMIM), an online catalog of human genes 
and genetic disorders. Nucleic Acids Res. 2015 Jan;43(Database issue):D789-98. 
doi:10.1093/nar/gku1205. Epub 2014 Nov 26. PubMed PMID: 25428349; 
PubMed Central PMCID: PMC4383985.

7.GeneReviews: http://www.ncbi.nlm.nih.gov/books/NBK1116/

8.MedGen: http://www.ncbi.nlm.nih.gov/medgen

9.OMIM:http://www.omim.org/
}
\author{
Zongfu Cao (caozongfu@nrifp.org.cn)
}
\keyword{Public databases }
