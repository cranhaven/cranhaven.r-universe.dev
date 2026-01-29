\name{ACATO}
\alias{ACATO}
\title{omnibus aggregated Cauchy association test}
\description{
This test combines P values via aggregated Cauchy association test (Liu, Y. et al., 2019)
}
\usage{
ACATO(p)
}

\arguments{
	\item{p}{a vector of P values to combine}
}
\details{
	ACATO is an omnibus aggregated Cauchy association test recently proposed by Liu, Y. et al. (2019).
	This function can be used to combine P values obtained by different gene-based tests for the same gene
	(see Examples below).
}
\value{
	a single P value which is a combination of the P values given in input
}
\references{
	Liu Y. et al. (2019) ACAT: a fast and powerful p value combination method for rare-variant analysis in sequencing studies. Am. J. Hum. Genet. 104, 410-421.
}
\examples{

cor.path <- system.file("testfiles/", package = "sumFREGAT")
score.file <- system.file("testfiles/CFH.full.vcf.gz", package = "sumFREGAT")

p.bt <- BT(score.file, genes = "CFH", cor.path = cor.path)$pvalue
p.skat <- SKAT(score.file, genes = "CFH", cor.path = cor.path)$pvalue
p.pca <- PCA(score.file, genes = "CFH", cor.path = cor.path, n = 85)$pvalue

p.combined <- ACATO(c(p.bt, p.skat, p.pca))

}
