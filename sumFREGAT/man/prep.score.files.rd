\name{prep.score.files}
\alias{prep.score.files}
\title{Prepare score files}
\description{
Calculates Z scores from P values and beta input
}
\usage{
prep.score.files(data, reference = "ref1KG.MAC5.EUR_AF.RData",
output.file.prefix)
}

\arguments{
	\item{data}{a file name or data.frame with two mandatory columns (case-insensitive header):\cr\cr
	"ID": unique names of genetic variants (rsIDs if 1000G correlation matrices will be used)\cr
	"P": P value\cr\cr
	Chromosome and positions are desirable:\cr\cr
	"CHROM": chromosome\cr
	"POS": positions for the same build as in \code{gene.file} (see \code{gene-based test functions})\cr\cr
	Map data are required to assign genetic variants to genes. If not present in input file, the function will
	attempt to link them from the reference file (see \code{reference} below).\cr\cr
	Additional columns that can be present in input file to be used in gene-based tests:\cr\cr
	"EA": effect allele\cr
	"BETA": effect size (betas and genetic correlations should be calculated for the same genotype coding)\cr
	"EAF": effect allele frequency\cr\cr
	Effect allele and size data can be proccesed with reference file only (see \code{reference} below).\cr\cr
	"REF": reference allele\cr
	"ALT": alternative allele\cr\cr
	"REF" and "ALT" columns can be used to compare alleles with those in reference file and exclude genetic variants if
	alleles do not match\cr\cr
	Annotation columns:\cr\cr
	"ANNO": functional annotations (like "intron_variant", "synonymous", "missense" etc.)\cr
	"PROB", "PROB1", "PROB2", "PROB3" etc.: probabilities of a variant to be causal, can be passed to
	\code{sumSTAAR()} (PHRED scale) or \code{FFGAS()}\cr\cr
	For example:\cr\cr
	CHROM POS ID EA P BETA EAF\cr
	1 196632134 1:196632134 T 0.80675 0.22946 0.00588\cr
	1 196632386 1:196632386 A 0.48694 0.65208 0.00588\cr
	1 196632470 1:196632470 G 0.25594 -0.19280 0.19412\cr\cr
	Avoid rounding of betas and P values as this can affect the precision of regional tests.\cr\cr
	The more data (columns) is present in input file, the more gene-based tests are available to run. ACAT test is available
	with minimal input (rsIDs and P values). Together with correlation matrices (reference matrices calculated from 1000G data
	are available at http://mga.bionet.nsc.ru/sumFREGAT/) it allows to run \code{minp()}, \code{simpleM}, and \code{sumchi()} tests.
	Adding info on effect allele ("EA") and effect size ("BETA") enables essentially all sumFREGAT tests. Adding allele frequencies
	enables standard weighting via beta distribution (see \code{gene-based test functions} for details).
	}

	\item{reference}{path to a reference file or data.frame with additional data needed to recode user \code{data} according to
	correlation matrices that will be used. Reference file for 1000G correlation matrices is available at
	http://mga.bionet.nsc.ru/sumFREGAT/. Reference file contains "ID" column with names of genetic variants that are used
	in correlation matrices as well as "REF" and "ALT" columns with alleles that were coded during calculation of correlation
	coefficients as 0 and 1, respectively. Effect sizes from \code{data} will be inverted for variants with effect alleles
	different from "ALT" alleles in reference data. If presented, "REF" and "ALT" columns from the input data will be used to
	sort out variants with alleles different from those in reference data. The reference file can also be a source of map data
	and allele frequencies if they are not present in \code{data}. "AF" column in the reference file represents the allele
	frequency of "ALT" allele.}

	\item{output.file.prefix}{if not set, the input file name will be used as output prefix.}

}
\value{
	
	does not return any value, writes output files with Z scores to be used in any type of
	gene-based analysis in sumFREGAT (see \code{gene-based test functions}).

}
\examples{

\dontrun{

data <- system.file("testfiles/CFH.dat", package = "sumFREGAT")
prep.score.files(data, output.file.prefix = "CFH")

# requires reference file "ref1KG.MAC5.EUR_AF.RData" (can be downloaded
# at http://mga.bionet.nsc.ru/sumFREGAT/)

data <- system.file("testfiles/CFH.full.input.dat", package = "sumFREGAT")
prep.score.files(data, reference = "ref1KG.MAC5.EUR_AF.RData",
	output.file.prefix = "CFH.full")

data <- system.file("testfiles/CFH.prob.dat", package = "sumFREGAT")
prep.score.files(data, reference = "ref1KG.MAC5.EUR_AF.RData",
	output.file.prefix = "CFH.prob")
}
}
