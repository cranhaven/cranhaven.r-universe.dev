\name{GenoScan.VCF.chr}
\alias{GenoScan.VCF.chr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scan a VCF file to study the association between an quantitative/dichotomous outcome variable and a region or whole chromosome by score type statistics allowing for multiple functional annotation scores.
}
\description{
Once the preliminary work is done by "GenoScan.prelim()", this function scan a target region or chromosome, and output results for all windows as well as an estimated significance threshold. For genome-wide scan, users can scan each chromosome individually, then the genome-wide significance threshold can be obtained by combining chromosome-wise thresholds:

alpha=1/(1/alpha_1+1/alpha_2+...+1/alpha_22).
}
\usage{
GenoScan.VCF.chr(result.prelim,vcf.filename,chr,pos.min=NULL,pos.max=NULL,
Gsub.id=NULL,annot.filename=NULL,cell.type=NULL,MAF.weights='beta',
test='combined',window.size=c(5000,10000,15000,20000,25000,50000),
MAF.threshold=1,impute.method='fixed')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{result.prelim}{
The output of function "GenoScan.prelim()"
}
  \item{vcf.filename}{
A character specifying the directory (including the file name) of the vcf file.
}
  \item{chr}{
Chromosome number.
}
  \item{pos.min}{
Minimum position of the scan. The default is NULL, where the scan starts at the first base pair.
}
  \item{pos.max}{
Maximum position of the scan. The default is NULL, where the scan ends at the last base pair, according to the chromosome sizes at:

http://hgdownload.cse.ucsc.edu/goldenPath/hg19/bigZips/hg19.chrom.sizes.
}
  \item{Gsub.id}{
The subject id corresponding to the genotype matrix, an n dimensional vector. This is used to match phenotype with genotype. The default is NULL, where the subject id in the vcf file is used.
}
  \item{annot.filename}{
A character specifying the directory (including the file name) of functional annotations. Currently GenoScan supports GenoNet scores across 127 tissues/cell types, which can be downloaded at:

http://www.openbioinformatics.org/annovar/download/GenoNetScores/
}
  \item{cell.type}{
A character specifying the tissue/cell type integrated in the analysis, in addition to standard dispersion and/or burden tests. The default is NULL, where no functional annotation is included. If cell.type='all', GenoNet scores across all 127 tissues/cell types are incorperated.
}
  \item{MAF.weights}{
Minor allele frequency based weight. Can be 'beta' to up-weight rare variants or 'equal' for a flat weight. The default is 'beta'.
}
  \item{test}{
Can be 'dispersion', 'burden' or 'combined'. The test is 'combined', both dispersion and burden tests are applied. The default is 'combined'.
}
  \item{window.size}{
Candidate window sizes in base pairs. The default is c(5000,10000,15000,20000,25000,50000). Note that extemely small window size (e.g. 1) requires large sample size.
}
  \item{MAF.threshold}{
Threshold for minor allele frequency. Variants above MAF.threshold are ignored. The default is 1.
}
  \item{impute.method}{
Choose the imputation method when there is missing genotype. Can be "random", "fixed" or "bestguess". Given the estimated allele frequency, "random" simulates the genotype from binomial distribution; "fixed" uses the genotype expectation; "bestguess" uses the genotype with highest probability.
}
%  \item{region.step}{
%GenoScan devides the chromosome into sub-regions. This is to choose the size of each sub-region. The default is 200kb. This is usually for internal use only.
%}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  \item{window.summary}{
Results for all windows. Each row presents a window.
}
  \item{M}{
Estimated number of effective tests.
}
  \item{threshold}{
Estimated threshold, 0.05/M.
}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{

# load example vcf file from package "seqminer"
vcf.filename = system.file("vcf/all.anno.filtered.extract.vcf.gz", package = "seqminer")

# simulated outcomes, covariates and inidividual id.
Y<-as.matrix(rnorm(3,0,1))
X<-as.matrix(rnorm(3,0,1))
id<-c("NA12286", "NA12341", "NA12342")

# fit null model
result.prelim<-GenoScan.prelim(Y,X=X,id=id,out_type="C",B=5000)

# scan the vcf file
result<-GenoScan.VCF.chr(result.prelim,vcf.filename,chr=1,pos.min=196621007,pos.max=196716634)


## this is how the actual genotype matrix from package "seqminer" looks like
example.G <- t(readVCFToMatrixByRange(vcf.filename, "1:196621007-196716634",annoType='')[[1]])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{VCF}
\keyword{scan}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
