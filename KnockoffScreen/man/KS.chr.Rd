\name{KS.chr}
\alias{KS.chr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scan a .vcf/.bgen file to identify the putative causal loci in whole-genome sequencing data
}
\description{
Once the preliminary work is done by "KS.prelim()", this function scan a chromosome given a set of pre-defined windows. It also evalautes invidual variants within those windows.
}
\usage{
KS.chr(result.prelim,seq.filename,window.bed,region.pos=NULL,
tested.pos=NULL,excluded.pos=NULL,M=5,thres.single=0.01,
thres.ultrarare=25,thres.missing=0.10,midout.dir=NULL,temp.dir=NULL,
jobtitle=NULL,Gsub.id=NULL,impute.method='fixed',
bigmemory=T,leveraging=T,LD.filter=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{result.prelim}{
The output of function "KS.prelim()"
}
  \item{seq.filename}{
A character specifying the directory (including the file name) of the vcf/bgen file.The algorithm will recognize the file extension and analyze the file accordingly.
}
  \item{window.bed}{
A matrix specifying the windows being tested. Each row presents a window (chr, start, end), similar to a .bed file. We recommand to define window.bed with window sizes 1000,5000,10000 (base pairs) for sample size ~5000. For studies with smaller sample size, we recommand to increase the window size for stable inference of ultra rare variants.
}
  \item{region.pos}{
A vector specifying the start and end of each region being scanned.This provides better control for memory use. Usually we define region.pos such that number of variants in each region is bounded by 5000. For example: region.pos=c(1,100,300) corresponds to two regions (1,100) and (100,300). Default is defined by window.bed.
}
  \item{tested.pos}{
A vector specifying position being tested. The default is to test all variants.
}
  \item{excluded.pos}{
A vector specifying position being excluded (due to QC or other filters). The default is to not exclude any variant.
}
  \item{M}{
Number of knockoffs per variant. The default is 5.
}
  \item{thres.single}{
The minor allele frequency threshold to define single genetic variants being tested. Variants with minor allele frequencies above the threshold will be tested individually. The default is 0.01 (for sample size ~5000). Smaller threshold requires a larger sample size.
}
  \item{thres.ultrarare}{
The minor allele count threshold to filter out ultrarare variants. Variants with minor allele counts below the threshold will be excluded. The default is 25.
}
  \item{thres.missing}{
The missing rate threshold to filter out variants with a low genotyping rate. Variants with missing rate above the threshold will be excluded. The default is 0.1.
}
  \item{midout.dir}{
A character specifying the directory to save intermediate results. It is recommended when large scale data is being analyzed.
}
  \item{temp.dir}{
A character specifying the directory to save temporary data. It is required when CADD or GenoNet scores are used.
}
  \item{jobtitle}{
A character specifying the job title.
}
  \item{Gsub.id}{
The subject id corresponding to the genotype matrix, an n dimensional vector. This is used to match phenotype with genotype. The default is NULL, where the subject id in the vcf file is used.
}
  \item{impute.method}{
Choose the imputation method when there is missing genotype. Can be "random", "fixed" or "bestguess". Given the estimated allele frequency, "random" simulates the genotype from binomial distribution; "fixed" uses the genotype expectation; "bestguess" uses the genotype with highest probability.
}
  \item{bigmemory}{
Whether "bigmemory" operation is applied. Default is TRUE.
}
  \item{leveraging}{
Whether "algorithmic leveraging" is applied. Default is TRUE.
}
  \item{LD.filter}{
Choose the LD filter for tightly linked variants. Default is 0.75. This applied a hierarchical clustering of genetic variants prior to the analysis, where variants in the same cluster have a pair-wise correlation >=0.75. Then the analysis is restricted to one randomly selected representative variant in each cluster.
}

%  \item{region.step}{
%GenoScan devides the chromosome into sub-regions. This is to choose the size of each sub-region. The default is 200kb. This is usually for internal use only.
%}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  \item{result.window}{
Results for all windows. Each row presents a window.
}
  \item{result.single}{
Results for all individual variants with minor allele frequency above the specified threshold. Each row presents a variant.
}
}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

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

library(KnockoffScreen)

# load example vcf file from package "seqminer"
vcf.filename = system.file("vcf/1000g.phase1.20110521.CFH.var.anno.vcf.gz", package = "seqminer")

## this is how the actual genotype matrix from package "seqminer" looks like
example.G <- t(readVCFToMatrixByRange(vcf.filename, "1:196621007-196716634",annoType='')[[1]])

# simulated outcomes, covariates and inidividual id.
Y<-as.matrix(rnorm(nrow(example.G),0,1))
X<-as.matrix(rnorm(nrow(example.G),0,1))
id<-rownames(example.G)

# fit null model
result.prelim<-KS.prelim(Y,X=X,id=id,out_type="C")

# Define the window.bed file
chr<-1
pos.min<-196621007;pos.max<-196716634
window.size=c(2000)

window.bed<-c();
for(size in window.size){
  pos.tag<-seq(pos.min,pos.max,by=size*1/2)
  window.bed<-rbind(window.bed,cbind(chr,pos.tag,pos.tag+size))
}
window.bed<-window.bed[order(as.numeric(window.bed[,2])),]

# scan the vcf file
midout.dir<-NULL # or '/YourProjectDir/MidResults/'
temp.dir<-NULL # or '/YourProjectDir/Temp_out/' #this is a folder to save temporary results
jobtitle<-'YourProjectTitle'

# we set thres.single=0.1,thres.ultrarare=0 for a proof of concept.
# note that the default for real data analysis is thres.single=0.01, thres.ultrarare=25
fit <- KS.chr(result.prelim,vcf.filename,window.bed,M=5,thres.single=0.1,thres.ultrarare=0,
midout.dir=midout.dir,temp.dir=temp.dir,jobtitle=jobtitle)

# summarize the results
result.summary<-KS.summary(fit$result.window,fit$result.single,M=5)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{VCF}
\keyword{analysis}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
