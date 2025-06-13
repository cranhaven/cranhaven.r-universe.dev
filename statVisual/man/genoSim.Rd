\name{genoSim}
\alias{genoSim}
\docType{data}
\title{
	An ExpressionSet Object Storing Simulated Genotype Data
}
\description{
	An ExpressionSet object storing simulated genotype data with 10 SNPs and 100 subjects. 
}
\usage{data("genoSim")}
%\format{
%  The format is:
%Formal class 'ExpressionSet' [package "Biobase"] with 7 slots
%  ..@ experimentData   :Formal class 'MIAME' [package "Biobase"] with 13 slots
%  .. .. ..@ name             : chr ""
%  .. .. ..@ lab              : chr ""
%  .. .. ..@ contact          : chr ""
%  .. .. ..@ title            : chr ""
%  .. .. ..@ abstract         : chr ""
%  .. .. ..@ url              : chr ""
%  .. .. ..@ pubMedIds        : chr ""
%  .. .. ..@ samples          : list()
%  .. .. ..@ hybridizations   : list()
%  .. .. ..@ normControls     : list()
%  .. .. ..@ preprocessing    : list()
%  .. .. ..@ other            : list()
%  .. .. ..@ .__classVersion__:Formal class 'Versions' [package "Biobase"] with 1 slot
%  .. .. .. .. ..@ .Data:List of 2
%  .. .. .. .. .. ..$ : int [1:3] 1 0 0
%  .. .. .. .. .. ..$ : int [1:3] 1 1 0
%  ..@ assayData        :<environment: 0x00000000056d1c60> 
%  ..@ phenoData        :Formal class 'AnnotatedDataFrame' [package "Biobase"] with 4 slots
%  .. .. ..@ varMetadata      :'data.frame':	2 obs. of  1 variable:
%  .. .. .. ..$ labelDescription: chr [1:2] NA NA
%  .. .. ..@ data             :'data.frame':	100 obs. of  2 variables:
%  .. .. .. ..$ sid: int [1:100] 1 2 3 4 5 6 7 8 9 10 ...
%  .. .. .. ..$ grp: num [1:100] 1 1 1 1 1 1 1 1 1 1 ...
%  .. .. ..@ dimLabels        : chr [1:2] "sampleNames" "sampleColumns"
%  .. .. ..@ .__classVersion__:Formal class 'Versions' [package "Biobase"] with 1 slot
%  .. .. .. .. ..@ .Data:List of 1
%  .. .. .. .. .. ..$ : int [1:3] 1 1 0
%  ..@ featureData      :Formal class 'AnnotatedDataFrame' [package "Biobase"] with 4 slots
%  .. .. ..@ varMetadata      :'data.frame':	2 obs. of  1 variable:
%  .. .. .. ..$ labelDescription: chr [1:2] NA NA
%  .. .. ..@ data             :'data.frame':	10 obs. of  2 variables:
%  .. .. .. ..$ snp    : int [1:10] 1 2 3 4 5 6 7 8 9 10
%  .. .. .. ..$ memSNPs: num [1:10] 1 1 0 0 0 0 0 0 0 0
%  .. .. ..@ dimLabels        : chr [1:2] "featureNames" "featureColumns"
%  .. .. ..@ .__classVersion__:Formal class 'Versions' [package "Biobase"] with 1 slot
%  .. .. .. .. ..@ .Data:List of 1
%  .. .. .. .. .. ..$ : int [1:3] 1 1 0
%  ..@ annotation       : chr ""
%  ..@ protocolData     :Formal class 'AnnotatedDataFrame' [package "Biobase"] with 4 slots
%  .. .. ..@ varMetadata      :'data.frame':	0 obs. of  1 variable:
%  .. .. .. ..$ labelDescription: chr(0) 
%  .. .. ..@ data             :'data.frame':	100 obs. of  0 variables
%  .. .. ..@ dimLabels        : chr [1:2] "sampleNames" "sampleColumns"
%  .. .. ..@ .__classVersion__:Formal class 'Versions' [package "Biobase"] with 1 slot
%  .. .. .. .. ..@ .Data:List of 1
%  .. .. .. .. .. ..$ : int [1:3] 1 1 0
%  ..@ .__classVersion__:Formal class 'Versions' [package "Biobase"] with 1 slot
%  .. .. ..@ .Data:List of 4
%  .. .. .. ..$ : int [1:3] 3 5 1
%  .. .. .. ..$ : int [1:3] 2 42 0
%  .. .. .. ..$ : int [1:3] 1 3 0
%  .. .. .. ..$ : int [1:3] 1 0 0
%}
\details{
	The simulated genotype data contain 50 cases and 50 controls. Each subject has genotype data for 10 SNPs. The first 2 SNPs have different minor allele frequencies (MAFs) between cases and controls (MAF for cases is 0.4 and MAF for controls is 0.2). We assume Hardy Weinberg Equilibrium.
	The remaining 8 SNPs have the same MAF (\eqn{MAF=0.2}) in both cases and controls.
}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(genoSim)

print(genoSim)
}
\keyword{datasets}
