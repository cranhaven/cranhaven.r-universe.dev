\name{PAS.binmod}
\alias{PAS.binmod}
\alias{binmod}
\title{
binmod
}
\description{
This is the main function for bin model analysis.
}
\usage{
binmod(x, y, map, beta0=NA, binsizelist=-1, full.search=FALSE, foldid=NA, ...)
}

\arguments{
  \item{x}{
    input matrix, of dimensions nobs*nvars; each row is a observation vector of an individual and each column is a genotypic indicator vector for a molecular marker.
}
\item{y}{a matrix of response variable (phenotypic observations), of dimensions nobs*1.}
\item{map}{A data frame for linkage map or physical map.}
\item{beta0}{Estimated SNP effects obtained by univariate analysis. By default, the glm function in R will be called by the binmod to calculate the estimates of effects.}
\item{binsizelist}{A list of binsizes to be considered in the analysis. A default list will be generated if the option was ignored or an invalid list has been specified.}
\item{full.search}{
  A logic indicator selecting search strategies. If FALSE was assigned, the binmod will complete the running as soon as the optimal binsize was found. Otherwise, analysis will be conducted for all binsizes on the list. 
}
\item{foldid}{
  An optional vector of values between 1 and nfold identifying what fold each observation is in. If not supplied, a random vector is generated under nfold=10.
}
  \item{\dots}{
Other parameters need to be passed to glmnet/r and glm/r. 
}
}
\details{
The function invokes binmod analysis for genomic value prediction. The default settings are strongly suggested for new users.
}
\value{
\item{grid}{information of all searched binsizes}
\item{grid$mselist}{a 'data.frame': nbinsizes of 4 variables # A list of mean square errors}
\item{grid$mselist$binsize}{size settings of the bins, eight in bp or cM.}
\item{grid$mselist$mse}{mean square error}
\item{grid$mselist$mse_std}{the standard deviation of MSEs}
\item{grid$mselist$nbin}{number of bins under the binsize setting}
\item{grid$optbinsize}{optimal binsize}
\item{grid$optid}{order of the optimal binsize in the grid}
\item{optimal}{result obtained under the optimal binsize}
\item{optimal$predict}{phentypic values and its' predicted values under the optimal model.}
\item{optimal$predict$y}{original phenotypic observations}
\item{optimal$predict$yp_cv}{predictions by 10-fold cross-validation.}
\item{optimal$beta}{estimated bin parameters}
\item{optimal$beta$beta}{bin effect}
\item{optimal$beta$SSx}{sum of square of bin indicator}
\item{optimal$beta$Se}{residual error}
\item{optimal$beta$Sb}{estimating error of bin effect}
\item{optimal$beta$Wald}{Wald-test statistics}
\item{optimal$beta$LOD}{LOD-test statistics}
\item{optimal$xbin}{indicator matrix of the bins under the optimal binsize}
\item{optimal$map}{'data.frame':	of  5 variables: #bin map}
\item{optimal$map$chr}{chromosome id}
\item{optimal$map$pos}{bin position}
\item{optimal$map$pos_id}{mean of the orders of markers in the bin}
\item{optimal$map$start_id}{the order the first maker in a bin}
\item{optimal$map$end_id}{the order the last maker in a bin}
\item{optimal$binsize}{optimal binsize}
\item{optimal$cv}{cross-validation results}
\item{optimal$cv$binsize}{binsize}
\item{optimal$cv$nbin}{number of bins under the binsize setting}
\item{optimal$cv$mse}{mean squared error obtained from cross-validation}
\item{optimal$cv$r}{Pearson's correlation coefficient obtained from cross-validation}
\item{snp}{SNP information}
\item{snp$map}{linkage map or physical map}
\item{snp$map$chr}{chromosome id}
\item{snp$map$pos}{marker position}
\item{snp$map$pos_id}{marker order}
\item{snp$effect}{single marker analysis result}
\item{snp$effect$beta}{SNP effect}
\item{snp$effect$SSx}{sum of square of genotypic indicator}
\item{snp$effect$Se}{residual variance}
\item{snp$effect$Sb}{estimating error of marker effect}
\item{snp$effect$Wald}{Wald-test statistics}
\item{snp$effect$LOD}{LOD test statistics}
\item{snp$mapinfo}{a brief summary of the map}
\item{snp$mapinfo$chr}{chromosome id}
\item{snp$mapinfo$start}{the position of the first marker on the chromosome}
\item{snp$mapinfo$end}{the position of the last marker on the chromosome}
\item{snp$mapinfo$length}{length of the chromosome}
\item{snp$mapinfo$nmark}{number of markers on the chromosome}
\item{snp$mapinfo$aver}{average interval of the chromosome}
\item{snp$mapinfo$min.interval}{the smallest interval size on the chromosome}
\item{cvfit}{A cv.glmnet project. See manual of glmnet for details.}
}
\references{
Zhiqiu Hu, Zhiquan Wang, and Shizhong Xu (2012) An infinitesimal model for quantitative trait genomic value prediction. PloS ONE
}
\examples{
#load PAS library
library (PAS)
#load the demo data
data (beef)
#perform binmod analysis under the default settings.
binmod.result=binmod (x , y , map)
#plot binmod result
plot(binmod.result)
str(binmod.result)
#Output the predicted phenotypic values that was obtained
#by 10-fold cross validation .
predict(binmod.result)
#predict the phenotypic values for new individuals
x1=x[sample(1:NROW(x) , 20), ]
bin.pred.x1=predict(binmod.result, newx=x1)
str(bin.pred.x1)
}
