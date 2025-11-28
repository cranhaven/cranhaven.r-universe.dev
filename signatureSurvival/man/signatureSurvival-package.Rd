\name{signatureSurvival-package}
\alias{signatureSurvival-package}
\alias{signatureSurvival}
\docType{package}
\title{
\packageTitle{signatureSurvival}
}
\description{
\packageDescription{signatureSurvival}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{signatureSurvival}
\packageIndices{signatureSurvival}
This package is used to create up and down signatures,do univariate or 
multivariate survival analysis and make forest plot for the results of 
multivariate Cox proportional hazard survival analysis. The steps for screening
signature are as following: At step1, users should perform differential 
expression analysis of genes in one or multiple microarray datasets or RNA-seq 
count datasets or the other expression datasets and then mark these 
differentally expressed (DE) genes selected with "up" and "down" using negative 
or positive t-values. At step2, retrieve survival (or clinical data) using these 
DE genes and construct a new survival data(age, sex, stages/smoking, month, 
status, and DE genes in column and patients in row). Note that expression data 
ofthe DE genes are listed in the right side in the survival data. At step 3,
perform musvtest.R (multiple univariate survival tests) or mvstest
(multiple multivariate survival tests) with covariates age, sex and/smoking ect.
Use p-value to select genes in big difference between low and high-survival 
probalities and use HR and up and down-regulation to classify genes selected 
into up and down groups in multiple cohorts. At step 4,use weight.R to
calculate weight values of each gene in signature and use signatureExp.R to 
caculate expression values of signature in all patients and move the expression
values to the last column in survival data. At step 5, perform MUKMplot.R or 
MMKMplot.R on signature in the survival data to plot Kaplan-Meier survival 
curves.
}
\author{
\packageAuthor{signatureSurvival}

Maintainer: \packageMaintainer{signatureSurvival}
}

\keyword{ package }

\examples{
data(GSE50081)
res<-musvtest(sdata=GSE50081,stn=3500,gn=3506,time="month",status="status",
quant=c("no",-0.2,0.2))

}
