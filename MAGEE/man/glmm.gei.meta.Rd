\name{glmm.gei.meta}
\alias{glmm.gei.meta}
\title{
GLMM based meta-analysis of single variant tests for gene-environment interactions
}
\description{
Use a glmmkin class object from the null GLMM to perform meta-analysis of single variant main effect score test, gene-environment interaction test, or joint test for association with genotypes in a GDS file .gds.
}
\usage{
glmm.gei.meta(files, outfile, interaction, SNPID = rep("SNPID", length(files)), 
              CHR = rep("CHR", length(files)), POS = rep("POS", length(files)), 
              Non_Effect_Allele = rep("Non_Effect_Allele", length(files)), 
              Effect_Allele = rep("Effect_Allele", length(files))) 
}
\arguments{
  \item{files}{
tab or space delimited plain text files (or compressed files that can be recognized by the R function read.table) with at least the following columns: SNPID, CHR, POS, Non_Effect_Allele, Effect_Allele, N_Samples, AF, Beta_Marginal, SE_Beta_Marginal, P_Value_Marginal, Beta_G, Beta_G_sex, SE_Beta_G, SE_Beta_G_sex, Cov_Beta_G_G.sex, P_Value_Interaction, P_Value_Joint. Generally, if each study performs score tests using genotypes in PLINK binary PED format or GDS format, the score test output from glmm.score can be directly used as input files. 
}
  \item{outfile}{
the output file name.
}
  \item{interaction}{
a numeric or a character vector indicating the environmental factors. If a numeric vector, it represents which indices in the order of covariates are the environmental factors; if a character vector, it represents the variable names of the environmental factors.
}
  \item{SNPID}{
a character vector of SNPID column names in each input file. The length and order must match the length and order of \code{files} (default = rep("SNPID", length(files))).
}
  \item{CHR}{
a character vector of CHR column names in each input file. The length and order must match the length and order of \code{files} (default = rep("CHR", length(files))).
}
  \item{POS}{
a character vector of POS column names in each input file. The length and order must match the length and order of \code{files} (default = rep("POS", length(files))).
}
  \item{Non_Effect_Allele}{
a character vector of Non_Effect_Allele column names in each input file. The length and order must match the length and order of \code{files} (default = rep("Non_Effect_Allele", length(files))).
}
  \item{Effect_Allele}{
a character vector of Effect_Allele column names in each input file. The length and order must match the length and order of \code{files} (default = rep("Effect_Allele", length(files))).
}
}

\value{
a data frame containing the following:
  \item{SNPID}{SNP name.}
  \item{CHR}{chromosome.}
  \item{POS}{physical position.}
  \item{Non_Effect_Allele}{non_effect allele frequency.}
  \item{Effect_Allele}{effect allele frequency.}
  \item{N_Samples}{number of samples.}
  \item{AF}{allele frequency.}
  \item{Beta_Marginal}{coefficient estimate for the marginal genetic effect.}
  \item{SE_Beta_Marginal}{standard error of the marginal genetic effect.}
  \item{P_Value_Marginal}{marginal effect score test p-value.}
  \item{Beta_G}{coefficient estimate for the genetic main effect.}
  \item{Beta_G-*}{coefficient estimate for the interaction terms.}
  \item{SE_Beta_G}{model-based standard error associated with the the genetic main effect.}
  \item{SE_Beta_G-*}{mdel-based standard error associated with any GxE or interaction covariate terms.}
  \item{Cov_Beta_G_G-*}{model-based covariance between the genetic main effect and any GxE or interaction covariate terms.}
  \item{P_Value_Interaction}{gene-environment interaction test p-value.}
  \item{P_Value_Joint}{joint test p-value.}
}

\references{
Chen, H., Wang, C., Conomos, M.P., Stilp, A.M., Li, Z., Sofer, T., Szpiro, A.A., Chen, W., Brehm, J.M., Celedón, J.C., Redline, S., Papanicolaou, G.J., Thornton, T.A., Laurie, C.C., Rice, K. and Lin, X. (2016) Control forpopulation structure and relatedness for binary traits in genetic association studies via logistic mixed models. The American Journal of Human Genetics 98, 653-666.
}
\author{
Xinyu Wang, Han Chen, Duy Pham
}
\examples{
\donttest{
  infile1 <- system.file("extdata", "meta1.txt", package = "MAGEE")
  infile2 <- system.file("extdata", "meta2.txt", package = "MAGEE")
  infile3 <- system.file("extdata", "meta3.txt", package = "MAGEE")
  infile4 <- system.file("extdata", "meta4.txt", package = "MAGEE")
  infile5 <- system.file("extdata", "meta5.txt", package = "MAGEE")
  outfile <- tempfile()
  glmm.gei.meta(files = c(infile1, infile2, infile3, infile4, infile5),
		outfile = outfile, interaction="sex")
  unlink(outfile)
}
}
\keyword{ generalized linear mixed model }
\keyword{ gene-environment interaction }
\keyword{ joint test }
\keyword{ meta-analysis }
