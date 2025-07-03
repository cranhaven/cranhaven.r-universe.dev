\name{glmm.gei}
\alias{glmm.gei}
\title{
GLMM based single variant tests for gene-environment interactions
}
\description{
Use a glmmkin class object from the null GLMM to perform single variant main effect score test, gene-environment interaction test, or joint test for association with genotypes in a GDS file .gds.
}
\usage{
glmm.gei(null.obj, interaction, geno.file, outfile, bgen.samplefile = NULL, 
  interaction.covariates = NULL,  meta.output = FALSE,
  covar.center="interaction.covariates.only", geno.center=TRUE, 
  cat.threshold = 20, MAF.range = c(1e-7, 0.5), MAC.cutoff = 1,
  miss.cutoff = 1, RSQ.cutoff = 0, missing.method = "impute2mean",
  nperbatch=100, is.dosage = FALSE, ncores = 1, verbose = FALSE)
}
\arguments{
  \item{null.obj}{
a class glmmkin object, returned by fitting the null GLMM using \code{glmmkin( )}.
}
  \item{interaction}{
a numeric or a character vector indicating the environmental factors. If a numeric vector, it represents which indices in the order of covariates are the environmental factors; if a character vector, it represents the variable names of the environmental factors.
}
  \item{geno.file}{
the full name of a GDS file (including the suffix .gds).
}
  \item{outfile}{
the output file name.
}
  \item{bgen.samplefile}{
path to the BGEN .sample file. Required when the BGEN file does not contain sample identifiers.
}
  \item{interaction.covariates}{
a numeric or a character vector indicating the interaction covariates. If a numeric vector, it represents which indices in the order of covariates are the interaction covariates; if a character vector, it represents the variable names of the interaction covariates.
}
\item{meta.output}{
boolean value to modiy the output file.If TRUE, the GxE effect estimate and variance and covariance associated with the effect estimate are included in the output file. (default = FALSE)
}
  \item{covar.center}{
a character value for the centering option for covariates. Possible values are "none", "all", or "interaction.covariates.only". Generally, centering exposures and covariates to have mean 0 before creating interaction terms would make the genetic main effect easier to interpret. However, if a subsequent meta-analysis is expected, then the exposures of interest should not be centered because in that case the genetic main effect may have different interpretations across studies (default = "interaction.covariates.only").
}
  \item{geno.center}{
a logical switch for centering genotypes before tests. If TRUE, genotypes will be centered to have mean 0 before tests, otherwise raw values will be directly used in tests (default = TRUE).
}
  \item{cat.threshold}{
a numeric cut-off to determine which interaction terms or interaction covariates should be automatically treated as categorical based on the number of levels (unique observations). They will be treated as categorical if the number of levels (unique observations) is less than or equal to this numeric cut-off (default = 20).
}
  \item{MAF.range}{
a numeric vector of length 2 defining the minimum and maximum minor allele frequencies of variants that should be included in the analysis (default = c(1e-7, 0.5)).
}
  \item{MAC.cutoff}{
the minimum minor allele count allowed for a variant to be included (default = 1, including all variants).
}
  \item{miss.cutoff}{
the maximum missing rate allowed for a variant to be included (default = 1, including all variants).
}
  \item{RSQ.cutoff}{
the minimum Rsq value, defined as the ratio of observed and expected genotypic variance under Hardy-Weinberg equilibrium, allowed for a variant to be included (default = 0, including all variants).
}
  \item{missing.method}{
method of handling missing genotypes.Either "impute2mean" or "omit" (default = "impute2mean").
}
  \item{nperbatch}{
an integer for how many SNPs should be tested in a batch (default = 100). The computational time can increase dramatically if this value is either small or large. The optimal value for best performance depends on the user's system.
}
  \item{is.dosage}{
a logical switch for whether imputed dosage should be used from a GDS \code{geno.file} (default = FALSE).
}
  \item{ncores}{
a positive integer indicating the number of cores to be used in parallel computing (default = 1).
}
  \item{verbose}{
a logical switch for whether failed matrix inversions should be written to \code{outfile}.err for debugging (default = FALSE).
}
}
\value{
NULL
}
\references{
Chen, H., Wang, C., Conomos, M.P., Stilp, A.M., Li, Z., Sofer, T., Szpiro, A.A., Chen, W., Brehm, J.M., Celedón, J.C., Redline, S., Papanicolaou, G.J., Thornton, T.A., Laurie, C.C., Rice, K. and Lin, X. (2016) Control forpopulation structure and relatedness for binary traits in genetic association studies via logistic mixed models. The American Journal of Human Genetics 98, 653-666.
}
\author{
Xinyu Wang, Han Chen, Duy Pham
}
\examples{
  library(GMMAT)
  data(example)
  attach(example)

  model0 <- glmmkin(disease ~ age + sex, data = pheno, kins = GRM,
                    id = "id", family = binomial(link = "logit"))
                   
  if(requireNamespace("SeqArray", quietly = TRUE) && requireNamespace("SeqVarTools",
    quietly = TRUE)) {
    infile <- system.file("extdata", "geno.gds", package = "MAGEE")
    gds_outfile <- tempfile()
    glmm.gei(model0, interaction='sex', geno.file = infile, outfile = gds_outfile)
    unlink(gds_outfile)
  }
  infile <- system.file("extdata", "geno.bgen", package = "MAGEE")
  samplefile <- system.file("extdata", "geno.sample", package = "MAGEE")
  bgen_outfile <- tempfile()
  glmm.gei(model0, interaction='sex', geno.file = infile, outfile = bgen_outfile,
           bgen.samplefile = samplefile)
  unlink(bgen_outfile)
}
\keyword{ generalized linear mixed model }
\keyword{ gene-environment interaction }
\keyword{ joint test }
