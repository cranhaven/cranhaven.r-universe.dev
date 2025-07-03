\name{MAGEE}
\alias{MAGEE}
\alias{MAGEE.prep}
\alias{MAGEE.lowmem}

\title{
Mixed model Association tests for GEne-Environment interactions
}
\description{
Use a glmmkin class object from the null GLMM to perform variant set-based main effect tests, gene-environment interaction tests, and joint tests for association with genotypes in a GDS file (.gds). 7 user-defined tests are included: Main effect variance component test (MV), Main effect hybrid test of burden and variance component test using Fisher's method (MF), Interaction variance component test (IV), Interaction hybrid test of burden and variance component test using Fisher's method (IF), Joint variance component test (JV), Joint hybrid test of burden and variance component test using Fisher's method (JF), and Joint hybrid test of burden and variance component test using double Fisher's procedures (JD). 
}
\usage{
MAGEE(null.obj, interaction, geno.file, group.file,  group.file.sep = "\t", 
      bgen.samplefile = NULL, interaction.covariates = NULL, meta.file.prefix = NULL,
      MAF.range = c(1e-7, 0.5), AF.strata.range = c(0, 1), MAF.weights.beta = c(1, 25), 
      miss.cutoff = 1, missing.method = "impute2mean", method = "davies", tests = "JF", 
      use.minor.allele = FALSE, auto.flip = FALSE, 
      Garbage.Collection = FALSE, is.dosage = FALSE, ncores = 1)

MAGEE.prep(null.obj, interaction, geno.file, group.file, interaction.covariates = NULL,
           group.file.sep = "\t", auto.flip = FALSE)
           
MAGEE.lowmem(MAGEE.prep.obj, geno.file = NULL, meta.file.prefix = NULL, 
             MAF.range = c(1e-7, 0.5), AF.strata.range = c(0, 1), 
             MAF.weights.beta = c(1, 25), miss.cutoff = 1, missing.method = "impute2mean",
             method = "davies", tests = "JF", use.minor.allele = FALSE, 
	     Garbage.Collection = FALSE, is.dosage = FALSE, ncores = 1)
}

\arguments{
  \item{null.obj}{
a class glmmkin object, returned by fitting the null GLMM using \code{glmmkin( )}.
}
  \item{interaction}{
a numeric or a character vector indicating the environmental factors. If a numberic vector, it represents which indices in the order of covariates are the environmental factors; if a character vector, it represents the variable names of the environmental factors.
}
  \item{geno.file}{
a .gds file for the full genotypes. The \code{sample.id} in \code{geno.file} should overlap \code{id_include} in \code{null.obj}. It is recommended that \code{sample.id} in \code{geno.file} include the full samples (at least all samples as specified in \code{id_include} of \code{null.obj}). It is not necessary for the user to take a subset of \code{geno.file} before running the analysis.
}
  \item{group.file}{
a plain text file with 6 columns defining the test units. There should be no headers in the file, and the columns are group name, chromosome, position, reference allele, alternative allele and weight, respectively.
}
  \item{group.file.sep}{
the delimiter in group.file (default = \code{"\t"}).
}
  \item{bgen.samplefile}{
path to the BGEN .sample file. Required when the BGEN file does not contain sample identifiers.
}
  \item{interaction.covariates}{
a numeric or a character vector indicating the interaction covariates. If a numeric vector, it represents which indices in the order of covariates are the interaction covariates; if a character vector, it represents the variable names of the interaction covariates.
}
  \item{meta.file.prefix}{
the prefix for meta-analysis (default = \code{"NULL"}).
}
  \item{MAF.range}{
a numeric vector of length 2 defining the minimum and maximum minor allele frequencies of variants that should be included in the analysis (default = c(1e-7, 0.5)).
}
  \item{AF.strata.range}{
a numeric vector of length 2 defining the minimum and maximum coding allele frequencies of variants in each stratum that should be included in the analysis, if the environmental factor is categorical (default = c(0, 1)).
}
  \item{MAF.weights.beta}{
a numeric vector of length 2 defining the beta probability density function parameters on the minor allele frequencies. This internal minor allele frequency weight is multiplied by the external weight given by the group.file. To turn off internal minor allele frequency weight and only use the external weight given by the group.file, use c(1, 1) to assign flat weights (default = c(1, 25)).
}
  \item{miss.cutoff}{
the maximum missing rate allowed for a variant to be included (default = 1, including all variants).
}
  \item{missing.method}{
method of handling missing genotypes. Either "impute2mean" or "impute2zero" (default = "impute2mean").
}
  \item{method}{
a method to compute p-values for the test statistics (default = "davies"). "davies" represents an exact method that computes a p-value by inverting the characteristic function of the mixture chisq distribution, with an accuracy of 1e-6. When "davies" p-value is less than 1e-5, it defaults to method "kuonen". "kuonen" represents a saddlepoint approximation method that computes the tail probabilities of the mixture chisq distribution. When "kuonen" fails to compute a p-value, it defaults to method "liu". "liu" is a moment-matching approximation method for the mixture chisq distribution.
}
  \item{tests}{
a character vector indicating which MAGEE tests should be performed ("MV" for the main effect variance component test, "MF" for the main effect combined test of the burden and variance component tests using Fisher's method, "IV" for the interaction variance component test, "IF" for the interaction combined test of the burden and variance component tests using Fisher's method, "JV" for the joint variance component test for main effect and interaction, "JF" for the joint combined test of the burden and variance component tests for main effect and interaction using Fisher's method, or "JD" for the joint combined test of the burden and variance component tests for main effect and interaction using double Fisher's method.). The "MV" and "IV" test are automatically included when performing "JV", and the "MF" and "IF" test are automatically included when performing "JF" or "JD" (default = "JF").
}
  \item{use.minor.allele}{
a logical switch for whether to use the minor allele (instead of the alt allele) as the coding allele (default = FALSE). It does not change "MV", "IV", and "JV" results, but "MF", "IF", "JF", and "JD" results will be affected.Along with the MAF filter, this option is useful for combining rare mutations, assuming rare allele effects are in the same direction.
}
  \item{auto.flip}{
a logical switch for whether to enable automatic allele flipping if a variant with alleles ref/alt is not found at a position, but a variant at the same position with alleles alt/ref is found (default = FALSE). Use with caution for whole genome sequence data, as both ref/alt and alt/ref variants at the same position are not uncommon, and they are likely two different variants, rather than allele flipping.
}
  \item{Garbage.Collection}{
a logical switch for whether to enable garbage collection in each test (default = FALSE). Pay for memory efficiency with slower computation speed.
}
  \item{is.dosage}{
a logical switch (default = FALSE).
}
  \item{ncores}{
a positive integer indicating the number of cores to be used in parallel computing (default = 1).
}
  \item{MAGEE.prep.obj}{
a class MAGEE.prep object, returned by \code{MAGEE.prep}.
}
}
\value{
a data frame with the following components:
  \item{group}{
name of the test unit group.
}
  \item{n.variants}{
number of variants in the test unit group that pass the missing rate and allele frequency filters.
}
  \item{miss.min}{
minimum missing rate for variants in the test unit group.
}
  \item{miss.mean}{
mean missing rate for variants in the test unit group.
}
  \item{miss.max}{
maximum missing rate for variants in the test unit group.
}
  \item{freq.min}{
minimum coding allele frequency for variants in the test unit group.
}
  \item{freq.mean}{
mean coding allele frequency for variants in the test unit group.
}
  \item{freq.max}{
maximum coding allele frequency for variants in the test unit group.
}
  \item{freq.strata.min}{
minimum coding allele frequency of each stratum if the environmental factor is categorical.
}
  \item{freq.strata.max}{
maximum coding allele frequency of each stratum if the environmental factor is categorical.
}
  \item{MV.pval}{
MV test p-value.
}
  \item{MF.pval}{
MF test p-value.
}
  \item{IV.pval}{
IV test p-value.
}
  \item{IF.pval}{
IF test p-value.
}
  \item{JV.pval}{
JV test p-value.
}
  \item{JF.pval}{
JF test p-value.
}
  \item{JD.pval}{
JD test p-value.
}
}
\references{
Wang, X., Lim, E., Liu, C, Sung, Y.J., Rao, D.C., Morrison, A.C., Boerwinkle, E., Manning, A. K., and Chen, H. (2020) Efficient gene-environment interaction tests for large biobank-scale sequencing studies. Genetic Epidemiology, 44(8): 908-923.
Chen, H., Huffman, J.E., Brody, J.A., Wang, C., Lee, S., Li, Z., Gogarten, S.M., Sofer, T., Bielak, L.F., Bis, J.C., et al. (2019) Efficient variant set mixed model association tests for continuous and binary traits in large-scale whole-genome sequencing studies. The American Journal of Human Genetics, 104 (2): 260-274.
}
\author{
Xinyu Wang, Han Chen, Duy Pham
}
\examples{
\donttest{
if(requireNamespace("SeqArray", quietly = TRUE) && requireNamespace("SeqVarTools",
  quietly = TRUE)) {
  library(GMMAT)
  data(example)
  attach(example)
  model0 <- glmmkin(disease ~ age + sex, data = pheno, kins = GRM,
    id = "id", family = binomial(link = "logit"))
  geno.file <- system.file("extdata", "geno.gds", package = "MAGEE")
  group.file <- system.file("extdata", "SetID.withweights.txt", package = "MAGEE")
  out <- MAGEE(model0, interaction='sex', geno.file, group.file, group.file.sep = "\t",
    tests=c("JV", "JF", "JD"))
  print(out)
}
}
}
\keyword{ variant set-based test }
\keyword{ generalized linear mixed model }
\keyword{ gene-environment interaction }
\keyword{ joint test }
