\name{MAGEE.meta}
\alias{MAGEE.meta}

\title{
Mixed model Association tests for GEne-Environment interactions meta-analysis
}
\description{
Use a glmmkin class object from the null GLMM to perform meta-analysis of variant set-based main effect tests, gene-environment interaction tests, and joint tests for association with genotypes in a GDS file (.gds). 
}
\usage{
MAGEE.meta(meta.files.prefix, n.files = rep(1, length(meta.files.prefix)), 
            cohort.group.idx = NULL, group.file, group.file.sep = "\t", 
            E.match=NULL, MAF.range = c(1e-7, 0.5), 
            MAF.weights.beta = c(1, 25), miss.cutoff = 1, method = "davies", 
            tests = "JF", use.minor.allele = FALSE)
}

\arguments{
  \item{meta.files.prefix}{
a character vector for prefix of intermediate files (.score.* and .var.*) required in a meta-analysis. Each element represents the prefix of .score.* and .var.* from one cohort. The length of vector should be equal to the number of cohorts.
}
 \item{n.files}{
an integer vector with the same length as meta.files.prefix, indicating how many sets of intermediate files (.score.* and .var.*) are expected from each cohort, usually as the result of multi-threading in creating the intermediate files (default = rep(1, length(meta.files.prefix))).
}
  \item{cohort.group.idx}{
a vector with the same length as meta.files.prefix, indicating which cohorts should be grouped together in the meta-analysis assuming homogeneous genetic effects. For example, c("a","b","a","a","b") means cohorts 1, 3, 4 are assumed to have homogeneous genetic effects, and cohorts 2, 5 are in another group with homogeneous genetic effects (but possibly heterogeneous with group "a"). If NULL, all cohorts are in the same group (default = NULL).
}
  \item{group.file}{
a plain text file with 6 columns defining the test units. There should be no headers in the file, and the columns are group name, chromosome, position, reference allele, alternative allele and weight, respectively.
}
  \item{group.file.sep}{
the delimiter in group.file (default = "\\t").
}
  \item{E.match}{
environmental factors that match the interactions (default = "\\t").
}
  \item{MAF.range}{
a numeric vector of length 2 defining the minimum and maximum minor allele frequencies of variants that should be included in the analysis (default = c(1e-7, 0.5)). Filter applied to the combined samples.
}
  \item{MAF.weights.beta}{
a numeric vector of length 2 defining the beta probability density function parameters on the minor allele frequencies. This internal minor allele frequency weight is multiplied by the external weight given by the group.file. To turn off internal minor allele frequency weight and only use the external weight given by the group.file, use c(1, 1) to assign flat weights (default = c(1, 25)). Applied to the combined samples.
}
  \item{miss.cutoff}{
the maximum missing rate allowed for a variant to be included (default = 1, including all variants). Filter applied to the combined samples.
}
  \item{method}{
a method to compute p-values for SKAT-type test statistics (default = "davies"). "davies" represents an exact method that computes a p-value by inverting the characteristic function of the mixture chisq distribution, with an accuracy of 1e-6. When "davies" p-value is less than 1e-5, it defaults to method "kuonen". "kuonen" represents a saddlepoint approximation method that computes the tail probabilities of the mixture chisq distribution. When "kuonen" fails to compute a p-value, it defaults to method "liu". "liu" is a moment-matching approximation method for the mixture chisq distribution.
}
  \item{tests}{
a character vector indicating which MAGEE tests should be performed ("MV" for the main effect variance component test, "MF" for the main effect combined test of the burden and variance component tests using Fisher's method, "IV" for the interaction variance component test, "IF" for the interaction combined test of the burden and variance component tests using Fisher's method, "JV" for the joint variance component test for main effect and interaction, "JF" for the joint combined test of the burden and variance component tests for main effect and interaction using Fisher's method, or "JD" for the joint combined test of the burden and variance component tests for main effect and interaction using double Fisher's method.). The "MV" and "IV" test are automatically included when performing "JV", and the "MF" and "IF" test are automatically included when performing "JF" or "JD" (default = "JF").
}
  \item{use.minor.allele}{
a logical switch for whether to use the minor allele (instead of the alt allele) as the coding allele (default = FALSE). It does not change "MV", "IV", and "JV" results, but "MF", "IF", "JF", and "JD" results will be affected. Along with the MAF filter, this option is useful for combining rare mutations, assuming rare allele effects are in the same direction.
}

}
\value{
  \item{group}{
name of the test unit group.
}
  \item{n.variants}{
number of variants in the test unit group that pass the missing rate and allele frequency filters.
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
Wang, X., Pham, D.T., Westerman, K.E., Pan, C., Manning, A.K., Chen, H., (2022) Genomic summary statistics and meta-analysis for set-based gene-environment interaction tests in large-scale sequencing studies. medrxiv.  
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
  meta.file.prefix <- tempfile()
  out <- MAGEE(model0, interaction="sex",geno.file=geno.file,
	       group.file=group.file, meta.file.prefix = meta.file.prefix,
	       MAF.range = c(0, 0.5), miss.cutoff = 1,
	       method = "davies", tests = c("JV","JF","JD"))
  print(out)
  out1 <- MAGEE.meta(meta.file.prefix, group.file = group.file, tests = c("JV","JF","JD"))
  print(out1)
  unlink(paste0(meta.file.prefix, c(".score", ".cov"), ".1"))
}
}
}
\keyword{ variant set-based test }
\keyword{ generalized linear mixed model }
\keyword{ gene-environment interaction }
\keyword{ joint test }
\keyword{ meta-analysis }
