## Tests for haplo.cc

context("Testing haplo.power for binary and quantitative")
tmp <- Sys.setlocale("LC_ALL", "C")
tmp <- Sys.getlocale()
options(stringsAsFactors=FALSE)
haplo <- rbind(
               c(     1,    2,    2,    1,    2),
               c(     1,    2,    2,    1,    1),
               c(     1,    1,    2,    1,    1),
               c(     1,    2,    1,    1,    2),
               c(     1,    2,    2,    2,    1),
               c(     1,    2,    1,    1,    1),
               c(     1,    1,    2,    2,    1),
               c(     1,    1,    1,    1,    2),
               c(     1,    2,    1,    2,    1),
               c(     1,    1,    1,    2,    1),
               c(     2,    2,    1,    1,    2),
               c(     1,    1,    2,    1,    2),
               c(     1,    1,    2,    2,    2),
               c(     1,    2,    2,    2,    2),
               c(     2,    2,    2,    1,    2),
               c(     1,    1,    1,    1,    1),
               c(     2,    1,    1,    1,    1),
               c(     2,    1,    2,    1,    1),
               c(     2,    2,    1,    1,    1),
               c(     2,    2,    1,    2,    1),
               c(     2,    2,    2,    1,    1))
dimnames(haplo)[[2]] <- paste("loc", 1:ncol(haplo), sep=".")
haplo <- data.frame(haplo)

haplo.freq <- c(0.170020121, 0.162977867, 0.123742455, 0.117706237, 0.097585513,
    0.084507042, 0.045271630, 0.039235412, 0.032193159, 0.019114688, 0.019114688,
    0.013078471, 0.013078471, 0.013078471, 0.013078471, 0.006036217, 0.006036217,
    0.006036217, 0.006036217, 0.006036217, 0.006036217)

## define index for risk haplotypes (having alleles 1-1 at loci 2 and 3)
haplo.risk <- (1:nrow(haplo))[haplo$loc.2==1 & haplo$loc.3==1]

## define index for baseline haplotype
base.index <-  1

############ Example for Case-control power/sample size
## specify OR for risk haplotypes
or <- 1.25

## determine beta regression coefficients for risk haplotypes

haplo.beta <- numeric(length(haplo.freq))
haplo.beta[haplo.risk] <-  log(or)

# Note that non-risk haplotypes have beta=0, as does the intercept
# (haplotype with base.index value). 
##if(verbose) cat("Compute total sample size for given power\n")

ss.cc <- haplo.power.cc(haplo, haplo.freq, base.index, haplo.beta, case.frac=.5, prevalence=.1, alpha=.05, power=.8)

##if(verbose) cat("Compute power for given sample size\n")

power.cc <- haplo.power.cc(haplo, haplo.freq, base.index, haplo.beta, case.frac=.5, prevalence=.1, alpha=.05, sample.size=11978)

############ Example for Quantitative trait power/sample size
# Because it can be easier to speficy genetic effect size in terms of
# a regression model R-squared value (r2), we use an
# auxiliary function to set up haplo.beta based on a specifed r2 value:
qthap <- find.haplo.beta.qt(haplo,haplo.freq,base.index,haplo.risk, r2=0.01, y.mu=0, y.var=1)
haplo.beta <- qthap$beta

# Compute sample size for given power

ss.qt <- haplo.power.qt(haplo, haplo.freq, base.index, haplo.beta, y.mu=0, y.var=1, alpha=.05, power=.80) 
  
# Compute power for given sample size

power.qt <- haplo.power.qt(haplo, haplo.freq, base.index, haplo.beta, y.mu=0, y.var=1, alpha=.05, sample.size = 2091)

if(0) {
  saveRDS(list(power.cc=power.cc, ss.cc=ss.cc, power.qt=power.qt, ss.qt=ss.qt), file="powercc.rds")

}
powerList <- readRDS("powercc.rds")


###################################################################
test_that("Power/SS for cc, qt", {
  expect_equal(power.cc, expected=powerList$power.cc, tolerance=1e-3)
  expect_equal(ss.cc, expected=powerList$ss.cc, tolerance=1e-3)
  expect_equal(power.qt, expected=powerList$power.qt, tolerance=1e-3)
  expect_equal(ss.qt, expected=powerList$ss.qt, tolerance=1e-3)
  })

