## Tests for haplo.glm anova

context("Testing anova and haplo.glm")
tmp <- Sys.setlocale("LC_ALL", "C")
tmp <- Sys.getlocale()
options(stringsAsFactor=FALSE)
label <-c("DQB","DRB","B")

  data(hla.demo)
  
  y <- hla.demo$resp
  y.bin <- 1*(hla.demo$resp.cat=="low")

  geno <- as.matrix(hla.demo[,c(17,18,21:24)])
  
  geno <- setupGeno(geno, miss.val=c(0,NA))

  # geno now has an attribute 'unique.alleles' which must be passed to
  # haplo.glm as allele.lev=attributes(geno)$unique.alleles, see below

  my.data <- data.frame(geno=geno, age=hla.demo$age, male=hla.demo$male,
                      y=y, y.bin=y.bin)

  seed <- c(17, 53, 1, 40, 37, 0, 62, 56, 5, 52, 12, 1)

set.seed(seed)
fit.hla.gaus.gender <- haplo.glm(y ~ male + geno, family = gaussian,
                 na.action="na.geno.keep",
                 data=my.data, locus.label=label,
                 control = haplo.glm.control(haplo.min.count=10))
coeff.hla.gender <- summary(fit.hla.gaus.gender)$coefficients
##if(verbose) cat("gaussian with covariate, multiplicative\n")
set.seed(seed)
fit.hla.gaus.inter <- haplo.glm(y ~ male * geno, family = gaussian,
                   na.action="na.geno.keep", data=my.data, locus.label=label,
                   control = haplo.glm.control(haplo.min.count = 10))

coeff.hla.inter <- summary(fit.hla.gaus.inter)$coefficients
#anova.hlagaus <- anova(fit.hla.gaus.gender, fit.hla.gaus.inter)

if(0) {
  saveRDS(fit.hla.gaus.gender, "fit.gaus.gender.rds")
  saveRDS(fit.hla.gaus.inter, "fit.gaus.inter.rds")
}

###################################################################
#### Basic functionality
###################################################################
fit.gender <- readRDS("fit.gaus.gender.rds")
fit.inter <- readRDS("fit.gaus.inter.rds")
#anova.inter <- anova(fit.gender, fit.inter)
coeffgender <- summary(fit.gender)$coefficients
coeffinter <- summary(fit.inter)$coefficients
test_that("Basic haplo.glm anova and glm coefficients", {
  expect_equal(coeff.hla.gender, expected=coeffgender, tolerance=1e-3)
  expect_equal(coeff.hla.inter, expected=coeffinter, tolerance=1e-3)
  })

