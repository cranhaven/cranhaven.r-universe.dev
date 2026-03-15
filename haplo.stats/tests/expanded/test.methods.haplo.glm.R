
## package: haplo.stats
## test script: haplo.glm
## created: 11/23/2011

## settings
verbose=TRUE
require(haplo.stats)
Sys.setlocale("LC_COLLATE", "C")
Sys.getlocale('LC_COLLATE')


if(verbose) cat("setting up data...\n")
  
 
  # prepare the hla dataset,
     # runs a lot longer, and MS alleles don't all start w/ 1, 2...
  label <-c("DQB","DRB","B")

  data(hla.demo)
  
  y <- hla.demo$resp
  y.bin <- 1*(hla.demo$resp.cat=="low")
  geno <- as.matrix(hla.demo[,c(17,18,21:24)])
  geno <- setupGeno(geno, miss.val=c(0,NA))

  # geno now has an attribute 'unique.alleles' which must be passed to
  # haplo.glm as allele.lev=attributes(geno)$unique.alleles, see below

  hla.data <- data.frame(geno=geno, age=hla.demo$age, male=hla.demo$male,
                      y=y, y.bin=y.bin)

  seed <- c(17, 53, 1, 40, 37, 0, 62, 56, 5, 52, 12, 1)


if(verbose) cat("fit a binary trait\n")
set.seed(seed)
fit.hla.bin <- haplo.glm(y.bin ~ male + geno, family = binomial,
                 na.action="na.geno.keep", data=hla.data, locus.label=label,
                 control = haplo.glm.control(haplo.min.count=8))

y.bin <- 1*(hla.demo$resp.cat=="low")
y.bin[2] <- NA
geno.hla <- as.matrix(hla.demo[,c(17,18,21:24)])
geno.hla[2,5] <- 2
geno.hla[3,] <- rep(NA, 6)
geno.hla <- setupGeno(geno.hla, miss.val=c(0,NA))

my.hla <- data.frame(geno.hla=geno.hla, age=hla.demo$age, male=hla.demo$male,
                     y=y, y.bin=y.bin)

if(verbose) cat(" hla binary trait with subject that are removed\n")

set.seed(seed)
fit.hla.miss <- haplo.glm(y.bin ~ male + geno.hla, family = binomial,
                     na.action="na.geno.keep",
                     data=my.hla, locus.label=label,
                     control = haplo.glm.control(haplo.min.count=8))


if(verbose) cat(" gaussian with covariates, additive\n")

set.seed(seed)
fit.hla.gaus.gender <- haplo.glm(y ~ male + geno, family = gaussian,
                 na.action="na.geno.keep",
                 data=hla.data, locus.label=label,
                 control = haplo.glm.control(haplo.min.count=5))

 
if(verbose) cat("SNAP data with resp and resp with added variance\n")
snapDF <- read.table("snapData.csv",header=TRUE, sep=",", stringsAsFactors=FALSE)

geno.rec <- setupGeno(snapDF[,-c(1:9)])
snap.data <- data.frame(resp=hla.demo$resp, respvar=hla.demo$resp*100, geno=geno.rec)

set.seed(seed)
fit.resp.hla <- haplo.glm(resp~geno, trait.type="gaussian",data=snap.data)

set.seed(seed)
fit.respvar.hla <- haplo.glm(respvar~geno, trait.type="gaussian",data=snap.data)
  

cat("summary function\n")

print(summary(fit.hla.bin),digits=3)


cat("fitted values for hlabin, hla-gaussian, hla-gaussian-hi-variance\n")

print(fitted(fit.hla.bin)[1:20],digits=3)

print(fitted(fit.resp.hla)[1:20],digits=3)
print(fitted(fit.respvar.hla)[1:20],digits=3)

cat("vcov for hlabin, hla-gaussian, hla-gaussian-hi-variance\n")

print(vcov(fit.hla.bin)[1:20,1:20],digits=3)

print(vcov(fit.resp.hla),digits=3)
print(vcov(fit.respvar.hla),digits=3)

cat("residuals  for hlabin, hla-gaussian, hla-gaussian-hi-variance\n")

print(residuals(fit.hla.bin, type="deviance")[1:20],digits=3)
print(residuals(fit.hla.bin, type="pearson")[1:20],digits=3)
print(residuals(fit.hla.bin, type="response")[1:20],digits=3)

print(residuals(fit.resp.hla)[1:20],digits=3)
print(residuals(fit.respvar.hla)[1:20],digits=3)

