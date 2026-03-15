## Tests for haplo.cc

context("Testing haplo.cc with and without covariates output")
tmp <- Sys.setlocale("LC_ALL", "C")
tmp <- Sys.getlocale()
options(stringsAsFactors=FALSE)
data(hla.demo)

 label <- c("DQB","DRB","B")

  y.bin <- 1*(hla.demo$resp.cat=="low")

  geno <- as.matrix(hla.demo[,c(17,18,21:24)])
 seed <- c(17, 53, 1, 40, 37, 0, 62, 56, 5, 52, 12, 1)
 
  
#  cc.hla <- haplo.cc(y.bin, geno, miss.val=0,locus.label=label, 
#           control=haplo.glm.control(haplo.min.count=8,  em.c=haplo.em.control()))
set.seed(seed)
cc.hla.adj <-  haplo.cc(y.bin, geno, x.adj=hla.demo[,c("male","age")],
                        miss.val=0,locus.label=label, 
                        control=haplo.glm.control(haplo.min.count=8,
                                                  em.c=haplo.em.control()))

set.seed(seed)
ntest <- 200
geno.test <- cbind(sample(1:2, size=ntest, replace=TRUE),
           sample(1:2, size=100, replace=TRUE),
           sample(2:3,size=ntest, replace=TRUE),
           sample(2:3, size=100, replace=TRUE),
           sample(2:4,size=ntest, replace=TRUE, prob=c(.5,.35,.15)),
           sample(2:4, size=100,  replace=TRUE, prob=c(.5,.35,.15)))
y.test <- sample(1:2,size=ntest, replace=TRUE,prob=c(.6, .4)) - 1
x.test <- cbind(rbinom(nrow(geno.test), 1, prob=.3), round(rnorm(nrow(geno.test), mean=50, sd=4)))
locus.label <- c("A", "B", "C")

set.seed(seed)
cc.test <- haplo.cc(y.test, geno.test, locus.label=locus.label,
                  ci.prob=.95, control=haplo.glm.control(haplo.min.count=4))

if(0) {
  saveRDS(cc.test, file="cc.test.rds")
  saveRDS(cc.hla.adj, file="cc.hla.adj.rds")
}
###########################################################################################################
#### Basic functionality
###########################################################################################################
cc.hla.adj.save <- readRDS("cc.hla.adj.rds")
cc.test.save <- readRDS("cc.test.rds")

test_that("Data.frames from haplo.cc", {
  expect_equal(cc.hla.adj$cc.df, expected=cc.hla.adj.save$cc.df, tolerance=1e-3)
  expect_equal(cc.test$cc.df, expected=cc.test.save$cc.df, tolerance=1e-3)
  })

