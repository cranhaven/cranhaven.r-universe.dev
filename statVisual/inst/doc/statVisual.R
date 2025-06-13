## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  echo = FALSE,
  collapse = TRUE,
  comment = "#>",
  fig.width = 12, 
  fig.height = 8,
  fig.align = "center"
)

## ----eval = T, echo=T, message=F----------------------------------------------
# packages in Bioconductor
library(Biobase)    # base package for Bioconductor
library(limma)      # linear models for continuous omics data
library(pvca)       # principal variance component analysis

# packages in CRAN
library(dplyr)      # data manipulation and pipe operation
library(factoextra) # extract and visualize results of multivariate data analysis
library(forestplot) # forest plot
library(gbm)        # generalized boosted regression models
library(GGally)     # extension to 'ggplot2'
library(ggdendro)   # dendrogram for data clustering
library(ggfortify)  # data visualization tools for statistical analysis results
library(ggplot2)    # create graphics based on "The Grammer of Graphics"
library(ggrepel)    # tidy text display in ggplot
library(glmnet)     # cross validation plot for glmnet
library(grDevices)  # R graphics devices and support for colors and fonts
library(gridExtra)  # Grid graphics
library(knitr)      # dynamic report generation
library(methods)    # formal methods and classes
library(pROC)       # display and analyze ROC curves
library(randomForest) # Random forest variable importance
library(reshape2)   # flexibly reshape data
library(rmarkdown)  # dynamic documents for R
library(rpart.plot) # plots for recursive partitioning for classification, regression and survival trees
library(tibble)     # simple data frames
library(stats)      # basic statistical functions

## ----eval = T, echo=T, message=F----------------------------------------------
library(statVisual)

## ----eval = F, echo = T, message = F------------------------------------------
#  library(help = statVisual)

## ----eval = F, echo = T, message = F------------------------------------------
#  help(Hist)
#  ?Hist

## ----eval = T, echo = T, message = F------------------------------------------
data(diffCorDat)

print(dim(diffCorDat))
print(diffCorDat[1:2,])

## ----eval = T, echo = T, message = F------------------------------------------
data(esSim)

print(dim(esSim))
print(esSim)

## ----eval = T, echo = T, message = F------------------------------------------
data(longDat)

print(dim(longDat))
print(longDat[1:2,])

## ----message = F, eval = T, echo = T, warning = F-----------------------------
# expression data
dat = exprs(esSim)
print(dim(dat))
print(dat[1:2,])

# phenotype data
pDat = pData(esSim)
print(dim(pDat))
print(pDat[1:2,])

# feature data
fDat = fData(esSim)
print(dim(fDat))
print(fDat[1:2,])

# choose the first probe which is over-expressed in cases
pDat$probe1 = dat[1,]

# check histograms of probe 1 expression in cases and controls
pDat$grp=factor(pDat$grp)
print(table(pDat$grp, useNA = "ifany"))

## ----message = F, eval = F, echo = T, warning = F-----------------------------
#  Hist(
#       data = pDat,
#       y = 'probe1',
#       group = 'grp')
#  

## ----message = F, eval = T, echo = T, warning = F-----------------------------
statVisual(type = 'Hist', 
       data = pDat, 
       y = 'probe1', 
       group = 'grp') 


## ----message = F, eval = F, echo = T, warning = F-----------------------------
#  
#  Den(
#      data = pDat,
#      y = 'probe1',
#      group = 'grp')

## ----message = F, eval = T, echo = T, warning = F-----------------------------
statVisual(type = 'Den',
    data = pDat, 
    y = 'probe1', 
    group = 'grp') 

## ----message = F, eval = F, echo = T, warning = F-----------------------------
#  XYscatter(
#    data = diffCorDat,
#    x = 'probe1',
#    y = 'probe2',
#    group = 'grp',
#    title = 'Scatter Plot: probe1 vs probe2')
#  

## ----message = F, eval = T, echo = T, warning = F-----------------------------
statVisual(type = 'XYscatter',
  data = diffCorDat, 
  x = 'probe1', 
  y = 'probe2', 
  group = 'grp', 
  title = 'Scatter Plot: probe1 vs probe2')

## ----message = F, eval = T, echo = T, warning = F-----------------------------
data(genoSim)

pDat = pData(genoSim)
geno = exprs(genoSim)

pDat$snp1 = geno[1,]
print(table(pDat$snp1, pDat$grp, useNA="ifany"))


## ----message = F, eval = F, echo = T, warning = F-----------------------------
#  stackedBarPlot(dat = pDat,
#  	       catVar = "snp1",
#  	       group = "grp",
#                 xlab = "snp1",
#  	       ylab = "Count",
#  	       group.lab = "grp",
#                 title = "Stacked barplots of counts for SNP1",
#                 catVarLevel = NULL)

## ----message = F, eval = T, echo = T, warning = F-----------------------------
statVisual(type = 'stackedBarPlot',
  dat = pDat, 
  catVar = "snp1", 
  group = "grp", 
  xlab = "snp1", 
  ylab = "Count", 
  group.lab = "grp",
  title = "Stacked barplots of counts for SNP1",
  catVarLevel = NULL)

## ----message = F, eval = T, echo = T, warning = F-----------------------------
statVisual(type = 'stackedBarPlot',
  dat = pDat, 
  catVar = "snp1", 
  group = "grp", 
  xlab = "snp1", 
  ylab = "Count", 
  group.lab = "grp",
  title = "Stacked barplots of counts for SNP1",
  catVarLevel = c(2, 0, 1))


## ----message = F, eval = T, echo = T, warning = F-----------------------------

library(tidyverse)
library(ggplot2)


print(head(mtcars))

print(table(mtcars$gear, useNA="ifany"))


## ----message = F, eval = F, echo = T, warning = F-----------------------------
#  
#  BiAxisErrBar(
#    dat = mtcars,
#    group = "gear",
#    y.left = "mpg",
#    y.right = "wt")
#  

## ----message = F, eval = T, echo = T, warning = F-----------------------------
statVisual(type = "BiAxisErrBar",
  dat= mtcars,
  group = "gear",
  y.left = "mpg",
  y.right = "wt")


## ----message = F, eval = F, echo = T,warning = F------------------------------
#  
#  LinePlot(
#    data = longDat,
#    x = 'time',
#    y = 'y',
#    sid = 'sid',
#    group = 'grp')
#  

## ----message = F, eval = T, echo = T,warning = F------------------------------
statVisual(type = "LinePlot",
  data = longDat,
  x = 'time',
  y = 'y',
  sid = 'sid',
  group = 'grp')

## ----message = F, eval = T, echo = T,warning = F------------------------------
library(dplyr)

## ----message = F, eval = F, echo = T,warning = F------------------------------
#  Box(
#      data = longDat,
#      x = 'time',
#      y = 'y',
#      group = 'grp',
#      title = "Boxplots across time")

## ----message = F, eval = T, echo = T,warning = F------------------------------
statVisual(type = 'Box', 
           data = longDat, 
           x = 'time', 
           y = 'y', 
           group = 'grp',
	   title = "Boxplots across time") 

## ----message = F, eval = F, echo = T, warning = F-----------------------------
#  ErrBar(
#    data = longDat,
#    x = 'time',
#    y = 'y',
#    group = 'grp',
#    title = "Dot plots across time")

## ----message = F, eval = T, echo = T, warning = F-----------------------------
statVisual(type = 'ErrBar', 
  data = longDat, 
  x = 'time', 
  y = 'y', 
  group = 'grp',
  title = "Dot plots across time") 


## ----message = F, eval = F, echo = T, warning = F-----------------------------
#  barPlot(
#    data = longDat,
#    x = 'time',
#    y = 'y',
#    group = 'grp',
#    title = "Bar plots across time")

## ----message = F, eval = T, echo = T, warning = F-----------------------------
statVisual(type = 'barPlot', 
  data = longDat, 
  x = 'time', 
  y = 'y', 
  group = 'grp',
  title = "Bar plots across time") 


## ----message = F, eval = T, echo = T, warning = F-----------------------------
library(ggdendro)
data(esSim)
dat = exprs(esSim)
print(dim(dat))
print(dat[1:2,])

# phenotype data
pDat = pData(esSim)
print(dim(pDat))
print(pDat[1:2,])

# choose the first 6 probes (3 OE probes, 2 UE probes, and 1 NE probe)
pDat$probe1 = dat[1,]
pDat$probe2 = dat[2,]
pDat$probe3 = dat[3,]
pDat$probe4 = dat[4,]
pDat$probe5 = dat[5,]
pDat$probe6 = dat[6,]

print(pDat[1:2, ])

# check histograms of probe 1 expression in cases and controls
pDat$grp=factor(pDat$grp)
print(table(pDat$grp, useNA = "ifany"))



## ----message = F, eval = F, echo = T, warning = F-----------------------------
#  
#  Dendro(
#         x = pDat[, c(3:8)],
#         group = pDat$grp)

## ----message = F, eval = T, echo = T,warning = F------------------------------
statVisual(type = 'Dendro', 
           x = pDat[, c(3:8)], 
           group = pDat$grp)

## ----message = F, eval = T, echo = T, warning = F-----------------------------
# generate simulated data
set.seed(1234567)
dat.x = matrix(rnorm(500), nrow = 100, ncol = 5)
dat.y = matrix(rnorm(500, mean = 2), nrow = 100, ncol = 5)
dat = rbind(dat.x, dat.y)
grp = c(rep(0, 100), rep(1, 100))
print(dim(dat))

res = iprcomp(dat, center = TRUE, scale.  =  FALSE)

# for each row, set one artificial missing value
dat.na=dat
nr=nrow(dat.na)
nc=ncol(dat.na)
for(i in 1:nr)
{
  posi=sample(x=1:nc, size=1)
  dat.na[i,posi]=NA
}

res.na = iprcomp(dat.na, center = TRUE, scale.  =  FALSE)

##
# pca plot
##
par(mfrow = c(3,1))
# original data without missing values
plot(x = res$x[,1], y = res$x[,2], xlab = "PC1", ylab  =  "PC2")
# perturbed data with one NA per probe 
# the pattern of original data is captured
plot(x = res.na$x[,1], y = res.na$x[,2], xlab = "PC1", ylab  =  "PC2", main = "with missing values")
par(mfrow = c(1,1))

## ----message = F, eval = T, echo = T, warning = F-----------------------------
data(esSim)
dat = exprs(esSim)
print(dim(dat))
print(dat[1:2,])

# phenotype data
pDat = pData(esSim)
print(dim(pDat))
print(pDat[1:2,])

# choose the first 6 probes (3 OE probes, 2 UE probes, and 1 NE probe)
pDat$probe1 = dat[1,]
pDat$probe2 = dat[2,]
pDat$probe3 = dat[3,]
pDat$probe4 = dat[4,]
pDat$probe5 = dat[5,]
pDat$probe6 = dat[6,]

print(pDat[1:2, ])

# check histograms of probe 1 expression in cases and controls
pDat$grp=factor(pDat$grp)
print(table(pDat$grp, useNA = "ifany"))


library(factoextra)

pca.obj = iprcomp(pDat[, c(3:8)], scale. = TRUE)

# scree plot
factoextra::fviz_eig(pca.obj, addlabels = TRUE)

## ----message = F, eval = F, echo = T, warning = F-----------------------------
#  PCA_score(prcomp_obj = pca.obj,
#            dims = c(1, 3),
#            data = pDat,
#            color = 'grp',
#            loadings = FALSE)
#  

## ----message = F, eval = T, echo = T, warning = F-----------------------------
statVisual(type = 'PCA_score',
           prcomp_obj = pca.obj, 
           dims = c(1, 2),
           data = pDat, 
           color = 'grp',
           loadings = FALSE)

## ----message = F, eval = T, echo = T, warning = F-----------------------------
data(esSim)
dat = exprs(esSim)
print(dim(dat))
print(dat[1:2,])

# phenotype data
pDat = pData(esSim)
print(dim(pDat))
print(pDat[1:2,])

# choose the first 6 probes (3 OE probes, 2 UE probes, and 1 NE probe)
pDat$probe1 = dat[1,]
pDat$probe2 = dat[2,]
pDat$probe3 = dat[3,]
pDat$probe4 = dat[4,]
pDat$probe5 = dat[5,]
pDat$probe6 = dat[6,]

print(pDat[1:2, ])

pDat$grp=factor(pDat$grp)

## ----message = F, eval = F, echo = T, warning = F-----------------------------
#  Heat(
#       data = pDat[, c(2:8)],
#       group = 'grp')
#  

## ----message = F, eval = T, echo = T,warning = F------------------------------
statVisual(type = 'Heat', 
           data = pDat[, c(2:8)], 
           group = 'grp')

## ----message = F, eval = T, echo = T, warning = F-----------------------------
library(pvca)

# create a fake Batch variable
data(esSim)
esSim$Batch=c(rep("A", 4), rep("B", 6), rep("C", 10))

## ----message = F, eval = F, echo = T, warning = F-----------------------------
#  PVCA(
#       clin_data = pData(esSim),
#       clin_subjid = "sid",
#       gene_data = exprs(esSim),
#       batch.factors = c("grp", "Batch"))

## ----message = F, eval = T, echo = T, warning = F-----------------------------

statVisual(type = 'PVCA',
           clin_data = pData(esSim), 
           clin_subjid = "sid", 
           gene_data = exprs(esSim), 
           batch.factors = c("grp", "Batch"))


## ----message = F, eval = T, echo = T, warning = F-----------------------------
library(ggrepel)
library(limma)

library(ggrepel)
library(limma)

# load the simulated dataset
data(esSim)
print(esSim)

# expression levels
y = exprs(esSim)
print(dim(y))
print(y[1:2,])

# phenotype data
pDat = pData(esSim)
print(dim(pDat))
print(pDat)

# design matrix
design = model.matrix(~grp, data = pDat)
print(design)

options(digits = 3)

# Ordinary fit
fit <- lmFit(y, design)
fit2 <- eBayes(fit)

# get result data frame
resFrame = topTable(fit2,coef = 2, number = nrow(esSim))
print(dim(resFrame))
print(resFrame[1:2,])
resFrame$sigFlag  =  resFrame$adj.P.Val < 0.05

resFrame$probe  =  rownames(resFrame)
# make sure set NA to genes non-differentially expressed
resFrame$probe[which(resFrame$sigFlag == FALSE)] = NA

print(resFrame[1:2,])
print(table(resFrame$sigFlag, useNA = "ifany"))


## ----message = F, eval = F, echo = T, warning = F-----------------------------
#  Volcano(
#    resFrame = resFrame,
#    stats = 'logFC',
#    p.value = 'P.Value',
#    group = 'sigFlag',
#    rowname.var = 'probe',
#    point.size = 1)
#  

## ----message = F, eval = T, echo = T,warning = F------------------------------
statVisual(type = 'Volcano',
           resFrame = resFrame, 
           stats = 'logFC', 
           p.value = 'P.Value', 
           group = 'sigFlag', 
           rowname.var = 'probe', 
           point.size = 1)


## ----message = F, eval = T, echo = T, warning = F-----------------------------
library(dplyr)
library(gridExtra)

data(esSim)
print(esSim)

# expression data
dat = exprs(esSim)
print(dim(dat))
print(dat[1:2,])

# phenotype data
pDat = pData(esSim)
print(dim(pDat))
print(pDat[1:2,])
pDat$grp = factor(pDat$grp)

# choose the first probe which is over-expressed in cases
pDat$probe1 = dat[1,]

# check histograms of probe 1 expression in cases and controls
print(table(pDat$grp, useNA = "ifany"))


## ----message = F, eval = F, echo = T, warning = F-----------------------------
#  BoxROC(
#    data = pDat,
#    group = 'grp',
#    y = 'probe1',
#    point.size = 1)

## ----message = F, eval = T, echo = T, warning = F-----------------------------
statVisual(type = 'BoxROC', 
           data = pDat, 
           group = 'grp', 
           y = 'probe1', 
           point.size = 1)

## ----message = F, eval = T, echo = T, warning = F-----------------------------
library(dplyr)
library(tibble)
library(glmnet)


data(esSim)
print(esSim)

# expression data
dat = exprs(esSim)
print(dim(dat))
print(dat[1:2,])

# phenotype data
pDat = pData(esSim)
print(dim(pDat))
print(pDat[1:2,])

# feature data
fDat = fData(esSim)
print(dim(fDat))
print(fDat[1:2,])

# choose the first 6 probes (3 OE probes, 2 UE probes, and 1 NE probe)
pDat$probe1 = dat[1,]
pDat$probe2 = dat[2,]
pDat$probe3 = dat[3,]
pDat$probe4 = dat[4,]
pDat$probe5 = dat[5,]
pDat$probe6 = dat[6,]

print(pDat[1:2, ])

# check histograms of probe 1 expression in cases and controls
print(table(pDat$grp, useNA = "ifany"))

pDat$grp = factor(pDat$grp)


## ----message = F, eval = F, echo = T, warning = F-----------------------------
#  cv_glmnet_plot(x = as.matrix(pDat[, c(3:8)]),
#                 y = pDat$grp,
#                 family = "binomial")

## ----message = F, eval = T, echo = T, warning = F-----------------------------
statVisual(type = "cv_glmnet_plot",
           x = as.matrix(pDat[, c(3:8)]), 
           y = pDat$grp, 
           family = "binomial")

## ----message = F, eval = T, echo = T, warning = F-----------------------------
library(dplyr)
library(randomForest)
library(tibble)

data(esSim)
dat = exprs(esSim)
print(dim(dat))
print(dat[1:2,])

# phenotype data
pDat = pData(esSim)
print(dim(pDat))
print(pDat[1:2,])

# choose the first 6 probes (3 OE probes, 2 UE probes, and 1 NE probe)
pDat$probe1 = dat[1,]
pDat$probe2 = dat[2,]
pDat$probe3 = dat[3,]
pDat$probe4 = dat[4,]
pDat$probe5 = dat[5,]
pDat$probe6 = dat[6,]

print(pDat[1:2, ])

pDat$grp=factor(pDat$grp)


rf_m = randomForest(
  x = pDat[, c(3:8)], 
  y = pDat$grp, 
  importance = TRUE, proximity = TRUE
)


## ----message = F, eval = F, echo = T, warning = F-----------------------------
#  ImpPlot(rf_m)

## ----message = F, eval = T, echo = T, warning = F-----------------------------
statVisual(type = 'ImpPlot', rf_m)

## ----message = F, eval = T, echo = T, warning = F-----------------------------
library(GGally)

data(esSim)
dat = exprs(esSim)
print(dim(dat))
print(dat[1:2,])

# phenotype data
pDat = pData(esSim)
print(dim(pDat))
print(pDat[1:2,])

# choose the first 6 probes (3 OE probes, 2 UE probes, and 1 NE probe)
pDat$probe1 = dat[1,]
pDat$probe2 = dat[2,]
pDat$probe3 = dat[3,]
pDat$probe4 = dat[4,]
pDat$probe5 = dat[5,]
pDat$probe6 = dat[6,]

print(pDat[1:2, ])

pDat$grp=factor(pDat$grp)


ggpairs(data = pDat, 
	mapping = ggplot2::aes_string(color = 'grp'), 
        columns = c('probe1', 'probe5', 'probe6'), 
        upper = list(continuous = "cor", 
                     combo = "box_no_facet", 
                     discrete = "facetbar", 
                     na = "na"), 
        lower = list(continuous = "points", 
                     combo = "facethist", 
                     discrete = "facetbar", 
                     na = "na"), 
        diag = list(continuous = "densityDiag", 
                    discrete = "barDiag", 
                    na = "naDiag"), 
        xlab = 'X', 
	ylab = 'Y', 
	title = 'Title')

## ----message = F, eval = T, echo = T, warning = F-----------------------------

ggcorr(data = pDat[, c(3:8)], 
       method = 'pairwise', 
       label = TRUE, 
       label_round = 2, 
       label_size = 4)

