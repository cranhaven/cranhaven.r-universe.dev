## ----initialsetup, include=FALSE----------------------------------------------
knitr::opts_chunk$set(cache=FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  library(data.table)
#  setwd("/mnt/scratch/quantgen/TENSOR_EVD/pipeline")
#  
#  PHENO <- read.csv("source/PHENO.csv")
#  GENO <- fread("source/GENO.csv", data.table=FALSE)
#  ECOV <- read.csv("source/ECOV.csv", row.names=1)
#  
#  # Select North region
#  PHENO <- PHENO[PHENO$region %in% 'North',]
#  PHENO$year_loc <- factor(as.character(PHENO$year_loc))
#  PHENO$genotype <- factor(as.character(PHENO$genotype))
#  
#  save(PHENO, file="data/pheno.RData")
#  
#  # Calculate the GRM
#  ID <- GENO[,1]
#  GENO <- as.matrix(GENO[,-1])
#  rownames(GENO) <- ID
#  X <- scale(GENO, center=TRUE, scale=FALSE)
#  KG <- tcrossprod(X)
#  KG <- KG[levels(PHENO$genotype),levels(PHENO$genotype)]
#  KG <- KG/mean(diag(KG))
#  
#  save(KG, file="data/GRM.RData")
#  
#  # Calculate the ERM
#  ECOV <- ECOV[,-grep("HI30_",colnames(ECOV))]
#  
#  KE <- tcrossprod(scale(ECOV))
#  KE <- KE[levels(PHENO$year_loc),levels(PHENO$year_loc)]
#  KE <- KE/mean(diag(KE))
#  
#  save(KE, file="data/ERM.RData")

## ----eval=FALSE---------------------------------------------------------------
#  JOBS <- expand.grid(nG = c(100,500,1000),
#                      nE = c(10,30,50),
#                      n = c(10000,20000,30000),
#                      alpha = c(0.90,0.95,0.98),
#                      replicate = 1:10)
#  dim(JOBS); head(JOBS)
#  #[1] 810   5
#  #    nG nE     n alpha replicate
#  #1  100 10 10000   0.9         1
#  #2  500 10 10000   0.9         1
#  #3 1000 10 10000   0.9         1
#  #4  100 30 10000   0.9         1
#  #5  500 30 10000   0.9         1
#  #6 1000 30 10000   0.9         1
#  
#  save(JOBS, file="/mnt/scratch/quantgen/TENSOR_EVD/pipeline/parms/JOBS1.RData")

## ----eval=TRUE----------------------------------------------------------------
out <- read.csv(url("https://raw.githubusercontent.com/MarcooLopez/tensorEVD/main/inst/extdata/results_simulation.txt"))
head(out[,1:7])
head(out[,8:12])  # results from the eigen function
head(out[,13:17])  # results from the tensorEVD function

## ----eval=T-------------------------------------------------------------------
source("https://raw.githubusercontent.com/MarcooLopez/tensorEVD/main/misc/functions.R")

## ----eval=T, fig.width = 7, fig.height = 5, fig.align='center'----------------
# Some data edits
out$alpha <- factor(100*out$alpha)

out$nG <- paste0("n[G]*' = '*",out$nG,"L")
out$nE <- paste0("n[E]*' = '*",out$nE,"L")
out$nG <- factor(out$nG, levels = unique(out$nG))
out$nE <- factor(out$nE, levels = unique(out$nE))

# Reshaping the data
measure <- c("time","Frobenius","CMD","nPC","pPC")
dat <- melt_data(out, id=c("nG","nE","n","alpha"),
                 measure=paste0(measure,"_"),
                 value.name=measure, variable.name="method")

color1 <- c('90%'="navajowhite2", '95%'="chocolate1", '98%'="red4")
color2 <- c(eigen="#E69F00", tensorEVD="#009E73", eigs="#56B4E9",
            trlan="#CC79A7", chol="#D55E00")

# Figure 1: Computation time ratio (eigen/tensorEVD)                  
dat0 <- out[out$alpha != "100",]
dat0$alpha <- factor(paste0(dat0$alpha,"%"))
dat0$ratio <- log10(dat0$time_eigen/dat0$time_tensorEVD)
dat0$n <- dat0$n/1000
breaks0 <- seq(1,4,by=1)

figure1 <- make_plot(dat0, type="line", x='n', y='ratio', group="alpha", 
                     group.label=NULL, facet="nG", facet2="nE", facet.type="grid",
                     xlab="Sample size (x1000)",
                     ylab="Computation time ratio (eigen/tensorEVD)",
                     group.color=color1, nSD=0, errorbar.size=0,
                     breaks.y=breaks0, labels.y=sprintf("%.f",10^breaks0),
                     scales="fixed")
#print(figure1)

## ----eval=T, fig.width = 7, fig.height = 5, fig.align='center'----------------
dat0 <- dat[dat$method %in% c("eigen","tensorEVD") & dat$alpha!="100",]
dat0$method <- factor(as.character(dat0$method))
dat0$alpha <- factor(as.character(dat0$alpha))

# Figure 2: Approximation accuracy using Frobenious norm
figure2 <- make_plot(dat0, x='alpha', y='Frobenius',
                     group="method", by="n", facet="nG", facet2="nE", facet.type="grid",
                     xlab=bquote(alpha~"x100% of variance of K"),
                     ylab=expression("Frobenius norm ("~abs(abs(K-hat(K)))[F]~")"),
                     by.label="Sample size", breaks.y=seq(0,500,by=100),
                     group.color=color2, rect.by.height=-0.05, ylim=c(0,NA))
#print(figure2)

## ----eval=T, fig.width = 7, fig.height = 5, fig.align='center'----------------
figure3 <- make_plot(dat0, x='alpha', y='pPC',
                     group="method", by="n", facet="nG", facet2="nE", facet.type="grid",
                     xlab=bquote(alpha~"x100% of variance of K"),
                     ylab="Number of eigenvectors/rank", by.label="Sample size",
                     group.color=color2, rect.by.height=-0.05,
                     hline=1, hline.color="red2", ylim=c(0,NA))
#print(figure3)

## ----eval=FALSE---------------------------------------------------------------
#  JOBS <- expand.grid(alpha = c(0.90,0.95,0.98,1.00),
#                      replicate = 1:5)
#  dim(JOBS); head(JOBS)
#  #[1] 20  2
#  #  alpha replicate
#  #1  0.90         1
#  #2  0.95         1
#  #3  0.98         1
#  #4  1.00         1
#  #5  0.90         2
#  #6  0.95         2
#  
#  save(JOBS, file="/mnt/scratch/quantgen/TENSOR_EVD/pipeline/parms/JOBS2.RData")

## ----eval=FALSE---------------------------------------------------------------
#  JOBS <- expand.grid(trait = c("yield","anthesis","silking","ASI"),
#                      method = c("eigen","tensorEVD"),
#                      alpha = c(0.90,0.95,0.98,1.00),
#                      replicate = 1:5)
#  JOBS <- JOBS[-which(JOBS$alpha==1.00 & JOBS$method=="tensorEVD"),]
#  dim(JOBS); head(JOBS)
#  #[1] 140   4
#  #     trait    method alpha replicate
#  #1    yield     eigen   0.9         1
#  #2 anthesis     eigen   0.9         1
#  #3  silking     eigen   0.9         1
#  #4      ASI     eigen   0.9         1
#  #5    yield tensorEVD   0.9         1
#  #6 anthesis tensorEVD   0.9         1
#  
#  save(JOBS, file="/mnt/scratch/quantgen/TENSOR_EVD/pipeline/parms/JOBS3.RData")

## ----eval=FALSE---------------------------------------------------------------
#  setwd("/mnt/scratch/quantgen/TENSOR_EVD/pipeline")
#  load("parms/JOBS3.RData")
#  prefix <- "output/genomic_prediction/ANOVA"
#  
#  out <- c()
#  for(k in 1:nrow(JOBS))
#  {
#    trait <- as.vector(JOBS[k,"trait"])
#    method <- as.vector(JOBS[k,"method"])
#    alpha <- as.vector(JOBS[k,"alpha"])
#    replicate <- as.vector(JOBS[k,"replicate"])
#  
#    suffix <- paste0(trait,"/",method,"/alpha_",100*alpha,"/rep_",replicate,"/VC.RData")
#    filename <- paste0(prefix,"/",suffix)
#    if(file.exists(filename)){
#      load(filename)
#      out <- rbind(out, VC)
#    }else{
#      message("File not found: '",suffix,"'")
#    }
#  }

## ----eval=TRUE----------------------------------------------------------------
out <- read.csv(url("https://raw.githubusercontent.com/MarcooLopez/tensorEVD/main/inst/extdata/results_ANOVA.txt"))
head(out)

## ----eval=T, fig.width = 6.5, fig.height = 4.5, fig.align='center'------------
out$alpha <- factor(paste0(100*out$alpha,"%"), levels=c("100%","98%","95%","90%"))
out$source <- factor(out$source, levels=c("G","E","GE","Error"))

trait <- c("yield", "anthesis", "silking", "ASI")[1]

myfun <- function(x) sprintf('%.3f', x)

# Figure 4: Phenotypic variance of yield
dat <- out[out$trait==trait,]
figure4 <- make_plot(dat, x='alpha', y='mean', SD="SD",
                     group="method", facet="source",
                     xlab=bquote(alpha~"x100% of variance of K"),
                     ylab=paste0("Proportion of variance of ",trait),
                     group.color=color2, scales="free_y",
                     ylabels=myfun, text=myfun, ylim=c(0,NA))
#print(figure4)

## ----eval=FALSE---------------------------------------------------------------
#  setwd("/mnt/scratch/quantgen/TENSOR_EVD/pipeline")
#  load("data/pheno.RData")
#  table(PHENO$CV_10fold)
#  #   1    2    3    4    5    6    7    8    9   10
#  #6180 6277 6246 5785 6160 5858 5492 5660 5436 5975

## ----eval=FALSE---------------------------------------------------------------
#  JOBS <- expand.grid(trait = c("yield","anthesis","silking","ASI"),
#                      method = c("eigen","tensorEVD"),
#                      alpha = c(0.90,0.95,0.98),
#                      fold = 1:10)
#  dim(JOBS); head(JOBS)
#  #[1] 240   4
#  #     trait    method alpha fold
#  #1    yield     eigen   0.9    1
#  #2 anthesis     eigen   0.9    1
#  #3  silking     eigen   0.9    1
#  #4      ASI     eigen   0.9    1
#  #5    yield tensorEVD   0.9    1
#  #6 anthesis tensorEVD   0.9    1
#  
#  save(JOBS, file="/mnt/scratch/quantgen/TENSOR_EVD/pipeline/parms/JOBS4.RData")

## ----eval=FALSE---------------------------------------------------------------
#  setwd("/mnt/scratch/quantgen/TENSOR_EVD/pipeline")
#  source("https://raw.githubusercontent.com/MarcooLopez/tensorEVD/main/misc/functions.R")
#  load("parms/JOBS4.RData")
#  prefix <- "output/genomic_prediction/10F_CV"
#  
#  dat <- c()
#  for(trait in levels(JOBS$trait)){
#    for(method in levels(JOBS$method)){
#      for(alpha in unique(JOBS$alpha)){
#        out0 <- c()
#        for(fold in unique(JOBS$fold)){
#          suffix <- paste0(trait,"/",method,"/alpha_",100*alpha,"/results_fold_",fold,".RData")
#          filename <- paste0(prefix,"/",suffix)
#          if(file.exists(filename)){
#            load(filename)
#            out0 <- rbind(out0, out)
#          }else{
#            message("File not found: '",suffix,"'")
#          }
#        }
#        tmp <- get_corr(out0, by="year_loc")
#        dat <- rbind(dat, data.frame(trait,method,alpha,tmp))
#      }
#    }
#  }
#  
#  # Reshaping the data
#  dat$trait <- factor(dat$trait, levels=levels(JOBS$trait))
#  out <- reshape2::dcast(dat, trait+alpha+year_loc+nRecords~method, value.var="correlation")
#  tmp <- reshape2::dcast(dat, trait+alpha+year_loc+nRecords~method, value.var="SE")[,levels(JOBS$method)]
#  colnames(tmp) <- paste0(colnames(tmp),".SE")
#  out <- data.frame(out, tmp)

## ----eval=TRUE----------------------------------------------------------------
out <- read.csv(url("https://raw.githubusercontent.com/MarcooLopez/tensorEVD/main/inst/extdata/results_10F_CV.txt"))
head(out)

## ----eval=T, fig.width = 6.0, fig.height = 6.5, fig.align='center'------------
out$trait <- factor(out$trait, levels=unique(out$trait))
out$alpha <- factor(paste0(100*out$alpha,"%"), levels=c("98%","95%","90%"))

# Figure 5: Within environment prediction correlation
rg <- range(c(out$eigen,out$tensorEVD))
if(requireNamespace("ggplot2", quietly=TRUE)){
  figure5 <-  ggplot2::ggplot(out, ggplot2::aes(tensorEVD, eigen)) +
              ggplot2::geom_abline(color="gray70", linetype="dashed") +
              ggplot2::geom_point(fill="#56B4E9", shape=21, size=1.4) +
              ggplot2::facet_grid(trait ~ alpha) +
              ggplot2::theme_bw() + ggplot2::xlim(rg) + ggplot2::ylim(rg)
}

#print(figure5)

