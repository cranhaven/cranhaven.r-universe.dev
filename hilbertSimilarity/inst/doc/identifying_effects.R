## ----echo=FALSE----------------------------------------------------------
library(knitr)
opts_chunk$set(warning=FALSE,
               fig.width=8,
               fig.height=8,
               fig.retina=1,
               fig.keep='high',
               fig.align='center')

## ------------------------------------------------------------------------
library(hilbertSimilarity)

## ------------------------------------------------------------------------
library(bodenmiller)
data(refPhenoMat)
data(untreatedPhenoMat)
data(refFuncMat)
data(untreatedFuncMat)
data(refAnnots)
refAnnots$Treatment <- 'reference'
data(untreatedAnnots)
fullAnnots <- rbind(refAnnots[,names(untreatedAnnots)],
                    untreatedAnnots)
fullAnnots$Treatment <- factor(fullAnnots$Treatment)
fullAnnots$Treatment <- relevel(fullAnnots$Treatment,'reference')
refMat <- cbind(refPhenoMat,refFuncMat)
untreatedMat <- cbind(untreatedPhenoMat,
                      untreatedFuncMat)
fullMat <- rbind(refMat,untreatedMat)
fullMat <- apply(fullMat,2,function(x) {
    x[x<0] <- 0
    return(x)
  }
)

## ----echo=FALSE----------------------------------------------------------
nCellTreats <- with(fullAnnots,table(Cells,Treatment))
pCellTreats <- apply(nCellTreats,2,function(x) x/sum(x))
kable(nCellTreats)

## ------------------------------------------------------------------------
# compute the 
nbins <- 3
cuts <- make.cut(fullMat,
                 n=nbins+1,
                 count.lim=40)

## ------------------------------------------------------------------------
cutFullMat <- do.cut(fullMat,cuts,type='combined')

## ------------------------------------------------------------------------
library(entropy)
miFullMat <- matrix(NA,nrow=ncol(fullMat),ncol = ncol(fullMat) )
for (i in seq(ncol(fullMat)-1)) {
  for (j in seq(i+1,ncol(fullMat))) {
    cur.tbl <- table(cutFullMat[,i],cutFullMat[,j])
    nent <- 1-mi.empirical(cur.tbl)/entropy.empirical(cur.tbl)
    miFullMat[i,j] <- miFullMat[j,i] <- nent
  }
}
dimnames(miFullMat) <- list(colnames(fullMat),colnames(fullMat))
hcFullMat <- hclust(as.dist(miFullMat))

## ------------------------------------------------------------------------
hc <- do.hilbert(cutFullMat[,rev(hcFullMat$order)],2)

## ------------------------------------------------------------------------
library(reshape2)
library(dplyr)
library(ggplot2)
treatment.table <- with(fullAnnots,
                        table(hc,Treatment))
treatment.pc <- apply(treatment.table,2,function(x) x/sum(x))
av <- andrewsProjection(t(treatment.pc),breaks=40)
treatment.proj <- t(treatment.pc) %*% t(av$freq)
melt(treatment.proj) %>% 
    rename(AndrewsIndex=Var2) %>% 
    mutate(AndrewsIndex=av$i[AndrewsIndex]) %>% 
    ggplot(aes(x=AndrewsIndex,y=value))+
    geom_line(aes(group=Treatment,color=Treatment))+
    theme_light(base_size=16)+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())

## ------------------------------------------------------------------------
# total cells per Hilbert index per treatment
treatment.table <- with(fullAnnots,
                        table(hc,Treatment))
# minimum number of cells
lim <- 40
# number of bootstraps
N <- 400
# significant fold change
flim <- 2
# limit of significance
p <- 0.95
# bins with enough cells
boot.ind <- rownames(treatment.table)[which(rowSums(treatment.table)>=lim)]
# number of cells in each bootstrap
ncells <- floor(min(colSums(treatment.table[boot.ind,]))/1000)*1000

## ------------------------------------------------------------------------
library(abind)
boot.mat <- lapply(seq(1,N),function(x) {
    if (x%/%100==x/100) cat(x,'\n')
    # bootstrap hilbert indices
    cur.boot <- sample(which(hc %in% boot.ind),
                       size=ncells,
                       replace=TRUE)
    return(table(factor(hc[cur.boot],levels=boot.ind),
                 droplevels(fullAnnots[cur.boot,'Treatment'])))
  }
)

boot.mat <- abind(boot.mat,along=3)

boot.log <- log2(boot.mat)
boot.fold <- sweep(boot.log[,-1,],c(1,3),boot.log[,1,])
boot.sign <- sign(boot.fold)
boot.sign[abs(boot.fold)<log2(flim)] <- 0
boot.res <- apply(boot.sign,c(1,2),function(x) table(sign(x))[c('-1','0','1')])
boot.res <- aperm(boot.res,c(2,1,3))
boot.res[is.na(boot.res)] <- 0
dimnames(boot.res)[[2]] <- c('-1','0','1')

## ------------------------------------------------------------------------
sig.res <- apply(boot.res[,c('-1','1'),]>(N*p),c(1,3),any)
sum.sig.res <- cbind(colSums(sig.res),
                     colSums(treatment.table[boot.ind[rowSums(sig.res)>0],colnames(sig.res)]))

p.cells.sig.res <- lapply(colnames(sig.res),
                          function(cur.cl) {
                              x <- treatment.table[boot.ind,cur.cl]
                              return(sum(x[sig.res[,cur.cl]])/sum(x))
                            }
                          )
names(p.cells.sig.res) <- colnames(sig.res)
sum.sig.res <- cbind(sum.sig.res,
                     round(100*unlist(p.cells.sig.res),0))
colnames(sum.sig.res) <- c('Number of indices',
                           'Number of Significant cells',
                           'Percent Significant Cells')
kable(sum.sig.res)

## ------------------------------------------------------------------------
kable(treatment.table[boot.ind[sig.res[,'BCR/FcR-XL']],])

## ------------------------------------------------------------------------
cur.treatment <- 'BCR/FcR-XL'
sig.cells <- with(fullAnnots,table(droplevels(Cells[hc %in% boot.ind[sig.res[,cur.treatment]]])))
total.sig.cells <- with(fullAnnots,table(droplevels(Cells[Cells %in% names(sig.cells) &
                                                       hc %in% boot.ind])))
treatment.sig.cells <- with(fullAnnots,
                            table(droplevels(Cells[hc %in% boot.ind[sig.res[,cur.treatment]] &
                                                     Treatment==cur.treatment])))
treatment.total.cells <- with(fullAnnots,
                              table(droplevels(Cells[Cells %in% names(sig.cells) &
                                                       hc %in% boot.ind &
                                                       Treatment==cur.treatment])))
sum.sig.cells <- cbind(sig.cells,
                       total.sig.cells,
                       round(100*sig.cells/total.sig.cells),
                       treatment.sig.cells,
                       treatment.total.cells,
                       round(100*treatment.sig.cells/treatment.total.cells))

colnames(sum.sig.cells) <- c('Number of Significant Cells',
                             'Total number of Cells',
                             'Percent of Total',
                             paste('Number of Signigicant Cells in',cur.treatment),
                             paste('Number of Cells in',cur.treatment),
                             paste('Percent of',cur.treatment))
kable(sum.sig.cells)

