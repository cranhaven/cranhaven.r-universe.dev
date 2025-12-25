## ----echo=FALSE----------------------------------------------------------
library(knitr)
opts_chunk$set(warning=FALSE,
               fig.width=8,
               fig.height=6,
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
  qx <- quantile(x,c(0.005,0.995))
  x[x<qx[1]] <- qx[1]
  x[x>qx[2]] <- qx[2]
  return(x)
})

## ----echo=FALSE----------------------------------------------------------
library(dplyr)
library(tidyr)
fullAnnots %>% 
    count(Cells,Treatment) %>% 
    group_by(Treatment) %>% 
    mutate(n=n/sum(n),
           n=round(100*n,2)) %>% 
    spread(Treatment,n)

## ------------------------------------------------------------------------
nbins <- 2
cuts <- make.cut(fullMat,
                 n=nbins+1,
                 count.lim=40)

## ----fig.height=8,fig.width=8--------------------------------------------
show.cut(cuts)

## ------------------------------------------------------------------------
cutFullMat <- do.cut(fullMat,cuts,type='combined')

## ------------------------------------------------------------------------
library(entropy)
miFullMat <- matrix(0,nrow=ncol(fullMat),ncol = ncol(fullMat) )
for (i in seq(ncol(fullMat)-1)) {
  for (j in seq(i+1,ncol(fullMat))) {
    cur.tbl <- table(cutFullMat[,i],cutFullMat[,j])
    nent <- 1-mi.empirical(cur.tbl)/entropy.empirical(cur.tbl)
    miFullMat[i,j] <- miFullMat[j,i] <- nent
  }
}
dimnames(miFullMat) <- list(colnames(fullMat),colnames(fullMat))
hcFullMat <- hclust(as.dist(miFullMat))
plot(hcFullMat)

## ------------------------------------------------------------------------
col.order <- hcFullMat$labels[rev(hcFullMat$order)]
hc <- do.hilbert(cutFullMat[,col.order],nbins)

## ------------------------------------------------------------------------
treatment.table <- with(fullAnnots,
                        table(hc,Treatment))
treatment.pc <- apply(treatment.table,2,function(x) x/sum(x))

## ------------------------------------------------------------------------
av <- andrewsProjection(t(treatment.pc),breaks=30)

## ------------------------------------------------------------------------
treatment.proj <- t(treatment.pc) %*% t(av$freq)

## ------------------------------------------------------------------------
library(ggplot2)
library(reshape2)
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
proj <- hilbertProjection(hc,target = 2)

## ------------------------------------------------------------------------
fullProj <- fullAnnots %>% 
    mutate(HilbertIndex=hc) %>% 
    group_by_at(vars(one_of(colnames(fullAnnots),'HilbertIndex'))) %>% 
    count() %>%
    bind_cols(as.data.frame(proj[.$HilbertIndex+1,]))
fullProjCount <- fullProj %>% 
    ungroup() %>% 
    count(HilbertIndex,Treatment) %>% 
    arrange(desc(n))
kable(head(fullProjCount))

## ----fig.height=8--------------------------------------------------------
fullProj %>% 
    group_by(Treatment) %>% 
    mutate(PC=n/sum(n)) %>% 
    ggplot(aes(x=V1,y=V2))+
    geom_tile(aes(fill=PC),
              width=24,
              height=24)+
    facet_wrap(~Treatment)+
    scale_fill_gradient(low='grey80',high='blue')+
    theme_light(base_size=16)+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())

## ----fig.width=6,fig.height=6--------------------------------------------
heatmap(log10(treatment.pc),
        scale = 'none',
        Colv = NA,
        Rowv = NA,
        labRow = NA,
        col = colorRampPalette(c('white',blues9))(256),
        margin = c(12,1))

## ------------------------------------------------------------------------
treatment.dist <- js.dist(t(treatment.table))
treatment.hc <- hclust(treatment.dist)

## ----fig.width=6,fig.height=8--------------------------------------------
heatmap(log10(treatment.pc),
        scale = 'none',
        Colv = as.dendrogram(treatment.hc),
        Rowv = NA,
        labRow = NA,
        col = colorRampPalette(c('white',blues9))(256),
        margin = c(12,2))

