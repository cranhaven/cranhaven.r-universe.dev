## ---- results='hide', message=FALSE-------------------------------------------

library(insectDisease)
library(plyr)
library(dplyr)
library(corrplot)
knitr::opts_chunk$set(fig.width=6, fig.height=6) 
oldpar <- par(mar=c(4,4,0.5,0.5))


## -----------------------------------------------------------------------------

data(nematode)
data(viruses)
data(nvpassoc)
data(negative)


## -----------------------------------------------------------------------------

head(nematode)


## -----------------------------------------------------------------------------

data(hosts)
table(hosts$InsectStatus) 


## -----------------------------------------------------------------------------

sort(table(nematode$HostSpecies), decreasing=TRUE)[1:5]


## -----------------------------------------------------------------------------

sort(table(viruses$HostSpecies), decreasing=TRUE)[1:5]


## -----------------------------------------------------------------------------

nema2 <- nematode %>% 
  dplyr::group_by(HostSpecies) %>% 
  dplyr::summarise(uniqueNema=length(unique(PathogenSpecies)))

par(mar=c(4,4,0.5,0.5))
hist(nema2$uniqueNema, col='dodgerblue', 
  main='', ylab='Frequency', 
  xlab='Unique nematode species per host species')


viru2 <- viruses %>% 
  dplyr::group_by(HostSpecies) %>% 
  dplyr::summarise(uniqueViru=length(unique(PathogenSpecies)))

par(mar=c(4,4,0.5,0.5))
hist(viru2$uniqueViru, col='dodgerblue', 
  main='', ylab='Frequency', 
  xlab='Unique viruses per host species')


## -----------------------------------------------------------------------------

nemaViru <- dplyr::full_join(nema2, viru2, by='HostSpecies')

plot(y=nemaViru$uniqueNema, 
  x=nemaViru$uniqueViru, 
  xlab='Unique viruses per host', 
  ylab='Unique nematodes per host', 
  pch=16, las=1,
  col=adjustcolor('dodgerblue',0.25)
)


## ----eval=TRUE----------------------------------------------------------------

#' subset the edwip data objects
#' 
#' @param dat the edwip data.frame
#' 
#' @return subset data

getSubset <- function(dat){
  nms <- c('PathogenSpecies', 'HostSpecies', 'Group')
  tmp <- dat[,which(colnames(dat) %in% nms)]
  tmp <- tmp[,order(colnames(tmp))]
  return(tmp)
}

## -----------------------------------------------------------------------------
edwip3 <- rbind(
  getSubset(nematode), 
  getSubset(viruses), 
  getSubset(nvpassoc)
)   
edwip3$interaction <- 1


## -----------------------------------------------------------------------------

neg <- getSubset(negative)
neg$interaction <- 0


## -----------------------------------------------------------------------------

edwip4 <- rbind(edwip3, neg)
edwip4$Group[which(edwip4$Group == 'Mollicutes')] <- 'Bacteria'
edwip4$Group[which(edwip4$Group == 'Viruses')] <- 'Virus'


## -----------------------------------------------------------------------------



bubble <- edwip4 %>%
  dplyr::group_by(Group) %>% 
  dplyr::summarise(n=length(HostSpecies), 
    nHosts=length(unique(HostSpecies)), 
    nPaths=length(unique(PathogenSpecies)), 
    positives=sum(interaction==1, na.rm=TRUE), 
    negatives=sum(interaction==0, na.rm=TRUE)
  ) 

colorz <- c('#E5FCC2', '#9DE0AD', '#45ADA8', '#547980', '#594F4F')

par(mar=c(4,6,0.5,0.5))
plot(x=1:2, 
  y=1:2, type='n', las=1,
  ylim=c(0,6), 
  xaxt='n', yaxt='n',
  xlim=c(0.75,2.25), 
  xlab='Host-parasite interaction', 
  ylab='')

scul <- sqrt(c(unlist(bubble[,6]), unlist(bubble[,5])))
scul <- 20*(scul / max(scul))

points(x=sort(rep(c(1,2),5)), 
  y=rep(1:5, 2), 
  col=1,
  bg=adjustcolor(colorz, 0.75), 
  cex=scul,
  pch=21)

text(x=c(1.2,1.35,1.6), 
  y=rep(5.25,3), cex=0.85, adj=0,
  c('Hosts', 'Pathogens', 'Interactions'))

text(x=rep(1.2, 5), 
  y=1:5, cex=1, adj=0,
  paste(bubble$nHosts))

text(x=rep(1.4, 5), 
  y=1:5, cex=1, adj=0,
  paste(bubble$nPaths))

text(x=rep(1.6, 5), 
  y=1:5, cex=1, adj=0,
  paste(bubble$n))

axis(1, at = c(1,2), 
  labels=c("Negative", "Positive")
)

axis(2, at = 1:5, las=1,
  labels=bubble$Group
)


## ----eval=TRUE----------------------------------------------------------------

tmp <- as.data.frame.matrix(
  with(edwip4[which(edwip4$interaction==1),], 
    table(HostSpecies, Group)
  )
)

par(mar=c(2,2,0.5,0.5))

corrplot::corrplot.mixed(cor(tmp, method='spearman'), 
	lower='number', upper='ellipse',
	tl.col=1, 
  insig='label_sig',
  number.cex=1.5, 
	addgrid.col=grey(0.5,0.5), 
	mar=c(0,0,1,0)
)

par(oldpar) 

