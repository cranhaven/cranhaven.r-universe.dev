## ----setup, include = FALSE---------------------------------------------------

## -----------------------------------------------------------------------------
#create the seed dataset
n <- 1024
data <- matrix(c(rep(0,n),rep(1,n)),ncol=1)

#add dimensions
for(i in 2:5) data <- cbind(c(rep(0,dim(data)[1]), rep(1, dim(data)[1])),rbind(data,data))

#scatter the points to clusters
set.seed(1)
data <- data + 0.2*rnorm(dim(data)[1]*dim(data)[2])
colnames(data) <- paste0('V',1:5)

## ---- fig.show='hold'---------------------------------------------------------
plot(data, pch=19, col=rgb(0,0,0,0.2))

## ---- fig.show='hold'---------------------------------------------------------
plot(data.frame(prcomp(data)$x), pch='.', col=rgb(0,0,0,0.2))

## -----------------------------------------------------------------------------
set.seed(1)
map <- EmbedSOM::SOM(data, xdim=24, ydim=24)

## ----eval=FALSE---------------------------------------------------------------
#  fs <- FlowSOM::ReadInput(as.matrix(data.frame(data)))
#  fs <- FlowSOM::BuildSOM(fsom=fs, xdim=24, ydim=24)

## -----------------------------------------------------------------------------
e <- EmbedSOM::EmbedSOM(data=data, map=map)

## ----eval=FALSE---------------------------------------------------------------
#  e <- EmbedSOM::EmbedSOM(fsom=fs)

## -----------------------------------------------------------------------------
e <- EmbedSOM::EmbedSOM(data=data, map=map, smooth=2, k=10)

## -----------------------------------------------------------------------------
head(e)

## ---- fig.show='hold'---------------------------------------------------------
plot(e, pch=19, cex=.5, col=rgb(0,0,0,0.2))

## ---- fig.show='hold'---------------------------------------------------------
EmbedSOM::PlotEmbed(e, pch=19, cex=.5, nbin=100)

## ---- fig.show='hold'---------------------------------------------------------
EmbedSOM::PlotEmbed(e, data=data, pch=19, cex=.5, alpha=0.3, value=1)

## ---- fig.show='hold'---------------------------------------------------------
EmbedSOM::PlotEmbed(e, data=data, pch=19, cex=.5, alpha=0.3, red=2, green=4)

## ---- fig.show='hold'---------------------------------------------------------
n_clusters <- 32
hcl <- hclust(dist(map$codes))
metaclusters <- cutree(hcl,n_clusters)[map$mapping[,1]]

EmbedSOM::PlotEmbed(e, pch=19, cex=.5, clust=metaclusters, alpha=.3)

## ---- fig.show='hold'---------------------------------------------------------
colors <- topo.colors(24*24, alpha=.3)[Matrix::invPerm(hcl$order)[map$mapping[,1]]]

EmbedSOM::PlotEmbed(e, pch=19, cex=.5, col=colors)

## ---- fig.show='hold'---------------------------------------------------------
EmbedSOM::PlotGG(e, data=data) + ggplot2::geom_hex(bins=80)

