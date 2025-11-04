## ----knitr-opts, include=FALSE------------------------------------------------
knitr::opts_chunk$set(comment = "#",collapse = TRUE,results = "hold",
                      fig.align = "center",dpi = 120)

## ----load-pkgs, message=FALSE-------------------------------------------------
library(ggplot2)
library(cowplot)
library(Ternary)
library(Rtsne)
library(fastTopics)

## ----simulate-data------------------------------------------------------------
set.seed(1)
X <- simulate_toy_gene_data(n = 400,m = 40,k = 3,s = 1000)$X

## ----fit-multinom-topic-model, message=FALSE----------------------------------
fit <- fit_poisson_nmf(X,k = 3,numiter = 100,verbose = "none",
                       control = list(extrapolate = TRUE))
fit <- poisson2multinom(fit)

## ----plot-topic-proportions, fig.height=3, fig.width=3------------------------
par0 <- par(mar = c(0,0,0,0))
pdat <- as.data.frame(fit$L)
TernaryPlot(alab = "k1",blab = "k2",clab = "k3",grid.col = "skyblue",
            grid.minor.lines = 0)
TernaryPoints(pdat,pch = 21,col = "white",bg = "black",cex = 0.8)
par(par0)

## ----tsne-from-counts-1, fig.height=3, fig.width=3----------------------------
tsne1 <- Rtsne(X,2,pca = FALSE,normalize = FALSE)
colnames(tsne1$Y) <- c("d1","d2")
ggplot(as.data.frame(tsne1$Y),aes(x = d1,y = d2)) +
  geom_point(shape = 21,color = "white",fill = "black",size = 2) +
  theme_cowplot(font_size = 12)

## ----tsne-from-counts-2, fig.height=3, fig.width=3----------------------------
kmax <- factor(apply(fit$L,1,which.max))
ggplot(cbind(tsne1$Y,data.frame(kmax)),aes(x = d1,y = d2,fill = kmax)) +
  geom_point(shape = 21,color = "white",size = 2,show.legend = FALSE) +
  scale_fill_manual(values = c("dodgerblue","darkorange","darkblue")) +
  theme_cowplot(font_size = 12)

## ----tsne-from-loadings, fig.height=3, fig.width=3----------------------------
tsne2 <- Rtsne(fit$L,2,pca = FALSE,normalize = FALSE)
colnames(tsne2$Y) <- c("d1","d2")
ggplot(cbind(tsne2$Y,data.frame(kmax)),aes(x = d1,y = d2,fill = kmax)) +
  geom_point(shape = 21,color = "white",size = 2,show.legend = FALSE) +
  scale_fill_manual(values = c("dodgerblue","darkorange","darkblue")) +
  theme_cowplot(font_size = 12)

## ----pca-from-loadings, fig.height=3, fig.width=3-----------------------------
pca <- prcomp(fit$L,center = TRUE,scale = FALSE)
ggplot(cbind(pca$x,data.frame(kmax)),aes(x = PC1,y = PC2,fill = kmax)) +
  geom_point(shape = 21,color = "white",size = 2,show.legend = FALSE) +
  scale_fill_manual(values = c("dodgerblue","darkorange","darkblue")) +
  theme_cowplot(font_size = 12)

