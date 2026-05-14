## -----------------------------------------------------------------------------
library(scLink)
count = readRDS(system.file("extdata", "example.rds", package = "scLink"))
genes = readRDS(system.file("extdata", "genes.rds", package = "scLink"))

## -----------------------------------------------------------------------------
count.norm = sclink_norm(count, scale.factor = 1e6, filter.genes = FALSE, gene.names = genes)

## ----eval = FALSE-------------------------------------------------------------
#  count.norm = sclink_norm(count, scale.factor = 1e6, filter.genes = TRUE, n = 500)

## ---- message=FALSE-----------------------------------------------------------
networks = sclink_net(expr = count.norm, ncores = 1, lda = seq(0.5, 0.1, -0.05))

## -----------------------------------------------------------------------------
networks$cor[1:3,1:3]

## -----------------------------------------------------------------------------
net1 = networks$summary[[1]]
names(net1)
### adjacency matrix
net1$adj[1:3,1:3]
### concentration matrix
net1$Sigma[1:3,1:3]
### BIC 
net1$bic
### number of edges
net1$nedge
### regularization parameter lambda
net1$lambda

## ----eval = FALSE-------------------------------------------------------------
#  corr = sclink_cor(expr = count.norm, ncores = 1)

