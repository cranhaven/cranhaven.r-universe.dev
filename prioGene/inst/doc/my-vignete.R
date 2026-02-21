## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ---- eval=FALSE, message=FALSE, warning=FALSE--------------------------------
#  if(!requireNamespace("devtools", quietly = TRUE))
#      install.packages("devtools")
#  devtools::install_github("huerqiang/prioGene")

## ---- eval=FALSE, message=FALSE, warning=FALSE--------------------------------
#  install.packages("prioGene")

## -----------------------------------------------------------------------------
library(prioGene)

## -----------------------------------------------------------------------------
net_disease <- deal_net(net,dise_gene)

## -----------------------------------------------------------------------------
genes_mat <- get_gene_mat(net_disease)
terms_mat <- get_term_mat(net_disease)
net_disease_term <- get_net_disease_term(genes_mat,net_disease)
node_weight <- get_node_weight(genes_mat)
edge_weight <- get_edge_weight(net_disease_term,terms_mat)

## -----------------------------------------------------------------------------
R_0<- get_R_0(dise_gene,node_weight,f=1)
result <- get_R(node_weight, net_disease_term, bet = 0.5, R_0 = R_0, threshold = 10^(-9))

## -----------------------------------------------------------------------------
sessionInfo()

