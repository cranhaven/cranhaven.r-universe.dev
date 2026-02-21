## prioGene: Prioritize candidate genes for complex non-communicable diseases

## :writing_hand: Authors
Erqiang Hu

College of Bioinformatics Science and Technology, Harbin Medical University

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/prioGene?color=green)](https://cran.r-project.org/package=prioGene)
![](https://cranlogs.r-pkg.org/badges/grand-total/prioGene?color=green)
![](https://cranlogs.r-pkg.org/badges/prioGene?color=green)
![](https://cranlogs.r-pkg.org/badges/last-week/prioGene?color=green)

## :arrow\_double\_down: Installation

Get the development version from github:

```r
if(!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")
devtools::install_github("huerqiang/prioGene")
```
Or  the released version from CRAN:

``` r
install.packages("prioGene")
```
-------

## Common operations on prioGene
```r
library(prioGene)
```

### 1. Construction of disease  related networks

The function `deal_net` could get a disease-related network by retaining disease-causing genes and their One-step interaction neighbors in a human biological network.
The parameter `net` means a human biological network, a matrix of two columns. The parameter `dise_gene` means a one-column-matrix of gene symbols obtained from the OMIM database or other disease-related databases. They need to be provided by the users.  We provide examples separately in the package: `prioGene::net` and `prioGene::dise_gene`.

```r
net_disease <- deal_net(net,dise_gene)
```

###  2.  Calculation of network weights

These five functions form a pipeline to weight the nodes and edges of the network based on functional information. GO function annotation information comes from `org.Hs.eg.db`.

```r
genes_mat <- get_gene_mat(net_disease)
terms_mat <- get_term_mat(net_disease)
net_disease_term <- get_net_disease_term(genes_mat,net_disease)
node_weight <- get_node_weight(genes_mat)
edge_weight <- get_edge_weight(net_disease_term,terms_mat)
```

### 3.  Prioritization of candidate genes

The prioritization of candidate genes was performed based on disease risk scores of each gene obtained from an iteration process considering disease risks transferred between genes.

```r
R_0<- get_R_0(dise_gene,node_weight,f=1)
result <- get_R(node_weight, net_disease_term, bet = 0.5, R_0 = R_0, threshold = 10^(-9))
```

