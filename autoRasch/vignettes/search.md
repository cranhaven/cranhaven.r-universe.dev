---
title: "autoRasch: The Semi-automated Rasch Analysis"
# output: 
#   html_vignette:
#     keep_md: true
#   pdf_document: default
# output: 
#   pdf_document:
#     latex_engine: pdflatex
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{autoRasch: The Semi-automated Rasch Analysis}
  %\VignetteEngine{knitr::knitr}
  %\VignetteEncoding{UTF-8}
---



```{}
# install.packages("remotes")
remotes::install_github("fwijayanto/autoRasch", build_manual = TRUE, build_vignettes = TRUE)
```


```r
library(autoRasch)
```


### Computing the criteron score (IPOQ-LL/IPOQ-LL-DIF)

Utilizing the generalized partial credit model (GPCM) and the generalized partial credit model with DIF (GPCM-DIF), we develop a score as a criterion to judge the quality of an itemset within an original survey, called the In-plus-out-of-questionnaire log-likelihood (IPOQ-LL) and In-plus-out-of-questionnaire log-likelihood with DIF (IPOQ-LL-DIF), respectively.

For example, we have a 9-item original survey and we want to examine how good to estimate persons' abilities using only item~7~, item~8~, and item~9~. To compute the IPOQ-LL score we simply run


```r
ipoqll_score <- compute_score(short_poly_data, incl_set = c(7:9), type = "ipoqll")
#> [1] "...done!"
#> [1] "...done!"
summary(ipoqll_score)
#> 
#> Score of the itemsets: 
#> 
#> IQ-LL:  -197.2905
#> OQ-LL:  -1325.928
#> IPOQ-LL:  -1523.218
```

Furthermore, to compute multiple IPOQ-LL scores of several itemsets simultanously, we simply use

```{}
ipoqll_scores <- compute_scores(short_poly_data, incl_sets = rbind(c(1:3),c(7:9)), type = "ipoqll", cores = 2)
View(ipoqll_scores[,1:12])
```


### Semi-automated Rasch analysis by searching the maximum of the (IPOQ-LL/IPOQ-LL-DIF) score

The IPOQ-LL obtains by totalling the IQ-LL and OQ-LL. Changing `type = ipoqlldif` means the IPOQ-LL-DIF score is computed, by considering the DIF effects, instead of the IPOQ-LL. This log-likelihood is a score for model comparison, which means that there are more items combinations to be compared in order to obtain the maximum. Hence, we conduct the semi-automated Rasch analysis using the IPOQ-LL score by running

```{}
stepwise_res <- stepwise_search(short_poly_data, criterion = "ipoqll", cores = 2, 
                                isTracked = TRUE)
```

This `stepwise_search()` aims to search the maximum IPOQ-LL score over all items combinations possible. This maximum score correspond to the "best" itemset according to the semi-automated Rasch analysis. Therefore, to speed up the search, we implements parallelization in every step of the stepwise selection search. If `isTracked = TRUE` the function prints the combination of items which returns the highest IPOQ-LL score at every step.

Obtaining the analysis result, we could plot

```{}
plot_search(stepwise_res, type="l")
```

The plot show the highest IPOQ-LL scores in every possible number of items in the itemsets. The numbers in the plot represent the item(s) which are removed (and added) to obtained the plotted scores, compared to the previous step. For instance, starting with full items, the highest IPOQ-LL score for itemset consisting with 8 items is obtained by removing item~1~. Subsequently, the highest IPOQ-LL score for itemset consisting with 7 items is obtained by removing item~2~.
