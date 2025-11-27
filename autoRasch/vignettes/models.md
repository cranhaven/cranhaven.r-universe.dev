---
title: "autoRasch: Working with the models"
author: "John Doe"
# output: 
#   html_vignette:
#     keep_md: true
#   pdf_document: default
# output: 
#   pdf_document:
#     latex_engine: pdflatex
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{autoRasch: Working with the models}
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


In this autoRasch package four models are implemented, i.e., the PCM, the GPCM, the PCM-DIF, and the GPCM-DIF. For binary responses, the PCM and GPCM will transform to the Rasch model and the 2-PL model, respectively.

### The PCM and PCM-DIF

The Partial Credit Model (PCM) is the model multi-categorical responses, which generalizes the Rasch model.


```r
pcm_res <- pcm(polydif_inh_dset[,13:19])
#> [1] "...done!"
```

Similar with the PCM the PCM-DIF generalize the Rasch model even more by parameterizing the DIF effect in the items. However, in the PCM-DIF, we need to define the groups of the subjects. In this simulated dataset, the groups are pre-designed as the first half and the rest.


```r
pcmdif_res <- pcm_dif(polydif_inh_dset[,13:19], groups_map = c(rep(0,245),rep(1,245)))
#> [1] "...done!"
```

Some of the S3 generic functions are implemented to this model, i.e., summary() and print(). However, to filter which model's parameter to plot, I add `par` argument. It could be filled in using `theta`, `beta`, `gamma`, `alpha`, and `delta`, depends on the models applied. Ignoring this argument shows all estimated parameters of the model. `Item Loc.` represents the item location which is obtained by averaging the threshold values. `*` indicates the occurence of a disordered threshold.  


```r
summary(pcm_res, par="beta")
#> 
#> 
#> The estimated difficulty scores:
#>       Th_1  Th_2  Th_3  Th_4 Item Loc.  
#> I 13 -4.79 -2.81 -1.83 -1.82     -2.81  
#> I 14 -2.87 -1.96 -0.61 -0.78     -1.55 *
#> I 15 -1.48 -0.51  0.07  0.18     -0.44  
#> I 16 -0.25  0.39  0.14  1.73      0.50 *
#> I 17  0.57  0.69  2.23  2.62      1.53  
#> I 18  1.74  1.66  3.72  3.62      2.68 *
#> I 19 -0.26 -0.14  0.05  0.58      0.06  
#> 
#> The most difficult item:  I 18
#> The easiest item:  I 13
#> There are 3 items which have disordered thresholds.
#> '*' Item has disordered thresholds.
```


```r
summary(pcmdif_res, par="delta")
#> 
#> 
#> The estimated DIF effects (gap of difficulties) (delta):
#>         V1
#> I 19 -6.05
```

For PCM and PCM-DIF, the estimated parameters could be plotted as a Person-Item map. However, as for PCM-DIF the items with DIF will be resolved by splitting the items based on the given groups. As the results, we may obtain more than one item with the same number.


```r
plot_PImap(pcm_res, main = "Person-Item map of the PCM")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)


```r
plot_PImap(pcmdif_res, main = "Person-Item map of the PCM-DIF")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

Since item~1~~9~ is a DIF-item, there are two item~1~~9~ in the Person-Item map of the PCM-DIF, the `I 19` and `I 19_a` for the reference and the focal group, respectively. Red color means that there is a threshold disorder within the item.

The `plot_ICC()` and `plot_EVC()` could be used to plot the item characteristic curve (ICC) and the expected value curve (EVC), respectively, for each item.


```r
plot_ICC(pcm_res, itemno = 5, main = "ICC of I 17; estimated using PCM")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

To implement the standard Rasch goodness-of-fit statistics, a specific S3 function have been developed to compute the (item/person) fit statistics, i.e., `fitStats()` function, for the PCM and the PCM-DIF. To summarize only item or person statistic, `itemfit()` or `personfit()` could be used, respectively.


```r
pcm_fit <- fitStats(pcm_res)
#> [1] "...done!"
itemfit(pcm_fit)
#> 
#> Item Fit Statistics:
#> 
#>      OutfitMnSq InfitMnSq OutfitZSTD InfitZSTD Alpha
#> I 13       0.38      0.65      -3.51     -4.32  1.48
#> I 14       0.40      0.51      -5.44     -8.22  1.96
#> I 15       0.52      0.55      -5.57     -7.65  1.58
#> I 16       0.49      0.51      -6.24     -8.41  1.76
#> I 17       0.49      0.61      -4.98     -6.20  1.52
#> I 18       0.36      0.56      -3.94     -5.82  1.65
#> I 19       3.82      2.78      14.65     16.52  0.32
```


```r
pcmdif_fit <- fitStats(pcmdif_res)
#> [1] "...done!"
itemfit(pcmdif_fit)
#> 
#> Item Fit Statistics:
#> 
#>      OutfitMnSq InfitMnSq OutfitZSTD InfitZSTD Alpha
#> I 13       0.35      0.64      -1.90     -4.42  1.43
#> I 14       0.39      0.50      -3.86     -8.13  1.87
#> I 15       0.60      0.63      -4.03     -5.87  1.51
#> I 16       0.57      0.53      -4.68     -7.52  1.69
#> I 17       0.54      0.65      -3.10     -5.25  1.45
#> I 18       0.35      0.54      -2.10     -5.94  1.60
#> I 19       2.44      1.09       4.36      1.14  0.88
```

### The GPCM and GPCM-DIF

The Generalized Partial Credit Model (GPCM) generalizes the Partial Credit Model by modelling the predictability of the subjects' responses (discrimination parameters).


```r
gpcm_res <- gpcm(polydif_inh_dset[,13:19])
#> [1] "...done!"
```

For further generalization to the PCM, the Generalized Partial Credit Model with DIF (GPCM-DIF) not only models the predictability, but also the effects of the differential functioning of each items.


```r
gpcmdif_res <- gpcm_dif(polydif_inh_dset[,13:19], groups_map = c(rep(0,245),rep(1,245)))
#> [1] "...done!"
```

Similar to the PCM and PCM-DIF, some of the S3 generic functions such as `summary()` and `print()` also applied to these models.


```r
summary(gpcm_res, par="alpha")
#> 
#> 
#> The estimated discrimination parameters:
#>          alpha
#> I 13 1.4754127
#> I 14 1.9572103
#> I 15 1.5756668
#> I 16 1.7582360
#> I 17 1.5187701
#> I 18 1.6454490
#> I 19 0.3242871
```


```r
summary(gpcmdif_res, par="delta")
#> 
#> 
#> The estimated DIF effects (gap of difficulties):
#>        Group1
#> I 13        0
#> I 14        0
#> I 15        0
#> I 16        0
#> I 17        0
#> I 18        0
#> I 19 -5.27476
```

Unlike `plot_PImap()` which only implemented to the PCM and PCM-DIF, `plot_EVC()` and `plot_ICC()` are also implemented to the GPCM and the GPCM-DIF.
