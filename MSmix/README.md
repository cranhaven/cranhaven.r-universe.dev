
# MSmix

This package allows the fit and analysis of finite Mixtures of Mallows models with Spearman Distance 
for full and partial rankings with arbitrary missing positions. 
Inference is conducted within the maximum likelihood framework via Expectation-Maximization algorithms. 
Estimation uncertainty is tackled via diverse versions of bootstrapping as well as via Hessian-based 
standard errors calculations. 

The most relevant reference of the methods is Crispino, Mollica, Astuti and Tardella (2023) 
https://link.springer.com/article/10.1007/s11222-023-10266-8

## Installation

To install the current release, use

``` r
install.packages("MSmix")
```

To install the current development version, use

``` r
#install.packages("remotes")
remotes::install_github("crispinomarta/MSmix")
```

## Citation

``` r
citation('MSmix')
#>To cite package 'MSmix' in publications use:
#>
#>  Crispino, M., Mollica, C., Astuti, V., Tardella, L. (2023). Efficient and accurate inference for mixtures of
#>  Mallows models with Spearman distance. Statistics and Computing, Vol. 33(98), pages 442--458, ISSN: 0960-3174,
#>  DOI: 10.1007/s11222-023-10266-8.
#>
#> A BibTeX entry for LaTeX users is
#> 
#>  @Article{,
#>    title = {Efficient and accurate inference for mixtures of Mallows models
#>                  with Spearman distance},
#>    author = {Marta Crispino and Cristina Mollica and Valerio Astuti and Luca Tardella},
#>    year = {2023},
#>    journal = {Statistics and Computing},
#>    volume = {33},
#>    number = {98},
#>    issn = {0960-3174},
#>    doi = {10.1007/s11222-023-10266-8},
#>  }

```
</div>
