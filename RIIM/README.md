# RIIM: Randomization-Based Inference Under Inexact Matching

## Author
Jianan Zhu, Jeffrey Zhang, Zijian Guo, and Siyu Heng.

## Maintainer
Jianan Zhu (Email: jz4698@nyu.edu)

## Description
RIIM is an R package for randomization-based inference for average treatment effects under inexact matching introduced in Zhu, Zhang, Guo and Heng (2024).

To install package RIIM in R from GitHub, please run the following commands:
```{r}
library(xgboost)
library(MASS)
library(mvtnorm)
library(VGAM)
library(optmatch)
install.packages("devtools") 
library(devtools) 
install_github("zoezhu098/RIIM")
library(RIIM)
```

## Reference
Zhu, J., Zhang, J., Guo, Z., & Heng, S. (2024). Randomization-Based Inference for Average Treatment Effect in Inexactly Matched Observational Studies. arXiv preprint, arXiv:2308.02005.

Hansen, B. B., & Klopfer, S. O. (2006). Optimal full matching and related designs via network flows. Journal of computational and Graphical Statistics, 15(3), 609-627.

Hansen, B. B. (2004). Full matching in an observational study of coaching for the SAT. Journal of the American Statistical Association, 99(467), 609-618.

Rosenbaum, P. R. (1991). A characterization of optimal designs for observational studies. Journal of the Royal Statistical Society: Series B (Methodological), 53(3), 597-610.

Fogarty, C. B. (2018). On mitigating the analytical limitations of finely stratified experiments. Journal of the Royal Statistical Society Series B: Statistical Methodology, 80(5), 1035-1056.

Kang, H., Kreuels, B., May, J., & Small, D. S. (2016). Full matching approach to instrumental variables estimation with application to the effect of malaria on stunting. The Annals of Applied Statistics, 10(1), 335-364.
