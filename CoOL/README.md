# Causes of Outcome Learning

To install the 'CoOL' package, you can run below code in R

```{r}
if(!require("devtools")) install.packages("devtools")
devtools::install_github("ekstroem/CoOL")
```

In order to plot dendrograms, you will also need to install the 'ggtree' package using below code in R
```{r}
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("ggtree")
```