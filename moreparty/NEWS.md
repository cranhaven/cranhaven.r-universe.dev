# moreparty 0.4.2 [CRAN]

* fixed Rd \link{} targets missing packages in documentation


# moreparty 0.4.1

## Changes in existing functions:

* `NiceTreePlot()`: compatibility with multinomial and multivariate trees
* `NodeTreePlot()`: compatibility with multinomial and multivariate trees
* `NodesInfo()`: compatibility with multinomial and multivariate trees


# moreparty 0.4 [CRAN]

## New function

* `NodeTreePlot()`: plots the results of each node of a conditional inference tree
* `PerfsRegression()`: performance measures for regression tasks
* `PerfsBinClassif()`: performance measures for binary classification tasks
* `NodesInfo()`: informations for each terminal node of a conditional inference tree
* `TreeStab()`: assesses the stability of a conditional inference tree by using bootstrap replications

## Changes in existing functions:

* `NiceTreePlot()`: new arguments (cex and justmin)
* `GetSplitStats()`: new summary element in the results + the function is no longer compatible with trees from the `party` package


# moreparty 0.3.3 [CRAN]

* dependency to 0.4.1 version of `vip` package 


# moreparty 0.3.2 [CRAN]

* bug fixes due to change in `vip` package


# moreparty 0.3.1 [CRAN]

* bug fix in vignettes due to changes in dependencies


# moreparty 0.3 [CRAN]

## New functions:

* `NiceTreePlot()`: plots conditional inference trees
* `EasyTreeVarImp()`: variable importance for conditional inference trees
* `ctreeUI()` and `ctreeServer()`: shiny module to build and analyse conditional inference trees
* `ictree()`: interactive (shiny) app for conditional inference trees
 
## Changes in existing functions:

* `GetSplitStats()` : results have been rearranged and a 'ratio' column has been added



# moreparty 0.2 [CRAN]

## New functions:

* `GetInteractionData()`: measures second order interactions between the covariates of a random forest
* `GetPartialData()`: computes partial dependencies of the covariates of a random forest
* `ggForestEffects()`: plots the effects of the covariates of a random forest in a ggplot dot plot
* `ggVarImp()`: plots variable importances of the covariates of a random forest in a ggplot dot plot

## Changes in existing functions:

* `GetAleData()`: bug fix for regression tasks