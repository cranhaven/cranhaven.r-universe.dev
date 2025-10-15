## ----echo=FALSE---------------------------------------------------------------
library(knitr)
library(AnalysisLin)

## -----------------------------------------------------------------------------
df <- data.frame(
  Descriptive_Statistics = c("desc_stat()","","","","",""),
  Data_Visualization = c("hist_plot()","dens_plot()", "bar_plot()","pie_plot()","qq_plot()","missing_value_plot()"),
  Correlation_Analysis = c("corr_matrix()", "corr_cluster()","","","",""),
  Feature_Engineering = c("missing_impute()", "pca()","","","","")
)
kable(df)

## -----------------------------------------------------------------------------
data("iris")
data("mtcars")
data("Titanic")
data("airquality")

## ----eval=FALSE---------------------------------------------------------------
#  desc_stat(mtcars)

## ----eval=FALSE---------------------------------------------------------------
#  desc_stat(iris)

## ----eval=FALSE---------------------------------------------------------------
#  desc_stat(airquality)

## ----eval=FALSE---------------------------------------------------------------
#  desc_stat(mtcars,max = F, min=F, sd=F,kurtosis = T,skewness = T,shapiro = T,anderson = T,lilliefors = T, jarque = T)

## ----eval=FALSE---------------------------------------------------------------
#  hist_plot(iris,subplot=F)

## ----eval=FALSE---------------------------------------------------------------
#  dens_plot(iris,subplot=T,nrow=2)

## ----eval=FALSE---------------------------------------------------------------
#  qq_plot(iris,subplot = T)

## ----eval=FALSE---------------------------------------------------------------
#  bar_plot(iris)

## ----eval=FALSE---------------------------------------------------------------
#  pie_plot(iris)

## ----eval=FALSE---------------------------------------------------------------
#  corr_matrix(mtcars)

## ----eval=FALSE---------------------------------------------------------------
#  corr_matrix(mtcars,corr_plot=T)

## ----eval=FALSE---------------------------------------------------------------
#  corr_matrix(mtcars,type='pearson')
#  corr_matrix(mtcars,type='spearman')

## ----eval=FALSE---------------------------------------------------------------
#  corr_cluster(mtcars,type='pearson')

## ----eval=FALSE---------------------------------------------------------------
#  corr_cluster(mtcars, type='spearman')

## ----eval=FALSE---------------------------------------------------------------
#  missing_values_plot(airquality)

## ----results='hide'-----------------------------------------------------------
impute_missing(airquality,method='mean')

## ----results='hide'-----------------------------------------------------------
impute_missing(airquality,method='mode')
impute_missing(airquality,method='median')
impute_missing(airquality,method='locf')
impute_missing(airquality,method='knn',k=5)

## ----eval=FALSE---------------------------------------------------------------
#  pca(mtcars,variance_threshold = 0.9,scale=T)

## ----eval=FALSE---------------------------------------------------------------
#  pca(mtcars,variance_threshold = 0.9,scale=TRUE,scree_plot=TRUE,biplot=TRUE)

