## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
hist(iris$Sepal.Width, main="Width of sepal")

## ----warning = FALSE,message = FALSE------------------------------------------
library(EDFtest)
set.seed("100")
x=iris$Sepal.Width
shape=estimate.gamma(x)[1]
# Anderson-Darling statistic and P-value
(asq=AD.gamma(x))
AD.gamma.pvalue(a=asq,shape=shape)$P
#Cramér-von Mises statistic and P-value
(wsq=CvM.gamma(x))
CvM.gamma.pvalue(w=wsq,shape=shape)$P
#You can also use following generic functions
gof.gamma(x,print=TRUE) #Imhof
gof.gamma.bootstrap(x,M=10000) #bootstrap

## ----warning = FALSE,message = FALSE------------------------------------------
set.seed("100")
# Anderson-Darling statistic and P-value
(asq=AD.normal(x))
AD.normal.pvalue(a=asq)$P
#Cramér-von Mises statistic and P-value
(wsq=CvM.normal(x))
CvM.normal.pvalue(w=wsq)$P
#You can also use following generic functions
gof.normal(x,print=TRUE) #Imhof
gof.normal.bootstrap(x,M=10000) #bootstrap

