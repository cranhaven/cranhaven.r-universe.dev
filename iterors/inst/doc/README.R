## ----eval=FALSE---------------------------------------------------------------
# total <- 0
# repeat total <- total + nextOr(it, break)

## ----eval=FALSE---------------------------------------------------------------
# total <- 0
# tryCatch(
#   repeat total <- total + nextElem(it),
#   error=function(x) {
#     if (conditionMessage(x) != "StopIteration") stop(x)
#   }
# )

## ----eval=FALSE---------------------------------------------------------------
# devtools::install_github('crowding/iterors')

## ----eval=FALSE---------------------------------------------------------------
# install.packages('iterors', dependencies=TRUE)

