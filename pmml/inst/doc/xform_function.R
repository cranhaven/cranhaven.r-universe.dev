## ---- echo = FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ---- echo=FALSE,warning=FALSE,message=FALSE,results="hide"-------------------
library(pmml)
library(knitr)

## -----------------------------------------------------------------------------
data(iris)
kable(head(iris,3))

## -----------------------------------------------------------------------------
iris_box <- xform_wrap(iris)

## -----------------------------------------------------------------------------
kable(head(iris_box$data,3))

## -----------------------------------------------------------------------------
kable(iris_box$field_data)

## -----------------------------------------------------------------------------
iris_box <- xform_function(iris_box,orig_field_name="Sepal.Length",
                         new_field_name="Sepal.Length.Sqrt",
                         expression="sqrt(Sepal.Length)")

## -----------------------------------------------------------------------------
kable(head(iris_box$data,3))

## -----------------------------------------------------------------------------
kable(iris_box$field_data[6,c(1:3,14)])

## -----------------------------------------------------------------------------
fit <- lm(Petal.Width ~ Sepal.Length.Sqrt, data=iris_box$data)
fit_pmml <- pmml(fit, transform=iris_box)

## -----------------------------------------------------------------------------
fit_pmml[[2]] #Data Dictionary node
fit_pmml[[3]][[1]] #Mining Schema node

## -----------------------------------------------------------------------------
fit_pmml[[3]][[3]]

## -----------------------------------------------------------------------------
iris_box <- xform_wrap(iris)
iris_box <- xform_function(iris_box,orig_field_name="Species",
                         new_field_name="Species.Setosa",
                         expression="if (Species == 'setosa') {1} else {0}")
kable(head(iris_box$data,3))

## -----------------------------------------------------------------------------
fit <- lm(Petal.Width ~ Species.Setosa, data=iris_box$data)
fit_pmml <- pmml(fit, transform=iris_box)
fit_pmml[[3]][[3]]

## -----------------------------------------------------------------------------
iris_box <- xform_wrap(iris)
iris_box <- xform_function(iris_box,orig_field_name="Sepal.Length,Petal.Length",
                         new_field_name="Length.Ratio",
                         expression="Sepal.Length / Petal.Length")

## -----------------------------------------------------------------------------
kable(head(iris_box$data,3))

## -----------------------------------------------------------------------------
fit <- lm(Petal.Width ~ Length.Ratio, data=iris_box$data)
fit_pmml <- pmml(fit, transform=iris_box)

## -----------------------------------------------------------------------------
fit_pmml[[2]] #Data Dictionary node
fit_pmml[[3]][[1]] #Mining Schema node

## -----------------------------------------------------------------------------
fit_pmml[[3]][[3]]

## -----------------------------------------------------------------------------
iris_box <- xform_wrap(iris)
iris_box <- xform_function(iris_box,orig_field_name="Sepal.Length,Petal.Length",
                         new_field_name="Length.Ratio",
                         expression="Sepal.Length / Petal.Length")

iris_box <- xform_function(iris_box,orig_field_name="Sepal.Length,Petal.Length,Sepal.Width",
                         new_field_name="Length.R.Times.S.Width",
                         expression="Length.Ratio * Sepal.Width")
kable(iris_box$field_data[6:7,c(1:3,14)])

## -----------------------------------------------------------------------------
fit <- lm(Petal.Width ~ Length.R.Times.S.Width, data=iris_box$data)
fit_pmml <- pmml(fit, transform=iris_box)


## -----------------------------------------------------------------------------
fit_pmml[[2]] #Data Dictionary node
fit_pmml[[3]][[1]] #Mining Schema node

## -----------------------------------------------------------------------------
fit_pmml[[3]][[3]]

## -----------------------------------------------------------------------------
iris_box <- xform_wrap(iris)

iris_box <- xform_function(wrap_object = iris_box,
                              orig_field_name = "Sepal.Length",
                              new_field_name = "SL_factor",
                              new_field_data_type = "factor",
                              expression = "if(Sepal.Length<5.1) {'level_A'} else if (Sepal.Length>6.6) {'level_B'} else {'level_C'}")

kable(head(iris_box$data, 3))

## -----------------------------------------------------------------------------
fit <- lm(Petal.Width ~ SL_factor, data=iris_box$data)
fit_pmml <- pmml(fit, transform=iris_box)

## ----echo=FALSE---------------------------------------------------------------
R <- c("+","-","/","*","^","<","<=",">",">=","&&","&","|","||","==","!=","!","ceiling","prod","log")
PMML <- c("+","-","/","*","pow","lessThan","lessOrEqual","greaterThan","greaterOrEqual","and","and","or","or","equal","notEqual","not","ceil","product","ln")

funcs_df <- data.frame(R, PMML)
knitr::kable(funcs_df)

## -----------------------------------------------------------------------------
isIn <- function(x, ...) {
  dots <- c(...)
  if (x %in% dots) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

isIn(1,2,1,4)

## -----------------------------------------------------------------------------
iris_box <- xform_wrap(iris)
iris_box <- xform_function(iris_box,orig_field_name="Species",
                         new_field_name="Species.Setosa.or.Versicolor",
                         expression="isIn(Species,'setosa','versicolor')")

## -----------------------------------------------------------------------------
kable(head(iris_box$data,3))

## -----------------------------------------------------------------------------
fit <- lm(Petal.Width ~ Species.Setosa.or.Versicolor, data=iris_box$data)
fit_pmml <- pmml(fit, transform=iris_box)
fit_pmml[[3]][[3]]

## -----------------------------------------------------------------------------
avg <- function(...) {
  dots <- c(...)
  return(mean(dots))
}

## -----------------------------------------------------------------------------
iris_box <- xform_wrap(iris)
iris_box <- xform_function(iris_box,orig_field_name="Sepal.Length,Petal.Length,Sepal.Width",
                         new_field_name="Length.Average.Ratio",
                         expression="avg(Sepal.Length,Petal.Length)/Sepal.Width")

## -----------------------------------------------------------------------------
kable(head(iris_box$data,3))

## -----------------------------------------------------------------------------
fit <- lm(Petal.Width ~ Length.Average.Ratio, data=iris_box$data)
fit_pmml <- pmml(fit, transform=iris_box)
fit_pmml[[3]][[3]]

## -----------------------------------------------------------------------------
function_to_pmml("1 + 2")

x <- 3
function_to_pmml("foo(bar(x * y))")

## -----------------------------------------------------------------------------
function_to_pmml("c(1,2,3)")

## -----------------------------------------------------------------------------
function_to_pmml("prod(1,2,na.rm=FALSE)") #produces incorrect PMML
function_to_pmml("prod(1,2)") #produces correct PMML

## -----------------------------------------------------------------------------
prod(c(1,2,3))
function_to_pmml("prod(c(1,2,3))")

## -----------------------------------------------------------------------------
function_to_pmml("pmmlT(((1+2))*(x))")

## -----------------------------------------------------------------------------
function_to_pmml("if(a<2) {x+3} else if (a>4) {4} else {5}")

