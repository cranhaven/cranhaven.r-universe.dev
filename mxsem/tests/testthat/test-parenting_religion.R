test_that("indirect and direct effects work", {

  # The following model is copied from Kerry, N., Prokosch, M. L., & Murray, D. R. (2023).
  # The holy father (and mother)? Multiple tests of the hypothesis that parenthood
  # and parental care motivation lead to greater religiosity. Personality and
  # Social Psychology Bulletin, 49(5), 709–726. https://doi.org/10.1177/01461672221076919

  # lavaan model by Nicholas Kerry and Marjorie Prokosch
  # at https://osf.io/aqfk3
  library(mxsem)
  library(lavaan)
  set.seed(123)

  model<-'ParentStatus~a*Age
Religiosity_7item~b*ParentStatus
Religiosity_7item~c*Age
PCAT~e*Age
Religiosity_7item~f*PCAT
ab:=a*b
ef:=e*f
total:=c+(ab)+(ef)
direct:=c'

  suppressWarnings(data <- lavaan::simulateData(model, sample.nobs = 1000))

  fit_lavaan <- cfa(model,
                    data = data,
                    missing = "ml",
                    fixed.x = FALSE)

  fit_mx  <- mxsem(model = model,
                   data = data) |>
    mxTryHard()

  testthat::expect_true(length(coef(fit_lavaan)) == length(omxGetParameters(fit_mx)))

  testthat::expect_true(abs(-2*logLik(fit_lavaan) -
                              fit_mx$fitfunction$result[[1]]) < 1e-4)

  summary_lavaan <- lavaan::summary(fit_lavaan)

  testthat::expect_true(abs(summary_lavaan$pe$est[summary_lavaan$pe$lhs == "direct"] -
                              fit_mx$direct$result[[1]]) < 1e-4)

  testthat::expect_true(abs(summary_lavaan$pe$est[summary_lavaan$pe$lhs == "ef"] -
                              fit_mx$ef$result[[1]]) < 1e-4)


  #Model 1b with nonsignificant 'n' and 'j' paths dropped
  model <- 'ParentStatus~a*Age
BDW~b*ParentStatus
Religiosity~c*BDW
PCAT~d*Age
STMO~e*PCAT
Religiosity~f*STMO
ParentStatus~~g*PCAT
STMO~h*ParentStatus
BDW~i*PCAT
Religiosity~k*PCAT
Religiosity~l*Age
STMO~m*Age
abc:=a*b*c
def:=d*e*f
ahf:=a*h*f
dic:=d*i*c
dk:=d*k
ic:=i*c
mf:=m*f
ef:=e*f
hf:=h*f
bc:=b*c
direct:=l'

  suppressWarnings(data <- lavaan::simulateData(model, sample.nobs = 1000))

  fit_lavaan <- cfa(model,
                    data = data,
                    missing = "ml",
                    fixed.x = FALSE)

  fit_mx  <- mxsem(model = model,
                   data = data) |>
    mxTryHard()

  testthat::expect_true(length(coef(fit_lavaan)) == length(omxGetParameters(fit_mx)))

  testthat::expect_true(abs(-2*logLik(fit_lavaan) -
                              fit_mx$fitfunction$result[[1]]) < 1e-4)

  summary_lavaan <- lavaan::summary(fit_lavaan)

  testthat::expect_true(abs(summary_lavaan$pe$est[summary_lavaan$pe$lhs == "direct"] -
                              fit_mx$direct$result[[1]]) < 1e-4)

  testthat::expect_true(abs(summary_lavaan$pe$est[summary_lavaan$pe$lhs == "ef"] -
                              fit_mx$ef$result[[1]]) < 1e-4)
})
