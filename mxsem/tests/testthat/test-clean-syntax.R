test_that("cleaning syntax works", {
  library(mxsem)
  # the following syntax is adapted from lavaan
  model <- '
  # latent variable definitions
     ind60 =~ x1 + x2 +
     x3
     dem60 =~
     y1 + a*y2 +
  b*y3 + c*y4
     dem65 =~     {data.y123 +
     z}*
     y5 + a*y6 + b*y7 + c*y8

  # regressions
      dem60 ~ ind60
    dem65 ~ ind60 + dem60

    # {commented algebra}
#no space after comment
  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6;  y3 ~
    ~ y7
    y4 ~~ y8
    y6 ~~ y8

    b := exp(c)'

  cleaned <- mxsem:::clean_syntax(model)
  expected <- c('ind60=~x1+x2+x3',
                'dem60=~y1+a*y2+b*y3+c*y4',
                'dem65=~{data.y123 +\n     z}*y5+a*y6+b*y7+c*y8',
                'dem60~ind60',
                'dem65~ind60+dem60',
                'y1~~y5',
                'y2~~y4+y6',
                'y3~~y7',
                'y4~~y8',
                'y6~~y8',
                'b:=exp(c)')

  testthat::expect_true(all(cleaned == expected))
})
