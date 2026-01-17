## ----setup, echo=FALSE--------------------------------------------------------
IS_GITHUB <- Sys.getenv("IS_GITHUB") != ""

## ----results='asis', echo=FALSE, eval=IS_GITHUB-------------------------------
#  cat('
#  [![R-CMD-check](https://github.com/qsbase/qs/workflows/R-CMD-check/badge.svg)](https://github.com/qsbase/qs/actions)
#  [![CRAN-Status-Badge](https://www.r-pkg.org/badges/version/qs)](https://cran.r-project.org/package=qs)
#  [![CRAN-Downloads-Badge](https://cranlogs.r-pkg.org/badges/qs)](https://cran.r-project.org/package=qs)
#  [![CRAN-Downloads-Total-Badge](https://cranlogs.r-pkg.org/badges/grand-total/qs)](https://cran.r-project.org/package=qs)
#  ')

## ----eval=FALSE---------------------------------------------------------------
#  library(qs)
#  df1 <- data.frame(x = rnorm(5e6), y = sample(5e6), z=sample(letters, 5e6, replace = T))
#  qsave(df1, "myfile.qs")
#  df2 <- qread("myfile.qs")

## ----eval=FALSE---------------------------------------------------------------
#  # CRAN version
#  install.packages("qs")
#  
#  # CRAN version compile from source (recommended)
#  remotes::install_cran("qs", type = "source", configure.args = "--with-simd=AVX2")

## ----eval=FALSE---------------------------------------------------------------
#  data.frame(a = rnorm(5e6),
#             b = rpois(5e6, 100),
#             c = sample(starnames$IAU, 5e6, T),
#             d = sample(state.name, 5e6, T),
#             stringsAsFactors = F)

## ----echo=FALSE, results='asis'-----------------------------------------------
if(IS_GITHUB) {
  cat('![](vignettes/df_bench_write.png "df_bench_write"){width=576px}')
} else {
  cat('![](df_bench_write.png "df_bench_write"){width=576px}')
}

## ----echo=FALSE, results='asis'-----------------------------------------------
if(IS_GITHUB) {
  cat('![](vignettes/df_bench_read.png "df_bench_read"){width=576px}')
} else {
  cat('![](df_bench_read.png "df_bench_read"){width=576px}')
}

## ----echo=FALSE, results='asis'-----------------------------------------------
if(IS_GITHUB) {
  cat('![](vignettes/altrep_bench.png "altrep_bench"){width=487px}')
} else {
  cat('![](altrep_bench.png "altrep_bench"){width=487px}')
}

## ----eval=FALSE---------------------------------------------------------------
#  # With byte shuffling
#  x <- 1:1e8
#  qsave(x, "mydat.qs", preset = "custom", shuffle_control = 15, algorithm = "zstd")
#  cat( "Compression Ratio: ", as.numeric(object.size(x)) / file.info("mydat.qs")$size, "\n" )
#  # Compression Ratio:  1389.164
#  
#  # Without byte shuffling
#  x <- 1:1e8
#  qsave(x, "mydat.qs", preset = "custom", shuffle_control = 0, algorithm = "zstd")
#  cat( "Compression Ratio: ", as.numeric(object.size(x)) / file.info("mydat.qs")$size, "\n" )
#  # Compression Ratio:  1.479294

## ----eval=FALSE---------------------------------------------------------------
#  library(qs)
#  x <- qserialize(c(1, 2, 3))
#  qdeserialize(x)
#  [1] 1 2 3

## ----eval=FALSE---------------------------------------------------------------
#  library(qs)
#  library(Rcpp)
#  sourceCpp("test.cpp")
#  # save file using Rcpp interface
#  test()
#  # read in file created through Rcpp interface
#  qread("/tmp/myfile.qs")
#  [1] 1 2 3

