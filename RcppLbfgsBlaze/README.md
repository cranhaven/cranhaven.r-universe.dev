## RcppLbfgsblaze

This package provides an implementation of the `L-BFGS` algorithm based on `Blaze` for `R` and `Rcpp`. 
The `L-BFGS` algorithm is a popular optimization algorithm for unconstrained optimization problems. 
`Blaze` is a high-performance `C++` math library for dense and sparse arithmetic. 
The package provides a simple interface to the `L-BFGS` algorithm and allows users to optimize 
their objective functions with Blaze vectors and matrices in `R` and `Rcpp`.

### Installation

You can install:

* the latest development version from github with

    ```R
    install.packages("remotes")
    remotes::install_github("ChingChuan-Chen/RcppLbfgsblaze")
    ```

If you encounter a bug, please file a reproducible example on [github](https://github.com/ChingChuan-Chen/RcppLbfgsblaze/issues).

### Logistic Model Fitting Benchmark

You can refer to the file [logisticBenchmark.R](./inst/examples/logisticBenchmark.R) to find the code.
Below code and corresponding results show that `RcppLbfgsblaze` provides a fast and efficient algorithm for logistic model fitting.
In the benchmark, `RcppLbfgsBlaze` is only slower than `RcppNumerical`, but faster than others.

    ```R
    source(system.file("examples", "logisticBenchmark.R", package = "RcppLbfgsBlaze"))
    # logistic model fitting benchmark for n = 10000, p = 100 and non-zero p = 6: nrep = 20
    # Unit: milliseconds
    #            expr      min        lq      mean    median        uq      max neval
    #         glm.fit 254.8283 259.26340 277.47036 263.97765 283.17805 336.0213    20
    #           optim  65.6519  69.05245  73.98623  70.32615  71.59410 148.2856    20
    #      optim_arma  16.1281  16.97425  22.05490  17.81580  18.27810  94.6341    20
    #          glmnet  38.5915  39.07820  44.26353  39.85460  41.21905 114.7233    20
    #           lbfgs  85.7390  88.19755  96.58998  92.39210  94.74080 165.2018    20
    #      lbfgs_arma  20.7813  21.87255  23.06615  22.71525  23.43450  29.2101    20
    #   RcppNumerical   8.5511   9.06755   9.81808   9.54100   9.92005  16.1381    20
    #  RcppLbfgsBlaze  10.0476  10.53910  11.26658  11.16235  11.59385  14.2053    20
    ```
    
When the sample size and number of predictors increase, `RcppLbfgsBlaze` will be faster than `RcppNumerical` and others.
It shows that `RcppLbfgsBlaze` provides relatively fast algorithm comparing to otehrs.

    ```R
    # logistic model fitting benchmark for n = 50000, p = 500 and non-zero p = 6: nrep = 20
    # Unit: milliseconds
    #            expr       min        lq      mean    median        uq       max neval
    #      optim_arma  547.1768  556.3317  578.6516  576.7488  594.5491  632.7897    20
    #      lbfgs_arma 1501.6494 1537.9691 1573.0044 1561.6261 1606.5876 1675.4529    20
    #   RcppNumerical  250.6525  255.3796  263.2894  262.5797  269.0240  286.2630    20
    #  RcppLbfgsBlaze  150.5987  154.8733  158.7338  156.8559  161.5664  173.0606    20
    ```

Above results are run on my desktop (i9-13900K, DDR5-4000 128GB).

### Authors

Ching-Chuan Chen

### License

MIT License
