
# sae.projection

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/sae.projection)](https://CRAN.R-project.org/package=sae.projection)
[![R-CMD-check](https://github.com/Alfrzlp/sae.projection/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Alfrzlp/sae.projection/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

## Author

Azka Ubaidillah, Ridson Al Farizal P, Silvi Ajeng Larasati, Amelia
Rahayu

## Maintainer

Ridson Al Farizal P <ridsonap@bps.go.id>

## Description

The **sae.projection** package provides a robust tool for *small area
estimation using a projection-based approach*. This method is
particularly beneficial in scenarios involving two surveys, the first
survey collects data solely on auxiliary variables, while the second,
typically smaller survey, collects both the variables of interest and
the auxiliary variables. The package constructs a working model to
predict the variables of interest for each sample in the first survey.
These predictions are then used to estimate relevant indicators for the
desired domains. This condition overcomes the problem of estimation in a
small area when only using the second survey data.

## Installation

You can install the development version of sae.projection from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Alfrzlp/sae.projection")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(sae.projection)
#> Loading required package: tidymodels
#> ── Attaching packages ────────────────────────────────────── tidymodels 1.3.0 ──
#> ✔ broom        1.0.8     ✔ recipes      1.3.1
#> ✔ dials        1.4.0     ✔ rsample      1.3.0
#> ✔ dplyr        1.1.4     ✔ tibble       3.3.0
#> ✔ ggplot2      3.5.2     ✔ tidyr        1.3.1
#> ✔ infer        1.0.8     ✔ tune         1.3.0
#> ✔ modeldata    1.4.0     ✔ workflows    1.2.0
#> ✔ parsnip      1.3.2     ✔ workflowsets 1.1.1
#> ✔ purrr        1.0.4     ✔ yardstick    1.3.2
#> ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
#> ✖ purrr::discard() masks scales::discard()
#> ✖ dplyr::filter()  masks stats::filter()
#> ✖ dplyr::lag()     masks stats::lag()
#> ✖ recipes::step()  masks stats::step()
library(dplyr)
```

## Regression

### Data

``` r
df_svy22_income <- df_svy22 %>% filter(!is.na(income))
df_svy23_income <- df_svy23 %>% filter(!is.na(income))
```

### Linear Regression Model

``` r
lm_proj <- ma_projection(
  income ~ age + sex + edu + disability,
  cluster_ids = "PSU", weight = "WEIGHT", strata = "STRATA",
  domain = c("PROV", "REGENCY"),
  working_model = linear_reg(),
  data_model = df_svy22_income,
  data_proj = df_svy23_income,
  nest = TRUE
)

lm_proj$projection
#>    PROV REGENCY     ypr      var_ypr   rse_ypr
#> 1    40       1 2355859 112850851251 14.259463
#> 2    40       2 2155687  14070372903  5.502588
#> 3    40       3 2088804   9711460386  4.717855
#> 4    40       4 2188828  12511600884  5.110281
#> 5    40       5 2138723  22768231513  7.055213
#> 6    40       6 2061848  10210621315  4.900827
#> 7    40       7 2085265   9976295858  4.789866
#> 8    40       8 2028248   6536668678  3.986183
#> 9    40       9 2233891  12561471412  5.017163
#> 10   40      10 2997192 470694250001 22.890476
#> 11   40      11 2061056  10060073816  4.866433
#> 12   40      12 2201031   9581460318  4.447231
#> 13   40      13 2213420  32332172691  8.123695
#> 14   40      14 2200890  13896011417  5.356075
#> 15   40      15 2269106  15294166332  5.450146
#> 16   40      16 2174782  12143176114  5.066994
#> 17   40      17 1965337  10785195069  5.284172
#> 18   40      18 1837433   8405642799  4.989694
#> 19   40      19 2208518  24478147649  7.084160
#> 20   40      20 2143794  13056774386  5.330094
#> 21   40      21 2067066   7984452688  4.322831
#> 22   40      22 2038540   9802389037  4.856761
#> 23   40      23 2393420  86969130589 12.321503
#> 24   40      24 2240375  20747721026  6.429313
#> 25   40      25 2195086  14570971410  5.499113
#> 26   40      26 2187382  24209446221  7.113247
#> 27   40      27 2217716  64407458495 11.443585
#> 28   40      28 1986641   6921930259  4.187880
#> 29   40      29 1965301   5897858683  3.907672
#> 30   40      30 2256849  17114638305  5.796707
#> 31   40      31 2267756  24897436539  6.957945
#> 32   40      32 2063165   8403451069  4.443189
#> 33   40      33 1982999   7567172983  4.386765
#> 34   40      34 2244054  21199661590  6.488302
#> 35   40      35 2137450  38140066571  9.136813
#> 36   40      36 2024760  10698498794  5.108435
#> 37   40      37 2055319  18930473132  6.694245
#> 38   40      38 2954545 314127308067 18.969774
#> 39   40      39 1955815  13455721680  5.930971
#> 40   40      40 1998518  23669511858  7.698154
#> 41   40      41 2387539  63038731173 10.516066
#> 42   40      42 2007678   8297754980  4.537183
#> 43   40      43 1983971   5647857868  3.787971
#> 44   40      44 2187359  41458503574  9.308653
#> 45   40      45 1986850  14801246261  6.123279
#> 46   40      46 2369022  48861397759  9.330694
#> 47   40      47 2116854   9339080301  4.565214
#> 48   40      48 2174280  19690945724  6.453833
#> 49   40      49 2239364  41590806935  9.106972
#> 50   40      50 2118603  13019855312  5.385840
#> 51   40      51 1927953   6732676376  4.255959
#> 52   40      52 1970130   7315602106  4.341404
#> 53   40      53 1954391   7289520282  4.368556
#> 54   40      54 2137444  15690069691  5.860272
#> 55   40      55 2017267   6428930853  3.974715
#> 56   40      56 2351050  50853300343  9.591748
#> 57   40      57 2162501  25747523120  7.420129
#> 58   40      58 2674530 313400377469 20.931591
#> 59   40      59 2335474  17471370430  5.659635
#> 60   40      60 2120891   9692679744  4.641983
#> 61   40      61 2098821  13706060975  5.578031
#> 62   40      62 1971450   9299512082  4.891525
#> 63   40      63 2209349  32676097067  8.181835
#> 64   40      64 1941592   6738894142  4.228014
#> 65   40      65 2070572   7158546619  4.086223
#> 66   40      66 2089372  17862942632  6.396771
#> 67   40      67 1900122   7741749874  4.630608
#> 68   40      68 1941208   6187014231  4.051992
#> 69   40      69 2279011  33956910517  8.085701
#> 70   40      70 2142969  14310388261  5.582257
#> 71   40      71 2341867  30248638021  7.426611
#> 72   40      72 1968031   7531981310  4.409839
#> 73   40      73 2333145  31242426098  7.575837
#> 74   40      74 2191866  15913071184  5.755235
#> 75   40      75 2390506  52976205496  9.628322
#> 76   40      76 1937820   6596212084  4.191157
#> 77   40      77 2057303  20972153977  7.039198
#> 78   40      78 2269991  34769289337  8.214362
#> 79   40      79 2166598  17221385107  6.056974
```

## Classification

### Data

``` r
df_svy22_neet <- df_svy22 %>%
  filter(between(age, 15, 24))
df_svy23_neet <- df_svy23 %>%
  filter(between(age, 15, 24))
```

### Logistic Regression

``` r
lr_proj <- ma_projection(
  formula = neet ~ sex + edu + disability,
  cluster_ids = "PSU",
  weight = "WEIGHT",
  strata = "STRATA",
  domain = c("PROV", "REGENCY"),
  working_model = logistic_reg(),
  data_model = df_svy22_neet,
  data_proj = df_svy23_neet,
  nest = TRUE
)

lr_proj$projection
#>    PROV REGENCY       ypr     var_ypr  rse_ypr
#> 1    40       1 1.1183886 0.006444221 7.177819
#> 2    40       2 1.2557005 0.006105655 6.222717
#> 3    40       3 1.1274297 0.006065054 6.907611
#> 4    40       4 1.1113317 0.005828028 6.869373
#> 5    40       5 1.0800544 0.006010325 7.177998
#> 6    40       6 1.1517670 0.006139404 6.802969
#> 7    40       7 1.0794256 0.007007080 7.754894
#> 8    40       8 1.2443838 0.005809545 6.125150
#> 9    40       9 1.2047362 0.004965885 5.849333
#> 10   40      10 0.9640855 0.006984372 8.668583
#> 11   40      11 1.0933346 0.005826673 6.981636
#> 12   40      12 1.1801704 0.005316427 6.178246
#> 13   40      13 1.1846172 0.007200787 7.163280
#> 14   40      14 1.0845802 0.006903031 7.660520
#> 15   40      15 1.1505433 0.006664377 7.095399
#> 16   40      16 1.1486635 0.005427596 6.413734
#> 17   40      17 1.2093561 0.005661401 6.221681
#> 18   40      18 1.1028660 0.006612135 7.373066
#> 19   40      19 1.2294521 0.004804973 5.638114
#> 20   40      20 1.1578711 0.006367011 6.891402
#> 21   40      21 1.2762862 0.005070637 5.579345
#> 22   40      22 1.2102351 0.005540878 6.150629
#> 23   40      23 1.2059524 0.005400307 6.093672
#> 24   40      24 1.1783683 0.005738102 6.428402
#> 25   40      25 1.1080694 0.005155669 6.480008
#> 26   40      26 1.2509160 0.005157540 5.741074
#> 27   40      27 1.2471413 0.005605561 6.003353
#> 28   40      28 1.1753644 0.004616937 5.781020
#> 29   40      29 1.0877470 0.005710199 6.947007
#> 30   40      30 1.0443887 0.006598061 7.777607
#> 31   40      31 1.2117605 0.006043338 6.415369
#> 32   40      32 1.1688809 0.005417636 6.297014
#> 33   40      33 1.1884573 0.006069021 6.555046
#> 34   40      34 1.1248072 0.006022908 6.899618
#> 35   40      35 1.1672254 0.006522315 6.919045
#> 36   40      36 1.2252635 0.005834061 6.233845
#> 37   40      37 1.0434104 0.006060357 7.460947
#> 38   40      38 1.2600382 0.004835964 5.518967
#> 39   40      39 1.1189457 0.006639541 7.282157
#> 40   40      40 1.0950714 0.005302188 6.649441
#> 41   40      41 1.1898514 0.005923444 6.468363
#> 42   40      42 1.2487870 0.007639726 6.999234
#> 43   40      43 1.0982993 0.006348754 7.254768
#> 44   40      44 1.1818942 0.005524671 6.288899
#> 45   40      45 1.2766450 0.006564731 6.346559
#> 46   40      46 1.1639300 0.005717720 6.496577
#> 47   40      47 1.2213804 0.006462049 6.581641
#> 48   40      48 1.1945465 0.005625430 6.278773
#> 49   40      49 1.0425183 0.005626184 7.194876
#> 50   40      50 0.9518069 0.007261626 8.952989
#> 51   40      51 1.3295578 0.005528923 5.592591
#> 52   40      52 1.0509247 0.005438234 7.017095
#> 53   40      53 1.1885369 0.005763973 6.387756
#> 54   40      54 1.1534033 0.005731217 6.563602
#> 55   40      55 1.2341504 0.005928711 6.238958
#> 56   40      56 1.1968135 0.006673016 6.825502
#> 57   40      57 1.1810190 0.005667091 6.374164
#> 58   40      58 1.1206938 0.005539195 6.641044
#> 59   40      59 1.0108983 0.005954844 7.633571
#> 60   40      60 1.1346988 0.006363699 7.030306
#> 61   40      61 1.2145020 0.005925792 6.338332
#> 62   40      62 1.1014442 0.005650843 6.824866
#> 63   40      63 1.2185587 0.005020100 5.814464
#> 64   40      64 1.1159900 0.005480422 6.633561
#> 65   40      65 1.1039627 0.005699648 6.838638
#> 66   40      66 1.1283841 0.006518595 7.155172
#> 67   40      67 1.0386828 0.005776468 7.317256
#> 68   40      68 0.9854114 0.005870607 7.775421
#> 69   40      69 1.1610866 0.005587405 6.437844
#> 70   40      70 1.1255552 0.006023267 6.895238
#> 71   40      71 1.2091412 0.005741644 6.266732
#> 72   40      72 1.2197376 0.005334661 5.988072
#> 73   40      73 1.1532298 0.005458433 6.406460
#> 74   40      74 1.1886667 0.006554084 6.810765
#> 75   40      75 1.2285712 0.005805988 6.202085
#> 76   40      76 1.1290612 0.006445259 7.110542
#> 77   40      77 1.1334380 0.005447368 6.511718
#> 78   40      78 1.0864061 0.006680768 7.523519
#> 79   40      79 1.1952840 0.004899836 5.856251
```

### LightGBM with Hyperparameter Tunning

``` r
library(bonsai)
show_engines("boost_tree")
#> # A tibble: 7 × 2
#>   engine   mode          
#>   <chr>    <chr>         
#> 1 xgboost  classification
#> 2 xgboost  regression    
#> 3 C5.0     classification
#> 4 spark    classification
#> 5 spark    regression    
#> 6 lightgbm regression    
#> 7 lightgbm classification
lgbm_model <- boost_tree(
  mtry = tune(), trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(),
  engine = "lightgbm"
)
```

``` r
lgbm_proj <- ma_projection(
  formula = neet ~ sex + edu + disability,
  cluster_ids = "PSU",
  weight = "WEIGHT",
  strata = "STRATA",
  domain = c("PROV", "REGENCY"),
  working_model = lgbm_model,
  data_model = df_svy22_neet,
  data_proj = df_svy23_neet,
  cv_folds = 3,
  grid = 20,
  nest = TRUE
)

lgbm_proj$projection
```

### Projection Estimator with Random Forest Algorithm

``` r
data(df_svy_A)
data(df_svy_B)
x_predictors <- names(df_svy_A)[5:19]

# Projection RF with feature selection and bias correction
result <- projection_randomforest(
  data_model = df_svy_A,
  target_column = "Y",
  predictor_cols = x_predictors,
  data_proj = df_svy_B,
  domain1 = "province",
  domain2 = "regency",
  psu = "num",
  ssu = NULL,
  strata = NULL,
  weights = "weight",
  feature_selection = TRUE,
  bias_correction = TRUE
)
#> Info: Bias correction is enabled. Calculating indirect estimation with bias
#> correction.
#> Starting preprocessing...
#> Preprocessing completed. Starting data split...
#> Data split completed. Checking for missing values...
#> Feature selection (RFE) enabled. Starting RFE...
#> Loading required package: lattice
#> 
#> 
#> Attaching package: 'caret'
#> 
#> 
#> The following objects are masked from 'package:yardstick':
#> 
#>     precision, recall, sensitivity, specificity
#> 
#> 
#> The following object is masked from 'package:purrr':
#> 
#>     lift
#> 
#> 
#> RFE completed. Features selected.
#> Features selected. Starting hyperparameter tuning...
#> Hyperparameter tuning completed. Training model...
#> note: only 7 unique complexity parameters in default grid. Truncating the grid to 7 .
#> Model training completed. Starting model evaluation...
#> Evaluation completed. Starting prediction on new data...
#> Prediction completed. Starting indirect estimation for domain...
#> Indirect estimation completed. Starting direct estimation...
#> Direct estimation completed. Starting bias correction estimation...
#> Bias-corrected estimation completed. Returning results...
print(result)
#> $model
#> Random Forest 
#> 
#> 1600 samples
#>    8 predictor
#>    2 classes: 'No', 'Yes' 
#> 
#> No pre-processing
#> Resampling: Cross-Validated (5 fold) 
#> Summary of sample sizes: 1280, 1281, 1279, 1280, 1280 
#> Addtional sampling using SMOTE
#> 
#> Resampling results across tuning parameters:
#> 
#>   mtry  splitrule   Accuracy   Kappa    
#>   2     gini        0.8918628  0.7596156
#>   2     extratrees  0.8818569  0.7374146
#>   3     gini        0.8912457  0.7583682
#>   3     extratrees  0.8843667  0.7431274
#>   4     gini        0.8868706  0.7489424
#>   4     extratrees  0.8831167  0.7407826
#>   5     gini        0.8837456  0.7425685
#>   5     extratrees  0.8824878  0.7394682
#>   6     gini        0.8831226  0.7413135
#>   6     extratrees  0.8831167  0.7411973
#>   7     gini        0.8849937  0.7455293
#>   7     extratrees  0.8843667  0.7439495
#>   8     gini        0.8800015  0.7347802
#>   8     extratrees  0.8818648  0.7388603
#> 
#> Tuning parameter 'min.node.size' was held constant at a value of 1
#> Accuracy was used to select the optimal model using the largest value.
#> The final values used for the model were mtry = 2, splitrule = gini
#>  and min.node.size = 1.
#> 
#> $importance
#> ranger variable importance
#> 
#>     Overall
#> x10 100.000
#> x4   33.334
#> x6   25.510
#> x1   19.271
#> x2   19.219
#> x8   18.952
#> x5    3.924
#> x12   0.000
#> 
#> $train_accuracy
#> Accuracy 
#>  0.95125 
#> 
#> $validation_accuracy
#> Accuracy 
#>    0.915 
#> 
#> $validation_performance
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction  No Yes
#>        No  121  11
#>        Yes  23 245
#>                                           
#>                Accuracy : 0.915           
#>                  95% CI : (0.8832, 0.9404)
#>     No Information Rate : 0.64            
#>     P-Value [Acc > NIR] : < 2e-16         
#>                                           
#>                   Kappa : 0.8121          
#>                                           
#>  Mcnemar's Test P-Value : 0.05923         
#>                                           
#>             Sensitivity : 0.8403          
#>             Specificity : 0.9570          
#>          Pos Pred Value : 0.9167          
#>          Neg Pred Value : 0.9142          
#>              Prevalence : 0.3600          
#>          Detection Rate : 0.3025          
#>    Detection Prevalence : 0.3300          
#>       Balanced Accuracy : 0.8987          
#>                                           
#>        'Positive' Class : No              
#>                                           
#> 
#> $data_proj
#> # A tibble: 8,000 × 21
#>    province regency id_ind   num weight    x1    x2    x3    x4    x5    x6
#>       <dbl>   <dbl>  <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1       31      19  18362     1      5     0     2     1    70     3     9
#>  2       31      25  24661     2      5     1     2     1    35     4     1
#>  3       31      30  29630     3      5     0     2     0    57     5     6
#>  4       31      18  17536     4      5     1     1     0    49     2     3
#>  5       31       2   1967     5      5     1     2     0    45     5     4
#>  6       31      14  13395     6      5     1     1     1    42     1     5
#>  7       31       5   4673     7      5     0     2     1    19     4     9
#>  8       31      27  26950     8      5     0     2     0    39     4    10
#>  9       31       5   4701     9      5     1     1     1    40     3     1
#> 10       31      22  21696    10      5     0     1     1    64     2     4
#> # ℹ 7,990 more rows
#> # ℹ 10 more variables: x7 <dbl>, x8 <dbl>, x9 <dbl>, x10 <dbl>, x11 <dbl>,
#> #   x12 <dbl>, x13 <dbl>, x14 <dbl>, x15 <dbl>, Est_Y <fct>
#> 
#> $Direct
#>    Estimation_Direct Variance  RSE
#> 31                64 0.000115 1.68
#> 
#> $Domain1_corrected_bias
#>    Estimation_Domain1 Variance  RSE Est_corrected Var_corrected RSE_corrected
#> 31              68.74  2.7e-05 0.75         65.84       3.2e-05          0.86
#> 
#> $Domain2_corrected_bias
#>       Estimation_Domain2 Variance  RSE Est_corrected Var_corrected
#> 31.1               66.30 0.001214 5.26         63.40      0.001440
#> 31.2               71.69 0.000927 4.25         68.79      0.001153
#> 31.3               68.61 0.000966 4.53         65.71      0.001192
#> 31.4               64.29 0.001093 5.14         61.39      0.001319
#> 31.5               64.21 0.001210 5.42         61.31      0.001436
#> 31.6               74.44 0.001057 4.37         71.54      0.001283
#> 31.7               66.03 0.001073 4.96         63.13      0.001299
#> 31.8               68.35 0.000992 4.61         65.45      0.001218
#> 31.9               57.87 0.001238 6.08         54.97      0.001464
#> 31.10              79.17 0.000764 3.49         76.27      0.000990
#> 31.11              70.17 0.001157 4.85         67.27      0.001383
#> 31.12              67.84 0.000961 4.57         64.94      0.001187
#> 31.13              67.19 0.001148 5.04         64.29      0.001374
#> 31.14              66.18 0.001081 4.97         63.28      0.001307
#> 31.15              65.70 0.001089 5.02         62.80      0.001315
#> 31.16              72.19 0.001188 4.77         69.29      0.001414
#> 31.17              65.32 0.001310 5.54         62.42      0.001536
#> 31.18              69.07 0.001101 4.80         66.17      0.001327
#> 31.19              71.65 0.001047 4.52         68.75      0.001273
#> 31.20              66.19 0.001066 4.93         63.29      0.001292
#> 31.21              77.08 0.000920 3.94         74.18      0.001146
#> 31.22              66.67 0.001043 4.85         63.77      0.001269
#> 31.23              73.04 0.000965 4.25         70.14      0.001191
#> 31.24              65.48 0.001148 5.17         62.58      0.001374
#> 31.25              74.09 0.000995 4.26         71.19      0.001221
#> 31.26              70.14 0.000993 4.49         67.24      0.001219
#> 31.27              68.87 0.001011 4.62         65.97      0.001237
#> 31.28              66.50 0.001098 4.98         63.60      0.001324
#> 31.29              69.61 0.001037 4.63         66.71      0.001263
#> 31.30              68.04 0.001121 4.92         65.14      0.001347
#> 31.31              63.29 0.001123 5.29         60.39      0.001349
#> 31.32              69.54 0.001075 4.72         66.64      0.001301
#> 31.33              69.89 0.001131 4.81         66.99      0.001357
#> 31.34              70.24 0.001020 4.55         67.34      0.001246
#> 31.35              68.11 0.001174 5.03         65.21      0.001400
#> 31.36              68.29 0.001056 4.76         65.39      0.001282
#> 31.37              69.00 0.001070 4.74         66.10      0.001296
#> 31.38              71.57 0.001033 4.49         68.67      0.001259
#> 31.39              70.33 0.000998 4.49         67.43      0.001224
#> 31.40              67.74 0.001175 5.06         64.84      0.001401
#>       RSE_corrected
#> 31.1           5.99
#> 31.2           4.94
#> 31.3           5.25
#> 31.4           5.92
#> 31.5           6.18
#> 31.6           5.01
#> 31.7           5.71
#> 31.8           5.33
#> 31.9           6.96
#> 31.10          4.13
#> 31.11          5.53
#> 31.12          5.31
#> 31.13          5.77
#> 31.14          5.71
#> 31.15          5.77
#> 31.16          5.43
#> 31.17          6.28
#> 31.18          5.51
#> 31.19          5.19
#> 31.20          5.68
#> 31.21          4.56
#> 31.22          5.59
#> 31.23          4.92
#> 31.24          5.92
#> 31.25          4.91
#> 31.26          5.19
#> 31.27          5.33
#> 31.28          5.72
#> 31.29          5.33
#> 31.30          5.63
#> 31.31          6.08
#> 31.32          5.41
#> 31.33          5.50
#> 31.34          5.24
#> 31.35          5.74
#> 31.36          5.48
#> 31.37          5.45
#> 31.38          5.17
#> 31.39          5.19
#> 31.40          5.77
```

### Projection Estimator with XGBoost Algorithm

``` r
data(df_svy_A)
data(df_svy_B)

# Remove unused identifier column
df_svy_A <- df_svy_A |> dplyr::select(-id_ind)
df_svy_B <- df_svy_B |> dplyr::select(-id_ind)

# Projection XGBoost with feature selection and bias correction
result <- projection_xgboost(
  target_col = "Y",
  data_model = df_svy_A,
  data_proj = df_svy_B,
  id = "num",
  STRATA = NULL,
  domain1 = "province",
  domain2 = "regency",
  weight = "weight",
  nfold = 10,
  test_size = 0.2,
  task_type = "classification",
  feature_selection = TRUE,
  corrected_bias = TRUE
)
#> 
#> Load data...
#> 
#> Final Task Type: binary 
#> 
#> Handling missing values in preprocessing...
#> 
#> Performing feature selection...
#> 
#> Applying SMOTE to balance classes...
#> 
#> Preparing data for XGBoost...
#> 
#> Performing hyperparameter tuning...
#> 
#> Evaluating model...
#> 
#> Performing survey estimation...
#> 
#> Computing Corrected Bias...
#> 
#> Computing Direct Estimate...
#> 
#> PSU, SSU, and STRATA are available. Proceeding with the data...
#> 
#> Corrected Bias...
#> 
#> Weight Index...
#> 
#> Bias...
#> 
#> Corrected Bias Calculation Completed...
print(result)
#> $metadata
#> $metadata$method
#> [1] "Projection Estimator With XGBoost Algorithm"
#> 
#> $metadata$model_type
#> [1] "classification"
#> 
#> $metadata$feature_selection_used
#> [1] TRUE
#> 
#> $metadata$corrected_bias_applied
#> [1] TRUE
#> 
#> $metadata$n_features_used
#> [1] 6
#> 
#> $metadata$model_params
#> $metadata$model_params$objective
#> [1] "binary:logistic"
#> 
#> $metadata$model_params$eval_metric
#> [1] "logloss"
#> 
#> $metadata$model_params$eta
#> [1] 0.1
#> 
#> $metadata$model_params$max_depth
#> [1] 7
#> 
#> $metadata$model_params$min_child_weight
#> [1] 1
#> 
#> $metadata$model_params$subsample
#> [1] 0.8
#> 
#> $metadata$model_params$colsample_bytree
#> [1] 0.7
#> 
#> $metadata$model_params$lambda
#> [1] 0
#> 
#> $metadata$model_params$alpha
#> [1] 0
#> 
#> $metadata$model_params$nthread
#> [1] 4
#> 
#> $metadata$model_params$validate_parameters
#> [1] TRUE
#> 
#> 
#> $metadata$features_selected
#> [1] "x1"  "x2"  "x4"  "x6"  "x10" "x11"
#> 
#> 
#> $estimation
#> $estimation$projected_data
#> # A tibble: 8,000 × 20
#>    province regency   num weight    x1    x2    x3    x4    x5    x6    x7    x8
#>       <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1       31      19     1      5     0     2     1    70     3     9     0   429
#>  2       31      25     2      5     1     2     1    35     4     1     0   469
#>  3       31      30     3      5     0     2     0    57     5     6     0    95
#>  4       31      18     4      5     1     1     0    49     2     3     0   295
#>  5       31       2     5      5     1     2     0    45     5     4     0   129
#>  6       31      14     6      5     1     1     1    42     1     5     0   269
#>  7       31       5     7      5     0     2     1    19     4     9     1   172
#>  8       31      27     8      5     0     2     0    39     4    10     1   555
#>  9       31       5     9      5     1     1     1    40     3     1     0   150
#> 10       31      22    10      5     0     1     1    64     2     4     0   277
#> # ℹ 7,990 more rows
#> # ℹ 8 more variables: x9 <dbl>, x10 <dbl>, x11 <dbl>, x12 <dbl>, x13 <dbl>,
#> #   x14 <dbl>, x15 <dbl>, P.hat <dbl>
#> 
#> $estimation$domain1_estimation
#>    province Estimation  RSE     Variance
#> 31       31      67.77 0.77 2.730403e-05
#> 
#> $estimation$domain2_estimation
#>       province regency Estimation  RSE     Variance
#> 31.1        31       1      65.22 5.38 0.0012329971
#> 31.2        31       2      71.69 4.25 0.0009268582
#> 31.3        31       3      67.26 4.67 0.0009875380
#> 31.4        31       4      63.81 5.20 0.0010998027
#> 31.5        31       5      63.68 5.48 0.0012173850
#> 31.6        31       6      73.33 4.49 0.0010865556
#> 31.7        31       7      64.59 5.12 0.0010944122
#> 31.8        31       8      68.81 4.56 0.0009846570
#> 31.9        31       9      57.36 6.14 0.0012416905
#> 31.10       31      10      77.78 3.64 0.0008002829
#> 31.11       31      11      71.82 4.66 0.0011182325
#> 31.12       31      12      67.40 4.62 0.0009680546
#> 31.13       31      13      67.19 5.04 0.0011483674
#> 31.14       31      14      63.77 5.24 0.0011162936
#> 31.15       31      15      63.77 5.24 0.0011162936
#> 31.16       31      16      71.01 4.92 0.0012183484
#> 31.17       31      17      64.74 5.61 0.0013196654
#> 31.18       31      18      67.53 4.98 0.0011304750
#> 31.19       31      19      69.59 4.75 0.0010910254
#> 31.20       31      20      66.19 4.93 0.0010657849
#> 31.21       31      21      74.48 4.22 0.0009901083
#> 31.22       31      22      65.73 4.95 0.0010577094
#> 31.23       31      23      72.55 4.31 0.0009763680
#> 31.24       31      24      64.47 5.29 0.0011629402
#> 31.25       31      25      72.02 4.49 0.0010442174
#> 31.26       31      26      69.19 4.59 0.0010103530
#> 31.27       31      27      69.81 4.52 0.0009942335
#> 31.28       31      28      67.00 4.93 0.0010893812
#> 31.29       31      29      69.12 4.68 0.0010464620
#> 31.30       31      30      65.98 5.16 0.0011571856
#> 31.31       31      31      60.87 5.57 0.0011507973
#> 31.32       31      32      69.04 4.77 0.0010852364
#> 31.33       31      33      69.89 4.81 0.0011314799
#> 31.34       31      34      67.80 4.81 0.0010650045
#> 31.35       31      35      64.32 5.48 0.0012405949
#> 31.36       31      36      68.29 4.76 0.0010564139
#> 31.37       31      37      68.00 4.85 0.0010881360
#> 31.38       31      38      70.05 4.66 0.0010650910
#> 31.39       31      39      69.38 4.60 0.0010166311
#> 31.40       31      40      66.67 5.19 0.0011948925
#> 
#> 
#> $performance
#> $performance$mean_train_accuracy
#> [1] 0.9324417
#> 
#> $performance$final_accuracy
#>  Accuracy 
#> 0.8919271 
#> 
#> $performance$confusion_matrix
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction   0   1
#>          0 313  41
#>          1  42 372
#>                                          
#>                Accuracy : 0.8919         
#>                  95% CI : (0.8678, 0.913)
#>     No Information Rate : 0.5378         
#>     P-Value [Acc > NIR] : <2e-16         
#>                                          
#>                   Kappa : 0.7826         
#>                                          
#>  Mcnemar's Test P-Value : 1              
#>                                          
#>             Sensitivity : 0.8817         
#>             Specificity : 0.9007         
#>          Pos Pred Value : 0.8842         
#>          Neg Pred Value : 0.8986         
#>              Prevalence : 0.4622         
#>          Detection Rate : 0.4076         
#>    Detection Prevalence : 0.4609         
#>       Balanced Accuracy : 0.8912         
#>                                          
#>        'Positive' Class : 0              
#>                                          
#> 
#> 
#> $bias_correction
#> $bias_correction$direct_estimation
#>    province Estimation  RSE     Variance
#> 31       31         64 1.68 0.0001152576
#> 
#> $bias_correction$corrected_domain1
#> # A tibble: 1 × 4
#>   province Est_corrected Var_corrected RSE_corrected
#>      <dbl>         <dbl>         <dbl>         <dbl>
#> 1       31          65.1      0.000033          0.88
#> 
#> $bias_correction$corrected_domain2
#>    province regency Estimation  RSE     Variance bias_est var_bias_est
#> 1        31       1      65.22 5.38 0.0012329971    -2.65 0.0002264521
#> 2        31       2      71.69 4.25 0.0009268582    -2.65 0.0002264521
#> 3        31       3      67.26 4.67 0.0009875380    -2.65 0.0002264521
#> 4        31       4      63.81 5.20 0.0010998027    -2.65 0.0002264521
#> 5        31       5      63.68 5.48 0.0012173850    -2.65 0.0002264521
#> 6        31       6      73.33 4.49 0.0010865556    -2.65 0.0002264521
#> 7        31       7      64.59 5.12 0.0010944122    -2.65 0.0002264521
#> 8        31       8      68.81 4.56 0.0009846570    -2.65 0.0002264521
#> 9        31       9      57.36 6.14 0.0012416905    -2.65 0.0002264521
#> 10       31      10      77.78 3.64 0.0008002829    -2.65 0.0002264521
#> 11       31      11      71.82 4.66 0.0011182325    -2.65 0.0002264521
#> 12       31      12      67.40 4.62 0.0009680546    -2.65 0.0002264521
#> 13       31      13      67.19 5.04 0.0011483674    -2.65 0.0002264521
#> 14       31      14      63.77 5.24 0.0011162936    -2.65 0.0002264521
#> 15       31      15      63.77 5.24 0.0011162936    -2.65 0.0002264521
#> 16       31      16      71.01 4.92 0.0012183484    -2.65 0.0002264521
#> 17       31      17      64.74 5.61 0.0013196654    -2.65 0.0002264521
#> 18       31      18      67.53 4.98 0.0011304750    -2.65 0.0002264521
#> 19       31      19      69.59 4.75 0.0010910254    -2.65 0.0002264521
#> 20       31      20      66.19 4.93 0.0010657849    -2.65 0.0002264521
#> 21       31      21      74.48 4.22 0.0009901083    -2.65 0.0002264521
#> 22       31      22      65.73 4.95 0.0010577094    -2.65 0.0002264521
#> 23       31      23      72.55 4.31 0.0009763680    -2.65 0.0002264521
#> 24       31      24      64.47 5.29 0.0011629402    -2.65 0.0002264521
#> 25       31      25      72.02 4.49 0.0010442174    -2.65 0.0002264521
#> 26       31      26      69.19 4.59 0.0010103530    -2.65 0.0002264521
#> 27       31      27      69.81 4.52 0.0009942335    -2.65 0.0002264521
#> 28       31      28      67.00 4.93 0.0010893812    -2.65 0.0002264521
#> 29       31      29      69.12 4.68 0.0010464620    -2.65 0.0002264521
#> 30       31      30      65.98 5.16 0.0011571856    -2.65 0.0002264521
#> 31       31      31      60.87 5.57 0.0011507973    -2.65 0.0002264521
#> 32       31      32      69.04 4.77 0.0010852364    -2.65 0.0002264521
#> 33       31      33      69.89 4.81 0.0011314799    -2.65 0.0002264521
#> 34       31      34      67.80 4.81 0.0010650045    -2.65 0.0002264521
#> 35       31      35      64.32 5.48 0.0012405949    -2.65 0.0002264521
#> 36       31      36      68.29 4.76 0.0010564139    -2.65 0.0002264521
#> 37       31      37      68.00 4.85 0.0010881360    -2.65 0.0002264521
#> 38       31      38      70.05 4.66 0.0010650910    -2.65 0.0002264521
#> 39       31      39      69.38 4.60 0.0010166311    -2.65 0.0002264521
#> 40       31      40      66.67 5.19 0.0011948925    -2.65 0.0002264521
#>    Est_corrected Var_corrected RSE_corrected
#> 1          62.57      0.001459          6.10
#> 2          69.04      0.001153          4.92
#> 3          64.61      0.001214          5.39
#> 4          61.16      0.001326          5.95
#> 5          61.03      0.001444          6.23
#> 6          70.68      0.001313          5.13
#> 7          61.94      0.001321          5.87
#> 8          66.16      0.001211          5.26
#> 9          54.71      0.001468          7.00
#> 10         75.13      0.001027          4.27
#> 11         69.17      0.001345          5.30
#> 12         64.75      0.001195          5.34
#> 13         64.54      0.001375          5.75
#> 14         61.12      0.001343          6.00
#> 15         61.12      0.001343          6.00
#> 16         68.36      0.001445          5.56
#> 17         62.09      0.001546          6.33
#> 18         64.88      0.001357          5.68
#> 19         66.94      0.001317          5.42
#> 20         63.54      0.001292          5.66
#> 21         71.83      0.001217          4.86
#> 22         63.08      0.001284          5.68
#> 23         69.90      0.001203          4.96
#> 24         61.82      0.001389          6.03
#> 25         69.37      0.001271          5.14
#> 26         66.54      0.001237          5.29
#> 27         67.16      0.001221          5.20
#> 28         64.35      0.001316          5.64
#> 29         66.47      0.001273          5.37
#> 30         63.33      0.001384          5.87
#> 31         58.22      0.001377          6.37
#> 32         66.39      0.001312          5.46
#> 33         67.24      0.001358          5.48
#> 34         65.15      0.001291          5.52
#> 35         61.67      0.001467          6.21
#> 36         65.64      0.001283          5.46
#> 37         65.35      0.001315          5.55
#> 38         67.40      0.001292          5.33
#> 39         66.73      0.001243          5.28
#> 40         64.02      0.001421          5.89
```
