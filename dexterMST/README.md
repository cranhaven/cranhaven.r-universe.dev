<!-- README.md is generated from README.Rmd. Please edit that file -->

# DexterMST

DexterMST is an R package acting as a companion to dexter and adding
facilities to manage and analyze data from multistage tests (MST). It
includes functions for importing and managing test data, assessing and
improving the quality of data through basic test and item analysis, and
fitting an IRT model, all adapted to the peculiarities of MST designs.
DexterMST typically works with project database files saved on disk.

## Installation

``` r
install.packages('dexterMST')
```

If you encounter a bug, please post a minimal reproducible example on
[github](https://github.com/dexter-psychometrics/dexter/issues). We post
news and examples on a
[blog](https://dexter-psychometrics.github.io/dexter/articles/blog/index.html),
it’s also the place for general questions.

## Example

Here is an example for a simple two-stage test.

``` r
library(dexterMST)
library(dplyr)
# start a project
db = create_mst_project(":memory:")

items = data.frame(item_id=sprintf("item%02i",1:70), item_score=1, delta=sort(runif(70,-1,1)))

design = data.frame(item_id=sprintf("item%02i",1:70),
                    module_id=rep(c('M4','M2','M5','M1','M6','M3', 'M7'),each=10))

routing_rules = routing_rules = mst_rules(
 `124` = M1[0:5] --+ M2[0:10] --+ M4, 
 `125` = M1[0:5] --+ M2[11:15] --+ M5,
 `136` = M1[6:10] --+ M3[6:15] --+ M6,
 `137` = M1[6:10] --+ M3[16:20] --+ M7)


scoring_rules = data.frame(
  item_id = rep(items$item_id,2), 
  item_score= rep(0:1,each=nrow(items)),
  response= rep(0:1,each=nrow(items))) # dummy respons
  

db = create_mst_project(":memory:")
add_scoring_rules_mst(db, scoring_rules)

create_mst_test(db,
                test_design = design,
                routing_rules = routing_rules,
                test_id = 'sim_test',
                routing = "all")
```

We can now plot the design

``` r
# plot test designs for all tests in the project
design_plot(db)
```

We now simulate data:

``` r
theta = rnorm(3000)

dat = sim_mst(items, theta, design, routing_rules,'all')
dat$test_id='sim_test'
dat$response=dat$item_score

add_response_data_mst(db, dat)
```

``` r
# IRT, extended nominal response model
f = fit_enorm_mst(db)

head(f)
```

| item_id | item_score |       beta |   SE_beta |
|:--------|-----------:|-----------:|----------:|
| item01  |          1 | -0.9091126 | 0.0627456 |
| item02  |          1 | -1.0254786 | 0.0629096 |
| item03  |          1 | -0.9740383 | 0.0628192 |
| item04  |          1 | -0.8511593 | 0.0627179 |
| item05  |          1 | -0.8545661 | 0.0627186 |
| item06  |          1 | -0.7966628 | 0.0627248 |

``` r
# ability estimates per person
rsp_data = get_responses_mst(db)
abl = ability(rsp_data, parms = f)
head(abl)
```

| booklet_id | person_id | booklet_score |      theta |
|:-----------|:----------|--------------:|-----------:|
| 124        | 1         |             1 | -3.8957687 |
| 136        | 10        |            18 |  0.6531690 |
| 124        | 100       |            14 | -0.6131689 |
| 124        | 1000      |            12 | -0.8930060 |
| 124        | 1001      |             7 | -1.6962625 |
| 124        | 1002      |             9 | -1.3464286 |

``` r
# ability estimates without item Item01
abl2 = ability(rsp_data, parms = f, item_id != "item01")

# plausible values
pv = plausible_values(rsp_data, parms = f, nPV = 5)
head(pv)
```

| booklet_id | person_id | booklet_score |        PV1 |        PV2 |        PV3 |        PV4 |        PV5 |
|:-----------|:----------|--------------:|-----------:|-----------:|-----------:|-----------:|-----------:|
| 124        | 1         |             1 | -2.1173109 | -2.8452992 | -1.8224958 | -2.2535875 | -2.7653987 |
| 124        | 100       |            14 | -0.9541693 | -0.3218994 | -0.5133103 | -0.7913669 | -0.6746883 |
| 124        | 1000      |            12 | -0.3432901 | -1.2258409 | -0.6831306 | -0.6257580 | -1.2091710 |
| 124        | 1001      |             7 | -1.3945366 | -1.7253245 | -1.6556693 | -1.4768176 | -1.3666073 |
| 124        | 1002      |             9 | -1.2284339 | -1.0867596 | -1.2299253 | -0.3411735 | -0.9039256 |
| 124        | 1003      |             4 | -1.9307668 | -2.9527556 | -1.7905466 | -1.6767628 | -2.4335210 |
