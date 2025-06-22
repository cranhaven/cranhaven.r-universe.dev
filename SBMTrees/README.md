# SBMTrees 
  <!-- badges: start -->
  [![R-CMD-check](https://github.com/zjg540066169/SBMtrees/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/zjg540066169/SBMtrees/actions/workflows/R-CMD-check.yaml)
  [![License: GPL-2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
  [![version](https://img.shields.io/badge/version-1.1-green.svg)](https://github.com/zjg540066169/SBMTrees)
  ![R](https://img.shields.io/badge/language-R-blue)
  ![C++](https://img.shields.io/badge/language-C%2B%2B-green)
  <!-- badges: end -->

The R package **SBMTrees** (Sequential imputation with Bayesian Trees Mixed-Effects models) implements a Bayesian non-parametric framework for imputing missing covariates and outcomes in longitudinal data under the Missing at Random (MAR) assumption. Its core model, the Bayesian Trees Mixed-Effects Model (BMTrees), extends Mixed-Effects BART by employing centralized Dirichlet Process (CDP) Normal Mixture priors, allowing it to handle non-normal random effects and errors, address model misspecification, and capture complex relationships. The package also includes two semiparametric variants, BMTrees_R and BMTrees_RE. Built on BMTrees, the longitudinal sequential imputation framework employs a Metropolis-Hastings (M-H) MCMC method to sequentially impute missing values by constructing univariate models in a fixed order, ensuring both simplicity and consistency with a valid joint distribution.

For more details on these models and their applications, please consult the following paper: "Nonparametric Bayesian Additive Regression Trees for Prediction and Missing Data Imputation in Longitudinal Studies".


## Installation
This package is based on `Rcpp`, `RcppArmadillo`, and `RcppDist`, please make sure these three packages can be installed.

This package can be installed from R CRAN:
```
install.packages("SBMTrees")
```
or Github:
```
require("devtools")
install_github("https://github.com/zjg540066169/SBMTrees")
library(SBMTrees)
```

## Models
This package is based on the mixed-effects model for longitudinal data: <img src="https://latex.codecogs.com/gif.latex?Y_{ij}=BART(X_{ij})+Z_{ij}b_i+\epsilon_{ij}" /> 

Different models impose different prior distributions on <img src="https://latex.codecogs.com/gif.latex?b_i"/> and <img src="https://latex.codecogs.com/gif.latex?\epsilon_{ij}" />. We also include the existing model Mixed-Effects BART (mixedBART) in this package.

<table>
   <tr>
      <th align="center">Models</th>
      <th align="center">Prior on random effects <img src="https://latex.codecogs.com/gif.latex?b_i" /> </th>
      <th align="center">Prior on random errors <img src="https://latex.codecogs.com/gif.latex?\epsilon_{ij}" /> </th>
   </tr>
   <tr>
      <td style="text-align:center" align="center" rowspan="1" colspan="1">BMTrees</td>
      <td style="text-align:center" align="center" colspan="1" rowspan="1">CDP Multivariate Normal Mixture</td>
      <td style="text-align:center" align="center" colspan="1" rowspan="1">CDP Normal Mixture</td>
   </tr>
   <tr>
      <td style="text-align:center" align="center" rowspan="1" colspan="1">BMTrees_R</td>
      <td style="text-align:center" align="center" colspan="1" rowspan="1">Multivariate Normal</td>
      <td style="text-align:center" align="center" colspan="1" rowspan="1">CDP Normal Mixture</td>
   </tr>
   <tr>
      <td style="text-align:center" align="center" rowspan="1" colspan="1">BMTrees_RE</td>
      <td style="text-align:center" align="center" colspan="1" rowspan="1">CDP Multivariate Normal Mixture</td>
      <td style="text-align:center" align="center" colspan="1" rowspan="1">Normal</td>
   </tr>
   <tr>
      <td style="text-align:center" align="center" rowspan="1" colspan="1">mixedBART</td>
      <td style="text-align:center" align="center" colspan="1" rowspan="1">Multivariate Normal</td>
      <td style="text-align:center" align="center" colspan="1" rowspan="1">Normal</td>
   </tr>
</table>

The inference is done with posterior samples by Gibbs samplers in C++. 


## Usage
There are two main functions in this package. `BMTrees_prediction` is employed to estimate and predict longitudinal outcomes. `sequential_imputation` is used to multiply-impute longitudinal missing covariates and outcomes. 

### Prediction
We first generate a data with some individuals, each has 6 follow-up time points. As described in paper, we can specify if the linear/nonlinear associations, normal/non-normal random effects and random error. For each subject, one to three time points were randomly chosen to form the testing dataset, while the remaining time points constituted the training dataset. The testing dataset accounted for roughly 40% of the total data.

This can be achieved by running the function `simulation_prediction(n_subject = 800, seed, nonlinear, nonrandeff, nonresidual)`. Here is an example:
```
data = simulation_prediction(n_subject = 800, seed = 1234, nonlinear = TRUE, nonrandeff = TRUE, nonresidual = TRUE) 
X_train = data$X_train # get predictors in training set
Y_train = data$Y_train # get outcomes in training set
Z_train = data$Z_train # get random predictors in training set
subject_id_train = data$subject_id_train # get subject id in training set

X_test = data$X_test # get predictors in testing set
Y_test = data$Y_test # get outcomes in testing set
Z_test = data$Z_test # get random predictors in testing set
subject_id_test = data$subject_id_test # get subject id in testing set

Y_test_true = data$Y_test_true # get ground truth
```

After we get data, we can run the prediction model based on function `BMTrees_prediction(X_train, Y_train, Z_train, subject_id_train, X_test, Z_test, subject_id_test, model = c("BMTrees", "BMTrees_R", "BMTrees_RE", "mixedBART"), binary = FALSE, nburn = 3000L, npost = 4000L, skip = 1L, verbose = TRUE, seed, tol = 1e-20, resample = 5, ntrees = 200, pi_CDP = 0.99)`. 

Here is an example to run the predictive model.
```
model = BMTrees_prediction(X_train, Y_train, Z_train, subject_id_train, X_test, Z_test, subject_id_test, model = "BMTrees", binary = FALSE, nburn = 3000L, npost = 4000L, skip = 1L, verbose = TRUE, seed = 1234)
model$post_predictive_y_test
model$post_sigma
```
The users can get the posterior predictive samples for Y_test and posterior draws of other parameters.


### Multiple Imputation
For imputation, we first generate a dataset comprising individuals, each with six follow-up time points. As described in the paper, we can specify whether the random effects and random errors follow normal or non-normal distributions. Different missingness mechanisms are applied to create MAR missing values, resulting in approximately 35% of the observations having missing data.

The data with missingness is generated by running the function `simulation_imputation = function(n_subject = 800, seed, nonrandeff, nonresidual, alligned = F)`. Here is an example:
```
data = simulation_imputation(n_subject = 800, seed = 1234, nonrandeff = TRUE, nonresidual = TRUE, alligned = F) 
X_mis = data$X_mis # get missing covariates
Y_mis = data$Y_mis # get missing outcomes
Z = data$Z # get random predictors
subject_id = data$subject_id  # get subject id
time = data$time # get time point

X_O = data$X_O # get the original covariates matrix which is complete, for evaluation
Y_O = data$Y_O # get the original outcome which is complete, for evaluation
```

After we get data, we can run the prediction model based on function `sequential_imputation(X_mis, Y_mis, Z, subject_id, type, binary_outcome, model = c("BMTrees", "BMTrees_R", "BMTrees_RE", "mixedBART"), nburn = 3000L, npost = 4000L, skip = 200L, verbose = TRUE, seed, tol = 1e-20, resample = 5, ntrees = 200, reordering = T, pi_CDP = 0.99)`. 

Here is an example to run the predictive model.
```
model = sequential_imputation(X_mis, Y_mis, Z, subject_id, rep(0, 9), F, model = "BMTrees", nburn = 3000L, npost = 4000L, skip = 200L, verbose = TRUE, seed = 1234)
model$imputed_data
model$imputed_data[,,10] # get imputed outcomes.
```
The returned `imputed_data` is a three-array, whose dimension is (npost / skip, N, p + 1). N is the number of observations. p is the number of covariates.


## Attribution

This package includes code derived from the [BART3](https://github.com/rsparapa/bnptools/tree/master) package, originally developed by Rodney Sparapani. 

The original source code, licensed under the [GNU General Public License version 2 (GPL-2)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html), has been modified as follows:
- We include part of the C++ code in BART3, primarily about functions about `wbart` and `cpwart`. We also modify some files to make sure our package can be successfully compiled.
- Modifications were made by Jungang Zou, 2024.

### Licensing

- The original BART3 package is licensed under the GNU General Public License version 2 (GPL-2).
- This package, as a derived work, is also licensed under the GNU General Public License version 2 (GPL-2) to comply with the licensing terms.



