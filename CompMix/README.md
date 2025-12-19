# CompMix

## A comprehensive toolkit for environmental mixtures analysis

* **Please make sure you have installed JAVA on your computer.If not please visit www.java.com**


* **Install all the dependency R packages**
```{r}
install.packages(c("mvtnorm","igraph","glmnet","gglasso","higlasso",
                   "hierNet","caret","e1071",'rJava',
                   'bartMachine', 'SuperLearner', 'gam',
                   'ipred', 'bartMachineJARs', 'car',
                   'missForest', 'itertools', 'iterators',
                   'xgboost', 'bkmr', 'qgcomp', 'gWQS',
                   'matrixStats', 'pROC',"devtools"))
devtools::install_github("umich-cphds/snif")
```

* **Install and load the package**
```{r}
devtools::install_github("haowei72/CompMix")
library(CompMix)
```



* **Overview**

 The users input the dataset consisting of outcome variable y, exposure variables x and covariates z, and specify test.pct to randomly split the dataset into training and testing datasets. By specifying interaction=TRUE, the Comp.Mix automatically calculates all the pairwise interactions among exposure variables. For users who wish to explore the interaction effects among some covariates and exposures, they can do so by including the specific covariates into the exposure variables x. 

* **Simulate data**
  ```{r}
  dat <- lmi_simul_dat(n=1000,p=20,q=5, block_idx=c(1,1,2,2,3,1,1,1,1,1,2,2,2,2,3,3,3,3,3,3), within_rho=0.6,btw_rho=0.1,R2=0.2, effect_size=1,effect_size_i=1, cancel_effect = FALSE)
  ```


* **Example 1**

The users would like to perform variable selections on main effects of exposures and covariates, and outcome, exposures and covariates are entered. For any individual interactions that the users would like to include in the models, they can add those into the covariate z.
```{r}
library(splines) # need to load splines to run snif
ex1 <- Comp.Mix(y=dat$y, x=dat$x, z=dat$z, test.pct=0.5, var.select = TRUE, interaction = FALSE, covariates.forcein = FALSE, bkmr.pip=0.5, seed=2023)
```
Results include exposures and covariates that are selected and their coefficients  by Lasso and Elastic-net, as well as sum-squared errors and correlations calculated from the testing data for model comparisons.

* **Example 2**

The users would like to perform variable selections on main effects of exposures and covariates, interactions among exposures. Outcome, exposures, and covariates are entered.  If the users would like to test interactions between certain covariate(s) and all the chemicals, they can move the covariate(s) from z to x. For any individual interactions that the users would like to include in the models, they can add those into the covariate(s) z.
```{r}
ex2 <- Comp.Mix(y=dat$y, x=dat$x, z=dat$z, test.pct=0.5, var.select = TRUE, interaction = TRUE, covariates.forcein = FALSE, bkmr.pip=0.5, seed=2023)
```
Results include exposures, interactions and covariates that are selected and their coefficients  by Lasso and Elastic-net, as well as sum-squared errors and correlations calculated from the testing data for model comparisons.


* **Example 3**

The users would like to perform variable selections on all main effects of exposures, and covariates, and all interactions among exposures and covariates. Outcome and exposures are entered, and covariates are blank, and the exposures x include all the covariates.  
```{r}
ex3 <- Comp.Mix(y=dat$y, x=dat$x, z=dat$z, test.pct=0.5, var.select = TRUE, interaction.exp.cov=TRUE, covariates.forcein = FALSE, bkmr.pip=0.5, seed=2023)
```
Results include exposures and covariates, and their interactions that are selected by Lasso and Elastic-net, HierNet and SNIF. Coefficients for the selected terms by Lasso and Enet. Sum-squared errors and correlations calculated from the testing data for model comparisons.


* **Example 4**

The users would like to perform variable selections on all main effects of exposures, while adjusting for covariates. Outcome, exposures, and covariates are entered. For any individual interactions that the users would like to adjust in the models, they can add those into the covariates z. 
```{r}
ex4 <- Comp.Mix(y=dat$y, x=dat$x, z=dat$z, test.pct=0.5, var.select = TRUE, interaction = FALSE, covariates.forcein = TRUE, bkmr.pip=0.5, seed=2023)
```
Results include exposures that are selected and their coefficients by Lasso and Elastic-net, exposures that are selected by BKMR, coefficients of covariates by Lasso and Elastic-net,  as well as sum-squared errors and correlations calculated from the testing data for model comparisons.


* **Example 5**

The users would like to perform variable selections on all main and interactions effects of exposures, while adjusting for covariates. Outcome, exposures, and covariates are entered. For any individual interactions that the users would like to adjust in the models, they can add those into the covariates z.
```{r}
ex5 <- Comp.Mix(y=dat$y, x=dat$x, z=dat$z, test.pct=0.5, var.select = TRUE, interaction = TRUE, covariates.forcein = TRUE, bkmr.pip=0.5, seed=2023)
```
Results include exposures and exposure interactions that are selected and their coefficients by Lasso and Elastic-net, covariate coefficient estimated by Lasso and Elastic-net. Sum-squared errors and correlations calculated from the testing data for model comparisons.

* **Example 6**

The users would like to rank the importance of exposure variables, while adjusting for covariates. Outcome, exposures, and covariates are entered. For any individual interactions that the users would like to adjust in the models, they can add those into the covariates z.
```{r}
ex6 <- Comp.Mix(y=dat$y, x=dat$x, z=dat$z, test.pct=0.5, var.select = FALSE, interaction = FALSE, covariates.forcein = TRUE, bkmr.pip=0.5, seed=2023)
```
Results include exposure ranking and mixture effect by WQS, positive and negative exposure effects and mixture effect by Q-gcomp. Sum-squared errors and correlations calculated from the testing data for model comparisons.

* **Example 7**

The users would like to rank the importance of exposure variables and covariates that are entered.  Interactions or other nonlinearity are accounted implicitly meaning that we would not know the selection of the any variables. 
```{r}
ex7 <- Comp.Mix(y=dat$y, x=dat$x, z=dat$z, test.pct=0.5, var.select = FALSE, interaction = FALSE, covariates.forcein = FALSE, bkmr.pip=0.5, seed=2023)
```
Results include ranking of the exposures and covariates by random forest as well as sum-squared errors and correlations calculated from the testing data for model comparisons.

* **Example 8**

The users would like to perform variable selections on main effects of exposures.  Outcome and exposures are entered while covariates are blank.
```{r}
ex8 <- Comp.Mix(y=dat$y, x=dat$x, z=dat$z, test.pct=0.5, var.select = TRUE, interaction = FALSE, covariates.forcein = FALSE, bkmr.pip=0.5, seed=2023)
```
Results include exposures that are selected and their coefficients  by Lasso and Elastic-net, exposures that are selected by BKMR, as well as sum-squared errors and correlations calculated from the testing data for model comparisons.

  ```
