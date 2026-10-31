# TRES 1.1.5

## Major changes
- Revise the help documentation for the function `OptStiefelGBB.R`.
- The Journal of Statistical Software paper accompanying this package has been published.

# TRES 1.1.4

## Major changes
- We change the title from ``Tensor Regression with Envelope Structure and Three Generic Envelope Estimation Approaches'' to ``Tensor Regression with Envelope Structure''.

## Minor change
- Revert `res <- kroncov(En)` to `res <- try(kroncov(En))` in function `TRR.fit`, which is forgotten to be reverted in last version.


# TRES 1.1.3

## Major changes
- Replace the `base::norm(x, 'F')` function with `sqrt(sum(x^2))` for some matrix `x`.
- In the `Data` segment of the printout of the `tensor` object, instead of just the first few elements, the summarized results of all elements are printed.

## Minor changes
- Add more output information returned from `OptManiMulitBallGBB()` to the output of `ballGBB1D()`.

---

# TRES 1.1.2

## Major changes

- Renaming: `TRR_sim -> TRRsim; TPR_sim -> TPRsim; TensEnv_dim -> TRRdim; ballGBB1D_bic -> oneD_bic; TensPLS_cv2d3d -> TPRdim; OptimballGBB1D -> OptM1D; EnvMU -> simplsMU`
- Deprecate:
- `plot.Tenv`:
    - Correct the direction of y axis. The coordinates of x and y increase from left to right, top to bottom; 
    - Add arguments `xlab = ""`, `ylab = ""`, `axes = TRUE`, `ask = TRUE`, remove arguments `xticks`, `yticks`. `axes` is a logical value specifying whether the axes should be drawn. If `ask = TRUE`, user is prompted before the second plot is shown (if exists).
- `ECD, simplsMU, manifold1D, manifoldFG, OptM1D, OptMFG`: Change optional arguments like `maxiter`, `tol`, to three dots `...`
- `fun1D, get_ini1D, ballGBB1D, OptManiMulitBallGBB`: Collected into file "1Dfunction.R". Help documentation are no longer supported.
- `MenvU_sim`: An option `wishart = FALSE` is provided to return the population matrices `M` and `U`. Other argument like `jitter` is also provided which adds an scaler matrix to `M` to ensures it positive-definiteness.
- `PMSE`: Calculates the prediction and mean squared error for both TRR and TPR models.
- `kroncov`: Add convergence criterion `tol` and the maximal iteration `maxiter`.


## Minor changes
- `OptStiefelGBB`: Hide `out` from the output.
- `manifoldFG`: Set default value for `Gamma_init`.
- `ttt.R`: Change arguments `X,Y` to `x,y`.
- `oneD_bic`: Add the estimated envelope basis `Gamma` to the output.
- `TRRdim`: Output the mean squared error using the selected envelope basis.
- `TRRdim, oneD_bic`: Argument `multiD` is changed to `C` to comply with the paper Zhang X, Mai Q (2018). "Model-Free Envelope Dimension Selection." Electronic Journal of Statistics.

### New functions
- `OptMFG`: New FG optimization function encapsulating the core function `OptStiefelGBB`.
- `show`: overloads `show` in package `rTensor`. With the overloaded `show`, only  the first 6 elements of tensor is printed out.

## Bugs
- In documentation of `subspace()`, the formula of subspace distance should be ||P_{A} - P_{B}||_F/√{2d}.
- Fix bat dataset where `x` was not binary.

---

# TRES 1.1.1

## Major changes
- Lists or environments data structures are also acceptable as a whole in `TRR.fit()` and `TPR.fit()`. They can be passed to argument `x`.
- Since the variance-covariance matrix is not available for `Tenv` class object, we remove S3 methods `vcov.Tenv()`. Use function `std_err()` if the standard error for the tensor coefficient from `TRR.fit()` is desired.
- Improved the computation efficiency by adopting cholesky decomposition in matrix inversion and the calculation of the square root of a matrix. 
- Add a real data set `EEG`. Refer to R help documentation for more details
- Add a new feature to `predict.Tenv()`: if the argument `newdata` is missing, the fitted values from the fitted model is returned.

## Minor changes
- Change arguments `Xn` and  `Yn` in all functions to `x` and `y` in accordance with other popular functions, e.g., `lm()`, `glm()`, etc.
- `plot.Tenv()`: Change the name of argument `thrd` to `level`.
- `kroncov`: Data `Tn` is centered before the estimation. 

---

# TRES 1.1.0

## Small changes
- Add some references in Description field in file DESCRIPTION
- Fix the document of `summary.Tenv`.
- Remove the parameter `ask` in plot.Tenv. 
- Add envelope basis `Gamma` list into `bat` and `square` datasets.
- Change the names of some parameters, like `bic_max` to `maxdim` in `TensEnv_dim`, `max_iter` to `maxiter` in `manifold1D`, `epsilon` to `tol` in `ECD`, `G_ini` to `Gamma_init` in `manifoldFG`, `G_hat` to `Gamma` in `manifoldFG`, `Yhat` to `pred` in `PMSE`.
- Rename `TRR`, `TPR` as `TRR.fit`, `TPR,fit`.

## S3 methods
- `print.Tenv`: Prints the call, coefficients from `TPR` and `TRR`, make the output more concise.
- `print.summary.Tenv`: Print call, dimensions of X, Y, sample size, mse, the coefficient and p_value. (invoked implicitly when there is no assignment of `summary.Tenv`).

---

# TRES 1.0.0

## Major changes
- Switch the arguments `Xn` and `Yn` in functions: `TensEnv_dim`, `TensPLS_cv2d3d`, `Tenv`, `Tenv_Pval`, `TPR` and `TRR`.
- Removed package `mvtnorm` in `DESCRIPTION`, it is not used in the package.
- Move all the packages in `Depends` in `DESCRIPTION` to `Imports` except for `ManifoldOptim`.
- Add examples to some functions: `PMSE`, `TensEnv_dim`, `TensPLS_cv2d3d`, `Tenv_Pval`.
- Add simulated data "bat" and "square" with which users can quickly verify the functions in the package.
```
# Usage
> data("bat")
> data("square")
```
- `TPR`, `TRR`: 
  - Change the output names of some functions. E.g., `Bhat` in the object returned from `TPR` and `TRR` is renamed as `coefficients`.
  - Append `fitted.values` and `residuals` into the output.
  - More robust to the input type: `Yn` can be vector or matrix and `Xn` can be matrix, array or tensor for `TPR`, `Xn` can be vector or
  matrix and `Yn` can be matrix, array or tensor for `TRR`.
  - Incorporate `FG_TRR` and `FG_TPR` into `TRR` and `TPR`. Add one more option for `method`, "FG". Also add argument `Gamma_init` for "FG" method. 
- `Tenv_Pval`: Change the data type of outputs from array to tensor. Change the name `se_mat` to `se`.
- `PMSE`: Rewrite `PMSE`. `Xn` can be matrix, array or tensor, `Yn` can be vector or matrix, and `Bhat` can be vector, matrix, array or tensor
as long as the dimensions match the ones of `Xn` and `Yn`.
- `TRR_sim`, `TPR_sim`: Add two simulation functions to generate data used in TRR and TPR which can help user quickly test the functions.

## S3 methods
Construct S3 object for `TRR` and `TPR` with `class` attribute "Tenv"
```
> data("bat")
> fit <- TRR(bat$Xn, bat$Yn, method = "standard")
> class(fit)
> [1] "Tenv"
```
- `predict.Tenv`: Make predictions of new data.
- `summary.Tenv`: Append dimensions of X,Y, sample size, mse, p_val and s.e. to the output object from `TPR` and `TRR`.
- `plot.Tenv`: Draw the plot of coefficients from `TRR` and `TPR`, and draw p_val plot from `TRR`. 
- `vcov.Tenv`: No covariance for coefficients. But for `TRR`, we print the standard error for coefficients.
- `fitted.default`: Calculates the fitted Y for `TPR` and `TRR` separately.
- `residuals.default`: Calculate Y minus fitted Y for `TPR` and `TRR`
- `coef.default`: Print the coefficients for `TPR` and `TRR`.

## Deprecated functions
- `FG_TPR`, `FG_TRR`
>  Help pages for deprecated functions are available at `help("TRES-deprecated")`.

## Bug fixed
- Default value `u=NULL` for `TPR` and `TRR`.
- Default value `maxdim=10` and `nfold=5` for `TensPLS_cv2d3d`
- `PMSE`: now accept tensor `Xn`
