## 1.0.1

### New features
- New `vcov` method

### Bug fixes
- Adapts `smooth_scam` function to new version of package `gratia`
- Fixes names in confidence intervals from `confint.cgaim`

## 1.0.0

### Changes
- Expanded documentation
- Merged all control parameters into a single one. Added function `control.cgaim`

### Bug fixes
- Fixed bug with select in `predict.cgaim` and `plot.cgaim`
- Fixed reporting active constraints from `osqp`
- Changed derivative function for `gam` (previous was deprecated)
- Bug in the use of `foreach` in `boot.cgaim`
- The main function now checks for irreducibility of the constraint matrix.
- Fixed crash when a wrong argument is given in `acons`.

## 0.4.00 - 2022-05-28

### New features
- Confidence intervals through normal approximation.
- New function dedicated to bootstrap resampling. Result can be used in other functions such as `confint`.

### Changes
- Changes in bootstrap confidence intervals. Evaluated at index values.
- Parallel through the `doParallel` package, made easier.
- QR decomposition for alpha updating to stabilize it.
- Internal change in function organization.

## 0.3.05

### Bug fixes
- Fixed bug related to model frame with covariates

## 0.3.04

### Changes
- Changed default solver to `osqp`.

### Bug fixes
- Fixed bug in replication of `bvec` when `Cmat` is passed through alpha_control.

## 0.3.03

### Bug fixes
- Fixed issue in the finding of active constraints in `quadprog` for `edf` (yes, again).

## 0.3.02

### Bug fixes
- Fixed issue in the finding of active constraints in `quadprog` for `edf`.

## 0.3.01

### Changes
- Slight change in convergence handling: now possibility to choose between convergence in RSS or coefficients.

### Bug fixes
- Fixed bug caused by naming variables in `g`.

## 0.3.00

### New features
- Add computation of effective degrees of freedom.
- Add GCV computation.
- Add `ctol` parameter to add a small margin to constraints.

### Changes
- Names of algorithm controlling arguments changed with specific help pages added.
- Internal reorganization of functions.

## 0.2.04

### Bug fixes
- Bug for the predict function when some data come from matrices.

## 0.2.03

### Bug fixes
- Fixed computation of bootstrap confidence intervals for ridge functions `g`.
- Got back to new form of documentation.

## 0.2.02

### Changes
- Restricted the computation of confidence intervals to percentile bootstrap. The others need a proper implementation.

### Bug fixes
- Removed the computation of all standard errors from the main function `cgaim`. They were half-baked and caused more problems than were useful.
- Fixed issues on the documentation

## 0.2.01

### Changes
- Now uses `mgcv::gam` instead of `scam` when no smoothing constraints are given. Doesn't apply to `scar` and `cgam` yet.

## 0.2.0

### New features
- Custom function `s` for non-index smooth terms. Maps to the right function depending on the function used at the GAM step.
- Possibility to use package `cgam` for constrained smooths.

### Changes
- Change of argument names in `g`.
- Warning messages for convergence failures.

### Bug fixes
- Fixes a bug related to argument `select` in `plot.cgaim`
- Fixes a bug related to the computation of covariance matrices after estimation
- Fixes bug occurring for single-index models without covariate

## 0.1.1

### Function 'g'
- argument 'label' now defaults to the name of the first variable given
- now takes list of matrices to facilitate calling the function
- smarter attribution of names inside each index

### Bug fixes
- fixes the mixing of names in elements `gfit` and `beta` from the output