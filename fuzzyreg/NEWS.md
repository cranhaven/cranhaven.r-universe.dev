## fuzzyreg v. 0.6.2, build 2023-03-10

- Fixed methods table rendering in the html vignette

## fuzzyreg v. 0.6.1, released 2023-03-09

- Fixed bugs in `fuzzify` and `predict.fuzzylm` to account for multiple variables in argument `x`
- Added LETTERS as default predictor variable names in `fuzzify`
- Updated vignette to include BFRL method and display in html


## fuzzyreg v. 0.6, released 2022-03-02

- Added method BFRL
- Updated `plot.fuzzylm` to display partial fit in multiple regression models


## fuzzyreg v. 0.5.2, released 2021-11-11

- Updated citation
- Updated documentation
- Skipped TEF test on M1 Mac


## fuzzyreg v. 0.5.1, released 2019-06-06

- Added an option to silence warnings in `fuzzylm`
- Fixed bugs with multiple fuzzy regression models


## fuzzyreg v. 0.5, released 2019-02-06

- Added model comparison measures to summary
- Added method FLAR
- Added functions `GOF`, `fuzzify`, `coef`
- Updates in documentation
- Fixed bugs
- Removed functions deprecated in version 0.3


## fuzzyreg v. 0.4.1, released 2018-12-05

- Added `bats` data
- Minor fixes


## fuzzyreg v. 0.4, released 2018-10-15

- Added function `TEF` to estimate total error of fit for model comparisons
- Added function `dom` to calculate degree of membership for a real number to a triangular fuzzy number
- Added unit tests
- Added vignette


## fuzzyreg v. 0.3, released 2018-06-07

- Fixed bug in `flr` to ensure the sensitivity of parameter estimation to changes in argument `omega`
- Renamed fuzzy regression functions to their published names
- Deprecated fuzzy regression functions with names following the authors' names


## fuzzyreg v. 0.2, released 2018-04-23

- First public release of the package