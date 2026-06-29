# BSL 3.2.1 (Release date: 2021-04)

* Added DBI to suggests.

# BSL 3.2.0

# BSL 3.1.0 (Release date: 2019-11)

* Added a new method ("BSLmisspec") in the main function "bsl", which considers model misspecification when running BSL.
* Added an option "whitening" in the main function "bsl", which allows greater shrinkage when using the Warton's shrinkage estimator.
* Re-designed S4 classes "BSL", "MODEL", and "PENALTY". More methods are supported in the classes.
* The package now uses a progress bar to track running progress in the "bsl" function (verbose = 1L).
* Other minor changes and bug fixes.

# BSL 3.0.0 (Release date: 2019-07-01)

* Added a new method ("uBSL") in the main function "bsl".
* Two shrinkage methods ("glasso" and "Warton") are available now for methods "BSL" and "semiBSL".
* A new S4 object class "BSLMODEL" is introduced to encapsulate the simulation function, summary statistic function, a point estimate of the parameter value and other arguments that are related to the model of interest. 
* "BSLMODEL" now uses log of the prior function for numerical stability.
* "BSLMODEL" supports vectorised simulation function ("fnSimVec") that produces n model simulations simultaneously.
* A number of arguments ("fnSim", "fnSum", "theta0", "fnPrior", "simArgs", "sumArgs" and "thetaNAmes") are replaced by "model" and will be deprecated in a future release.
* Added a function "gaussianRankCorr" that computes the Gaussian rank correlation matrix.
* It is now possible to use Gaussian rank correlation (GRC = TRUE) in the "BSL" method for more robustness.
* Other minor changes.


# BSL 2.0.0

* Second major release. Please note some arguments have been renamed.
* Added a new method ("semiBSL") in the main function "bsl".
* Now bsl returns an S4 class object instead of S3. The plot method gives an option for whether ggplot2 or R graphics (the default) will be used.
* The simulation function and summary statistics function do not need to have a list as the second argument now.
* The new bsl function uses "parallelArgs" instead of "parallel_packages", where users can specify all arguments supported by "foreach".
* Code update for three examples: MA(2), multivariate G & K, and cell biology.


# BSL 0.1.0

* Initial release
* Includes BSL and BSLasso
* Data and code for three examples: MA(2), multivariate G & K, and cell biology
