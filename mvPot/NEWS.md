# mvPot 0.1.6
* Fix an compilation issue on Fedora.

# mvPot 0.1.5

* Exposed number of replications for integration.
* Fix to censoredLikelihoodXS for cases when likelihood != "mgp".
* New option for censoredLikelihoodBR (matrix of data, Poisson and binomial likelihood).
* Updated documentation.
* Added reference to tutorial.

# mvPot 0.1.4

* New functions censoredLikelihoodXS and rExtremalStudentParetoProcess. 
* Censored likelihood for Brown-Resnick renamed censoredLikelihoodBR.
* New efficient simulation algorithm for Pareto process.
* Remove deprecated method in C code.
* Correction of typos.

# mvPot 0.1.3

* Correction of cluster definition condition for parallel computing.
* Addition of mvTProbQuasiMonteCarlo for efficient quasi Monte Carlo estimation of t distribution functions.

# mvPot 0.1.2

* Modification of genVecQMC to return previous prime integer and ready to use generating vector (no division by p is needed).
* Replaced calls to erf by pnorm in C++ library.
* mvtNormCpp is now registered when package is loaded.


# mvPot 0.1.1

* Added a `NEWS.md` file to track changes of the package.
* Fix compilation issues with Solaris distribution.
* Slight modifications on the documentation.



