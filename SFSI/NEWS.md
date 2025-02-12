### Version 1.4.1 (Aug-23-2024)

  - Some warnings were fixed in the documentation as required by CRAN.
  - Fixed bugs in functions 'fitBLUP', 'SGP', 'getGenCov': an error was produced if 'y' has 2 dimensions but it is a 'data.frame'. This was fixed by using 'as.matrix(y)'
  - Fixed bug in 'multitrait.plot' function
  - Fixed bug in checkpoint in function 'fitBLUP': an error was produced whenever Z = NULL & K = NULL & ntraits > 1. This error is not produced if an EVD is provided

### Version 1.4 (Jun-19-2024)

  - Changes in function names: SSI -> SGP,  SSI.CV -> SGP.CV. Results from both functions are of the class "SGP" standing for "sparse genomic prediction"
  - Training and testing sets in function 'SGP' can now be defined using integer vectors as 'SGP(...,trn, tst)'. In the former version this was defined as 'SSI(..., trn_tst)', where 'trn_tst' was be a vector with 0's (for tst) and 1's (for trn)
  - In cross-validation, training set is defined as 'SGP.CV(...,trn)'. In the former version this was 'SSI.CV(...,trn_tst)'
  - Method 'fitted' is replaced by method 'predict'
  - A multi-trait analysis can be performed using the function 'SGP' with argument 'y' being either a matrix or a vector. In the later case, different genotypes and traits are specified by arguments 'ID_geno' and 'ID_trait', respectively
  - Likewise, for the 'getGenCov', arguments 'ID_geno' and 'ID_trait' can be also used if argument 'y' is a vector
  - Likewise, for the within-trait analysis of the function 'fitBLUP', arguments 'ID_geno' and 'ID_trait' can be also used if argument 'y' is a vector
  - Eigenvalues (d) and eigenvectors (U) in functions 'fitBLUP' and 'getGenCov' can be passed as argument 'EVD' being a list as per the function 'eigen'

### Version 1.3.1 (Nov-17-2023)

  - Old dependencies R-packages were removed
  - Calls to functions 'Kronecker' and 'Kronecker_cov' from the tensorEVD R-package were added
  - Functions to work with triangular matrices were removed


### Version 1.3.0 (Aug-15-2023)

**New features**

  - Function 'fitBLUP' allows the solution of the mixed model for multiple traits when input 'y' has more than one column
  - Function 'getGenCov' allows the calculation of all pairs of columns of input 'y' so a genetic covariance matrix can be formed
  - Function 'SSI' is extended to the multi-trait case if input 'y' has more than one column. In this case within-trait genetic/residual covariances varU and varE are calculated using function 'getGenCov' when are not provided
  - Arguments 'trn' and 'tst' in function 'SSI(...,trn,tst)' can be be now passed as 'SSI(...,trn_tst)', where 'trn_tst' can be a vector with 0's (for tst) and 1's (for trn)
  - New function 'prune' added (see manual)
  - New functions to work with triangular matrices added (see manual)

### Version 1.2.0 (Aug-16-2022)

**New features**

  - Functions 'solveEN' and 'LARS' allow solving several regressions by iterating over columns of argument 'Gamma'
  - Function 'SSI' allows either saving or returning the coefficients through 'save.beta' and 'return.beta' arguments
  - Function 'SSI' returns also genetic values 'u' of testing subjects
  - Methods 'summary', 'fitted', and 'plot' can be implemented for a desired response variable 'y' different from the specified in object$y, e.g., fitted(object, y)


### Version 1.1.0 (Mar-10-2022)

**New features**

  - Some problems were fixed in the documentation structure as required by CRAN.
  - Functions 'lars2', 'SSI_CV', 'plotNet', 'plotPath' changed their names to 'LARS', 'SSI.CV', 'net.plot', and 'path.plot', respectively.
  - Some arguments' functions changed their names to a more informative name (e.g., 'minLambda' => 'lambda.min')
  - More functionalities added to 'net.plot' function


### Version 1.0.1 (Jan-26-2022)

**New features**

  - Functions 'SSI' and 'SSI_CV' allow providing either 'theta' (residual/genetic variances ratio) or the 'h2' (heritability)

**Bug fixes**

  - C-based routine associated to the 'readBinary' function now uses the 'Rf_allocMatrix' method to handle matrices whose length (number of rows x number of columns) exceed 2^31-1 = 2147483647


### Version 1.0.0 (Sep-30-2021)

**New features**

  - Function 'solveEN' allows early stop when a user-provided number of non-zero predictors (at a given value of lambda) is reached (argument 'maxDF')
  - Functions 'solveEN' and 'lars2' return object 'beta' as matrix with predictors in rows (rather than in columns)
  - Function 'cov2cor2' allows multiplying the resulting correlation matrix times a constant 'a' (default is 'a=1')
  - Provided 'wheatHTP' dataset includes now an array of 4-folds partitions ('CV' column in object 'Y') and calculations of genetic and residual covariances between YLD and each of the wavelengths ('genCOV_xy' and 'resCOV_xy' objects), and among YLD from each environment ('genCOV_yy' object). Residuals covariances among YLD from each environment ('resCOV_yy' object) is also included

**Bug fixes**

  - Function 'fitBLUP' performs the new checking varU <= 2*var(y) to declare a possible error if FALSE
  - Function reshape2::melt is used instead of reshape::melt


### Version 0.4.0 (May-12-2021)

**New features**

  - More detailed functions' documentation
  - Function 'fitBLUP' performs a quality control for very small or negatives eigenvalues
  - Function 'saveBinary' does not save columns' nor rows' names anymore
  - Function 'SSI' uses now a C-based routine called 'add2diag' created to add a numeric value to the diagonal of a symmetric matrix (single or double precision). This routine is not at the user level
  - Function 'getGenCov' has the argument 'warn' to whether show warnings from 'BLUP' analyses

**Bug fixes**

  - All C-based routines: a 'long long' variable type, instead of an 'int' type, was used for indexing arrays (matrices). This change allows dealing with matrices whose length (number of rows x number of columns) exceed 2^31-1 = 2147483647 (e.g., a matrix of 46341 x 46341)  


### Version 0.3.0 (Apr-29-2021)

**Features**

- First released version
- Function 'solveMixed' (from GitHub version) was renamed to 'fitBLUP'
