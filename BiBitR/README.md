
<!-- README.md is generated from README.Rmd. Please edit that file -->
Installing the BiBitR Package
-----------------------------

#### Release

``` r
install.packages("BiBitR")
```

#### Development

``` r
install.packages("devtools") # If not yet installed on your R Version
devtools::install_github("hadley/devtools") # Only run this if your currently installed 
                                            # devtools version is <= 1.12 (recursive dependencies bug)

devtools::install_github("ewouddt/BiBitR")
```

Should the installation of `BiBitR` or `devtools::install_github("hadley/devtools")` throw an error, please install the dependencies manually, then try:

``` r
install.packages(c("flexclust","biclust"))
devtools::install_github("ewouddt/BiBitR")
```

Details - BiBit
---------------

`BiBitR` is a simple R wrapper which directly calls the original Java code for applying the BiBit algorithm after which the output is transformed to a `Biclust` S4 class object. The original Java code can be found at <http://eps.upo.es/bigs/BiBit.html> by Domingo S. Rodriguez-Baena, Antonia J. Perez-Pulido and Jesus S. Aguilar-Ruiz.

More details about the **BiBit** algorithm can be found in:

-   [Domingo S. Rodriguez-Baena, Antonia J. Perez-Pulido and Jesus S. Aguilar-Ruiz (2011), "A biclustering algorithm for extracting bit-patterns from binary datasets", *Bioinformatics*](http://bioinformatics.oxfordjournals.org/content/early/2011/08/08/bioinformatics.btr464.abstract).

The `bibit` function uses the original Java code directly (with the intended input and output). Because the Java code was not refactored, the `rJava` package could not be used.

The `bibit` function does the following:

1.  Convert R matrix to a `.arff` output file.
2.  Use the `.arff` file as input for the Java code which is called by `system()`.
3.  The outputted `.txt` file from the Java BiBit algorithm is read in and transformed to a `Biclust` object.

Because of this procedure, there is a chance of *overhead* when applying the algorithm on large datasets. Make sure your machine has enough RAM available when applying to big data.

**Note:**
If you want to circumvent the internal R function to convert the matrix to `.arff` format, look at the documentation of the `arff_row_col` parameter of the `bibit` function. You can input the original input files for the java algorithm with this parameter. The original input files can also be generated with the `make_arff_row_col` function in the package.

Details - BiBit with Noise Allowance
------------------------------------

The BiBit algorithm was also slightly adapted to allow some noise in the biclusters. This can be done with the `bibit2` function in the `BiBitR` package. It is the same function as `bibit`, but with an additional new noise parameter which allows 0's in the discovered biclusters.

`bibit2` follows the same steps as described in the Details section above. Following the general steps of the BiBit algorithm, the allowance for noise in the biclusters is inserted in the original algorithm as such:

1.  Binary data is encoded in bit words.
2.  Take a pair of rows as your starting point.
3.  Find the maximal overlap of 1's between these two rows and save this as a pattern/motif. You now have a bicluster of 2 rows and N columns in which N is the number of 1's in the motif.
4.  Check all remaining rows if they match this motif, *however* allow a specific amount of 0's in this matching as defined by the `noise` parameter. Those rows that match completely or those within the allowed noise range are added to bicluster.
5.  Go back to *Step 2* and repeat for all possible row pairs.

**Note:** Biclusters are only saved if they satisfy the `minr` and `minc` parameter settings and if the bicluster is not already contained completely within another bicluster.

What you will end up with are biclusters not only consisting out of 1's, but biclusters in which 2 rows (the starting pair) are all 1's and in which the other rows could contain 0's (= noise).

**Note:** Because of the extra checks involved in the noise allowance, using noise might increase the computation time a little bit.

The `noise` parameter determines the amount of zero's allowed in the bicluster (i.e. in the extra added rows to the starting row pair) and can take on the following values:

-   `noise=0`: No noise allowed. This gives the same result as using the `bibit` function.
-   `0<noise<1`: The noise parameter will be a noise percentage. The number of allowed 0's in a (extra) row in the bicluster will depend on the column size of the bicluster. More specifically `zeros_allowed = ceiling(noise * columnsize)`. For example for `noise=0.10` and a bicluster column size of `5`, the number of allowed 0's would be `1`.
-   `noise>=1`: The noise parameter will be the number of allowed 0's in a (extra) row in the bicluster independent from the column size of the bicluster. In this noise option, the noise parameter should be an integer.

Alternative Strategy
--------------------

Normally you would apply BiBit (with/without noise) directly on the binary data. However due to the nature of the algorithm, namely starting an exhaustive search from each row-pair, it is also possible to look for specific patterns of interest.

To do this, simply add 2 (artificial) identical rows which contain the pattern/motif of interest (e.g. 2 rows with 1's in specific columns and 0 everywhere else). You can do this multiple times if multiple patterns/motifs are of interest.

This procedure is currently implemented in the package in the `bibit3` function. It allows to drive the BiBit algorithm to only look for one or multiple full or sub patterns (which increases the speed). See the Documentation of `bibit3` for more info.

Column Extension Procedure
--------------------------

The package also allows you extend the bibit biclusters in the column dimension. More info can be found in the documentation of the `bibit2`, `bibit3` and `bibit_columnextension` functions. The goal of the procedure is:

-   *BiBit with Noise Allowance*: Try to add noise to the 2 initial perfect rows.
-   *Bibit With Patterns (Alternative Strategy)*: Do the rows, which overlap partly or fully with the given pattern, have other similarities outside the given pattern?

BiBit Workflow for larger, meaningful patterns
----------------------------------------------

Looking for Noisy Biclusters in large data using BiBit (`bibit2`) often results in many (overlapping) biclusters. In order decrease the number of biclusters and find larger meaningful patterns which make up noisy biclusters, the following workflow (`BiBitWorkflow`) can be applied.
Note that this workflow is primarily used for data where there are many more rows (e.g. patients) than columns (e.g. symptoms). For example the workflow would discover larger meaningful symptom patterns which, conditioned on the allowed noise/zeros, subsets of the patients share.

Return Value
------------

A Biclust S4 Class object.

Example
-------

``` r
library(BiBitR)

data <- matrix(sample(c(0,1),100*100,replace=TRUE,prob=c(0.9,0.1)),nrow=100,ncol=100)
data[1:10,1:10] <- 1   # BC1
data[11:20,11:20] <- 1 # BC2
data[21:30,21:30] <- 1 # BC3
data <- data[sample(1:nrow(data),nrow(data)),sample(1:ncol(data),ncol(data))]

result1 <- bibit(data,minr=2,minc=2)
result1
MaxBC(result1)

result2 <- bibit2(data,minr=5,minc=5,noise=0.2)
result2
MaxBC(result2)

result3 <- bibit2(data,minr=5,minc=5,noise=3)
result3
MaxBC(result3)
```
