# combinIT: A Combined Interaction Test for Unreplicated Two-Way Tables
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/combinIT)](https://cran.r-project.org/package=combinIT)
[![License: GPL v2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

There are several non-functional-form-based interaction tests for testing interaction in unreplicated two-way layouts.
However, no single test can detect all patterns of possible interaction and the tests are sensitive to a particular pattern of interaction.
This package combines six non-functional-form-based interaction tests for testing additivity.

# Summary

These six tests were proposed by Boik (1993), Piepho (1994), Kharrati-Kopaei and Sadooghi-Alvandi (2007), Franck et al. (2013), Malik et al. (2016)
and Kharrati-Kopaei and Miller (2016). The p-values of these six tests are combined by Bonferroni, Sidak, Jacobi polynomial expansion, and the Gaussian
copula methods to provide researchers with a testing approach which leverages many existing methods to detect disparate forms of non-additivity.
This package is based on the following published paper: Shenavari and Kharrati-Kopaei (2018) 
**A Method for Testing Additivity in Unreplicated Two-Way Layouts Based on Combining Multiple Interaction Tests**. 
In addition, several sentences in help files or descriptions were copied from that paper.


# Updates of the Version 2.0.0

-    In the name of two-part functions, '.' changed to '_'. For example, the function
     'Boik.test' changed to `Boik_test`.

-    All reports of tests are tidy and justified.

-    The function 'interactionplot' changed to 'interaction_plot'

-    The function `CPI_test` changed to 'CI_test'.

# New Additions

-   The function 'justify' were added as utiles functions to make tidy the reports.

-   Some unit tests were added to the package.


# Minor improvements and bug fixes in last version

Some bugs in `CI_test`, `Boik_test`, `Piepho_test`, 'KKSA_test', 'KKM_test' are Fixed, 
'Malik_test', and `Franck_test`. Some typos were corrected in function documents. In addition, `ITtestClass_methods`, combtest_methods`, and `plots` were improved. 

# Installation from GitHub

You can install this R package from this GitHub repository with:

``` r
# install.packages("remotes")
remotes::install_github("haghbinh/combinIT")
```
