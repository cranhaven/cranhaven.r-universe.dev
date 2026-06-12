combinIT 2.0.1
==============

Maintenance Update
------------------
- Updated compiler standard from C++11 to **C++17** to ensure compatibility with recent versions of **RcppArmadillo** (≥ 14.6.3).
- Added the compiler flag `-DARMA_USE_CURRENT` to avoid fallback compilation warnings during CRAN checks.
- No user-visible changes; all package functions and results remain identical to version 2.0.0.




combinIT 2.0.0
===========

Updates
-------
-    In the name of two-part functions, '.' changed to '_'. For example, the function
     'Boik.test' changed to `Boik_test`.

-    All reports of tests are tidy and justified.

-    The function 'interactionplot' changed to 'interaction_plot'

-    The function `CPI_test` changed to 'CI_test'.

New Additions
-------------

-   The function 'justify' were added as utiles functions to make tidy the reports.

-   Some unit tests were added to the package.


combinIT 2.0.0
===========

Updates
-------
-    In the name of two-part functions, '.' changed to '_'. For example, the function
     'Boik.test' changed to `Boik_test`.

-    All reports of tests are tidy and justified.

-    The function 'interactionplot' changed to 'interaction_plot'

-    The function `CPI_test` changed to 'CI_test'.

New Additions
-------------

-   The function 'justify' were added as utiles functions to make tidy the reports.

-   Some unit tests were added to the package.


Minor improvements and bug fixes
--------------------------------

-   Fixed some bugs in `CI_test`, `Boik_test`, `Piepho_test`, 'KKSA_test', 'KKM_test',      'Malik_test', and `Franck_test`. Some typos were corrected in function documents. In     addition, `ITtestClass_methods`, combtest_methods`, and `plots` were improved. 
