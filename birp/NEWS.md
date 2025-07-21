# birp 0.0.5

Addressed CRAN installation failure on Fedora server:
 
 * Removed dependency on R library (which fails to be located on CRAN Fedora server)

# birp 0.0.4

Addressed CRAN installation failure on Fedora server:

* Added more search paths for locating R include directories (needed to link C++ libraries against R)

Added vignette and improved function documentations.

# birp 0.0.3

Addressed CRAN comments, specifically:

* Added more contributors and inst/AUTHORS to explain code contributions in detail.
* Replaced cat() by message().

# birp 0.0.2

Addressed CRAN comments, specifically:

* Improved DESCRIPTION
* Use TRUE and FALSE instead of T and F, define \value everywhere, removed \dontrun{}, added examples in examples to inst/extdata.
* Added a verbose argument that allows to turn off messages to the console completely.

# birp 0.0.1

* Initial CRAN submission.
