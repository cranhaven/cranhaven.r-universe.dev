# rswipl: Embed SWI-Prolog into an R package

_Matthias Gondan, Department of Psychology, Universität Innsbruck, Austria_

Supported by the Erasmus+ programme of the European Commission, 2019-1-EE01-KA203-051708

The purpose of this package is to embed SWI-Prolog into an R library,
such that other packages can link to the SWI-Prolog runtime without the need
to install the program on their computer. This R package is *not* meant to
be used directly. Please use the R package `rolog` instead. Install this
package if you do not have the administrative privilege to install SWI-Prolog
on your computer.

## License

This R package is distributed under FreeBSD simplified license. SWI-Prolog is 
distributed under its own license (BSD-2).

## Installation

Please use R version >= 4.2. The package is on CRAN, it can be installed using 
`install.packages("rswipl")` from the R environment.

If you need an installation from the current sources, use

`install.packages("remotes")`

`remotes::install_github("mgondan/rswipl")`

Please note that under Windows, you need the RTools build system in the version
of your R program. 
