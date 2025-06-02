\name{pems.utils-package}
\alias{pems.utils-package}
\alias{pems.utils}
\docType{package}
\title{
  pems.utils 
}
\description{
  The R package pems.utils contains a range of functions for the routine handling 
  and analysis of data collected by portable emissions measurement systems (PEMS) 
  and other similar mobile monitoring systems.  
  
}
\details{
\tabular{ll}{
Package: \tab pems.utils\cr
Type: \tab Package\cr
Version: \tab 0.3.0.7\cr 
Date: \tab 2024-12-28\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}

The \code{pems.utils} functions have been arranged according to usage, as 
follows:

1. Getting data in and out of \code{pems.utils}.

1.1. Functions for making and importing datasets for use with \code{pems.utils}: \code{\link{pems}}, 
\code{\link{import2PEMS}}, etc.

1.2. Exporting data from pems objects and R: \code{\link{export.data}}.

2. Data Structure and General Handling

2.1. The pems object structure: \code{\link{pems.structure}}, \code{\link{getPEMSElement}}, 
\code{\link{pemsData}}, etc. 

3. Generic pems handling

3.1. pems objects, \code{\link{pems.generics}}.

3.2. pems.element objects, \code{\link{pems.element.generics}}.

4. Structure Handling 

4.1. Merging pems objects: \code{\link{merge.pems}}, \code{\link{align}}, etc. 

4.2. Referencing pems objects: \code{\link{referencing.pems.data}}, \code{\link{refRow}}, etc.

4.3. Time handling functions: \code{\link{regularize}}, etc.  

4.4. Unit handler functions: \code{\link{getUnits}}, \code{\link{setUnits}}, \code{\link{convertUnits}}, etc.

5. pems Data Handling 

5.1. Plots for pems objects: \code{\link{pems.plots}}, \code{\link{latticePlot}}, \code{\link{pemsPlot}}, etc.

6. Calculations

6.1. Common calculations: \code{\link{common.calculations}}, \code{\link{calcDistance}}, \code{\link{calcAccel}}, etc.

6.2. Common \code{\link{check...}} functions for the routine handling of function arguments/user inputs.
 
6.3. Other correction code

6.4. Analysing data in pems objects: \code{\link{summary.reports}}

7. Refernce datasets, examples, look-up tables, etc.

7.1. Example datasets: \code{\link{pems.1}}.

7.2. look-up tables: \code{\link{ref.unit.conversions}}, etc.

8. Specialist code

8.1. VSP calculations: \code{\link{calcVSP}}, etc.

8.2. Emissions calculations: \code{\link{calcEm}}, etc.

9. Other Code 

9.1. Tidyverse related code... \code{\link{pems.tidyverse}}

}
\author{
Karl Ropkins
Maintainer: Karl Ropkins <k.ropkins@its.leeds.ac.uk>
}
\references{
  Functions in \code{pems.utils} make extensive use of code developed by others. 
  In particular, I gratefully acknowledge the huge contributions of the R Core Team 
  and numerous contributors in developing and maintaining R:

  R Development Core Team (2011). R: A language and environment for
  statistical computing. R Foundation for Statistical Computing,
  Vienna, Austria. ISBN 3-900051-07-0, URL http://www.R-project.org/.

}

\keyword{ package }

\seealso{
  \code{\link{pems}}, \code{\link{import2PEMS}} 
}

%\examples{
%############
%#to do
%}

