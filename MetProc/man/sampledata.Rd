\name{sampledata}
\docType{data}
\alias{sampledata}
\title{Simulated Metabolomics Data}
\description{
  This is a simulated dataset to show the format of the metabolomics data; patterns of missing data are generated roughly from a real metabolomics experiment. Rows represent metabolites and columns represent samples. The file contains 100 metabolites (rows) and 505 samples (480 biological sample columns and 25 pooled plasma columns) sorted by injection order. There are 20 biological samples between pooled plasma runs. Pooled plasma columns have prefix \sQuote{PPP} and biological samples are simple integers with no prefix. 
}
\format{The first row (Date) contains the date of processing. The second row (Inject) contains the injection number and is ordered from 1 to 505. The third row contains the column headers:\cr\cr
  \code{Metab} is the metabolite ID.\cr
  \code{Meth} is the type of metabolite.\cr
  \code{HMDB} is the HMDB ID of the metabolite, if it exists.\cr
  \code{m/z} is the mass-to-charge ratio of the metabolite.\cr
  \code{rt} is the retention time.\cr
  \code{Com} contains any comments.\cr
  \code{ProcID} is the processing ID of the metabolite.\cr\cr
  The remaining columns are either pooled plasma samples (prefix: \sQuote{PPP}) or biological samples (prefix: No prefix). The basic structure of the csv file is as follows:\cr
\tabular{llllllllllllll}{
     \tab  \tab  \tab  \tab  \tab  \tab Date \tab 415 \tab 415 \tab .. \tab 415 \tab 415 \tab 415 \tab ..\cr
     \tab  \tab  \tab  \tab  \tab  \tab Inject \tab 1 \tab 2 \tab .. \tab 21 \tab 22 \tab 23 \tab ..\cr
    Metab \tab Meth \tab HMDB \tab m/z \tab rt \tab Com \tab ProcID \tab PPP1 \tab 1 \tab .. \tab 20 \tab PPP2 \tab 21 \tab ..\cr
    M1 \tab Lipid \tab H1 \tab 304 \tab 8.7 \tab  \tab 1 \tab 6.7 \tab 6.7 \tab .. \tab 5.0 \tab 6.7 \tab 4.6 \tab ..\cr 
    M2 \tab Lipid \tab H2 \tab 309 \tab 7.6 \tab  \tab 2 \tab 1.0 \tab 1.1 \tab .. \tab 1.1 \tab 1.0 \tab 1.2 \tab ..\cr
    .. \tab .. \tab .. \tab .. \tab .. \tab .. \tab .. \tab .. \tab .. \tab .. \tab .. \tab .. \tab .. \tab .. \cr
    M100 \tab Lipid \tab H100 \tab 249 \tab 6.2 \tab  \tab 100 \tab 2.4 \tab 1.9 \tab .. \tab 2.2 \tab 2.4 \tab 1.6 \tab ..\cr
  }
}

\usage{sampledata}
\keyword{datasets}
\seealso{See \code{\link{read.met}} for example of reading this csv file for use.\cr See \code{\link{MetProc-package}} for examples of running the full process.
}