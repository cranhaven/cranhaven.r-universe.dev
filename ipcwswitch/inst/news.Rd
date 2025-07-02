\name{NEWS}
\title{NEWS file for the ipcwswitch package}

\section{Changes in version 1.0.2}{
  \itemize{
    \item 03 Feb 2019  The data.ipcw.rda dataset was removed and reimplaced by the SHIdat.rda dataset
    \item 03 Feb 2019  The data.ipcw.Rd was removed (in the man folder).
    \item 03 Feb 2019  The SHIdat.Rd was created (in the man folder)
    \item 03 Feb 2019  Change the way to return the output in replicRows.R (l.89)
    \item 03 Feb 2019  Change l.85 in cens.ipw.Rd (link to new SHIdat example)
    \item 03 Feb 2019  Change l.40 and l.46 in ipcw.Rd (link to new SHIdat example)
    \item 03 Feb 2019  Change l.77 in replicRows.Rd (link to new SHIdat example)
    \item 03 Feb 2019  Change l.46 in timesTokeep.Rd (link to new SHIdat example)
    \item 03 Feb 2019  Change l.60 in wideToLongTDC.Rd (link to new SHIdat example)
    \item 03 Feb 2019  Change l.62 in cens.ipw.R (link to new SHIdat example)
    \item 03 Feb 2019  Change l.29 and l.30 in ipcw.R (link to new SHIdat example)
    \item 03 Feb 2019  Change l.60 in replicRows.R (link to new SHIdat example)
    \item 03 Feb 2019  Change l.28 in timesTokeep.R (link to new SHIdat example)
    \item 03 Feb 2019  Change l.38 in wideToLongTDC.R (link to new SHIdat example)
    }
}

\section{Changes in version 1.0.3}{
  \itemize{
    \item 12 May 2019  New replicRows.R function with additional parameters (\code{times1} and \code{times2} parameters to allow collecting times in each arm separately, and \code{arm} parameter to be able to do each computation)
    \item 12 May 2019  New ipcw.R function. The new \code{for} loop included in this version gathers patients by their identifiers, which are indicated by the additional parameter \code{id}. Also, the new \code{type} parameter allows to specify the type of survival curve.
    }
}


\section{Changes in version 1.0.4}{
  \itemize{
    \item 16 February 2021 Correction made l.66 in replicRows.R: change from \code{rep\\times2} into \code{rep.times2}
    \item 16 February 2021 Change in wideToLong.R (l.82 and l.83) to no longer call \code{order()} on data frame 
    \item 16 February 2021 Update of the main reference in all functions and related documentation
  }
}

