\name{ODB-news}
\title{ODB package NEWS}
\section{Changes in version 1.2.1 (2020-03-10)}{
  \itemize{
    \item{isClosed() now complies with both old and new versions of RJDBC::dbSendUpdate}
  }
}
\section{Changes in version 1.2.0 (2018-05-27)}{
  \itemize{
    \item{README file added}
    \item{Support web links updated}
    \item{Minor DESCRIPTION file updates to comply with new CRAN policies}
    \item{'ODB-package.Rd' updated to use the package meta-data macros}
    \item{Dependencies fixed in 'NAMESPACE'}
    \item{Package file structure modified to meet the standard}
    \item{'java' directory with URL to the java source code added}
    \item{Template updated to last LibreOffice version (5.1.6.2)}
  }
}
\section{Changes in version 1.1.1 (2012-07-23)}{
  \itemize{
    \item{Zip command check added before odb.close processing}
    \item{'UTF8' encoding name replaced by 'UTF-8' for Solaris compatibility}
  }
}
\section{Changes in version 1.1.0 (2012-07-21)}{
  \itemize{
    \item{NAMESPACE addition}
    \item{Main class ODB switched to uppercases}
    \item{odb.zip removed, utils::zip used instead}
    \item{XML package dependency removed for Mac OS compatibility, regular expressions used instead}
    \item{File paths stored normalized, to avoid odb.close problems on particular platforms}
    \item{Template updated to last LibreOffice version (3.5.5)}
    \item{Notice added to odb.read and odb.write help pages about name quoting [Manuel Eugster]}
    \item{odb.insert now accepts negative numerics [Christian Sachs]}
  }
}
