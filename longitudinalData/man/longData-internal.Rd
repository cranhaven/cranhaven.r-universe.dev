\name{longData-internal}
\alias{catShort}
\alias{printShort}
\alias{printMatrixShort}
\alias{printOneTraj}
\alias{printTrajLong}
%\alias{cleanProg}
\alias{METHODS}

\alias{medianNA}
\alias{meanNA}
\alias{sdNA}
\alias{sdcNA}
\alias{varNA}
\alias{rangeNA}

\alias{which.minNA}

\alias{is.tna}

%\alias{NAtrunc}

\alias{showLongData}
\alias{showLongData3d}
\alias{showListPartition}

%\alias{trajImput.copy.end}
%\alias{trajImput.copy.middle}

%\alias{trajImput.copyMean}
%\alias{trajImput.globalSlope.begin}
%\alias{trajImput.globalSlope.end}
%\alias{trajImput.interpoLin}

%\alias{trajImput.interpoLin.middle}
%\alias{trajImput.interpoLin2}

%\alias{trajImput.interpoLin3}
%\alias{trajImput.localSlope.begin}

%\alias{trajImput.localSlope.end}
%\alias{trajImput.LOCB}
%\alias{trajImput.LOCB.begin}

%\alias{trajImput.LOCB.middle}
%\alias{trajImput.LOCF}
%\alias{trajImput.LOCF.end}

%\alias{trajImput.LOCF.middle}

\alias{canonicalizeAndMergeScene}
\alias{colorScene}
\alias{t2ve}
\alias{is.Triangles3D}
\alias{colorTriangles}



\title{~ longData: internal objects and methods ~}
\description{longData: internal objects and methods}
\details{These are not to be called by the user.
  \itemize{
    \item medianNA : median with option na.rm=TRUE
    \item meaanNA : mean with option na.rm=TRUE
    \item sdNA : sandard deviation with option na.rm=TRUE
    \item sdcNA : standard deviation corrected (n-1)/n with option na.rm=TRUE
    \item varNA : var with option na.rm=TRUE
    \item rangeNA : range with option na.rm=TRUE
    \item which.minNA : which.min that return NA if it argument has only
    NA values.
    \item NAtrunc : function that remove only the NA value in the tail
    of its argument.
    \item showLongData : methods use to define the function 'show'
    \item METHODS : constant=c("manhattan", "euclidean", "minkowski",
    "maximum", "canberra", "binary")
  }
}



\section{Author}{
Christophe Genolini\cr
1. UMR U1027, INSERM, Université Paul Sabatier / Toulouse III / France\cr
2. CeRSME, EA 2931, UFR STAPS, Université de Paris Ouest-Nanterre-La Défense / Nanterre / France
}

\references{
  [1] C. Genolini and B. Falissard\cr
  "KmL: k-means for longitudinal data"\cr
  Computational Statistics, vol 25(2), pp 317-328, 2010\cr

  [2] C. Genolini and B. Falissard\cr
  "KmL: A package to cluster longitudinal data"\cr
  Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011\cr
}

\keyword{internal}

