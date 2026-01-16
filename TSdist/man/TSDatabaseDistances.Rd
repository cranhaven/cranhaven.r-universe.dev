\name{TSDatabaseDistances}
\alias{TSDatabaseDistances}

\title{
TSdist distance matrix computation.
}
\description{
TSdist distance matrix computation for time series databases.
}
\usage{
TSDatabaseDistances(X, Y=NULL, distance, ...)
}
\arguments{
  \item{X}{
  Time series database saved in a numeric matrix, a list, an \code{mts} object, a \code{zoo} object  or \code{xts} object.
}

  \item{Y}{
  Time series database saved in a numeric matrix, a list, an \code{mts} object, a \code{zoo} object  or \code{xts} object. Should only be defined for calculation of distance matrices between two different databases so default value is NULL.
}
  
  \item{distance}{
 Distance measure to be used. It must be one of: \code{"euclidean"}, \code{"manhattan"}, \code{"minkowski"}, \code{"infnorm"}, \code{"ccor"}, \code{"sts"}, \code{"dtw"}, \code{"keogh.lb"}, \code{"edr"}, \code{"erp"}, \code{"lcss"}, \code{"fourier"}, \code{"tquest"}, \code{"dissim"}, \code{"acf"}, \code{"pacf"}, \code{"ar.lpc.ceps"}, \code{"ar.mah"}, \code{"ar.mah.statistic"}, \code{"ar.mah.pvalue"}, \code{"ar.pic"}, \code{"cdm"}, \code{"cid"}, \code{"cor"}, \code{"cort"}, \code{"wav"}, \code{"int.per"}, \code{"per"}, \code{"mindist.sax"}, \code{"ncd"}, \code{"pred"}, \code{"spec.glk"}, \code{"spec.isd"},
                        \code{"spec.llr"}, \code{"pdc"}, \code{"frechet"}, \code{"tam"})
}f
\item{...}{
  Additional parameters required by the chosen distance measure.
}
}
\details{

The distance matrix of a time series database is calculated by providing the pair-wise distances between the series that conform it. \code{x} can be saved in a numeric matrix, a list or a \code{mts}, \code{zoo} or \code{xts} object. The following distance methods are supported: 

\itemize{
\item "euclidean": Euclidean distance. \code{\link{EuclideanDistance}}
\item "manhattan": Manhattan distance. \code{\link{ManhattanDistance}}
\item "minkowski": Minkowski distance. \code{\link{MinkowskiDistance}}
\item "infnorm": Infinite norm distance. \code{\link{InfNormDistance}}
\item "ccor": Distance based on the cross-correlation. \code{\link{CCorDistance}}
\item "sts": Short time series distance. \code{\link{STSDistance}}
\item "dtw": Dynamic Time Warping distance. \code{\link{DTWDistance}}. Uses the \pkg{dtw} package (see \code{\link[dtw]{dtw}}).
\item "lb.keogh": LB_Keogh lower bound for the Dynamic Time Warping distance. \code{\link{LBKeoghDistance}}
\item "edr": Edit distance for real sequences. \code{\link{EDRDistance}}
\item "erp": Edit distance with real penalty. \code{\link{ERPDistance}}
\item "lcss": Longest Common Subsequence Matching. \code{\link{LCSSDistance}}
\item "fourier": Distance based on the Fourier Discrete Transform. \code{\link{FourierDistance}}
\item "tquest": TQuest distance. \code{\link{TquestDistance}}
\item "dissim": Dissim distance. \code{\link{DissimDistance}}
\item "acf": Autocorrelation-based dissimilarity \code{\link{ACFDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.ACF}}).
\item "pacf": Partial autocorrelation-based dissimilarity \code{\link{PACFDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.PACF}}).
\item "ar.lpc.ceps": Dissimilarity based on LPC cepstral coefficients \code{\link{ARLPCCepsDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.AR.LPC.CEPS}}).
\item "ar.mah": Model-based dissimilarity proposed by Maharaj (1996, 2000) \code{\link{ARMahDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.AR.MAH}}).
\item "ar.pic": Model-based dissimilarity measure proposed by Piccolo (1990) \code{\link{ARPicDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.AR.PIC}}).
\item "cdm": Compression-based dissimilarity measure \code{\link{CDMDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.CDM}}).
\item "cid": Complexity-invariant distance measure \code{\link{CIDDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.CID}}).
\item "cor": Dissimilarities based on Pearson's correlation \code{\link{CorDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.COR}}).
\item "cort": Dissimilarity index which combines temporal correlation and raw value
behaviors \code{\link{CortDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.CORT}}).
\item "int.per": Integrated periodogram based dissimilarity \code{\link{IntPerDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.INT.PER}}).
\item "per": Periodogram based dissimilarity \code{\link{PerDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.PER}}).
\item "mindist.sax": Symbolic Aggregate Aproximation based dissimilarity  \code{\link{MindistSaxDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.MINDIST.SAX}}).
\item "ncd": Normalized compression based distance \code{\link{NCDDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.NCD}}).
\item "pred": Dissimilarity measure cased on nonparametric forecasts \code{\link{PredDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.PRED}}).
\item "spec.glk": Dissimilarity based on the generalized likelihood ratio test \code{\link{SpecGLKDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.SPEC.GLK}}).
\item "spec.isd": Dissimilarity based on the integrated squared difference between the log-spectra \code{\link{SpecISDDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.SPEC.ISD}}).
\item "spec.llr": General spectral dissimilarity measure using local-linear estimation of the log-spectra \code{\link{SpecLLRDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.SPEC.LLR}}).
\item "pdc": Permutation Distribution Distance \code{\link{PDCDistance}}. Uses the \pkg{pdc} package (see \code{\link[pdc]{pdcDist}}).
\item "frechet": Frechet distance \code{\link{FrechetDistance}}. Uses the \pkg{longitudinalData} package (see \code{\link[longitudinalData]{distFrechet}}).
\item "tam": Time Aligment Measurement \code{\link{TAMDistance}}. 
}


Some distance measures may require additional arguments. See the individual help pages (detailed above) for more information about each method. These parameters should be named in order to avoid mismatches.

Finally, for options \code{dissim}, \code{dissimapprox} and \code{sts}, databases conformed of series with different sampling rates can be introduced as a \code{list} of \code{zoo}, \code{xts} or \code{ts} objects, where each element in the list is a time series with its own time index.


}

\value{
\item{D}{
  The computed distance matrix of the time series database. In some cases, such as  ar.mahDistance or predDistance, some additional information is also provided. 
}

}


\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.
}

\examples{

# The object example.database is a numeric matrix that saves
# 6 ARIMA time series in a row-wise format. For more information
# see help page of example.databases:

help(example.database)
data(example.database)

# To calculate the distance matrix of this database: 

TSDatabaseDistances(example.database, distance="manhattan")
TSDatabaseDistances(example.database, distance="edr", epsilon=0.2)
TSDatabaseDistances(example.database, distance="fourier", n=20)

# The object zoo.database is a zoo object that saves
# the same 6 ARIMA time series saved in example.database. 

data(zoo.database)

# To calculate the distance matrix of this database: 

TSDatabaseDistances(zoo.database, distance="manhattan")
TSDatabaseDistances(zoo.database, distance="edr", epsilon=0.2)
TSDatabaseDistances(zoo.database, distance="fourier", n=20)

}
