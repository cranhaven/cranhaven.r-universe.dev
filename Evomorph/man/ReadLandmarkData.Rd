\encoding{utf-8}
\name{ReadLandmarkData}
\alias{ReadLandmarkData}
\title{Extract landmark data}
\description{Read landmark coordinates and scales from a TPS file}
\usage{
ReadLandmarkData(datafile, m_byscale = T)
}
\arguments{
\item{datafile}{TPS file containing two dimensional landmark data}
\item{m_byscale}{Logical value (Default m_byscale=TRUE). If TRUE, landmark coordinates are multiply by scale}
}
\details{ 
The function returns landmark coordinates (array and data.frame format) and the scale of each specimen. If \code{m_byscale} = T the coordinates are multiple by the corresponding scale.
\code{ReadLandmarkData} only works with TPS files that has only landmark coordinates (presence of CURVES and OUTLINES may cause malfunction).}
\value{
It returns a 3 element list:
\item{df}{Landmark coordinates as a data.frame. Sp factor correspond to species number.}
\item{array}{Landmark coordinates as a three dimensional array.}
\item{scale}{Vector of scales value for each specimen.}
}
\author{Cabrera Juan Manuel}