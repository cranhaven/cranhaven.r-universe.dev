\name{circuit}
\alias{circuit}
\docType{data}
\title{Integrated Circuit Data}
\description{
An experiment on integrated circuits was reported by Phadke et al.
(1983). The width of lines made by a photoresist-nanoline tool were
measured in five different locations on silicon wafers, measurements being
taken before and after an etching process being treated separately.
Here, the pre-etching data are analyzed. The eight experimental factors
(A-H) were arranged in an L18 orthogonal array and produced 33
measurements at each of five locations, giving a total of 165 observations.
There were no whole-plot (i.e. between-wafer) factors.
}
\usage{data("circuit")}
\format{
  A data frame with 165 observations on the following 13 variables.
  \describe{
    \item{\code{Width}}{width of lines made by a photoresist-nanoline tool}
    \item{\code{A}}{a factor A with levels \code{1}, \code{2} and \code{3}}
    \item{\code{B}}{a factor B with levels \code{1}, \code{2} and \code{3}}
    \item{\code{C}}{a factor C with levels \code{1}, \code{2} and \code{3}}
    \item{\code{D}}{a factor D with levels \code{1}, \code{2} and \code{3}}
    \item{\code{E}}{a factor E with levels \code{1}, \code{2} and \code{3}}
    \item{\code{F}}{a factor F with levels \code{1}, \code{2} and \code{3}}
    \item{\code{G}}{a factor G with levels \code{1}, \code{2} and \code{3}}
    \item{\code{H}}{a factor H with levels \code{1}, \code{2} and \code{3}}
    \item{\code{I}}{a factor I with levels \code{1}, \code{2} and \code{3}}
    \item{\code{Wafer}}{wafers indenfifier}
    \item{\code{waf}}{replication number for two replicates}
    \item{\code{exp}}{experiment number for differenct array}
  }
}

\references{
Phadke, M.S., Kacka, R.N., Speeney, D.V. and Grieco, M.J. (1983).
Off-line quality control for integrated circuit fabrication using experimental
design. Bell System Technical Journal, 62, 1273--1309.
}

