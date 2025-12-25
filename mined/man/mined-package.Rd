\name{mined-package}
\alias{mined-package}
\docType{package}
\title{
mined package
%\packageTitle{FMEDesign}
}
\description{
Generate minimum energy design (MinED) samples from an unnormalized probability density function. The asymptotic distribution of MinED samples converges to the target distribution and therefore, MinED can be viewed as a deterministic sample from the target distribution. The details of MinED and the algorithm used for generating it can be found in Joseph, Dasgupta, Tuo, and Wu (2015) and Joseph, Wang, Gu, Lv, and Tuo (2019). This research is supported by a U.S. National Science Foundation grant DMS-1712642.
% \packageDescription{FMEDesign}
}

\details{
\tabular{ll}{
Package: \tab mined\cr
Type: \tab Package\cr
Version: \tab 1.0-3\cr
Date: \tab 2022-06-19\cr
License: \tab LGPL-2.1\cr
}
Important functions in this package are: \code{\link{mined}} generates Minimum Energy Design samples from an unnormalized density function, \code{\link{SelectMinED}} selects Minimum Energy Design samples from candidate points, and \code{\link{Lattice}} generates good rank-1 lattice rules. 
}
\author{
Dianpeng Wang and V. Roshan Joseph

Maintainer: Dianpeng Wang <wdp@bit.edu.cn>
}
\references{
Joseph, V. R., Dasgupta, T., Tuo, R., and Wu, C. F. J. (2015). "Sequential Exploration of Complex Surfaces Using Minimum Energy Designs". \emph{Technometrics}, 57, 64-74.

Joseph, V. R., Wang, D., Gu, L., Lv, S., and Tuo, R. (2019). "Deterministic Sampling of Expensive Posteriors Using Minimum Energy Designs", \emph{Technometrics}, 61, 297-308, arXiv:1712.08929, DOI:10.1080/00401706.2018.1552203.
}
% ~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
%\keyword{ package }
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
%\examples{
%~~ simple examples of the most important functions ~~
%}

\keyword{ package }