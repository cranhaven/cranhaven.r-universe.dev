\name{Distance Measures}
\alias{Distance Measures}
\alias{distBrayCurtis}
\alias{distChaoJaccCorr}
\alias{distChaoJaccNaive}
\alias{distChaoSorCorr}
\alias{distChaoSorNaive}
\alias{distChord}
\alias{distJaccard}
\alias{distMatching}
\alias{distMorisitaHorn}
\alias{distOchiai}
\alias{distPreston}
\alias{distRogersTanimoto}
\alias{distSimRatio}
\alias{distSorensen}
\alias{distWhittaker}

\title{
Plug-in distance-measure functions for \code{\link{distShell}}.
}
\description{
Each function takes two (interchangeable) vectors of data and returns a measure of distance between them. Vectors may be just 1/0 values (presence-absence data) or non-negative integers (count data).
}
\usage{
distBrayCurtis(d1, d2)
distChaoJaccCorr(d1, d2)
distChaoJaccNaive(d1, d2)
distChaoSorCorr(d1, d2)
distChaoSorNaive(d1, d2)
distChord(d1, d2)
distJaccard(d1, d2)
distMatching(d1, d2)
distMorisitaHorn(d1, d2)
distOchiai(d1, d2)
distPreston(d1, d2)
distRogersTanimoto(d1, d2)
distSimRatio(d1, d2)
distSorensen(d1, d2)
distWhittaker(d1, d2)

}
\arguments{
  \item{d1, d2}{
vectors of equal length, specifying the two cases to be compared.
}
}
\details{
\describe{
  \item{distBrayCurtis}{
Complement of the Bray-Curtis index, see Magurran p.246, where it is referred to as the 'quantitative Sorensen' index. Based on count data.
}
  \item{distChaoJaccCorr, distChaoJaccNaive, distChaoSorCorr, distChaoSorNaive}{
Each is the complement of one of a series of similarity indices which allow for (1) relative abundance of shared species and (2) estimation of number of shared species not detected. Based on count data. See Chao et al. 2005.
}
  \item{distChord}{
Both vectors are normalized so that the sum of squares = 1, ie. they lie on the surface of a sphere of unit radius. The distance measure is the length of the cord joining the two points through the sphere, which is in [0, sqrt(2)], ie. it is sqrt(2) for sites with no species in common. Based on count data. See Zuur et al 2007:166, Legendre & Legendre 1998:279.
}
  \item{distJaccard}{
Complement of the Jaccard index of similarity; also known as "Marczewski-Steinhaus distance". Based on presence-absence data. No. of shared species / Overall number of species.
}
  \item{distMatching}{
A simple matching index: the proportion of elements which match in two presence-absence vectors (ie. present in both or absent in both). Zuur et al 2007:165.
}
  \item{distMorisitaHorn}{
The complement of the Morisita-Horn index of similarity. Based on count data. See Magurran 2004:246 The Morisita-Horn index is also known as "simplified Morisita". The "Morisita" and "Horn" indices are different again! See Krebs 1999:470-471.
}
  \item{distOchiai}{
Complement of the Ochiai coefficient of similarity. Based on count data. See Zuur et al 2007:167, Legendre & Legendre 1998:276.
}
  \item{distPreston}{
Preston's coefficient of faunal dissimilarity (z). Based on presence-absence data. See Preston 1962:418.
}
  \item{distRogersTanimoto}{
Complement of Rogers and Tanimoto's coefficient of similarity. Based on presence-absence data. See Zuur et al 2007:165.
}
  \item{distSimRatio}{
Complement of the Similarity Ratio. Based on count data. See Zuur et al 2007:167.
}
  \item{distSorensen}{
Complement of Sorensen (or Dice) index of similarity. Based on presence-absence data. No. of shared species / Average number of species. Same as Whittaker's distance measure for Incidence (presence-absence) data minus one (Magurran 2004:244).
}
  \item{distWhittaker}{
Whittaker's index of association for Abundance (count) data. See Zuur et al 2007:170.
}
}
}
\value{
a scalar, the distance between the two vectors.
}
\references{
\itemize{
\item Chao, A; R L Chazdon; R K Colwell; T-J Shen. 2005. A new statistical
   approach for assessing similarity of species composition with incidence
   and abundance data. \emph{Ecology Letters} 8:148-159.
\item Krebs, C J 1999. Ecological Methodology. Addison Wesley Longman.
\item Magurran, A E 2004. \emph{Measuring biological diversity}. Blackwell. 
\item Preston, F W. 1962. The canonical distribution of commonness and rarity: Part II. \emph{Ecology} 43:410-432. 
\item Zuur, A F; E N Ieno; G M Smith 2007. \emph{Analysing ecological data}. Springer. 
\item Legendre, P; L Legendre 1998. \emph{Numerical ecology}. Elsevier, Amsterdam NL.
}
}
\author{
Mike Meredith
}
\seealso{
The basic distance computation function is \code{\link{dist}} in package stats. Other functions are \code{vegan::vegdist} and \code{labdsv::dsvdis}.

These functions provide the following distance measures:
\itemize{
  \item       binary (in dist) = asymmetric binary = Steinhaus
  \item       binomial (in vegdist)
  \item       bray/curtis (in dsvdis) = bray (in vegdist)
  \item       canberra (in dist and vegdist)
  \item       chao (in vegdist)
  \item       chisq  (in dsvdis)
  \item       euclidean (in dist and vegdist)
  \item       gower (in vegdist)
  \item       horn (in vegdist) = Morisita-Horn or simplified Morisita 
  \item       jaccard (in vegdist)
  \item       kulczynski (in vegdist)
  \item       manhattan (in dist and vegdist)
  \item       maximum (in dist)
  \item       minkowski (in dist)
  \item       morisita (not simplified!) (in vegdist)
  \item       mountford (in vegdist)
  \item       ochiai (in dsvdis)
  \item       raup (in vegdist) = Raup-Crick
  \item       roberts  (in dsvdis)
  \item       ruzicka (in dsvdis)
  \item       sorensen  (in dsvdis)
  \item       steinhaus (in dsvdis)= binary 
}
}
\examples{
data(distTestData)

distShell(distTestData, distJaccard)

distShell(distTestData, distMorisitaHorn)
}
