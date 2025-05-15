\name{findOptimalSmacofSym}
\alias{findOptimalSmacofSym}
\title{Selecting the optimal multidimensional scaling (MDS) procedure}
\description{Selecting the optimal multidimensional scaling procedure - metric MDS (by varying all combinations of normalization methods, distance measures, and metric MDS models)
and nonmetric MDS (by varying all combinations of normalization methods and distance measures)}
\usage{
findOptimalSmacofSym(table,
critical_stress=(max(as.numeric(gsub(",",".",table[,"STRESS 1"],fixed=TRUE)))+
min(as.numeric(gsub(",",".",table[,"STRESS 1"],fixed=TRUE))))/2,
critical_HHI=NA)
}
\arguments{
\item{table}{
result from \code{\link{optSmacofSym_nMDS}} or \code{\link{optSmacofSym_mMDS}}. Data frame ordered by increasing value of Stress-1 fit measure or HHI index with columns:

\code{Normalization method}

\code{Distance measure}

\code{MDS model}

\code{Spline degree}

\code{STRESS 1}

\code{HHI spp}

}
\item{critical_stress}{threshold value of Kruskal's Stress-1 fit measure. Default - mid-range of Kruskal's Stress-1 fit measures calculated for all MDS procedures}
\item{critical_HHI}{threshold value of Hirschman-Herfindahl HHI index. Only one parameter critical_stress or critical_HHI can be set, and the function finds the optimal value among the procedures for which the selected measure is lower or equal treshold value }
}
\value{
\item{Nr}{number of row in \code{table} with optimal multidimensional scaling procedure}
\item{Normalization_method}{normalization method used for optimal multidimensional scaling procedure}
\item{MDS_model}{MDS model used for optimal multidimensional scaling procedure}
\item{Spline_degree}{Additional spline.degree value for optimal procedure, if mspline model is used for simulation. For other models there is no value for this field}
\item{Distance_measure}{distance measure used for optimal multidimensional scaling procedure}
\item{STRESS_1}{value of Kruskal Stress-1 fit measure for optimal multidimensional scaling procedure}
\item{HHI_spp}{Hirschman-Herfindahl HHI index, calculated based on stress per point, for optimal multidimensional scaling procedure}
}
\author{
Marek Walesiak \email{marek.walesiak@ue.wroc.pl}, Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl} 

Department of Econometrics and Computer Science, Wroclaw University of Economics and Business, Poland
}
\references{
Borg, I., Groenen, P.J.F. (2005), Modern Multidimensional Scaling. Theory and Applications, 2nd Edition, Springer Science+Business Media, New York. ISBN: 978-0387-25150-9. Available at: \url{https://link.springer.com/book/10.1007/0-387-28981-X}.

Borg, I., Groenen, P.J.F., Mair, P. (2013), Applied Multidimensional Scaling, Springer, Heidelberg, New York, Dordrecht, London. Available at: \doi{10.1007/978-3-642-31848-1}.

De Leeuw, J., Mair, P. (2015), Shepard Diagram, Wiley StatsRef: Statistics Reference Online, John Wiley & Sons Ltd.

Dudek, A., Walesiak, M. (2020), The Choice of Variable Normalization Method in Cluster Analysis, pp. 325-340, [In:] K. S. Soliman (Ed.), Education Excellence and Innovation Management: A 2025 Vision to Sustain Economic Development during Global Challenges, Proceedings of the 35th International Business Information Management Association Conference (IBIMA), 1-2 April 2020, Seville, Spain. ISBN: 978-0-9998551-4-1.

Herfindahl, O.C. (1950), Concentration in the Steel Industry, Doctoral thesis, Columbia University.

Hirschman, A.O. (1964). The Paternity of an Index, The American Economic Review, Vol. 54, 761-762.

Walesiak, M. (2014), Przegląd formuł normalizacji wartości zmiennych oraz ich własności w statystycznej analizie wielowymiarowej [Data Normalization in Multivariate Data Analysis. An Overview and Properties], Przegląd Statystyczny, tom 61, z. 4, 363-372. Available at: \doi{10.5604/01.3001.0016.1740}.

Walesiak, M. (2016a), Wybór grup metod normalizacji wartości zmiennych w skalowaniu wielowymiarowym [The Choice of Groups of Variable Normalization Methods in Multidimensional Scaling], Przegląd Statystyczny, tom 63, z. 1, 7-18. Available at: \doi{10.5604/01.3001.0014.1145}.

Walesiak, M. (2016b), Visualization of Linear Ordering Results for Metric Data with the Application of Multidimensional Scaling, Ekonometria, 2(52), 9-21. Available at: \doi{10.15611/ekt.2016.2.01}.

Walesiak, M., Dudek, A. (2017), \emph{Selecting the Optimal Multidimensional Scaling Procedure for Metric Data with R Environment}, STATISTICS IN TRANSITION new series, September, Vol. 18, No. 3, pp. 521-540.

Walesiak, M., Dudek, A. (2020), Searching for an Optimal MDS Procedure for Metric and Interval-Valued Data using mdsOpt R package, pp. 307-324, [In:] K. S. Soliman (Ed.), Education Excellence and Innovation Management: A 2025 Vision to Sustain Economic Development during Global Challenges, Proceedings of the 35th International Business Information Management Association Conference (IBIMA), 1-2 April 2020, Seville, Spain. ISBN: 978-0-9998551-4-1.
}
\seealso{\code{\link{data.Normalization}}, \code{\link{dist.GDM}}, \code{\link{dist}}, \code{\link{smacofSym}}
}
\examples{
  \donttest{
  library(mdsOpt)
  metnor<-c("n1","n2","n3","n5","n5a","n8","n9","n9a","n11","n12a")
  metscale<-c("ratio","interval")
  metdist<-c("euclidean","manhattan","maximum","seuclidean","GDM1")
  data(data_lower_silesian)
  res<-optSmacofSym_mMDS(data_lower_silesian,normalizations=metnor,
  distances=metdist,mdsmodels=metscale,outDec=".")
  print(findOptimalSmacofSym(res))
  }
}
\keyword{multidimensional scaling}
\keyword{nonmetric MDS}
\keyword{variable normalization methods}
\keyword{distance measures}
\keyword{metric data}
\keyword{optimize}
