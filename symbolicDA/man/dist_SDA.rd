\name{dist_SDA}
\alias{dist_SDA}
\title{distance measurement for symbolic data}
\description{calculates distances between symbolic objects described by interval-valued, multinominal and multinominal with weights variables}
\usage{
dist_SDA(table.Symbolic,type="U_2",subType=NULL,gamma=0.5,power=2,probType="J",
probAggregation="P_1",s=0.5,p=2,variableSelection=NULL,weights=NULL)
}
\arguments{
\item{table.Symbolic}{symbolic data table}
\item{type}{distance measure for boolean symbolic objects: H, U_2, U_3, U_4, C_1, SO_1, SO_2, SO_3, SO_4, SO_5; mixed symbolic objects: L_1, L_2}
\item{subType}{comparison function for C_1 and SO_1: D_1, D_2, D_3, D_4, D_5}
\item{gamma}{gamma parameter for U_2 and U_3, gamma [0, 0.5]}
\item{power}{power parameter for U_2 and U_3; power [1, 2, 3, ..]}
\item{probType}{distance measure for probabilistic symbolic objects: J, CHI, REN, CHER, LP}
\item{probAggregation}{agregation function for J, CHI, REN, CHER, LP: P_1, P_2}
\item{s}{parameter for Renyi (REN) and Chernoff (CHE) distance, s [0, 1)}
\item{p}{parameter for Minkowski (LP) metric; p=1 - manhattan distance, p=2 - euclidean distance}
\item{variableSelection}{numbers of variables used for calculation or NULL for all variables}
\item{weights}{weights of variables for Minkowski (LP) metrics}
}
\value{
distance matrix of symbolic objects
}
\author{
Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl}, Justyna Wilk \email{justyna.wilk@ue.wroc.pl}
Department of Econometrics and Computer Science, Wroclaw University of Economics, Poland \url{http://keii.ue.wroc.pl/symbolicDA/}
}
\references{
Billard L., Diday E. (eds.) (2006), \emph{Symbolic Data Analysis, Conceptual Statistics and Data Mining}, John Wiley & Sons, Chichester.

Bock H.H., Diday E. (eds.) (2000), \emph{Analysis of Symbolic Data. Explanatory methods for extracting statistical information from complex data}, Springer-Verlag, Berlin.

Diday E., Noirhomme-Fraiture M. (eds.) (2008), \emph{Symbolic Data Analysis with SODAS Software}, John Wiley & Sons, Chichester.

Ichino, M., & Yaguchi, H. (1994),\emph{Generalized Minkowski metrics for mixed feature-type data analysis}. IEEE Transactions on Systems, Man, and Cybernetics, 24(4), 698-708. Available at: \doi{10.1109/21.286391}.

Malerba D., Espozito F, Giovalle V., Tamma V. (2001), \emph{Comparing Dissimilarity Measures for Symbolic Data Analysis}, "New Techniques and Technologies for Statistcs" (ETK NTTS'01), pp. 473-481.

Malerba, D., Esposito, F., Monopoli, M. (2002), \emph{Comparing dissimilarity measures for probabilistic symbolic objects}, In: A. Zanasi, C.A. Brebbia, N.F.F. Ebecken, P. Melli (Eds.), Data Mining III, "Series Management Information Systems", Vol. 6, WIT Press, Southampton, pp. 31-40.
}
\details{
Distance measures for boolean symbolic objects:

H - Hausdorff's distance for objects described by interval-valued variables,
U_2, U_3, U_4 - Ichino-Yaguchi's distance measures for objects described by interval-valued and/or multinominal variables,
C_1, SO_1, SO_2, SO_3, SO_4, SO_5 - de Carvalho's distance measures for objects described by interval-valued and/or multinominal variables.

Distance measurement for probabilistic symbolic objects consists of two steps: 
1. Calculation of distance between objects for each variable using componentwise distance measures: J (Kullback-Leibler divergence), CHI (Chi-2 divergence), REN (Renyi's divergence), CHER (Chernoff's distance), LP (modified Minkowski metrics).
2. Calculation of aggregative distance between objects based on componentwise distance measures using objectwise distance measure: P_1 (manhattan distance), P_2 (euclidean distance). 

Distance measures for mixed symbolic objects - modified Minkowski metrics: L_1 (manhattan distance), L_2 (euclidean distance).

See file \url{../doc/dist_SDA.pdf} for further details

NOTE !!!: In previous version of package this functian has been called dist.SDA.
}
\seealso{
\code{\link{DClust}}, \code{\link{index.G1d}}; \code{dist.Symbolic} in \code{clusterSim} library
}
\examples{
# LONG RUNNING - UNCOMMENT TO RUN
#data("cars",package="symbolicDA")
#dist<-dist_SDA(cars, type="U_3", gamma=0.3, power=2)
#print(dist)
}
\keyword{symbolic}
\concept{SDA}
