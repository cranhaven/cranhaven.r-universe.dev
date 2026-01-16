\name{qualityCriterion}
\Rdversion{1.1}
\alias{qualityCriterion}
%\alias{qualityCriterion,matrix,integer-method}
\alias{qualityCriterion,matrix,ANY-method}
\alias{qualityCriterion,array,ANY-method}
\alias{qualityCriterion,LongData,Partition-method}
\alias{qualityCriterion,LongData3d,Partition-method}


\title{
  ~ Function: qualityCriterion ~
}
\description{
  Given a \code{\linkS4class{LongData}} and a
  \code{\linkS4class{Partition}}, the fonction \code{qualityCriterion} calculate
  some qualities criterion.
}
\usage{
qualityCriterion(traj,clusters,imputationMethod="copyMean")
}
\arguments{
  \item{traj}{\code{[LongData]} or \code{[matrix]}: object containing
    the trajectories on which the criterion is calculate.}

  \item{clusters}{\code{[Paritition]} or \code{[vector(integer)]}:
    clusters to which individual belongs.}

  \item{imputationMethod}{\code{[character]}: if some value are missing in the
    \code{LongData}, it is necessary to impute them. Then the function
    \code{qualityCriterion} call the function \code{\link{imputation}} using
    the method \code{method}.}
}

\details{
  Given a \code{\linkS4class{LongData}} and a
  \code{\linkS4class{Partition}} (or a \code{matrix} and a vector of
  \code{integer}), the fonction \code{qualityCriterion} calculate several
  quality criterion and return then as a list (see 'value' below).

  If some individual have no clusters (ie if \code{Partition} has some
  missing values), the corresponding trajectories are exclude from the
  calculation.

  Note that if there is an empty cluster or an empty trajectory, most of
  the criterions are anavailable.

  Basicaly, 6 non-parametrics criterions are computed.
  In addition, ASSUMING THAT in each clusters C and for each time T,
  the variable follow a NORMAL LAW (mean and standard deviation of the variable at time T restricted
  to clusters C), it is possible to compute the the posterior
  probabilities of the individual trajectories and the
  likelihood. From there, we can also compute the BIC, the AIC and
  the global posterior probability. The function \code{qualityCriterion}
  also compute these criterion. But the user should alway keep in mind
  that these criterion are
  valid ONLY under the hypothesis of normality. If this
  hypothèsis is not respected, algorithm like k-means will converge but the BIC and AIC
  will have no meaning.

  IMPORTANT NOTE: Some criterion should be maximized, some other should be
  minimized. This might be confusing for the non expert. In order to
  simplify the comparison of the criterion, \code{qualityCriterion}
  compute the OPPOSITE of the criterion that should be minimized (Ray & Bouldin, Davies & Turi, BIC and AIC). Thus,
  all the criterion computed by this function should be maximized.
}

\section{Non-parametric criterion}{
  Notations: k=number of clusters; n=number of individual;
  B=Between variance ; W=Within variance
  The criterion are:
  \describe{
    \item{Calinski.Harabatz}{\code{[numeric]}: Calinski and Harabatz
      criterion: \code{c(k)=Trace(B)/Trace(W)*(n-k)/(k-1)}.}
    \item{Calinski.Harabatz2}{\code{[numeric]}: Calinski and Harabatz
      criterion modified by Krysczuk: \code{c(k)=Trace(B)/Trace(W)*(n-1)/(n-k)}.}
    \item{Calinski.Harabatz3}{\code{[numeric]}: Calinski and Harabatz
      criterion modified by Genolini:
      \code{g(k)=Trace(B)/Trace(W)*(n-k)/sqrt(k-1)}.}
    \item{Ray.Turi}{\code{[numeric]}: Ray and Turi criterion: \code{r(k)=-Vintra/Vinter} with
      \code{Vintra=Sum(dist(x,center(x)))} and
      \code{Vinter=min(dist(center_i,center_j)^2)}. (The "true" index of
      Ray and Turi is \code{Vintra/Vinter} and should me minimized. See IMPORTANT NOTE  above.)}
    \item{Davies.Bouldin}{\code{[numeric]}: Davies and Bouldin criterion:
      \code{d(k)=-mean(Proximite(cluster_i,cluster_j))} with
      \code{Proximite(i,j)=(DistInterne(i)+DistInterne(j))/(DistExterne(i,j))}. (The "true" index of
      Davies and Bouldin is \code{mean(Proximite())} and should me
      minimized. See IMPORTANT NOTE above.)}
    \item{random}{\code{[numeric]}: random value following the normal law N(0,1).}
  }
}

\section{Parametric criterion}{
  All the parametric indices should be minimized. So the function
  \code{qualityCriterion} compute their opposite (see IMPORTANT NOTE above.)

  Notation: L=likelihood; h=number of parameters; n=number of
  trajectories; t=number of time measurement; N=total number of
  measurement (N=t.n).


  SECOND IMPORTANT NOTE: the formula of parametrics criterion ofen
  include the size of the population. In the specific case on
  longitudinal data, the definition of the "size of the population" is
  not obvious. It can be either the number of individual \code{n}, or the number of
  measurement \code{N=n.t}. So, the function \code{qualityCriterion} gives
  two version of all the non parametrics criterion, the first using \code{n},
  the second using \code{N}.

  \describe{
    \item{BIC}{\code{[numeric]}: Bayesian Information Criterion: BIC=2*log(L)-h*log(n). See IMPORTANT NOTE above.}
    \item{BIC2}{\code{[numeric]}: Bayesian Information Criterion: BIC=2*log(L)-h*log(N). See IMPORTANT NOTE above.}
    \item{AIC}{\code{[numeric]}: Akaike Information Criterion, bis: AIC=2*log(L)-2*h. See IMPORTANT NOTE above.}
    \item{AICc}{\code{[numeric]}: Akaike Information Criterion with correction: AIC=AIC+(2h(h+1))/(n-h-1). See IMPORTANT NOTE above.}
    \item{AICc2}{\code{[numeric]}: Akaike Information Criterion with correction, bis: AIC=AIC+(2h(h+1))/(n-h-1). See IMPORTANT NOTE above.}
%    \item{entropy}{\code{[numeric]}: Entropy : -Sum_i Pi.log(Pi)}
%    \item{ICL}{\code{[numeric]}: BIC penalised by the entropy: ICL=BIC-2.entropy. See IMPORTANT NOTE above.}
%    \item{ICL2}{\code{[numeric]}: BIC2 penalised by the entropy: ICL2=BIC-2.entropy. See IMPORTANT NOTE above.}
  }
}


\value{A list with three fields: the first is the list of the criterions.
  the second is the clusters post probabilities;
  the third is the matrix of the individual post probabilities.
}

\seealso{
  \code{\linkS4class{LongData}}, \code{\linkS4class{Partition}},
  \code{\link{imputation}}.
}


\section{Author}{
  Christophe Genolini\cr
  1. UMR U1027, INSERM, Université Paul Sabatier / Toulouse III / France\cr
  2. CeRSM, EA 2931, UFR STAPS, Université de Paris Ouest-Nanterre-La Défense / Nanterre / France
}

\references{
  [1] C. Genolini and B. Falissard\cr
  "KmL: k-means for longitudinal data"\cr
  Computational Statistics, vol 25(2), pp 317-328, 2010\cr

  [2] C. Genolini and B. Falissard\cr
  "KmL: A package to cluster longitudinal data"\cr
  Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011\cr
}

\examples{
##################
### Preparation of some artificial data
par(ask=TRUE)
data(artificialLongData)
ld <- longData(artificialLongData)


### Correct partition
part1 <- partition(rep(1:4,each=50))
plotTrajMeans(ld,part1)
(cr1 <- qualityCriterion(ld,part1))

### Random partition
part2 <- partition(floor(runif(200,1,5)))
plotTrajMeans(ld,part2)
(cr2 <- qualityCriterion(ld,part2))

### Partition with 3 clusters instead of 4
part3 <- partition(rep(c(1,2,3,3),each=50))
plotTrajMeans(ld,part3)
(cr3 <- qualityCriterion(ld,part3))


### Comparisons of the Partition
plot(c(cr1[[1]],cr2[[1]],cr3[[1]]),main="The highest give the best partition
(according to Calinski & Harabatz criterion)")
par(ask=FALSE)
}

\keyword{package}
\keyword{cluster}  % Clustering
\keyword{methods}
