\name{clusEvol}
\alias{clusEvol}
\title{clusEvol}
\usage{
clusEvol(x=NULL,objects=NULL, time = NULL,target.vars = NULL,
                      time.base=NULL,sel.obj=NULL,init = NULL,logscale = FALSE,
                      ng = NULL,clm = "pam",scale=TRUE,clstats = FALSE,...)
}
\description{

clusEvol is a function that allows us to use exploratory \code{what if} questions in the sense that the present information of an object is plugged-in a dataset in a previous time frame so that we can explore its evolution (and of its neighbours) to the present. 
}


\arguments{
  \item{x}{Dataframe. panel data input.}
  \item{objects}{Character; variable name of objects.}
  \item{time}{Character; variable name of time .}
  \item{target.vars}{Character; selected variables for Cluster Evolution Analytics (CEA).}
  \item{time.base}{Numeric; selected time for CEA.}
  \item{sel.obj}{Character; selected object for CEA.}
  \item{init}{Numeric; initiation time.}
  \item{logscale}{Logical; TRUE if data should be logscaled.}
  \item{ng}{Numeric; number of desired clusters.}
  \item{clm}{Character; (pam,kmeans,choose).}
  \item{scale}{Logical; TRUE if data is scaled in clm.}
  \item{clstats}{Logical; TRUE if cluster statistics should be returned.}
  \item{...}{Parameters used in 'clm' method.}
}

\details{
clusEvol can be synthetized has the following steps

- Identify clusters to which \code{sel.obj} is similar in \code{time.base}
- The data of \code{sel.obj} in \code{time.base} is plugged-in in each time period.
- Clusters are generated in each time period with data from \code{sel.obj} in \code{time.base}.

}

\value{
  Returns an object of class \code{clusEvol}.
  An object of class \code{clusEvol} is a list containing
  at most the following components:
  \item{datos}{clean input data;}
  \item{target.vars}{selected variables;}
  \item{results}{data frame of negighbours of sel.obj;}
  \item{ECk}{clusters that \code{sel.obj} belongs to;}
  \item{ECkTot}{all clusters that \code{sel.obj} belongs to;}
  \item{Clus}{dataframe objects, cluster and time.}
  \item{sumdat}{summary statistics of \code{datos}.}
  \item{kmodelSol}{output of clustering algorithm in each iteration.}
  \item{clusterStats}{cluster statistics in each clustering.}
  \item{sl}{evolution in time with \code{target.vars} included (subset of Clus but only for \code{sel.obj)}.}
  \item{sel}{input variable names of objects in time.}
  \item{sel.obj}{input chatarcher of selected object.}
  
  
  }

\author{Víctor Morales Oñate, \email{victor.morales@uv.cl}, \url{https://sites.google.com/site/moralesonatevictor/},\url{https://www.linkedin.com/in/vmoralesonate/}
Bolívar Morales Oñate, \email{bmoralesonate@gmail.com}, \url{https://sites.google.com/site/moralesonatevictor/}
}

\references{
  Morales-Oñate, V., and Morales-Oñate, B. (2024). \emph{Cluster Evolution Analytics}. \url{https://mpra.ub.uni-muenchen.de/120220/}
}

\examples{
library(clusEvol)
data(actpas)

solclusEvol <- clusEvol(x=actpas,objects="razon_social", 
time = "fecha",target.vars = c("montoAct","operAct"),
                        time.base=max(actpas$fecha),
                        sel.obj="BANCO SOLIDARIO S.A.",init = min(actpas$fecha),
                        logscale = TRUE,ng = 5,clm = "pam")
print(solclusEvol)
}
\keyword{Cluster Evolution Analytics}
