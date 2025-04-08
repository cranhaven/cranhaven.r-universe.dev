\name{nassCDS}
\alias{nassCDS}
\docType{data}
\title{Airbag and other influences on accident fatalities}

\description{US data, for 1997-2002, from police-reported car crashes in
which there is a harmful event (people or property), and from which at
least one vehicle was towed. Data are restricted to front-seat
occupants, include only a subset of the variables recorded, and
are restricted in other ways also.}
\usage{nassCDS}
\format{
  A data frame with 26217 observations on the following 15 variables.
  \describe{
    \item{\code{dvcat}}{ordered factor with levels (estimated impact
      speeds) \code{1-9km/h}, \code{10-24}, \code{25-39}, \code{40-54},
      \code{55+}
    }
    \item{\code{weight}}{Observation weights, albeit of uncertain
      accuracy, designed to account for varying sampling probabilities.}
    \item{\code{dead}}{factor with levels \code{alive} \code{dead}}
    \item{\code{airbag}}{a factor with levels \code{none} \code{airbag}}
    \item{\code{seatbelt}}{a factor with levels \code{none} \code{belted}}
    \item{\code{frontal}}{a numeric vector; 0 = non-frontal, 1=frontal impact}
    \item{\code{sex}}{a factor with levels \code{f} \code{m}}
    \item{\code{ageOFocc}}{age of occupant in years}
    \item{\code{yearacc}}{year of accident}
     \item{\code{yearVeh}}{Year of model of vehicle; a numeric vector}
     \item{\code{abcat}}{Did one or more (driver or passenger) airbag(s)
       deploy? This factor has levels \code{deploy} \code{nodeploy}
       \code{unavail}}
    \item{\code{occRole}}{a factor with levels \code{driver} \code{pass}}
    \item{\code{deploy}}{a numeric vector: 0 if an airbag was
      unavailable or did not deploy; 1 if one or more bags deployed.}
    \item{\code{injSeverity}}{a numeric vector; 0:none, 1:possible injury,
      2:no incapacity, 3:incapacity, 4:killed; 5:unknown, 6:prior death}
    \item{\code{caseid}}{character, created by pasting together the
      populations sampling unit, the case number, and the vehicle
      number.  Within each year, use this to uniquely identify the
      vehicle.
      }
  }
  }
\details{
  Data collection used a multi-stage probabilistic sampling scheme.
  The observation weight, called national inflation factor
  (\code{national}) in the data from NASS, is the inverse
  of an estimate of the selection probability.  These data
  include a subset of the variables from the NASS dataset.  Variables
  that are coded here as factors are coded as numeric values in that
  dataset.
}

\source{
  \url{https://www.stat.colostate.edu/~meyer/airbags.htm}
%  \url{ftp://ftp.nhtsa.dot.gov/nass/}

See also\\
\url{https://maths-people.anu.edu.au/~johnm/datasets/airbags/}
}
\references{
  Meyer, M.C. and Finney, T. (2005): \emph{Who wants airbags?}. Chance
  18:3-16.

  Farmer, C.H. 2006. \emph{Another look at Meyer and Finney's \sQuote{Who wants
    airbags?}}. Chance 19:15-22.

  Meyer, M.C. 2006.  \emph{Commentary on "Another look at Meyer and
    Finney's \sQuote{Who wants airbags?}}. Chance 19:23-24.

  For analyses based on the alternative FARS (Fatal Accident Recording
  System) data, and associated commentary, see:

  Cummings, P; McKnight, B, 2010. \emph{Accounting for vehicle, crash, and
occupant characteristics in traffic crash studies.} Injury Prevention 16:
363-366. [The relatively definitive analyses in this paper use a matched
cohort design,

  Olson, CM; Cummings, P, Rivara, FP, 2006. \emph{Association of first- and
  second-generation air bags with front occupant death in car crashes: a
  matched cohort study.} Am J Epidemiol 164:161-169. [The relatively
definitive analyses in this paper use a matched cohort design, using
data taken from the FARS (Fatal Accident Recording System) database.]

Braver, ER; Shardell, M; Teoh, ER, 2010. \emph{How have changes in air
bag designs affected frontal crash mortality?} Ann Epidemiol 20:499-510.


The web page \url{http://www-fars.nhtsa.dot.gov/Main/index.aspx} has a
    menu-based interface into the FARS (Fatality Analysis Recording
    System) data. The FARS database aims to include every accident in
    which there was at least one fatality.
}
\examples{
data(nassCDS)
xtabs(weight ~ dead + airbag, data=nassCDS)
xtabs(weight ~ dead + airbag + seatbelt + dvcat, data=nassCDS)
tab <- xtabs(weight ~ dead + abcat, data=nassCDS,
             subset=dvcat=="25-39"&frontal==0)[, c(3,1,2)]
round(tab[2, ]/apply(tab,2,sum)*100,2)
}
\keyword{datasets}
