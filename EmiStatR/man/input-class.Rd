\name{input-class}
\Rdversion{1.1}
\docType{class}
\alias{input-class}
\alias{input}
\alias{EmiStatR,input-method}

\title{Class \code{"input"}}
\description{
The class provides a container for inputs required to invoke \code{EmiStatR} method.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{input()} or \code{new("input")}.
}
\section{Slots}{
  \describe{
    \item{\code{spatial}:}{Object of class \code{"numeric"}, 0 (default ) for non-spatial 
    input, 1 for spatial input (not implemented).}
    \item{\code{zero}:}{Object of class \code{"numeric"}, aproximation to zero value. 
    Default 1E-5.}
    %\item{\code{mc}:}{Object of class \code{"numeric"}, 0 (default) for simulation 
    %without Monte Carlo sampling, 1 for Monte Carlo simulations with hydraulic 
    %parameters (not implemented), 2 for Monte Carlo simulations with hydraulic 
    %and water quality parameters (not implemented), 3 for Monte Carlo simulations 
    %and Latin Hypercube sampling with hydraulic and water quality parameters 
    %(not implemented).} 
    \item{\code{folder}:}{Object of class \code{"character"}, path for the output.    
    Default \code{getwd()}} 
    %\item{\code{folderOutput}:}{Object of class \code{"character"}, path of the 
    %directory to save outputs. By default the same as \code{folder}. }
    \item{\code{cores}:}{Object of class \code{"numeric"}, number of CPU cores 
    to be used in parallel computing. If cores = 0 no parallel computation is  
    done. Default 1.}
    \item{\code{ww}:}{Object of class \code{"list"}. This list contains three 
    numeric elements for the wastewater characteristics. First element
    \code{qs}, individual water consumption of households [l/(PE d)]. Second
    element \code{CODs}, sewage pollution - COD concentration [g/(PE d)]. Third
    element \code{NH4s}, sewage pollution - NH4 concentration [g/(PE d)].}
    \item{\code{inf}:}{Object of class \code{"list"}. This list contains three 
    numeric elements for infiltration water characteristics. First 
    element \code{qf}, infiltration water inflow [l/(s ha)]. Second element
    \code{CODf}, infiltration water pollution - COD concentration [g/(PE d)].
    Third element \code{NH4f}, infiltration water pollution - NH4 concentration 
    [g/(PE d)].}
    \item{\code{rw}:}{Object of class \code{"list"}. This list contains three 
    elements for rainwater characteristics. First 
    element \code{CODr} (numeric), rainwater pollution - COD concentration [mg/l].
    Second element \code{NH4r} (numeric), rainwater pollution - NH4 concentration 
    [mg/l]. Third element \code{stat} (character), name of the rain measurement 
    station.}
    \item{\code{P1}:}{Object of class \code{"data.frame"} with two columns named 
    1. "time" for specifying the date and time in format YYYY-m-d HH:MM:SS, 
    2. "P [mm]" for specifying the depth values of the rainfall time series in millimeters. 
    Optionally, instead of rainfall depth values can be provided values of direct runoff 
    in cubic meters entering in the system. 
    If runoff values are provided then the second column containing these values
    should be named as "Runoff_Volume" or "runoff_volume", 
    otherwise this column is treated as rainfall depth.}
    \item{\code{st}:}{Object of class 
    \code{"list"}. This object contains n lists, 
    where n is the number of structures to simulate. Every list should contain 18
    elements: 
    \code{id}, numeric, identification number [-]; 
    \code{ns}, character, name of the structure [-];
    \code{nm}, character, name of the municipality [-]; 
    \code{nc}, character, name of the catchment [-];
    \code{numc}, numeric, number of the catchment [-]; 
    \code{use}, character, use of the soil [-]; 
    \code{Atotal}, numeric, total area [ha]; 
    \code{Aimp}, numeric, impervious area [ha];
    \code{tfS}, numeric, time flow structure [time step]; 
    \code{pe}, numeric, population equivalent [PE];
    \code{V}, numeric, volume [m3]; 
    \code{lev2vol}, list of 2, lev and vol, defining the curve lev (level [m]) 
    to vol (volume [m3]);
    \code{Qd}, numeric, maximum throttled outflow [l/s]; 
    \code{Dd}, numeric, orifice diameter [m];
    \code{Cd}, numeric, orifice coefficient [-];  
    \code{Cimp}, numeric, coefficient for impervious area [-]; and 
    \code{Cper}, numeric, coefficient for pervious area [-].}
    
    \item{\code{pe.ts.file}:}{Object of class \code{"character"} with the path and file 
    name of the comma separated value (csv) file that contains the montly (seasonal) 
    factors for population equivalent (\code{pe}). The first 
    column of this file should be "time" in format "Y-m-d H:M:S" and should span for the
    entire length of the desired time series. 
    The second column should contain the population equivalent as numeric class for the 
    specified time, i.e. the desired \code{pe} time series with daily, weekly and monthly factors
    already applied.
    Default empty string (""). 
    If not empty string is defined then \code{pe.daily.file}, \code{pe.weekly}, \code{pe.seasonal}
    are omitted.}
    \item{\code{pe.daily.file}:}{Object of class \code{"data.frame"} that contains the daily factors for population
    equivalent. The first 
    column should be the time in format "H:M:S" and should span for 24 hours. 
    The second column should contain the factors as numeric class for the specified time.     These factors must average to 1.}
    \item{\code{pe.weekly}:}{Object of class \code{"list"} that contains the factors for population
    equivalent per day of the week with 7 elements called "mon" for Monday, "tue" for Tuesday, 
    "wed" for Wednesday, "thu" for Thursday, "fri" for Friday, "sat" for Saturday, and "sun" for Sunday. 
    These factors must average to 1.}
    \item{\code{pe.seasonal}:}{Object of class \code{"list"} that contains the factors for population
    equivalent per month of the year with 12 elements called with the three first lower case letters 
    of the month from "jan" for January to "dec" for December. These factors must average to 1.}
    \item{\code{qs.ts.file}:}{Object of class \code{"character"} with the path and file 
    name of the comma separated value (csv) file that contains the monthly (seasonal) 
    factors for water consumption (\code{qs}). The first 
    column of this file should be "time" in format "Y-m-d H:M:S" and should span for the
    entire length of the desired time series. 
    The second column should contain the population equivalent as numeric class for the 
    specified time, i.e. the desired \code{qs} time series with daily, weekly and monthly factors
    already applied.
    Default empty string (""). 
    If not empty string is defined then \code{pe.daily.file}, \code{pe.weekly}, \code{pe.seasonal}
    are omitted.}
    \item{\code{qs.daily.file}:}{Object of class \code{"character"} with the path and file 
    name of the comma separated value (csv) file that contains the daily factors for water
    consumption. The first 
    column of this file should be the time in format "H:M:S" and should span for 24 hours. 
    The second column should contain the factors as numeric class for the specified time. 
    These factors must average to 1.}
    \item{\code{qs.weekly}:}{Object of class \code{"list"} that contains the factors for water 
    consumption per day of the week with 7 elements called "mon" for Monday, "tue" for Tuesday, 
    "wed" for Wednesday, "thu" for Thursday, "fri" for Friday, "sat" for Saturday, and "sun" for Sunday. 
    These factors must average to 1.}
    \item{\code{qs.seasonal}:}{Object of class \code{"list"} that contains the factors for water 
    consumption per month of the year with 12 elements called with the three first lower case letters 
    of the month from "jan" for January to "dec" for December. These factors must average to 1.}
    \item{\code{export}:}{Object of class \code{"numeric"}. If 1 (default) then the 
    results are saved in \code{folderOutput}. Set to 0 for not writing in 
    output files.}
  }
}

\section{Methods}{
  \describe{
    \item{EmiStatR}{\code{signature(x = "input")}: execute {EmiStatR} function }
    }
}

%\references{
%%%  ~~put references to the literature/web site here~~
%}

\author{
J.A. Torres-Matallana
}

%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
%}
\examples{
## loading EmiStatR
library("EmiStatR")
showClass("input")

## running EmiStatR with user defined input
data("Esch_Sure2010")
P1 <- Esch_Sure2010[1:1000,]  # selecting just the first 1,000 rows
station <- "Esch-sur-Sure"

# defining estructures E1
E1 <- list(id = 1, ns = "FBH Goesdorf", nm = "Goesdorf", nc = "Obersauer", numc = 1, 
           use = "R/I", Atotal = 36, Aimp = 25.2, Cimp = 0.80, Cper = 0.30,
           tfS = 1, pe = 650, Qd = 5, 
           Dd = 0.15, Cd = 0.18, V = 190, lev.ini = 0.10,
           lev2vol = list(lev = c(.06, 1.10, 1.30, 3.30), 
                          vol = c(0.5, 31, 45, 190))
           )

# defining Input objet
input.user <- input(spatial = 0, zero = 1e-5, folder = getwd(),
                    cores = 1,
                    ww = list(qs = 150, CODs = 104, NH4s = 4.7), 
                    inf = list(qf= 0.04, CODf = 0, NH4f =0),
                    rw = list(CODr = 0, NH4r = 0, stat = station), 
                    P1 = P1, st = list(E1=E1), export = 0)

str(input.user)

# invoking EmiStatR
sim <- EmiStatR(input.user)

## a visualisation example 
dev.new()
par(mfrow=c(2,2), oma = c(0,0,2,0))
plot(x=sim[[1]][[1]][[2]], y=sim[[1]][[1]][[3]], typ="l", col="blue", 
     xlab = "time", ylab = colnames(sim[[1]][[1]])[3], main = "Precipitation")
plot(x=sim[[1]][[1]][[2]], y=sim[[1]][[1]][[10]], typ="l", col="blue", 
     xlab = "time", ylab = colnames(sim[[1]][[1]])[10], main = "CSO, volume")
plot(x=sim[[1]][[1]][[2]], y=sim[[1]][[1]][[13]], typ="l", col="blue", 
     xlab = "time", ylab = colnames(sim[[1]][[1]])[13], main = "CSO, COD concentration")
plot(x=sim[[1]][[1]][[2]], y=sim[[1]][[1]][[14]], typ="l", col="blue", 
     xlab = "time", ylab = colnames(sim[[1]][[1]])[14], main = "CSO, NH4 concentration")
mtext(paste("Structure", sim[[1]][[3]][[1]]), outer=TRUE, cex = 1.5)
}

\keyword{classes}

%\usage{
%Input(...)
%}
%\arguments{
%  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
%}
%}