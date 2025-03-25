\name{EmiStatR-methods}
\docType{methods}
\alias{EmiStatR-methods}
\alias{EmiStatR}
%\alias{EmiStatR,Input-method}
\title{S4 Methods for Function \code{EmiStatR}}
\description{
 S4 methods for function \code{EmiStatR}. Given the inputs either from the Shiny 
 applications "EmiStatR_input - Shiny" and "EmiStatR_inputCSO - Shiny" or 
 user-defined, 
 the methods invoke the main core of the tool and writes the output files 
 in the specified folder. 
}

\usage{
EmiStatR(x)
}

\arguments{
\item{x}{An object of class input}
}

\value{Object of class \code{"list"}. This object contains N lists, 
    where N is the number of structures to simulate. Each list contains
    a list with three elements: a data.frame named "out1", a data.frame 
    named "out2", a vector named "lista". "out1" contains n observations of
    24 variables, where n is the length of the precipitation time series. The 
    24 variables are the following time series: \cr 
    1. id, identification number \cr
    2. Time [y-m-d h:m:s] \cr 
    3. P [mm], precipitation \cr
    4. i [mm/(min)], intensity (if available) \cr
    5. V_r [m3], rain water volume \cr
    6. V_dw [m3], dry weather volume \cr
    7. cs_mr [-], combined sewage mixing ratio \cr
    8. o_tfyn [yes=1/no=0], status variable to know when the Combined 
    Sewer Overflow Tank (CSOT) is filling up \cr
    9. V_Tank [m3], volume of CSOT filling up\cr
    10. V_Ov [m3], overflow volume \cr
    11. B_COD_Ov [kg], Chemical Oxygen Demand (COD) overflow load \cr 
    12. B_NH4_Ov [kg], ammonium (NH4) overflow load \cr 
    13. C_COD_Ov [mg/l], COD overflow concentration \cr
    14. C_NH4_Ov [mg/l], NH4 overflow concentration \cr
    15. d_Ov [min], total duration of overflows\cr
    16. f_Ov [ocurrence], frequency of overflows (just an approximation) \cr
    17. V_InTank [m3], volume at entrance of the CSOT \cr
    18. B_COD_InTank [Kg], COD load at entrance of the CSOT \cr 
    19. B_NH4_InTank [Kg], NH4 load at entrance of the CSOT \cr
    20. C_COD_InTank [mg/l], COD concentration at entrance of the CSOT \cr
    21. C_NH4_InTank [mg/l], NH4 concentration at entrance of the CSOT  \cr
    22. Q_Ov [l/s], overflow flow \cr
    23  pe.season [PE], population equivalents in the catchment \cr
    24  qs.season [l/PE/d], water consumption in the catchment \cr 
    \cr
    The summary of the overflow data, "out2", contains 15 observations of 
    2 variables. The 15 observations are:\cr
    1. Period [day], length of time of the precipitation time series \cr
    2. Duration, d_Ov, [min], overflow duration\cr
    3. Frecuency, f_Ov, [ocurrence] (aprox.), overflow frecuency \cr
    4. Total volume, V_Ov, [m3], total overflow volume\cr
    5. Average flow, Q_Ov, [l/s], average overflow flow\cr
    6. Total COD load, B_COD_Ov, [kg], total COD load in overflow\cr
    7. Average COD concentration, C_COD_ov_av, [mg/l], in overflow\cr
    8. 99.9th percentile COD concentration, C_COD_Ov_99.9, [mg/l], in overflow\cr 
    9. Maximum COD concentration, C_COD_Ov_max, [mg/l], in overflow\cr
    10. Total NH4 load, B_NH4_Ov, [kg], total NH4 load in overflow\cr
    11. Average NH4 concentration,C_NH4_Ov_av, [mg/l], in overflow\cr
    12. 99.9th percentile NH4 concentration, C_NH4_Ov_99.9, [mg/l], in overflow\cr
    13. Maximum NH4 concentration, C_NH4_Ov_max, [mg/l], in overflow\cr
    14. Structure summary results, (a descriptive text line)\cr
    15. Volume Tank, VTank [m3], total volumen in the CSO tank\cr
    \cr 
    "Lista" contains the identification name(s) of the N structure(s). 
    If export is allowed then three 
    plain text .csv files are created, one for "out1", the second for "out2",
    the third one a summary for all the structures based in "out2". 
    Also, one .pdf file is printed which illustrates
    the precipitation and Combined Sewer Overflow (CSO) volume, COD concentration,
    and NH4 concentration time series. These files are exported
    to the directory EmiStatR_output located in the \code{folderOutput} path.}

\section{Methods}{
\describe{

\item{\code{signature(x = "input")}}{
execute {EmiStatR} function
}
}}

\author{
J.A. Torres-Matallana; K. Klepiszewski; U. Leopold; G.B.M. Heuvelink
}

\examples{
## running GUI 
library("EmiStatR")

appDir <- system.file("shiny", package = "EmiStatR")

## (uncomment for running)
# setwd(appDir)
# runApp("EmiStatR_input")
# runApp("EmiStatR_inputCSO")

## executing EmiStatR
input.default <- input()

## uncomment following lines to execute
# sim   <- EmiStatR(input.default)
# str(sim)

## a dummy example of plot
# par(mfrow=c(2,2), oma = c(0,0,2,0))
# plot(x=sim[[1]][[1]][[2]], y=sim[[1]][[1]][[3]], typ="l", col="blue", 
#      xlab = "time", ylab = colnames(sim[[1]][[1]])[3], main = "Precipitation")
# plot(x=sim[[1]][[1]][[2]], y=sim[[1]][[1]][[10]], typ="l", col="blue", 
#      xlab = "time", ylab = colnames(sim[[1]][[1]])[10], main = "CSO, volume")
# plot(x=sim[[1]][[1]][[2]], y=sim[[1]][[1]][[13]], typ="l", col="blue", 
#      xlab = "time", ylab = colnames(sim[[1]][[1]])[13], main = "CSO, COD concentration")
# plot(x=sim[[1]][[1]][[2]], y=sim[[1]][[1]][[14]], typ="l", col="blue", 
#      xlab = "time", ylab = colnames(sim[[1]][[1]])[14], main = "CSO, NH4 concentration")
# mtext(paste("Structure", sim[[1]][[3]][[1]]), outer=TRUE, cex = 1.5)
}

\keyword{methods}
\keyword{input}
\keyword{EmiStatR}