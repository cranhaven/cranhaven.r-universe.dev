\name{IllRivValleyCty}
\docType{data}
\alias{IllRivValleyCty}
\title{Water-quality data for 05586100 Illinois River at Valley City, Ill.}
\description{
  Scatterplots of water-quality data for 05586100 Illinois River at 
  Valley City, Ill.
  \if{html}{\figure{05586100Simazine.png}{Simazine concentrations in the
  Illinois River at Valley City, Ill.} \cr}  
  \if{html}{\figure{05586100Prometon.png}{Prometon concentrations in the
  Illinois River at Valley City, Ill.} \cr}
  \if{html}{\figure{05586100Metribuzin.png}{Metribuzin concentrations in the
  Illinois River at Valley City, Ill.} \cr}
  \if{html}{\figure{05586100EPTC.png}{EPTC concentrations in the
  Illinois River at Valley City, Ill.} \cr}  
  \if{latex}{\figure{05586100Simazine.pdf}{Simazine concentrations in the
  Illinois River at Valley City, Ill.} \cr}  
  \if{latex}{\figure{05586100Prometon.pdf}{Prometon concentrations in the
  Illinois River at Valley City, Ill.} \cr}  
  \if{latex}{\figure{05586100Metribuzin.pdf}{Metribuzin concentrations in the
  Illinois River at Valley City, Ill.} \cr}
  \if{latex}{\figure{05586100EPTC.pdf}{EPTC concentrations in the
  Illinois River at Valley City, Ill.} \cr}
}
\details{
  Chemical concentration data are in the columns that start with a P and are 
  followed by a number.  Qualification codes for the concentration data are in 
  the columns that start with an R followed by the same numbers as the 
  associated concentration data.  For example, column P04035 indicates simazine 
  data, 04035, being the U.S. Geological Survey parameter code for simazine.
  The qualification codes for the simazine concentrations are found in 
  the column R04035, indicating a U.S. Geological Survey remark code.
  Remark codes include _ or nothing, indicating no qualification of the 
  value in the associated concentration field; <, indicating a censored
  value that is less than the number reported in the associated 
  concentration field; and E, indicating that the value has been estimated.
  See Oblinger Childress and others (1999) for information on the remark codes 
  used by the U.S. Geological Survey.
  The streamflow and sediment anomalies were generated using the R package 
  waterData (Ryberg and Vecchia, 2012).
}
\usage{IllRivValleyCty}
\keyword{datasets}
\format{A data frame containing 168 water-quality samples for 4 constituents.  
There are  20 variables.
  \tabular{lll}{
   site \tab character \tab Site abbreviation for study \cr
   staid \tab character \tab USGS Station identification number \cr
   dates \tab date \tab Date water-quality sample collected \cr
   yrc \tab numeric \tab Year \cr
   moc \tab numeric \tab Month \cr
   dac \tab numeric \tab Day \cr
   jdayc \tab numeric \tab Julian day from first day of associated streamflow data used \cr
   R04035 \tab character \tab Remark code (blank, _, <, or E) \cr
   P04035 \tab numeric \tab Simazine, water, filtered, recoverable, micrograms per liter \cr
   R04037 \tab character \tab Remark code (blank, _, <, or E) \cr
   P04037 \tab numeric \tab Prometon, water, filtered, recoverable, micrograms per liter \cr
   R82630 \tab character \tab Remark code (blank, _, <, or E) \cr
   P82630 \tab numeric \tab Metribuzin, water, filtered, recoverable, micrograms per liter \cr
   R82668 \tab character \tab Remark code (blank, _, <, or E) \cr
   P82668 \tab numeric \tab EPTC, water, filtered (0.7 micron glass fiber filter), recoverable, micrograms per liter \cr
   dflow \tab numeric \tab Streamflow, cubic feet per second \cr
   flowa30 \tab numeric \tab 30-day streamflow anomaly \cr
   flowa1 \tab numeric \tab 1-day streamflow anomaly \cr
   dsed \tab numeric \tab Sediment concentration, milligrams per liter \cr
   seda30 \tab numeric \tab 30-day sediment anomaly \cr
   seda1 \tab numeric \tab 1-day sediment anomaly
  }
}
\examples{
data(swData)

# summary of water-quality concentrations
apply(IllRivValleyCty[, grep("P[[:digit:]]", 
dimnames(IllRivValleyCty)[[2]])], 2, summary)

# scatter plot of simazine concentrations
cenScatPlot(IllRivValleyCty, pname = "04035")

# Simazine scatter plot with many additional plotting arguments

par(las = 1, tcl = 0.5)
cenScatPlot(IllRivValleyCty, pname = "04035", 
            site = "05586100 Illinois River at Valley City, Ill.",
            ylabel = "Simazine concentration, in micrograms per liter", 
            legcex = 0.7, 
            ylim = c(0, 0.4), yaxs = "i", 
            xlim = c(as.Date("1996-01-01"), as.Date("2012-01-01")), 
            xaxs = "i", xaxt = "n")
axdates <- c("1996-01-01", "2000-01-01", "2004-01-01", "2008-01-01",
           "2012-01-01")
axis(1, as.Date(axdates), labels = c("1996", "2000", "2004", "2008", "2012"))

# Prometon scatter plot
cenScatPlot(IllRivValleyCty, pname = "04037", 
            site = "05586100 Illinois River at Valley City, Ill.",
            ylabel = "Prometon concentration, in micrograms per liter", 
            legcex = 0.7, 
            ylim = c(0, 0.06), yaxs = "i", 
            xlim=c(as.Date("1996-01-01"), as.Date("2012-01-01")), xaxs = "i", 
            xaxt = "n")
axdates<-c("1996-01-01", "2000-01-01", "2004-01-01", "2008-01-01", "2012-01-01")
axis(1, as.Date(axdates), labels = c("1996", "2000", "2004", "2008","2012"))

# Metribuzin scatter plot
cenScatPlot(IllRivValleyCty, pname="82630", 
            site="05586100 Illinois River at Valley City, Ill.",
            ylabel="Metribuzin concentration, in micrograms per liter", 
            legcex=0.7, 
            ylim=c(0,0.3), yaxs="i", cex.lab=0.9, cex.axis=0.9,
            xlim=c(as.Date("1996-01-01"), 
                   as.Date("2012-01-01")), xaxs="i", 
            xaxt="n")
axdates<-c("1996-01-01", "2000-01-01", "2004-01-01", "2008-01-01", 
           "2012-01-01")
axis(1, as.Date(axdates), labels=c("1996", "2000", "2004", "2008",
                                               "2012"), cex.axis=0.9)
# EPTC scatter plot
cenScatPlot(IllRivValleyCty, pname = "82668", 
            site = "05586100 Illinois River at Valley City, Ill.",
            ylabel = "EPTC concentration, in micrograms per liter", 
            legcex = 0.7, ylim = c(0, 0.08), yaxs = "i", 
            xlim=c(as.Date("1996-01-01"), as.Date("2012-01-01")), 
            xaxs = "i", xaxt = "n")
axdates <- c("1996-01-01", "2000-01-01", "2004-01-01", "2008-01-01", 
           "2012-01-01")
axis(1, as.Date(axdates), labels = c("1996", "2000", "2004", "2008","2012"))
}
\source{Data provided by Patrick Phillips, U.S. Geological Survey,
        New York Water Science Center.}
\references{
  Oblinger Childress, C.J., Foreman, W.T., Connor, B.F., and Maloney, T.J.,
  1999, New reporting procedures based on long-term method detection levels
  and some considerations for interpretations of water-quality data provided
  by the U.S. Geological Survey Open-File Report 99--193, 19 p. [Also 
  available at \url{https://water.usgs.gov/owq/OFR_99-193/index.html}.]
}
\keyword{datasets}


