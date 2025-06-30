\name{qwMoRivOmaha}
\docType{data}
\alias{qwMoRivOmaha}
\title{Water-quality data for 06610000 Missouri River at Omaha, Nebr.}
\description{
  Scatterplots of water-quality data for 06610000 Missouri River at Omaha, Nebr.
  
  \if{html}{\figure{06610000Simazine.png}{Simazine concentrations in the
                                          Missouri River at Omaha, Nebr.} \cr}  
  \if{html}{\figure{06610000Prometon.png}{Prometon concentrations in the
                                          Missouri River at Omaha, Nebr.} \cr}
  \if{html}{\figure{06610000Cyanazine.png}{Cyanazine concentrations in the
                                          Missouri River at Omaha, Nebr.} \cr}
  \if{html}{\figure{06610000Metolachlor.png}{Metolachlor concentrations in the
                                          Missouri River at Omaha, Nebr.} \cr}
  \if{html}{\figure{06610000Alachlor.png}{Alachlor concentrations in the
                                          Missouri River at Omaha, Nebr.} \cr}
  \if{html}{\figure{06610000Metribuzin.png}{Metribuzin concentrations in the
                                            Missouri River at Omaha, Nebr.} \cr}
  \if{html}{\figure{06610000Trifluralin.png}{Trifluralin concentrations in the
                                            Missouri River at Omaha, Nebr.} \cr}
  \if{html}{\figure{06610000EPTC.png}{EPTC concentrations in the
                                      Missouri River at Omaha, Nebr.} \cr}  
  \if{latex}{\figure{06610000Simazine.pdf}{Simazine concentrations in the
                                           Missouri River at Omaha, Nebr.} \cr}  
  \if{latex}{\figure{06610000Prometon.pdf}{Prometon concentrations in the
                                           Missouri River at Omaha, Nebr.} \cr}  
  \if{latex}{\figure{06610000Cyanazine.pdf}{Cyanazine concentrations in the
                                          Missouri River at Omaha, Nebr.} \cr}
  \if{latex}{\figure{06610000Metolachlor.pdf}{Metolachlor concentrations in the
                                          Missouri River at Omaha, Nebr.} \cr}
  \if{latex}{\figure{06610000Alachlor.pdf}{Alachlor concentrations in the
                                          Missouri River at Omaha, Nebr.} \cr}
  \if{latex}{\figure{06610000Metribuzin.pdf}{Metribuzin concentrations in the
                                             Missouri River at Omaha, Nebr.} \cr}
  \if{latex}{\figure{06610000Trifluralin.pdf}{Trifluralin concentrations in the
                                            Missouri River at Omaha, Nebr.} \cr}
  \if{latex}{\figure{06610000EPTC.pdf}{EPTC concentrations in the
                                       Missouri River at Omaha, Nebr.} \cr}
}
\details{
  Chemical concentration data are in the columns that start with a P and are 
  followed by a number.  Qualification codes for the concentration data are in 
  the columns that start with an R followed by the same numbers as the 
  associated concentration data.  For example, column P04035 indicates   
  simazine data, 04035, being the U.S. Geological Survey parameter code 
  for simazine.  The qualification codes for the simazine concentrations 
  are found in the column R04035, indicating a U.S. Geological Survey 
  remark code.  Remark codes include _ or nothing, indicating no 
  qualification of the value in the associated concentration field; <,
  indicating a censored value that is less than the number reported in the
  associated concentration field; and E, indicating that the value has 
  been estimated. See Oblinger Childress and others (1999) for information 
  on the remark codes used by the U.S. Geological Survey.
}
\usage{qwMoRivOmaha}
\keyword{datasets}
\format{A data frame containing 115 water-quality samples for eight 
        chemical constituents.  There are  20 variables.
        \tabular{lll}{
          staid \tab character \tab USGS Station identification number \cr
          dates \tab date \tab Date water-quality sample collected \cr
          times \tab numeric \tab Time sample was collected \cr
          R04035 \tab character \tab Remark code (blank, _, <, or E) \cr
          P04035 \tab numeric \tab Simazine, water, filtered, recoverable, micrograms per liter \cr
          R04037 \tab character \tab Remark code (blank, _, <, or E) \cr
          P04037 \tab numeric \tab Prometon, water, filtered, recoverable, micrograms per liter \cr
          R04041 \tab character \tab Remark code (blank, _, <, or E) \cr
          P04041 \tab numeric \tab Cyanazine, water, filtered, recoverable, micrograms per liter \cr
          R39415 \tab character \tab Remark code (blank, _, <, or E) \cr
          P39415 \tab numeric \tab Metolachlor, water, filtered, recoverable, micrograms per liter \cr
          R46342 \tab character \tab Remark code (blank, _, <, or E) \cr
          P46342 \tab numeric \tab Alachlor, water, filtered, recoverable, micrograms per liter \cr
          R82630 \tab character \tab Remark code (blank, _, <, or E) \cr
          P82630 \tab numeric \tab Metribuzin, water, filtered, recoverable, micrograms per liter \cr
          R82661 \tab character \tab Remark code (blank, _, <, or E) \cr
          P82661 \tab numeric \tab Trifluralin, water, filtered (0.7 micron glass fiber filter), recoverable, micrograms per liter \cr
          R82668 \tab character \tab Remark code (blank, _, <, or E) \cr
          P82668 \tab numeric \tab EPTC, water, filtered (0.7 micron glass fiber filter), recoverable, micrograms per liter
        }
}
\examples{
  data(swData)
  
  # summary of water-quality concentrations
  apply(qwMoRivOmaha[, grep("P[[:digit:]]", dimnames(qwMoRivOmaha)[[2]])], 
  2, summary)

  # scatter plot of Simazine concentrations
  cenScatPlot(qwMoRivOmaha, pname = "04035")
  
  # scatter plot with many additional plotting arguments
  par(las = 1, tcl = 0.5)
  cenScatPlot(qwMoRivOmaha, pname = "04035", 
              site = "06610000 Missouri River at Omaha, Nebr.",
              ylabel = "Simazine concentration, in micrograms per liter", 
              legcex = 0.7, qwcols = c("R", "P"),
              ylim = c(0, 0.1), yaxs = "i", 
              xlim = c(as.Date("1996-01-01"), as.Date("2004-01-01")), 
              xaxs = "i", xaxt = "n")
  axdates <- c("1996-01-01", "1998-01-01", "2000-01-01", "2002-01-01", 
            "2004-01-01")
  axis(1, as.Date(axdates), labels = c("1996", "1998", "2000", "2002", "2004"))
  
  # Prometon scatter plot
  cenScatPlot(qwMoRivOmaha, pname = "04037", 
              site = "06610000 Missouri River at Omaha, Nebr.",
              ylabel = "Prometon concentration, in micrograms per liter", 
              legcex = 0.7, qwcols = c("R", "P"),
              ylim = c(0, 0.1), yaxs = "i", 
              xlim = c(as.Date("1996-01-01"), 
                     as.Date("2004-01-01")), xaxs = "i", xaxt = "n")
  axdates <- c("1996-01-01", "1998-01-01", "2000-01-01", "2002-01-01", 
             "2004-01-01")
  axis(1, as.Date(axdates), labels = c("1996", "1998", "2000", "2002", "2004"))

  # Cyanazine scatter plot
  cenScatPlot(qwMoRivOmaha, pname = "04041", 
              site = "06610000 Missouri River at Omaha, Nebr.",
              ylabel = "Cyanazine concentration, in micrograms per liter",
              legcex = 0.7, qwcols = c("R", "P"), 
              ylim = c(0.001, 5), yaxs = "i", 
              xlim = c(as.Date("1996-01-01"), as.Date("2004-01-01")), 
              xaxs = "i", xaxt = "n", log = "y")
  axdates <- c("1996-01-01", "1998-01-01", "2000-01-01", "2002-01-01", 
             "2004-01-01")
  axis(1, as.Date(axdates), labels = c("1996", "1998", "2000", "2002", "2004"))

  # Metolachlor scatter plot
  cenScatPlot(qwMoRivOmaha, pname = "39415", 
              site = "06610000 Missouri River at Omaha, Nebr.",
              ylabel = "Metolachlor concentration, in micrograms per liter",
              legcex = 0.7, qwcols = c("R", "P"),
              ylim = c(0.001,5), yaxs = "i", 
              xlim = c(as.Date("1996-01-01"), as.Date("2004-01-01")), 
              xaxs = "i", xaxt = "n", log = "y", legpos = "bottomleft")
  axdates <- c("1996-01-01", "1998-01-01", "2000-01-01", "2002-01-01", 
             "2004-01-01")
  axis(1, as.Date(axdates), labels = c("1996", "1998", "2000", "2002", "2004"))

  # Alachlor scatter plot
  cenScatPlot(qwMoRivOmaha, pname = "46342", 
              site = "06610000 Missouri River at Omaha, Nebr.",
              ylabel = "Alachlor concentration, in micrograms per liter", 
              legcex = 0.7, qwcols = c("R", "P"),
              ylim = c(0, 0.1), yaxs = "i", 
              xlim = c(as.Date("1996-01-01"), as.Date("2004-01-01")), 
              xaxs = "i", xaxt = "n")
  axdates <- c("1996-01-01", "1998-01-01", "2000-01-01", "2002-01-01", 
             "2004-01-01")
  axis(1, as.Date(axdates), labels = c("1996", "1998", "2000", "2002", "2004"))
  
  # Metribuzin scatter plot
  cenScatPlot(qwMoRivOmaha, pname = "82630", 
              site = "06610000 Missouri River at Omaha, Nebr.",
              ylabel = "Metribuzin concentration, in micrograms per liter",
              legcex = 0.7, qwcols = c("R", "P"),
              ylim = c(0, 0.1), yaxs = "i", 
              xlim = c(as.Date("1996-01-01"), as.Date("2004-01-01")), 
              xaxs = "i", xaxt = "n")
  axdates <- c("1996-01-01", "1998-01-01", "2000-01-01", "2002-01-01", 
             "2004-01-01")
  axis(1, as.Date(axdates), labels = c("1996", "1998", "2000", "2002", "2004"))

  # Trifluralin scatter plot
  cenScatPlot(qwMoRivOmaha, pname = "82661", 
              site = "06610000 Missouri River at Omaha, Nebr.",
              ylabel = "Trifluralin concentration, in micrograms per liter",
              legcex = 0.7, qwcols = c("R", "P"),
              ylim = c(0, 0.03), yaxs = "i", 
              xlim = c(as.Date("1996-01-01"), as.Date("2004-01-01")), 
              xaxs = "i", xaxt = "n")
  axdates<-c("1996-01-01", "1998-01-01", "2000-01-01", "2002-01-01", 
             "2004-01-01")
  axis(1, as.Date(axdates), labels = c("1996", "1998", "2000", "2002", "2004"))

  # EPTC scatter plot
  cenScatPlot(qwMoRivOmaha, pname = "82668", 
              site = "06610000 Missouri River at Omaha, Nebr.",
              ylabel = "EPTC concentration, in micrograms per liter", 
              legcex = 0.7, qwcols = c("R", "P"),
              ylim = c(0.001, 1), yaxs = "i",
              xlim = c(as.Date("1996-01-01"), as.Date("2004-01-01")), 
              xaxs = "i", xaxt = "n", log = "y")
  axdates <- c("1996-01-01", "1998-01-01", "2000-01-01", "2002-01-01", 
             "2004-01-01")
  axis(1, as.Date(axdates), labels =c ("1996", "1998", "2000", "2002", "2004"))
}
\source{Data provided by Patrick Phillips, U.S. Geological Survey,
        New York Water Science Center.}
\references{
  Oblinger Childress, C.J., Foreman, W.T., Connor, B.F., and Maloney, T.J.,
  1999, New reporting procedures based on long-term method detection levels
  and some considerations for interpretations of water-quality data provided
  by the U.S. Geological Survey Open-File Report 99--193, 19 p. [Also 
  available at \url{http://water.usgs.gov/owq/OFR_99-193/index.html}.]
}
\keyword{datasets}
  