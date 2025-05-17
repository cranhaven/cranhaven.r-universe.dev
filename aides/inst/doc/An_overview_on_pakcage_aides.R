## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = FALSE, fig.align = "left", out.width = 100------------------------
knitr::include_graphics("aides_logo.png")

## -----------------------------------------------------------------------------
library(aides)

## ----setup, echo = FALSE, warning = FALSE, message = FALSE--------------------
library(aides)
library(meta)

## ----eval = FALSE-------------------------------------------------------------
#  library(meta)
#  data("Olkin1995")
#  dataOlkin1995 <- Olkin1995

## ----eval = FALSE-------------------------------------------------------------
#  dataOlkin1995$n <- dataOlkin1995$n.exp + dataOlkin1995$n.cont

## ----eval = FALSE-------------------------------------------------------------
#  shapiro.test(dataOlkin1995$n)
#  ks.test(dataOlkin1995$n, "pnorm")

## ----result-distrSS, eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE----
data("Olkin1995")
dataOlkin1995 <- Olkin1995
dataOlkin1995$n <- dataOlkin1995$n.exp + dataOlkin1995$n.cont
shapiro.test(dataOlkin1995$n)
ks.test(dataOlkin1995$n, "pnorm")

## ----eval = FALSE-------------------------------------------------------------
#  PlotDistrSS(n = n,
#              data = dataOlkin1995,
#              study = author,
#              time = year)

## ----eval = FALSE-------------------------------------------------------------
#  PlotDistrSS(n = n,
#              data = dataOlkin1995,
#              study = author,
#              time = year,
#              method = "ks")

## ----plot-distrSS, fig.cap = "An example for visualization of distribution of study sizes", eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE, results = "hide", fig.height = 5, fig.width = 8, fig.align = "center", out.width = "100%"----
data("Olkin1995")
dataOlkin1995 <- Olkin1995
dataOlkin1995$n <- dataOlkin1995$n.exp + dataOlkin1995$n.cont
PlotDistrSS(n = n,
            data = dataOlkin1995, 
            study = author, 
            time = year,
            method = "ks")

## ----eval = FALSE-------------------------------------------------------------
#  TestDisparity(n = n,
#                data = dataOlkin1995,
#                study = author,
#                time = year)

## ----result-disparity, eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE----
data("Olkin1995")
dataOlkin1995 <- Olkin1995
dataOlkin1995$n <- dataOlkin1995$n.exp + dataOlkin1995$n.cont
TestDisparity(n = n, 
              data = dataOlkin1995, 
              study = author, 
              time = year)

## ----eval = FALSE-------------------------------------------------------------
#  TestDisparity(n = n,
#                data = dataOlkin1995,
#                study = author,
#                time = year,
#                plot = TRUE)

## ----plot-disparity-outlier, eval = TRUE, fig.cap = "An example for disparity-outlier plot", echo = FALSE, warning = FALSE, message = FALSE, results = "hide", fig.height = 4, fig.width = 8, fig.align = "center", out.width = "100%"----
data("Olkin1995")
dataOlkin1995   <- Olkin1995
dataOlkin1995$n <- dataOlkin1995$n.exp + dataOlkin1995$n.cont
TestDisparity(n = n,
              data = dataOlkin1995, 
              study = author, 
              time = year, 
              plot = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  rsltDisparity <- TestDisparity(n = n,
#                                 data = dataOlkin1995,
#                                 study = author,
#                                 time = year,
#                                 vrblty = "MAD")

## ----result-disparity-vrblty-MAD, eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE----
data("Olkin1995")
dataOlkin1995 <- Olkin1995
dataOlkin1995$n <- dataOlkin1995$n.exp + dataOlkin1995$n.cont
rsltDisparity <- TestDisparity(n = n, 
                               data = dataOlkin1995, 
                               study = author, 
                               time = year,
                               vrblty = "MAD")

## ----eval = FALSE-------------------------------------------------------------
#  PlotDisparity(rsltDisparity,
#                which = "CV",
#                szFntAxsX = 1)

## ----plot-disparity-variability-MAD, eval = TRUE, fig.cap = "An example for disparity-variability (robust) plot", echo = FALSE, warning = FALSE, message = FALSE, results = "hide", fig.height = 6, fig.width = 8, fig.align = "center", out.width = "100%"----
data("Olkin1995")
dataOlkin1995   <- Olkin1995
dataOlkin1995$n <- dataOlkin1995$n.exp + dataOlkin1995$n.cont
rsltDisparity <- TestDisparity(n = n,
                               data = dataOlkin1995, 
                               study = author, 
                               time = year,
                               vrblty = "MAD", 
                               plot = FALSE)

PlotDisparity(rsltDisparity, 
              which = "CV",
              szFntAxsX = 1)

## ----eval = FALSE-------------------------------------------------------------
#  library(meta)
#  data("Fleiss1993bin")
#  dataFleiss1993bin <- Fleiss1993bin

## ----eval = FALSE-------------------------------------------------------------
#  dataFleiss1993bin$n  <- dataFleiss1993bin$n.asp + dataFleiss1993bin$n.plac
#  dataFleiss1993bin$se <- sqrt((1 / dataFleiss1993bin$d.asp) - (1 / dataFleiss1993bin$n.asp) + (1 / dataFleiss1993bin$d.plac) - (1 / dataFleiss1993bin$n.plac))

## ----eval = FALSE-------------------------------------------------------------
#  TestDiscordance(n = n,
#                  se = se,
#                  study = study,
#                  data = dataFleiss1993bin)

## ----result-discordance, eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE----
data("Fleiss1993bin")
dataFleiss1993bin    <- Fleiss1993bin
dataFleiss1993bin$n  <- dataFleiss1993bin$n.asp + dataFleiss1993bin$n.plac
dataFleiss1993bin$se <- sqrt((1 / dataFleiss1993bin$d.asp) - (1 / dataFleiss1993bin$n.asp) + (1 / dataFleiss1993bin$d.plac) - (1 / dataFleiss1993bin$n.plac))
TestDiscordance(n = n, 
                se = se, 
                study = study,
                data = dataFleiss1993bin)

## ----eval = FALSE-------------------------------------------------------------
#  TestDiscordance(n = n,
#                  se = se,
#                  study = study,
#                  data = dataFleiss1993bin,
#                  plot = TRUE)

## ----plot-discordance, fig.cap = "An example for discordance plot", eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE, results = "hide", fig.height = 6, fig.width = 12, fig.align = "center", out.width = "100%"----
data("Fleiss1993bin")
dataFleiss1993bin <- Fleiss1993bin
dataFleiss1993bin$n  <- dataFleiss1993bin$n.asp + dataFleiss1993bin$n.plac
dataFleiss1993bin$se <- sqrt((1 / dataFleiss1993bin$d.asp) - (1 / dataFleiss1993bin$n.asp) + (1 / dataFleiss1993bin$d.plac) - (1 / dataFleiss1993bin$n.plac))
TestDiscordance(n = n, 
                se = se, 
                study = study, 
                data = dataFleiss1993bin, 
                plot = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  library(meta)
#  data("Fleiss1993bin")
#  dataFleiss1993bin <- Fleiss1993bin

## ----eval = FALSE-------------------------------------------------------------
#  DoSA(Fleiss1993bin,
#       source = study,
#       time = year,
#       r1 = d.asp,
#       n1 = n.asp,
#       r2 = d.plac,
#       n2 = n.plac,
#       measure = "RR",
#       PES = 0.1,
#       RRR = 0.2,
#       group = c("Aspirin", "Placebo"))

## ----result-DoSA, eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE----
data("Fleiss1993bin")
dataFleiss1993bin <- Fleiss1993bin
DoSA(Fleiss1993bin, 
     source = study, 
     time = year,
     r1 = d.asp, 
     n1 = n.asp, 
     r2 = d.plac, 
     n2 = n.plac, 
     measure = "RR",
     PES = 0.1,
     RRR = 0.2,
     group = c("Aspirin", "Placebo"))

## ----eval = FALSE-------------------------------------------------------------
#  DoSA(Fleiss1993bin,
#       source = study,
#       time = year,
#       r1 = d.asp,
#       n1 = n.asp,
#       r2 = d.plac,
#       n2 = n.plac,
#       measure = "RR",
#       PES = 0.1,
#       RRR = 0.2,
#       group = c("Aspirin", "Placebo"),
#       plot = TRUE)

## ----plot-sequential, fig.cap = "An example for sequential analysis", eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE, results = "hide", fig.height = 6, fig.width = 8, fig.align = "center", out.width = "100%"----
data("Fleiss1993bin")
dataFleiss1993bin <- Fleiss1993bin
DoSA(Fleiss1993bin, 
     source = study, 
     time = year,
     r1 = d.asp, 
     n1 = n.asp, 
     r2 = d.plac, 
     n2 = n.plac, 
     measure = "RR",
     PES = 0.1,
     RRR = 0.2,
     group = c("Aspirin", "Placebo"),
     plot = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  DoOSA(Fleiss1993bin,
#        source = study,
#        time = year,
#        r1 = d.asp,
#        n1 = n.asp,
#        r2 = d.plac,
#        n2 = n.plac,
#        measure = "RR",
#        group = c("Aspirin", "Placebo"))

## ----result-OSA, eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE----
data("Fleiss1993bin")
dataFleiss1993bin <- Fleiss1993bin
DoOSA(Fleiss1993bin, 
      source = study, 
      time = year,
      r1 = d.asp, 
      n1 = n.asp, 
      r2 = d.plac, 
      n2 = n.plac, 
      measure = "RR",
      group = c("Aspirin", "Placebo"))

## ----eval = FALSE-------------------------------------------------------------
#  DoOSA(Fleiss1993bin,
#        source = study,
#        time = year,
#        r1 = d.asp,
#        n1 = n.asp,
#        r2 = d.plac,
#        n2 = n.plac,
#        measure = "RR",
#        group = c("Aspirin", "Placebo"),
#        plot = TRUE)

## ----plot-OSA, fig.cap = "An example for illustrating observed sequential analysis", eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE, results = "hide", fig.height = 6, fig.width = 8, fig.align = "center", out.width = "100%"----
data("Fleiss1993bin")
dataFleiss1993bin <- Fleiss1993bin
DoOSA(Fleiss1993bin,
      source = study,
      time = year,
      r1 = d.asp,
      n1 = n.asp,
      r2 = d.plac,
      n2 = n.plac,
      measure = "RR",
      group = c("Aspirin", "Placebo"),
      plot = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  output <- DoOSA(Fleiss1993bin,
#                  source = study,
#                  time = year,
#                  r1 = d.asp,
#                  n1 = n.asp,
#                  r2 = d.plac,
#                  n2 = n.plac,
#                  measure = "RR",
#                  group = c("Aspirin", "Placebo"),
#                  SAP = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  PlotOSA(output,
#          lgcZone = TRUE)

## ----plot-OSA-zone, fig.cap = "An example for illustrating colorful zones on observed sequential analysis", eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE, results = "hide", fig.height = 8, fig.width = 8, fig.align = "center", out.width = "100%"----
data("Fleiss1993bin")
dataFleiss1993bin <- Fleiss1993bin
output <- DoOSA(Fleiss1993bin, 
                source = study,
                time = year,
                r1 = d.asp, 
                n1 = n.asp, 
                r2 = d.plac, 
                n2 = n.plac, 
                measure = "RR",
                group = c("Aspirin", "Placebo"),
                plot = FALSE,
                SAP = TRUE)

PlotOSA(output,
        lgcZone = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  output <- DoOSA(Fleiss1993bin,
#                  source = study,
#                  time = year,
#                  r1 = d.asp,
#                  n1 = n.asp,
#                  r2 = d.plac,
#                  n2 = n.plac,
#                  measure = "RR",
#                  group = c("Aspirin", "Placebo"),
#                  SAP = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  PlotPower(output)

## ----plot-power, fig.cap = "An example for illustrating plot curve of observed sequential analysis", eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE, results = "hide", fig.height = 6, fig.width = 8, fig.align = "center", out.width = "100%"----
data("Fleiss1993bin")
dataFleiss1993bin <- Fleiss1993bin
output <- DoOSA(Fleiss1993bin, 
                source = study,
                time = year,
                r1 = d.asp, 
                n1 = n.asp, 
                r2 = d.plac, 
                n2 = n.plac, 
                measure = "RR",
                group = c("Aspirin", "Placebo"),
                plot = FALSE,
                SAP = TRUE)

PlotPower(output)

