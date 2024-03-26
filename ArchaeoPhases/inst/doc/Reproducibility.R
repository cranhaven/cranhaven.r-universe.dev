## ---- echo = FALSE, message = FALSE-----------------------------------------------------------------------------------
knitr::opts_chunk$set(comment = "")
options(width = 120, max.print = 20)
library(ArchaeoPhases)


## ----read_oxcal-------------------------------------------------------------------------------------------------------
## load ArchaeoPhases
library(ArchaeoPhases)
## read remote file
data(oxc)
## returns TRUE, if ox.csv has not changed on the server
original_file(oxc)

## ----marginal_plot----------------------------------------------------------------------------------------------------
## create mariginal plot object
oxc.mar <- marginal_plot(oxc)
## use plot method to reproduce marginal plot
oxc.mar.plot <- plot(oxc.mar)
## check for identity returns FALSE because calls differ
identical(oxc.mar, oxc.mar.plot)
## check for data identity returns TRUE
identical(oxc.mar$x, oxc.mar.plot$x)
## reproduce the marginal plot object
oxc.mar.rep <- reproduce(oxc.mar)
## check for object identity returns TRUE
identical(oxc.mar.rep, oxc.mar)

## ----mult_marginal----------------------------------------------------------------------------------------------------
oxc.stats <- multi_marginal_statistics(oxc)
oxc.stats$statistics

## ---------------------------------------------------------------------------------------------------------------------
attr(oxc.mar, "call")

