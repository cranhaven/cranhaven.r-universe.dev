## ---- echo = FALSE, message = FALSE-----------------------------------------------------------------------------------
hasData <- requireNamespace("ArchaeoPhases.dataset", quietly = TRUE)
if (!hasData) {
    knitr::opts_chunk$set(eval = FALSE)
    msg <- paste("Note: Examples in this vignette require that the",
    "`ArchaeoPhases.dataset` package be installed. The system",
    "currently running this vignette does not have that package",
    "installed, so code examples will not be evaluated.")
    msg <- paste(strwrap(msg), collapse="\n")
    message(msg)
}
knitr::opts_chunk$set(comment = "")
options(width = 120, max.print = 5)
library(ArchaeoPhases)
library(ArchaeoPhases.dataset)


## ----fig.align='center',fig.width=6,fig.height=5----------------------------------------------------------------------
data("KADatesChronoModel")
tempo_plot(KADatesChronoModel, c(2:10), level = 0.95)

## ----fig.align='center',fig.width=6,fig.height=5----------------------------------------------------------------------
tempo_activity_plot(KADatesChronoModel, c(2:10))

## ----fig.align='center',fig.width=6,fig.height=5----------------------------------------------------------------------
data("KADatesChronoModel")
OccurrencePlot(KADatesChronoModel, c(2:17), level = 0.95, newWindow= FALSE)

