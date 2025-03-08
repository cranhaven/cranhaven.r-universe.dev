## ----options, echo = FALSE, message = FALSE-----------------------------------------------------------------------------------------------------------------------------

library(knitr)
opts_chunk$set(
    echo = TRUE, 
    results = 'asis', 
    warning = FALSE, 
    error = FALSE, message = FALSE, cache = FALSE,
    fig.width = 8, fig.height = 7,
    fig.path = "./figures_vignette/",
    fig.align = 'center')
options(width = 170)#, stringsAsFactors = FALSE
options(warn = 1)#instead of warn = 0 by default -> to have place where warnings occur in the call to Sweave function

heightLineIn  <- 0.2


## ----loadPackages, message = FALSE--------------------------------------------------------------------------------------------------------------------------------------

library(clinDataReview)


## ----loadData-----------------------------------------------------------------------------------------------------------------------------------------------------------

library(clinUtils)

data(dataADaMCDISCP01)
labelVars <- attr(dataADaMCDISCP01, "labelVars")

dataLB <- dataADaMCDISCP01$ADLBC
dataDM <- dataADaMCDISCP01$ADSL
dataAE <- dataADaMCDISCP01$ADAE


## ----annotateData, message = TRUE, warning = TRUE-----------------------------------------------------------------------------------------------------------------------

dataLBAnnot <- annotateData(
    data = dataLB, 
    annotations = list(data = dataDM, vars = c("ETHNIC", "ARM")), 
    verbose = TRUE
)
knitr::kable(
    head(dataLBAnnot), 
    caption = paste("Laboratory parameters annotated with",
        "demographics information with the `annotatedData` function"
    )
)

## ----filterData, message = TRUE, warning = TRUE-------------------------------------------------------------------------------------------------------------------------

dataLBAnnotTreatment <- filterData(
    data = dataLBAnnot, 
    filters = list(var = "ARM", value = "Placebo", rev = TRUE), 
    verbose = TRUE
)
knitr::kable(
    unique(dataLBAnnotTreatment[, c("USUBJID", "ARM")]), 
    caption = paste("Subset of laboratory parameters filtered",
        "with placebo patients"
    )
)

## ----transformData, message = TRUE, warning = TRUE----------------------------------------------------------------------------------------------------------------------

eDishData <- transformData(
    data = subset(dataLB, PARAMCD %in% c("ALT", "BILI")),
    transformations = list(
        type = "pivot_wider",
        varsID = c("USUBJID", "VISIT"), 
        varsValue = c("LBSTRESN", "LBNRIND"),
        varPivot = "PARAMCD"
    ),
    verbose = TRUE,
    labelVars = labelVars
)
knitr::kable(head(eDishData))


## ----processData--------------------------------------------------------------------------------------------------------------------------------------------------------

dataLBAnnotTreatment2 <- processData(
    data = dataLB,
    processing = list(
        list(annotate = list(data = dataDM, vars = c("ETHNIC", "ARM"))),
        list(filter = list(var = "ARM", value = "Placebo", rev = TRUE))
    ),
    verbose = TRUE
)

identical(dataLBAnnotTreatment, dataLBAnnotTreatment2)


## ----sessionInfo, echo = FALSE------------------------------------------------------------------------------------------------------------------------------------------

print(sessionInfo())


