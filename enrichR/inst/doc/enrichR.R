## ---- echo = FALSE, eval = TRUE-----------------------------------------------
websiteLive <- TRUE

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  library(devtools)
#  install_github("wjawaid/enrichR")

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  install.packages("enrichR")

## ---- echo = TRUE, eval = TRUE------------------------------------------------
library(enrichR)
websiteLive <- getOption("enrichR.live")
if (websiteLive) {
    listEnrichrSites()
    setEnrichrSite("Enrichr") # Human genes   
}

## ---- echo = TRUE, eval = TRUE------------------------------------------------
if (websiteLive) dbs <- listEnrichrDbs()

## ---- echo = TRUE, eval = TRUE------------------------------------------------
## if (is.null(dbs)) websiteLive <- FALSE
if (websiteLive) head(dbs)

## ---- echo = FALSE, results='asis'--------------------------------------------
library(knitr)
if (websiteLive) kable(head(dbs[c(1:6),-4]))

## ---- echo = TRUE, eval = TRUE------------------------------------------------
dbs <- c("GO_Molecular_Function_2015", "GO_Cellular_Component_2015", "GO_Biological_Process_2015")
if (websiteLive) {
    enriched <- enrichr(c("Runx1", "Gfi1", "Gfi1b", "Spi1", "Gata1", "Kdr"), dbs)
}

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  if (websiteLive) enriched[["GO_Biological_Process_2015"]]

## ---- echo = FALSE, results = 'asis'------------------------------------------
success <- ifelse(exists("enriched"), TRUE, FALSE) & websiteLive
success <- success & (length(enriched) >= 3)
success <- success & all(dim(enriched[[3]]) > 2)                  
if (success) {
    x <- head(enriched[["GO_Biological_Process_2015"]])
    x[,1] <- gsub("GO:", "GO_", x[,1])
    kable(x)
}

## ---- echo = TRUE, eval = TRUE, fig.width = 8, fig.height = 6, fig.align = "center", dpi = 100----
if (websiteLive) {
    plotEnrich(enriched[[3]], showTerms = 20, numChar = 40, y = "Count", orderBy = "P.value")
}

