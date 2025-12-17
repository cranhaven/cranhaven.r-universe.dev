## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ----vignetteSetup, echo=FALSE, message=FALSE, warning = FALSE----------------
## For links
library("BiocStyle")

## Track time spent on making the vignette
startTime <- Sys.time()

## Bib setup
library("RefManageR")

## Write bibliography information
bib <- c(
    R = citation(),
    BiocStyle = citation("BiocStyle")[1],
    knitr =bibentry(
     bibtype = "InCollection",
     booktitle = "Implementing Reproducible Computational Research",
     title = "knitr: A Comprehensive Tool for Reproducible Research in R",
     author = as.person("Yihui Xie [aut]"),
     editor = as.person("Victoria Stodden, Friedrich Leisch, Roger D. Peng"),
     year = "2014",
     publisher = "Chapman and Hall/CRC",
     isbn = "978-1466561595",
     url = "https://www.routledge.com/Implementing-Reproducible-Research/Stodden-Leisch-Peng/p/book/9781466561595"
   ),
    Matrix = citation("Matrix")[1],
    RefManageR = citation("RefManageR")[1],
    rmarkdown = citation("rmarkdown")[1],
    S4Vectors = citation("S4Vectors")[1],
    sessioninfo = citation("sessioninfo")[1]
)

## ----'install', eval = FALSE--------------------------------------------------
#   install.packages("SRTsim")

## ----setup, message = FALSE, warning = FALSE----------------------------------
library("SRTsim")

## ----'reference-based tissue-wise simulation'---------------------------------
## explore example SRT data 
str(exampleLIBD)

example_count   <- exampleLIBD$count
example_loc     <- exampleLIBD$info[,c("imagecol","imagerow","layer")]
colnames(example_loc) <- c("x","y","label")

## create a SRT object
simSRT  <- createSRT(count_in=example_count,loc_in =example_loc)


## Set a seed for reproducible simulation
set.seed(1)

## Estimate model parameters for data generation
simSRT1 <- srtsim_fit(simSRT,sim_schem="tissue")

## Generate synthetic data with estimated parameters
simSRT1 <- srtsim_count(simSRT1)

## Explore the synthetic data
simCounts(simSRT1)[1:5,1:5]
simcolData(simSRT1)

## ----'reference-based domain-specific simulation'-----------------------------

## Set a seed for reproducible simulation
set.seed(1)

## Estimate model parameters for data generation
simSRT2 <- srtsim_fit(simSRT,sim_scheme='domain')

## Generate synthetic data with estimated parameters
simSRT2 <- srtsim_count(simSRT2)

## Explore the synthetic data
simCounts(simSRT2)[1:5,1:5]

## ----'tissue simulation metrics comparison'-----------------------------------

## Compute metrics 
simSRT1   <- compareSRT(simSRT1)

## Visualize Metrics
visualize_metrics(simSRT1)

## ----'pattern comparison'-----------------------------------------------------
visualize_gene(simsrt=simSRT1,plotgn = "ENSG00000183036",rev_y=TRUE)
visualize_gene(simsrt=simSRT2,plotgn = "ENSG00000168314",rev_y=TRUE)

## ----createVignette, eval=FALSE-----------------------------------------------
#  ## Create the vignette
#  library("rmarkdown")
#  system.time(render("SRTsim.Rmd"))
#  
#  ## Extract the R code
#  library("knitr")
#  knit("SRTsim.Rmd", tangle = TRUE)

## ----reproduce1, echo=FALSE---------------------------------------------------
## Date the vignette was generated
Sys.time()

## ----reproduce2, echo=FALSE---------------------------------------------------
## Processing time in seconds
totalTime <- diff(c(startTime, Sys.time()))
round(totalTime, digits = 3)

## ----reproduce3, echo=FALSE---------------------------------------------------
## Session info
library("sessioninfo")
original <- options("width")
options(width = 120)
session_info()
options(original)

## ----vignetteBiblio, results = 'asis', echo = FALSE, warning = FALSE, message = FALSE----
## Print bibliography
PrintBibliography(bib, .opts = list(hyperlink = "to.doc", style = "html"))

