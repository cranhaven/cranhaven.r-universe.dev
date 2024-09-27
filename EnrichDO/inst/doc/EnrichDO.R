## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup,results='hide'-----------------------------------------------------
library(EnrichDO)

## ----label=init,eval=TRUE,echo=TRUE,collapse=FALSE,cache=TRUE-----------------
demo.data=c(1636,351,102,2932,3077,348,4137,54209,5663,5328,23621,3416,3553)
demo_result<-doEnrich(interestGenes=demo.data)
show(demo_result)

## ----eval=TRUE----------------------------------------------------------------
head(doterms)
showDoTerms(doterms)

## -----------------------------------------------------------------------------
weighted_demo<-doEnrich(interestGenes=demo.data,
                           test="fisherTest",
                           method="holm",
                           m=1,
                           minGsize=10,
                           maxGsize=2000,
                           delta=0.05,
                           penalize=TRUE)

## -----------------------------------------------------------------------------
penalF_demo<-doEnrich(interestGenes=demo.data, penalize=FALSE)

## -----------------------------------------------------------------------------
Tradition_demo<-doEnrich(demo.data, traditional=TRUE)

## ----eval=TRUE----------------------------------------------------------------
writeDoTerms(doterms,file=file.path(tempdir(), "doterms.txt"))

## ----eval=TRUE----------------------------------------------------------------
writeResult(EnrichResult = demo_result,file=file.path(tempdir(), "result.txt"), Q=1, P=1)

## ----fig.cap="bar plot",fig.align='center',fig.width=7,fig.height=5-----------
drawBarGraph(EnrichResult=demo_result, n=10, delta=0.05)

## ----fig.cap="point plot",fig.align='center',fig.width=7,fig.height=5---------
drawPointGraph(EnrichResult=demo_result, n=10, delta=0.05)

## ----fig.cap="tree plot",fig.align='center',fig.width=7,fig.height=5----------

drawGraphViz(EnrichResult=demo_result, n=10, numview=FALSE, pview=FALSE, labelfontsize=17)


## ----fig.cap="heatmap",fig.align='center',fig.width=7,fig.height=5------------
drawHeatmap(interestGenes=demo.data,
            EnrichResult=demo_result,
            gene_n=10,
            fontsize_row=8,
            readable=TRUE)

## -----------------------------------------------------------------------------
#Firstly, read the wrireResult output file,using the following two lines
data <- read.delim(file.path(system.file("examples", package="EnrichDO"), "result.txt"))
enrich <- convDraw(resultDO=data)

#then, Use the drawing function you need
drawGraphViz(enrich=enrich)    #Tree diagram
drawPointGraph(enrich=enrich, delta = 0.05)  #Bubble diagram
drawBarGraph(enrich=enrich, delta = 0.05)    #Bar plot

## ----session-info,cache = F,echo=T,message=T,warning=FALSE--------------------
sessionInfo()

