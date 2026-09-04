## ----setup, echo= FALSE,include = FALSE---------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----fitting GIMMEgVAR--------------------------------------------------------

#load library
library("GIMMEgVAR")

#read in example data
data(sampleData)

#set ouptdirectory
outputDirectory <- getwd() 

#fit GIMMEgVAR and assign results to object named 'res' 
res <- GIMMEgVAR(outputPath = outputDirectory,
                 data = sampleData,
                 gamma = 0,
                 variableNames = c("V1","V2","V3","V4","V5"),
                 idvar = "Idnum",
                 gimmeGVARThreshold = .50,
                 mimic = "current")


## ----indivdual plot syntax----------------------------------------------------

#plot PDC (temporal/lagged network model for person 1)
plot(res[["resultsGIMMEgVAR"]][["01"]],"PDC", layout="circle")

#plot PCC (contemporaneous network model for person 1)
plot(res[["resultsGIMMEgVAR"]][["01"]],"PCC", layout="circle")


