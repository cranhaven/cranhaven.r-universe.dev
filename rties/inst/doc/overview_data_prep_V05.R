## ----setup, include = FALSE---------------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=F------------------------------------------------------------------
#  install.packages("rties")

## -----------------------------------------------------------------------------
library(rties)

## -----------------------------------------------------------------------------
demoData <- rties_ExampleData_Demo
head(demoData, 12)

## ---- eval=FALSE--------------------------------------------------------------
#  demoData$person <- ifelse(demoData$female == 1, demoData$couple, demoData$couple + 500 )

## ---- eval=FALSE--------------------------------------------------------------
#  demoData <- makeDist(basedata=demoData, dyadId="couple", personId="person", time_name="time", dist_name="empathy")

## ---- eval=FALSE--------------------------------------------------------------
#  demo <- read.csv("/Users/emily/Documents/Projects/R-TIES/Data/demo.csv")

## -----------------------------------------------------------------------------
data1 <- rties_ExampleDataFull
str(data1)

## ---- fig.width=5, fig.height=5-----------------------------------------------
temp1 <- subset(data1, select=c(reltime:conflict))
histAll(temp1)

## ---- dpi=180-----------------------------------------------------------------
temp2 <- subset(data1, couple %in% c(2,3,5,8,10,12,13,17,19))
plotRaw(basedata=temp2, dyad="couple", obs_name="dial", dist_name="female", time_name="time", dist0name="Men", dist1name= "Women", plot_obs_name="Dial")

## ---- eval=FALSE--------------------------------------------------------------
#  # save them as a list
#  plots <- plotRaw(basedata=temp2, dyad="couple", obs_name="dial", dist_name="female", time_name="time", dist0name="Men", dist1name= "Women", plot_obs_name="Dial", printPlots=F)
#  
#  # print them as a pdf
#  pdf("rawPlots.pdf")
#  plotRaw(basedata=temp2, dyad="couple", obs_name="dial", dist_name="female", time_name="time", dist0name="Men", dist1name= "Women", plot_obs_name="Dial")
#  dev.off()

## -----------------------------------------------------------------------------
dyads <- c(19)
data1 <- removeDyads(basedata=data1, dyads=dyads, dyadId="couple")

## -----------------------------------------------------------------------------
data2 <- dataPrep(basedata=data1, dyadId="couple", personId="person", obs_name="dial", dist_name="female", time_name="time", time_lag=5) 
str(data2)

