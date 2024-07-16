## ----message=FALSE, echo=FALSE, warning=FALSE---------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.height = 7,
  fig.width = 9,
  dpi=100,
  results = "asis"
)

## -----------------------------------------------------------------------------
library(visualpred)
dataf<-na.omit(nba)
listconti<-c("GP", "MIN", "PTS", "FGM", "FGA", "FG", "P3Made", "P3A", 
  "P3", "FTM", "FTA", "FT", "OREB", "DREB", "REB", "AST", "STL", 
  "BLK", "TOV")
listclass<-c("")
vardep<-"TARGET_5Yrs"

result<-famdcontourlabel(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,Idt="name",
title="Outliers in FAMD plots",title2="",selec=1,inf=0.001,sup=0.999,cutprob=0.9)


## ----echo=T-------------------------------------------------------------------
result[[3]]

## ----echo=T-------------------------------------------------------------------
result[[10]]
result[[11]]
result[[12]]

