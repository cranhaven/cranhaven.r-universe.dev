## ----message=FALSE, echo=FALSE, warning=FALSE---------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.height = 12,
  fig.width = 9,
  dpi=100,
  results = "asis"
)

## -----------------------------------------------------------------------------
require(egg)
library(visualpred)
dataf<-spiral
listconti<-c("x1","x2")
  listclass<-c("")
  vardep<-"clase"
  result<-famdcontour(dataf=spiral,listconti=listconti,listclass=listclass,vardep=vardep,
  title="GLM",title2="  ",selec=0,modelo="glm",classvar=0)
    g1<-result[[2]]+theme(legend.position = "none")
    result<-famdcontour(dataf=spiral,listconti=listconti,listclass=listclass,vardep=vardep,
title="NNET",title2="  ",selec=0,modelo="nnet",classvar=0)
    g2<-result[[2]]+theme(legend.position = "none")
    result<-famdcontour(dataf=spiral,listconti=listconti,listclass=listclass,vardep=vardep,
title="RF",title2="  ",selec=0,modelo="rf",classvar=0)
    g3<-result[[2]]+theme(legend.position = "none")
    result<-famdcontour(dataf=spiral,listconti=listconti,listclass=listclass,vardep=vardep,
title="GBM",title2="  ",selec=0,modelo="gbm",classvar=0)
    g4<-result[[2]]+theme(legend.position = "none")
  result<-famdcontour(dataf=spiral,listconti=listconti,listclass=listclass,vardep=vardep,title="SVM",
title2="  ",selec=0,modelo="svm",classvar=0)
    g5<-result[[2]]+theme(legend.position = "none")

  ggarrange(g1,g2,g3,g4,g5,ncol =2,nrow=3)
  

## ----echo=F-------------------------------------------------------------------
require(egg)
library(visualpred)
dataf<-spiral
listconti<-c("x1","x2")
  listclass<-c("")
  vardep<-"clase"
  result<-famdcontour(dataf=spiral,listconti=listconti,listclass=listclass,vardep=vardep,
title="NNET 3,maxit=200,decay=0.01",title2="  ",Dime1="Dim.1",Dime2="Dim.2",selec=0,modelo="nnet",
classvar=0)
    g1<-result[[2]]+theme(legend.position = "none")
    result<-famdcontour(dataf=spiral,listconti=listconti,listclass=listclass,vardep=vardep,
title="NNET 10,maxit=1000,decay=0.001",title2="  ",
Dime1="Dim.1",Dime2="Dim.2",selec=0,modelo="nnet",nodos=10,maxit=1000,decay=0.001,
classvar=0)
  g2<-result[[2]]+theme(legend.position = "none")
    result<-famdcontour(dataf=spiral,listconti=listconti,listclass=listclass,vardep=vardep,
title="NNET 15,maxit=2500,decay=0.0001",title2="  ",Dime1="Dim.1",Dime2="Dim.2",selec=0,modelo="nnet",nodos=15,
maxit=2500,decay=0.0001,classvar=0)
    g3<-result[[2]]+theme(legend.position = "none")
    result<-famdcontour(dataf=spiral,listconti=listconti,listclass=listclass,vardep=vardep,
  title="NNET 20,maxit=2500,decay=0.0001",title2="  ",Dime1="Dim.1",Dime2="Dim.2",selec=0,modelo="nnet",nodos=25,
  maxit=2500,decay=0.0001,classvar=0)
    g4<-result[[2]]+theme(legend.position = "none")
  
  ggarrange(g1,g2,g3,g4,ncol =2,nrow=2)
  

## ----echo=F-------------------------------------------------------------------
require(egg)
library(visualpred)
dataf<-na.omit(nba)
  listconti<-c("GP", "MIN", "PTS", "FGM", "FGA", "FG", "P3Made", "P3A", 
  "P3", "FTM", "FTA", "FT", "OREB", "DREB", "REB", "AST", "STL", 
  "BLK", "TOV")
  listclass<-c("")
  vardep<-"TARGET_5Yrs"
  
  result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
  title="GLM",title2="  ",Dime1="Dim.1",Dime2="Dim.2",selec=1,modelo="glm",classvar=0)
    g1<-result[[2]]+theme(legend.position = "none")
    result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
  title="NNET",title2="  ",Dime1="Dim.1",Dime2="Dim.2",selec=1,modelo="nnet",classvar=0)
    g2<-result[[2]]+theme(legend.position = "none")
    result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
  title="RF",title2="  ",Dime1="Dim.1",Dime2="Dim.2",selec=1,modelo="rf",classvar=0)
    g3<-result[[2]]+theme(legend.position = "none")
    result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
  title="GBM",title2="  ",Dime1="Dim.1",Dime2="Dim.2",selec=1,modelo="gbm",classvar=0)
    g4<-result[[2]]+theme(legend.position = "none")
  result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,title="SVM",
  title2="  ",Dime1="Dim.1",Dime2="Dim.2",selec=1,modelo="svm",classvar=0)
  g5<-result[[2]]+theme(legend.position = "none")
  result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
title="SVM C=0.00001,gamma=0.00001",
title2="  ",Dime1="Dim.1",Dime2="Dim.2",selec=1,modelo="svm",classvar=0,C=0.00001,gamma=0.00001)
    g6<-result[[2]]+theme(legend.position = "none")
  
  ggarrange(g1,g2,g3,g4,g5,g6,ncol =2,nrow=3)

