## ----message=FALSE, echo=FALSE, warning=FALSE---------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.height = 6,
  fig.width = 7.71,
  dpi=100,
  results = "asis"
)

## -----------------------------------------------------------------------------
library(visualpred)
dataf<-na.omit(Hmda)
listconti<-c("dir", "lvr", "ccs","uria")
listclass<-c("pbcr", "dmi", "self")
vardep<-c("deny")
dataf<-dataf[,c(listconti,listclass,vardep)]
set.seed(123)
train_ind <- sample(seq_len(nrow(Hmda)), size = 1700)
train <- dataf[train_ind, ]
test <- dataf[-train_ind, ]
formu<-paste("factor(",vardep,")~.")
model <- glm(formula(formu),family=binomial(link='logit'),data=train)
proba<- predict(model,test,type="response")
result<-famdcontour(dataf=test,listconti=listconti,listclass=listclass,vardep=vardep,
proba=proba,title="Test Contour under GLM",title2="  ",selec=0,modelo="glm",classvar=0)

result[[2]]

## -----------------------------------------------------------------------------
library(randomForest)
model <- randomForest(formula(formu),data=train,mtry=4,nodesize=10,ntree=300)
proba <- predict(model, test,type="prob")
proba<-as.vector(proba[,2])

result<-famdcontour(dataf=test,listconti=listconti,listclass=listclass,vardep=vardep,
proba=proba,title="Test Contour under Random Forest",title2="  ",Dime1="Dim.1",Dime2="Dim.2",
selec=0,modelo="glm",classvar=0)

result[[2]]

## -----------------------------------------------------------------------------
library(ggplot2)
library(visualpred)
dataf<-na.omit(Hmda)
listconti<-c("dir", "lvr", "ccs","uria")
listclass<-c("pbcr", "dmi", "self")
vardep<-c("deny")

result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
title="",title2="  ",selec=0,modelo="glm",classvar=0,depcol=c("gold2","deeppink3"))

result[[2]]+
ggtitle("Hmda data",subtitle="2380 obs")+theme(
    plot.title = element_text(hjust=0.5,color="darkorchid"),
    plot.subtitle= element_text(hjust=0.5,color="violet")
  )

## -----------------------------------------------------------------------------
result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
title="Dim.1 and Dim.2",title2="  ",Dime1="Dim.1",Dime2="Dim.2",
selec=0,modelo="glm",classvar=0)

result[[4]]
result[[5]]

result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
title="Dim.3 and Dim.4",title2="  ",Dime1="Dim.3",Dime2="Dim.4",
selec=0,modelo="glm",classvar=0)

result[[4]]
result[[5]]

