## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(oncoPredict)

#Apply idwas() function.

#Determine the parameters of the idwas() function...
#Set the drug_prediction parameter.
#Make sure rownames() are samples, and colnames() are drugs. Also make sure this data is a data frame.
drug_prediction<-as.data.frame(read.table('DrugPredictions.txt', header=TRUE, row.names=1))
#In this example, I had to replace the '.' in the names of these TCGA samples with '-' so that they are of the same form as samples in the mutation  data (you may not have to do this).
colnames(drug_prediction)<-gsub(".", "-", colnames(drug_prediction), fixed=T)
#Make sure the sample identifiers in the 'drug prediction' data are of similar form as the sample identifiers in the 'data' parameter.
cols=colnames(drug_prediction)
colnames(drug_prediction)<-substring(cols, 3, nchar(cols))
drug_prediction<-as.data.frame(t(drug_prediction))

wd<-tempdir()
savedir<-setwd(wd)


