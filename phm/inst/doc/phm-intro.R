## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(phm)

## -----------------------------------------------------------------------------
tst=c("This is a test text",
      "This is a test text 2",
      "This is another test text",
      "This is another test text 2",
      "This girl will test text that man",
      "This boy will test text that man")

## -----------------------------------------------------------------------------
pd=phraseDoc(tst)

## -----------------------------------------------------------------------------
as.matrix(pd)

## -----------------------------------------------------------------------------
freqPhrases(pd,3)

## -----------------------------------------------------------------------------
getDocs(pd,c("test text","another test text"))

## -----------------------------------------------------------------------------
getPhrases(pd, 1:2)

## -----------------------------------------------------------------------------
pd=removePhrases(pd, "test text")
as.matrix(pd)

## -----------------------------------------------------------------------------
textDist(c(1,2,0),c(0,1,1))

## -----------------------------------------------------------------------------
(M1=matrix(c(0,1,0,2,0,10,0,14),4))
(M2=matrix(c(12,0,8,0,1,3,1,2),4))
textDist(M1,M2)

## -----------------------------------------------------------------------------
M=matrix(c(0,1,0,2,0,10,0,14,12,0,8,0,1,0,1,0),4)
colnames(M)=1:4;rownames(M)=c("A","B","C","D")
M
(tdm=textDistMatrix(M))
class(tdm)

## -----------------------------------------------------------------------------
M=matrix(c(rep(0,4),0,1,0,2,0,10,0,14,12,0,8,0,1,0,1,0,rep(0,4)),4)
colnames(M)=1:6;rownames(M)=c("A","B","C","D")
M

## -----------------------------------------------------------------------------
(tc=textCluster(M,3))

## -----------------------------------------------------------------------------
#This shows for each document what cluster it is in
tc$cluster

#This shows for each cluster how many documents it contains
tc$size

#This matrix shows the centroid for each cluster on the columns, with terms on
#the rows
tc$centroids

## -----------------------------------------------------------------------------
showCluster(M,tc$cluster,1)
showCluster(M,tc$cluster,2)
showCluster(M,tc$cluster,3)

## -----------------------------------------------------------------------------
(df=data.frame(id=LETTERS[1:3],text=c("First text","Second text","Third text"),
              title=c("N1","N2","N3"),author=c("Smith","Jones","Jones")))

#Create the corpus
co=tm::VCorpus(DFSource(df))

#The content of one of the documents
co[[1]]$content

#The meta data of one of the documents; all variables are present.
co[[1]]$meta

