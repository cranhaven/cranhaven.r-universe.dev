## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
#require(utf8)

## ----example1-----------------------------------------------------------------
require(AugmenterR)
NovelSample=Generate(iris,regression=TRUE)
print(NovelSample)

## ----example2-----------------------------------------------------------------
require(AugmenterR)
Setosa=GenerateMultipleCandidates(iris,'setosa',5,0.9,40)
Virginica=GenerateMultipleCandidates(iris,'virginica',5,0.9,40)
Versicolor=GenerateMultipleCandidates(iris,'versicolor',5,0.9,40)

head(Setosa)

## ----JoinData-----------------------------------------------------------------
df=data.frame(iris,source='Original')
Setosa=data.frame(Setosa,source='Sinthetic')
Virginica=data.frame(Virginica,source='Sinthetic')
Versicolor=data.frame(Versicolor,source='Sinthetic')
df=rbind(df,Setosa,Virginica,Versicolor)
require(ggplot2)
ggplot2::ggplot(df) + aes(x=Sepal.Length,col=source) + facet_wrap(~Species) + geom_histogram(aes(y=..density..) )

## ----PCA----------------------------------------------------------------------
pc=prcomp(df[,1:4],center=TRUE,scale=TRUE)$x

pc=data.frame(pc,df[,5:6])

ggplot2::ggplot(pc) + aes(x=pc[,1],y=pc[,2],col=Species) +facet_wrap(~source) + labs(x='First Component',y='Second Component') + geom_point()

