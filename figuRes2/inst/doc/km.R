## ---- results='hide'----------------------------------------------------------
remove(list=ls())
require(figuRes2)
require(survival)
require(ggplot2)
require(plyr)
require(scales)
default.settings()

## -----------------------------------------------------------------------------
data(km.data)
working.df <- km.data
head(working.df)

## -----------------------------------------------------------------------------
km.M <- km.plot(parent.df = subset(working.df, SEX=="M"),
                censor.col = "CENSOR", 
                centime.col = "CENTIME.MON",
                category.col = "TRTGRP",
                category.palette = c("red", "blue"),
                at.risk.palette = c("red","blue"),                       
                linetype.palette = c("solid","dotted"), 
                x.label = "Time Since Randomization (months)", 
                x.limits=c(-3,48),
                x.ticks=seq(0,48,6),
                y.limits=c(0,.25), 
                y.ticks=seq(0,.25,.05))

## ---- fig.cap="Kaplan-Meier Graphic for Males"--------------------------------
km.M[[1]]

## ---- fig.cap="Corresponding At Risk Table for Males"-------------------------
km.M[[2]]

## -----------------------------------------------------------------------------
km.F <- km.plot(parent.df = subset(working.df, SEX=="F"),
                censor.col = "CENSOR", 
                centime.col = "CENTIME.MON",
                category.col = "TRTGRP",
                category.palette = c("red", "blue"),
                at.risk.palette = c("red","blue"),
                linetype.palette = c("solid","dotted"), 
                x.label = "Time Since Randomization (months)",
                x.limits=c(-3,48),
                x.ticks=seq(0,48,6),
                y.limits=c(0,.25), 
                y.ticks=seq(0,.25,.05))

## ---- fig.cap="Kaplan-Meier Graph for Females"--------------------------------
km.F[[1]]

## ---- fig.cap="Number at Risk Table for Females"------------------------------
km.F[[2]]

## -----------------------------------------------------------------------------
comeback.M <- sync.ylab.widths(list(
  km.M[[1]]+
         ggtitle("Kaplan Meier-Plot of Time to\nFirst MACE: Males") +
    guides(color=F, linetype=F), 
  km.M[[2]]+labs(x=NULL, y="At Risk")
  ))
    
comeback.F <- sync.ylab.widths(list(
  km.F[[1]]+
    ggtitle("Kaplan Meier-Plot of Time to\nFirst MACE: Females") +
    guides(color=F,linetype=F) +
    theme(axis.ticks.y=element_line(color="white")) + 
    labs(y=NULL) +
    scale_y_continuous(labels=NULL, limits=c(0,.25), breaks=seq(0,.25,.05)),
  km.F[[2]]+
    labs(x=NULL, y=NULL) +
    scale_y_discrete(labels=NULL))
  )

## ---- fig.cap="A Figure that arranges four graphics"--------------------------
build.page(interior.h = c(.8, .2),
           interior.w = c(.6,.4),
           ncol=2, nrow=2,
           interior = list(comeback.M[[1]], comeback.F[[1]],
                           comeback.M[[2]], comeback.F[[2]]))

## ---- fig.cap="A Figure that better arranges four graphics"-------------------
build.page(interior.h = c(.8, .2),
           interior.w = c(.5,.5),
           ncol=2, nrow=2,
           interior = list(comeback.M[[1]], comeback.F[[1]],
                           comeback.M[[2]], comeback.F[[2]]))

