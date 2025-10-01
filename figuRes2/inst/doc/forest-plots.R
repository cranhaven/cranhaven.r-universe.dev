## ---- results='hide'----------------------------------------------------------
remove(list=ls())
require(figuRes2)
require(survival)
require(ggplot2)
require(scales)
require(stringr)
require(plyr)
require(grid)
require(gridExtra)
require(reshape2)
require(gtable)
default.settings()

## -----------------------------------------------------------------------------
data(forest.data)
working.df <- forest.data
head(working.df)
dim(working.df)

## -----------------------------------------------------------------------------
working.df.1 <- working.df[1:16,]
working.df.1 

## -----------------------------------------------------------------------------
working.df.1$rank <- rev(1:16)

## -----------------------------------------------------------------------------
working.df.1$category <- factor(0)

## -----------------------------------------------------------------------------
working.df.1$label.rank <- rev(1:16)

## -----------------------------------------------------------------------------
working.df.1$labels <- paste(working.df.1$subgroup, working.df.1$level)
working.df.1

## ---- fig.cap= "First Pass at a Forest Plot"----------------------------------
p1 <- forest.plot(parent.df = working.df.1, 
            y.rank.col = "rank",  # line segment's y-axis rank
            Point.Est = "hr",     # line segment's dot
            lower.lim = "low",     # line segment's lower endpoint
            upper.lim = "high",    # line segment's upper endpoint
            y.label.rank.col = "label.rank",  # label's y-axis rank
            y.label.col = "labels", # label's text value
            x.label = "Estimate", 
            y.label = NULL,
            log.trans = TRUE, 
            x.limits = c(0.21, 5), 
            x.ticks = 2^(-2:2), 
            category.color = "category", # This colors the points and line segments
            background.palette = c("red", "blue"), 
            category.palette = c("red", "blue"), 
            shape.palette = c(16, 16), 
            flip.palette = FALSE) 
print(p1)

## ---- fig.cap= "Second Pass: Label fix"---------------------------------------
p2 <- p1 + scale_y_continuous(
  breaks = p1$data$LABEL.RANKS,
  labels = c(
    "Prior Myocardial Infaction: No",
    "Yes",
    "Prior coronary revasc.: No",
    "Yes",
    "Multivessel CHD: No",
    "Yes",
    "CHD Event Relative to Randomization: Recent",
    "Remote",
    expression(paste("Age ", phantom() >= 60,": No")),
    "Yes",
    "Diabetes req. pharm.: No",
    "Yes",
    "HDL-C < 40 mg/dL: No",
    "Yes",
    "Current or previous smoker: Yes",
    "No"))
print(p2)

## ---- fig.cap= "First Pass at a Table plot"-----------------------------------
t1 <- table.plot(
    parent.df = working.df.1,
    y.rank.col= "rank",
    category.color= "category",
    text.col1 = "hr",
    text.col2 = "low",
    text.col3 = "high",
    text.col4 = NULL,
    text.size = 3,
    xtick.labs = c("Estimate", "LCI", "UCI"),
    x.label= "Text",
    y.label= "Item",
    y.label.rank.col = "label.rank",  #  this identifies the y-axis values for labels
    y.label.col = "subgroup", 
    category.palette = c("red", "blue"))
print(t1)

## ---- fig.cap= "Second pass at table plot"------------------------------------
t2 <- table.plot(
    parent.df = working.df.1,
    y.rank.col= "rank",
    category.color= "category",
    text.col1 = "hr",
    text.col2 = "low",
    text.col3 = "high",
    text.col4 = NULL,
    text.size=3,
    xtick.labs = c("Estimate", "LCI", "UCI"),
    x.label= "",
    y.label=NULL,
    y.label.rank.col = "label.rank",  #  this identifies the y-axis values for labels
    y.label.col = NULL, 
    category.palette = c("grey40", "blue"))
print(t2)

## ---- fig.cap= "An assembled forest plot figure"------------------------------
build.page(interior.h = c(1),
           interior.w = c(1/2, 1/2),
           ncol=2, nrow=1, interior=list(p2+ggtitle(""), t2+ggtitle("")) )
annotate.page(override = "", title=list("Title Line 1", "","","",""))

## ---- fig.cap= "Experimenting with the width: 60%/40%"------------------------
build.page(interior.h = c(1),
           interior.w = c(.6, .4),
           ncol=2, nrow=1, interior=list(p2+ggtitle(""), t2+ggtitle("")) )
annotate.page(override = "", title=list("Title Line 1", "","","",""))

## ---- fig.cap= "An assembled forest plot figure"------------------------------
build.page(interior.h = c(1),
           interior.w = c(.8, .2),
           ncol=2, nrow=1, interior=list(p2+ggtitle(""), t2+ggtitle("")) )
annotate.page(override = "", title=list("Title Line 1", "","","",""))

## ---- fig.cap= "Manipulating the vertical placement of line segments"---------
working.df.1$rank2 <- working.df.1$rank + duplicated(working.df.1$subgroup)*.5
p3 <- forest.plot(parent.df = working.df.1, 
            y.rank.col = "rank2",  # line segment's y-axis rank
            Point.Est = "hr",     # line segment's dot
            lower.lim = "low",     # line segment's lower endpoint
            upper.lim = "high",    # line segment's upper endpoint
            y.label.rank.col = "rank2",  # label's y-axis rank
            y.label.col = "labels", # label's text value
            x.label = "Estimate", 
            y.label = NULL,
            log.trans = TRUE, 
            x.limits = c(0.21, 5), 
            x.ticks = 2^(-2:2), 
            category.color = "category", # This colors the points and line segments
            background.palette = c("red", "blue"), 
            category.palette = c("red", "blue"), 
            shape.palette = c(16, 16), 
            flip.palette = FALSE) 

# This step is same as before, with swap in the breaks argument
p4 <- p3 + scale_y_continuous(
  breaks = p3$data$RANK,
  labels = c(
    "Prior MI: No",
    "Yes",
    "Prior Coronary Revasc.: No",
    "Yes",
    "Multivessel CHD: No",
    "Yes",
    "CHD Event Relative to Randomization: Recent",
    "Remote",
    expression(paste("Age ", phantom() >= 60,": No")),
    "Yes",
    "Diabetes req. pharm.: No",
    "Yes",
    "HDL-C < 40 mg/dL: No",
    "Yes",
    "Current or previous smoker: No",
    "Yes"))
# This is same as t2, save swap of rank for rank2
t3 <- table.plot(
    parent.df = working.df.1,
    y.rank.col = "rank2",
    category.color = "category",
    text.col1 = "hr",
    text.col2 = "low",
    text.col3 = "high",
    text.col4 = NULL,
    text.size=3,
    xtick.labs = c("Estimate", "LCI", "UCI"),
    x.label= "",
    y.label =NULL,
    y.label.rank.col = "rank2",  #  this identifies the y-axis values for labels
    y.label.col = NULL, 
    category.palette = c("grey40", "blue"))

build.page(interior.h = c(1),
           interior.w = c(.8, .2),
           ncol = 2, nrow = 1, interior=list(p4 + ggtitle(""), t3+ggtitle("")) )
annotate.page(override = "", title=list("Title Line 1", "","","",""))

