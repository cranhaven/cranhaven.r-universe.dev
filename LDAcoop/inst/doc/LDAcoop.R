## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----define caption, echo = FALSE---------------------------------------------
mycaption <- "Figure: Fitting LDA data; 
 assuming independence (blue), taking cooperativity into account (grey)"

## ----setup,echo=FALSE,fig.width=5, fig.height=3, fig.cap=mycaption------------
#install.packages("LDAcoop")
library(LDAcoop)
data(LDAdata)
BT20 <- subset.data.frame(x = LDAdata,
                          subset = (name == "BT.20") & (Group == 8))
BT20 <- BT20[,c("S-value","# Tested","# Clonal growth","Group","replicate")]
BT20 <- BT20[BT20$`S-value`<1000,]
out <- LDA_prepare_plot(LDA_tab = BT20)
  par(
    mar = c(2.5, 3.5, 0.5, 0.5),
    mgp = c(1.5, 0.5, 0)
  )
out[[1]][[1]]$color <- "#545454"
xl <- 750
LDA_plot_activity(LDA_obj = out[[1]],uncertainty.band = T,xlim = c(0,xl))
#statmod::elda(tested = BT20$`# Tested`,
#              dose = BT20$`S-value`,
#              response = BT20$`# Clonal growth`)
#          Lower Estimate    Upper
#Group 1 836.5526 645.4182 497.9539
lines(x = c(0,xl),y = c(0,-xl/645.4182),lwd=2,col="blue")
polygon(x = c(0,xl,xl,1),y = c(0,-xl/836.5526,-xl/497.9539,0),border = F,
        col = rgb(red = 0,green = 0,blue = 1,alpha = 0.5,maxColorValue = 1))

## ----showData-----------------------------------------------------------------
#install.packages("LDAcoop")
library(LDAcoop)
data(LDAdata)
head(LDAdata)

## ----data.table---------------------------------------------------------------
BT20 <- subset.data.frame(x = LDAdata,
                          subset = name == "BT.20")
BT20 <- BT20[,c("S-value","# Tested","# Clonal growth","Group","replicate")]
round(LDA_table(x = BT20),digits = 3)

## ----data.plot,fig.width=6, fig.height=4--------------------------------------
LDA_plot(LDA_tab = BT20,uncertainty.band = T)

## ----single.first,fig.width=6, fig.height=3-----------------------------------
cell.line <- unique(LDAdata$name)[2]
AD <- subset.data.frame(LDAdata, subset = (name==cell.line) & 
                         (replicate==1) & (Group == 2))[,4:6]
LDA_plot(LDA_tab = AD,uncertainty = "act", uncertainty.band = T)
LDA_table(x = AD)
full_model_fit <- LDA_activity_single(x = AD)

## ----set1, fig.width=6, fig.height=3------------------------------------------
AD <- subset.data.frame(LDAdata, subset = (name==cell.line) &
                         (Group == 2))[,c(4:6,3,2)]
LDA_plot(LDA_tab = AD,uncertainty = "act", uncertainty.band = T)
LDA_table(x = AD[,1:3],ref_class = 0)

## ----BT20ep-------------------------------------------------------------------
LDA_table_act <- LDA_table(x = BT20,uncertainty = "act")
cbind(round(LDA_table_act[,1:5],digits = 1),
      round(LDA_table_act[,6:9],digits = 4))
LDA_table_ep <- LDA_table(x = BT20,uncertainty = "ep")
cbind(round(LDA_table_ep[,1:5],digits = 1),
      round(LDA_table_ep[,6:9],digits = 4))

