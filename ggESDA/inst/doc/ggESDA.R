## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(warning=FALSE)
knitr::opts_chunk$set(message=FALSE)
knitr::opts_chunk$set(out.width = "100%")
knitr::opts_chunk$set(fig.align = 'center')

library(knitr)

## ----library------------------------------------------------------------------
library(ggESDA)

## ----createData---------------------------------------------------------------
#aggregated by the variable Species in iris
iris_interval<-classic2sym(iris,groupby = "Species")$intervalData
iris_interval
class(iris_interval)
dim(iris_interval)

myFacedata<-RSDA::facedata
head(myFacedata,5)
class(myFacedata)
dim(myFacedata)

## ----ggInterval_index,eval=FALSE----------------------------------------------
#  ggInterval_index(facedata, aes(x = AD))

## ---- echo=FALSE--------------------------------------------------------------
include_graphics("images/ggInterval_index.png")

## ----ggInterval_index2,eval=FALSE---------------------------------------------
#  m <- mean(facedata$AD)
#  Concepts <- as.factor(rep(c("FRA", "HUS", "INC", "ISA", "JPL", "KHA",
#                              "LOT", "PHI", "ROM"), each = 3))
#  ggInterval_index(facedata, aes(x = AD, fill = Concepts))+
#    theme_bw() +
#    scale_fill_brewer(palette = "Set2")+
#    geom_segment(x = m, xend = m, y = 0, yend = 27,
#                 lty = 2, col = "red", lwd = 1) +
#    geom_text(aes(x = m, y = 28), label = "Mean")+
#    scale_y_continuous(breaks = 1:27,
#                       labels = rownames(facedata))

## ---- echo=FALSE--------------------------------------------------------------
include_graphics("images/ggInterval_index2.png")

## ----ggInterval_minmax,eval=FALSE---------------------------------------------
#  
#  ggInterval_minmax(facedata, aes(x = AD, size = 3))+
#    scale_color_manual(values = c("darkblue", "darkred")) +
#    coord_fixed(ratio = 1)
#    theme_bw()

## ---- echo=FALSE--------------------------------------------------------------
include_graphics("images/ggInterval_minmax.png")

## ----ggInterval_boxplot,eval=FALSE--------------------------------------------
#  ggInterval_boxplot(facedata, plotAll = T) +
#    theme_bw()

## ---- echo=FALSE--------------------------------------------------------------
include_graphics("images/ggInterval_boxplot.png")

## ----ggInterval_boxplot2,eval=FALSE-------------------------------------------
#  ggInterval_boxplot(data=myFacedata,aes(x=AD,col="black",lty=2,lwd=1.2))+
#    scale_fill_manual(values = c("red","yellow",
#                                 "green","blue","grey"),
#                      labels=c("0%","25%","50%","75%","100%"),
#                      name="quantile")

## ---- echo=FALSE--------------------------------------------------------------
include_graphics("images/ggInterval_boxplot2.png")

## ----ggInterval_hist,eval=FALSE-----------------------------------------------
#  equal_bin <- ggInterval_hist(facedata, plotAll = T) +
#                theme_bw()
#  unequal_bin <- ggInterval_hist(facedata, plotAll = T,
#                                 method = "unequal-bin") +
#                theme_bw()
#  ggarrange(equal_bin, unequal_bin, ncol = 2)

## ---- echo=FALSE--------------------------------------------------------------
include_graphics("images/ggInterval_hist.png")

## ----ggInterval_centerRange,eval=FALSE----------------------------------------
#  ggInterval_centerRange(iris_interval,aes(x = Sepal.Width)) +
#    geom_text(label = rownames(iris_interval), vjust = -0.8) +
#    scale_x_continuous(limits = c(2.6, 3.4)) +
#    scale_y_continuous(limits = c(1.3, 2.2))

## ---- echo=FALSE--------------------------------------------------------------
include_graphics("images/ggInterval_centerRange.png")

## ----ggInterval_centerRange2,eval=FALSE---------------------------------------
#  ggInterval_centerRange(myFacedata[11:20, ],aes(x = GH))+
#    geom_text(label = rownames(myFacedata)[11:20], vjust = -0.8, size =  3)

## ---- echo=FALSE--------------------------------------------------------------
include_graphics("images/ggInterval_centerRange2.png")

## ----ggInterval_scatter,eval=FALSE--------------------------------------------
#  myCol <- rep(RColorBrewer::brewer.pal(9, "Set1"), each = 3)
#  ggInterval_scatter(data = facedata, aes(x = AD, y = BC)) +
#    scale_fill_manual(values = myCol, name = "CONCEPTS",
#                      label = rownames(facedata)) +
#    theme_bw()

## ---- echo=FALSE--------------------------------------------------------------
include_graphics("images/ggInterval_scatter.png")

## ----ggInterval_scaMatrix,eval=FALSE------------------------------------------
#  ggInterval_scaMatrix(facedata)

## ---- echo=FALSE--------------------------------------------------------------
include_graphics("images/ggInterval_scaMatrix.png")

## ----ggInterval_2Dhist,eval=FALSE---------------------------------------------
#  ggInterval_2Dhist(iris_interval, aes(x = Sepal.Length, y = Petal.Length))

## ---- echo=FALSE--------------------------------------------------------------
include_graphics("images/ggInterval_2Dhist.png")

## ----ggInterval_2Dhist2,eval=FALSE--------------------------------------------
#  ggInterval_2Dhist(facedata, aes(x = BC, y = AH, col = "gray50")) +
#    scale_fill_gradient(
#      low = "gray85",
#      high = "red"
#    ) +
#    theme_bw()

## ---- echo=FALSE--------------------------------------------------------------
include_graphics("images/ggInterval_2Dhist2.png")

## ----ggInterval_2DhistMatrix,eval=FALSE---------------------------------------
#  ggInterval_2DhistMatrix(facedata,
#                          xBins = 10,
#                          yBins = 10,
#                          removeZero = T,
#                          addFreq = F)

## ---- echo=FALSE--------------------------------------------------------------
include_graphics("images/ggInterval_2DhistMatrix.png")

## ----ggInterval_indexImage,eval=FALSE-----------------------------------------
#  ggInterval_indexImage(facedata, aes(x = AD)) +
#    coord_flip()
#  

## ---- echo=FALSE--------------------------------------------------------------
include_graphics("images/ggInterval_indexImage.png")

## ----ggInterval_indexImage2,eval=FALSE----------------------------------------
#  p1 <- ggInterval_indexImage(facedata, plotAll = T, column_condition = T,
#                        full_strip = T)
#  
#  p2 <- ggInterval_indexImage(facedata, plotAll = T, column_condition = F,
#                        full_strip = T)
#  
#  ggpubr::ggarrange(p1, p2, ncol = 2)
#  

## ---- echo=FALSE--------------------------------------------------------------
include_graphics("images/ggInterval_indexImage2.png")

## ----ggInterval_3Dscatter,eval=FALSE------------------------------------------
#  ggInterval_3Dscatter(iris_interval, aes(Sepal.Length, Petal.Length, Petal.Width))

## ---- echo=FALSE--------------------------------------------------------------
include_graphics("images/ggInterval_3Dscatter.png")

## ----ggInterval_3Dscatter2,eval=FALSE-----------------------------------------
#  ggInterval_3Dscatter(myFacedata[1:8, ], aes(AD, BC, AH), scale = TRUE)

## ---- echo=FALSE--------------------------------------------------------------
include_graphics("images/ggInterval_3Dscatter2.png")

## ----ggInterval_radar,eval=FALSE----------------------------------------------
#  
#  p1 <- ggInterval_radar(Environment,
#                   plotPartial = 2,
#                   showLegend = F,
#                   base_circle = T,
#                   base_lty = 2,
#                   addText = F) +
#    labs(title = "") +
#    theme_bw() +
#    scale_fill_manual(values = c("gray50")) +
#    scale_color_manual(values = c("gray50"))
#  
#  
#  p2 <- ggInterval_radar(Environment,
#                         plotPartial = 7,
#                         showLegend = F,
#                         base_circle = F,
#                         base_lty = 1,
#                         addText = T) +
#    labs(title = "") +
#    theme_bw() +
#    scale_fill_manual(values = c("gray50")) +
#    scale_color_manual(values = c("gray50"))
#  ggpubr::ggarrange(p1, p2, ncol = 2)

## ---- echo=FALSE--------------------------------------------------------------
include_graphics("images/ggInterval_radar.png")

## ----ggInterval_radar2,eval=FALSE---------------------------------------------
#  p1 <- ggInterval_radar(Environment,
#                   plotPartial = c(1, 4),
#                   showLegend = F,
#                   addText = F) +
#    scale_fill_manual(values = c("darkblue", "darkred")) +
#    scale_color_manual(values = c("darkblue", "darkred"))
#  
#  p2 <- ggInterval_radar(Environment,
#                         plotPartial = c(1, 4),
#                         showLegend = F,
#                         addText = F,
#                         base_circle = F,
#                         base_lty = 1,
#                         type = "rect") +
#    scale_fill_manual(values = c("darkblue", "darkred")) +
#    scale_color_manual(values = c("darkblue", "darkred"))
#  ggpubr::ggarrange(p1, p2, ncol = 2)

## ---- echo=FALSE--------------------------------------------------------------
include_graphics("images/ggInterval_radar2.png")

## ----ggInterval_radar3,eval=FALSE---------------------------------------------
#  
#  dataSetList <- list(AbaloneIdt = AbaloneIdt,
#                      BLOOD = BLOOD,
#                      Cardiological = Cardiological,
#                      facedata = facedata,
#                      oils = oils,
#                      mushroom = mushroom,
#                      Environment = Environment)
#  myFill <- c("white", "gray10", "gray20",
#              "gray30", "gray40", "gray50",
#              "gray60", "gray70", "white",
#              "white", "white")
#  myCol <- myFill; myCol[1] <- "black"
#  pList <- NULL
#  u <- 1
#  for(i in dataSetList){
#    p <- ggInterval_radar(i,
#                          base_circle = F,
#                          base_lty = 1,
#                          type = "quantile",
#                          quantileNum = 10,
#                          showLegend = F,
#                          Drift = 0)+
#      labs(title = names(dataSetList)[u]) +
#      scale_fill_manual(values = rev(myFill)) +
#      scale_color_manual(values = rev(myCol))
#      ggthemes::theme_hc()
#    pList[[u]] <- p
#    u <- u + 1
#  }
#  
#  gridExtra::marrangeGrob(pList, nrow = 2, ncol = 4,
#                          top = "")

## ---- echo=FALSE--------------------------------------------------------------
include_graphics("images/ggInterval_radar3.png")

## ----ggInterval_PCA,fig.show='hide'-------------------------------------------


CONCEPT <- rep(c("FRA", "HUS", "INC", "ISA", "JPL", "KHA",
           "LOT", "PHI", "ROM"), each = 3)
p <- ggInterval_PCA(facedata, poly = T,
                    concepts_group = CONCEPT)
p$ggplotPCA <- p$ggplotPCA + theme(legend.position = "top") + 
  theme_bw() 

p2 <- ggInterval_PCA(facedata, poly = F,
                    concepts_group = CONCEPT)
p2$ggplotPCA <- p2$ggplotPCA + theme(legend.position = "top") +
  theme_bw()

ggpubr::ggarrange(p$ggplotPCA, p2$ggplotPCA, ncol = 2)


## ---- echo=FALSE--------------------------------------------------------------
include_graphics("images/ggInterval_PCA.png")

## ----pcaLoadings--------------------------------------------------------------
myPCA <- p2
myPCA$loadings

## -----------------------------------------------------------------------------
cumsum(myPCA$sdev/sum(myPCA$sdev))

## -----------------------------------------------------------------------------
head(myPCA$scores_interval[,1:3])

