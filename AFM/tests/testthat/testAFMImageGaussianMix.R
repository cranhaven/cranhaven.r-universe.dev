library(testthat)
assignInNamespace("cedta.override", c(data.table:::cedta.override,"AFM"),"data.table")
assignInNamespace("cedta.override", c(data.table:::cedta.override,"AFMImageAnalyser"),"data.table")

source("./configuration.R")


test_that("performGaussianMixCalculation", {
  
  # library(AFM)
  # 
  # data(AFMImageCollagenNetwork)
  # 
  # AFMImage<-AFMImageCollagenNetwork
  # AFMImage@fullfilename<-"/Users/one/AFMImageCollagenNetwork.txt"
  # gMixAnalysis<-AFMImageGaussianMixAnalysis()
  # 
  # gMixAnalysis@updateProgress<- function(value = NULL, detail = NULL, message = NULL) {
  #   if (exists("progressGaussianMix")){
  #     if (!is.null(message)) {
  #       progressGaussianMix$set(message = message, value = 0)
  #     }else{
  #       progressGaussianMix$set(value = value, detail = detail)
  #     }
  #   }
  # }
  # 
  # gMixAnalysis<-performGaussianMixCalculation(AFMImageGaussianMixAnalysis= gMixAnalysis, AFMImage)
  # print("done performGaussianMixCalculation")
  
  
  # data(AFMImageCollagenNetwork)
  # 
  # newAFMImage<-AFMImageCollagenNetwork
  # newAFMImage@fullfilename<-"/Users/one/AFMImageCollagenNetwork.txt"
  # gaussianMixAnalysis<-AFMImageGaussianMixAnalysis( )
  # 
  # mepsilon=1e-4
  # min=3
  # max=3
  # AFMImage<-AFMImageCollagenNetwork
  # 
  # 
  # gaussianMixAnalysis<-AFMImageGaussianMixAnalysis()
  # gaussianMixAnalysis@minGaussianMix<-min
  # gaussianMixAnalysis@maxGaussianMix<-max
  # gaussianMixAnalysis@epsilonGaussianMix<-mepsilon
  # # Create a closure to update progress
  # gaussianMixAnalysis@updateProgress<- function(value = NULL, detail = NULL, message = NULL) {
  #   if (exists("progressGaussianMix")){
  #     if (!is.null(message)) {
  #       progressGaussianMix$set(message = message, value = 0)
  #     }else{
  #       progressGaussianMix$set(value = value, detail = detail)
  #     }
  #   }
  # }
  # gaussianMixAnalysis<-performGaussianMixCalculation(AFMImageGaussianMixAnalysis= gaussianMixAnalysis, AFMImage= newAFMImage)
  # 
  # mixtureNumberOfComponents<-3
  # heights<-AFMImage@data$h
  # distinct.heights <- sort(unique(heights))
  # heights.k<-gaussianMixAnalysis@gaussianMix[mixtureNumberOfComponents][[1]]
  # tcdfs <- pnormmix(distinct.heights,mixture=heights.k)
  # ecdfs <- ecdf(heights)(distinct.heights)
  # 
  # TheExpDT<-data.table(tcdfs,ecdfs)
  # p1 <- ggplot(data=TheExpDT)
  # p1 <- p1 + geom_point(aes(tcdfs, ecdfs, colour = "blue"),data=TheExpDT, show.legend = FALSE)
  # p1 <- p1 + ylab("Empirical CDF")
  # p1 <- p1 + geom_abline(slope=1, intercept = 0)
  # p1 <- p1 + xlab("Theoretical CDF")
  # 
  # densityCurves<-data.frame(x=density(heights)$x , y=density(heights)$y, style=rep("Kernel", length(density(heights)$y)))
  # x <- seq(min(density(heights)$x),max(density(heights)$x),length=1000)
  # 
  # densityCurves2<-data.frame(x=x, y=dnormalmix(x,heights.k), style=rep("Mixture", length(dnormalmix(x,heights.k))))
  # allHeights<-rbind(densityCurves,densityCurves2)
  # 
  # p2<-ggplot(allHeights, aes(x=x, y=y)) +
  #   geom_line(alpha=0.8,size=1.2, aes(color=style)) 
  # p2 <- p2 + ylab("Density")
  # p2 <- p2 + xlab("Heights (nm)")
  # p2 <- p2 + theme(legend.title=element_blank())
  # 
  # allComponents<-data.table(heights=c(0),counts=c(0), component.number=c(0))
  # 
  # for(component.number in seq(1, mixtureNumberOfComponents)) {
  #   tlength=1000
  #   x <- seq(min(density(heights)$x),max(density(heights)$x),length=tlength)
  #   y   <- dnorm(x,mean=(heights.k$mu[component.number]), sd=heights.k$sigma[component.number])*length(heights)*heights.k$lambda[component.number]
  #   allComponents<-rbind(allComponents, data.table(heights=x,counts=y, component.number=rep(component.number,tlength)))
  # }
  # allComponents<-allComponents[-1,]
  # 
  # 
  # p3 <- ggplot(data=allComponents)
  # p3 <- p3 + geom_point(data=allComponents, aes(heights, counts), size=1.05, color="#FF0000")
  # p3 <- p3 + geom_histogram(data= data.frame(heights=heights), aes(x=heights), binwidth=1, color="#000000", fill="#000080", alpha=0.4)
  
})