library(testthat)
assignInNamespace("cedta.override", c(data.table:::cedta.override,"AFM"),"data.table")
assignInNamespace("cedta.override", c(data.table:::cedta.override,"AFMImageAnalyser"),"data.table")

source("./configuration.R")


test_that("performGaussianMixesExport", {

  # library(AFM)
  # library(data.table)
  # 
  # data(AFMImageCollagenNetwork)
  # 
  # AFMImage<-AFMImageCollagenNetwork
  # AFMImage@fullfilename<-"/Users/one/AFMImageCollagenNetwork.txt"
  # gMixAnalysis<-AFMImageGaussianMixAnalysis()
  # 
  # # from two components
  # gMixAnalysis@minGaussianMix<-2
  # # to four components
  # gMixAnalysis@maxGaussianMix<-3
  # # convergence criteria
  # gMixAnalysis@epsilonGaussianMix<-1
  # 
  # 
  # # Create a closure to update progress
  # gMixAnalysis@updateProgress<- function(value = NULL, detail = NULL, message = NULL) {
  #   if (exists("progressGaussianMix")){
  #     if (!is.null(message)) {
  #       progressGaussianMix$set(message = message, value = 0)
  #     }else{
  #       progressGaussianMix$set(value = value, detail = detail)
  #     }
  #   }
  # }
  # gMixAnalysis<-performGaussianMixCalculation(AFMImageGaussianMixAnalysis= gMixAnalysis, AFMImage)
  # 
  # gMixAnalysis
  # 
  # 
  # gMixAnalysis@summaryMixture
  # gMixAnalysis@tcdfsEcdfsCheck
  # 
  # 
  # options(java.parameters = "-Xmx2000m") 
  # 
  # library(xlsx)
  # filenameExportGaussianMixes<-"filename.xls"
  # 
  # res=data.table(number_of_components=c(0),
  #                #component=c(0),
  #                mean=c(0),
  #                sd=c(0),
  #                lambda=c(0))
  # 
  # 
  # totalNbOfMixtures<-length(v$AFMImageAnalyser@gaussianMixAnalysis@gaussianMix)
  # totalNbOfMixtures<-length(gMixAnalysis@gaussianMix)
  # #totalNbOfMixtures<-length(v$AFMImageAnalyser@gaussianMixAnalysis@gaussianMix) - length(v$AFMImageAnalyser@gaussianMixAnalysis@gaussianMix[sapply(v$AFMImageAnalyser@gaussianMixAnalysis@gaussianMix, is.null)]) + 1
  # 
  # #for (mixtureNumberOfComponents in seq(v$AFMImageAnalyser@gaussianMixAnalysis@minGaussianMix,totalNbOfMixtures)) {
  # 
  # for (mixtureNumberOfComponents in seq(gMixAnalysis@minGaussianMix,totalNbOfMixtures)) {
  #   
  #   # if (!is.null(v$AFMImageAnalyser@gaussianMixAnalysis@gaussianMix[mixtureNumberOfComponents][[1]])) {
  #   #   mixture<-v$AFMImageAnalyser@gaussianMixAnalysis@gaussianMix[mixtureNumberOfComponents][[1]]
  #   mixture<-gMixAnalysis@gaussianMix[mixtureNumberOfComponents][[1]]
  #   for(component.number in seq(1, mixtureNumberOfComponents)) {
  #     if (length(mixture)>0) {
  #       mean=mixture$mu[component.number]
  #       sd=mixture$sigma[component.number]
  #       lambda=mixture$lambda[component.number]
  #       
  #       res=rbind(res, data.table(number_of_components=mixtureNumberOfComponents,
  #                                 #component=component.number,
  #                                 mean=mean,
  #                                 sd=sd,
  #                                 lambda=lambda))
  #     }
  #     #   }
  #   }
  # }
  # res<-res[-1,]
  # res<-res[order(number_of_components, mean)]
  # res
  # 
  # 
  # write.xlsx(data.frame(res), file=filenameExportGaussianMixes, sheetName="Summary", row.names=FALSE)
  # 
  # 
  # 
  # #heights<-v$AFMImageAnalyser@AFMImage@data$h
  # heights<-AFMImage@data$h
  # distinct.heights <- sort(unique(heights))
  # 
  # totalNbOfMixtures<-length(v$AFMImageAnalyser@gaussianMixAnalysis@gaussianMix) - length(v$AFMImageAnalyser@gaussianMixAnalysis@gaussianMix[sapply(v$AFMImageAnalyser@gaussianMixAnalysis@gaussianMix, is.null)])
  # totalNbOfMixtures<-length(gMixAnalysis@gaussianMix) - length(gMixAnalysis@gaussianMix[sapply(gMixAnalysis@gaussianMix, is.null)])
  # print(totalNbOfMixtures)
  # 
  # 
  # mixtureNumberOfComponents<-2
  # #for (mixtureNumberOfComponents in seq(v$AFMImageAnalyser@gaussianMixAnalysis@minGaussianMix,length(v$AFMImageAnalyser@gaussianMixAnalysis@gaussianMix))) {
  # for (mixtureNumberOfComponents in seq(gMixAnalysis@minGaussianMix,length(gMixAnalysis@gaussianMix))) {  
  #   baseSheetName<-paste0(mixtureNumberOfComponents,"-components-")
  #   #mixtureNumberOfComponents<-2
  #   print(paste("mixtureNumberOfComponents= ",mixtureNumberOfComponents))
  #   #if (!is.null(v$AFMImageAnalyser@gaussianMixAnalysis@gaussianMix[mixtureNumberOfComponents][[1]])) {
  #   if (!is.null(gMixAnalysis@gaussianMix[mixtureNumberOfComponents][[1]])) {
  #     # heights.k<-v$AFMImageAnalyser@gaussianMixAnalysis@gaussianMix[mixtureNumberOfComponents][[1]]
  #     heights.k<-gMixAnalysis@gaussianMix[mixtureNumberOfComponents][[1]]
  #     
  #     tcdfs <- pnormmix(distinct.heights,mixture=heights.k)
  #     ecdfs <- ecdf(heights)(distinct.heights)
  #     TheExpDT<-data.table(tcdfs,ecdfs)
  #     
  #     oneSheetName<-paste0(baseSheetName,"tcdfs-ecdfs")
  #     write.xlsx(data.frame(tcdfs=TheExpDT$tcdfs, ecdfs= TheExpDT$ecdfs), 
  #                file=filenameExportGaussianMixes, 
  #                sheetName=oneSheetName,
  #                append=TRUE,
  #                row.names=FALSE)    
  #     
  #     densityCurves<-data.frame(x=density(heights)$x , y=density(heights)$y, style=rep("Kernel", length(density(heights)$y)))
  #     x <- seq(min(density(heights)$x),max(density(heights)$x),length=1000)
  #     densityCurves2<-data.frame(x=x, y=dnormalmix(x,heights.k), style=rep("Mixture", length(dnormalmix(x,heights.k))))
  #     allHeights<-rbind(densityCurves,densityCurves2)
  #     
  #     oneSheetName<-paste0(baseSheetName,"density-heights")
  #     write.xlsx(data.frame(density=allHeights$x, heights= allHeights$y,curve=allHeights$style),
  #                file=filenameExportGaussianMixes, 
  #                sheetName=oneSheetName,
  #                append=TRUE, row.names=FALSE)    
  #     
  #     allComponents<-data.table(heights=c(0),counts=c(0), component.number=c(0))
  #     
  #     for(component.number in seq(1, mixtureNumberOfComponents)) {
  #       tlength=1000
  #       x <- seq(min(density(heights)$x),max(density(heights)$x),length=tlength)
  #       y   <- dnorm(x,mean=(heights.k$mu[component.number]), sd=heights.k$sigma[component.number])*length(heights)*heights.k$lambda[component.number]
  #       allComponents<-rbind(allComponents, data.table(heights=x,counts=y, component.number=rep(component.number,tlength)))
  #     }
  #     allComponents<-allComponents[-1,]
  #     
  #     oneSheetName<-paste0(baseSheetName,"heights-counts")
  #     write.xlsx( data.frame(heights=heights),
  #                 file=filenameExportGaussianMixes, 
  #                 sheetName=oneSheetName,
  #                 append=TRUE, row.names=FALSE)  
  #     oneSheetName<-paste0(baseSheetName,"components-counts")
  #     write.xlsx(data.frame(allComponents),
  #                file=filenameExportGaussianMixes, 
  #                sheetName=oneSheetName,
  #                append=TRUE, row.names=FALSE)  
  #     
  #     
  #   }
  #   
  # }
  # 
  # 
  # print("done performGaussianMixCalculation")

})
