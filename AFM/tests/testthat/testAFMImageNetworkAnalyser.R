library(testthat)
assignInNamespace("cedta.override", c(data.table:::cedta.override,"AFM"),"data.table")
assignInNamespace("cedta.override", c(data.table:::cedta.override,"AFMImageAnalyser"),"data.table")

source("./configuration.R")


test_that("identifyNetwork", {
  # library(AFM)
  # library(parallel)
  # 
  # data(AFMImageCollagenNetwork)
  # AFMImage<-AFMImageCollagenNetwork
  # AFMIA = new("AFMImageNetworksAnalysis")
  # AFMIA@heightNetworksslider=10
  # AFMIA@filterNetworkssliderMin=150
  # AFMIA@filterNetworkssliderMax=300
  # AFMIA@smallBranchesTreatment=TRUE
  # clExist<-TRUE
  # cl <- makeCluster(3,outfile="")
  # AFMIA<-transformAFMImageForNetworkAnalysis(AFMImageNetworksAnalysis=AFMIA,AFMImage= AFMImage)
  # AFMIA<-identifyNodesAndEdges(cl=cl,AFMImageNetworksAnalysis= AFMIA,maxHeight= 300)
  # AFMIA<-identifyEdgesFromCircles(cl=cl,AFMImageNetworksAnalysis= AFMIA, MAX_DISTANCE = 75)
  # AFMIA<-identifyIsolatedNodes(AFMIA)
  # AFMIA<-createGraph(AFMIA)
  # AFMIA<-calculateShortestPaths(cl=cl, AFMImageNetworksAnalysis=AFMIA)
  # AFMIA<-calculateNetworkParameters(AFMImageNetworksAnalysis=AFMIA, AFMImage=AFMImage)
  # AFMIA<-calculateHolesCharacteristics(AFMImageNetworksAnalysis=AFMIA)
  # stopCluster(cl)
})