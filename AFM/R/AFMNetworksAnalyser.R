require(igraph)
require(dbscan)
require(data.table)
require(sp)
require(parallel)
require(mixtools)
require(grDevices)


HASHSIZE<-512*512
RADIUS_MULTIPLIER<-2
BIGGER_CIRCLE_RADIUS<-4
BIGGER_CIRCLE_RADIUS_MULTILPLIER<-2
SAMPLE_ON_THIN_PORTIONS<-25 # percents
MAX_DISTANCE<-64
AREA_MIN <-25
CLUSTER_COUNT_MIN <- 45
CIRCLE_RADIUS_INIT <- 25


setOldClass("igraph")

#' AFM image networks analysis class
#' 
#' A S4 class to handle the networks calculation 
#'
#' @slot vertexHashsize hash to transform coordinates to vertexId
#' @slot binaryAFMImage the AFMImage after transformation before analysis
#' @slot binaryAFMImageWithCircles the AFMImage after transformation with the spotted circles
#' @slot circlesTable a data.table of identified circles
#' @slot edgesTable  a data.table of edges
#' @slot fusionedNodesCorrespondance  a data.table of corresponsdance between intial node and fusioned node
#' @slot fusionedNodesEdgesTable a data.table of nodes fusioned because of intersecting
#' @slot isolatedNodesTable a data.table of isolated nodes
#' @slot heightNetworksslider used multiplier of heights to facilitate analysis
#' @slot filterNetworkssliderMin used filter minimum value to facilitate analysis
#' @slot filterNetworkssliderMax used filter maximum value to facilitate analysis
#' @slot smallBranchesTreatment boolean - smallest circle used or not
#' @slot originalGraph a list of \code{\link{igraph}}
#' @slot skeletonGraph a list of \code{\link{igraph}}
#' @slot shortestPaths a data.table of shortest paths
#' @slot networksCharacteristics a data.table to store the skeleton graph characteristics
#' @slot graphEvcent an array to store Evcent
#' @slot graphBetweenness an array to store the graph betweenness
#' @slot libVersion version of the AFM library used to perform the analysis
#' @slot updateProgress a function to update a graphical user interface
#' @name AFMImageNetworksAnalysis-class
#' @rdname AFMImageNetworksAnalysis-class
#' @exportClass AFMImageNetworksAnalysis
#' @author M.Beauvais
AFMImageNetworksAnalysis<-setClass("AFMImageNetworksAnalysis",
                                   slots = c(
                                     vertexHashsize="numeric",
                                     binaryAFMImage="AFMImage",
                                     binaryAFMImageWithCircles="AFMImage",
                                     circlesTable="data.table",
                                     edgesTable="data.table",
                                     fusionedNodesCorrespondance="data.table",
                                     fusionedNodesEdgesTable="data.table",
                                     isolatedNodesList="numeric",
                                     heightNetworksslider="numeric",
                                     filterNetworkssliderMin="numeric",
                                     filterNetworkssliderMax="numeric",
                                     smallBranchesTreatment="logical",
                                     originalGraph="igraph", 
                                     skeletonGraph="igraph",
                                     shortestPaths="data.table",
                                     networksCharacteristics="data.table",
                                     holes="data.table",
                                     holesCharacteristics="data.table",
                                     graphEvcent="numeric",
                                     graphBetweenness="numeric",
                                     libVersion="character",
                                     updateProgress="function"))

#' Constructor method of AFMImageNetworksAnalysis Class.
#'
#' @param .Object an AFMImageNetworksAnalysis Class
#' @param vertexHashsize hash to transform coordinates to vertexId
#' @param binaryAFMImage the AFMImage after transformation before analysis
#' @param binaryAFMImageWithCircles the AFMImage after transformation with the spotted circles
#' @param circlesTable a data.table of identified circles
#' @param edgesTable  a data.table of edges
#' @param fusionedNodesCorrespondance  a data.table of correspon
#' @param fusionedNodesEdgesTable a data.table of corresponsdance between intial node and fusioned node
#' @param isolatedNodesList a data.table of isolated nodes
#' @param heightNetworksslider used multiplier of heights to facilitate analysis
#' @param filterNetworkssliderMin used filter minimum value to facilitate analysis
#' @param filterNetworkssliderMax used filter maximum value to facilitate analysis
#' @param smallBranchesTreatment boolean - smallest circle used or not
#' @param originalGraph a list of \code{\link{igraph}}
#' @param skeletonGraph a list of \code{\link{igraph}}
#' @param shortestPaths a data.table of shortest path
#' @param networksCharacteristics a data.table to store the skeleton graph characteristics
#' @param holes a data.table to store the cluster number of each point
#' @param holesCharacteristics a data.table to summarize the data about holes
#' @param graphEvcent an array to store Evcent
#' @param graphBetweenness an array to store the graph betweenness
#' @param libVersion version of the AFM library used to perform the analysis
#' @rdname AFMImageNetworksAnalysis-class
#' @export
setMethod("initialize", "AFMImageNetworksAnalysis", function(.Object, 
                                                             vertexHashsize,
                                                             binaryAFMImage,
                                                             binaryAFMImageWithCircles,
                                                             circlesTable,
                                                             edgesTable,
                                                             fusionedNodesCorrespondance,
                                                             fusionedNodesEdgesTable,
                                                             isolatedNodesList,
                                                             heightNetworksslider,
                                                             filterNetworkssliderMin,
                                                             filterNetworkssliderMax,
                                                             smallBranchesTreatment,
                                                             originalGraph, 
                                                             skeletonGraph,
                                                             shortestPaths,
                                                             networksCharacteristics,
                                                             holes,
                                                             holesCharacteristics,
                                                             graphEvcent,
                                                             graphBetweenness,
                                                             libVersion)  
{
  if(!missing(vertexHashsize)) .Object@vertexHashsize<-vertexHashsize
  if(!missing(binaryAFMImage)) .Object@binaryAFMImage<-binaryAFMImage
  if(!missing(binaryAFMImageWithCircles)) .Object@binaryAFMImageWithCircles<-binaryAFMImageWithCircles
  if(!missing(circlesTable)) .Object@circlesTable<-circlesTable
  if(!missing(edgesTable)) .Object@edgesTable<-edgesTable
  if(!missing(fusionedNodesCorrespondance)) .Object@fusionedNodesCorrespondance<-fusionedNodesCorrespondance
  if(!missing(fusionedNodesEdgesTable)) .Object@fusionedNodesEdgesTable<-fusionedNodesEdgesTable
  if(!missing(isolatedNodesList)) .Object@isolatedNodesList<-isolatedNodesList
  if(!missing(originalGraph)) .Object@originalGraph<-originalGraph
  if(!missing(skeletonGraph)) .Object@skeletonGraph<-skeletonGraph
  if(!missing(shortestPaths)) .Object@shortestPaths<-shortestPaths
  if(!missing(networksCharacteristics)) .Object@networksCharacteristics<-networksCharacteristics
  if(!missing(holes)) .Object@holes<-holes
  if(!missing(holesCharacteristics)) .Object@holesCharacteristics<-holesCharacteristics
  if(!missing(graphEvcent)) .Object@graphEvcent<-graphEvcent
  if(!missing(graphBetweenness)) .Object@graphBetweenness<-graphBetweenness
  if(!missing(heightNetworksslider)) .Object@heightNetworksslider<-heightNetworksslider
  if(!missing(filterNetworkssliderMin)) .Object@filterNetworkssliderMin<-filterNetworkssliderMin
  if(!missing(filterNetworkssliderMax)) .Object@filterNetworkssliderMax<-filterNetworkssliderMax
  if(!missing(smallBranchesTreatment)) .Object@smallBranchesTreatment<-smallBranchesTreatment
  #if(!missing(libVersion))
  .Object@libVersion<-as.character(packageVersion("AFM"))
  #validObject(.Object)      
  return(.Object)
})


#' Wrapper function AFMImageNetworksAnalysis
#'
#' @rdname AFMImageNetworksAnalysis-class
#' @export
AFMImageNetworksAnalysis <- function() {
  return(new("AFMImageNetworksAnalysis"))
}

#' Multiply, filter the heights and make a binary AFMImage from the transformed AFMImage
#'
#' \code{transformAFMImageForNetworkAnalysis} update  \code{\link{AFMImageNetworksAnalysis}} making a binary AFMImage
#' 
#' @param AFMImage an \code{\link{AFMImage}} from Atomic Force Microscopy
#' @param AFMImageNetworksAnalysis n \code{\link{AFMImageNetworksAnalysis}} to store the results of the transformation
#' 
#' @name transformAFMImageForNetworkAnalysis
#' @rdname transformAFMImageForNetworkAnalysis-methods
#' @exportMethod transformAFMImageForNetworkAnalysis
#' @author M.Beauvais
setGeneric(name= "transformAFMImageForNetworkAnalysis", 
           def= function(AFMImageNetworksAnalysis, AFMImage) {
             return(standardGeneric("transformAFMImageForNetworkAnalysis"))
           })

#' @rdname transformAFMImageForNetworkAnalysis-methods
#' @aliases transformAFMImageForNetworkAnalysis,AFMImage-method
setMethod(f="transformAFMImageForNetworkAnalysis", "AFMImageNetworksAnalysis",
          definition= function(AFMImageNetworksAnalysis, AFMImage) {
            newAFMImage<-multiplyHeightsAFMImage(AFMImage, multiplier=AFMImageNetworksAnalysis@heightNetworksslider)
            newAFMImage<-filterAFMImage(newAFMImage,
                                        Min=AFMImageNetworksAnalysis@filterNetworkssliderMin,
                                        Max=AFMImageNetworksAnalysis@filterNetworkssliderMax)
            newAFMImage<-makeBinaryAFMImage(newAFMImage)
            AFMImageNetworksAnalysis@binaryAFMImage<-copy(newAFMImage)
            AFMImageNetworksAnalysis@vertexHashsize<-newAFMImage@samplesperline*newAFMImage@lines
            return(AFMImageNetworksAnalysis)
          })

#' Calculate networks on the surface
#'
#' \code{calculateNetworks} update  \code{\link{AFMImageNetworksAnalysis}}
#' 
#' @param AFMImage an \code{\link{AFMImage}} from Atomic Force Microscopy
#' @param AFMImageNetworksAnalysis n \code{\link{AFMImageNetworksAnalysis}} to store the results of networks analysis
#' 
#' @name calculateNetworks
#' @rdname calculateNetworks-methods
#' @exportMethod calculateNetworks
#' @author M.Beauvais
setGeneric(name= "calculateNetworks", 
           def= function(AFMImageNetworksAnalysis, AFMImage) {
             return(standardGeneric("calculateNetworks"))
           })

#' @rdname calculateNetworks-methods
#' @aliases calculateNetworks,AFMImage-method
setMethod(f="calculateNetworks", "AFMImageNetworksAnalysis",
          definition= function(AFMImageNetworksAnalysis, AFMImage) {
            
            counter<-0
            totalLength<-2
            if (!is.null(AFMImageNetworksAnalysis@updateProgress)&&
                is.function(AFMImageNetworksAnalysis@updateProgress)&&
                !is.null(AFMImageNetworksAnalysis@updateProgress())) {
              text <- paste0("Creating networks")
              AFMImageNetworksAnalysis@updateProgress(value= 0, detail = text)
              
              counter<-counter+1
              value<-counter / totalLength
              text <- paste0("Creating networks", round(counter, 2),"/",totalLength)
              AFMImageNetworksAnalysis@updateProgress(value= value, detail = text)
              print("update")
            }
            
            AFMImageNetworksAnalysis@originalGraph<-calculateIgraph(AFMImageNetworksAnalysis= AFMImageNetworksAnalysis, AFMImage = AFMImage)
            
            if (!is.null(AFMImageNetworksAnalysis@updateProgress)&&
                is.function(AFMImageNetworksAnalysis@updateProgress)&&
                !is.null(AFMImageNetworksAnalysis@updateProgress())) {
              text <- paste0("Creating networks skeleton")
              AFMImageNetworksAnalysis@updateProgress(value= 0, detail = text)
              
              counter<-counter+1
              value<-counter / totalLength
              text <- paste0("Creating networks", round(counter, 2),"/",totalLength)
              AFMImageNetworksAnalysis@updateProgress(value= value, detail = text)
              print("update")
            }
            
            AFMImageNetworksAnalysis<-calculateNetworkSkeleton(AFMImageNetworksAnalysis= AFMImageNetworksAnalysis, AFMImage = AFMImage)
            
            return(AFMImageNetworksAnalysis)
          })

#' Get Network parameters
#'
#' Get basic network parameters :
#' Total root mean square Roughness or Total Rrms or totalRMSRoughness_TotalRrms\cr
#' Mean roughness or Ra or MeanRoughness_Ra
#'
#' \code{getNetworkParameters} returns a data.table of network parameters
#' @param AFMImageNetworksAnalysis an \code{\link{AFMImageNetworksAnalysis}}
#' @param AFMImage an \code{\link{AFMImage}}
#' @return a data.table of network parameters: 
#' \itemize{
#'   \item totalNumberOfNodes the total number of nodes with degree different of 2
#'   \item totalNumberOfNodesWithDegreeTwoOrMore  the total number of nodes with degree 2 or more
#'   \item totalNumberOfNodesWithDegreeOne the total number of nodes with degree one
#'   \item numberOfNodesPerArea  the total number of nodes with degree diffrent of 2 per area
#'   \item numberOfNodesPerSurfaceArea the total number of nodes with degree diffrent of 2 per surface area
#'   \item MeanPhysicalDistanceBetweenNodes the mean physical distance between nodes of degree different of two
#' }
#' @author M.Beauvais
#' @name getNetworkParameters
#' @rdname getNetworkParameters-methods
#' @exportMethod getNetworkParameters
#' @examples
#' \dontrun{
#' library(AFM)
#' library(parallel)
#' 
#' data(AFMImageCollagenNetwork)
#' AFMImage<-AFMImageCollagenNetwork
#' AFMIA = new("AFMImageNetworksAnalysis")
#' AFMIA@heightNetworksslider=10
#' AFMIA@filterNetworkssliderMin=150
#' AFMIA@filterNetworkssliderMax=300
#' AFMIA@smallBranchesTreatment=TRUE
#' clExist<-TRUE
#' cl <- makeCluster(2,outfile="")
#' AFMIA<-transformAFMImageForNetworkAnalysis(AFMImageNetworksAnalysis=AFMIA,AFMImage= AFMImage)
#' AFMIA<-identifyNodesAndEdges(cl=cl,AFMImageNetworksAnalysis= AFMIA,maxHeight= 300)
#' AFMIA<-identifyEdgesFromCircles(cl=cl,AFMImageNetworksAnalysis= AFMIA, MAX_DISTANCE = 75)
#' AFMIA<-identifyIsolatedNodes(AFMIA)
#' AFMIA<-createGraph(AFMIA)
#' AFMIA<-calculateShortestPaths(cl=cl, AFMImageNetworksAnalysis=AFMIA)
#' AFMIA<-calculateNetworkParameters(AFMImageNetworksAnalysis=AFMIA, AFMImage=AFMImage)
#' AFMIA<-calculateHolesCharacteristics(AFMImageNetworksAnalysis=AFMIA)
#' stopCluster(cl)
#' }
setGeneric(name= "getNetworkParameters", 
           def= function(AFMImageNetworksAnalysis, AFMImage) {
             return(standardGeneric("getNetworkParameters"))
           })

#' @rdname getNetworkParameters-methods
#' @aliases getNetworkParameters,AFMImage-method
setMethod(f="getNetworkParameters", "AFMImageNetworksAnalysis",
          definition= function(AFMImageNetworksAnalysis, AFMImage) {
            node_degree<-NULL
            
            # get parameters about the image
            param<-getRoughnessParameters(AFMImage)
            
            # network parameters
            g<-AFMImageNetworksAnalysis@skeletonGraph
            
            verticesAnalysisDT<-data.table(vid=V(g)$name, node_degree=unname(degree(g)))
            verticesAnalysisDT
            directedConnectedNodesDT<-AFMImageNetworksAnalysis@shortestPaths
            directedConnectedNodesDT
            
            #Total Number of nodes
            totalNumberOfNodes<-nrow(verticesAnalysisDT[node_degree!=2])
            
            #Surface
            area<-param$area
            
            #Number of nodes with degree > 2
            totalNumberOfNodesWithDegreeTwoOrMorePerArea<-nrow(verticesAnalysisDT[node_degree>2])/area
            
            #Number of nodes with degree = 1
            totalNumberOfNodesWithDegreeOnePerArea<-nrow(verticesAnalysisDT[node_degree==1])/area
            
            #Surface area of a grid of heights
            surfaceArea<-param$surfaceArea
            
            #Nodes (degree>2 or =1) / area
            numberOfNodesPerArea<-(nrow(verticesAnalysisDT[node_degree>2])+nrow(verticesAnalysisDT[node_degree==1]))/area
            
            #Nodes (degree>2 or =1) / surface area
            numberOfNodesPerSurfaceArea<-(nrow(verticesAnalysisDT[node_degree>2])+nrow(verticesAnalysisDT[node_degree==1]))/surfaceArea
            
            #Mean physical distance between nodes (degree>2)
            MeanPhysicalDistanceBetweenNodes<-mean(directedConnectedNodesDT$physicalDistance)
            
            
            resultDT=data.table(totalNumberOfNodes=totalNumberOfNodes,
                                totalNumberOfNodesWithDegreeTwoOrMorePerArea=totalNumberOfNodesWithDegreeTwoOrMorePerArea,
                                totalNumberOfNodesWithDegreeOnePerArea=totalNumberOfNodesWithDegreeOnePerArea,
                                numberOfNodesPerArea=numberOfNodesPerArea,
                                numberOfNodesPerSurfaceArea=numberOfNodesPerSurfaceArea,
                                MeanPhysicalDistanceBetweenNodes=MeanPhysicalDistanceBetweenNodes)
            
            return(resultDT)
          })

#' Get vertex id from x,y coordinates
#'
#' \code{getVertexId} return the vertexId
#' 
#' @param AFMImage an \code{\link{AFMImage}} from Atomic Force Microscopy
#' @param x coordinates in x axis
#' @param y coordinates in y axis
#' @author M.Beauvais
#' @export
getVertexId<-function(AFMImage,x,y) {
  if ((x<0)||(x>AFMImage@samplesperline)||
      (y<0)||(y>AFMImage@lines)) return(-1)
  #print(paste("getVertexId",x,y,as.numeric(x+HASHSIZE*y)))
  #return(as.numeric(x+AFMImage@samplesperline*y))
  return(as.numeric(x+HASHSIZE*y))
  
}

#' Calculate Gaussian Mixture with two components from the AFM Image.
#'
#' \code{calculateGaussianMixture} return a data.table containing the result of the Gaussian Mixture and result of the test
#' 
#' @param AFMImage an \code{\link{AFMImage}} from Atomic Force Microscopy
#' @author M.Beauvais
#' @export
#' @examples
#' \dontrun{
#' library(AFM)
#' data(AFMImageOfNetworks)
#' mixtureCharacteristics<-calculateGaussianMixture(AFMImageOfNetworks)
#' print(mixtureCharacteristics)
#' }
calculateGaussianMixture<-function(AFMImage) {
  k = 2
  filename<-AFMImage@fullfilename
  
  heights<-AFMImage@data$h*10
  heights<-heights+abs(min(heights))
  #heights[heights>300]<-300
  
  mixmdl = normalmixEM(heights,k=2, arbmean = TRUE)
  summary(mixmdl)
  mixmdl
  
  # CDF of mixture of two normals
  pmnorm <- function(x, mu, sigma, pmix) {
    pmix[1]*pnorm(x,mu[1],sigma[1]) + (1-pmix[1])*pnorm(x,mu[2],sigma[2])
  }
  test <- ks.test(heights, pmnorm, mu=mixmdl$mu, sigma=mixmdl$sigma, pmix=mixmdl$lambda)
  print(test)
  
  
  if (mixmdl$mu[1]<mixmdl$mu[2]) {
    invert=0
    min_mu<-mixmdl$mu[1]
    min_sigma<-mixmdl$sigma[1]
    min_lambda<-mixmdl$lambda[1]
    max_mu<-mixmdl$mu[2]
    max_sigma<-mixmdl$sigma[2]
    max_lambda<-mixmdl$lambda[2]
  }else{
    invert=1
    min_mu<-mixmdl$mu[2]
    min_sigma<-mixmdl$sigma[2]
    min_lambda<-mixmdl$lambda[2]
    max_mu<-mixmdl$mu[1]
    max_sigma<-mixmdl$sigma[1]
    max_lambda<-mixmdl$lambda[1]
  }
  
  
  gaussianMixture<- data.table(filename=filename,
                               invert=invert,
                               min_mu=min_mu,
                               min_sigma=min_sigma,
                               min_lambda=min_lambda,
                               max_mu=max_mu,
                               max_sigma=max_sigma,
                               max_lambda=max_lambda,
                               ks_test_pvalue=test$p.value,
                               ks_test_D=unname(test$statistic))
  return(gaussianMixture)
  
}

#' Get x,y coordinates from vertex id
#'
#' \code{getCoordinatesFromVertexId} return a list x,y coordinates
#' 
#' @param vId the vertex id
#' @author M.Beauvais
#' @export
getCoordinatesFromVertexId<-function(vId) {
  vertexId<-as.numeric(vId)
  y<-floor(vertexId/HASHSIZE)
  x<-vertexId-y*HASHSIZE
  return(data.table(vId=vId, coords.x1=x,coords.x2=y))
}
# getCoordinatesFromVertexId<-function(AFMImage, vId) {
#   # vertexId<-as.numeric(vId)
#   # y<-floor(vertexId/HASHSIZE)
#   # x<-vertexId-y*HASHSIZE
#   # return(c(x,y))
#   vertexId<-as.numeric(vId)
#   y<-floor(vertexId/HASHSIZE)
#   x<-vertexId-y*HASHSIZE
#   return(data.table(vId=vId, coords.x1=x,coords.x2=y))
# }

#' #' @export
#' getCoordinatesFromVertexId2<-function(AFMImage, vId) {
#'   vertexId<-as.numeric(vId)
#'   y<-floor(vertexId/HASHSIZE)
#'   x<-vertexId-y*HASHSIZE
#'   return(data.table(vId=vId, coords.x1=x,coords.x2=y))
#' }

#' Get getNetworkGridLayout
#'
#' \code{getNetworkGridLayout} return a list x,y coordinates
#' 
#' @param AFMImage an \code{\link{AFMImage}} from Atomic Force Microscopy
#' @param vId the vertex id
#' @author M.Beauvais
#' @export
getNetworkGridLayout<-function(AFMImage, vId) {
  vertexId<-as.numeric(vId)
  y<-floor(vertexId/HASHSIZE)
  x<-vertexId-y*HASHSIZE
  return(data.table(x=x,y=y))
}

#' Does an edge exist ?
#'
#' \code{existsEdge} return TRUE if an edge exists for this vertex id
#' 
#' @param AFMImage an \code{\link{AFMImage}} from Atomic Force Microscopy
#' @param vertexId the vertex id
#' @author M.Beauvais
#' @export
existsEdge<-function(AFMImage, vertexId) {
  # print(vertexId)
  if ((vertexId<1)||(vertexId>(AFMImage@samplesperline+HASHSIZE*(AFMImage@lines-1)))) {
    # print("return FALSE")
    return(FALSE)
  }
  # print(vertexId)
  
  
  coordinates<-getCoordinatesFromVertexId(vertexId)
  # print(coordinnates)
  id<-coordinates[1]+AFMImage@samplesperline*coordinates[2]
  # print(id)
  if (AFMImage@data$h[id]>0) {
    # print("return TRUE")
    return(TRUE)
  }
  # print("return FALSE")
  return(FALSE)
}

#' Get surrounding vertices from x,y coordinates
#'
#' \code{getSurroundingVertexesList} return the vertexId
#' 
#' @param AFMImage an \code{\link{AFMImage}} from Atomic Force Microscopy
#' @param x coordinates in x axis
#' @param y coordinates in y axis
#' @author M.Beauvais
#' @export
getSurroundingVertexesList<-function(AFMImage,x,y) {
  #   print(x)
  #   print(y)
  horizontalWeight<-AFMImage@hscansize/AFMImage@samplesperline
  verticalWeight<-AFMImage@vscansize/AFMImage@lines
  diagWeight<-sqrt((AFMImage@vscansize/AFMImage@lines)^2+(AFMImage@hscansize/AFMImage@samplesperline)^2)
  
  currentVertexId<-getVertexId(AFMImage,x,y)
  vList=data.table()
  #x+1 y
  nearVertexId<-getVertexId(AFMImage,x+1,y) 
  # print(nearVertexId)
  if (existsEdge(AFMImage, nearVertexId)) vList<-rbind(vList, data.table(from=as.character(currentVertexId), to=as.character(nearVertexId), weight=as.numeric(horizontalWeight)))
  #x+1 y+1
  nearVertexId<-getVertexId(AFMImage,x+1,y+1) 
  #print(existsEdge(AFMImage, nearVertexId))
  if (existsEdge(AFMImage, nearVertexId)) vList<-rbind(vList, data.table(from=as.character(currentVertexId), to=as.character(nearVertexId), weight=as.numeric(diagWeight)))
  
  #x y+1
  nearVertexId<-getVertexId(AFMImage,x,y+1) 
  if (existsEdge(AFMImage, nearVertexId)) vList<-rbind(vList, data.table(from=as.character(currentVertexId), to=as.character(nearVertexId), weight=as.numeric(verticalWeight)))
  
  #x-1 y+1
  nearVertexId<-getVertexId(AFMImage,x-1,y+1) 
  if (existsEdge(AFMImage, nearVertexId)) vList<-rbind(vList, data.table(from=as.character(currentVertexId), to=as.character(nearVertexId), weight=as.numeric(diagWeight)))
  
  #x-1 y
  nearVertexId<-getVertexId(AFMImage,x-1,y) 
  if (existsEdge(AFMImage, nearVertexId)) vList<-rbind(vList, data.table(from=as.character(currentVertexId), to=as.character(nearVertexId), weight=as.numeric(horizontalWeight)))
  
  #x-1 y-1
  nearVertexId<-getVertexId(AFMImage,x-1,y-1) 
  if (existsEdge(AFMImage, nearVertexId)) vList<-rbind(vList, data.table(from=as.character(currentVertexId), to=as.character(nearVertexId), weight=as.numeric(diagWeight)))
  
  #x y-1
  nearVertexId<-getVertexId(AFMImage,x,y-1) 
  if (existsEdge(AFMImage, nearVertexId)) vList<-rbind(vList, data.table(from=as.character(currentVertexId), to=as.character(nearVertexId), weight=as.numeric(verticalWeight)))
  
  #x+1 y-1
  nearVertexId<-getVertexId(AFMImage,x+1,y-1) 
  if (existsEdge(AFMImage, nearVertexId)) vList<-rbind(vList, data.table(from=as.character(currentVertexId), to=as.character(nearVertexId), weight=as.numeric(diagWeight)))
  return(vList)
}

#' isAdjacentToBetterVertex
#'
#' \code{isAdjacentToBetterVertex} return TRUE if vertex is adjacent to a better vertex
#' 
#' @param AFMImage an \code{\link{AFMImage}} from Atomic Force Microscopy
#' @param x coordinates in x axis
#' @param y coordinates in y axis
#' @author M.Beauvais
#' @export
isAdjacentToBetterVertex<-function(AFMImage,x,y) {
  #   print(x)
  #   print(y)
  
  currentVertexId<-getVertexId(AFMImage,x,y) 
  currentH<-AFMImage@data$h[currentVertexId]
  
  if(currentH<=0) return(FALSE)
  
  #x+1 y
  nearVertexId<-getVertexId(AFMImage,x+1,y) 
  if ((nearVertexId>0)&(currentH<=AFMImage@data$h[nearVertexId])) return(TRUE)
  
  #x+1 y+1
  nearVertexId<-getVertexId(AFMImage,x+1,y+1) 
  if ((nearVertexId>0)&(currentH<=AFMImage@data$h[nearVertexId])) return(TRUE)
  
  #x y+1
  nearVertexId<-getVertexId(AFMImage,x,y+1) 
  if ((nearVertexId>0)&(currentH<=AFMImage@data$h[nearVertexId])) return(TRUE)
  
  #x-1 y+1
  nearVertexId<-getVertexId(AFMImage,x-1,y+1) 
  if ((nearVertexId>0)&(currentH<=AFMImage@data$h[nearVertexId])) return(TRUE)
  
  #x-1 y
  nearVertexId<-getVertexId(AFMImage,x-1,y) 
  if ((nearVertexId>0)&(currentH<=AFMImage@data$h[nearVertexId])) return(TRUE)
  
  #x-1 y-1
  nearVertexId<-getVertexId(AFMImage,x-1,y-1) 
  if ((nearVertexId>0)&(currentH<=AFMImage@data$h[nearVertexId])) return(TRUE)
  
  #x y-1
  nearVertexId<-getVertexId(AFMImage,x,y-1) 
  if ((nearVertexId>0)&(currentH<=AFMImage@data$h[nearVertexId])) return(TRUE)
  
  #x+1 y-1
  nearVertexId<-getVertexId(AFMImage,x+1,y-1) 
  if ((nearVertexId>0)&(currentH<=AFMImage@data$h[nearVertexId])) return(TRUE)
  
  return(FALSE)
}

#' gridIgraphPlot
#'
#' \code{gridIgraphPlot} return TRUE if vertex is adjacent to a better vertex
#' 
#' @param AFMImage an \code{\link{AFMImage}} from Atomic Force Microscopy
#' @param g the networks
#' @author M.Beauvais
#' @export
gridIgraphPlot<-function(AFMImage, g){
  # define the layout matrix
  coordinatesVector<-getNetworkGridLayout(AFMImage, V(g)$name)
  #coordinatesVector
  
  l<-matrix(coordinatesVector$y ,byrow = TRUE)
  l<-cbind(l, coordinatesVector$x)
  #l
  
  # plot(all, layout=All_layout, vertex.size=2, vertex.label=V(All)$name,
  #      vertex.color="green", vertex.frame.color="red", edge.color="grey",  
  #      edge.arrow.size=0.01, rescale=TRUE,vertex.label=NA, vertex.label.dist=0.0,
  #      vertex.label.cex=0.5, add=FALSE,   vertex.label.font=.001)
  plot(g, layout=l, 
       vertex.shape="circle", vertex.size=2, vertex.label=NA, vertex.color="red", vertex.frame.color="red",
       edge.color="grey"
  )
  
}

#' Calculate iGraph from AFMImage
#'
#' \code{calculateIgraph} return 
#' 
#' @param AFMImage an \code{\link{AFMImage}} from Atomic Force Microscopy
#' @param AFMImageNetworksAnalysis an \code{\link{AFMImageNetworksAnalysis}} from Atomic Force Microscopy
#' @author M.Beauvais
#' @export
calculateIgraph<-function(AFMImage, AFMImageNetworksAnalysis) {
  if (missing(AFMImageNetworksAnalysis)) {
    AFMImageNetworksAnalysis<-NULL
  }
  graphicalUpdate<-FALSE
  graphicalCounter<-0
  
  if (!is.null(AFMImageNetworksAnalysis)&&
      !is.null(AFMImageNetworksAnalysis@updateProgress)&&
      is.function(AFMImageNetworksAnalysis@updateProgress)&&
      !is.null(AFMImageNetworksAnalysis@updateProgress())) {
    graphicalUpdate<-TRUE
    totalLength<-AFMImage@samplesperline*(AFMImage@lines-1)
  }
  
  if (graphicalUpdate) {
    AFMImageNetworksAnalysis@updateProgress(message="1/2 - Generating edges list", value=0)
  }
  print(paste("Generating edge list"))
  
  counter<-1
  #edgeList=data.table()  
  edgeList <- vector("list", AFMImage@samplesperline*AFMImage@lines+1)
  
  for (x in seq(1: AFMImage@samplesperline)) {
    for (y in seq(1: (AFMImage@lines-1))) {
      currentVertexId<-getVertexId(AFMImage,x,y)
      if (existsEdge(AFMImage, currentVertexId)) {
        #edgeList<-rbind(edgeList, getSurroundingVertexesList(AFMImage,x,y))
        edgeList[[counter]] <- getSurroundingVertexesList(AFMImage,x,y)
        counter<-counter+1
      }
      if (graphicalUpdate) {
        graphicalCounter<-graphicalCounter+1
        if (graphicalCounter/100==floor(graphicalCounter/100)) {
          value<-graphicalCounter / totalLength
          text <- paste0(round(graphicalCounter, 2),"/",totalLength)
          AFMImageNetworksAnalysis@updateProgress(value= 0, detail = text)
        }
      }
    }
  }
  
  if (graphicalUpdate) {
    AFMImageNetworksAnalysis@updateProgress(message="2/2 - Generating network", value=0)
  }
  
  newEdgeList<-rbindlist(edgeList)
  el=as.matrix(newEdgeList)
  print(paste("Creating graph"))
  g<-graph_from_edgelist(el[,1:2], directed=FALSE)
  print(paste("Created",counter,"vertices"))
  AFMImageNetworksAnalysis@originalGraph<-g
  return(g)
}

#' getListOfDiameters
#'
#' \code{getListOfDiameters} return 
#' 
#' @param g list of igraph networks
#' @author M.Beauvais
#' @export
getListOfDiameters<-function(g) {
  LIST_OF_DIAMETERS = c()
  listOfGraph=decompose(g)
  for(g in listOfGraph){
    LIST_OF_DIAMETERS=c(LIST_OF_DIAMETERS, diameter(g, directed = FALSE, unconnected = TRUE, weights = NULL))
  }
  return(LIST_OF_DIAMETERS)  
}

#' canBeRemoved
#'
#' \code{canBeRemoved} return 
#' 
#' @param vertexId a vertex id
#' @param g a igraph
#' @param allVertices list of all vertices
#' @param DEGREE_LIMIT_FOR_CANDIDATE_VERTICE degree
#' 
#' @author M.Beauvais
#' @export
canBeRemoved<-function(vertexId, g, allVertices, DEGREE_LIMIT_FOR_CANDIDATE_VERTICE) {
  avList<-adjacent_vertices(g, v=c(vertexId), mode = c("all"))
  avListNew<-unique(avList[[vertexId]]$name)
  found<-NULL
  if (nrow(allVertices[, c("found"):=vertexId %in% avListNew & degree<(DEGREE_LIMIT_FOR_CANDIDATE_VERTICE+1)][found==TRUE])>0) {
    return(FALSE)
  }else{
    return(TRUE)
  }
  
}

#' calculateNetworkSkeleton
#'
#' \code{calculateNetworkSkeleton} return 
#' 
#' @param AFMImage an \code{\link{AFMImage}} from Atomic Force Microscopy
#' @param AFMImageNetworksAnalysis an \code{\link{AFMImageNetworksAnalysis}} from Atomic Force Microscopy
#' @author M.Beauvais
#' @export
calculateNetworkSkeleton<-function(AFMImage, AFMImageNetworksAnalysis) {
  if (missing(AFMImageNetworksAnalysis)) {
    AFMImageNetworksAnalysis<-NULL
    return(new("list"))
  }
  
  g<-AFMImageNetworksAnalysis@originalGraph
  
  graphicalUpdate<-FALSE
  graphicalCounter<-0
  
  if (!is.null(AFMImageNetworksAnalysis)&&
      !is.null(AFMImageNetworksAnalysis@updateProgress)&&
      is.function(AFMImageNetworksAnalysis@updateProgress)&&
      !is.null(AFMImageNetworksAnalysis@updateProgress())) {
    graphicalUpdate<-TRUE
    totalLength<-length(V(g))
    
  }
  
  
  DEGREE_LIMIT_FOR_CANDIDATE_VERTICE=4
  NUMBER_OF_NETWORKS = length(decompose(g))
  LIST_OF_DIAMETERS<-getListOfDiameters(g)
  print(LIST_OF_DIAMETERS)  
  
  
  #   distance_table(g, directed = FALSE)
  #   coreness(g)
  
  
  verticesThatCantBeRemovedList=c()
  print(paste("starting with ", length(V(g)), " vertices"))
  
  if (graphicalUpdate) {
    AFMImageNetworksAnalysis@updateProgress(message="1/1 - removing vertices and edges", value=0)
  }
  
  continueExploration<-TRUE
  while(continueExploration) {
    
    edgeList<-V(g)$name
    
    uniqueVerticesList<-unique(edgeList)
    uniqueVerticesList
    # degree de chaque noeud
    edgeDegreeList<-degree(g, v=uniqueVerticesList, mode = c("all"), loops = FALSE, normalized = FALSE)
    edgeDegreeList
    
    # liste ordonn'e9e croissante des noeuds en fonction du degree
    
    allVertices<-data.table(vertexId=uniqueVerticesList, degree=edgeDegreeList)
    # get-list of adjacent vertices with degree > 2 (can't remove if degree < 2)
    allVertices<-allVertices[order(degree)]
    listOfCandidateVertices<-allVertices[degree>DEGREE_LIMIT_FOR_CANDIDATE_VERTICE]
    
    listOfCandidateVertices<-listOfCandidateVertices[!listOfCandidateVertices$vertexId %in% verticesThatCantBeRemovedList]
    
    continueExploration<-FALSE
    if (nrow(listOfCandidateVertices)>0) {
      
      #             res<-sapply(listOfCandidateVertices$vertexId, canBeRemoved, g=g, allVertices=allVertices, simplify=F)
      #             vMatrix<-as.matrix(res, ncol=2)
      #             
      #             verticesToBeRemoved<-data.table(vertexId= rownames(vMatrix), toBeRemoved= vMatrix[,1])[toBeRemoved==TRUE]$vertexId
      #             print(paste("to be removed",verticesToBeRemoved))
      #             
      #             if (length(verticesToBeRemoved)>0) {
      #               g<-delete_vertices(g, c(verticesToBeRemoved))
      #               #continueExploration<-TRUE
      #               continueExploration<-continueExploration+1
      #             }
      #       
      for (vi in seq(1:nrow(listOfCandidateVertices))){
        onevertexId=listOfCandidateVertices$vertexId[vi]
        if (canBeRemoved(onevertexId, g=g, allVertices=allVertices, DEGREE_LIMIT_FOR_CANDIDATE_VERTICE=DEGREE_LIMIT_FOR_CANDIDATE_VERTICE)) {
          vId<-listOfCandidateVertices$vertexId[vi]
          
          # store the list of adjacent vertices of the node before deleting it
          avList<-unique(adjacent_vertices(g, v=c(vId), mode = c("all"))[[vId]]$name)
          
          
          g<-delete_vertices(g, listOfCandidateVertices$vertexId[vi])
          continueExploration<-TRUE
          
          NEW_LIST_OF_DIAMETERS=getListOfDiameters(g)
          #print(NEW_LIST_OF_DIAMETERS)  
          
          # did the vertex removal split the network or diminish the diameter
          if ((length(decompose(g))>NUMBER_OF_NETWORKS)||(!identical(LIST_OF_DIAMETERS,NEW_LIST_OF_DIAMETERS))) {
            print (paste("should not have removed", vId))
            verticesThatCantBeRemovedList=c(verticesThatCantBeRemovedList, listOfCandidateVertices$vertexId[vi])
            
            g<-g+vertices(as.numeric(vId))
            
            listOfEdges=c()
            for(j in seq(1,length(avList))) {
              listOfEdges=c(listOfEdges, vId, avList[j], avList[j],vId)
            }
            g<-g+edges(listOfEdges)
          }else{
            print("61")
            NEW_LIST_OF_DIAMETERS=getListOfDiameters(g)
            if ((!identical(LIST_OF_DIAMETERS,NEW_LIST_OF_DIAMETERS))) {
              print (paste("should not have removed", vId))
              verticesThatCantBeRemovedList=c(verticesThatCantBeRemovedList, listOfCandidateVertices$vertexId[vi])
              
              g<-g+vertices(as.numeric(vId))
              
              listOfEdges=c()
              for(j in seq(1,length(avList))) {
                listOfEdges=c(listOfEdges, vId, avList[j], avList[j],vId)
              }
              
              g<-g+edges(listOfEdges)
            }
            break
          }
        }
        
      }
      if (graphicalUpdate) {
        graphicalCounter<-graphicalCounter+1
        value<-graphicalCounter / totalLength
        text <- paste0(round(graphicalCounter, 2),"/",totalLength)
        AFMImageNetworksAnalysis@updateProgress(value= 0, detail = text)
      }
      
    }else{
      continueExploration<-FALSE
    }
  }
  print(paste("ending with ", length(V(g)), " vertices"))
  
  AFMImageNetworksAnalysis@skeletonGraph<-g
  
  return(AFMImageNetworksAnalysis)
}

#' Calculate topology image (TBC)
#'
#' \code{getTopologyAFMImage} return the global topological distance
#' 
#' @param BinaryAFMImage an \code{\link{AFMImage}} from Atomic Force Microscopy in a binary format 0 or 1 values for heigths
#' @param AFMImageNetworksAnalysis an \code{\link{AFMImageNetworksAnalysis}} from Atomic Force Microscopy
#' @author M.Beauvais
#' @export
getTopologyAFMImage<-function(BinaryAFMImage, AFMImageNetworksAnalysis){
  
  filterVector<-unlist(BinaryAFMImage@data$h)
  
  topology<-c()
  
  
  for (x in 1:BinaryAFMImage@samplesperline) {
    for (y in 1:BinaryAFMImage@lines) {
      if(x==1) {
        bX=seq(from=0, to=BinaryAFMImage@samplesperline-1, by=1)
      }else{
        if (x==BinaryAFMImage@samplesperline) {
          bX=seq(from=x-1, to=0, by=-1)
        }else{
          bX=seq(from=x-1, to=0, by=-1)
          bX=c(bX, seq(from=1, to=BinaryAFMImage@samplesperline-x, by=1))
        }
      }
      # bX
      
      if(y==1) {
        bY=seq(from=0, to=BinaryAFMImage@lines-1, by=1)
      }else{
        if (y==BinaryAFMImage@lines) {
          bY=seq(from=y-1, to=0, by=-1)
        }else{
          bY=seq(from=y-1, to=0, by=-1)
          bY=c(bY, seq(from=1, to=BinaryAFMImage@lines-y, by=1))
        }
      }
      # bY
      
      
      bX=BinaryAFMImage@hscansize*bX
      bY=BinaryAFMImage@vscansize*bY
      
      bX<-matrix(rep(bX,BinaryAFMImage@lines), ncol=BinaryAFMImage@lines, byrow=TRUE )
      bY<-matrix(rep(bY,BinaryAFMImage@samplesperline), ncol=BinaryAFMImage@samplesperline, byrow=FALSE )
      
      nm=as.numeric(1/sqrt(bX^2+bY^2))
      nm[is.infinite(nm)]<-0
      #nm*filterVector
      res<-sum(nm*filterVector)
      topology<-c(topology,res)
      #print(res)
      
    }
  }
  
  
  scanby<-BinaryAFMImage@scansize/BinaryAFMImage@samplesperline
  endScan<-BinaryAFMImage@scansize*(1-1/BinaryAFMImage@samplesperline)
  
  topologyAFMImage<-AFMImage(
    data = data.table(x = rep(seq(0,endScan, by= scanby), times = BinaryAFMImage@lines),
                      y = rep(seq(0,endScan, by= scanby), each = BinaryAFMImage@samplesperline),
                      h = topology),
    samplesperline = BinaryAFMImage@samplesperline, lines = BinaryAFMImage@lines,
    vscansize = BinaryAFMImage@vscansize, hscansize = BinaryAFMImage@hscansize, scansize = BinaryAFMImage@scansize,
    fullfilename = BinaryAFMImage@fullfilename )
  
  
  
  return(topologyAFMImage)
  
}

#' get a segment of points thanks to Bresenham line algorithm
#'
#' \code{getBresenham2DSegment} return the Bresenham segment in 2D from extremities coordinates
#' 
#' @param x1 abscissa coordinates of the first point
#' @param y1 ordinate coordinates of the first point
#' @param x2 abscissa coordinates of the second point
#' @param y2 ordinate coordinates of the second point
#' @return a data.table of points - data.table(x, y)
#' @author M.Beauvais
#' @export
getBresenham2DSegment<-function(x1, y1, x2, y2) {
  resX=c()
  resY=c()
  
  dx<-x2-x1
  dy<-y2-y1
  
  #print(paste("getBresenham2DSegment",dx,dy))
  
  if (dx !=0) {
    if (dx > 0) {
      if (dy !=0) {
        if (dy > 0) {
          if (dx >= dy) {
            e<-dx
            dx <- e  * 2 
            dy <- dy * 2  
            while(TRUE){
              resX=c(resX,x1); resY=c(resY, y1)
              x1 <- x1 + 1
              if (x1 == x2) break
              e <- e - dy
              if (e < 0) {
                y1 <- y1 + 1
                e <- e + dx 
              }
            }
          } else {
            e <- dy
            dy <- e * 2
            dx <- dx * 2 
            while(TRUE){ 
              resX=c(resX,x1); resY=c(resY, y1)
              y1 <- y1 + 1
              if (y1 == y2) break
              e <- e - dx
              if (e < 0) {
                x1 <- x1 + 1 
                e <- e + dy
              }
            }
          }
        }else if (dy < 0){ # dy < 0 (et dx > 0)
          
          
          if (dx >= -dy) {
            e <- dx
            dx <- e * 2
            dy <- dy * 2
            while(TRUE){  
              resX=c(resX,x1); resY=c(resY, y1)
              x1 <- x1 + 1
              if (x1 == x2) break
              e <- e + dy
              if (e < 0) {
                y1 <- y1 - 1 
                e <- e + dx
              }
            }
          } else{
            e <- dy
            dy <- e * 2 
            dx <- dx * 2
            #print(c(e,dy,dx))
            while(TRUE){  
              resX=c(resX,x1); resY=c(resY, y1)
              #print(c(x1, y1))
              y1 <- y1 - 1
              if (y1 == y2) break
              e <- e - dx
              #print(paste(c("e",e)))
              if (e < 0) {
                x1 <- x1 + 1
                if(x1>x2) x1=x2 # MB !!!
                e <- e - dy
                #print(paste(c("e",e)))
              }
            }
          }
          
        }
      }  else if (dy == 0){ # dy = 0 (et dx > 0)
        while(x1 != x2) {
          resX=c(resX,x1); resY=c(resY, y1) 
          x1 <- x1 + 1
        }
      }
    }else if (dx<0) {  # dx < 0
      dy <- y2 - y1
      if (dy != 0) {
        if (dy > 0) {
          if (-dx >= dy) {
            e <- dx
            dx <- e * 2 
            dy <- dy * 2  
            while(TRUE){
              resX=c(resX,x1); resY=c(resY, y1) 
              x1 <- x1 - 1
              if (x1 == x2) break
              e <- e + dy
              if (e >= 0) {
                y1 <- y1 + 1 
                e <- e + dx 
              }
            }
          }else{
            e <- dy
            dy <- e * 2
            dx <- dx * 2 
            while(TRUE){ 
              resX=c(resX,x1); resY=c(resY, y1) 
              y1 <- y1 + 1
              if ( y1 == y2) break 
              e <- e + dx
              if (e <= 0) {
                x1 <- x1 - 1  
                e <- e + dy 
              }
            }
          }
        }else if(dy <0) {  # dy < 0 (et dx < 0)
          if (dx <= dy) {
            e <- dx
            dx <- e * 2 
            dy <- dy * 2  
            while(TRUE){  
              resX=c(resX,x1); resY=c(resY, y1)
              x1 <- x1 - 1
              if (x1 == x2) break
              e <- e - dy
              if (e >= 0) {
                y1 <- y1 - 1
                e <- e + dx 
              }
            }
          } else { 
            e <- dy
            dy <- e * 2 
            dx <- dx * 2 
            
            while(TRUE){
              resX=c(resX,x1); resY=c(resY, y1)
              y1 <- y1 - 1
              if ( y1 == y2 ) break
              e <- e - dx
              if (e >= 0) {
                x1 <- x1 - 1
                e <- e + dy
              }
            }
          }
        } 
      } else if (dy==0) {  # dy = 0 (et dx < 0)
        while(x1!=x2) {
          resX=c(resX,x1); resY=c(resY, y1)
          x1 <- x1 - 1
        }
      }
    }
  } else if (dx==0) {  # dx = 0
    dy <- y2 - y1
    if (dy != 0) {
      if (dy > 0) {
        while(y1 != y2) {
          resX=c(resX,x1); resY=c(resY, y1)
          y1 <- y1 + 1
        } 
        
      } else if (dy < 0) { # dy < 0 (et dx = 0)
        while(y1!=y2) {
          resX=c(resX,x1); resY=c(resY, y1)
          y1 <- y1 - 1
        }
        
      }
      
    }
    
  }
  resX=c(resX,x2); resY=c(resY, y2)
  pts = data.table(x=resX, y=resY)
  
  return(pts)
}


#' thin an Image in matrix format
#' 
#' @param imageMatrix a matrix of an AFM image
#' @export
#' @author M.Beauvais
thinImage <- function(imageMatrix)
{
  absDiff <- function(matrix1,matrix2)
  {
    r <- nrow(matrix1)
    c <- ncol(matrix1)
    destMatrix <- matrix1
    for(r in 0:r-1)
    {
      for(c in 0:c-1)
      {
        destMatrix[r,c] <- abs(matrix1[r,c]-matrix1[r,c])
      }
    }
    return(destMatrix)
  }
  
  countNonZero <- function(inputMatrix)
  {
    return(length(inputMatrix[inputMatrix > 0]))
  }
  
  thinningIteration <- function(imageMatrix, iter)
  {
    imageInput <- imageMatrix
    r <- nrow(imageInput) - 1
    c <- ncol(imageInput) - 1
    for(i in 2:r)
    {
      for(j in 2:c)
      {
        p2 <- imageInput[i-1, j]
        p3 <- imageInput[i-1, j+1]
        p4 <- imageInput[i, j+1]
        p5 <- imageInput[i+1, j+1]
        p6 <- imageInput[i+1, j]
        p7 <- imageInput[i+1, j-1]
        p8 <- imageInput[i, j-1]
        p9 <- imageInput[i-1, j-1]
        A  <- (p2 == 0 && p3 == 1) + (p3 == 0 && p4 == 1) + 
          (p4 == 0 && p5 == 1) + (p5 == 0 && p6 == 1) + 
          (p6 == 0 && p7 == 1) + (p7 == 0 && p8 == 1) +
          (p8 == 0 && p9 == 1) + (p9 == 0 && p2 == 1)
        B  <- p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9
        if(iter == 0){
          m1 <- (p2 * p4 * p6)
          m2 <- (p4 * p6 * p8)
        }
        else {
          m1 <- (p2 * p4 * p8)
          m2 <- (p2 * p6 * p8)
        }
        if (A == 1 && (B >= 2 && B <= 6) && m1 == 0 && m2 == 0)
        {
          imageInput[i,j] <- 0
        }
      }
    }
    return(imageInput)
  }
  
  im <- imageMatrix
  prev <- im
  repeat {
    im <- thinningIteration(im, 0)
    im <- thinningIteration(im, 1)
    diff <- absDiff(im, prev)
    prev <- im
    if(countNonZero(diff) <= 0)
    {
      break
    }
  } 
  
  return(im)
}

#' identify largest circles in binary image
#'
#' \code{identifyNodesWithCircles} return TRUE if vertex is adjacent to a better vertex
#' 
#' @param AFMImageNetworksAnalysis a \code{\link{AFMImageNetworksAnalysis}}
#' @param ... cl: a cluster object from the parallel package
#' @return AFMImageNetworksAnalysis the \code{\link{AFMImageNetworksAnalysis}} instance
#' @author M.Beauvais
#' @export
identifyNodesWithCircles<-function(...,AFMImageNetworksAnalysis) {
  force(AFMImageNetworksAnalysis)
  
  BIGGER_CIRCLE_RADIUS<-4
  BIGGER_CIRCLE_RADIUS_MULTILPLIER<-2
  SAMPLE_ON_THIN_PORTIONS<-25 # percents
  MAX_DISTANCE<-64
  AREA_MIN <-25
  CLUSTER_COUNT_MIN <- 45
  CIRCLE_RADIUS_INIT <- 25
  
  cluster<-node<-mindist<-maxdist<-keep<-NULL
  nbOfCircles<-maxArea<-h<-NULL
  clusterLon<-clusterLat<-cluster<-IDX<-keepThinPoints<-meandist<-NULL
  #cl<-cl
  #spDistsN1<-nbOfCircles<-maxArea<-h<-NULL
  
  args<-names(list(...))
  print(args)
  if (is.null(args)) {
    clExist<-FALSE
  }else{
    clExist<-c(match('cl',args)!=-1)  
    cl<-cl
  }
  
  
  if (clExist) {
    print("using parallel")
    requireNamespace("parallel")
  }
  
  newCircleAFMImage<-copy(AFMImageNetworksAnalysis@binaryAFMImage)
  newCircleAFMImage2<-copy(AFMImageNetworksAnalysis@binaryAFMImage)
  circleRadius<-CIRCLE_RADIUS_INIT
  iteration<-0
  rm(avgDT)
  
  while(circleRadius>0) {
    
    iteration=iteration+1
    circleRadius=circleRadius-1
    blockSize<-circleRadius*2+1
    
    print(paste0("circleRadius:",circleRadius))
    
    if ((blockSize>newCircleAFMImage@samplesperline)|((blockSize-1)>newCircleAFMImage@lines)) {
      print(paste0("too big blockSize", blockSize))
    }else{
      
      if (circleRadius>0) {
        circleCenter<-c(circleRadius, circleRadius)
        
        circlePts = SpatialPoints(cbind(rep(1:(blockSize),blockSize), rep(1:(blockSize),1,each= blockSize)))
        # pts
        circlenm <- sp::spDistsN1(pts=circlePts, pt=circleCenter, longlat=FALSE)
        # nm
        # nm<circleRadius
        
        # find all blocks in image
        # and check if the circle with biggest radius inside the block exists in the image
        # if yes, set all the height of all the points inside circle to 10
        binaryAFMImageMatrix<-matrix(newCircleAFMImage@data$h, ncol=newCircleAFMImage@samplesperline)
        newBlockAFMImageMatrix<-matrix(newCircleAFMImage@data$h, ncol=newCircleAFMImage@samplesperline)
        
        allXY<-expand.grid(1:(newCircleAFMImage@samplesperline-blockSize), 1:(newCircleAFMImage@lines-blockSize))
        orderXY<-sample.int(nrow(allXY), nrow(allXY), replace = FALSE)
        allXY<-data.table(allXY)
        colnames(allXY)<-c("x","y")
        # for (x in seq(1:(newCircleAFMImage@samplesperline-blockSize))) {
        #   for (y in seq(1:(newCircleAFMImage@lines-blockSize))) {
        
        for (indexXY in seq(1:length(orderXY))) {
          
          x<-allXY[orderXY[indexXY], ]$x
          y<-allXY[orderXY[indexXY], ]$y
          #print(paste(x,y))
          #heights<-newCircleAFMImage@data$h
          tempMatrix<-binaryAFMImageMatrix[x:(x+blockSize),y:(y+blockSize)]
          
          
          if ((!anyNA(as.vector(tempMatrix)[circlenm<=circleRadius]))&
              (all(as.vector(tempMatrix)[circlenm<=circleRadius] == 1) == TRUE)) {
            
            print (paste(x,y))
            
            newBlockAFMImageMatrix[x+circleRadius, y+circleRadius]<-10
            
            newBlockAFMImage<-copy(newCircleAFMImage)
            newBlockAFMImage@data$h<-as.vector(newBlockAFMImageMatrix)
            #displayIn3D(newBlockAFMImage, noLight=TRUE)
            
            newBlockAFMImage2<-copy(newBlockAFMImage)
            newBlockAFMImage2@data$h[newBlockAFMImage2@data$h<3]<-0
            #displayIn3D(newBlockAFMImage2, noLight=TRUE)
            
            newBlockAFMImageMatrix2<-matrix(newBlockAFMImage2@data$h, ncol=newBlockAFMImage2@samplesperline)
            # get coordinates of non 0 elements
            nonZeroElements<-which(newBlockAFMImageMatrix2!=0,arr.ind = T)
            
            #print(paste("circleRadius",circleRadius))
            
            lat<-nonZeroElements[,1]
            lon<-nonZeroElements[,2]
            
            nodesToBeRemoved=data.table(lon,lat,circleRadius)
            
            
            #print(nodesToBeRemoved)        
            newCircleAFMImage@data$h[nodesToBeRemoved$lon+1+nodesToBeRemoved$lat*newCircleAFMImage@samplesperline]<-0
            
            for(oneCenter in seq(1, nrow(nodesToBeRemoved))) {
              center<-c(nodesToBeRemoved[oneCenter,]$lat, nodesToBeRemoved[oneCenter,]$lon)
              
              
              # Use a bigger circle that will be removed from image
              # in order to exclude other nodes that could be very near
              circleRadius2=circleRadius+BIGGER_CIRCLE_RADIUS
              #circleRadius2=circleRadius
              blockSize2=circleRadius2*BIGGER_CIRCLE_RADIUS_MULTILPLIER+1
              
              
              pts = SpatialPoints(cbind(rep(0:(blockSize2-1),blockSize2)+center[1]-circleRadius2, rep(0:(blockSize2-1),1,each= blockSize2)+center[2]-circleRadius2))
              pts<-pts[pts$coords.x1>0&pts$coords.x1<newCircleAFMImage2@lines&pts$coords.x2>0&pts$coords.x2<newCircleAFMImage2@samplesperline]
              
              nm <- sp::spDistsN1(pts=pts, pt=center, longlat=FALSE)
              # points that are inside the circle
              listOfPointsInsideCircle<-pts[nm<=circleRadius2]
              newCircleAFMImage@data$h[listOfPointsInsideCircle$coords.x1+1+(listOfPointsInsideCircle$coords.x2)*newCircleAFMImage@samplesperline]<-0
              #displayIn3D(newCircleAFMImage, noLight=TRUE)
            }
            binaryAFMImageMatrix<-matrix(newCircleAFMImage@data$h, ncol=newCircleAFMImage@samplesperline)
            newBlockAFMImageMatrix<-matrix(newCircleAFMImage@data$h, ncol=newCircleAFMImage@samplesperline)
            # displayIn3D(newCircleAFMImage2, noLight=TRUE)
            
            if (!exists("avgDT")) {
              avgDT<-nodesToBeRemoved
              avgDT2<-copy(avgDT)
            }else{
              avgDT2<-nodesToBeRemoved
              avgDT<-rbind(avgDT,avgDT2)
            }
            #print(avgDT2)
            #print(paste("circleRadius=",circleRadius,"- nb of centers",nrow(avgDT2)))
            for(oneCenter in seq(1, nrow(avgDT2))) {
              center<-c(avgDT2[oneCenter,]$lat, avgDT2[oneCenter,]$lon)
              #center<-c(0,0)
              #pts = SpatialPoints(cbind(rep(1:blockSize,blockSize)+center[1]-circleRadius, rep(1:blockSize,1,each= blockSize)+center[2]-circleRadius))
              pts = SpatialPoints(cbind(rep(0:(blockSize2-1),blockSize2)+center[1]-circleRadius2, rep(0:(blockSize2-1),1,each= blockSize2)+center[2]-circleRadius2))
              pts<-pts[pts$coords.x1>0&pts$coords.x1<newCircleAFMImage2@lines&pts$coords.x2>0&pts$coords.x2<newCircleAFMImage2@samplesperline]
              nm <- sp::spDistsN1(pts=pts, pt=center, longlat=FALSE)
              # points that are inside the circle
              listOfPointsInsideCircle<-pts[nm<=circleRadius]
              #print(listOfPointsInsideCircle$coords.x1)
              newCircleAFMImage2@data$h[listOfPointsInsideCircle$coords.x1+1+(listOfPointsInsideCircle$coords.x2)*newCircleAFMImage2@samplesperline]<-newCircleAFMImage2@samplesperline+iteration*10
            }
          }
        }
      }
    }
  }
  
  # displayIn3D(newCircleAFMImage2, noLight=TRUE)
  # displayIn3D(newCircleAFMImage, noLight=TRUE)
  
  if (AFMImageNetworksAnalysis@smallBranchesTreatment) {
    
    # finding the extra small nodes
    untreatedPoints<-newCircleAFMImage@data[h!=0]
    
    islandsDT<-cbind(lon=untreatedPoints$y*newCircleAFMImage@lines/newCircleAFMImage@vscansize, lat=untreatedPoints$x*newCircleAFMImage@samplesperline/newCircleAFMImage@hscansize)
    
    DBSCAN <- dbscan(islandsDT, eps = 1.5, MinPts = 3, borderPoints=FALSE)
    #plot(untreatedPoints$y, untreatedPoints$x, col = DBSCAN$cluster, pch = 20)
    #plot(islandsDT, col = DBSCAN$cluster, pch = 20)
    
    islandsDT<-data.table(islandsDT,cluster=DBSCAN$cluster)
    setkeyv(islandsDT, "cluster")
    
    isolatedIslandsDT<-islandsDT[cluster==0,]
    isolatedIslandsDT
    islandsDT<-islandsDT[cluster!=0,]
    islandsDT
    
    identifyLinksBetweenClustersAndExistingNodes<-function(clusterN, AFMImageNetworksAnalysis, MAX_DISTANCE, avgDT, islandsDT) {
      requireNamespace("data.table")
      requireNamespace("sp")
      requireNamespace("AFM")
      
      clusterLon<-clusterLat<-cluster<-IDX<-keepThinPoints<-meandist<-NULL
      
      print(clusterN)
      resDT<-data.table(cluster=c(0), clusterLon=c(0), clusterLat=c(0), existingNodeLon=c(0), existingNodeLat=c(0))
      centers1<-islandsDT[islandsDT$cluster %in% clusterN,]
      
      
      # define the points in the circle
      otherNodes<-copy(avgDT)
      
      for (center_index in seq(1,nrow(centers1))) {
        center1<-centers1[center_index,]
        circleRadius1<-1
        #otherNodes<-allNodesAsSpatialPoints[!(allNodesAsSpatialPoints$coords.x1==center1$lon&allNodesAsSpatialPoints$coords.x2==center1$lat)]
        
        minLat<- ifelse((center1$lat-MAX_DISTANCE)>0, center1$lat-MAX_DISTANCE, 0)
        maxLat<- ifelse((center1$lat+MAX_DISTANCE)<AFMImageNetworksAnalysis@binaryAFMImage@lines, center1$lat+MAX_DISTANCE, AFMImageNetworksAnalysis@binaryAFMImage@lines-1)
        
        minLon<- ifelse((center1$lon-MAX_DISTANCE)>0, center1$lon-MAX_DISTANCE, 0)
        maxLon<- ifelse((center1$lon+MAX_DISTANCE)<AFMImageNetworksAnalysis@binaryAFMImage@samplesperline, center1$lon+MAX_DISTANCE, AFMImageNetworksAnalysis@binaryAFMImage@samplesperline-1)
        
        otherNodes2<-copy(avgDT[lon>=minLon&lon<=maxLon&lat>=minLat&lat<=maxLat,])
        otherNodes2$dist<-sp::spDistsN1(pts=matrix(c(otherNodes2$lon, otherNodes2$lat), ncol=2), pt=c(center1$lon, center1$lat), longlat=FALSE)
        #otherNodes
        
        otherNodes2<-otherNodes2[with(otherNodes2, order(otherNodes2$dist)), ]
        otherNodes2<-otherNodes2[otherNodes2$dist<MAX_DISTANCE,]
        
        if (nrow(otherNodes2)>0) {
          for (centerId2Nb in seq(1, nrow(otherNodes2))) {
            pt<-otherNodes2[centerId2Nb,]
            if (AreNodesConnected(AFMImageNetworksAnalysis@binaryAFMImage, center1, circleRadius1, data.table(lon=pt$lon, lat=pt$lat), pt$circleRadius)) {
              #print("yes")
              resDT=rbind(resDT, data.table(cluster=clusterN, clusterLon=center1$lon, clusterLat=center1$lat, existingNodeLon=pt$lon, existingNodeLat=pt$lat))
            }
          }
        }
      }
      resDT<-resDT[-1,]
      return(resDT)
    }
    
    print(paste("number of nodes=", nrow(avgDT)))
    print(paste("number of clusters=", length(unique(islandsDT$cluster))))
    
    
    if(exists("avgDT")) {
      
      start.time <- Sys.time()
      print(start.time)
      if(clExist) {
        parallel::clusterEvalQ(cl , c(library("data.table"),library("sp"), library("AFM")))
        parallel::clusterExport(cl, c("AFMImageNetworksAnalysis", "MAX_DISTANCE", "avgDT", "islandsDT"), envir=environment())
        res<-parallel::parLapply(cl, unique(islandsDT$cluster),identifyLinksBetweenClustersAndExistingNodes, AFMImageNetworksAnalysis, MAX_DISTANCE, avgDT, islandsDT)
      }else{
        res<-lapply(unique(islandsDT$cluster),identifyLinksBetweenClustersAndExistingNodes, AFMImageNetworksAnalysis, MAX_DISTANCE, avgDT, islandsDT)
      }
      end.time <- Sys.time()
      time.taken <- end.time - start.time
      print(paste0("time.taken: ",time.taken))
      
      
      resDT<-rbindlist(res)
      connectedIslandsDT<-islandsDT[cluster %in% unique(resDT$cluster),]
      
      # plot existing clusters
      #plot( connectedIslandsDT$lon, connectedIslandsDT$lat,col = connectedIslandsDT$cluster, pch = 20)
      #plot( islandsDT$lon, islandsDT$lat,col = islandsDT$cluster, pch = 20)
      
      # calculate distance between points in the cluster and existing nodes
      resDT$node<-paste0(resDT$existingNodeLon,"-",resDT$existingNodeLat)
      resDT
      resDT$dist<-sapply(1:nrow(resDT),function(i) sp::spDistsN1(pts=as.matrix(resDT[i,2:3,with=FALSE]),pt=as.matrix(resDT[i,4:5,with=FALSE]),longlat=FALSE))
      resDT
      
      # find the closest existing node
      resDT[,meandist:=mean(dist), by=list(cluster,node)]
      resDT[,mindist:=min(meandist), by=list(cluster)]
      setkey(resDT, meandist, mindist)
      resDT$keep<-sapply(1:nrow(resDT),function(i) if (resDT[i,8,with=FALSE] == resDT[i,9,with=FALSE]) return(TRUE) else return(FALSE))
      resDT
      
      
      
      #MB TODO
      # more in the width or in the height ?
      print("start spliting segment regularly...")
      clusterChar = data.table (
        cluster = unique(islandsDT$cluster),
        minLon = islandsDT[, min(lon), by=cluster]$V1,
        maxLon = islandsDT[, max(lon), by=cluster]$V1,
        minLat = islandsDT[, min(lat), by=cluster]$V1,
        maxLat = islandsDT[, max(lat), by=cluster]$V1)
      clusterChar$area<-(clusterChar$maxLat-clusterChar$minLat)*(clusterChar$maxLon-clusterChar$minLon)
      clusterChar$count<-islandsDT[,.N, by=cluster]$N
      
      clusterChar$shape<-sapply(1:nrow(clusterChar),function(i) {
        if ((clusterChar[i,]$maxLon-clusterChar[i,]$minLon)>(clusterChar[i,]$maxLat-clusterChar[i,]$minLat)) {
          return("width")
        }else{
          return("height")
        }
      })
      print(clusterChar)
      
      
      
      rm(resDT6)
      i=3
      # 30 % in regular space
      resDT6<-lapply(1:nrow(clusterChar),function(i) {
        
        if ((clusterChar[i,]$area <= AREA_MIN)|(clusterChar[i,]$count <= CLUSTER_COUNT_MIN)) {
          # if sample not extremely small
          if (!clusterChar[i,]$count <= 3) {
            # sample only one point if the cluster is with small area
            clusterN<-clusterChar[i,]$cluster
            resDT2<-islandsDT[cluster %in% clusterN,]
            #print(resDT2)
            sampleC<-sample(1:nrow(resDT2),1)
            return(data.table(cluster=clusterN, lon = resDT2[sampleC,]$lon, lat=  resDT2[sampleC,]$lat))
          }
        }else {
          # every 5 pixel
          if (clusterChar[i,]$shape == "height") {
            
            totalHeight<-clusterChar[i,]$maxLat-clusterChar[i,]$minLat
            #print(totalHeight)
            
            sSample = floor(SAMPLE_ON_THIN_PORTIONS*totalHeight/100)
            
            if (sSample>0) {
              if (sSample>5) sSample <-5
              latVector <- seq(from = clusterChar[i,]$minLat + 1, 
                               to = clusterChar[i,]$maxLat - 1,
                               by = sSample )
              latVector <- floor(latVector)
              #j=3
              resDT5<-lapply(1:length(latVector),function(j, clusterN = clusterChar[i,]$cluster) {
                resDT2<-islandsDT[lat %in% latVector[j] & cluster %in% clusterN,]
                #print(resDT2)
                avgDTLon<-floor(mean(resDT2$lon))
                #print(paste("cluster ", clusterN, "keep ",avgDTLon, latVector[j] ))
                return(data.table(cluster=clusterN, lon = avgDTLon, lat= latVector[j]))
              })
              
              print(resDT5)
              
              return(rbindlist(resDT5))
            }
          }else{
            totalWidth<-clusterChar[i,]$maxLon-clusterChar[i,]$minLon
            #print(totalHeight)
            
            sSample = floor(SAMPLE_ON_THIN_PORTIONS*totalWidth/100)
            
            if (sSample>0) {
              lonVector <- seq(from = clusterChar[i,]$minLon + 1, 
                               to = clusterChar[i,]$maxLon - 1,
                               by = sSample )
              lonVector <- floor(lonVector)
              #j=1
              resDT5<-lapply(1:length(lonVector),function(j, clusterN = clusterChar[i,]$cluster) {
                resDT2<-islandsDT[lon %in% lonVector[j] & cluster %in% clusterN,]
                #print(resDT2)
                avgDTLat<-floor(mean(resDT2$lat))
                #print(paste("cluster ", clusterN, "keep ",avgDTLon, lonVector[j] ))
                return(data.table(cluster=clusterN, lon = lonVector[j], lat= avgDTLat))
              }
              )
              print(resDT5)
              return(rbindlist(resDT5))
            }
          }
        }
      })
      resDT6<-rbindlist(resDT6)
      resDT6<-unique(resDT6)
      resDT6<-resDT6[complete.cases(resDT6),]
      
      resDT6<-resDT6[lon != 0 & lon != (AFMImageNetworksAnalysis@binaryAFMImage@samplesperline-1) & lat!=0 & lat!=(AFMImageNetworksAnalysis@binaryAFMImage@lines-1) ,]
      
      resDT6
      
      # good vizualisationof intermediary results
      
      # resDT7<-copy(resDT6)
      # resDT7$cluster<-rep(888, nrow(resDT7))
      # resDT7
      # islandsDT2<-rbind(islandsDT, resDT7)
      #plot( islandsDT2$lon, islandsDT2$lat,col = islandsDT2$cluster, pch = 20)
      #plot(untreatedPoints$y, untreatedPoints$x, col = DBSCAN$cluster, pch = 20)
      
      # get the list of closest existing nodes
      print("start defining the farthest points...")
      
      setkey(resDT, cluster)
      resDT2<-copy(unique(resDT))
      resDT2<-data.table(cluster=resDT2$cluster, existingNodeLon=resDT2$existingNodeLon,existingNodeLat=resDT2$existingNodeLat)
      setkey(resDT2, cluster)
      resDT2<-unique(resDT2)
      
      
      # get the list of farthest points from the existing nodes in previous list
      setkey(resDT2, cluster)
      setkey(connectedIslandsDT, cluster)
      print(connectedIslandsDT)
      print(unique(connectedIslandsDT))
      print(resDT2)
      connectedIslandsDT<-merge(connectedIslandsDT, resDT2, by="cluster", allow.cartesian=TRUE)
      connectedIslandsDT$dist<-sapply(1:nrow(connectedIslandsDT),function(i) sp::spDistsN1(pts=as.matrix(connectedIslandsDT[i,2:3,with=FALSE]),
                                                                                           pt=as.matrix(connectedIslandsDT[i,4:5,with=FALSE]),longlat=FALSE))
      connectedIslandsDT[,maxdist:=max(dist), by=list(cluster)]
      connectedIslandsDT$keep<-sapply(1:nrow(connectedIslandsDT),function(i) if (connectedIslandsDT[i,6,with=FALSE] == connectedIslandsDT[i,7,with=FALSE]) return(TRUE) else return(FALSE))
      connectedIslandsDT
      
      
      
      farthestPoints<-data.table(lon=connectedIslandsDT[keep %in% c(TRUE),]$lon,
                                 lat=connectedIslandsDT[keep %in% c(TRUE),]$lat,
                                 cluster=connectedIslandsDT[keep %in% c(TRUE),]$cluster,
                                 dist=connectedIslandsDT[keep %in% c(TRUE),]$dist)
      farthestPoints
      # if farthest nodes is not connected, create an intermediary node
      #TODO
      # compare distance of farthest nodes with distance of other node, if close to another node and connected, do not use the node
      sapply(1:nrow(resDT),function(i) if (resDT[i,8,with=FALSE] == resDT[i,9,with=FALSE]) return(TRUE) else return(FALSE))
      
      resDT$dist<-as.numeric(resDT$dist)
      farthestPoints$keep<-sapply(1:nrow(farthestPoints),function(i)  if (nrow(resDT[clusterLon %in% farthestPoints[i,1,with=FALSE]$lon & clusterLat %in% farthestPoints[i,2,with=FALSE]$lat & dist < (1.05*farthestPoints[i,4,with=FALSE]$dist),])>0) return(FALSE) else    return(TRUE))
      farthestPoints
      farthestPoints[keep %in% c(TRUE),]
      avgDT<-rbind(avgDT, data.table(lon=farthestPoints[keep %in% c(TRUE),]$lon, lat=farthestPoints[keep %in% c(TRUE),]$lat,circleRadius=rep(0, nrow(farthestPoints[keep %in% c(TRUE),]))))
      
      
      avgDT<-rbind(avgDT, data.table(lon = resDT6$lon, lat = resDT6$lat, circleRadius=rep(0, nrow(resDT6))))
      avgDT
      
      # start of thinning image      
      # # sapply(1:nrow(resDT6), function(i, resDT6) {
      # #   connectedIslandsDT[lon %in% c(resDT6[i,]$lon) & lat %in% c(resDT6[i,]$lat),]$keep<-TRUE
      # # }, resDT6)
      # 
      # 
      # # thin remain of image
      # print("thining islands")
      # # input from algorithm
      # connectedIslandsDT
      # 
      # #     plot( connectedIslandsDT$lon, connectedIslandsDT$lat,col = connectedIslandsDT$cluster, pch = 20)
      # #     displayIn3D(AFMImageNetworksAnalysis@binaryAFMImage, noLight=TRUE)
      # #     displayIn3D(AFMImageNetworksAnalysis@binaryAFMImageWithCircles, noLight=TRUE)
      # 
      # 
      # mtx <- matrix(0, nrow=AFMImageNetworksAnalysis@binaryAFMImage@lines, 
      #               ncol=AFMImageNetworksAnalysis@binaryAFMImage@samplesperline)
      # mtx[connectedIslandsDT$lat+1+AFMImageNetworksAnalysis@binaryAFMImage@samplesperline*connectedIslandsDT$lon]<-1
      # #mtx[islandsDT$lat+1+AFMImageNetworksAnalysis@binaryAFMImage@samplesperline*islandsDT$lon]<-1
      # islandsDT
      # # pimage(mtx)
      # 
      # 
      # singleImageMatrix<-matrix(AFMImageNetworksAnalysis@binaryAFMImage@data$h, ncol=AFMImageNetworksAnalysis@binaryAFMImage@samplesperline)
      # # #Display the binary image
      # # pimage(singleImageMatrix)
      # # pimage(thinImage(singleImageMatrix))
      # 
      # 
      # 
      # #Thin the image using our thinning library 
      # thin <- mtx
      # thin <- thinImage(mtx)
      # pimage(thin)
      # 
      # #Display the thinned image
      # # pimage(thin)
      # 
      # #
      # # keep only the points after thinning
      # 
      # thinPoints<-which(thin!=0,arr.ind = T)
      # 
      # connectedIslandsDT$thinPoints<-FALSE
      # connectedIslandsDT$keepThinPoints<-FALSE
      # 
      # 
      # apply(thinPoints, 1, function(x, connectedIslandsDT) {
      #   connectedIslandsDT[lat %in% c(x[1]-1) & lon %in% c(x[2]-1),thinPoints:= TRUE]
      # }, connectedIslandsDT)
      # 
      # # connectedIslandsDT$thinPoints
      # 
      # # take a sample of all the thin point per cluster
      # #connectedIslandsDT[ , `:=`( COUNT = .N , IDX = 1:.N ) , by = cluster ]
      # #connectedIslandsDT$COUNT<-NULL
      # connectedIslandsDT[ , `:=`(  IDX = 1:.N )  ]
      # # connectedIslandsDT
      # 
      # listOfClusters<-unique(connectedIslandsDT$cluster)
      # sapply(listOfClusters,  function(x, connectedIslandsDT, SAMPLE_ON_THIN_PORTIONS) {
      #   allIndexes<-connectedIslandsDT[cluster %in% x & thinPoints == TRUE,]$IDX
      #   keepThinIndexes<-sample(allIndexes, floor(length(allIndexes)*SAMPLE_ON_THIN_PORTIONS/100))
      #   connectedIslandsDT[IDX %in% keepThinIndexes,keepThinPoints:= TRUE]
      # },connectedIslandsDT, SAMPLE_ON_THIN_PORTIONS)
      # # connectedIslandsDT
      # 
      # # add keepThinPoints to avgDT
      # # connectedIslandsDT[keepThinPoints == TRUE,]
      # 
      # avgDT<-rbind(avgDT, 
      #              data.table(lon = connectedIslandsDT[keepThinPoints == TRUE,]$lon, 
      #                         lat = connectedIslandsDT[keepThinPoints == TRUE,]$lat, 
      #                         circleRadius = rep(0, nrow(connectedIslandsDT[keepThinPoints == TRUE,]))))
    }
    
    # avgDT
    # displayIn3D(AFMImageNetworksAnalysis@binaryAFMImage, noLight=TRUE)
    
  } 
  AFMImageNetworksAnalysis@binaryAFMImageWithCircles<-copy(newCircleAFMImage2)
  avgDT$keep<-rep(TRUE, nrow(avgDT))
  AFMImageNetworksAnalysis@circlesTable<-copy(unique(avgDT))
  return(AFMImageNetworksAnalysis)
  
}

#' getIntersectionPointWithBorder to be described
#'
#' \code{getIntersectionPointWithBorder} return a data.table
#' 
#' @param AFMImage a \code{\link{AFMImage}} from Atomic Force Microscopy
#' @param center center
#' @param r radius
#' @param deg degree
#' @author M.Beauvais
#' @export
getIntersectionPointWithBorder<-function(AFMImage, center, r, deg) {
  theta <- (deg * pi) / (180)
  x = center$lon + r * cos(theta)
  y = center$lat + r * sin(theta)
  
  pt=data.table(lat=y, lon=x)
  return(pt)
}

#' get a triangle starting from center, two segments of length r with angles deg1 and deg2 
#'
#' \code{getTriangle} return a data.table points of a triangle
#' 
#' @param AFMImage a \code{\link{AFMImage}} from Atomic Force Microscopy
#' @param center center
#' @param r length of segment
#' @param deg1 angle 1
#' @param deg2 angel 2
#' @author M.Beauvais
#' @export
getTriangle<-function(AFMImage, center, r, deg1, deg2) {
  pt1=getIntersectionPointWithBorder(AFMImage, center, r, deg1)
  pt2=getIntersectionPointWithBorder(AFMImage, center, r, deg2)
  
  trianglePts=data.table(lon=c(center$lon, pt1$lon, pt2$lon,center$lon), lat=c(center$lat, pt1$lat, pt2$lat,center$lat))
  return(trianglePts)
}

#' existsSegment checks if a segment exists in an AFMImage; check if all the heights at the segment coordinates are different to zero.
#'
#' \code{existsSegment} return a boolean
#' 
#' @param AFMImage a \code{\link{AFMImage}} from Atomic Force Microscopy or a binary \code{\link{AFMImage}}
#' @param segment a data.table coming from the getBresenham2Dsegment - x and y should start from 1,1 #TODO Segment class
#' @return TRUE if all the heights of the segment are different from zero
#' @author M.Beauvais
#' @export
existsSegment<-function(AFMImage, segment) {
  #print(segment)
  res<-!any(AFMImage@data$h[segment$x+(segment$y)*AFMImage@samplesperline]==0)
  #print(res)
  return(res)
}
#test existsSegment(binaryAFMImage,       segment= getBresenham2DSegment(10, 9,11, 9))
# existsSegment(binaryAFMImage, segment= getBresenham2DSegment(504,358,511,335))
# binaryAFMImage@samplesperline
# segment= getBresenham2DSegment(504,358,511,335)
# segment
# binaryAFMImage@data$h[segment$y+1+segment$x*binaryAFMImage@samplesperline]

#
#
# center$lon and center$lat
#Test
# library(sp)
# library(data.table)
# 
# Lines<-64
# Samplesperline<-64
# ScanSize<-128
# scanby<-ScanSize/Samplesperline
# endScan<-ScanSize*(1-1/Samplesperline)
# fullfilename="circlesMatrixImage"
# 
# binaryAFMImage<-AFMImage(
#      data = data.table(x = rep(seq(0,endScan, by= scanby), times = Lines),
#                        y = rep(seq(0,endScan, by= scanby), each = Samplesperline), 
#                        h = rep(1, Lines*Samplesperline*10)),
#      samplesperline = Samplesperline, lines = Lines, 
#      vscansize = ScanSize, hscansize = ScanSize, scansize = ScanSize, 
#      fullfilename = fullfilename )
# getCircleSpatialPoints(binaryAFMImage, center=data.table(lon=20, lat=15), circleRadius=0)
#getCircleSpatialPoints(binaryAFMImage, center= data.table(lon=20, lat=10), circleRadius=5)
#getCircleSpatialPoints(binaryAFMImage, center= data.table(lon=20, lat=10), circleRadius=0)
#getCircleSpatialPoints(binaryAFMImage, center= data.table(lon=20, lat=10), circleRadius=1)
#center= data.table(lon=20, lat=10)

#' get the spatial points on the circle including the center of the circle
#' 
#' @param binaryAFMImage a binary \code{\link{AFMImage}} from Atomic Force Microscopy
#' @param center the center of the circle with center$lon as the x coordinates and center$lat as the y coordinates
#' @param circleRadius the radius of the circle
#' @return a \code{\link{SpatialPoints}} object of all the points of the circle including the center of the circle  
#' @export
#' @author M.Beauvais
getCircleSpatialPoints<-function(binaryAFMImage, center, circleRadius) {
  
  if (circleRadius<0) {
    stop("getCircleSpatialPoints - the radius is inferior to 0")
    return()
  }
  if (circleRadius>0) {
    blockSize<-circleRadius*2+1
    
    pts = SpatialPoints(cbind(rep(1:blockSize,blockSize)+center$lon-circleRadius-1, rep(1:blockSize,1,each= blockSize)+center$lat-circleRadius-1))
    #print(pts)
    pts<-pts[pts$coords.x1>0&pts$coords.x1<binaryAFMImage@lines&pts$coords.x2>0&pts$coords.x2<binaryAFMImage@samplesperline]
    #plot(pts)
    nm <- sp::spDistsN1(pts=matrix(c(pts$coords.x1, pts$coords.x2), ncol=2), pt=c(center$lon, center$lat), longlat=FALSE)
    #print(nm)
    
    #centerAllpoints<-pts[nm==circleRadius]
    centerAllpoints<-pts[nm<=circleRadius]
    
    
    uniqueX2<-unique(centerAllpoints$coords.x2)
    #uniqueX2
    
    
    res<-lapply(1:length(uniqueX2),function(i,centerAllpoints, uniqueX2) {
      allX1<-centerAllpoints[centerAllpoints$coords.x2 == uniqueX2[i]]$coords.x1
      #print(allX1)
      min<-min(allX1)
      max<-max(allX1)
      
      if (min!=max) {
        x1<-c(min, max)
        return(data.table(x1, x2=rep(uniqueX2[i], 2)))
      }else{
        return(data.table(x1=min, x2=uniqueX2[i]))
      }
    },centerAllpoints, uniqueX2)
    
    resDT<-rbindlist(res)
    #resDT<-rbind(resDT, data.table(x1=center$lon,x2=center$lat)
    centerAllpoints<-SpatialPoints(cbind(
      c(resDT$x1, center$lon),
      c(resDT$x2, center$lat)
    ))
    #plot(centerAllpoints)
  }else{
    # circleRadius == 0
    centerAllpoints<-SpatialPoints(cbind(center$lon, center$lat))
  }
  #print("ok")
  return(centerAllpoints)
}


# test
# Test
#AreNodesConnected(binaryAFMImage, data.table(lon=226, lat=344), 10, data.table(lon=25, lat=344), 5)
#AreNodesConnected(binaryAFMImage, data.table(lon=226, lat=344), 10, data.table(lon=25, lat=344), 0)
#AreNodesConnected(binaryAFMImage, center1, circleRadius1, data.table(lon=pt$coords.x1, lat=pt$coords.x2), pt$circleRadius)


# AreNodesConnected(binaryAFMImage, data.table(lon=76, lat=60), 1, data.table(lon=79, lat=65), 0)
# AreNodesConnected(binaryAFMImage, data.table(lon=76, lat=60), 0, data.table(lon=79, lat=65), 0)
# 
# circle1AllPoints<-getCircleSpatialPoints(binaryAFMImage, data.table(lon=76, lat=60), 1)
# circle1AllPoints<-circle1AllPoints[which(binaryAFMImage@data$h[circle1AllPoints$coords.x2+1+circle1AllPoints$coords.x1*binaryAFMImage@samplesperline]!=0)]
# circle1AllPoints
# 
# existsSegment(binaryAFMImage, segment= getBresenham2DSegment(76,60,79,65))
# binaryAFMImage@data$h[segment$y+1+segment$x*binaryAFMImage@samplesperline]
# which(binaryAFMImage@data$h[segment$y+1+segment$x*AFMImage@samplesperline]!=0)

#' check if nodes represented by circles are connected. The function defines all the possible segments between the circles and check if at least one segment exists.
#' 
#' @param binaryAFMImage a binary \code{\link{AFMImage}} from Atomic Force Microscopy
#' @param center1 the center of the circle with center$lon as the x coordinates and center$lat as the y coordinates
#' @param radius1 the radius of the circle
#' @param center2 the center of the circle with center$lon as the x coordinates and center$lat as the y coordinates
#' @param radius2 the radius of the circle
#' @return TRUE if the nodes are connected
#' @export
#' @author M.Beauvais
AreNodesConnected<-function(binaryAFMImage, center1, radius1, center2, radius2) {
  # print(center1)
  # print(radius1)
  # print(center2)
  # print(radius2)
  
  if (radius1>0) {
    circle1AllPoints<-getCircleSpatialPoints(binaryAFMImage, center1, radius1)
  }else{
    circle1AllPoints<-getCircleSpatialPoints(binaryAFMImage, center1, 1)
    circle1AllPoints<-circle1AllPoints[which(binaryAFMImage@data$h[circle1AllPoints$coords.x2+1+circle1AllPoints$coords.x1*binaryAFMImage@samplesperline]!=0)]
  }
  # print(circle1AllPoints)
  # print(length(circle1AllPoints))
  #plot(circle1AllPoints)
  
  if (radius2>0)  circle2AllPoints<-getCircleSpatialPoints(binaryAFMImage, center2, radius2)
  else{
    circle2AllPoints<-getCircleSpatialPoints(binaryAFMImage, center2, 1)
    circle2AllPoints<-circle2AllPoints[which(binaryAFMImage@data$h[circle2AllPoints$coords.x2+1+circle2AllPoints$coords.x1*binaryAFMImage@samplesperline]!=0)]
  }
  
  
  #print(circle2AllPoints)
  #print(length(circle2AllPoints))
  #points(circle2AllPoints)
  if ((length(circle1AllPoints))&(length(circle2AllPoints)>0)) {
    for (circlePt1Nb in seq(1, length(circle1AllPoints))) {
      circlePt1<-circle1AllPoints[circlePt1Nb,]
      
      for (circlePt2Nb in seq(1, length(circle2AllPoints))) {
        circlePt2<-circle2AllPoints[circlePt2Nb,]
        segment<-getBresenham2DSegment(circlePt1$coords.x1, circlePt1$coords.x2,
                                       circlePt2$coords.x1, circlePt2$coords.x2)
        if (existsSegment(binaryAFMImage, segment)) {
          print(paste("segment exists",center1$lon, center1$lat,":",center2$lon, center2$lat))
          return(TRUE)
        }else{
          #print("FALSE")
        }
      }
    }
  }
  return(FALSE)
}
# binaryAFMImage<-AFMImageNetworksAnalysis@binaryAFMImage
# center1
# radius1<-circleRadius1
# center2<-data.table(lon=pt$lon, lat=pt$lat)
# radius2<-pt$circleRadius
# 
#binaryAFMImage@data$h[segment$x+(segment$y)*binaryAFMImage@samplesperline]
# binaryAFMImage@data$h[segment$y-1+(segment$x-1)*binaryAFMImage@samplesperline]
# binaryAFMImage@data$h[segment$x-1+(segment$y-1)*binaryAFMImage@samplesperline]
# displayIn3D(binaryAFMImage, noLight=TRUE)
# 
# segment[,1]-1
#

#' calculate the angle between two vectors
#' 
#' @param x a vector
#' @param y a vector
#' @return the angle between the vectors
#' @export
#' @author M.Beauvais
getAngle <- function(x,y){
  dot.prod <- x%*%y 
  norm.x <- norm(x,type="2")
  norm.y <- norm(y,type="2")
  # print(dot.prod)
  # print(norm.x)
  # print(norm.y)
  theta <- acos(dot.prod / (norm.x * norm.y))
  if (is.nan(theta)) theta=0
  return(as.numeric(theta))
}
# test
# getAngle(c(2,12), c(1,6))
# getAngle(c(2,12), c(4,24))

#' check if all the angles between one edge and a list of edges is superior to a specified value.
#' 
#' @param binaryAFMImage a binary \code{\link{AFMImage}} from Atomic Force Microscopy
#' @param edge1 one edge
#' @param edges2 list of edges
#' @param minAngle the minimum angle value 
#' @return TRUE if all the angle are superior to the specified value
#' @export
#' @author M.Beauvais
isAngleBetweenEdgesAlwaysSuperiorToMinAngle<-function(binaryAFMImage, edge1, edges2, minAngle) {
  #print(edge1)
  #print(edges2)
  
  coordsFromEdge1=getCoordinatesFromVertexId(as.numeric(edge1$from))
  coordsToEdge1=getCoordinatesFromVertexId(as.numeric(edge1$to))
  
  coordsFromEdges2=getCoordinatesFromVertexId(as.numeric(edges2$from))
  coordsToEdges2=getCoordinatesFromVertexId(as.numeric(edges2$to))
  
  # allYCoordinates<-cbind(coordsFromEdges2, coordsToEdges2)
  # print(allYCoordinates)
  x=c(coordsToEdge1$coords.x1-coordsFromEdge1$coords.x1, 
      coordsToEdge1$coords.x2-coordsFromEdge1$coords.x2)
  
  for (y in seq(1,nrow(coordsToEdges2))){
    y=c(coordsToEdges2[y,]$coords.x1-coordsFromEdges2[y,]$coords.x1, coordsToEdges2[y,]$coords.x2-coordsFromEdges2[y,]$coords.x2)
    
    angle<-getAngle(x,y)
    if (angle>pi) angle<-angle-pi
    # print(angle)
    #print(paste("x=",x,"y=",y,"angle=",180*angle/pi, "degrees"))
    if (angle<minAngle) {
      #print(paste(c(x,"->",y)))
      return(FALSE)
    }
  }
  return(TRUE)
}
# isAngleBetweenEdgesAlwaysSuperiorToMinAngle(edge1=data.table(from=vid1, to=vid2), edges2=existingEdgesVid1,0.52)
# 
# existingEdges<-data.table(from = c("6553685"), to = c("2097229"),arrows = c("to"))
# isAngleBetweenEdgesAlwaysSuperiorToMinAngle(edge1=data.table(from=1835079, to=6553685), edges2=existingEdges,0.52)
# isAngleBetweenEdgesAlwaysSuperiorToMinAngle(edge1=data.table(from=6553685, to=1835079), edges2=existingEdges,0.52)

#' display the network of nodes and edges
#' 
#' @param AFMImage an \code{\link{AFMImage}} from Atomic Force Microscopy
#' @param edges list of edges
#' @param isolates list of isolated edges
#' @export
#' @author M.Beauvais
displaygridIgraphPlotFromEdges<-function(AFMImage, edges, isolates) {
  #print(edges)
  alledges2<-as.vector(t(matrix(c(edges$from,edges$to),ncol=2)))
  vnodes<-unique(c(edges$from, edges$to))
  vnodes<-data.frame(id=vnodes, label = vnodes)
  
  # coords=getCoordinatesFromVertexId(as.numeric(levels(vnodes$id))[vnodes$id])
  # coords
  # coords[order(coords.x1),]
  
  g<-graph(edges= alledges2,isolates=isolates, directed=FALSE)
  gridIgraphPlot(AFMImage, g)
}


#' display the network of nodes and edges
#' 
#' @param AFMImageNetworksAnalysis an \code{\link{AFMImageNetworksAnalysis}}
#' @export
#' @author M.Beauvais
displaygridIgraphPlot<-function(AFMImageNetworksAnalysis) {
  keep<-NULL
  displaygridIgraphPlotFromEdges(AFMImageNetworksAnalysis@binaryAFMImage,
                                 AFMImageNetworksAnalysis@edgesTable[keep %in% c(TRUE),],
                                 AFMImageNetworksAnalysis@isolatedNodesList)
  
}

#' displayColoredNetworkWithVerticesSize
#' 
#' display network
#' 
#' @param AFMImageNetworksAnalysis a \code{\link{AFMImageNetworksAnalysis}}
#' @param fullfilename a directory plus filename for export
#' @export
#' @author M.Beauvais
displayColoredNetworkWithVerticesSize<-function(AFMImageNetworksAnalysis, fullfilename) { 
  vid<-node_degree<-vidorder<-edgeWeigth<-NULL
  
  if(missing(fullfilename)){
    save <- FALSE
  }else{
    save <- TRUE
  }
  
  AFMImage<-AFMImageNetworksAnalysis@binaryAFMImage
  g<-AFMImageNetworksAnalysis@skeletonGraph
  
  # good vizualisation to be kept !!!!
  verticesAnalysisDT<-data.table(vid=V(g)$name, node_degree=unname(degree(g)))
  # verticesAnalysisDT$coords.x1<-getCoordinatesFromVertexId(AFMImage,verticesAnalysisDT$vid)$coords.x1
  # verticesAnalysisDT$coords.x2<-getCoordinatesFromVertexId(AFMImage,verticesAnalysisDT$vid)$coords.x2
  verticesAnalysisDT$coords.x1<-getCoordinatesFromVertexId(verticesAnalysisDT$vid)$coords.x1
  verticesAnalysisDT$coords.x2<-getCoordinatesFromVertexId(verticesAnalysisDT$vid)$coords.x2
  
  verticesAnalysisDT$vid<-as.numeric(verticesAnalysisDT$vid)
  setkey(verticesAnalysisDT, vid)
  
  cDT<-AFMImageNetworksAnalysis@circlesTable
  cDT$vid<-getVertexId(AFMImage,cDT$lon,cDT$lat)
  setkey(cDT, vid)
  
  circlesDT<-merge(verticesAnalysisDT, cDT, all = TRUE)
  circlesDT
  
  setkeyv(circlesDT, "vid")
  listOfVerticesDT<-data.table(vid=as.numeric(V(g)$name), vidorder=seq(1, length(V(g)$name)))
  setkeyv(listOfVerticesDT, "vid")
  
  finalDT<-merge(listOfVerticesDT,circlesDT)
  finalDT$color<-"black"
  finalDT[node_degree==0,]$color<-rep("black", nrow(finalDT[node_degree==0,]))
  finalDT[node_degree==1,]$color<-rep("blue", nrow(finalDT[node_degree==1,]))
  finalDT[node_degree>2,]$color<-rep("red", nrow(finalDT[node_degree>2,]))
  finalDT[node_degree==2,]$color<-rep("grey", nrow(finalDT[node_degree==2,]))
  
  vertexcolor<-finalDT[order(vidorder),]$color
  
  # define the layout matrix
  coordinatesVector<-getNetworkGridLayout(AFMImage, V(g)$name)
  #coordinatesVector
  
  l<-matrix(coordinatesVector$y ,byrow = TRUE)
  l<-cbind(l, coordinatesVector$x)
  
  # plot.igraph(g, layout=l, 
  #             vertex.shape="circle", vertex.size=2, vertex.label=NA, vertex.color="red", vertex.frame.color="red",
  #             edge.color="grey"
  # )
  
  
  
  # plot.igraph(g, layout=l, 
  #             vertex.shape="circle", vertex.size=2, vertex.label=NA, vertex.color=vertexcolor, vertex.frame.color=vertexcolor,
  #             edge.color="grey"
  # )
  
  vertexsize<-finalDT[order(vidorder),]$circleRadius
  print(vertexsize)
  
  
  # plot.igraph(g, layout=l,
  #             vertex.shape="circle", vertex.size=vertexsize, vertex.label=NA, vertex.color=vertexcolor, vertex.frame.color=vertexcolor,
  #             edge.color="grey"
  # )
  
  
  
  # calculate edge weigth
  # mean of vertices size
  rm(edgeDT)
  edgeDT<-copy(as.data.table(get.edgelist(g)))
  colnames(edgeDT)<-c("vid","to")
  edgeDT$vid<-as.character(edgeDT$vid)
  edgeDT
  finalDT2<-data.table(vid=as.character(finalDT$vid), from_node_degree=finalDT$node_degree, from_circleRadius= finalDT$circleRadius)
  setkeyv(edgeDT, "vid")
  setkeyv(finalDT2, "vid")
  edgeDT<-merge(edgeDT,finalDT2, all.x=TRUE)
  
  colnames(edgeDT)<-c("from","vid","from_node_degree","from_circleRadius")
  edgeDT$vid<-as.character(edgeDT$vid)
  edgeDT
  finalDT2<-data.table(vid=as.character(finalDT$vid), to_node_degree=finalDT$node_degree, to_circleRadius= finalDT$circleRadius)
  setkeyv(edgeDT, "vid")
  setkeyv(finalDT2, "vid")
  edgeDT<-merge(edgeDT,finalDT2, all.x=TRUE)
  
  # edge weigth calculation
  edgeDT$edgeWeigth<-NULL
  
  
  #edgeDT[, nb:=.I,]
  edgeDT[,edgeWeigth:=(edgeDT$from_circleRadius+edgeDT$to_circleRadius)/2,by=.I]
  edgeDT
  #E(g)$weight <- edgeDT$edgeWeigth*50
  if (save) png(filename = fullfilename,width = 1024, height = 1024, units = "px")
  
  plot.igraph(g, layout=l,
              vertex.shape="circle", vertex.size=vertexsize, vertex.label=NA, vertex.color=vertexcolor, vertex.frame.color=vertexcolor,
              edge.width= edgeDT$edgeWeigth, edge.color="grey"
  )
  if (save) dev.off()
}

#' identifyNodesAndEdges
#' 
#' find nodes and edges
#' 
#' @param ... cl: a cluster object from the parallel package
#' @param AFMImageNetworksAnalysis a \code{\link{AFMImageNetworksAnalysis}}
#' @param maxHeight a double for filtering the heights - upper to this height the heights are set to zero
#' @return AFMImageNetworksAnalysis a \code{\link{AFMImageNetworksAnalysis}}
#' 
#' @export
#' @author M.Beauvais
identifyNodesAndEdges<-function(..., AFMImageNetworksAnalysis,maxHeight){
  print(paste("identifyNodesAndEdges"))
  force(AFMImageNetworksAnalysis)
  filename<-lon<-lat<-minDistance<-from_cluster<-to_cluster<-total<-vid<-NULL
  meanLon<-meanLat<-NULL
  
  args<-names(list(...))
  print(args)
  if (is.null(args)) {
    clExist<-FALSE
  }else{
    clExist<-c(match('cl',args)!=-1)
    print(paste("clExist= ",clExist))
  }
  
  
  if (clExist) {
    print("using parallel")
    requireNamespace("parallel")
    cl<-cl
  }else{
    print("Not using parallel")
  }
  
  binaryAFMImage<-copy(AFMImageNetworksAnalysis@binaryAFMImage)
  #displayIn3D(binaryAFMImage, noLight=TRUE)
  newCircleAFMImage<-copy(AFMImageNetworksAnalysis@binaryAFMImage)
  newCircleAFMImage2<-copy(AFMImageNetworksAnalysis@binaryAFMImage)
  
  avgDT<-data.table(cluster=c(" "),lon = c(0), lat = c(0), circleRadius= c(0), keep=c(FALSE), vid=c(0))
  
  
  cluster<-node<-mindist<-maxdist<-keep<-NULL
  nbOfCircles<-maxArea<-h<-NULL
  clusterLon<-clusterLat<-cluster<-IDX<-keepThinPoints<-meandist<-NULL
  
  circlesMatrixFilename<-paste0(filename, "-circlesMatrix.RData")
  if (clExist) {
    circlesMatrix<-getMaxCircleMatrix(cl=cl, newCircleAFMImage = newCircleAFMImage,CIRCLE_RADIUS_INIT=CIRCLE_RADIUS_INIT)
  }else{
    circlesMatrix<-getMaxCircleMatrix(newCircleAFMImage = newCircleAFMImage,CIRCLE_RADIUS_INIT=CIRCLE_RADIUS_INIT)
  }
  # save(circlesMatrix,file= paste0(dirOutput,circlesMatrixFilename))
  #load(file= paste0(dirOutput,circlesMatrixFilename))
  
  #circlesMatrixAFMImage<-getAFMImageFromMatrix(binaryAFMImage, circlesMatrix)
  #displayIn3D(circlesMatrixAFMImage, noLight = TRUE)
  circlesMatrixAFMImage<-getAFMImageFromMatrix(binaryAFMImage, circlesMatrix)
  #displayIn3D(circlesMatrixAFMImage, noLight = TRUE)
  
  
  listOfFilters<-sort(unique(circlesMatrixAFMImage@data$h), decreasing = TRUE)
  listOfFilters
  filterIndex<-0
  
  while(filterIndex<length(listOfFilters)){
    #while(filterIndex<3){
    # using the radius map and filter it
    filterIndex<-filterIndex+1
    
    maxFilter = listOfFilters[filterIndex]
    maxFilter
    print(maxFilter)
    if (maxFilter > 0) {
      if (filterIndex != 1) max<-listOfFilters[filterIndex-1] else max<-maxHeight
      max
      
      AFMImageNetworksAnalysis = new("AFMImageNetworksAnalysis")
      AFMImageNetworksAnalysis@heightNetworksslider=1
      AFMImageNetworksAnalysis@filterNetworkssliderMin=maxFilter
      AFMImageNetworksAnalysis@filterNetworkssliderMax<-max
      AFMImageNetworksAnalysis@smallBranchesTreatment=FALSE
      AFMImageNetworksAnalysis<-transformAFMImageForNetworkAnalysis(AFMImageNetworksAnalysis, copy(circlesMatrixAFMImage))
      
      newAFMImage<-AFMImageNetworksAnalysis@binaryAFMImage
      #displayIn3D(binaryAFMImage, noLight=TRUE)
      #displayIn3D(circlesMatrixAFMImage, noLight=TRUE)
      #displayIn3D(newAFMImage, noLight=TRUE)
      
      # if points were removed in a previous step, do not take them into account
      newAFMImage@data$h[newCircleAFMImage@data$h == 0]<-0
      #displayIn3D(newAFMImage, noLight=TRUE)
      #displayIn3D(newCircleAFMImage2, noLight=TRUE)
      print("ok")
      #dbscan
      # withtreatedPoints<-newAFMImage@data
      # allPointsislandsDT<-cbind(lon=1+withtreatedPoints$y*newAFMImage@lines/newAFMImage@vscansize, lat=1+withtreatedPoints$x*newAFMImage@samplesperline/newAFMImage@hscansize)
      untreatedPoints<-newAFMImage@data[h!=0]
      untreatedPoints
      
      if (nrow(untreatedPoints)>0) {
        # lon and lat both start from 1
        rm(islandsDT)
        islandsDT<-cbind(lat=1+untreatedPoints$y*newAFMImage@lines/newAFMImage@vscansize, lon=1+untreatedPoints$x*newAFMImage@samplesperline/newAFMImage@hscansize)
        islandsDT
        islandsDT<-data.table(lat=islandsDT[,1], lon=islandsDT[,2])
        
        # remove points that are near the border
        lon_border_width<-floor(newAFMImage@samplesperline*5/100)
        lon_border_width
        lat_border_width<-floor(newAFMImage@lines*5/100)
        lat_border_width
        islandsDT<-islandsDT[islandsDT$lat>lat_border_width &  islandsDT$lat<(newAFMImage@lines-lat_border_width)&
                               islandsDT$lon>lon_border_width &  islandsDT$lon<(newAFMImage@samplesperline-lon_border_width)]
        islandsDT
        # if (nrow(untreatedPoints)==1) islandsDT<-as.matrix(islandsDT[islandsDT[,1]>lat_border_width &  islandsDT[,1]<(newAFMImage@lines-lat_border_width)&
        #                                                                islandsDT[,2]>lon_border_width &  islandsDT[,2]<(newAFMImage@samplesperline-lon_border_width)], ncol=2, byrow = FALSE)
        # islandsDT
        #if (!all(dim(islandsDT)==0)&nrow(islandsDT)>0) {
        if (nrow(islandsDT)>0) {
          # checks (very important)
          # circlesMatrix[islandsDT$lon,islandsDT$lat]
          # circlesMatrixAFMImage@data$h[((islandsDT$lat)-1)*circlesMatrixAFMImage@samplesperline + islandsDT$lon]
          
          
          DBSCAN <- dbscan::dbscan(islandsDT, eps = 1.5, MinPts = 1, borderPoints=FALSE)
          #plot(untreatedPoints$y, untreatedPoints$x, col = DBSCAN$cluster, pch = 20)
          #plot(islandsDT, col = DBSCAN$cluster, pch = 20)
          
          islandsDT<-data.table(islandsDT,cluster=DBSCAN$cluster)
          setkeyv(islandsDT, "cluster")
          
          isolatedIslandsDT<-islandsDT[cluster==0,]
          isolatedIslandsDT
          
          islandsDT<-islandsDT[cluster!=0,]
          #print(islandsDT)
          plot( islandsDT$lat, islandsDT$lon, col = islandsDT$cluster, pch = 20, xlim = c(0, newAFMImage@samplesperline), ylim=c(0,newAFMImage@lines))
          
          print("start spliting segment regularly...")
          clusterChar = data.table (
            cluster = unique(islandsDT$cluster),
            minLon = islandsDT[, min(lon), by=cluster]$V1,
            maxLon = islandsDT[, max(lon), by=cluster]$V1,
            minLat = islandsDT[, min(lat), by=cluster]$V1,
            maxLat = islandsDT[, max(lat), by=cluster]$V1)
          clusterChar$area<-(clusterChar$maxLat-clusterChar$minLat)*(clusterChar$maxLon-clusterChar$minLon)
          clusterChar$count<-islandsDT[,.N, by=cluster]$N
          
          clusterChar$shape<-sapply(1:nrow(clusterChar),function(i) {
            if ((clusterChar[i,]$maxLon-clusterChar[i,]$minLon)>(clusterChar[i,]$maxLat-clusterChar[i,]$minLat)) {
              return("width")
            }else{
              return("height")
            }
          })
          print(clusterChar)
          print(maxFilter)
          
          rm(resDT6)
          
          if (maxFilter==1){
            print("STOOP")
          }
          #i<-2
          #i<-4
          print(paste("calculation of primary nodes for maxFilter=", maxFilter))
          resDT6<-lapply(1:nrow(clusterChar),function(i,islandsDT, clusterChar, maxFilter,AREA_MIN,CLUSTER_COUNT_MIN) {
            meanLon<-meanLat<-NULL
            # print("i")
            print(paste("cluster",i))
            totalHeight<-clusterChar[i,]$maxLat-clusterChar[i,]$minLat
            #print(totalHeight)
            totalWidth<-clusterChar[i,]$maxLon-clusterChar[i,]$minLon
            #print(totalWidth)
            clusterN<-clusterChar[i,]$cluster
            #print(clusterN)
            
            # if the cluster is small compared to maxFilter, take the barycenter of the cluster
            if(((clusterChar[i,]$shape == "height")&totalHeight<maxFilter)|
               ((clusterChar[i,]$shape == "width")&totalWidth<maxFilter)) {
              # taking the barycenter
              clusterN<-clusterChar[i,]$cluster
              resDT2<-islandsDT[cluster %in% clusterN,]
              
              tempResDT2<-copy(resDT2)
              tempResDT2[, meanLon:=mean(lon), by=cluster]
              tempResDT2[, meanLat:=mean(lat), by=cluster]
              tempResDT2[, dist:=sqrt((lon-meanLon)^2+(lat-meanLat)^2), by=cluster]
              tempResDT2[, minDistance:=min(dist), by=cluster]
              tempResDT2[dist == minDistance, c("lon","lat","cluster"),]
              tempResDT2<-unique(tempResDT2[dist == minDistance, c("lon","lat","cluster"),],by="cluster",fromLast=TRUE)
              # print("tempResDT2")
              # print(tempResDT2)
              # circleRadius<-circlesMatrixAFMImage@data[circlesMatrixAFMImage@data$y %in% (circlesMatrixAFMImage@vscansize*(tempResDT2$lat-1)/circlesMatrixAFMImage@samplesperline) &
              #                                             circlesMatrixAFMImage@data$x %in% (circlesMatrixAFMImage@hscansize*(tempResDT2$lon-1)/circlesMatrixAFMImage@lines)]$h
              # print(circleRadius)
              circleRadius<-rep(c(maxFilter), times=as.integer(nrow(tempResDT2)))
              
              return(data.table(cluster=clusterN, lon= tempResDT2$lon, lat= tempResDT2$lat, circleRadius= circleRadius))
              
            }else {
              minLat<-clusterChar[i,]$minLat
              maxLat<-clusterChar[i,]$maxLat
              minLon<-clusterChar[i,]$minLon
              maxLon<-clusterChar[i,]$maxLon
              
              theta<-atan2((maxLon-minLon),(maxLat-minLat))
              hypothenuse<-maxFilter
              
              # regularly spaced points depending on circleRadius
              if (clusterChar[i,]$shape == "height") {
                # number
                if (cos(theta)!=0) {
                  regularSpace<-hypothenuse/cos(theta)
                }else{
                  regularSpace<-hypothenuse/sin(theta)
                }
                numberOfIntermediaryPoints<-floor(totalHeight/(regularSpace*2)-1)
                if (numberOfIntermediaryPoints<=0) {
                  numberOfIntermediaryPoints<-0
                  vectorOfLat<-c(clusterChar[i,]$minLat, clusterChar[i,]$maxLat)
                  vectorOfLat
                }else{
                  #regularSpace<-floor(totalHeight/(numberOfIntermediaryPoints+1))
                  vectorOfLat<-seq(from = (minLat+regularSpace), to = (minLat+totalHeight-regularSpace) , by=regularSpace*2)
                  vectorOfLat<-round(vectorOfLat,0)
                  vectorOfLat
                }
                # use the medium horizontal position for each vectorOfLat
                resDT5<-lapply(1:length(vectorOfLat),function(j, islandsDT, vectorOfLon, clusterN) {
                  resDT2<-islandsDT[lat %in% vectorOfLat[j]  & cluster %in% clusterN,]
                  #print(resDT2)
                  avgDTLon<-floor(mean(resDT2$lon))
                  # print("vectorOfLat")
                  # circleRadius<-circlesMatrixAFMImage@data[circlesMatrixAFMImage@data$y %in% (circlesMatrixAFMImage@vscansize*(vectorOfLat[j]-1)/circlesMatrixAFMImage@samplesperline) &
                  #                                            circlesMatrixAFMImage@data$x %in% (circlesMatrixAFMImage@hscansize*(avgDTLon-1)/circlesMatrixAFMImage@lines)]$h
                  # print(circleRadius)
                  circleRadius<-rep(maxFilter, times=as.integer(length(avgDTLon)))
                  
                  return(data.table(cluster=clusterN, lon = avgDTLon, lat= vectorOfLat[j], circleRadius= circleRadius))
                },islandsDT, vectorOfLat, clusterChar[i,]$cluster)
                
                return(rbindlist(resDT5))
                
              }else{
                if (cos(theta)!=0) {
                  regularSpace<-hypothenuse/sin(theta)
                }else{
                  regularSpace<-hypothenuse/cos(theta)
                }
                
                numberOfIntermediaryPoints<-floor(totalWidth/(regularSpace*2)-1)
                
                if (numberOfIntermediaryPoints<=0) {
                  numberOfIntermediaryPoints<-0
                  vectorOfLon<-c(minLon, maxLon)
                  vectorOfLon
                }else{
                  #regularSpace<-floor(totalWidth/(numberOfIntermediaryPoints+1))
                  vectorOfLon<-seq(from = (minLon+regularSpace), to = (maxLon-regularSpace) , by=regularSpace*2)
                  #vectorOfLon<-c(minLon,vectorOfLon, maxLon)
                  vectorOfLon<-round(vectorOfLon,0)
                  vectorOfLon
                }
                #j=2
                resDT5<-lapply(1:length(vectorOfLon),function(j, islandsDT, vectorOfLon, clusterN) {
                  resDT2<-islandsDT[lon %in% vectorOfLon[j] & cluster %in% clusterN,]
                  avgDTLat<-floor(mean(resDT2$lat))
                  # print("vectorOfLon")
                  # circleRadius<-circlesMatrixAFMImage@data[circlesMatrixAFMImage@data$y %in% (circlesMatrixAFMImage@vscansize*(avgDTLat-1)/circlesMatrixAFMImage@samplesperline) &
                  #                                            circlesMatrixAFMImage@data$x %in% (circlesMatrixAFMImage@hscansize*(vectorOfLon[j]-1)/circlesMatrixAFMImage@lines),]$h
                  # print(circleRadius)
                  circleRadius<-rep(maxFilter, times=as.integer(length(avgDTLat)))
                  
                  return(data.table(cluster=clusterN, lon = vectorOfLon[j], lat= avgDTLat, circleRadius= circleRadius))
                },islandsDT, vectorOfLon, clusterChar[i,]$cluster)
                return(rbindlist(resDT5))
              }
            }
          }, islandsDT, clusterChar,maxFilter,AREA_MIN,CLUSTER_COUNT_MIN)
          
          if (maxFilter==1){
            print("STOOP")
          }
          
          resDT6<-rbindlist(resDT6)
          resDT6<-unique(resDT6)
          resDT6<-resDT6[complete.cases(resDT6),]
          resDT6<-resDT6[lon != 0 & lon != (AFMImageNetworksAnalysis@binaryAFMImage@samplesperline-1) & lat!=0 & lat!=(AFMImageNetworksAnalysis@binaryAFMImage@lines-1) ,]
          print(resDT6)
          resDT6<-resDT6[circleRadius!=0,]
          
          if (nrow(resDT6)==0) break;
          print("ok")
          avgDT6<-data.table(cluster=  paste0(maxFilter,"-",resDT6$cluster) , lon = resDT6$lon, lat = resDT6$lat, 
                             circleRadius=as.vector(circlesMatrix[cbind(resDT6$lon,resDT6$lat)]),
                             keep=rep(TRUE, nrow(resDT6)),
                             vid= getVertexId(AFMImageNetworksAnalysis@binaryAFMImage,resDT6$lon, resDT6$lat))
          #print(avgDT6)
          avgDT6$circleRadius
          
          avgDT<-rbind(avgDT, avgDT6)
          avgDT
          
          from<-to<-NULL
          alledges<-c()
          allvertices<-c()
          
          
          # bag of each envelope
          # no need because of !exists("")
          #pointsInsideEnvelopesToBeRemovedDT=data.table(vid=c(0),lon=c(0),lat=c(0))
          
          
          # print("for envelope")
          for(clusterName in unique(avgDT6$cluster)) {
            # TODO if (nrow(avgDT6[cluster %in% clusterName,c("lon","lat"),])>1){
            centers<-as.matrix(avgDT6[cluster %in% clusterName & circleRadius != 0,c("lon","lat"),])
            centers
            colnames(centers) <- NULL
            r<-unlist(unname(c(avgDT6[cluster %in% clusterName & circleRadius != 0,c("circleRadius"),])))
            
            tryCatch({
              # library(PlotRegionHighlighter)
              # library(sp)
              print(paste("calculate enveloppe of cluster of points ", clusterName,"with RADIUS_MULTIPLIER=",RADIUS_MULTIPLIER))
              envelope <- generatePolygonEnvelope(AFMImageNetworksAnalysis, centers, r*RADIUS_MULTIPLIER)
              pointsInsideEnvelopeDT<-getAllPointsToRemove(AFMImageNetworksAnalysis, envelope)
              colnames(pointsInsideEnvelopeDT)<-c("vid","lat","lon")
              pointsInsideEnvelopeDT
              
              # add enveloppe to be removed
              # print("*** Removing cluster of nodes")
              # print(centers)
              # print(r*RADIUS_MULTIPLIER)
              if (!exists("pointsInsideEnvelopesToBeRemovedDT")) pointsInsideEnvelopesToBeRemovedDT<- pointsInsideEnvelopeDT else
                pointsInsideEnvelopesToBeRemovedDT<-rbind(pointsInsideEnvelopesToBeRemovedDT, pointsInsideEnvelopeDT)
              
            }, error = function(e) {
              print("extra nodes error 2")
              print(e)
              warning(paste("extra nodes error",e))
            }, finally = {
              #print("extra nodes identified")
            })
          }
          
          # iterate on all possible edges in order to remove edge between nodes
          identifyLinksBetweenNodesToCreateNodes<-function(k,AFMImageNetworksAnalysis, newCircleAFMImage, MAX_DISTANCE,allPossibleEdges) {
            requireNamespace("data.table")
            requireNamespace("sp")
            requireNamespace("parallel")
            requireNamespace("AFM") #TODO
            #print(k)
            # TODO if kept remove parameter
            binaryAFMImage<-AFMImageNetworksAnalysis@binaryAFMImage
            binaryAFMImage<-newCircleAFMImage
            
            currentEdge<-allPossibleEdges[,k]
            centerId<-currentEdge[1]
            otherCenterId<-currentEdge[2]
            
            circlesTable<-AFMImageNetworksAnalysis@circlesTable[keep %in% c(TRUE),]
            
            vedges<-data.table(from = c(""), to = c(""),arrows = c("to"))
            
            center1= circlesTable[centerId,]
            circleRadius1=circlesTable[centerId,]$circleRadius
            vid1<-getVertexId(AFMImageNetworksAnalysis@binaryAFMImage,center1$lon, center1$lat)
            
            
            otherNodes<-circlesTable[otherCenterId,]
            #otherNodes<-allNodesAsSpatialPoints[-centerId,]
            otherNodes$dist<-sp::spDistsN1(pts=matrix(c(otherNodes$lon, otherNodes$lat), ncol=2), pt=c(center1$lon, center1$lat), longlat=FALSE)
            
            otherNodes<-otherNodes[with(otherNodes, order(otherNodes$dist)), ]
            otherNodes<-otherNodes[otherNodes$dist<MAX_DISTANCE,]
            #print(otherNodes)
            
            
            if (nrow(otherNodes)!=0){
              #centerId2Nb<-1
              #centerId2Nb<-10
              pt<-otherNodes[1,]
              vid2<-getVertexId(AFMImageNetworksAnalysis@binaryAFMImage,pt$lon, pt$lat)
              
              if (AreNodesConnected(binaryAFMImage, center1, circleRadius1, data.table(lon=pt$lon, lat=pt$lat), pt$circleRadius)) {
                vedges<-rbind(vedges, data.table(from = vid1, to = vid2,arrows = c("to")))
                print(paste(vid1,vid2, " edge found"))
              }else{
                #print("edge not found")
              }
              
            }else{
              #print("node too far")
            }
            return(vedges[-1,])
          }
          
          
          edgesProcessed<-c()
          
          
          avgDT6$circleRadius=avgDT6$circleRadius
          avgDT6$keep<-rep(TRUE, nrow(avgDT6))
          avgDT6$vid<-as.character(getVertexId(AFMImageNetworksAnalysis@binaryAFMImage,avgDT6$lon, avgDT6$lat))
          avgDT6
          avgDT$keep<-rep(TRUE, nrow(avgDT))
          avgDT$vid<-as.character(getVertexId(AFMImageNetworksAnalysis@binaryAFMImage,avgDT$lon, avgDT$lat))
          avgDT
          
          circlesTable<-avgDT[-1,]
          circlesTable$vid<-as.character(getVertexId(AFMImageNetworksAnalysis@binaryAFMImage,circlesTable$lon, circlesTable$lat))
          circlesTable
          AFMImageNetworksAnalysis@circlesTable<-copy(circlesTable)
          #AFMImageNetworksAnalysis@binaryAFMImage<-binaryAFMImage
          
          
          #
          # now identify extra nodes based on links between nodes
          #
          print(paste("number of circles=", nrow(circlesTable)))
          if (is.list(circlesTable) & nrow(circlesTable) > 1) {
            print(paste(nrow(avgDT6),nrow(circlesTable)))
            allPossibleEdges<-combn(seq(nrow(circlesTable)-nrow(avgDT6)+1,nrow(circlesTable)), 2, simplify = TRUE)
            
            if (nrow(circlesTable)!=nrow(avgDT6)){
              allPossibleEdges<-cbind(allPossibleEdges,
                                      rbind(rep(seq(nrow(circlesTable)-nrow(avgDT6)+1,nrow(circlesTable)),each=nrow(circlesTable)-nrow(avgDT6)),
                                            rep(seq(1,nrow(circlesTable)-nrow(avgDT6)),nrow(avgDT6), by=nrow(avgDT6)))
              )
            }
            # egdes between new nodes / new nodes and new nodes / old nodes
            allPossibleEdges
            
            print(paste("number of possible edges=", ncol(allPossibleEdges)))
            print("identifyLinksBetweenNodesToCreateNodes")
            start.time <- Sys.time()
            print(start.time)
            if(clExist) {
              #cl<-cl
              print("************************************ using parallel")
              parallel::clusterEvalQ(cl , c(library("data.table"),library("sp"), library("AFM"),library("parallel")))
              parallel::clusterExport(cl, c("AFMImageNetworksAnalysis","newCircleAFMImage","MAX_DISTANCE","allPossibleEdges"), envir=environment())
              res<-parallel::parLapplyLB(cl, 1:ncol(allPossibleEdges),identifyLinksBetweenNodesToCreateNodes, AFMImageNetworksAnalysis, newCircleAFMImage, MAX_DISTANCE,allPossibleEdges)
            }else{
              res<-lapply(1:ncol(allPossibleEdges),identifyLinksBetweenNodesToCreateNodes, AFMImageNetworksAnalysis, newCircleAFMImage, MAX_DISTANCE,allPossibleEdges)
            }
            end.time <- Sys.time()
            time.taken <- end.time - start.time
            vedges<-rbindlist(res)
            
            print(vedges)
            print(paste0("time.taken: ",time.taken))
            
            if (nrow(vedges)>0) {
              
              # find edge for the biggest nodes
              avgDT6
              setkeyv(vedges, c("from","to"))
              vedges<-unique(vedges)
              colnames(vedges)<-c("vid","to","arrows")
              vedges$vid<-as.character(vedges$vid)
              vedges
              setkeyv(vedges, "vid")
              setkeyv(avgDT6, "vid")
              vedges<-merge(vedges,avgDT6[,c("vid","circleRadius","lon","lat","cluster"),], all.x=TRUE)
              colnames(vedges)<-c("from","vid","arrows","from_circleRadius","from_lon","from_lat","from_cluster")
              vedges
              setkeyv(vedges, "vid")
              vedges<-merge(vedges,avgDT6[,c("vid","circleRadius","lon","lat","cluster"),], all.x=TRUE)
              colnames(vedges)<-c("from","to","arrows","from_circleRadius","from_lon","from_lat","to_cluster","to_circleRadius","to_lon","to_lat","from_cluster")
              vedges$total<-vedges$from_circleRadius+vedges$to_circleRadius
              vedges$dist<-sqrt((vedges$from_lon-vedges$to_lon)^2+(vedges$from_lat-vedges$to_lat)^2)
              
              
              # no edge inside the same cluster
              vedges<-vedges[from_cluster != to_cluster,]
              
              # not several edges between the same couple of clusters
              fromto_cluster<-sapply(1:nrow(vedges), function(i) {
                from_cluster<-vedges[i,]$from_cluster
                to_cluster<-vedges[i,]$to_cluster
                ifelse(from_cluster<to_cluster,return(paste0(from_cluster," ",to_cluster)),
                       return(paste0(to_cluster," ",from_cluster)))
              } )
              vedges$fromto_cluster<-fromto_cluster
              vedges<-vedges[order(dist, decreasing = FALSE),]
              vedges<-unique(vedges, by="fromto_cluster")
              # remove already processed edge
              vedges<-vedges[!vedges$fromto_cluster %in% edgesProcessed,]
              addedNode<-FALSE
              
              if (nrow(vedges)>0) {
                
                
                removeDuplicatedEdge<-sapply(1:nrow(vedges), function(i) {
                  from_cluster<-vedges[i,]$from_cluster
                  to_cluster<-vedges[i,]$to_cluster
                  fromto_cluster<-vedges[i,]$fromto_cluster
                  #print(paste("removeDuplicatedEdge - ", from_cluster))
                  #print(paste("removeDuplicatedEdge - ", to_cluster))
                  
                  split_from_cluster<-unlist(strsplit(from_cluster, split=" ", fixed=TRUE))
                  split_to_cluster<-unlist(strsplit(to_cluster, split=" ", fixed=TRUE))
                  
                  new_split=c(split_from_cluster, split_to_cluster)
                  #print(paste("removeDuplicatedEdge - ", new_split))
                  #print(paste("removeDuplicatedEdge - ", length(new_split)==length(unique(new_split))))
                  if (length(new_split)!=length(unique(new_split))) return(TRUE)
                  
                  
                  split_fromto_cluster<-unlist(strsplit(fromto_cluster, split=" ", fixed=TRUE))
                  #print(paste("removeDuplicatedEdge - ", length(split_fromto_cluster)))
                  if (length(split_fromto_cluster)>2) return(TRUE)
                  
                  # pos1 = grepl(from_cluster, to_cluster, fixed=TRUE)
                  # pos2 = grepl(to_cluster, from_cluster, fixed=TRUE)
                  # if (pos1 ==  FALSE | pos2 != FALSE) {
                  #   return(FALSE)
                  # }else{
                  #   return(TRUE)
                  # }
                  return(FALSE)
                } )
                vedges$remove<-removeDuplicatedEdge
                #print(vedges)
                
                vedges<-vedges[!vedges$remove,]
                
                # priority to edge with low distances
                vedges<-vedges[order(total, -dist, decreasing = TRUE),]
                
                print(vedges)
                
                # eliminate triangles
                allVertices=unique(c(vedges$from, vedges$to))
                allVertices
                if(clExist) {
                  vedges<-simplifyNetwork(cl=cl, allVertices=allVertices, allEdges=vedges)
                }else{
                  vedges<-simplifyNetwork(allVertices=allVertices, allEdges=vedges)
                }
                vedges<-vedges[!vedges$remove,]
                
                # no edge between clusters that are connected
                vedges<-vedges[!dist<total,]
                
                # distance should be at least three times the current 
                #vedges<-vedges[dist>=maxFilter*3,]
                
                # TODO simplify network / triangles
                # Browse[2]> vedges
                # from      to arrows from_circleRadius from_lon from_lat to_cluster to_circleRadius to_lon to_lat from_cluster total distance
                # 1: 12582955 9961521     to                 4       49       38        4-3               4     43     48          4-5     8 11.66190
                # 2: 13631505 9961495     to                 4       23       38        4-2               4     17     52          4-6     8 15.23155
                # 3:  8650775 5242889     to                 4        9       20        4-1               4     23     33          4-2     8 19.10497
                # 4: 12582955 9961495     to                 4       23       38        4-2               4     43     48          4-5     8 22.36068
                # 5: 12582955 5242889     to                 4        9       20        4-1               4     43     48          4-5     8 44.04543
                # fromto_cluster remove
                # 1:        4-3 4-5  FALSE
                # 2:        4-2 4-6  FALSE
                # 3:        4-1 4-2  FALSE
                # 4:        4-2 4-5  FALSE
                # 5:        4-1 4-5  FALSE
                
                #!!lk;l;:
                
                vedges2<-copy(vedges)
                vedges2$old_from<-vedges2$from
                vedges2$old_to<- vedges2$to
                vedges2$from<-vedges2$from_cluster
                vedges2$to<- vedges2$to_cluster
                
                allVertices2=unique(c(vedges2$from, vedges2$to))
                if(clExist) {
                  vedges2<-simplifyNetwork(cl=cl, allVertices=allVertices2, allEdges=vedges2)
                }else{
                  vedges2<-simplifyNetwork(allVertices=allVertices2, allEdges=vedges2)
                }
                vedges2<-vedges2[!vedges2$remove,]
                vedges2
                
                vedges2$from<-vedges2$old_from
                vedges2$to<- vedges2$old_to
                vedges2$old_from<-NULL
                vedges2$old_to<- NULL
                vedges<-copy(vedges2)
                
                # add nodes on the possible edge
                addedNode<-TRUE
                edgeIndex<-0
                while(edgeIndex<nrow(vedges)) {
                  print("trying to add nodes on edges")   
                  
                  #print("add node on edges")
                  addedNode<-TRUE
                  
                  # find envelopes for first edge only
                  avgDT6
                  edgeIndex<-edgeIndex+1
                  # print(vedges[edgeIndex,]$from)
                  # print(vedges[edgeIndex,]$to)
                  
                  
                  cluster
                  
                  centers<-as.matrix(rbind(circlesTable[vid %in% vedges[edgeIndex,]$from,c("lon","lat"),],
                                           circlesTable[vid %in% vedges[edgeIndex,]$to,c("lon","lat"),]),rownames.force=NA)
                  colnames(centers) <- NULL
                  
                  r<-unlist(unname(c(circlesTable[vid %in% vedges[edgeIndex,]$from,c("circleRadius"),],
                                     circlesTable[vid %in% vedges[edgeIndex,]$to,c("circleRadius"),])))
                  
                  r
                  
                  tryCatch({
                    # library(PlotRegionHighlighter)
                    # library(sp)
                    # envelope <- generatePolygonEnvelope(AFMImageNetworksAnalysis, centers, r)
                    # pointsInsideEnvelopeDT<-getAllPointsToRemove(AFMImageNetworksAnalysis, envelope)
                    # colnames(pointsInsideEnvelopeDT)<-c("vid","lat","lon")
                    # pointsInsideEnvelopeDT
                    
                    envelopeToBeRemoved <- generatePolygonEnvelope(AFMImageNetworksAnalysis, centers, r*RADIUS_MULTIPLIER)
                    pointsInsideEnvelopeToBeRemovedDT<-getAllPointsToRemove(AFMImageNetworksAnalysis, envelopeToBeRemoved)
                    colnames(pointsInsideEnvelopeToBeRemovedDT)<-c("vid","lat","lon")
                    pointsInsideEnvelopeToBeRemovedDT
                    
                    print("Removing edges envelopes for edges")
                    # print(centers)
                    # print(r*RADIUS_MULTIPLIER)
                    # add enveloppe to be removed
                    pointsInsideEnvelopesToBeRemovedDT<-rbind(pointsInsideEnvelopesToBeRemovedDT, pointsInsideEnvelopeToBeRemovedDT)
                    
                    print("start spliting segment regularly...")
                    edgesProcessed<-c(edgesProcessed, vedges[edgeIndex,]$fromto_cluster)
                    
                    clusterChar = data.table (
                      cluster = vedges[edgeIndex,]$fromto_cluster,
                      minLon = min(pointsInsideEnvelopeToBeRemovedDT$lon),
                      maxLon = max(pointsInsideEnvelopeToBeRemovedDT$lon),
                      minLat = min(pointsInsideEnvelopeToBeRemovedDT$lat),
                      maxLat = max(pointsInsideEnvelopeToBeRemovedDT$lat))
                    
                    
                    clusterChar$area<-(clusterChar$maxLat-clusterChar$minLat)*(clusterChar$maxLon-clusterChar$minLon)
                    #clusterChar$count<-islandsDT[,.N, by=c(cluster]$N
                    
                    clusterChar$shape<-sapply(1:nrow(clusterChar),function(i) {
                      if ((clusterChar[i,]$maxLon-clusterChar[i,]$minLon)>(clusterChar[i,]$maxLat-clusterChar[i,]$minLat)) {
                        return("width")
                      }else{
                        return("height")
                      }
                    })
                    print(clusterChar)
                    
                    
                    #i<-1
                    # split regularly on the edge
                    # only one edge no need of lapply
                    resDT60<-lapply(1:nrow(clusterChar),function(i,pointsInsideEnvelopeDT, circlesMatrixAFMImage, clusterChar, maxFilter, centers, r) {
                      print(i)
                      totalHeight<-clusterChar[i,]$maxLat-clusterChar[i,]$minLat
                      #print(totalHeight)
                      totalWidth<-clusterChar[i,]$maxLon-clusterChar[i,]$minLon
                      #print(totalWidth)
                      
                      # only on cluster...
                      totalHeight<-abs(centers[1,2]-centers[2,2])
                      #print(totalHeight)
                      totalWidth<-abs(centers[1,1]-centers[2,1])
                      #print(totalWidth)
                      clusterN<-clusterChar[i,]$cluster
                      #print(clusterN)
                      
                      
                      
                      
                      if(((clusterChar[i,]$shape == "height")&totalHeight<maxFilter)|
                         ((clusterChar[i,]$shape == "width")&totalWidth<maxFilter)) {
                        # taking the barycenter
                        tempResDT2<-copy(pointsInsideEnvelopeDT)
                        tempResDT2$cluster<-rep(1, nrow(tempResDT2))
                        tempResDT2[, meanLon:=mean(lon), by=cluster]
                        tempResDT2[, meanLat:=mean(lat), by=cluster]
                        tempResDT2[, dist:=sqrt((lon-meanLon)^2+(lat-meanLat)^2), by=cluster]
                        tempResDT2[, minDistance:=min(dist), by=cluster]
                        tempResDT2[dist == minDistance, c("lon","lat","cluster"),]
                        tempResDT2<-unique(tempResDT2[dist == minDistance, c("lon","lat","cluster"),],by="cluster",fromLast=TRUE)
                        tempResDT2
                        
                        # circleRadius<-circlesMatrixAFMImage@data[circlesMatrixAFMImage@data$y %in% (circlesMatrixAFMImage@vscansize*(tempResDT2$lat-1)/circlesMatrixAFMImage@samplesperline) &
                        #                                             circlesMatrixAFMImage@data$x %in% (circlesMatrixAFMImage@hscansize*(tempResDT2$lon-1)/circlesMatrixAFMImage@lines)]$h
                        # print(circleRadius)
                        circleRadius<-rep(c(maxFilter), times=as.integer(nrow(tempResDT2)))
                        
                        return(data.table(cluster=clusterN, lon= tempResDT2$lon, lat= tempResDT2$lat, circleRadius= circleRadius))
                        
                      }else {
                        minLat<-min(c(centers[1,2],centers[2,2]))
                        maxLat<-max(c(centers[1,2],centers[2,2]))
                        minLon<-min(c(centers[1,1],centers[2,1]))
                        maxLon<-max(c(centers[1,1],centers[2,1]))
                        
                        theta<-atan2((maxLon-minLon),(maxLat-minLat))
                        hypothenuse<-maxFilter
                        
                        # regularly spaced points depending on circleRadius
                        if (clusterChar[i,]$shape == "height") {
                          # number
                          if (cos(theta)!=0) {
                            regularSpace<-hypothenuse/cos(theta)
                          }else{
                            regularSpace<-hypothenuse/sin(theta)
                          }
                          numberOfIntermediaryPoints<-floor(totalHeight/regularSpace-1)
                          if (numberOfIntermediaryPoints<=0) {
                            numberOfIntermediaryPoints<-0
                            vectorOfLat<-c(clusterChar[i,]$minLat, clusterChar[i,]$maxLat)
                            vectorOfLat
                          }else{
                            #regularSpace<-floor(totalHeight/(numberOfIntermediaryPoints+1))
                            vectorOfLat<-seq(from = (minLat+regularSpace), to = (minLat+totalHeight-regularSpace) , by=regularSpace)
                            vectorOfLat<-round(vectorOfLat,0)
                            vectorOfLat
                          }
                          
                          # use the medium horizontal position for each vectorOfLat
                          resDT5<-lapply(1:length(vectorOfLat),function(j, pointsInsideEnvelopeDT, vectorOfLat, clusterN) {
                            resDT2<-pointsInsideEnvelopeDT[lat %in% vectorOfLat[j],]
                            #print(resDT2)
                            avgDTLon<-floor(mean(resDT2$lon))
                            #print(paste("cluster ", clusterN, "keep ",avgDTLon, vectorOfLat[j] ))
                            circleRadius<-circlesMatrixAFMImage@data[circlesMatrixAFMImage@data$y %in% (circlesMatrixAFMImage@vscansize*(vectorOfLat[j]-1)/circlesMatrixAFMImage@samplesperline) &
                                                                       circlesMatrixAFMImage@data$x %in% (circlesMatrixAFMImage@hscansize*(avgDTLon-1)/circlesMatrixAFMImage@lines)]$h
                            circleRadius
                            
                            return(data.table(cluster=clusterN, lon = avgDTLon, lat= vectorOfLat[j], circleRadius= circleRadius))
                          },pointsInsideEnvelopeDT, vectorOfLat, clusterChar[i,]$cluster)
                          
                          return(rbindlist(resDT5))
                          
                        }else{
                          if (cos(theta)!=0) {
                            regularSpace<-hypothenuse/sin(theta)
                          }else{
                            regularSpace<-hypothenuse/cos(theta)
                          }
                          
                          numberOfIntermediaryPoints<-floor(totalWidth/regularSpace-1)
                          
                          if (numberOfIntermediaryPoints<=0) {
                            numberOfIntermediaryPoints<-0
                            vectorOfLon<-c(minLon, maxLon)
                            vectorOfLon
                          }else{
                            #regularSpace<-floor(totalWidth/(numberOfIntermediaryPoints+1))
                            vectorOfLon<-seq(from = (minLon+regularSpace), to = (maxLon-regularSpace) , by=regularSpace)
                            #vectorOfLon<-c(minLon,vectorOfLon, maxLon)
                            vectorOfLon<-round(vectorOfLon,0)
                            vectorOfLon
                          }
                          #j=1
                          resDT5<-lapply(1:length(vectorOfLon),function(j, pointsInsideEnvelopeDT, vectorOfLon, clusterN) {
                            resDT2<-pointsInsideEnvelopeDT[lon %in% vectorOfLon[j],]
                            #print(resDT2)
                            avgDTLat<-floor(mean(resDT2$lat))
                            #print(paste("cluster ", clusterN, "keep ",avgDTLon, vectorOfLon[j] ))
                            circleRadius<-circlesMatrixAFMImage@data[circlesMatrixAFMImage@data$y %in% (circlesMatrixAFMImage@vscansize*(avgDTLat-1)/circlesMatrixAFMImage@samplesperline) &
                                                                       circlesMatrixAFMImage@data$x %in% (circlesMatrixAFMImage@hscansize*(vectorOfLon[j]-1)/circlesMatrixAFMImage@lines)]$h
                            circleRadius
                            
                            return(data.table(cluster=clusterN, lon = vectorOfLon[j], lat= avgDTLat, circleRadius= circleRadius))
                          },pointsInsideEnvelopeDT, vectorOfLon, clusterChar[i,]$cluster)
                          return(rbindlist(resDT5))
                          
                        }
                      }
                      
                    }, pointsInsideEnvelopeToBeRemovedDT, circlesMatrixAFMImage, clusterChar,maxFilter=max(r), centers, r)
                    
                    resDT60<-rbindlist(resDT60)
                    resDT60<-unique(resDT60)
                    resDT60<-resDT60[complete.cases(resDT60),]
                    resDT60<-resDT60[lon != 0 & lon != (AFMImageNetworksAnalysis@binaryAFMImage@samplesperline-1) & lat!=0 & lat!=(AFMImageNetworksAnalysis@binaryAFMImage@lines-1) ,]
                    #resDT60<-resDT60[circleRadius!=0]
                    print("ok")
                    resDT60$keep<-rep(TRUE,nrow(resDT60))
                    resDT60$vid<-as.character(getVertexId(AFMImageNetworksAnalysis@binaryAFMImage,resDT60$lon, resDT60$lat))
                    print("nodes added on one edge")
                    print(resDT60)
                    
                    resDT60$circleRadius<-rep(max(r),nrow(resDT60))
                    print(resDT60)
                    
                    
                    # add nodes regularly in enveloppe
                    avgDT6<-rbind(avgDT6, resDT60)
                    avgDT6
                    
                    # merge cluster for one vid
                    # the cluster name is sorted
                    for (avid in avgDT6[duplicated(avgDT6,by=c("vid"))]$vid){
                      allCluster<-unlist(strsplit(avgDT6[vid %in% avid,]$cluster," "))
                      mergeCluster<-unique(allCluster)
                      finalCluster=paste(mergeCluster[order(mergeCluster)],collapse=" ")
                      print(finalCluster)
                      avgDT6[vid %in% avid,]$cluster<-finalCluster
                    }
                    for (avid in avgDT6[duplicated(avgDT6,by=c("vid"))]$vid){
                      pointDT<-avgDT6[vid %in% avid,]
                      
                      # circleRadius<-circlesMatrixAFMImage@data[circlesMatrixAFMImage@data$y %in% (circlesMatrixAFMImage@vscansize*pointDT$lat/circlesMatrixAFMImage@samplesperline) &
                      #                                            circlesMatrixAFMImage@data$x %in% (circlesMatrixAFMImage@hscansize*pointDT$lon/circlesMatrixAFMImage@lines)]$h
                      circleRadius<-max(avgDT6[vid %in% avid,]$circleRadius)
                      avgDT6[vid %in% avid,]$circleRadius<-circleRadius
                    }
                    
                    avgDT6<-unique(avgDT6)
                    #print(avgDT6)
                    # Manage addition and removal of nodes
                    avgDT7<-copy(resDT60)
                    newCircleAFMImage2<-addNode(newCircleAFMImage2, avgDT7,filterIndex)
                    
                    avgDT7$circleRadius<-avgDT7$circleRadius*RADIUS_MULTIPLIER
                    newCircleAFMImage<-removeNode(newCircleAFMImage, avgDT7) #, RADIUS_MULTIPLIER, BIGGER_CIRCLE_RADIUS, BIGGER_CIRCLE_RADIUS_MULTILPLIER)
                    maxFilter
                    
                    # remove all the envelopes
                    # displayIn3D(newCircleAFMImage, noLight=TRUE)
                    # displayIn3D(newCircleAFMImage2, noLight=TRUE)
                    
                    
                    avgDT<-rbind(avgDT, resDT60)
                    avgDT
                    
                    #newCircleAFMImage@data$h[(pointsInsideEnvelopeDT$coords.x2-1)*newCircleAFMImage@samplesperline+(pointsInsideEnvelopeDT$coords.x1)]<-0
                    #        circlesMatrixAFMImage@data$h[((islandsDT[,1])-1)*circlesMatrixAFMImage@samplesperline + islandsDT[,2]]
                  }, error = function(e) {
                    print("extra nodes error 1")
                    print(e)
                    warning(paste("extra nodes error 1",e))
                  }, finally = {
                    #print("extra nodes identified")
                  })
                  
                }
                
              }
              
              if (!addedNode) print("no more edge to analyse")
            }
            
            # remove enveloppes from nodes
            print(pointsInsideEnvelopesToBeRemovedDT)
            #TBD good 
            #newCircleAFMImage@data$h[pointsInsideEnvelopesToBeRemovedDT$coords.x2+1+(pointsInsideEnvelopesToBeRemovedDT$coords.x1)*newCircleAFMImage@samplesperline]<-0
            newCircleAFMImage@data$h[pointsInsideEnvelopesToBeRemovedDT$lon+1+(pointsInsideEnvelopesToBeRemovedDT$lat)*newCircleAFMImage@samplesperline]<-0
            
          }else{
            if (is.list(circlesTable) & nrow(circlesTable) == 1) {
              # Manage addition and removal of nodes
              avgDT7<-copy(avgDT6)
              avgDT7$circleRadius<-avgDT7$circleRadius*RADIUS_MULTIPLIER
              newCircleAFMImage<-removeNode(newCircleAFMImage, avgDT7) #, RADIUS_MULTIPLIER, BIGGER_CIRCLE_RADIUS, BIGGER_CIRCLE_RADIUS_MULTILPLIER)
              newCircleAFMImage2<-addNode(newCircleAFMImage2, avgDT6,filterIndex)
              
              # remove enveloppes from nodes
              print(pointsInsideEnvelopesToBeRemovedDT)
              #TBD good 
              #newCircleAFMImage@data$h[pointsInsideEnvelopesToBeRemovedDT$coords.x2+1+(pointsInsideEnvelopesToBeRemovedDT$coords.x1)*newCircleAFMImage@samplesperline]<-0
              newCircleAFMImage@data$h[pointsInsideEnvelopesToBeRemovedDT$lon+1+(pointsInsideEnvelopesToBeRemovedDT$lat)*newCircleAFMImage@samplesperline]<-0
            }
            
          }
          #displayIn3D(newCircleAFMImage, noLight=FALSE)
          #displayIn3D(newCircleAFMImage2, noLight=FALSE)  
        }
      }
    }
  }
  #}
  print(filterIndex)
  avgDT<-avgDT[-1]
  
  
  #displayIn3D(AFMImageNetworksAnalysis@binaryAFMImage, noLight=FALSE)
  #displayIn3D(newCircleAFMImage, noLight=FALSE)
  #displayIn3D(newCircleAFMImage2, noLight=FALSE)
  
  
  AFMImageNetworksAnalysis@smallBranchesTreatment<-FALSE
  
  # small branches treatment
  if (AFMImageNetworksAnalysis@smallBranchesTreatment) {
    # newCircleAFMImage<-copy(AFMImageNetworksAnalysis@binaryAFMImage)
    # displayIn3D(newCircleAFMImage, noLight=FALSE)
    
    # remove the edge of the image
    edgeWidth<-listOfFilters[1]
    
    removedEdgeData<-copy(newCircleAFMImage@data)
    removedEdgeData[removedEdgeData$x<edgeWidth*newCircleAFMImage@hscansize/newCircleAFMImage@samplesperline]<-0
    removedEdgeData[removedEdgeData$y<edgeWidth*newCircleAFMImage@vscansize/newCircleAFMImage@lines]<-0
    removedEdgeData[removedEdgeData$x>(1-edgeWidth/newCircleAFMImage@samplesperline)*newCircleAFMImage@hscansize]<-0
    removedEdgeData[removedEdgeData$y>(1-edgeWidth/newCircleAFMImage@lines)*newCircleAFMImage@vscansize]<-0
    newCircleAFMImage@data<-copy(removedEdgeData)
    #displayIn3D(newCircleAFMImage, noLight=FALSE)
    
    # finding the extra small nodes
    untreatedPoints<-newCircleAFMImage@data[h!=0]
    
    islandsDT<-cbind(lon=1+untreatedPoints$y*newCircleAFMImage@lines/newCircleAFMImage@vscansize, 
                     lat=1+untreatedPoints$x*newCircleAFMImage@samplesperline/newCircleAFMImage@hscansize)
    
    if (nrow(islandsDT)>0) {
      DBSCAN <- dbscan(islandsDT, eps = 1.5, MinPts = 3, borderPoints=FALSE)
      #plot(untreatedPoints$y, untreatedPoints$x, col = DBSCAN$cluster, pch = 20)
      #plot(islandsDT, col = DBSCAN$cluster, pch = 20)
      
      islandsDT<-data.table(islandsDT,cluster=DBSCAN$cluster)
      setkeyv(islandsDT, "cluster")
      
      isolatedIslandsDT<-islandsDT[cluster==0,]
      isolatedIslandsDT
      islandsDT<-islandsDT[cluster!=0,]
      islandsDT
      
      if (nrow(islandsDT)>0) {
        #clusterN<-1
        identifyLinksBetweenClustersAndExistingNodes<-function(clusterN, AFMImageNetworksAnalysis, MAX_DISTANCE, avgDT, islandsDT) {
          requireNamespace("data.table")
          requireNamespace("sp")
          requireNamespace("AFM")
          
          clusterLon<-clusterLat<-cluster<-IDX<-keepThinPoints<-meandist<-NULL
          
          #print(clusterN)
          resDT<-data.table(cluster=c(0), clusterLon=c(0), clusterLat=c(0), existingNodeLon=c(0), existingNodeLat=c(0))
          centers1<-islandsDT[islandsDT$cluster %in% clusterN,]
          
          
          # define the points in the circle
          otherNodes<-copy(avgDT)
          
          for (center_index in seq(1,nrow(centers1))) {
            center1<-centers1[center_index,]
            circleRadius1<-1
            #otherNodes<-allNodesAsSpatialPoints[!(allNodesAsSpatialPoints$coords.x1==center1$lon&allNodesAsSpatialPoints$coords.x2==center1$lat)]
            
            minLat<- ifelse((center1$lat-MAX_DISTANCE)>0, center1$lat-MAX_DISTANCE, 0)
            maxLat<- ifelse((center1$lat+MAX_DISTANCE)<AFMImageNetworksAnalysis@binaryAFMImage@lines, center1$lat+MAX_DISTANCE, AFMImageNetworksAnalysis@binaryAFMImage@lines-1)
            
            minLon<- ifelse((center1$lon-MAX_DISTANCE)>0, center1$lon-MAX_DISTANCE, 0)
            maxLon<- ifelse((center1$lon+MAX_DISTANCE)<AFMImageNetworksAnalysis@binaryAFMImage@samplesperline, center1$lon+MAX_DISTANCE, AFMImageNetworksAnalysis@binaryAFMImage@samplesperline-1)
            
            otherNodes2<-copy(avgDT[lon>=minLon&lon<=maxLon&lat>=minLat&lat<=maxLat,])
            otherNodes2$dist<-sp::spDistsN1(pts=matrix(c(otherNodes2$lon, otherNodes2$lat), ncol=2), pt=c(center1$lon, center1$lat), longlat=FALSE)
            #otherNodes
            
            otherNodes2<-otherNodes2[with(otherNodes2, order(otherNodes2$dist)), ]
            otherNodes2<-otherNodes2[otherNodes2$dist<MAX_DISTANCE,]
            
            if (nrow(otherNodes2)>0) {
              for (centerId2Nb in seq(1, nrow(otherNodes2))) {
                pt<-otherNodes2[centerId2Nb,]
                # print(center1)
                # print(circleRadius1)
                # print(pt)
                if (AreNodesConnected(AFMImageNetworksAnalysis@binaryAFMImage, center1, circleRadius1, data.table(lon=pt$lon, lat=pt$lat), pt$circleRadius)) {
                  #print("yes")
                  resDT=rbind(resDT, data.table(cluster=clusterN, clusterLon=center1$lon, clusterLat=center1$lat, existingNodeLon=pt$lon, existingNodeLat=pt$lat))
                }
              }
            }
          }
          resDT<-resDT[-1,]
          return(resDT)
        }
        
        print(paste("number of nodes=", nrow(avgDT)))
        print(paste("number of clusters=", length(unique(islandsDT$cluster))))
        
        
        #MB TODO
        # more in the width or in the height ?
        print("start spliting segment regularly for small branches...")
        clusterChar = data.table (
          cluster = unique(islandsDT$cluster),
          minLon = islandsDT[, min(lon), by=cluster]$V1,
          maxLon = islandsDT[, max(lon), by=cluster]$V1,
          minLat = islandsDT[, min(lat), by=cluster]$V1,
          maxLat = islandsDT[, max(lat), by=cluster]$V1)
        clusterChar$area<-(clusterChar$maxLat-clusterChar$minLat)*(clusterChar$maxLon-clusterChar$minLon)
        clusterChar$count<-islandsDT[,.N, by=cluster]$N
        
        clusterChar$shape<-sapply(1:nrow(clusterChar),function(i) {
          if ((clusterChar[i,]$maxLon-clusterChar[i,]$minLon)>(clusterChar[i,]$maxLat-clusterChar[i,]$minLat)) {
            return("width")
          }else{
            return("height")
          }
        })
        print(clusterChar)
        
        # TODO
        print("calculate extra node from edge")
        rm(resDT6)
        i=3
        #i=6
        resDT6<-lapply(1:nrow(clusterChar),function(i,islandsDT, clusterChar, maxFilter,AREA_MIN,CLUSTER_COUNT_MIN) {
          meanLon<-meanLat<-NULL
          
          print(i)
          totalHeight<-clusterChar[i,]$maxLat-clusterChar[i,]$minLat
          #print(totalHeight)
          totalWidth<-clusterChar[i,]$maxLon-clusterChar[i,]$minLon
          #print(totalWidth)
          
          #if ((clusterChar[i,]$area <= AREA_MIN)|(clusterChar[i,]$count <= CLUSTER_COUNT_MIN)) {
          if(((clusterChar[i,]$shape == "height")&totalHeight<maxFilter)|
             ((clusterChar[i,]$shape == "width")&totalWidth<maxFilter)) {
            
            
            # # if sample not extremely small
            # if (!clusterChar[i,]$count <= 3) {
            # taking the barycenter is useless because of low number of points
            # sample only one point if the cluster is with small area
            clusterN<-clusterChar[i,]$cluster
            resDT2<-islandsDT[cluster %in% clusterN,]
            #print(resDT2)
            #sampleC<-sample(1:nrow(resDT2),1)
            
            
            tempBarycenterDT<-copy(resDT2)
            tempBarycenterDT[, meanLon:=mean(lon), by=cluster]
            tempBarycenterDT[, meanLat:=mean(lat), by=cluster]
            tempBarycenterDT[, dist:=sqrt((lon-meanLon)^2+(lat-meanLat)^2), by=cluster]
            tempBarycenterDT[, minDistance:=min(dist), by=cluster]
            tempBarycenterDT[dist == minDistance, c("lon","lat","cluster"),]
            tempBarycenterDT<-unique(tempBarycenterDT[dist == minDistance, c("lon","lat","cluster"),],by="cluster",fromLast=TRUE)
            #tempBarycenterDT<-unique(islandsDT,by="cluster",fromLast=TRUE)
            tempBarycenterDT
            
            
            return(data.table(cluster=clusterN, lon = tempBarycenterDT$lon, lat=  tempBarycenterDT$lat))
            #return(data.table(cluster=clusterN, lon = resDT2[sampleC,]$lon, lat=  resDT2[sampleC,]$lat))
            # }
          }else {
            # regularly spaced points depending on circleRadius
            if (clusterChar[i,]$shape == "height") {
              # number
              #sSample = floor(SAMPLE_ON_THIN_PORTIONS*totalHeight/100)
              numberOfIntermediaryPoints<-floor(totalHeight/maxFilter-2)
              if (numberOfIntermediaryPoints<=0) {
                numberOfIntermediaryPoints<-0
                vectorOfLat<-c(clusterChar[i,]$minLat, clusterChar[i,]$maxLat)
                vectorOfLat
              }else{
                regularSpace<-floor(totalHeight/(numberOfIntermediaryPoints+1))
                vectorOfLat<-seq(from = (clusterChar[i,]$minLat+regularSpace), to = (clusterChar[i,]$minLat+totalHeight-regularSpace) , by=regularSpace)
                vectorOfLat<-c(clusterChar[i,]$minLat,vectorOfLat, clusterChar[i,]$maxLat)
                vectorOfLat
              }
              
              # use the medium horizontal position for each vectorOfLat
              resDT5<-lapply(1:length(vectorOfLat),function(j, islandsDT, vectorOfLat, clusterN) {
                resDT2<-islandsDT[lat %in% vectorOfLat[j] & cluster %in% clusterN,]
                #print(resDT2)
                avgDTLon<-floor(mean(resDT2$lon))
                #print(paste("cluster ", clusterN, "keep ",avgDTLon, vectorOfLat[j] ))
                return(data.table(cluster=clusterN, lon = avgDTLon, lat= vectorOfLat[j]))
              },islandsDT, vectorOfLat, clusterChar[i,]$cluster)
              
              #print(resDT5)
              #print(rbindlist(resDT5))
              return(rbindlist(resDT5))
              
            }else{
              numberOfIntermediaryPoints<-floor(totalWidth/maxFilter-2)
              if (numberOfIntermediaryPoints<=0) {
                numberOfIntermediaryPoints<-0
                vectorOfLon<-c(clusterChar[i,]$minLon, clusterChar[i,]$maxLon)
                vectorOfLon
              }else{
                regularSpace<-floor(totalWidth/(numberOfIntermediaryPoints+1))
                vectorOfLon<-seq(from = (clusterChar[i,]$minLon+regularSpace), to = (clusterChar[i,]$minLon+totalWidth-regularSpace) , by=regularSpace)
                vectorOfLon<-c(clusterChar[i,]$minLon,vectorOfLon, clusterChar[i,]$maxLon)
                vectorOfLon
              }
              #j=1
              resDT5<-lapply(1:length(vectorOfLon),function(j, islandsDT, vectorOfLon, clusterN) {
                resDT2<-islandsDT[lon %in% vectorOfLon[j] & cluster %in% clusterN,]
                #print(resDT2)
                avgDTLat<-floor(mean(resDT2$lat))
                #print(paste("cluster ", clusterN, "keep ",avgDTLon, vectorOfLon[j] ))
                return(data.table(cluster=clusterN, lon = vectorOfLon[j], lat= avgDTLat))
              },islandsDT, vectorOfLon, clusterChar[i,]$cluster)
              
              #print(resDT5)
              #print(rbindlist(resDT5))
              return(rbindlist(resDT5))
            }
          }
          
        }, islandsDT, clusterChar,maxFilter=1,AREA_MIN,CLUSTER_COUNT_MIN)
        
        resDT6<-rbindlist(resDT6)
        resDT6<-unique(resDT6)
        resDT6<-resDT6[complete.cases(resDT6),]
        resDT6<-resDT6[lon != 0 & lon != (AFMImageNetworksAnalysis@binaryAFMImage@samplesperline-1) & lat!=0 & lat!=(AFMImageNetworksAnalysis@binaryAFMImage@lines-1) ,]
        resDT6
        print("ok")
        
        resDT6$keep<-rep(TRUE, nrow(resDT6))
        resDT6$circleRadius=rep(0, nrow(resDT6))
        resDT6$vid<-as.character(getVertexId(AFMImageNetworksAnalysis@binaryAFMImage,resDT6$lon, resDT6$lat))
        
        avgDT<-rbind(avgDT, data.table(lon = resDT6$lon, lat = resDT6$lat, circleRadius=resDT6$circleRadius, 
                                       keep=resDT6$keep, vid=resDT6$vid))
        avgDT
        
      }
    }
  }
  print(avgDT)
  
  for (avid in avgDT[duplicated(avgDT,by=c("vid"))]$vid){
    allCluster<-unlist(strsplit(avgDT[vid %in% avid,]$cluster," "))
    mergeCluster<-unique(allCluster)
    finalCluster=paste(mergeCluster[order(mergeCluster)],collapse=" ")
    print(finalCluster)
    avgDT[vid %in% avid,]$cluster<-finalCluster
  }
  for (avid in avgDT[duplicated(avgDT,by=c("vid"))]$vid){
    pointDT<-avgDT[vid %in% avid,]
    circleRadius<-circlesMatrixAFMImage@data[circlesMatrixAFMImage@data$y %in% (circlesMatrixAFMImage@vscansize*pointDT$lat/circlesMatrixAFMImage@samplesperline) &
                                               circlesMatrixAFMImage@data$x %in% (circlesMatrixAFMImage@hscansize*pointDT$lon/circlesMatrixAFMImage@lines)]$h
    avgDT[vid %in% avid,]$circleRadius<-circleRadius
  }
  
  avgDT<-unique(avgDT)
  print(avgDT)
  
  AFMImageNetworksAnalysis@binaryAFMImage<-copy(binaryAFMImage)
  AFMImageNetworksAnalysis@binaryAFMImageWithCircles<-copy(newCircleAFMImage2)
  AFMImageNetworksAnalysis@circlesTable<-copy(unique(avgDT))
  
  return(AFMImageNetworksAnalysis)  
}



#' display the network of nodes and edges
#' 
#' @param ... cl: a cluster object from the parallel package
#' @param AFMImageNetworksAnalysis a \code{\link{AFMImageNetworksAnalysis}}
#' @param MAX_DISTANCE the maximum distance between nodes to check if nodes are connected. Default value is 40.
#' @export
#' @author M.Beauvais
identifyEdgesFromCircles<-function(...,AFMImageNetworksAnalysis, MAX_DISTANCE=40) {
  print("BOOOM")
  force(AFMImageNetworksAnalysis)
  keep<-fromto<-NULL
  
  args<-names(list(...))
  print(args)
  if (is.null(args)) {
    clExist<-FALSE
  }else{
    clExist<-c(match('cl',args)!=-1)
    cl<-cl
  }
  
  
  if (clExist) {
    print("using parallel")
    requireNamespace("parallel")
  }
  
  from<-to<-NULL
  alledges<-c()
  allvertices<-c()
  
  # for all the nodes of the future networks
  
  # for all the points in the circles in the plot
  # identify if a link is available with all the other nodes
  identifyLinksBetweenNodes<-function(k, AFMImageNetworksAnalysis, MAX_DISTANCE, allPossibleEdges) {
    requireNamespace("data.table")
    requireNamespace("sp")
    requireNamespace("parallel")
    requireNamespace("AFM") #TODO
    #print(paste("k",k))
    keep<-NULL
    currentEdge<-allPossibleEdges[,k]
    
    
    centerId<-currentEdge[1]
    otherCenterId<-currentEdge[2]
    
    #!
    circlesTable<-AFMImageNetworksAnalysis@circlesTable[keep %in% c(TRUE),]
    
    vedges<-data.table(from = c(""), to = c(""),arrows = c("to"))
    
    center1= circlesTable[centerId,]
    circleRadius1=circlesTable[centerId,]$circleRadius
    vid1<-getVertexId(AFMImageNetworksAnalysis@binaryAFMImage,center1$lon, center1$lat)
    
    
    otherNodes<-circlesTable[otherCenterId,]
    otherNodes$dist<-sp::spDistsN1(pts=matrix(c(otherNodes$lon, otherNodes$lat), ncol=2), pt=c(center1$lon, center1$lat), longlat=FALSE)
    otherNodes<-otherNodes[with(otherNodes, order(otherNodes$dist)), ]
    otherNodes<-otherNodes[otherNodes$dist<MAX_DISTANCE,]
    
    
    
    if (nrow(otherNodes)!=0){
      #print(otherNodes)
      #centerId2Nb<-1
      #centerId2Nb<-10
      pt<-otherNodes[1,]
      vid2<-getVertexId(AFMImageNetworksAnalysis@binaryAFMImage,pt$lon, pt$lat)
      
      # if (k == 512) {
      #   print("******************** 512")
      #   print(center1)
      #   print(circleRadius1)
      #   print(pt)
      # }
      
      if (AreNodesConnected(AFMImageNetworksAnalysis@binaryAFMImage, center1, circleRadius1, data.table(lon=pt$lon, lat=pt$lat), pt$circleRadius)) {
        #print(paste("segment exists",center1$lon, center1$lat,":",pt$coords.x1, pt$coords.x2))
        vedges<-rbind(vedges, data.table(from = vid1, to = vid2,arrows = c("to")))
        #displaygridIgraphPlotFromEdges(AFMImageNetworksAnalysis@binaryAFMImage, edges=vedges[-1,],  isolates = c())
      }
    }else{
      #print("node too far")
    }
    return(vedges[-1,])
  }
  
  circlesTable<-data.table(copy(AFMImageNetworksAnalysis@circlesTable[keep %in% c(TRUE),]))
  circlesTable$vid<-as.character(getVertexId(AFMImageNetworksAnalysis@binaryAFMImage,circlesTable$lon, circlesTable$lat))
  circlesTable
  
  print(paste("number of circles=", nrow(circlesTable)))
  
  
  if (is.list(circlesTable) & nrow(circlesTable) > 1) {
    allPossibleEdges<-combn(1:nrow(circlesTable), 2, simplify = TRUE)
    print(paste("number of possible edges=", ncol(allPossibleEdges)))
    
    
    start.time <- Sys.time()
    print(start.time)
    if(clExist) {
      print("using parallel for identifyLinksBetweenNodes")
      #cl<-cl
      parallel::clusterEvalQ(cl , c(library("data.table"),library("sp"), library("AFM"),library("parallel")))
      parallel::clusterExport(cl, c("AFMImageNetworksAnalysis","MAX_DISTANCE","allPossibleEdges"), envir=environment())
      res<-parallel::parLapply(cl, 1:ncol(allPossibleEdges),identifyLinksBetweenNodes, AFMImageNetworksAnalysis, MAX_DISTANCE,allPossibleEdges)
    }else{
      res<-lapply(1:ncol(allPossibleEdges),identifyLinksBetweenNodes, AFMImageNetworksAnalysis, MAX_DISTANCE,allPossibleEdges)
    }
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    vedges<-rbindlist(res)
    print(vedges)
    print(paste0("time.taken: ",time.taken))
    
    # new treatment
    allEdges<-copy(vedges)
    allEdges
    allEdges$from.coords.x1<-sapply(1:nrow(allEdges),function(i) getCoordinatesFromVertexId( allEdges[i,c("from"),with=FALSE])$coords.x1)
    allEdges$from.coords.x2<-sapply(1:nrow(allEdges),function(i) getCoordinatesFromVertexId(allEdges[i,c("from"),with=FALSE])$coords.x2)
    allEdges$to.coords.x1<-sapply(1:nrow(allEdges),function(i) getCoordinatesFromVertexId(allEdges[i,c("to"),with=FALSE])$coords.x1)
    allEdges$to.coords.x2<-sapply(1:nrow(allEdges),function(i) getCoordinatesFromVertexId(allEdges[i,c("to"),with=FALSE])$coords.x2)
    allEdges$dist<-sapply(1:nrow(allEdges),function(i) sp::spDistsN1(pts=as.matrix(cbind(allEdges[i,]$from.coords.x1,allEdges[i,]$from.coords.x2)),
                                                                     pt=as.matrix(cbind(allEdges[i,]$to.coords.x1,allEdges[i,]$to.coords.x2)),longlat=FALSE))
    
    allEdges$keep<-rep(TRUE, nrow(allEdges))
    
    newcirclesTable<-copy(circlesTable[,c("vid","circleRadius"),])
    
    setkey(allEdges, from)
    colnames(newcirclesTable)<-c("from","from_circleRadius")
    setkey(newcirclesTable,from)
    allEdges<-merge(allEdges, newcirclesTable,all.x = TRUE)
    allEdges
    
    setkey(allEdges, to)
    colnames(newcirclesTable)<-c("to","to_circleRadius")
    setkey(newcirclesTable,to)
    allEdges<-merge(allEdges, newcirclesTable,all.x = TRUE)
    #print(allEdges)
    
    
    allVertices<-unique(c(allEdges$from, allEdges$to))
    #print(allVertices)
    
    # for each edge (u, v):
    #   for each vertex w:
    #   if (v, w) is an edge and (w, u) is an edge return true
    #   else return false
    setkey(allEdges, from, to)
    totalNumberOfEdges<-nrow(allEdges)
    print(paste0("totalNumberOfEdges=",totalNumberOfEdges))
    
    print("simplify network")
    if(clExist) {
      allEdges<-simplifyNetwork(cl=cl, allVertices=allVertices, allEdges=allEdges)
    }else{
      allEdges<-simplifyNetwork(allVertices=allVertices, allEdges=allEdges)
    }
    # allEdges
    # allEdges[keep %in% c(FALSE),]
    print("simplify ended")
    # displaygridIgraphPlotFromEdges(AFMImageNetworksAnalysis@binaryAFMImage,
    #                                allEdges,
    #                                AFMImageNetworksAnalysis@isolatedNodesList)
    # 
    # displaygridIgraphPlotFromEdges(AFMImageNetworksAnalysis@binaryAFMImage,
    #                                allEdges[keep %in% c(TRUE),],
    #                                AFMImageNetworksAnalysis@isolatedNodesList)
    
    mn <- pmin(allEdges$to, allEdges$from)
    mx <- pmax(allEdges$to, allEdges$from)
    int <- as.numeric(interaction(mn, mx))
    allEdges<-allEdges[match(unique(int), int),]
    
    AFMImageNetworksAnalysis@edgesTable<-copy(allEdges)
    AFMImageNetworksAnalysis@edgesTable<-AFMImageNetworksAnalysis@edgesTable[dist !=0,]
    
    
    # displaygridIgraphPlotFromEdges(AFMImageNetworksAnalysis@binaryAFMImage,
    #                                AFMImageNetworksAnalysis@edgesTable[keep %in% c(TRUE),],
    #                                AFMImageNetworksAnalysis@isolatedNodesList)
    #
    # displaygridIgraphPlotFromEdges(AFMImageNetworksAnalysis@binaryAFMImage,
    #                                AFMImageNetworksAnalysis@edgesTable,
    #                                AFMImageNetworksAnalysis@isolatedNodesList)
    
  }else{
    warning("no treatment because no circle")
  }
  return(AFMImageNetworksAnalysis)
}


#' fusion the nodes that are intersecting
#' 
#' manage the fusion of nodes which circles instersect
#' keep all the circles, manage a fusion table
#' node id / fusion id
#' 
#' 
#' @param AFMImageNetworksAnalysis the AFMImageNetworksAnalysis instance
#' @return a list of edges with fusioned nodes
#' @export
#' @author M.Beauvais
fusionCloseNodes<-function(AFMImageNetworksAnalysis) {
  
  group<-mean_lon<-lon<-mean_lat<-lat<-vertexId<-from<-to<-vedges<-NULL
  
  AFMImageNetworksAnalysis@circlesTable
  AFMImageNetworksAnalysis@circlesTable$group<-rep(0, nrow(AFMImageNetworksAnalysis@circlesTable))
  groupNumber<-0
  for (centerId in seq(1, nrow(AFMImageNetworksAnalysis@circlesTable))) {
    #centerId=6
    print(paste0(centerId," / ", nrow(AFMImageNetworksAnalysis@circlesTable)))
    
    center<- AFMImageNetworksAnalysis@circlesTable[centerId,]
    
    radiusVector<-center$circleRadius+AFMImageNetworksAnalysis@circlesTable$circleRadius
    
    distVector<-sp::spDistsN1(pts=matrix(c(AFMImageNetworksAnalysis@circlesTable$lon,AFMImageNetworksAnalysis@circlesTable$lat),ncol=2),
                              pt=matrix(c(center$lon,center$lat),ncol=2),
                              longlat=FALSE)
    intersectVector<-distVector-radiusVector-2
    
    # print(radiusVector)
    # print(distVector)
    print(intersectVector)
    listOfIntersect<-which(intersectVector<0)
    #print(listOfIntersect)
    if (length(listOfIntersect)>1) {
      print("to be grouped")
      if (all(AFMImageNetworksAnalysis@circlesTable[listOfIntersect,]$group==0)) {
        groupNumber<-groupNumber+1
        AFMImageNetworksAnalysis@circlesTable[listOfIntersect,]$group<-groupNumber
      }else{
        print("special")
        #print(AFMImageNetworksAnalysis@circlesTable[listOfIntersect&group!=0])
        existingGroupNumber<-AFMImageNetworksAnalysis@circlesTable[listOfIntersect,][group!=0,][1]$group
        AFMImageNetworksAnalysis@circlesTable[listOfIntersect,]$group<-existingGroupNumber
      }
      #print(AFMImageNetworksAnalysis@circlesTable[listOfIntersect])
    }
    
  }
  AFMImageNetworksAnalysis@circlesTable
  
  
  nbOfNodesToFusion<-length(unique(AFMImageNetworksAnalysis@circlesTable[group!=0,]$group))
  nbOfNodesToFusion
  if (nbOfNodesToFusion>0) {
    # define new coordinates for all points
    # wh<-which(AFMImageNetworksAnalysis@circlesTable$group==0)
    # AFMImageNetworksAnalysis@circlesTable[wh]$new_lat<-AFMImageNetworksAnalysis@circlesTable[wh]$lat
    # AFMImageNetworksAnalysis@circlesTable[wh]$new_lon<-AFMImageNetworksAnalysis@circlesTable[wh]$lon
    # AFMImageNetworksAnalysis@circlesTable
    
    AFMImageNetworksAnalysis@circlesTable[, mean_lon:=floor(mean(lon)), by=group] 
    AFMImageNetworksAnalysis@circlesTable[, mean_lat:=floor(mean(lat)), by=group]
    AFMImageNetworksAnalysis@circlesTable[group==0, mean_lon:=lon] 
    AFMImageNetworksAnalysis@circlesTable[group==0, mean_lat:=lat] 
    AFMImageNetworksAnalysis@circlesTable
    
    
    # define edge correspondance table
    
    newvedges<-data.table(vertexId=getVertexId(AFMImageNetworksAnalysis@binaryAFMImage, AFMImageNetworksAnalysis@circlesTable[group!=0,]$lon, AFMImageNetworksAnalysis@circlesTable[group!=0,]$lat),
                          new_vertexId=getVertexId(AFMImageNetworksAnalysis@binaryAFMImage, AFMImageNetworksAnalysis@circlesTable[group!=0,]$mean_lon, AFMImageNetworksAnalysis@circlesTable[group!=0,]$mean_lat))
    newvedges
    setkey(newvedges, vertexId)
    
    # tranform the isolated nodes
    isolates<-AFMImageNetworksAnalysis@isolatedNodesList
    isolates %in% newvedges$vertexId
    newvedges
    onewh<-which(isolates %in% newvedges$vertexId)
    for(index in onewh) {
      print(index)
      oldvertexId<-isolates[index]
      print(oldvertexId)
      newVertexId<-newvedges[vertexId %in% oldvertexId]$new_vertexId
      print(newVertexId)
      isolates<-replace(isolates, isolates==oldvertexId, as.character(newVertexId))
    }
    isolates<-unique(isolates)
    isolates
    
    # tranform the edges with the fusioned edge
    newvedges2<-copy(AFMImageNetworksAnalysis@edgesTable)
    newvedges2
    
    onewh<-which(newvedges2$from %in% newvedges$vertexId)
    for(index in onewh) {
      print(index)
      oldvertexId<-newvedges2[index,]$from
      print(oldvertexId)
      newVertexId<-newvedges[vertexId %in% oldvertexId,]$new_vertexId
      print(newVertexId)
      newvedges2[index, from:=as.character(newVertexId)]
    }
    newvedges2
    
    onewh<-which(newvedges2$to %in% newvedges$vertexId)
    for(index in onewh) {
      print(index)
      oldvertexId<-newvedges2[index,]$to
      print(oldvertexId)
      newVertexId<-newvedges[vertexId %in% oldvertexId,]$new_vertexId
      print(newVertexId)
      newvedges2[index, to:=as.character(newVertexId)]
    }
    newvedges2
  }else{
    newvedges2<-vedges
  }
  #print(newvedges2)
  
  
  AFMImageNetworksAnalysis@fusionedNodesCorrespondance<-copy(newvedges)
  if (typeof(newvedges2) %in% c("data.table")) {
    AFMImageNetworksAnalysis@fusionedNodesEdgesTable<-copy(newvedges2)
  }else{
    AFMImageNetworksAnalysis@fusionedNodesEdgesTable<-copy(AFMImageNetworksAnalysis@edgesTable) 
  }
  return(AFMImageNetworksAnalysis)
}

#' identify isolated nodes comparing the list of edges and the list of nodes
#' 
#' @param AFMImageNetworksAnalysis the AFMImageNetworksAnalysis instance
#' @return the updated instance of AFMImageNetworksAnalysis
#' @export
#' @author M.Beauvais
identifyIsolatedNodes<-function(AFMImageNetworksAnalysis) {
  
  if (!(is.list(AFMImageNetworksAnalysis@circlesTable) & length(AFMImageNetworksAnalysis@circlesTable) == 0)) {
    isolates<-getVertexId(AFMImageNetworksAnalysis@binaryAFMImage, AFMImageNetworksAnalysis@circlesTable$lon, AFMImageNetworksAnalysis@circlesTable$lat)
    print(isolates)
    vedges<-AFMImageNetworksAnalysis@edgesTable
    AFMImageNetworksAnalysis@isolatedNodesList<-isolates[!isolates %in% vedges$from & !isolates %in% vedges$to]
  }else{
    warning("no treatment - no circle identified")
  }
  return(AFMImageNetworksAnalysis)
}
# AFMImageNetworksAnalysis<-identifyNodesWithCircles(AFMImageNetworksAnalysis= AFMImageNetworksAnalysis)
# AFMImageNetworksAnalysis<-identifyEdgesFromCircles(AFMImageNetworksAnalysis= AFMImageNetworksAnalysis)
# AFMImageNetworksAnalysis<-identifyIsolatedNodes(AFMImageNetworksAnalysis)
# AFMImageNetworksAnalysis<-getEdgesAfterNodesFusion(AFMImageNetworksAnalysis)


#' calculate the physical distances between nodes
#' 
#' @param pathVidVector a network path
#' @param hscale the hscale of the \code{\link{AFMImage}} from Atomic Force Microscopy
#' @param vscale the vscale of the \code{\link{AFMImage}} from Atomic Force Microscopy
#' @return the physical distance the extrmities of the path
#' @export
#' @author M.Beauvais
calculatePhysicalDistanceFromPath<-function(pathVidVector, hscale, vscale) {
  physicalDistance<-0
  
  vid1<-pathVidVector[1]
  for (pathInd in seq(2, length(pathVidVector))) {
    vid2<-pathVidVector[pathInd]
    vid1Coords<-getCoordinatesFromVertexId(vid1)
    vid2Coords<-getCoordinatesFromVertexId(vid2)
    
    physicalDistance<-physicalDistance+sqrt((hscale*(vid1Coords$coords.x1-vid2Coords$coords.x1))^2+(vscale*(vid1Coords$coords.x2-vid2Coords$coords.x2))^2)
    vid1<-pathVidVector[pathInd]
  }
  
  return(physicalDistance)
}
# TODO check if strsplit return results
#path<-strsplit(directedConnectedNodesDT[1,]$shortest_path,"-")[[1]]
#calculatePhysicalDistanceFromPath(newAFMImage, path)

#' create the igraph weighted graph from the nodes and edges
#' 
#' @param AFMImageNetworksAnalysis a \code{\link{AFMImageNetworksAnalysis}}
#' @export
#' @author M.Beauvais
createGraph<-function(AFMImageNetworksAnalysis) {
  keep<-NULL
  
  if (!(is.list(AFMImageNetworksAnalysis@circlesTable) & length(AFMImageNetworksAnalysis@circlesTable) == 0)) {
    
    isolatedNodesList<-AFMImageNetworksAnalysis@isolatedNodesList
    from<-to<-NULL
    
    ultimateNetwork<-copy(AFMImageNetworksAnalysis@edgesTable[keep %in% c(TRUE),])
    isolates<-isolatedNodesList[!isolatedNodesList %in% ultimateNetwork$from & !isolatedNodesList %in% ultimateNetwork$to]
    
    totalVerticesNumber<-length(unique(c(ultimateNetwork$from, ultimateNetwork$to, isolates)))
    print(paste("totalVerticesNumber:",totalVerticesNumber))
    
    listOfVertices<-unique(c(ultimateNetwork$from, ultimateNetwork$to, isolates))
    names(listOfVertices)<-unique(c(ultimateNetwork$from, ultimateNetwork$to, isolates))
    alledges2<-as.vector(t(matrix(c(ultimateNetwork$from,ultimateNetwork$to),ncol=2)))
    g<-graph(edges=alledges2, directed=FALSE, isolates=isolates)
    E(g)$weight <-ultimateNetwork$dist
    AFMImageNetworksAnalysis@skeletonGraph<-g
    
    ultimateNetwork<-copy(AFMImageNetworksAnalysis@edgesTable)
    isolates<-isolatedNodesList[!isolatedNodesList %in% ultimateNetwork$from & !isolatedNodesList %in% ultimateNetwork$to]
    listOfVertices<-unique(c(ultimateNetwork$from, ultimateNetwork$to, isolates))
    names(listOfVertices)<-unique(c(ultimateNetwork$from, ultimateNetwork$to, isolates))
    alledges2<-as.vector(t(matrix(c(ultimateNetwork$from,ultimateNetwork$to),ncol=2)))
    g<-graph(edges=alledges2, directed=FALSE, isolates=isolates)
    E(g)$weight <-ultimateNetwork$dist
    AFMImageNetworksAnalysis@originalGraph<-g
  }else{
    warning("no treatment - no circle identified")
  }
  return(AFMImageNetworksAnalysis)
}


#' calculate the  shortest path between adjacent nodes
#' 
#' Calculate the shortest path between all nodes of degree different to 2
#' that are connected with nodes of degree equal to 2
#' Calculate the distance between the above nodes.
#' 
#' @param ... cl: a cluster object from the parallel package
#' @param AFMImageNetworksAnalysis a \code{\link{AFMImageNetworksAnalysis}}
#' @export
#' @author M.Beauvais
calculateShortestPaths<-function(...,AFMImageNetworksAnalysis) {
  force(AFMImageNetworksAnalysis)
  
  if ((is.list(AFMImageNetworksAnalysis@circlesTable) & length(AFMImageNetworksAnalysis@circlesTable) == 0)) {
    warning("not treatment - no circle identified")
  }else{
    
    
    
    args<-names(list(...))
    print(args)
    if (is.null(args)) {
      clExist<-FALSE
    }else{
      clExist<-c(match('cl',args)!=-1)  
      cl<-cl
    }
    
    
    if (clExist) {
      print("using parallel")
      requireNamespace("parallel")
    }else {
      print("no parallel")
    }
    
    
    workerFunc <- function(vid1index, g, hscale, vscale, nodesAnalysisDT) {
      requireNamespace("igraph")
      requireNamespace("data.table")
      requireNamespace("AFM")
      
      directedConnectedNodesDT<-data.table(vid1=c(""), vid1NodeDegree= c(0), 
                                           vid2=c(""), vid2NodeDegree= c(0), 
                                           numberOfNodesInShortestPath=c(0),
                                           shortest_path=c(""),
                                           physicalDistance=c(0))
      
      tryCatch({
        nb<-0
        print(paste0(vid1index," / ", nrow(nodesAnalysisDT)))
        #TODO calculate distance matrix
        # vid2index by distance
        # when nbOfShortestPath for vid1 reach degree of node then break
        nbOfShortestPath<-0
        
        vid1Node<-nodesAnalysisDT[vid1index,]
        vid1<- vid1Node$vid
        vid1NodeDegree<-vid1Node$node_degree
        
        for (vid2index in seq(1, nrow(nodesAnalysisDT))) {
          #print(paste0(vid1index," / ", nrow(nodesAnalysisDT),"-",vid2index))
          
          vid2Node<-nodesAnalysisDT[vid2index,]
          vid2<- vid2Node$vid
          vid2NodeDegree<-vid2Node$node_degree
          
          if (!vid1 %in% vid2) {
            allPath<-all_shortest_paths(g, vid1, vid2)
            #print(allPath)
            #print(is.null(allPath$res))
            if (length(allPath$res)>0) {
              for(pathIndex in seq(1,length(allPath$res))) {
                #if (length(allPath$res)>0) {
                path<-allPath$res[[pathIndex]]$name
                #TODO is it working if two points are in separate graph ?
                numberOfNodesInShortestPath<-length(path)
                #print(allPath$res[[1]]$name %in% nodesAnalysisDT$vid)
                
                #all(nodesAnalysisDT[path[2:(length(path)-1)],]$node_degree)
                
                # are all the intermediary nodes of degree equal to 2
                #numberOfNodesInShortestPath<-length(which(path %in% nodesAnalysisDT$vid == TRUE))
                if (all(path[2:(length(path)-1)] %in% nodesAnalysisDT$vid == FALSE)) {
                  print(c("interresting", vid1, vid2))
                  print(path)
                  #nbOfShortestPath<-nbOfShortestPath+1
                  physicalDistance<-calculatePhysicalDistanceFromPath(path, hscale, vscale)
                  print(physicalDistance)
                  #totalPhysicalDistance<-totalPhysicalDistance+physicalDistance
                  
                  # TODO fill with reverse path
                  directedConnectedNodesDT<-rbindlist(list(directedConnectedNodesDT, 
                                                           data.table(vid1=vid1, vid1NodeDegree=vid1NodeDegree,
                                                                      vid2=vid2, vid2NodeDegree = vid2NodeDegree, 
                                                                      numberOfNodesInShortestPath=numberOfNodesInShortestPath, 
                                                                      shortest_path=paste0(path, collapse = "-"),
                                                                      physicalDistance=physicalDistance)))
                  
                  print("all")
                }
              }
            }
          }
        }
      }, error = function(e) {
        print("error")
      })
      return(directedConnectedNodesDT[-1,]) 
    }
    
    
    # start   
    hscale<-AFMImageNetworksAnalysis@binaryAFMImage@hscansize/AFMImageNetworksAnalysis@binaryAFMImage@samplesperline
    vscale<-AFMImageNetworksAnalysis@binaryAFMImage@vscansize/AFMImageNetworksAnalysis@binaryAFMImage@lines
    g<-AFMImageNetworksAnalysis@skeletonGraph
    node_degree<-NULL
    verticesAnalysisDT<-data.table(vid=V(g)$name, node_degree=unname(degree(g)))
    nodesAnalysisDT<-copy(verticesAnalysisDT[node_degree!=2 & node_degree!=0,])
    values <- seq(1, nrow(nodesAnalysisDT))
    
    print("calculating shortest paths")
    print(paste(nrow(nodesAnalysisDT), "calls", nrow(nodesAnalysisDT)^2,"loops"))
    start.time <- Sys.time()
    print(start.time)
    if (clExist) {
      cl<-cl
      parallel::clusterEvalQ(cl , c(library("data.table"),library("igraph"), library("AFM")))
      parallel::clusterExport(cl, c("g","hscale", "vscale", "nodesAnalysisDT"),envir=environment())
      res <- parallel::parLapply(cl, values, workerFunc, 
                                 g, hscale, vscale,  
                                 nodesAnalysisDT)
    }else{
      res <- lapply(values, workerFunc, 
                    g, hscale, vscale,  
                    nodesAnalysisDT)
    }
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(paste0("time.taken: ",time.taken))
    
    
    directedConnectedNodesDT<-rbindlist(res)
    
    AFMImageNetworksAnalysis@shortestPaths<-directedConnectedNodesDT
  }
  return(AFMImageNetworksAnalysis)
}

#' get the networks parameters
#' 
#' Calculate and return the networks parameters
#' 
#' @param AFMImageNetworksAnalysis a \code{\link{AFMImageNetworksAnalysis}}
#' @param AFMImage a \code{\link{AFMImage}}
#' @return a data.table with all the parameters
#' @export
#' @author M.Beauvais
calculateNetworkParameters<-function(AFMImageNetworksAnalysis, AFMImage) {
  vid<-node_degree<-hist<-physicalDistance<-vid1NodeDegree<-vid2NodeDegree<-NULL
  
  if ((is.list(AFMImageNetworksAnalysis@circlesTable) & length(AFMImageNetworksAnalysis@circlesTable) == 0)) {
    warning("not treatment - no circle identified")
  }else{
    
    # samplename<-basename(AFMImageNetworksAnalysis@binaryAFMImage@fullfilename)
    
    g<-AFMImageNetworksAnalysis@skeletonGraph
    
    verticesAnalysisDT<-data.table(vid=V(g)$name, node_degree=unname(degree(g)))
    # verticesAnalysisDT
    # nrow(verticesAnalysisDT)
    # nrow(verticesAnalysisDT[node_degree>4])
    # nrow(verticesAnalysisDT[node_degree==1])
    
    param<-getRoughnessParameters(AFMImage)
    param
    
    numberOfNodesPerArea<-(nrow(verticesAnalysisDT[node_degree!=2,]))/param$area
    numberOfNodesPerSurfaceArea<-(nrow(verticesAnalysisDT[node_degree!=2,]))/param$surfaceArea
    
    ggplot(verticesAnalysisDT, aes(node_degree)) +geom_histogram( binwidth=1, fill=NA, color="black") + theme_bw()   #nicer looking
    
    directedConnectedNodesDT<-AFMImageNetworksAnalysis@shortestPaths
    directedConnectedNodesDT
    
    mean(directedConnectedNodesDT$physicalDistance)
    hist(directedConnectedNodesDT$physicalDistance)
    ggplot(directedConnectedNodesDT, aes(physicalDistance)) +geom_histogram( binwidth=50, fill=NA, color="black") + theme_bw()   #nicer looking
    
    
    max(directedConnectedNodesDT$physicalDistance)
    
    
    
    #Total Number of nodes
    totalNumberOfNodes<-nrow(verticesAnalysisDT[node_degree!=2,])
    
    #Number of nodes with degree > 2
    totalNumberOfNodesWithDegreeThreeOrMorePerArea<-nrow(verticesAnalysisDT[node_degree>2,])/param$area
    
    #Number of nodes with degree = 1
    totalNumberOfNodesWithDegreeOnePerArea<-nrow(verticesAnalysisDT[node_degree==1,])/param$area
    
    # Number Of Isolated Nodes
    NumberOfIsolatedNodesPerArea<-length(AFMImageNetworksAnalysis@isolatedNodesList)/param$area
    
    #Surface
    area<-param$area
    
    #Surface area of a grid of heights
    surfaceArea<-param$surfaceArea
    
    #Nodes (degree>2 or =1) / area
    numberOfNodesPerArea<-(nrow(verticesAnalysisDT[node_degree!=2,]))/param$area
    
    #Nodes (degree>2 or =1) / surface area
    numberOfNodesPerSurfaceArea<-(nrow(verticesAnalysisDT[node_degree!=2,]))/param$surfaceArea
    
    #Mean physical distance between nodes (degree!=2)
    MeanPhysicalDistanceBetweenNodes<-mean(directedConnectedNodesDT$physicalDistance)
    
    tryCatch({
      # calculate distance between Highly Connected nodes and terminal nodes
      MeanPhysicalDistanceToTerminalNodes<-mean(directedConnectedNodesDT[(vid1NodeDegree>2&vid2NodeDegree==1),]$physicalDistance)
    },
    error=function(cond) {
      MeanPhysicalDistanceToTerminalNodes<-NA
    })
    
    tryCatch({
      # calculate distance between Highly Connected nodes
      MeanPhysicalDistanceBetweenHighlyConnectedNodes<-mean(directedConnectedNodesDT[(vid1NodeDegree>2&vid2NodeDegree>2),]$physicalDistance)
    },
    error=function(cond) {
      MeanPhysicalDistanceBetweenHighlyConnectedNodes<-NA
    })
    
    tryCatch({
      # calculate distance between terminal nodes
      MeanPhysicalDistanceBetweenTerminalNodes<-mean(directedConnectedNodesDT[(vid1NodeDegree==1&vid2NodeDegree==1),]$physicalDistance)
    },
    error=function(cond) {
      MeanPhysicalDistanceBetweenTerminalNodes<-NA
    })
    
    #Mean physical size of nodes 
    MeanPhysicalSizeOfHighlyConnectedNodes<-mean(AFMImageNetworksAnalysis@circlesTable[vid %in% verticesAnalysisDT[node_degree>2,]$vid,]$circleRadius)
    SDPhysicalSizeOfHighlyConnectedNodes<-sd(AFMImageNetworksAnalysis@circlesTable[vid %in% verticesAnalysisDT[node_degree>2,]$vid,]$circleRadius)
    
    MeanPhysicalSizeOfTerminalNodes<-mean(AFMImageNetworksAnalysis@circlesTable[vid %in% verticesAnalysisDT[node_degree==1,]$vid,]$circleRadius)
    SDPhysicalSizeOfTerminalNodes<-sd(AFMImageNetworksAnalysis@circlesTable[vid %in% verticesAnalysisDT[node_degree==1,]$vid,]$circleRadius)
    
    # graph density
    print("calculating graph density")
    graphDensity<-graph.density(g)
    print(graphDensity)
    
    
    # Global cluster coefficient: (close triplets/all triplets)
    graphTransitivity<-transitivity(g, type="global")
    
    
    edgeConnectivity<-edge.connectivity(g)
    
    # Same as graph adhesion
    graphAdhesion=graph.adhesion(g)
    
    # Diameter of the graph
    graphDiameter<-max(directedConnectedNodesDT$physicalDistance)
    
    # Reciprocity of the graph
    graphReciprocity<-reciprocity(g)
    
    # Number of islands
    NumberOfIslands<-clusters(g)$no
    NumberOfIslandsPerArea<-clusters(g)$no/param$area
    
    # in sociology theory
    # gate keepers: low Eigenvector centrality and high Betweenness centrality , 
    # contact with important nodes: high Eigenvector centrality and low Betweenness centrality
    graphEvcent<-evcent(g)$vector
    graphBetweenness<-betweenness(g)
    
    #displaygridIgraphPlot(AFMImageNetworksAnalysis)
    
    # paramDT<-data.table(
    #   area=area,
    #   surfaceArea=surfaceArea)
    # paramDT
    
    resultDT=data.table(NumberOfIslandsPerArea=NumberOfIslandsPerArea,
                        NumberOfIsolatedNodesPerArea=NumberOfIsolatedNodesPerArea, 
                        totalNumberOfNodes=totalNumberOfNodes,
                        totalNumberOfNodesWithDegreeThreeOrMorePerArea=totalNumberOfNodesWithDegreeThreeOrMorePerArea,
                        totalNumberOfNodesWithDegreeOnePerArea=totalNumberOfNodesWithDegreeOnePerArea,
                        totalNumberOfNodesPerArea=numberOfNodesPerArea,
                        totalNumberOfNodesPerSurfaceArea=numberOfNodesPerSurfaceArea,
                        MeanPhysicalDistanceBetweenNodes=MeanPhysicalDistanceBetweenNodes,
                        MeanPhysicalDistanceToTerminalNodes=MeanPhysicalDistanceToTerminalNodes,
                        MeanPhysicalDistanceBetweenHighlyConnectedNodes=MeanPhysicalDistanceBetweenHighlyConnectedNodes,
                        MeanPhysicalDistanceBetweenTerminalNodes=MeanPhysicalDistanceBetweenTerminalNodes,
                        MeanPhysicalSizeOfHighlyConnectedNodes=MeanPhysicalSizeOfHighlyConnectedNodes,
                        SDPhysicalSizeOfHighlyConnectedNodes=SDPhysicalSizeOfHighlyConnectedNodes,
                        MeanPhysicalSizeOfTerminalNodes=MeanPhysicalSizeOfTerminalNodes,
                        SDPhysicalSizeOfTerminalNodes=SDPhysicalSizeOfTerminalNodes,
                        graphDiameter=graphDiameter,
                        graphDensity=graphDensity,
                        graphTransitivity=graphTransitivity,
                        edgeConnectivity=edgeConnectivity,
                        graphAdhesion=graphAdhesion,
                        graphReciprocity=graphReciprocity)
    #resultDT
    
    AFMImageNetworksAnalysis@networksCharacteristics<-resultDT
    AFMImageNetworksAnalysis@graphEvcent<-graphEvcent
    AFMImageNetworksAnalysis@graphBetweenness<-graphBetweenness
  }
  return(AFMImageNetworksAnalysis)
}


#' get the networks parameters
#' 
#' Calculate the holes characteristics
#' 
#' @param AFMImageNetworksAnalysis a \code{\link{AFMImageNetworksAnalysis}}
#' @return a data.table with all the parameters
#' @export
#' @author M.Beauvais
calculateHolesCharacteristics<-function(AFMImageNetworksAnalysis) {
  # holes statistics
  holesIslandsDT<-getHolesStatistics(AFMImageNetworksAnalysis@binaryAFMImage)
  #numberOfHoles<-unique(holesIslandsDT$cluster)
  holeStats<-holesIslandsDT[,.N,by="cluster"]
  
  AFMImageNetworksAnalysis@holes<-holesIslandsDT
  AFMImageNetworksAnalysis@holesCharacteristics<-holeStats
  return(AFMImageNetworksAnalysis)
}

#' removeNode
#' 
#' remove a node from an AFMImage
#' 
#' @param circleAFMImage a \code{\link{AFMImage}}
#' @param nodeDT a data.table lon lat circleRadius
#' @return an \code{\link{AFMImage}}
#' @author M.Beauvais
removeNode<-function(circleAFMImage, nodeDT) {
  
  #print(paste("removing",nrow(nodeDT), "nodes"))
  for (i in seq(1, nrow(nodeDT))){
    circleRadius<-nodeDT[i,]$circleRadius
    center<-c(nodeDT[i,]$lat, nodeDT[i,]$lon)
    circleRadius2=circleRadius #+BIGGER_CIRCLE_RADIUS
    blockSize2=circleRadius2+1 #*BIGGER_CIRCLE_RADIUS_MULTILPLIER+1
    
    circleCenter<-c(circleRadius2, circleRadius2)
    circlePts = SpatialPoints(cbind(rep(1:(blockSize2),blockSize2), rep(1:(blockSize2),1,each= blockSize2)))
    circlenm <- sp::spDistsN1(pts=circlePts, pt=circleCenter, longlat=FALSE)
    pts = SpatialPoints(cbind(rep(0:(blockSize2-1),blockSize2)+center[1]-circleRadius2, rep(0:(blockSize2-1),1,each= blockSize2)+center[2]-circleRadius2))
    pts<-pts[pts$coords.x1>0&pts$coords.x1<circleAFMImage@lines&pts$coords.x2>0&pts$coords.x2<circleAFMImage@samplesperline]
    nm <- sp::spDistsN1(pts=pts, pt=center, longlat=FALSE)
    listOfPointsInsideCircle<-pts[nm<=circleRadius]
    #circleAFMImage@data$h[listOfPointsInsideCircle$coords.x2+1+(listOfPointsInsideCircle$coords.x1)*circleAFMImage@samplesperline]<-0
    circleAFMImage@data$h[listOfPointsInsideCircle$coords.x1+1+(listOfPointsInsideCircle$coords.x2)*circleAFMImage@samplesperline]<-0
  }
  return(circleAFMImage)
}

#' addNode
#' 
#' add a node to an AFMImage 
#' 
#' @param circleAFMImage a \code{\link{AFMImage}}
#' @param nodeDT nodeDT a data.table lon lat circleRadius
#' @param filterIndex an integer
#' @return an \code{\link{AFMImage}}
#' @author M.Beauvais
addNode<-function(circleAFMImage, nodeDT,filterIndex) {
  #print(paste("adding",nrow(nodeDT), "nodes"))
  for (i in seq(1, nrow(nodeDT))){
    circleRadius<-nodeDT[i,]$circleRadius
    center<-c(nodeDT[i,]$lon, nodeDT[i,]$lat)
    circleRadius2=circleRadius+BIGGER_CIRCLE_RADIUS
    blockSize2=circleRadius2*BIGGER_CIRCLE_RADIUS_MULTILPLIER+1
    
    circleCenter<-c(circleRadius2, circleRadius2)
    circlePts = SpatialPoints(cbind(rep(1:(blockSize2),blockSize2), rep(1:(blockSize2),1,each= blockSize2)))
    circlenm <- sp::spDistsN1(pts=circlePts, pt=circleCenter, longlat=FALSE)
    pts = SpatialPoints(cbind(rep(0:(blockSize2-1),blockSize2)+center[1]-circleRadius2, rep(0:(blockSize2-1),1,each= blockSize2)+center[2]-circleRadius2))
    pts<-pts[pts$coords.x1>0&pts$coords.x1<circleAFMImage@lines&pts$coords.x2>0&pts$coords.x2<circleAFMImage@samplesperline]
    nm <- sp::spDistsN1(pts=pts, pt=center, longlat=FALSE)
    listOfPointsInsideCircle<-pts[nm<=circleRadius]
    circleAFMImage@data$h[listOfPointsInsideCircle$coords.x1+1+(listOfPointsInsideCircle$coords.x2)*circleAFMImage@samplesperline]<-circleAFMImage@samplesperline+filterIndex*10
  }
  return(circleAFMImage)
}

#' removeLonguestEdge
#' 
#'  Find and remove the longuest edge if it is unique
#'    
#' @param k an integer
#' @param res res ?
#' @param sides data.table
#' @param myRes data.table?
#' @param vertex1 a vertex ?
#' @return a data.table with from, to
#' @author M.Beauvais
removeLonguestEdge<-function(k, res, sides, myRes, vertex1) {
  from<-to<-NULL
  
  thirdSide<-sides[k,]
  thirdSide
  secondSide <- myRes[(from %in% c(thirdSide$from) & to %in% c(vertex1)) | 
                        (to %in% c(thirdSide$from)&(from %in% c(vertex1))),]
  secondSide
  firstSide <- myRes[(from %in% c(thirdSide$to) & to %in% c(vertex1)) | 
                       (to %in% c(thirdSide$to)&(from %in% c(vertex1))),]
  firstSide
  
  distV<-rbind(firstSide, secondSide, thirdSide)
  
  if (nrow(distV)<3) return(res[-1,])
  # print(distV)
  
  maxDistV<-which.max(distV$dist)
  maxDistV
  #print(distV[maxDistV,])
  if(length(unique(distV$dist))>1) {
    maxDistVal<-distV[maxDistV,]$dist
    if (nrow(distV[dist %in% maxDistVal,])==1) {
      res<-rbind(res,data.table(distV[maxDistV,c("from","to"),with=FALSE]))
    }
  }
  return(res[-1,])
}

#' getMaxCircleMatrix
#'
#' for each pixel of the image,
#'  if the pixel is not empty
#' try to place one circle
#' start with biggets circle
#' as soon as a circle is found the circle, the pixel is associated with with the circle raidus
#' 
#' @param ... cl: a cluster object from the parallel package
#' @param newCircleAFMImage a \code{\link{AFMImage}}
#' @param CIRCLE_RADIUS_INIT CIRCLE_RADIUS_INIT
#' @return res a matrix
#' @export
#' @author M.Beauvais
getMaxCircleMatrix<-function(..., newCircleAFMImage, CIRCLE_RADIUS_INIT) {
  x<-y<-NULL
  args<-names(list(...))
  print(args)
  if (is.null(args)) {
    print("not using parallel for getMaxCircleMatrix")
    clExist<-FALSE
  }else{
    print("using parallel for getMaxCircleMatrix")
    clExist<-c(match('cl',args)!=-1)  
    cl<-cl
  }
  
  binaryAFMImageMatrix<-matrix(newCircleAFMImage@data$h, ncol=newCircleAFMImage@samplesperline)
  
  maxCircleRadiusMatrix<-matrix(data=rep(0,newCircleAFMImage@samplesperline*newCircleAFMImage@lines),
                                nrow=newCircleAFMImage@lines,
                                ncol=newCircleAFMImage@samplesperline)
  
  
  initialAllXY<-data.table(which(binaryAFMImageMatrix!=0,arr.ind = T))
  colnames(initialAllXY)<-c("x","y")
  setkey(initialAllXY, x)
  initialAllXY$x<-as.numeric(initialAllXY$x)
  initialAllXY$y<-as.numeric(initialAllXY$y)
  
  #matrixElementsDT<-data.table(x=c(0),y=c(0),radius=c(0))
  rm(matrixElementsDT)
  
  circleRadius<-CIRCLE_RADIUS_INIT
  #circleRadius<-4
  iteration<-0
  #rm(avgDT)
  
  start.time <- Sys.time()
  print(start.time)
  while(circleRadius>1) {
    
    iteration=iteration+1
    circleRadius=circleRadius-1
    blockSize<-circleRadius*2+1
    
    allXY <- copy(initialAllXY[x<=newCircleAFMImage@samplesperline-blockSize&y<=newCircleAFMImage@lines-blockSize])
    print(paste0("circleRadius:",circleRadius))
    print(paste0(nrow(allXY)," loops"))
    
    if ((blockSize>newCircleAFMImage@samplesperline)|((blockSize-1)>newCircleAFMImage@lines)) {
      print(paste0("too big blockSize", blockSize))
    }else{
      
      
      circleCenter<-c(circleRadius, circleRadius)
      circlePts = sp::SpatialPoints(cbind(rep(1:(blockSize),blockSize), rep(1:(blockSize),1,each= blockSize)))
      circlenm <- sp::spDistsN1(pts = circlePts, pt = circleCenter, longlat=FALSE)
      
      
      if(clExist) {
        #cl<-cl
        parallel::clusterEvalQ(cl , c(library("data.table"),library("sp"), library("AFM"),library("parallel")))
        parallel::clusterExport(cl, c("allXY","newCircleAFMImage","binaryAFMImageMatrix","maxCircleRadiusMatrix","circleRadius","circlenm"), envir=environment())
        matrixElements<-parallel::parLapply(cl, 1:nrow(allXY),identifyMaxCircleRadius, allXY, newCircleAFMImage, binaryAFMImageMatrix,maxCircleRadiusMatrix,circleRadius,circlenm)
      }else{
        matrixElements<-lapply(1:nrow(allXY),identifyMaxCircleRadius, allXY, newCircleAFMImage, binaryAFMImageMatrix,maxCircleRadiusMatrix,circleRadius,circlenm)
      }
      
      
      if (!exists("matrixElementsDT"))  matrixElementsDT<-rbindlist(matrixElements)
      else matrixElementsDT<-rbind(matrixElementsDT, rbindlist(matrixElements))
      
      #print(matrixElementsDT)
      
      # setkeyv(allXY, c("x","y"))
      # setkeyv(matrixElementsDT, c("x","y"))
      
      initialAllXY<-data.table::fsetdiff(x = initialAllXY, y=matrixElementsDT[,1:2,])
      print(paste("elements left: ", nrow(initialAllXY)))
    }
  }
  end.time <- Sys.time()
  print(paste0("start.time: ",start.time))
  print(paste0("end.time: ",end.time))
  time.taken <- end.time - start.time
  print(paste0("time.taken: ",time.taken))
  
  missingX<-setdiff(seq(1,newCircleAFMImage@samplesperline),unique(matrixElementsDT$x))
  missingY<-setdiff(seq(1,newCircleAFMImage@lines),unique(matrixElementsDT$y))
  
  matrixElementsDT<-rbind(matrixElementsDT, 
                          data.table(x=c(missingX,rep(1, length(missingY))),y=c(rep(1, length(missingX)), missingY),radius=c(rep(0, length(missingX)+length(missingY))))
  )
  
  res <- as.matrix(dcast.data.table(data=matrixElementsDT, x ~ y, value.var="radius", fun.aggregate = max, fill=0)[,-1, with=FALSE])
  # res
  # max(res)
  return(res)
}

#' simplifyNetwork
#' 
#' simplify the network keeping only the important edges
#' 
#' @param ... cl: a cluster object from the parallel package
#' @param allVertices a data.table of vertices
#' @param allEdges a data.table of edges
#' @return a data.table of edges
#' @export
#' @author M.Beauvais
simplifyNetwork<-function(..., allVertices, allEdges){
  from<-to<-fromto<-NULL
  
  args<-names(list(...))
  print(args)
  if (is.null(args)) {
    clExist<-FALSE
  }else{
    clExist<-c(match('cl',args)!=-1)  
  }
  
  
  if (clExist) {
    print("using parallel")
    requireNamespace("parallel")
  }
  
  allVertices<-as.character(allVertices)
  allEdges$from<-as.character(allEdges$from)
  allEdges$to<-as.character(allEdges$to)
  
  # find triangles in a network and eliminate longuest edge
  # allVertices a vector of Vertices
  # allEdges a data.table with following columns: from, to, dist
  findTriangleAndEdgeToEliminate<-function(j,allVertices, allEdges) {
    requireNamespace("data.table")
    
    
    res<-data.table(from=c(0),to=c(0))
    vertex1<-allVertices[j]
    #print(paste(j, vertex1))
    # if (vertex1 %in% c(10485790) | vertex1 %in% c(11796515)) {
    #   print("problem1")
    # }
    myRes<-allEdges[from %in% vertex1 | to %in% vertex1, ]
    
    # all nodes linked to Vertex
    allNodeLinkToVertex<-unique(c(myRes$from, myRes$to))
    allNodeLinkToVertex<-allNodeLinkToVertex[! allNodeLinkToVertex %in% vertex1]
    
    sides<-allEdges[from %in% allNodeLinkToVertex & to %in% allNodeLinkToVertex,]
    # if (nrow(sides[from %in% c(9699366) | to %in%  c(9699366),])>0) {
    #   #53:  9699368  9699366     to             38             37           40           37  2.000000 TRUE                 1               1
    #   print("Problem2")
    # }
    
    #finalRes<-lapply(1:1,removeLonguestEdge,res, sides, myRes, vertex1)
    
    finalRes<-lapply(1:nrow(sides),removeLonguestEdge,res, sides, myRes, vertex1)
    finalRes<-rbindlist(finalRes)
    # if (vertex1 %in% c(10485790) | vertex1 %in% c(11796515)) {
    #   print("problem3")
    # }
    finalRes<-unique(finalRes)
    return(finalRes)
  }
  
  start.time <- Sys.time()
  print(start.time)
  if(clExist) {
    cl<-cl
    parallel::clusterEvalQ(cl , c(library("data.table")))
    parallel::clusterExport(cl, c("allVertices", "allEdges"), envir=environment())
    resRemoveEdge<-parallel::parLapply(cl, 1:length(allVertices),findTriangleAndEdgeToEliminate , allVertices=allVertices, allEdges=allEdges)
  }else{
    resRemoveEdge<-lapply(1:length(allVertices),findTriangleAndEdgeToEliminate , allVertices=allVertices, allEdges=allEdges)
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(paste0("time.taken: ",time.taken))
  
  resRemoveEdge<-rbindlist(resRemoveEdge)
  resRemoveEdge$fromto<-paste0(resRemoveEdge$from,"-",resRemoveEdge$to)
  setkey(resRemoveEdge, fromto)
  resRemoveEdge<-unique(resRemoveEdge)
  resRemoveEdge$fromto<-NULL
  
  print("resRemoveEdge")
  #print(resRemoveEdge)
  allEdges$keep<-rep(TRUE, nrow(allEdges))
  allEdges$remove<-rep(FALSE, nrow(allEdges))
  indexOfEdgeToBeRemoved<-1
  for(indexOfEdgeToBeRemoved in seq(1,nrow(resRemoveEdge))) {
    allEdges[(allEdges$from %in% c(resRemoveEdge[indexOfEdgeToBeRemoved,]$from) & 
                allEdges$to %in% c(resRemoveEdge[indexOfEdgeToBeRemoved,]$to)),]$remove<-TRUE
    allEdges[(allEdges$from %in% c(resRemoveEdge[indexOfEdgeToBeRemoved,]$from) & 
                allEdges$to %in% c(resRemoveEdge[indexOfEdgeToBeRemoved,]$to)),]$keep<-FALSE
  }
  
  print("end simplifyNetwork")
  return(allEdges)
}

#' generatePolygonEnvelope
#' 
#' generate a convex polygon from circles
#' 
#' @param AFMImageNetworksAnalysis a \code{\link{AFMImageNetworksAnalysis}}
#' @param centers a matrix ?
#' @param radius a vector of radius
#' @return a polygon
#' @export
#' @author M.Beauvais
generatePolygonEnvelope<-function(AFMImageNetworksAnalysis, centers, radius){
  #chull<-lines<-SpatialPolygons<-Polygons<-SpatialPointsDataFrame<-NULL
  
  # check if center and radius are in image
  # TBD
  binaryAFMImage<-AFMImageNetworksAnalysis@binaryAFMImage
  
  x1<-x2<-c()
  
  #i<-1
  for (i in seq(1, nrow(centers))) {
    circleRadius<-radius[i]
    center<-centers[i,]
    # center<-c(10,30)
    # circleRadius<-9
    if (circleRadius<0) {
      stop("getCircleSpatialPoints - the radius is inferior to 0")
      return()
    }
    
    
    if (circleRadius>0) {
      blockSize<-circleRadius*2+1
      
      pts = sp::SpatialPoints(cbind(rep(1:blockSize,blockSize)+center[1]-circleRadius-1, rep(1:blockSize,1,each= blockSize)+center[2]-circleRadius-1))
      #print(pts)
      pts<-pts[pts$coords.x1>0&pts$coords.x1<binaryAFMImage@lines&pts$coords.x2>0&pts$coords.x2<binaryAFMImage@samplesperline]
      #plot(pts)
      nm <- sp::spDistsN1(pts = matrix(c(pts$coords.x1, pts$coords.x2), ncol=2), pt=c(center[1], center[2]), longlat=FALSE)
      #print(nm)
      
      centerAllpoints<-pts[nm<=circleRadius]
      #plot(centerAllpoints)
      
      centerAllpoints<-SpatialPoints(cbind(
        c(centerAllpoints$coords.x1, center[1]),
        c(centerAllpoints$coords.x2, center[2])
      ))
      
      x1<-c(x1,centerAllpoints$coords.x1)
      x2<-c(x2,centerAllpoints$coords.x2)
      # X<-cbind(x1,x2)
      # plot(X, cex = 0.5)
      
      
    }else{
      # circleRadius == 0
      centerAllpoints<-sp::SpatialPoints(cbind(center$lon, center$lat))
      x1<-c(x1,center[2])
      x2<-c(x2,center[1])
    }
  }
  X<-cbind(x2,x1)
  ch <- chull(X)
  coords <- X[c(ch, ch[1]), ]  # closed polygon
  #plot(X, cex = 0.5)
  #lines(coords)
  sp_poly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coords)), ID=1)))
  
  return(sp_poly)
}
#envelope <- generatePolygonEnvelope(AFMImageNetworksAnalysis, centers, r)

#' getAllPointsToRemove
#' 
#' get the points inside envelope
#' 
#' @param AFMImageNetworksAnalysis a \code{\link{AFMImageNetworksAnalysis}}
#' @param envelope an envelope of points ?
#' @return a data.table of points
#' @export
#' @author M.Beauvais
getAllPointsToRemove<-function(AFMImageNetworksAnalysis, envelope) {
  #envelopeSR1=Polygons(list(Polygon(envelope$XY)),"r1")
  # sr=SpatialPolygons(list(envelopeSR1))
  
  sr=envelope
  
  Lines<-AFMImageNetworksAnalysis@binaryAFMImage@lines
  Samplesperline<-AFMImageNetworksAnalysis@binaryAFMImage@samplesperline
  pts = cbind(rep(seq(1,Samplesperline, by= 1), times = Lines), rep(seq(1,Lines, by= 1), each = Samplesperline))
  pts
  dimnames(pts)[[1]] = seq(1,Lines*Samplesperline)
  df = data.frame(a = seq(1,Lines*Samplesperline))
  row.names(df) = seq(1,Lines*Samplesperline)
  
  
  #options(warn=1) # show warnings where they occur
  mySP<-sp::SpatialPointsDataFrame(pts, df, match.ID = TRUE) # don't warn
  
  # retrieve overlay per polygon:
  resOver<-sp::over(x=mySP, y=sr)
  resOver[!is.na(resOver)]
  
  vId<-as.integer(names(resOver[!is.na(resOver)]))
  HASHSIZE<-Samplesperline
  
  vertexId<-as.numeric(vId)
  y<-floor(vertexId/HASHSIZE)
  x<-vertexId-y*HASHSIZE
  return(data.table(vId=vId, coords.x1=x,coords.x2=y))
}

#' identifyMaxCircleRadius
#' 
#' identifyMaxCircleRadius
#' 
#' @param i an integer
#' @param allXY combinations of ?
#' @param newCircleAFMImage a \code{\link{AFMImage}}
#' @param binaryAFMImageMatrix a \code{\link{AFMImage}}
#' @param maxCircleRadiusMatrix a matrix
#' @param circleRadius a vector of radius ?
#' @param circlenm a ?
#' @return a data table with x,y,radius columns
#' @author M.Beauvais
identifyMaxCircleRadius<-function(i,allXY, newCircleAFMImage, binaryAFMImageMatrix,maxCircleRadiusMatrix,circleRadius,circlenm) {
  x<-allXY[i,]$x
  y<-allXY[i,]$y
  #print (paste(x,y,"center: ",x+circleRadius, y+circleRadius))
  resDT<-data.table(x=c(0),y=c(0),radius=c(0))
  blockSize<-circleRadius*2+1
  if(binaryAFMImageMatrix[x+circleRadius,y+circleRadius]!=0) {
    if(maxCircleRadiusMatrix[x+circleRadius,y+circleRadius]==0) {
      tempMatrix<-binaryAFMImageMatrix[x:(x+blockSize),y:(y+blockSize)]
      if ((!anyNA(as.vector(tempMatrix)[circlenm<=circleRadius]))&
          (all(as.vector(tempMatrix)[circlenm<=circleRadius] == 1) == TRUE)) {
        #print (paste(x,y,"center: ",x+circleRadius, y+circleRadius))
        resDT<-rbind(resDT,data.table(x=c(x+circleRadius),y=c(y+circleRadius),radius=c(circleRadius)))
      }
    }
  }
  return(resDT[-1,])
}
