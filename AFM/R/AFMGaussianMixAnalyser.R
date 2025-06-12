require("data.table")
require("mixtools")

# normality tests
require(gridExtra)
require(ggplot2)


#if(getRversion() >= "3.1.0") utils::suppressForeignCheck(c("r", "roughness","x","predict.gstat"))


#' @title AFM image Gaussian Mix analysis class
#' 
#' @description \code{AFMImageGaussianMixAnalysis} handles an \code{\link{AFMImage}} Gaussian mix of heights analysis 
#' 
#' @slot minGaussianMix the minimum number of components to calculate
#' @slot maxGaussianMix the maximum number of components to calculate
#' @slot epsilonGaussianMix the convergence criterion
#' @slot gaussianMix a data.table to store the calculated Gaussian mixes
#' @slot summaryMixture a data.table to summaryse the mixtures
#' @slot tcdfsEcdfsCheck an array to store the points to draw tcdfs ecdfs check
#' @slot densityCurvesAllHeights an array to store the points to draw the density curves
#' @slot eachComponentsCounts an array to store the points to draw counts of each components
#' @slot updateProgress a function to update a graphical user interface
#' @name AFMImageGaussianMixAnalysis-class
#' @rdname AFMImageGaussianMixAnalysis-class
#' @author M.Beauvais
AFMImageGaussianMixAnalysis<-setClass("AFMImageGaussianMixAnalysis",
                              slots = c(
                                minGaussianMix="numeric",
                                maxGaussianMix="numeric",
                                epsilonGaussianMix="numeric",
                                gaussianMix="array",
                                summaryMixture="data.table",
                                tcdfsEcdfsCheck="array",
                                densityCurvesAllHeights="array",
                                eachComponentsCounts="array",
                                updateProgress="function"),
                              validity = function(object) { 
                                return(TRUE)
                              }
)

#' Constructor method of AFMImageGaussianMixAnalysis Class.
#' 
#' @param .Object an AFMImageGaussianMixAnalysis object
#' @rdname AFMImageGaussianMixAnalysis-class
#' @export
setMethod("initialize",
          "AFMImageGaussianMixAnalysis",
          function(.Object) {
            .Object@minGaussianMix<-2
            .Object@maxGaussianMix<-2
            .Object@epsilonGaussianMix<-1e-4
            .Object@gaussianMix<-array()
            .Object@summaryMixture<-data.table()
            .Object@tcdfsEcdfsCheck<-array()
            .Object@densityCurvesAllHeights<-array()
            .Object@eachComponentsCounts<-array()
            validObject(.Object) ## valide l'objet
            return(.Object)
          })

#' Wrapper function AFMImageGaussianMixAnalysis
#'
#' @rdname AFMImageGaussianMixAnalysis-class
#' @export
AFMImageGaussianMixAnalysis <- function() {
  return(new("AFMImageGaussianMixAnalysis"))
}

#' Method \code{eachComponentsCounts} returns a data.table of Gaussian mixes
#' @name AFMImageGaussianMixAnalysis-class
#' @rdname AFMImageGaussianMixAnalysis-class
setGeneric("summaryMixture",function(object){standardGeneric("summaryMixture")})
setGeneric(name= "summaryMixture<-", 
           def= function(AFMImageGaussianMixAnalysis, value) {
             return(standardGeneric("summaryMixture<-"))
           })


#' @rdname AFMImageGaussianMixAnalysis-class
#' @aliases summaryMixture
#' @param object a \code{\link{AFMImageGaussianMixAnalysis}}
setMethod("summaryMixture",signature=signature(object='AFMImageGaussianMixAnalysis'),
          function(object) {
            return(object@summaryMixture)
          }
)
setReplaceMethod(f="summaryMixture",
                 signature(AFMImageGaussianMixAnalysis = "AFMImageGaussianMixAnalysis", value = "data.table"),
                 definition= function(AFMImageGaussianMixAnalysis, value) {
                   AFMImageGaussianMixAnalysis@summaryMixture <- value
                   return(AFMImageGaussianMixAnalysis)
                 })

#' Method \code{eachComponentsCounts} returns a data.table of Gaussian mixes
#' @name AFMImageGaussianMixAnalysis-class
#' @rdname AFMImageGaussianMixAnalysis-class
setGeneric("eachComponentsCounts",function(object){standardGeneric("eachComponentsCounts")})
setGeneric(name= "eachComponentsCounts<-", 
           def= function(AFMImageGaussianMixAnalysis, value) {
             return(standardGeneric("eachComponentsCounts<-"))
           })


#' @rdname AFMImageGaussianMixAnalysis-class
#' @aliases eachComponentsCounts
setMethod("eachComponentsCounts",signature=signature(object='AFMImageGaussianMixAnalysis'),
          function(object) {
            return(object@eachComponentsCounts)
          }
)
setReplaceMethod(f="eachComponentsCounts",
                 signature(AFMImageGaussianMixAnalysis = "AFMImageGaussianMixAnalysis", value = "array"),
                 definition= function(AFMImageGaussianMixAnalysis, value) {
                   AFMImageGaussianMixAnalysis@eachComponentsCounts <- value
                   return(AFMImageGaussianMixAnalysis)
                 })


#' Method \code{tcdfsEcdfsCheck} returns a data.table of Gaussian mixes
#' @name AFMImageGaussianMixAnalysis-class
#' @rdname AFMImageGaussianMixAnalysis-class
setGeneric("tcdfsEcdfsCheck",function(object){standardGeneric("tcdfsEcdfsCheck")})
setGeneric(name= "tcdfsEcdfsCheck<-", 
           def= function(AFMImageGaussianMixAnalysis, value) {
             return(standardGeneric("tcdfsEcdfsCheck<-"))
           })



#' Method \code{densityCurvesAllHeights} returns a data.table of Gaussian mixes
#' @name AFMImageGaussianMixAnalysis-class
#' @rdname AFMImageGaussianMixAnalysis-class
setGeneric("densityCurvesAllHeights",function(object){standardGeneric("densityCurvesAllHeights")})
setGeneric(name= "densityCurvesAllHeights<-", 
           def= function(AFMImageGaussianMixAnalysis, value) {
             return(standardGeneric("densityCurvesAllHeights<-"))
           })


#' @rdname AFMImageGaussianMixAnalysis-class
#' @aliases densityCurvesAllHeights
setMethod("densityCurvesAllHeights",signature=signature(object='AFMImageGaussianMixAnalysis'),
          function(object) {
            return(object@densityCurvesAllHeights)
          }
)
setReplaceMethod(f="densityCurvesAllHeights",
                 signature(AFMImageGaussianMixAnalysis = "AFMImageGaussianMixAnalysis", value = "array"),
                 definition= function(AFMImageGaussianMixAnalysis, value) {
                   AFMImageGaussianMixAnalysis@densityCurvesAllHeights <- value
                   return(AFMImageGaussianMixAnalysis)
                 })


#' Method \code{tcdfsEcdfsCheck} returns a data.table of Gaussian mixes
#' @name AFMImageGaussianMixAnalysis-class
#' @rdname AFMImageGaussianMixAnalysis-class
setGeneric("tcdfsEcdfsCheck",function(object){standardGeneric("tcdfsEcdfsCheck")})
setGeneric(name= "tcdfsEcdfsCheck<-", 
           def= function(AFMImageGaussianMixAnalysis, value) {
             return(standardGeneric("tcdfsEcdfsCheck<-"))
           })


#' @rdname AFMImageGaussianMixAnalysis-class
#' @aliases tcdfsEcdfsCheck
setMethod("tcdfsEcdfsCheck",signature=signature(object='AFMImageGaussianMixAnalysis'),
          function(object) {
            return(object@tcdfsEcdfsCheck)
          }
)
setReplaceMethod(f="tcdfsEcdfsCheck",
                 signature(AFMImageGaussianMixAnalysis = "AFMImageGaussianMixAnalysis", value = "array"),
                 definition= function(AFMImageGaussianMixAnalysis, value) {
                   AFMImageGaussianMixAnalysis@tcdfsEcdfsCheck <- value
                   return(AFMImageGaussianMixAnalysis)
                 })





#' Method \code{GaussianMix} returns a data.table of Gaussian mixes
#' @name AFMImageGaussianMixAnalysis-class
#' @rdname AFMImageGaussianMixAnalysis-class
setGeneric("gaussianMix",function(object){standardGeneric("gaussianMix")})
setGeneric(name= "gaussianMix<-", 
           def= function(AFMImageGaussianMixAnalysis, value) {
             return(standardGeneric("gaussianMix<-"))
           })


#' @rdname AFMImageGaussianMixAnalysis-class
#' @aliases gaussianMix
setMethod("gaussianMix",signature=signature(object='AFMImageGaussianMixAnalysis'),
          function(object) {
            return(object@gaussianMix)
          }
)
setReplaceMethod(f="gaussianMix",
                 signature(AFMImageGaussianMixAnalysis = "AFMImageGaussianMixAnalysis", value = "array"),
                 definition= function(AFMImageGaussianMixAnalysis, value) {
                   AFMImageGaussianMixAnalysis@gaussianMix <- value
                   return(AFMImageGaussianMixAnalysis)
                 })

#' Method \code{minGaussianMix} returns a data.table of Gaussian mixes
#' @name AFMImageGaussianMixAnalysis-class
#' @rdname AFMImageGaussianMixAnalysis-class
setGeneric("minGaussianMix",function(object){standardGeneric("minGaussianMix")})
setGeneric(name= "minGaussianMix<-", 
           def= function(AFMImageGaussianMixAnalysis, value) {
             return(standardGeneric("minGaussianMix<-"))
           })


#' @rdname AFMImageGaussianMixAnalysis-class
#' @aliases minGaussianMix
setMethod("minGaussianMix",signature=signature(object='AFMImageGaussianMixAnalysis'),
          function(object) {
            return(object@minGaussianMix)
          }
)
setReplaceMethod(f="minGaussianMix",
                 signature(AFMImageGaussianMixAnalysis = "AFMImageGaussianMixAnalysis", value = "numeric"),
                 definition= function(AFMImageGaussianMixAnalysis, value) {
                   AFMImageGaussianMixAnalysis@minGaussianMix <- value
                   return(AFMImageGaussianMixAnalysis)
                 })



#' Method \code{maxGaussianMix} returns a data.table of Gaussian mixes
#' @name AFMImageGaussianMixAnalysis-class
#' @rdname AFMImageGaussianMixAnalysis-class
setGeneric("maxGaussianMix",function(object){standardGeneric("maxGaussianMix")})
setGeneric(name= "maxGaussianMix<-", 
           def= function(AFMImageGaussianMixAnalysis, value) {
             return(standardGeneric("maxGaussianMix<-"))
           })


#' @rdname AFMImageGaussianMixAnalysis-class
#' @aliases maxGaussianMix
setMethod("maxGaussianMix",signature=signature(object='AFMImageGaussianMixAnalysis'),
          function(object) {
            return(object@maxGaussianMix)
          }
)
setReplaceMethod(f="maxGaussianMix",
                 signature(AFMImageGaussianMixAnalysis = "AFMImageGaussianMixAnalysis", value = "numeric"),
                 definition= function(AFMImageGaussianMixAnalysis, value) {
                   AFMImageGaussianMixAnalysis@maxGaussianMix <- value
                   return(AFMImageGaussianMixAnalysis)
                 })



#' Method \code{epsilonGaussianMix} returns a data.table of Gaussian mixes
#' @name AFMImageGaussianMixAnalysis-class
#' @rdname AFMImageGaussianMixAnalysis-class
setGeneric("epsilonGaussianMix",function(object){standardGeneric("epsilonGaussianMix")})
setGeneric(name= "epsilonGaussianMix<-", 
           def= function(AFMImageGaussianMixAnalysis, value) {
             return(standardGeneric("epsilonGaussianMix<-"))
           })


#' @rdname AFMImageGaussianMixAnalysis-class
#' @aliases epsilonGaussianMix
setMethod("epsilonGaussianMix",signature=signature(object='AFMImageGaussianMixAnalysis'),
          function(object) {
            return(object@epsilonGaussianMix)
          }
)
setReplaceMethod(f="epsilonGaussianMix",
                 signature(AFMImageGaussianMixAnalysis = "AFMImageGaussianMixAnalysis", value = "numeric"),
                 definition= function(AFMImageGaussianMixAnalysis, value) {
                   AFMImageGaussianMixAnalysis@epsilonGaussianMix <- value
                   return(AFMImageGaussianMixAnalysis)
                 })





#' Perform  the calculation for the Gaussian mixes
#' 
#' \code{\link{performGaussianMixCalculation}} perform all the calculation for PSD exploitation
#' @param AFMImageGaussianMixAnalysis an \code{\link{AFMImageGaussianMixAnalysis}} to manage and store the results of PSD analysis
#' @param AFMImage an \code{\link{AFMImage}} from Atomic Force Microscopy
#' @author M.Beauvais
#' @export
#' @examples
#' \dontrun{
#' library(AFM)
#' 
#' data(AFMImageCollagenNetwork)
#' 
#' AFMImage<-AFMImageCollagenNetwork
#' AFMImage@fullfilename<-"/Users/one/AFMImageCollagenNetwork.txt"
#' gMixAnalysis<-AFMImageGaussianMixAnalysis()
#' # from two components
#' gMixAnalysis@minGaussianMix<-2
#' # to four components
#' gMixAnalysis@maxGaussianMix<-4
#' # convergence criteria
#' gMixAnalysis@epsilonGaussianMix<-1e-4
#' # Create a closure to update progress
#' gMixAnalysis@updateProgress<- function(value = NULL, detail = NULL, message = NULL) {
#'   if (exists("progressGaussianMix")){
#'     if (!is.null(message)) {
#'       progressGaussianMix$set(message = message, value = 0)
#'     }else{
#'       progressGaussianMix$set(value = value, detail = detail)
#'     }
#'   }
#' }
#' gMixAnalysis<-performGaussianMixCalculation(AFMImageGaussianMixAnalysis= gMixAnalysis, AFMImage)
#' print("done performGaussianMixCalculation")
#' }
performGaussianMixCalculation<-function(AFMImageGaussianMixAnalysis, AFMImage) {
  number_of_components<-ecdf<-density<-NULL
  
  
  # if (is.function(AFMImagePSDAnalysis@updateProgress)) {
  #   AFMImagePSDAnalysis@updateProgress(message="1/3 - Calculating PSD2D", value=0)
  # }
  
  # if (is.function(AFMImageGaussianMixAnalysis@updateProgress)&&
  #     !is.null(AFMImageGaussianMixAnalysis@updateProgress())) {
  #   AFMImageGaussianMixAnalysis@updateProgress(message="Calculating Gaussian Mixes", value=0)
  # }  
  
  if (is.function(AFMImageGaussianMixAnalysis@updateProgress)) {
    AFMImageGaussianMixAnalysis@updateProgress(message="Calculating Gaussian Mixes", value=0)
  }

  
  #data(AFMImageCollagenNetwork)
  #AFMImage<-AFMImageCollagenNetwork
  
  # parameters
  min<-AFMImageGaussianMixAnalysis@minGaussianMix
  max<-AFMImageGaussianMixAnalysis@maxGaussianMix
  mepsilon<-AFMImageGaussianMixAnalysis@epsilonGaussianMix
  
  gaussianMixList = array(list(), max)

  min_height<- 0
  max_height<- 3000
  heights<-AFMImage@data$h
  heights<-heights[heights<(max_height/10)]

  # allH<-data.table(h=heights)
  # g<-ggplot(allH, aes(h)) + geom_histogram(binwidth = 0.1)
  # print(g)

  mixtureCounter<-0
  mixtureNumberOfComponents<-min
  for(mixtureNumberOfComponents in seq(min,max)){
    if (is.function(AFMImageGaussianMixAnalysis@updateProgress)) {
      mixtureCounter<-mixtureCounter+1
      AFMImageGaussianMixAnalysis@updateProgress(message=paste("Calculating Gaussian Mixes", mixtureCounter ,"/",(as.numeric(max)-as.numeric(min)+1)) ,  value=0) #detail = paste0((as.numeric(max)-as.numeric(min)+1),"/",(as.numeric(max))),
    }
    

      heights.k<- mixtools::normalmixEM(heights,
                                        k=mixtureNumberOfComponents,
                                        arbmean = TRUE,
                                        ECM=TRUE,
                                        verb=TRUE,
                                        maxit=10000,
                                        epsilon=mepsilon)
      #heights.k
      gaussianMixList[[mixtureNumberOfComponents]]<-heights.k
  }
  
  AFMImageGaussianMixAnalysis@gaussianMix<-gaussianMixList
  
  # creating the summary
  res=data.table(number_of_components=c(0),
                 #component=c(0),
                 mean=c(0),
                 sd=c(0),
                 lambda=c(0))
  
  
  totalNbOfMixtures<-length(AFMImageGaussianMixAnalysis@gaussianMix)
  #totalNbOfMixtures<-length(gMixAnalysis@gaussianMix)
  
  for (mixtureNumberOfComponents in seq(AFMImageGaussianMixAnalysis@minGaussianMix,totalNbOfMixtures)) {
    #for (mixtureNumberOfComponents in seq(gMixAnalysis@minGaussianMix,totalNbOfMixtures)) {
    
    if (!is.null(AFMImageGaussianMixAnalysis@gaussianMix[mixtureNumberOfComponents][[1]])) {
      mixture<-AFMImageGaussianMixAnalysis@gaussianMix[mixtureNumberOfComponents][[1]]
      #mixture<-gMixAnalysis@gaussianMix[mixtureNumberOfComponents][[1]]
      for(component.number in seq(1, mixtureNumberOfComponents)) {
        if (length(mixture)>0) {
          mean=mixture$mu[component.number]
          sd=mixture$sigma[component.number]
          lambda=mixture$lambda[component.number]
          
          res=rbind(res, data.table(number_of_components=mixtureNumberOfComponents,
                                    #component=component.number,
                                    mean=mean,
                                    sd=sd,
                                    lambda=lambda))
        }
      }
    }
  }
  res<-res[-1,]
  res<-res[order(number_of_components, mean)]
  res
  
  AFMImageGaussianMixAnalysis@summaryMixture<-res
  
  # creating points to draw curves
  AFMImageGaussianMixAnalysis@tcdfsEcdfsCheck<-array(list(), max)
  AFMImageGaussianMixAnalysis@densityCurvesAllHeights<-array(list(), max)
  AFMImageGaussianMixAnalysis@eachComponentsCounts<-array(list(), max)
  

  heights<-AFMImage@data$h
  distinct.heights <- sort(unique(heights))
  
  totalNbOfMixtures<-length(AFMImageGaussianMixAnalysis@gaussianMix) - length(AFMImageGaussianMixAnalysis@gaussianMix[sapply(AFMImageGaussianMixAnalysis@gaussianMix, is.null)])
  print(totalNbOfMixtures)
  
  
  for (mixtureNumberOfComponents in seq(AFMImageGaussianMixAnalysis@minGaussianMix,length(AFMImageGaussianMixAnalysis@gaussianMix))) {
    baseSheetName<-paste0(mixtureNumberOfComponents,"-components-")
    print(paste("mixtureNumberOfComponents= ",mixtureNumberOfComponents))
    if (!is.null(AFMImageGaussianMixAnalysis@gaussianMix[mixtureNumberOfComponents][[1]])) {
      #if (!is.null(gMixAnalysis@gaussianMix[mixtureNumberOfComponents][[1]])) {
      heights.k<-AFMImageGaussianMixAnalysis@gaussianMix[mixtureNumberOfComponents][[1]]
      #heights.k<-gMixAnalysis@gaussianMix[mixtureNumberOfComponents][[1]]
      
      tcdfs <- pnormmix(distinct.heights,mixture=heights.k)
      ecdfs <- ecdf(heights)(distinct.heights)
      TheExpDT<-data.table(tcdfs,ecdfs)
      
      AFMImageGaussianMixAnalysis@tcdfsEcdfsCheck[[mixtureNumberOfComponents]]<-TheExpDT

      densityCurves<-data.frame(x=density(heights)$x , y=density(heights)$y, style=rep("Kernel", length(density(heights)$y)))
      x <- seq(min(density(heights)$x),max(density(heights)$x),length=1000)
      densityCurves2<-data.frame(x=x, y=dnormalmix(x,heights.k), style=rep("Mixture", length(dnormalmix(x,heights.k))))
      allHeights<-rbind(densityCurves,densityCurves2)

      AFMImageGaussianMixAnalysis@densityCurvesAllHeights[[mixtureNumberOfComponents]]<-allHeights
            
      allComponents<-data.table(heights=c(0),counts=c(0), component.number=c(0))

      for(component.number in seq(1, mixtureNumberOfComponents)) {
        tlength=1000
        x <- seq(min(density(heights)$x),max(density(heights)$x),length=tlength)
        y   <- dnorm(x,mean=(heights.k$mu[component.number]), sd=heights.k$sigma[component.number])*length(heights)*heights.k$lambda[component.number]
        allComponents<-rbind(allComponents, data.table(heights=x,counts=y, component.number=rep(component.number,tlength)))
      }
      allComponents<-allComponents[-1,]

      AFMImageGaussianMixAnalysis@eachComponentsCounts[[mixtureNumberOfComponents]]<-allComponents
    }
  }
  

  
  
  #print(gaussianMixList)
  return(AFMImageGaussianMixAnalysis)
}

getGaussianMix<-function(exportDirectory, sampleName) {
  exportCsvFilename<-paste(sampleName,"-gaussian-mix.png", sep="")
  exportCsvFullFilename<-paste(exportDirectory, exportCsvFilename, sep="/")
  return(exportCsvFullFilename)
}

#' pnormmix distribution of a mixture of normals
#' 
#' @param q a vector of quantiles
#' @param mixture a gaussian mixture
#' @export
pnormmix <- function(q,mixture) {
  lambda <- mixture$lambda
  k <- length(lambda)
  pnorm.from.mix <- function(q,component) {
    lambda[component]*pnorm(q,mean=mixture$mu[component],
                            sd=mixture$sigma[component])
  }
  pnorms <- sapply(1:k,pnorm.from.mix,q=q)
  return(rowSums(pnorms))
}

#' dnormalmix density of a mixture of normals
#' 
#' @param x a vector of quantiles
#' @param mixture a gaussian mixture
#' @param log perform a log transsformation of the result
#' @export
dnormalmix <- function(x,mixture,log=FALSE) {
  lambda <- mixture$lambda
  k <- length(lambda)
  like.component <- function(x,component) {
    lambda[component]*dnorm(x,mean=mixture$mu[component],
                            sd=mixture$sigma[component])
  }
  likes <- sapply(1:k,like.component,x=x)
  d <- rowSums(likes)
  if (log) {
    d <- log(d)
  }
  return(d)
}

#' loglike sum of density of a mixture of normals
#' 
#' @param x a vector of quantiles
#' @param mixture a gaussian mixture
#' @export
loglike.normalmix <- function(x,mixture) {
  loglike <- dnormalmix(x,mixture,log=TRUE)
  return(sum(loglike))
}



