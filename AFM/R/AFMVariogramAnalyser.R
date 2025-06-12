require("pracma")

require("data.table")

require("gstat")
require(sp)


require("stringr")

require(gridExtra)
#require(reshape2)
require(ggplot2)

if(getRversion() >= "3.1.0") utils::suppressForeignCheck(c("h", "x", "y","TheData","chosenFitSample","predict.gstat"))

#' @title AFM Image Variogram Model class
#' 
#' @description \code{AFMImageVariogramModel}stores the evaluation of one experimental variogram model
#' 
#' @slot model the variogram model name
#' @slot fit.v the values from the \code{\link[gstat]{fit.variogram}} function in the gstat package
#' @slot mykrige the values from the \code{\link[gstat]{krige}} function in the gstat library
#' @slot res a data.table to store: (cor) the correlation between the predicted sample and the real sample (press) the sum of the square of the differences between real and predicted values for each point of the sample
#' @slot cor to be removed ?
#' @slot press to be removed ?
#' @slot sill to be removed ?
#' @slot imageFullfilename to be removed ?
#' @name AFMImageVariogramModel-class
#' @rdname AFMImageVariogramModel-class
#' @exportClass AFMImageVariogramModel
#' @author M.Beauvais
AFMImageVariogramModel<-setClass("AFMImageVariogramModel",
                                 slots = c(model="character", 
                                           fit.v="data.table",
                                           mykrige="SpatialPointsDataFrame",
                                           res="data.table",
                                           cor="numeric", 
                                           press="numeric", 
                                           sill="numeric",
                                           imageFullfilename="character")
)

#' Constructor method of AFMImageVariogramModel Class.
#'
#' @param .Object an AFMImageVariogramModel object
#' @param model the variogram model name
#' @param fit.v the values from the \code{\link[gstat]{fit.variogram}} function in the gstat package
#' @param mykrige the values from the \code{\link[gstat]{krige}} function in the gstat library
#' @param res a data.table to store: (cor) the correlation between the predicted sample and the real sample (press) the sum of the square of the differences between real and predicted values for each point of the sample
#' @param cor to be removed ?
#' @param press to be removed ?
#' @param sill to be removed ?
#' @param imageFullfilename to be removed ?
#' @rdname AFMImageVariogramModel-class
#' @export
setMethod("initialize", "AFMImageVariogramModel", function(.Object,
                                                           model, 
                                                           fit.v=data.table(),
                                                           mykrige,
                                                           res=data.table(),
                                                           cor, 
                                                           press, 
                                                           sill,
                                                           imageFullfilename)
{ 
  if(!missing(model)) .Object@model<- model
  #if(!missing(fit.v)) 
  .Object@fit.v<- fit.v
  if(!missing(mykrige)) .Object@mykrige<- mykrige
  #if(!missing(res)) 
  .Object@res<- res
  if(!missing(cor)) .Object@cor<- cor
  if(!missing(press)) .Object@press<- press
  if(!missing(sill)) .Object@sill<- sill
  if(!missing(imageFullfilename)) .Object@imageFullfilename<- imageFullfilename
  validObject(.Object)      
  return(.Object)
})

#' Wrapper function AFMImageVariogramModel
#'
#' @rdname AFMImageVariogramModel-class
#' @export
AFMImageVariogramModel <- function() {
  return(new("AFMImageVariogramModel"))
}

#' @title AFM Image log-log experimental variogram slope analysis
#' 
#' @description \code{omniVariogramSlopeAnalysis} stores the analysis of the second slope in roughness against lenghtscale
#' 
#' @slot intersection_sill to be removed ?
#' @slot sill to be removed ?
#' @slot slope to be removed ?
#' @slot yintersept to be removed ?
#' @name omniVariogramSlopeAnalysis-class
#' @rdname omniVariogramSlopeAnalysis-class
#' @exportClass omniVariogramSlopeAnalysis
#' @author M.Beauvais
omniVariogramSlopeAnalysis<-setClass("omniVariogramSlopeAnalysis",
                                     slots = c(intersection_sill="numeric", 
                                               sill="numeric", 
                                               slope="numeric",
                                               yintersept="numeric",
                                               tangente_point1="numeric",
                                               tangente_point2="numeric"),
                                     validity = function(object) { 
                                       return(TRUE)
                                     }
)

#' Constructor method of omniVariogramSlopeAnalysis Class.
#' 
#' @param .Object an omniVariogramSlopeAnalysis object
#' @rdname omniVariogramSlopeAnalysis-class
#' @export
setMethod("initialize",
          "omniVariogramSlopeAnalysis",
          function(.Object) {
            .Object@intersection_sill<-0
            .Object@sill<-0
            .Object@slope<-0
            .Object@yintersept<-0
            .Object@tangente_point1<-0
            .Object@tangente_point2<-0
            validObject(.Object) ## valide l'objet
            return(.Object)
          })

#' Wrapper function omniVariogramSlopeAnalysis
#'
#' @rdname omniVariogramSlopeAnalysis-class
#' @export
omniVariogramSlopeAnalysis <- function() {
  return(new("omniVariogramSlopeAnalysis"))
}

#' @title AFM image variogram analysis class
#' 
#' @description \code{AFMImageVariogramAnalysis} manages the variogram analysis of an \code{\link{AFMImage}}
#'
#' @slot width (optional) a distance step for the calculation of the variograms
#'   (e.g.: width=  integer of (scan Size divided by number of lines)= ceil(1000 / 512) for AFMImageOfAluminiumInterface
#' @slot omnidirectionalVariogram a data.table to store the omnidirectional variogram
#' @slot variogramSlopeAnalysis a AFMImageVariogramAnalysis to analyse slope in log log omnidirectional semivariogram
#' @slot directionalVariograms a data.table to store the directional variograms
#' @slot sampleFitPercentage a sample size as a percentage of random points in the \code{\link{AFMImage}}. These points will be used to fit the variogram models.
#' @slot chosenFitSample the chosen random points  of the \code{\link{AFMImage}} to perform the fitting of the variogram models.
#' @slot cuts the cuts for spplot of the \code{\link{AFMImage}}. The same cuts will be used for the predicted \code{\link{AFMImage}}
#' @slot variogramModels A list of \code{\link{AFMImageVariogramModel}} containing the various evaluated variogram models.
#' @slot fullfilename to be removed ?
#' @slot updateProgress a function to update a graphical user interface
#' @name AFMImageVariogramAnalysis-class
#' @rdname AFMImageVariogramAnalysis-class
#' @exportClass AFMImageVariogramAnalysis 
#' @author M.Beauvais
#'
AFMImageVariogramAnalysis<-setClass("AFMImageVariogramAnalysis",
                                    slots = c(
                                      width="numeric",
                                      sampleVariogramPercentage="numeric",
                                      omnidirectionalVariogram="data.table",
                                      variogramSlopeAnalysis="omniVariogramSlopeAnalysis",
                                      expectedSill="numeric",
                                      expectedRange="numeric",
                                      directionalVariograms="data.table",
                                      sampleFitPercentage="numeric",
                                      sampleValidatePercentage="numeric",
                                      chosenFitSample="numeric",
                                      cuts="numeric",
                                      variogramModels="list",
                                      fullfilename="character",
                                      updateProgress="function"),
                                    validity = function(object) { 
                                      if (object@sampleFitPercentage > 1 ) {
                                        ## sample can't be more than 100%
                                        print("sample fit percentage can't be more than 100%")
                                        return(FALSE)
                                      } else return(TRUE)
                                    }
)

#' Constructor method of AFMImageVariogramAnalysis Class.
#'
#' @param .Object an AFMImageVariogramAnalysis class
#' @param sampleFitPercentage a sample size as a percentage (e.g. "5" for 5 percents) of random points in the \code{\link{AFMImage}}. These points will be used to fit the variogram models.
#' @param updateProgress a function to update a graphical user interface
#' @rdname AFMImageVariogramAnalysis-class
#' @export
setMethod("initialize",
          "AFMImageVariogramAnalysis",
          function(.Object, sampleFitPercentage, updateProgress) {
            if (!missing(updateProgress)) {
              .Object@updateProgress<-updateProgress  
            }
            .Object@width<-0
            .Object@sampleFitPercentage <- sampleFitPercentage
            .Object@omnidirectionalVariogram<-data.table()
            .Object@directionalVariograms<-data.table()
            #.Object@updateProgress
            validObject(.Object) ## valide l'objet
            return(.Object)
          })

#' Wrapper function AFMImageVariogramAnalysis
#'
#' @rdname AFMImageVariogramAnalysis-class
#' @export
AFMImageVariogramAnalysis <- function(sampleFitPercentage) {
  return(new("AFMImageVariogramAnalysis", sampleFitPercentage=sampleFitPercentage))
}


#' Method \code{variogramModels} returns a list of variogram model evaluation
#' @name AFMImageVariogramAnalysis-class
#' @rdname AFMImageVariogramAnalysis-class
#' @param object a \code{AFMImageVariogramAnalysis} object
setGeneric("variogramModels",function(object){standardGeneric("variogramModels")})
setGeneric(name= "variogramModels<-", 
           def= function(AFMImageVariogramAnalysis, value) {
             return(standardGeneric("variogramModels<-"))
           })

#' @rdname AFMImageVariogramAnalysis-class
#' @aliases variogramModels
setMethod("variogramModels",signature=signature(object='AFMImageVariogramAnalysis'),
          function(object) {
            return(object@variogramModels)
          }
)
setReplaceMethod(f="variogramModels", 
                 signature(AFMImageVariogramAnalysis = "AFMImageVariogramAnalysis", value = "list"),
                 definition= function(AFMImageVariogramAnalysis, value) {
                   if (is.null(value)) {
                     print("variogramModels is null")
                     return(AFMImageVariogramAnalysis)
                   }
                   AFMImageVariogramAnalysis@variogramModels <- value
                   return(AFMImageVariogramAnalysis)
                 })

#' Method \code{omnidirectionalVariogram} returns the omnidirectional variogram
#' @name AFMImageVariogramAnalysis-class
#' @rdname AFMImageVariogramAnalysis-class
setGeneric("omnidirectionalVariogram",function(object){standardGeneric("omnidirectionalVariogram")})
setGeneric(name= "omnidirectionalVariogram<-", 
           def= function(AFMImageVariogramAnalysis, value) {
             return(standardGeneric("omnidirectionalVariogram<-"))
           })

#' @rdname AFMImageVariogramAnalysis-class
#' @aliases omnidirectionalVariogram
setMethod("omnidirectionalVariogram",signature=signature(object='AFMImageVariogramAnalysis'),
          function(object) {
            return(object@omnidirectionalVariogram)
          }
)
setReplaceMethod(f="omnidirectionalVariogram",
                 signature(AFMImageVariogramAnalysis = "AFMImageVariogramAnalysis", value = "data.table"),
                 definition= function(AFMImageVariogramAnalysis, value) {
                   AFMImageVariogramAnalysis@omnidirectionalVariogram <- value
                   return(AFMImageVariogramAnalysis)
                 })

#' Method \code{directionalVariograms} returns the directional variograms
#' @name AFMImageVariogramAnalysis-class
#' @rdname AFMImageVariogramAnalysis-class
setGeneric("directionalVariograms",function(object){standardGeneric("directionalVariograms")})
setGeneric(name= "directionalVariograms<-", 
           def= function(AFMImageVariogramAnalysis, value) {
             return(standardGeneric("directionalVariograms<-"))
           })

#' @rdname AFMImageVariogramAnalysis-class
#' @aliases directionalVariograms
setMethod("directionalVariograms",signature=signature(object='AFMImageVariogramAnalysis'),
          function(object) {
            return(object@directionalVariograms)
          }
)
setReplaceMethod(f="directionalVariograms", 
                 signature(AFMImageVariogramAnalysis = "AFMImageVariogramAnalysis", value = "data.table"),
                 definition= function(AFMImageVariogramAnalysis, value) {
                   AFMImageVariogramAnalysis@directionalVariograms <- value
                   return(AFMImageVariogramAnalysis)
                 })

#' getDTModelEvaluation method
#' 
#' @param AFMImageVariogramAnalysis an AFMImageVariogramAnalysis object
#' @rdname AFMImageVariogramAnalysis-getDTModelEvaluation-method
#' @exportMethod getDTModelEvaluation
setGeneric(name= "getDTModelEvaluation", 
           def= function(AFMImageVariogramAnalysis) {
             return(standardGeneric("getDTModelEvaluation"))
           })

#' @name getDTModelEvaluation
#' @aliases getDTModelEvaluation getDTModelEvaluation,AFMImageVariogramAnalysis-method
#' @docType methods
#' @rdname AFMImageVariogramAnalysis-getDTModelEvaluation-method
#' @export
setMethod(f="getDTModelEvaluation", "AFMImageVariogramAnalysis",
          definition= function(AFMImageVariogramAnalysis) {
            res = data.table()
            totalL<-length(AFMImageVariogramAnalysis@variogramModels)
            if (totalL>0) {
              res = data.table()
              for (i in seq(1:totalL)){
                res=rbind(res, AFMImageVariogramAnalysis@variogramModels[[i]]@res)
              }
            }
            return(res)
          })

#' getDTModelSillRange method
#' 
#' @param AFMImageVariogramAnalysis an AFMImageVariogramAnalysis object
#' @rdname AFMImageVariogramAnalysis-getDTModelSillRange-method
#' @exportMethod getDTModelSillRange
setGeneric(name= "getDTModelSillRange", 
           def= function(AFMImageVariogramAnalysis) {
             return(standardGeneric("getDTModelSillRange"))
           })

#' @name getDTModelSillRange
#' @aliases getDTModelSillRange getDTModelSillRange,AFMImageVariogramAnalysis-method
#' @docType methods
#' @rdname AFMImageVariogramAnalysis-getDTModelSillRange-method
#' @export
setMethod(f="getDTModelSillRange", "AFMImageVariogramAnalysis",
          definition= function(AFMImageVariogramAnalysis) {
            res = data.table()
            totalL<-length(AFMImageVariogramAnalysis@variogramModels)
            if (totalL>0) {
              res = data.table()
              for (i in seq(1:totalL)){
                fit.v<-AFMImageVariogramAnalysis@variogramModels[[i]]@fit.v
                model<-paste(fit.v$model[1], fit.v$model[2],sep=", ")
                #print(model)
                value<-data.frame(model, sill=sum(fit.v$psill), range = sum(fit.v$range))
                res=rbind(res,value)
              }
            }
            return(res)
          })



#' Method \code{variogramSlopeAnalysis} returns the slope anaylis on the log-log omnidirectional experimental semi variogram
#' @name AFMImageVariogramAnalysis-class
#' @rdname AFMImageVariogramAnalysis-class
setGeneric("variogramSlopeAnalysis",function(object){standardGeneric("variogramSlopeAnalysis")})
setGeneric(name= "variogramSlopeAnalysis<-", 
           def= function(AFMImageVariogramAnalysis, value) {
             return(standardGeneric("variogramSlopeAnalysis<-"))
           })

#' @rdname AFMImageVariogramAnalysis-class
#' @aliases variogramSlopeAnalysis
setMethod("variogramSlopeAnalysis",signature=signature(object='AFMImageVariogramAnalysis'),
          function(object) {
            return(object@variogramSlopeAnalysis)
          }
)
setReplaceMethod(f="variogramSlopeAnalysis",
                 signature(AFMImageVariogramAnalysis = "AFMImageVariogramAnalysis", value = "omniVariogramSlopeAnalysis"),
                 definition= function(AFMImageVariogramAnalysis, value) {
                   AFMImageVariogramAnalysis@variogramSlopeAnalysis <- value
                   return(AFMImageVariogramAnalysis)
                 })


#' evaluateVariogramModels method to evaluate the basic variogram models
#' 
#' evaluateVariogramModels method to evaluate the basic variogram models available in the \code{\link{gstat}} package
#' A \code{\link{AFMImageVariogramAnalysis}} method to handle the variogram analysis of an \code{\link{AFMImage}}.
#' The variogram models used can be seen with the show.vgms() function from the \code{\link{gstat}} package.
#'
#' @param AFMImageVariogramAnalysis an object
#' @param AFMImage an \code{\link{AFMImage}}
#' @exportMethod evaluateVariogramModels
#' @rdname AFMImageVariogramAnalysis-evaluateVariogramModels-method
#' @examples 
#' \dontrun{
#' library(AFM)
#' 
#' data("AFMImageOfRegularPeaks")
#' # take an extract of the image to fasten the calculation      
#' AFMImage<-extractAFMImage(AFMImageOfRegularPeaks, 40, 40, 32)
#' # e.g. AFMImage@@fullfilename<-"/users/ubuntu/AFMImageOfRegularPeaks-extract.txt"
#' AFMImage@@fullfilename<-paste(tempdir(), "AFMImageOfRegularPeaks-extract.txt", sep="/")
#'  
#' AFMImageAnalyser<-AFMImageAnalyser(AFMImage)
#'  
#'  # Variogram analysis 
#' sampleFitPercentage<-3.43/100
#' variogramAnalysis<-AFMImageVariogramAnalysis(sampleFitPercentage)
#' variogramAnalysis@@omnidirectionalVariogram<- 
#'               AFM::calculateOmnidirectionalVariogram(AFMImage=AFMImage,
#'                                                      AFMImageVariogramAnalysis=variogramAnalysis)
#' variogramAnalysis@@directionalVariograms<- 
#'               AFM::calculateDirectionalVariograms(AFMImage=AFMImage,
#'                                                  AFMImageVariogramAnalysis=variogramAnalysis)
#'  
#' # manage model evaluations
#' AFMImageVariogram<-variogramAnalysis@@omnidirectionalVariogram
#' class(AFMImageVariogram)=c("gstatVariogram","data.frame")
#' variogramAnalysis<-evaluateVariogramModels(variogramAnalysis, AFMImage)
#' 
#' mergedDT<-getDTModelEvaluation(variogramAnalysis)
#' mergedDT
#' sillRangeDT<-getDTModelSillRange(variogramAnalysis)
#' sillRangeDT
#' }
#' 
setGeneric(name= "evaluateVariogramModels", 
           def= function(AFMImageVariogramAnalysis, AFMImage) {
             return(standardGeneric("evaluateVariogramModels"))
           })

#' @name evaluateVariogramModels
#' @aliases evaluateVariogramModels evaluateVariogramModels,AFMImageVariogramAnalysis-method
#' @docType methods
#' @rdname AFMImageVariogramAnalysis-evaluateVariogramModels-method
#' @export
setMethod(f="evaluateVariogramModels", "AFMImageVariogramAnalysis",
          definition= function(AFMImageVariogramAnalysis, AFMImage) {
            
            AFMImageVariogram<-AFMImageVariogramAnalysis@omnidirectionalVariogram
            AFMImagesampleFitPercentage<-AFMImageVariogramAnalysis@sampleFitPercentage
            
            class(AFMImageVariogram)=c("gstatVariogram","data.frame")
            filename<-basename(AFMImage@fullfilename)
            
            # get the cut
            #print(typeof(getCutsOfSpplotFromAFMImage(AFMImage)))
            AFMImageVariogramAnalysis@cuts<-getCutsOfSpplotFromAFMImage(AFMImage)
            
            
            # 3.43% for 9000 points to model
            # use 512*512 - 9000 to validate
            #AFMImagesampleFitPercentage<- 3.43/100
            TheData<-as.data.frame(AFMImage@data)
            TheData=na.omit(TheData)
            
            # We randomly split the data into two parts. 
            # From the data, AFMImagesampleFitPercentage observations will be used for variogram modeling 
            # and the rest will be used for prediction and evaluation
            totalSampleSize<-AFMImage@samplesperline*AFMImage@lines
            
            choose<-floor(totalSampleSize*AFMImagesampleFitPercentage)
            chosenFitSample<-sample(1:totalSampleSize, choose) 
            AFMImageVariogramAnalysis@chosenFitSample<-chosenFitSample
            
            #print(paste("Kriging sample size", choose))
            #print(paste("Validation sample size", choose))
            
            part_model <- TheData[chosenFitSample, ]
            part_model2 <- TheData[chosenFitSample, ]
            
            coordinates(part_model) = ~x+y
            #proj4string(part_model)=CRS("+init")
            proj4string(part_model)=CRS()
            is.projected(part_model)
            
            part_valid <- TheData[-chosenFitSample, ] 
            part_valid2 <- TheData[-chosenFitSample, ] 
            part_valid <- TheData
            part_valid2 <- TheData
            coordinates(part_valid) = ~x+y
            #proj4string(part_valid)=CRS("+init")
            proj4string(part_valid)=CRS()
            is.projected(part_valid)
            
            # starting value to fit the models
            expectedSill<-calculateWavModelExpectedSill(AFMImageVariogram)
            expectedRange<-calculateWavModelExpectedRange(AFMImageVariogram, expectedSill)
            expectedNugget<-0
            
            print(paste("expected sill for wav model is", expectedSill))
            print(paste("expected range for wav model is", expectedRange))
            
            # get all vgm models
            allVariogramModelEvaluation<-c()
            notUsedModels<-c("Nug","Int", "Err", "Lin", "Pow", "Leg", "Spl")
            
            notUsedModels<-c("Nug","Int", "Err", "Lin", "Pow", "Leg", "Spl")
            #"Nug","Exp","Sph","Gau","Exc","Mat","Ste","Cir","Lin","Bes","Pen","Per","Wav","Hol","Log","Pow","Spl","Leg","Err","Int"
            # TODO MB
            notUsedModels<-c("Nug","Exp","Sph","Gau","Exc","Mat","Ste","Cir","Lin","Bes","Per","Wav","Hol","Log","Pow","Spl","Leg","Err","Int")
            
            
            # for updateProgress
            counter<-0
            totalCounter<-3*(length(vgm()$short)-length(notUsedModels))
            
            for (testedModel in vgm()$short){
              if (match(testedModel, notUsedModels, nomatch = FALSE)) {
                print(paste("the model", testedModel,"will not be used"))
              }else {
                variogramModelEvaluation <- new("AFMImageVariogramModel")
                #print(testedModel)
                modelName<-paste(testedModel)
                
                tryCatch({
                  psill<-expectedSill
                  range<-expectedRange
                  expectedNugget<-0
                  fitsills = c(FALSE, TRUE)
                  
                  if(testedModel=="Wav") {
                    fitsills = c(TRUE, TRUE)                  
                  }
                  if(testedModel=="Exp") {
                    psill<-expectedSill
                    range<-expectedRange/2.2
                  }
                  if(testedModel=="Bes") {
                    psill<-expectedSill
                    range<-expectedRange/4.4
                  }
                  if (testedModel=="Ste") {
                    psill<-expectedSill
                    range<-expectedRange*0.6
                  }
                  if (testedModel=="Gau") {
                    psill<-5
                    range<-1
                  }
                  if (testedModel=="Log") {
                    psill<-5
                    range<-1
                  }
                  if (testedModel=="Pow") {
                    psill<-5
                    range<-1
                  }
                  if (testedModel=="Exc") {
                    psill<-5
                    range<-1
                    expectedNugget<-expectedSill
                    fitsills = c(TRUE, TRUE)
                  }
                  if (testedModel=="Hol") {
                    psill<-expectedSill
                    range<-expectedRange/2
                    expectedNugget<-0
                  }
                  if (testedModel=="Per") {
                    psill<-expectedSill/2
                    range<-expectedRange
                    expectedNugget<-expectedSill
                    fitsills = c(TRUE, TRUE)
                  }
                  if (testedModel=="Leg") {
                    psill<-expectedSill/2
                    range<-expectedRange
                  }
                  # fit model to experimental variogram
                  #vgm<-vgm(psill= psill,model=testedModel,range= range,nugget=0)
                  print(paste("Fit variogram for model", testedModel, "..."))
                  print(paste("expected sill ", psill))
                  print(paste("expected range ", range))
                  print(paste("expected nugget ", expectedNugget))
                  
                  
                  vgm<-vgm(psill= psill,model=testedModel,range= range,nugget=expectedNugget)
                  
                  #TODO 
                  print("1")
                  if (!is.null(AFMImageVariogramAnalysis@updateProgress)) {
                    if (is.function(AFMImageVariogramAnalysis@updateProgress)&&
                        !is.null(AFMImageVariogramAnalysis@updateProgress())) {
                      counter<-counter+1
                      AFMImageVariogramAnalysis@updateProgress(detail=paste0("fitting ",modelName, " model"), value=counter/totalCounter)
                    }
                  }
                  print("2")
                  fit.v <- fit.variogram(AFMImageVariogram, vgm, fit.sills = fitsills, warn.if.neg=TRUE)
                  #print(fit.v$psill)
                  if (as.numeric(fit.v$psill[2]) > 0) {
                    # krigging on a sample
                    #print("Kriging...")
                    if (!is.null(AFMImageVariogramAnalysis@updateProgress)) {
                      if (is.function(AFMImageVariogramAnalysis@updateProgress)&&
                          !is.null(AFMImageVariogramAnalysis@updateProgress())) {
                        counter<-counter+1
                        AFMImageVariogramAnalysis@updateProgress(detail=paste0("krigging ",modelName, " model"), value=counter/totalCounter)
                      }
                    }
                    
                    mykrige<-mykrigefunction(fit.v, part_model, part_valid)
                    
                    # Evaluate quality of krigging
                    print("Evaluate quality...")
                    if (!is.null(AFMImageVariogramAnalysis@updateProgress)) {
                      if (is.function(AFMImageVariogramAnalysis@updateProgress)&&
                          !is.null(AFMImageVariogramAnalysis@updateProgress())) {
                        counter<-counter+1
                        AFMImageVariogramAnalysis@updateProgress(detail=paste0("evaluating ",modelName, " model"), value=counter/totalCounter)
                      }
                    }
                    
                    myStatsFromKrige<-statsFromKrige(filename, vgm, part_valid,mykrige)
                    res<-data.table(myStatsFromKrige)
                    print(res$cor)
                    if (!is.na(res$cor)){
                      variogramModelEvaluation@model<-testedModel
                      variogramModelEvaluation@fit.v<-data.table(fit.v)
                      variogramModelEvaluation@mykrige<-mykrige  
                      variogramModelEvaluation@res<-data.table(myStatsFromKrige)  
                      allVariogramModelEvaluation<-c(allVariogramModelEvaluation, variogramModelEvaluation)
                      print("done")
                    }else{
                      print(paste("correlation not calculated for model", testedModel, sep=" "))
                    }
                  }else{
                    print(paste("sill is negative for model", testedModel, sep=" "))
                  }
                  
                }, warning=function(w) {
                  message(paste("warning with", testedModel,w))
                  counter<-counter+1
                }
                ,error=function(e) message(paste("Pb2 with", testedModel,e))
                )
              }
            }
            if(!is.null(allVariogramModelEvaluation)) AFMImageVariogramAnalysis@variogramModels<-allVariogramModelEvaluation
            # for each model, evaluate it
            
            return(AFMImageVariogramAnalysis)
          })


#' @title updateProgress
#' @description is a function used by a GUI such as shiny GUI
#' @name updateProgress
#' @aliases updateProgress updateProgress,AFMImageVariogramAnalysis-method
#' @docType methods
#' @param AFMImageVariogramAnalysis an \code{\link{AFMImageVariogramAnalysis}}
#' @param value shiny progress bar value
#' @param detail shiny progress bar detail
#' @param message shiny progress bar message
#' @rdname AFMImageVariogramAnalysis-updateProgress-method
#' @export
setGeneric(name= "updateProgress", 
           def= function(AFMImageVariogramAnalysis, value, detail, message) {
             return(standardGeneric("updateProgress"))
           })

calculateWavModelExpectedSill<-function(omnidirectionalVariogram) {
  return(floor(mean(tail(omnidirectionalVariogram$gamma, floor(length(omnidirectionalVariogram$gamma)/2)))))
}

calculateWavModelExpectedRange<-function(omnidirectionalVariogram, expectedSill) {
  # first intersection (lowest distance) between expectedSill and the omnidirectionalVariogram
  indexDistMin<-head(which(omnidirectionalVariogram$gamma > expectedSill),n=1)
  if (indexDistMin>0) {
    expectedRange<-(omnidirectionalVariogram$dist[indexDistMin])
    # find the fist peak
    thresh = 0
    x<-tail(omnidirectionalVariogram$gamma, n= length(omnidirectionalVariogram$gamma)-indexDistMin)
    pks <- which(diff(sign(diff(x, na.pad = FALSE)), na.pad = FALSE) < 0) + 2
    if (!missing(thresh)) {
      pks[x[pks - 1] - x[pks] > thresh]
    }
    else pks
    
    if (length(pks)>0) {
      #print(paste("pks",pks))
      expectedRange2<-(omnidirectionalVariogram$dist[pks[1]+indexDistMin])
      print(expectedRange)
      print(expectedRange2)
      expectedRange<-(expectedRange+ expectedRange2)/2
    }
  } else expectedRange<-1
  
  return(floor(expectedRange))
}

#' calculate a width to be used for experimental variogram calculation
#' 
#' calculate a width to be used for experimental variogram calculation in order to generate a line
#' instead of a cloud of points. If the chosen width is too small, the experimental variogram will
#' be a cloud of points instead of a line.
#' 
#' \code{getAutomaticWidthForVariogramCalculation} returns the width to be used for variogram calculation
#' @param AFMImage an \code{\link{AFMImage}} from Atomic Force Microscopy
#' @return the smallest width to be used for variogram calculation
#' @author M.Beauvais
#' @rdname AFMImageVariogramAnalyser-getAutomaticWidthForVariogramCalculation
#' @export
#' @examples
#' \dontrun{
#' library(AFM)
#' 
#' data(AFMImageOfAluminiumInterface)
#' print(getAutomaticWidthForVariogramCalculation(AFMImageOfAluminiumInterface))
#' }
#' 
getAutomaticWidthForVariogramCalculation<-function(AFMImage){
  return(ceiling(max(AFMImage@hscansize/AFMImage@samplesperline, AFMImage@vscansize/AFMImage@lines)))
}

#' Calculate experimental omnidirectional semi-variogram
#' 
#' \code{calculateOmnidirectionalVariogram} returns the semivariance calculated for all the directions
#' calculate the experimental omnidirectional variogram of an \code{\link{AFMImage}} with the \code{\link[gstat]{variogram}} function of the gstat package.
#' The experimental semi-variogram is used to fit (find the best sill and range) the theoretical variogram models.
#' With 512*512 images, it takes several minutes to calculate.
#' 
#' @param AFMImage an \code{\link{AFMImage}} from Atomic Force Microscopy
#' @param AFMImageVariogramAnalysis an \code{\link{AFMImageVariogramAnalysis}} to manage and store the result of variogram analysis
#' @return the semivariance calculated in all the directions
#' @rdname AFMImageVariogramAnalyser-calculateOmnidirectionalVariogram
#' @author M.Beauvais
#' @export
#' @examples
#' \dontrun{
#' library(AFM)
#' library(ggplot2)
#' 
#' data(AFMImageOfRegularPeaks)
#' variogramAnalysis<-AFMImageVariogramAnalysis(sampleFitPercentage=3.43/100)
#' avario<-AFM::calculateOmnidirectionalVariogram(AFMImageVariogramAnalysis= variogramAnalysis, 
#'                                                AFMImage= AFMImageOfRegularPeaks)
#' dist<-gamma<-NULL
#' p <- ggplot(avario, aes(x=dist, y=gamma))
#' p <- p + geom_point()
#' p <- p + geom_line()
#' p <- p + ylab("semivariance")
#' p <- p + xlab("distance (nm)")
#' p <- p + ggtitle("Experimental semivariogram")
#' p
#' }
calculateOmnidirectionalVariogram<- function(AFMImageVariogramAnalysis, AFMImage) {
  
  if (is.null(AFMImageVariogramAnalysis@width)||(AFMImageVariogramAnalysis@width==0)) {
    AFMImageVariogramAnalysis@width=getAutomaticWidthForVariogramCalculation(AFMImage)
    
    print(paste("using automatic width of", AFMImageVariogramAnalysis@width))
  }
  width<-AFMImageVariogramAnalysis@width
  
  print(paste("calculating omnidirectional variogram using width of", width,"..."))
  data<-AFMImage@data
  x<-y<-NULL
  setkey(data,x,y)
  coordinates(data) = ~x+y
  #proj4string(data)=sp::CRS("+no_defs")
  proj4string(data)=CRS()
  is.projected(data)
  return(data.table(gstat::variogram(data$h ~ x+y , data, width=width)))
}

#' Calculate experimental directional semi-variograms
#'
#' calculate four experimental directional variograms of an \code{\link{AFMImage}} with the \code{\link[gstat]{variogram}} function of the gstat package.
#' The directional semi-variogram can be used to check the isotropy of the sample.
#' Note: The sample will be isotropic if the slopes of the four variograms are similar.
#' 
#' \code{calculateDirectionalVariograms} returns the directional variograms
#' @param AFMImage an \code{\link{AFMImage}} from Atomic Force Microscopy
#' @param AFMImageVariogramAnalysis an \code{\link{AFMImageVariogramAnalysis}} to manage and store the result of variogram analysis
#' @return Four directional variograms
#' @rdname AFMImageVariogramAnalyser-calculateDirectionalVariograms
#' @author M.Beauvais
#' @export
#' @examples
#' \dontrun{
#' library(AFM)
#' library(ggplot2)
#' 
#' data(AFMImageOfRegularPeaks)
#' variogramAnalysis<-AFMImageVariogramAnalysis(sampleFitPercentage=3.43/100)
#' varios<-AFM::calculateDirectionalVariograms(AFMImage= AFMImageOfRegularPeaks, 
#'                                             AFMImageVariogramAnalysis= variogramAnalysis)
#' dist<-gamma<-NULL
#' p <- ggplot(varios, aes(x=dist, y=gamma,  
#'                         color= as.factor(dir.hor), 
#'                         shape=as.factor(dir.hor)))
#' p <- p + expand_limits(y = 0)
#' p <- p + geom_point()
#' p <- p + geom_line()
#' p <- p + ylab("semivariance (nm^2)")
#' p <- p + xlab("distance (nm)")
#' p <- p + ggtitle("Directional")
#' p
#' }
calculateDirectionalVariograms<- function(AFMImageVariogramAnalysis,AFMImage) {
  print("calculating directional variograms...")
  if (is.null(AFMImageVariogramAnalysis@width)||(AFMImageVariogramAnalysis@width==0)) {
    AFMImageVariogramAnalysis@width<-getAutomaticWidthForVariogramCalculation(AFMImage)
    print(paste("using automatic width of", AFMImageVariogramAnalysis@width))
  }
  
  width<-AFMImageVariogramAnalysis@width
  x<-y<-NULL
  data<-AFMImage@data
  setkey(data,x,y)
  coordinates(data) = ~x+y
  #proj4string(data)=CRS("+init")
  proj4string(data)=CRS()
  is.projected(data)
  return(data.table(gstat::variogram(data$h ~ x+y, data, width=width, alpha = c(0, 45, 90, 135))))
}




mykrigefunction<-function(fit.v, part_model, part_valid) {
  #The predictions will be performed on the other spatial locations than the part valid data set
  krige(h ~ x+y, part_model, part_valid, model = fit.v)
}

statsFromKrige<-function(name, vgm, part_valid, part_valid_pr) {
  modelName<-paste(vgm$model, collapse=", ")
  #Compute the difference between the predicted values and the true values: 
  difference <- part_valid$h - part_valid_pr$var1.pred 
  #summary(difference)
  #Compute the prediction sum of squares
  PRESS <- sum(difference^2)
  data.frame(name= name ,model=modelName, cor= cor(part_valid_pr$var1.pred,part_valid$h), press=PRESS)
}



#' Calculate slopes and intersections in variogram

#' \code{getAutoIntersectionForOmnidirectionalVariogram} returns the slope in the omnidirectional variograms
#' @param AFMImageAnalyser an \code{\link{AFMImageAnalyser}}
#' @return an \code{\link{omniVariogramSlopeAnalysis}}
#' @author M.Beauvais
#' @export
getAutoIntersectionForOmnidirectionalVariogram<-function(AFMImageAnalyser){
  data<-AFMImageAnalyser@variogramAnalysis@omnidirectionalVariogram
  data<-data.table(dist=log10(data$dist), gamma=log10(data$gamma))
  #data$dist<-as.numeric(data$dist)
  
  aval<-max(data$dist)
  index<-which(data$dist<= aval)[1]
  
  lengthData<-length(data$dist)-index
  print(paste("lengthData=",lengthData))
  
  minimumR <- function(data, space, x, y) {
    lengthData<-nrow(data)
    
    aorigin<-0
    #borigin <- data[lengthData]$gamma
    borigin <- max(data$gamma)
    #print(borigin)
    
    finalres2=c()
    finalres = c(Inf,0,0)
    for (i in seq(1, length(x))) {
      x1=x[i]
      for (j in seq(1, length(y))) {
        if (abs(j-i)>=space) {
          x2=y[j]
          
          if ((x1<1)||(x2<1)||(x1>lengthData)||(x2>lengthData)||(x1==x2)) {
            inter <- data[1]$dist
          } else{
            if (x1<x2) {
              myx=data[seq(x1,x2)]$dist
              myy=data[seq(x1,x2)]$gamma
            }
            if (x1>x2) {
              myx=data[seq(x2,x1)]$dist
              myy=data[seq(x2,x1)]$gamma
            }    
            
            res <- lm(myy~myx)
            b<-unname(res$coefficients[1])
            a<-unname(res$coefficients[2])
            inter <- (borigin-b)/a
            if ((inter<finalres[1])&(inter>0)) {
              finalres=c(inter, x1, x2, borigin,a,b)
              print(finalres)
            }
          }
        }
        #print(paste(x1, x2))
      }
      #finalres2=c(finalres2, inter)
    }
    
    omniVariogramSlopeAnalysis = new("omniVariogramSlopeAnalysis")
    omniVariogramSlopeAnalysis@intersection_sill=finalres[1]
    omniVariogramSlopeAnalysis@tangente_point1=finalres[2]
    omniVariogramSlopeAnalysis@tangente_point2=finalres[3]
    omniVariogramSlopeAnalysis@sill=finalres[4]
    omniVariogramSlopeAnalysis@slope=finalres[5]
    omniVariogramSlopeAnalysis@yintersept=finalres[6]
    #print(omniVariogramSlopeAnalysis)
    return(omniVariogramSlopeAnalysis)
  }
  
  lengthData<-nrow(data)
  #newMax=ceiling(data[c(lengthData),]$dist/20)
  newMax=2
  # if (second_slope==FALSE) {
    aby<-1
    print(aby)
    x <- seq(1, newMax,by=aby)
    print(x)
    z <- minimumR(data, space= 1, x,x)
  # } else {
  #   aby<-1
  #   print(aby)
  #   space=ceiling(lengthData/4)
  #   print(space)
  #   x <- seq(newMax,lengthData, by=aby)
  #   z <- minimumR(data, space= space, x,x)
  # }
  return(z)
}

#' Get the graph of the Log Log omnidiretction variogram

#' \code{getLogLogOmnidirectionalSlopeGraph} returns Get the graph of the Log Log omnidirectional variogram
#' @param AFMImageAnalyser an \code{\link{AFMImageAnalyser}}
#' @param withFratcalSlope a boolean to indicate if the graph should contain a line representating the slope for the calculation of the fractal index and topothesy
#' @return a ggplot2 graph
#' @author M.Beauvais
#' @export
#' @examples
#' \dontrun{
#' library(AFM)
#' library(ggplot2)
#' 
#' data(AFMImageOfRegularPeaks)
#' 
#'AFMImageAnalyser = new("AFMImageAnalyser",
#'          fullfilename="/home/ubuntu/AFMImageOfRegularPeaks-Analyser.txt")
#'variogramAnalysis<-AFMImageVariogramAnalysis(sampleFitPercentage=3.43/100)
#'AFMImageAnalyser@variogramAnalysis<-variogramAnalysis
#'AFMImageAnalyser@variogramAnalysis@omnidirectionalVariogram<-
#'      calculateOmnidirectionalVariogram(AFMImage= AFMImageOfRegularPeaks, 
#'                                        AFMImageVariogramAnalysis= variogramAnalysis)
#'p<-getLogLogOmnidirectionalSlopeGraph(AFMImageAnalyser,  withFratcalSlope=TRUE)
#'p
#' } 
getLogLogOmnidirectionalSlopeGraph<-function(AFMImageAnalyser, withFratcalSlope=FALSE) {
  tryCatch({
    omniVariogramSlopeAnalysis=getAutoIntersectionForOmnidirectionalVariogram(AFMImageAnalyser)
    
    
    # resVarioDT <- data.table(
    #   name=basename(AFMImage@fullfilename),
    #   variogram_slope=omniVariogramSlopeAnalysis@slope
    # )
    # 
    # if (!exists("resVario")) {
    #   resVario<-copy(resVarioDT)
    # }else{
    #   resVario=rbind(resVario,resVarioDT)
    # }
    # print(resVario)
    # 
    # 
    # # AFMImageAnalyser@variogramAnalysis@omnidirectionalVariogram
    # # AFMImageAnalyser@variogramAnalysis@omnidirectionalVariogram[5,]
    # # AFMImageAnalyser@variogramAnalysis@omnidirectionalVariogram[7,]
    # 
    
    dist<-gamma<-id<-NULL
    myvgm<-AFMImageAnalyser@variogramAnalysis@omnidirectionalVariogram
    myvgm<-data.table(dist=log10(myvgm$dist), gamma=log10(myvgm$gamma))
    
    p1<-ggplot(myvgm, aes(x = dist, y = gamma)) + geom_point()   
    p1 <- p1 + ylab("log semivariance")
    p1 <- p1 + xlab("log lag distance (nm)")
    if (withFratcalSlope) {
      omniVariogramSlopeAnalysis=getAutoIntersectionForOmnidirectionalVariogram(AFMImageAnalyser)
      p1 <- p1 + geom_abline(intercept = omniVariogramSlopeAnalysis@yintersept, slope = omniVariogramSlopeAnalysis@slope)
    }
    p1 <- p1 + ggtitle(paste0(basename(AFMImage@fullfilename)," semivariance - slope=", omniVariogramSlopeAnalysis@slope ))
    p1 <- p1 + expand_limits(y = 0)
    p1 <- p1 + guides(colour=FALSE)
    return(p1)
    
    # png(file = paste0(exportDirectory,"/",basename(AFMImage@fullfilename),"-variograms.png"), bg = "transparent", width = 1024, height = 768)
    # print(p1)
    # dev.off()
    
    
  }, error = function(e) {print(paste("Impossible to find variograms intersections automaticaly",e))})
  
}




saveSpplotFromKrige<-function(fullfilename, modelName, part_valid_pr, cuts, withoutLegend) {
  if(missing(withoutLegend)) {
    withoutLegend=FALSE
  }
  expectedWidth = 400
  expectHeight = 300
  
  colLimit<-length(cuts)+3
  cols <- getSpplotColors(colLimit) 
  if (withoutLegend) {
    p<-spplot(part_valid_pr["var1.pred"], cuts=cuts, col.regions=cols,key=list(lines=FALSE, col="transparent"))
  }else{
    p<-spplot(part_valid_pr["var1.pred"], cuts=cuts, col.regions=cols)
  }
  print(paste("saving", basename(fullfilename)))
  png(filename=fullfilename, units = "px", width=expectedWidth, height=expectHeight)
  print(p)
  dev.off()
}


getAFMImageFromKrige<-function(AFMImage, vgm, part_valid_pr) {
  predictedImageFilename<-paste(basename(AFMImage@fullfilename), vgm$model[2], "predicted",sep="-")
  predictedImageFullFilename<-paste(dirname(AFMImage@fullfilename), predictedImageFilename, sep="/")
  
  AFMImage(data = data.table(x = AFMImage@data$x,
                             y = AFMImage@data$y, 
                             h = part_valid_pr["var1.pred"]@data), 
           samplesperline = AFMImage@samplesperline, 
           lines = AFMImage@lines, 
           hscansize = AFMImage@hscansize,
           vscansize = AFMImage@vscansize,
           scansize = AFMImage@scansize,
           fullfilename = predictedImageFullFilename)
}

getSpplotColors<-function(colLimit) {
  blues9[3:colLimit]  
}

getCutsOfSpplotFromAFMImage<-function(AFMImage) {
  initialAFMImage<-as.data.frame(AFMImage@data)
  coordinates(initialAFMImage) = ~x+y
  #proj4string(initialAFMImage)=CRS("+init")
  proj4string(initialAFMImage)=CRS()
  is.projected(initialAFMImage)
  
  p<-spplot(initialAFMImage["h"])
  
  mystr<-unlist(strsplit( as.character(p$legend$bottom$args$key$text)," "))
  mystr<-unlist(strsplit(mystr,","))
  cuts<-as.double(unlist(strsplit(mystr, "[^[:digit:]^.^-]")))
  cuts <- unique(cuts[!is.na(cuts)])
  return(cuts)
}



#' Save on disk an AFMImage as a Lattice (trellis) plot
#' 
#' save a Lattice (trellis) plot of an \code{\link{AFMImage}} using the \code{\link[sp]{spplot}} method of the sp package.
#' This function is used to evaluate visually the quality of the predicted surface when a variogram model is used.
#'
#' \code{saveSpplotFromAFMImage} save a a Lattice (trellis) plot of an \code{\link{AFMImage}} on disk
#' @param AFMImage an \code{\link{AFMImage}} from Atomic Force Microscopy
#' @param fullfilename directory and filename to save to png
#' @param expectedWidth (optional) expected width of the saved image. Default is 400px.
#' @param expectHeight (optional) expected height of the saved image. Default is 300px.
#' @param withoutLegend (optional) set at FALSE, the cuts legend will be included in the plot. Default is FALSE.
#' @author M.Beauvais
#' @rdname AFMVariogramAnalyser-saveSpplotFromAFMImage
#' @export
#' @examples 
#' \dontrun{
#' library(AFM)
#' 
#' data(AFMImageOfAluminiumInterface)
#' saveSpplotFromAFMImage(AFMImageOfAluminiumInterface,
#'                        paste(tempdir(), "myFileWithoutLegend.png", sep="/"), 800,800, TRUE)
#' saveSpplotFromAFMImage(AFMImageOfAluminiumInterface, 
#'                        paste(tempdir(), "myFileWithLegend.png", sep="/"), 800,800, FALSE)
#' }
saveSpplotFromAFMImage<-function(AFMImage, fullfilename, expectedWidth, expectHeight, withoutLegend) {
  if (missing(expectedWidth)) expectedWidth = 400
  if (missing(expectHeight))expectHeight = 300
  if(missing(withoutLegend))withoutLegend=FALSE
  
  p<-getSpplotFromAFMImage(AFMImage, expectedWidth, expectHeight, withoutLegend)
  
  png(filename=fullfilename, units = "px", width=expectedWidth, height=expectHeight)
  print(p)
  dev.off()
}

#' Get an AFMImage as a Lattice (trellis) plot
#' 
#' get a Lattice (trellis) plot of an \code{\link{AFMImage}} using the \code{\link[sp]{spplot}} method of the sp package.
#' This function is used to evaluate visually the quality of the predicted surface when a variogram model is used.
#'
#' \code{getSpplotFromAFMImage} get a Lattice (trellis) plot of an \code{\link{AFMImage}} on disk
#' @param AFMImage an \code{\link{AFMImage}} from Atomic Force Microscopy
#' @param expectedWidth (optional) expected width of the saved image. Default is 400px.
#' @param expectHeight (optional) expected height of the saved image. Default is 300px.
#' @param withoutLegend (optional) set at FALSE, the cuts legend will be included in the plot. Default is FALSE.
#' @author M.Beauvais
#' @rdname AFMVariogramAnalyser-getSpplotFromAFMImage
#' @export
#' @examples 
#' \dontrun{
#' library(AFM)
#' 
#' data(AFMImageOfAluminiumInterface)
#' p<-getSpplotFromAFMImage(AFMImageOfAluminiumInterface, 800,800, TRUE)
#' print(p)
#' }
getSpplotFromAFMImage<-function(AFMImage, expectedWidth, expectHeight, withoutLegend) {
  if (missing(expectedWidth)) expectedWidth = 400
  if (missing(expectHeight))expectHeight = 300
  if(missing(withoutLegend))withoutLegend=FALSE
  
  initialAFMImage<-as.data.frame(AFMImage@data)
  coordinates(initialAFMImage) = ~x+y
  #proj4string(initialAFMImage)=CRS("+init")
  proj4string(initialAFMImage)=CRS()
  is.projected(initialAFMImage)
  
  cuts <- getCutsOfSpplotFromAFMImage(AFMImage)
  colLimit<-length(cuts)+3
  cols <- getSpplotColors(colLimit) 
  
  if (withoutLegend) {
    p<-spplot(initialAFMImage["h"], cuts=cuts, col.regions=cols,key=list(lines=FALSE, col="transparent"))
  }else{
    p<-spplot(initialAFMImage["h"], cuts=cuts, col.regions=cols)
  }
  
  #initialAFMImage.lowres <- aggregate(initialAFMImage["h"], fact = 2, fun = mean)
  
  
  
  return(p)
}

getSpplotImagefullfilename<-function(exportDirectory, sampleName) {
  return(paste(exportDirectory, paste(sampleName,"-real.png",sep=""),sep="/"))
}

getDirectionalVarioCsvFullfilename<-function(exportDirectory, sampleName) {
  exportCsvFilename<-paste(sampleName,"-directional-variograms.csv", sep="")
  exportCsvFullFilename<-paste(exportDirectory, exportCsvFilename, sep="/")
  return(exportCsvFullFilename)
}

getDirectionalVarioPngFullfilename<-function(exportDirectory, sampleName) {
  directionalGraphName=paste(sampleName,"directional-variograms",sep="-")
  exportpng2FullFilename<-paste(exportDirectory, paste(directionalGraphName,"png",sep="."),sep="/")
  return(exportpng2FullFilename)
}


getOmnidirectionalVarioCsvFullfilename<-function(exportDirectory, sampleName) {
  exportCsvFilename<-paste(sampleName,"-omnidirectional-variograms.csv", sep="")
  exportCsvFullFilename<-paste(exportDirectory, exportCsvFilename, sep="/")
  return(exportCsvFullFilename)
}


getOmnidirectionalVarioPngFullfilename<-function(exportDirectory, sampleName) {
  omnidirectionalGraphName=paste(sampleName,"omnidirectional-variogram",sep="-")
  exportOpngFullFilename<-paste(exportDirectory, paste(omnidirectionalGraphName,"png",sep="."),sep="/")
  return(exportOpngFullFilename)
}

getVarioPngchosenFitSample<-function(exportDirectory, sampleName) {
  exportpngFilename<-paste(sampleName, "chosen-sample.png",sep="-")
  exportpngFullFilename<-paste(exportDirectory, exportpngFilename, sep="/")
  return(exportpngFullFilename)
}

getSpplotPredictedImageFullfilename<-function(exportDirectory, sampleName, modelName) {
  predictedfilename<-paste(sampleName,modelName,"predicted.png", sep="-")
  predictedfullfilename<-paste(exportDirectory, predictedfilename,sep="/")
  return(predictedfullfilename)
}
