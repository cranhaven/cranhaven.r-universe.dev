#' @title Class for PROSPER results
#' @description This \code{class} simplifies the analysis of PROSPER results, by providing a fixed structure with some basic summary and plot methods.

#' @slot Call \code{character}, the model call of the simulation.
#' @slot simstruc \code{numeric vector} with 2 numbers, the first defines the number of repetitions for the simulation, the second defines the number of simulation cycles in each repetition of the simulation.
#' @slot loci \code{numeric}, the number of loci used, 0 when no genetic is included.
#' @slot simData \code{data.table}, all simulated data is stored here. The first column is treated as the number of the simulation run, the second column the number of the repetition, the third as the simulation cycle/year, the forth as the type of data (genotype based or only year based), the fifth as the genotype (\code{character}) (NA when data is year based). All following columns are up to specific the model design.
#' @keywords internal   

#' @importFrom graphics legend points
#' @importFrom grDevices dev.new
#' @importFrom data.table data.table
#' @import data.table

#' @name prosper-class
#' @rdname prosper-class
#' @exportClass prosper

setClass(Class = "prosper",
         representation = representation(Call = "call",
                                         simstruc = "numeric",
                                         loci = "numeric",
                                         simData = "data.table"),
         prototype = prototype(Call = NULL, simstruc = c(1,1), loci = 2, simData = data.table( "numRep"=rep(1,9), "simCircle"=rep(1,9),"genotype"=as.character(apply(expand.grid(rep(list(0:2),2)),1,paste,collapse="")))),
         validity = function(object){
            if( object@loci < 0 | object@loci > 4)
                stop("Values for loci must be 0 <= loci <= 4")             
            
            if( any(object@simstruc < 0))
                stop("Values for simstruc must be > 0")               
            if( length(object@simstruc) != 2)
                stop("Simstruc must have 2 values")   
            
            if( length(object@simData) < 4) stop("simData needs at least 4 columns. See Details of prosper()")          
            
            if( any(!is.numeric(object@simData[[1]]) | !is.numeric(object@simData[[2]])))
                warning("The first 2 columns of simData must be numeric")                                
            if( !is.character(object@simData[[3]])){
                #warning("The genotype column of simData must be character and is coerced to it.")
                object@simData[[3]] <- as.character(object@simData[[3]])
                set(object@simData, j=4L, value=as.character(object@simData[[3]]))
                } 
            .datatable.aware = TRUE               
            return(TRUE)
         })
         

#' @title Creation of a PROSPER object
#' @description The creation method for PROSPER objects.
#' @export
#' @keywords internal   

# param DFobject \code{data.frame}, must have a column defining the genotypes or only one row with "0" when no genetics are simulated. When genetics are includet, the genotypes are descibet 


#' @param Call \code{character}, the model call of the simulation.
#' @param simstruc \code{numeric vector} with two numbers, the first defines the number of repetitions for the simulation, the second defines the number of simulation cycles in each repetition of the simulation.
#' @param loci \code{numeric}, the number of loci used (up to 4), 0 when no genetic is included.
#' @param simData \code{data.table}, all simulated data is stored here. The first column is treated as the number of the simulation run, the second column the number of the repetition, the third as the simulation cycle/year, the forth as the type of data (genotype based or only year based), the fifth as the genotype (\code{character}) (NA when data is year based). All following columns are up to specific the model design.

#' @details The genotypes are described by their relevant loci (up to 4). Each locus can have 0, 1 or 2 alleles in a diploid genome. So a genotype for two loci is coded like that: 00 (no relevant alleles), 21 (2 alleles at the first locus, and 1 at the second). For this coding \code{loci} must be "2".

#' @examples \donttest{
#' require(data.table)
#' struc_preparation2(Rmx=10, af=c(0.01,0.8), epis=0, dom=c(1,0.3))
#' simdata <- as.data.table(dfgenotype)
#'    simdata[,"repetition":=1]
#'    simdata[,"simcycle":=1]
#'    setcolorder(simdata, c(c("repetition", "simcycle"),
#'    colnames(simdata)[!(colnames(simdata) %in%
#'    c("simulation_run", "repetition", "simcycle"))])) 
#' prosper(simstruc = c(5,10), loci=2, simData = simdata)  }
#'

#' @importFrom methods new 
#' @importFrom data.table setcolorder
#' @return An object of class "prosper".                     


prosper <- function(Call=match.call(definition = sys.function(sys.parent(n=2)), call = sys.call(sys.parent(n=2))), simstruc=numeric(), loci=numeric(), simData=as.data.table()) {
           methods::new("prosper", 
           Call=Call, 
           simstruc = simstruc, loci=loci, simData=simData)
           }                                                                                        
              


         
#' @title Extraction Simulation Data
#' @description \code{intern_SimData} extracts the simulation data from a \code{prosper}-object
#' @export

#' @param object \code{prosper}, the simulation data of this object will be extracted.
#' @return a \code{data.table} object 
#' @examples
#' library(data.table)
#' struc_preparation2(Rmx=10, af=c(0.01,0.8), epis=0, dom=c(1,0.3))
#'    #simdata[,"repetition":=1]
#'    #simdata[,"simcycle":=1]
#'    #setcolorder(simdata, c(c("repetition", "simcycle"),
#'    #colnames(simdata)[!(colnames(simdata) %in%
#'    #c("simulation_run", "repetition", "simcycle"))])) 
#' #prosp_ob <- prosper(simstruc = c(5,10), loci=2, simData = simdata)
#' #intern_SimData(prosp_ob)
#' @keywords internal    


#' @name intern_SimData
#' @importFrom data.table setcolorder

setGeneric("intern_SimData", function(object) {
    standardGeneric("intern_SimData")
})

#' @rdname intern_SimData
setMethod("intern_SimData",
      signature = "prosper",
      definition = function(object) {
          return(object@simData)
})

#' @title replacement of Simulation Data
#' @description \code{intern_SimData<-} replaces the simulation data of a \code{prosper}-object.
#' @param object \code{prosper}, the simulation data of this object replaced.
#' @param value object fitting the requirements for simulation data of the \code{class prosper}, this replaces the existing simulation data. 
#' @return object of class \code{prosper}
#' @keywords internal   

setGeneric("intern_SimData<-",
       function(object, value) {
           standardGeneric("intern_SimData<-")
})



# describeIn intern_SimData replacement by reference is done. Used in \code{struc_addSimData}.
setReplaceMethod("intern_SimData",
             signature="prosper",
             function(object, value) {
                 object@simData<- value
                 validObject(object) # could add other checks
                 return(object)
})



#' @title Wrapper for \code{intern_SimData<-}  generic
#' @description This function ensures the correct form of data, before adding data of a new simulation cycle to a \code{prosper} object using \code{intern_SimData}.
#' @param object a \code{prosper} object
#' @param value code{data.frame} or code{data.table}
#' @param add \code{logical}, when \code{TRUE} adding data to the existing data. Otherwise the value is completely new
#' @export
#' @name struc_addSimData  
#' @keywords internal   

setGeneric( "struc_addSimData",
            function(object, value, add=TRUE) {
              standardGeneric("struc_addSimData")})

#' @rdname struc_addSimData
setMethod("struc_addSimData",
           signature="prosper",
           function( object, value, add=TRUE){
                     if(!is(object,"prosper")) stop("object must be of type prosper")
                     if(!is.data.frame(value)) stop("value must be of type data.table or data.frame")
                     if(add==FALSE)
                        {intern_SimData(object)<-value
                     }else{
                         #check whether the new data fits to the existing
                         differingClasses <-   which(sapply(intern_SimData(object),class) != sapply(value, class)) 
                         if(length(intern_SimData(object)) != length(value)) stop("value must have the same length as simData")
                         if(any(sapply(intern_SimData(object),class) != sapply(value, class)))
                                       {print(paste("These columns have differing classes:", names(intern_SimData(object))[differingClasses]))
                                                              stop("value must have the same classes as simData")}
                            
                         intern_SimData(object)<- rbindlist(list(intern_SimData(object), value), use.names=TRUE)
                         }
                     return(object)
                     })         
         

setMethod( "show",
           "prosper",
           function( object){
            cat("This is an object of class prosper.\n\n")
            cat("The complete PROSPER simulation model Call:\n\n")
            print(object@Call)                                                                                            
            cat("\n\n\n")
            cat("The simulated data:\n\n")
            print(as.data.table(object@simData))       
           }
          )
          
          
###-----------------------------------------------------------------------------          
###---- as(dfgenotype, "data.frame") 
#die as function generieren (generic)
#setAs("prosper", "data.frame", function(from){
#   return(from@simData)
# } )
 
#die (generische) Methode bei "as.data.frame"""anmelden"
#setMethod("as.data.frame",signature(x="prosper"),function(x){return(as(x,"data.frame"))})
#setMethod("as.data.frame",signature(x="prosper"),function(x){as.data.frame(x)})

###---- as(dfgenotype, "data.table") 
setAs("prosper", "data.table", function(from){
   return(from@simData)
 } )
 
#setMethod("as.data.table",signature(x="prosper"),function(x){return(x)})
