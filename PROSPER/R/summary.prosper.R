 #' @title Summary for prosper objects
 #' @rdname summary 
# @aliases summary summary.prosper
 
 #' @seealso \code{\link{plot}}
 #' @description This function gives a first overview of a prosper simulation result.
 #' @export    
 
 #' @details 
 #' For the population dynamic part, means and standard deviations per simulation cycle for every numeric variable is calculated. Variability is introduced by repetitions. Additionally, a simple overview on factorial variables is given. For further calculations the numbers of individuals for each simulation cycle and repetition are provided.
 #'
 #' For the genetic part,  means and standard deviations of proportions of individuals with only sensitive alleles (allSalleles), only resistance alleles (allRalleles), mixed alleles (RSalleles), and the proportions of resistance alleles in the population (Ralleles) are calculated for each simulation cycle. The repetitions are responsible for the variability. 
 

 #' @param object \code{prosper}, the result of a simulation with PROSPER.
 #' @param geneticSumCol column name of the prosper object the genetic summary is built of. \code{character}. 
 #' @param ... not in use. 
 #' @return A named list of data.tables. 

#' @examples
#' data(param.LOLRI)
#' mod_lolri    <- prosper.LOLRI(param.weed=param.LOLRI, area=30, af = c(0.005, 0.01),
#'                                        duration=3, repetitions=2)
#' summary(mod_lolri)
#' @import data.table

#' @name summary

if (!isGeneric("summary"))
      setGeneric("summary", function(object, ...) standardGeneric("summary")) 
      
#' @rdname summary
 
setMethod( "summary",
           "prosper",
           function( object, geneticSumCol="SB_autumn_end", ...){               
            cat("The complete PROSPER simulation model Call:\n")
            print(object@Call)                  
            cat("\n")
            simdata <- intern_SimData(object)
            repesim <- c("repetition", "simcycle")
#--   the population summary
 colclass <- sapply(simdata,class)[-c(1:4)]
                                  
                                  
 for( col in which(colclass=="character")) set(simdata, j=col, value=as.factor(simdata[[col]]))
  if( TRUE) {
     namelist_num <- which(colclass=="numeric" | colclass=="integer")        #the numeric variables
     namelist_fac <- which(colclass=="factor")                               #the factorial variables
     namelist_other <- !which(colclass=="factor" |                           #all other variables
     colclass=="numeric" | colclass=="integer")
   }



#-----------   the numeric summary
 #summing the genotypes for each rep and simcy
 #select all result cols. 4 column are repetition, simcycle, genotype and resistance value
 sumgt <- simdata[,lapply(.SD,sum), by=repesim,.SDcols=4+namelist_num]   
 #calculating the mean for each simcy over all repetitions.
 res1 <- sumgt[,lapply(.SD,mean), by="simcycle", .SDcols=3:length(sumgt)]     
 names(res1) <- paste0(names(res1),"_mean")
 #calculating the standard diviation for each simcy over all repetitions.
 res2 <- sumgt[,lapply(.SD,sd), by="simcycle", .SDcols=3:length(sumgt)]   
 names(res2) <- paste0(names(res2),"_sd")
 #combining and ordering the columns
 positions <- c(c(1:length(names(res1)))+c(0:(length(names(res1))-1)),
                c(1:length(names(res2)))+c(1:(length(names(res2)))))
                                  
 res_num <- cbind(res1,res2)
 setcolorder(res_num, neworder=order(positions))
 res_num[,1:=NULL]
 names(res_num)[1] <- names(sumgt)[2]

#-------------   the factorial summary
#shows the factorlevels                         
 res_fc <- simdata[, list(Variable=names(.SD), flevels=lapply(.SD,levels)), .SDcols=names(simdata)[4+namelist_fac]]
 res_oth <- paste("No summary for other types of variables yet included")
results_population <- list("numerics"=res_num, "factors"=res_fc, "other variably types"=res_oth, "counts"=sumgt)                                   

#-------------   the genetic summary
if(simdata$genotype[1]!="all"){

#if(levels(as.factor(simdata$genotype))!="all"){
      
colGenRes <- which(names(simdata)==geneticSumCol)
digitsum <- function(x) ifelse(is.na(x),NA,sum(floor(x / 10^(0:(nchar(x) - 1))) %% 10))
allel_sum <- unlist(lapply(as.numeric(as.character(simdata$genotype)),digitsum))
max_alleles <- max(allel_sum)
                                   
sumIndividuals <- simdata[,base::sum(.SD),by=repesim, .SDcols=colGenRes, with=TRUE]
sumALLalleles <- simdata[,base::sum(.SD)*max_alleles,by=repesim, .SDcols=colGenRes, with=TRUE]
                                   
allSalleles <- simdata[allel_sum==0,.SD,by=repesim, .SDcols=colGenRes, with=TRUE]
                              
allRalleles <- simdata[allel_sum==max_alleles,.SD,by=repesim,
                       .SDcols=colGenRes, with=TRUE]
sumRSalleles <- simdata[c(allel_sum!=0 & allel_sum!=max_alleles),base::sum(.SD),by=repesim, .SDcols=colGenRes, with=TRUE]
                                                 
sumRalleles <- simdata[,base::sum(.SD*allel_sum),by=repesim, .SDcols=colGenRes, with=TRUE]
                                   
#- creation of the result object ---                                   
genetic <- sumRalleles[,c(1,2)]
genetic[,c("prop_allSalleles"):=c(allSalleles[,3]/sumIndividuals$V1)]
genetic[,c("prop_allRalleles"):=c(allRalleles[,3]/sumIndividuals$V1)]
genetic[,c("prop_RSalleles"):=c(sumRSalleles[,3]/sumIndividuals$V1)]
genetic[,c("prop_Ralleles"):=c(sumRalleles[,3]/sumALLalleles$V1)]
                                   
gen_mean <- genetic[,lapply(.SD,mean),by="simcycle",.SDcols=3:6]
names(gen_mean)[-1] <- paste("m_",names(gen_mean)[-1],sep="")
                                   
                                   
gen_sd <- genetic[,lapply(.SD,sd),by="simcycle",.SDcols=3:6]
names(gen_sd)[-1] <- paste("sd_",names(gen_sd)[-1],sep="")
                                  
positions <- c(c(1:length(names(gen_mean)))+c(0:(length(names(gen_mean))-1)),
               c(1:length(names(gen_sd)))+c(1:(length(names(gen_sd)))))
res_gen <- cbind(gen_mean,gen_sd)
setcolorder(res_gen, neworder=order(positions))
res_gen[,1:=NULL]
return(list("population"=results_population,"genetics"=res_gen)) 
}
else {
   return(list("population"=results_population))
}
                                
                                                                     
                              
}#END function          
)#END Methode



      
         
         
