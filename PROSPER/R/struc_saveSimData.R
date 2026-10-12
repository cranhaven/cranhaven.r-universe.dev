#' @title Saving simulation data
#' @seealso  \code{\link{struc_endSim}}
#' @description \code{struc_saveSimData} collects the data of one simulation cycle and returns an object to start a new simulation cycle. 

#' @export

#  @template dfgenotype
#' @param rep_counter number of the current repetition. \code{integer}.
#' @param simcycle number of the current simulation cycle. \code{integer}.
#' @param start_names names of the columns with the first population stage in the simulation. \code{character}.
#' @param end_names names of the columns with the last population stage in the simulation. These are taken as the first stage in the next simulation cycle. \code{character}.
# @param namestokeep \code{character vector}, the column names of \code{dfgenotype} that are necessary for all simulation cycles and not calculated within each cycle.

#' @param simstruc  two numbers, the first defines the number of repetitions for the simulation, the second defines the number of simulation cycles in each repetition of the simulation. \code{numeric vector} with length 2.

#' @details In the first simulation cycle the object \code{sim_result} of the class \code{prosper} is created and the first cycle results are saved. This object is the same for all cycles in all repetitions and gets all the results. The function can only be called once per simulation run.

#' @return A \code{prosper} object. SimData as a \code{data.table} with the repetition in the first and the simulation cycle in the second column.
#' @import data.table
#' @importFrom methods is new validObject

#' @examples
#' struc_preparation2(Rmx=10, af=c(2,1), epis=0, dom=c(1,0.3))
#' dfgenotype$"SB_autumn" <- c(4,23,0,123,53,98,45,3245,234)
#' dfgenotype$"SB_autumn_end" <- c(0,2,0,123,434,5234,5678,123,2)
#' #creation of an example object with data of the first year

#' struc_saveSimData(rep_counter=1, simcycle=1, simstruc=c(5, 10), 
#'                                  start_names="SB_autumn", end_names="SB_autumn_end")
#' #creating some fictional population stages
#' dfgenotype$"SB_autumn" <- c(1,1,1,1,0,0,0,0,4)
#' dfgenotype$"SB_autumn_end" <- c(67,67,67,67,67,67,67,67,67)

#' #appending rows with the new results to the first results. necessary columns are inserted.
#' struc_saveSimData( rep_counter=1, simcycle=2,
#'                      start_names="SB_autumn", end_names="SB_autumn_end",
#'                       simstruc=c(repetitions, duration)) 
#' sim_result                               
#' rm(sim_result, dfgenotype, mf, xprobab)                               
                               
struc_saveSimData <-
function(rep_counter, simcycle, start_names, end_names, simstruc){

cat("struc_saveSimData starts...")
dfgenotype <- get0("dfgenotype", envir = parent.frame(n = 1))
loci <- get0("n_loci", envir = parent.frame(n = 1))
namestokeep =c("genotype", "resist") 


if(rep_counter==1 & simcycle==1){
    simdata <- as.data.table(dfgenotype)
    simdata[,"repetition":=as.numeric(rep_counter)]
    simdata[,"simcycle":=as.numeric(simcycle)]
    setcolorder(simdata, c(c("repetition", "simcycle"),colnames(simdata)[!(colnames(simdata) %in% c("simulation_run", "repetition", "simcycle"))])) 
    cat(" step 1a...") 
        
    simdatob <- prosper(simstruc=simstruc,loci=loci,simData=simdata)
    cat(" step 2a...") 
    assign("sim_result", value=simdatob, pos = -1, envir=parent.frame(n = 1))
}else{
    cat(" step 1...") 
    simdata <- as.data.table(dfgenotype)
    simdata[,"repetition":=as.numeric(rep_counter)]
    simdata[,"simcycle":=as.numeric(simcycle)]

    setcolorder(simdata, c(c("repetition", "simcycle"),colnames(simdata)[!(colnames(simdata) %in% c("repetition", "simcycle"))]))
    simdatob <- get0("sim_result", envir = parent.frame(n = 1))   
    cat(" step 2...") 
    simdataob_add <- struc_addSimData(object=simdatob, value= simdata, add=TRUE)              
    cat(" step 3...") 
    assign("sim_result", value=simdataob_add, pos = -1, envir=parent.frame(n = 1))
    }#END else(if(simdata))

if(length(start_names) != length(end_names)) stop("start_names and end_names in struc_saveSimData must have the same length")

nextsim <- dfgenotype[c(namestokeep, end_names)]
names(nextsim) <- c(namestokeep, start_names)

assign("dfgenotype", value=nextsim, pos = -1, envir=parent.frame(n = 1))
cat(" step 4...")             


cat("finished!\n")
invisible(simdatob)
}
