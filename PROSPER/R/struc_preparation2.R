#' @title Generating data input and output structures 
#' @seealso \code{\link{sel_resist}} \code{\link{gen_freq}}
#' @description \code{struc_preparation} creates the data input and output structures (data.frames and table) for the simulation run, 'dfgenotype', 'xprobab' and 'mf'. 

#' @export
#' @template af
#' @template epis
#' @template dom
#' @template Rmx

#' @details Prior to the simulation, a data.frame is generated to save results (\code{dfgenotype}). Additionally, a table with recombination probabilities (\code{xprobab}) is calculated. During the simulation run, probability values are not computed again but looked up in the table. PROSPER assumes diploid plants and maximum four resistance genes. To calculate the phenotypic resistance value for each genotype \code{\link{sel_resist}} is called. If \code{n_loci < 0} the structure is set up for no genetics at all in the simulation.
#' 


#' @section Warning:
#' The run of \code{struc_preparation} is time consuming. Duration strongly increases with the number of genes under consideration, \code{n_loci}.
#' @return Returns a \code{list} of two \code{data.frame} and a \code{table}:
#' \enumerate{
#'   \item mf: all possible combinations of parental genotypes (see 'dfgenotype$genotype') are saved in one column 'mf' (male, female). The column 'mf' is a character vector. Each string of the vector has twice the length of the number of resistance loci under consideration.
#'   \item dfgenotype: the structure to save the results of one simulation cycle (year). After each cycle the data is reset to the new start values. The first column 'genotype' is a character vector. Each string of the vector has the length of the number of resistance loci under consideration. Each locus can have 0, 1 or 2 resistance alleles. The second column 'resist' saves resistance values that are calculated according to the equation in the section 'details'.
#'   \item xprobab: the probabilities of occurrence for all possible genotypes in the offspring (F-generation) with all possible parent genotypes (P-generation). Free recombination is assumed. Column names are the combinations of parental genes, row names are genotypes of the offspring.

#' }
#'
#' @examples 
#' #generate the genotype and probability tables for a simulation with two resistance 
#' #loci with one dominant and one partial dominant resistant allele, no epistasis, and a 
#' #maximumx resistance value of 10. 
#' ls()
#' struc_preparation2(Rmx=10, af=c(0.01,0.5), epis=0, dom=c(1,0.3))
#' ls()
#' rm(dfgenotype, mf, xprobab)




#struc_preparation <- function(Rmx, n_loci, epis, dom){                              



struc_preparation2 <- function(Rmx=NA, af=NA, epis=NA, dom=NA){                              
cat("struc_preparation starts...")

n_loci <- NA #not ideal
tmp_param <- c(Rmx, af, epis, dom)
if(anyNA(tmp_param) & !all(is.na(tmp_param))) {stop("struc_preparation2: you need to define all genetic parameters if you want to use them.")}
if(!anyNA(tmp_param)) {
                      gen_check(Rmx=Rmx ,af=af, epis=epis, dom=dom)
                      assign("n_loci", n_loci, pos = -1, envir=parent.frame(n = 1))                
                      assign("dom", dom, pos = -1, envir=parent.frame(n = 1))
                      assign("af", af, pos = -1, envir=parent.frame(n = 1))
}else{
      n_loci <- 0
      af <- NULL
      assign("n_loci", n_loci, pos = -1, envir=parent.frame(n = 1))
      assign("af", af, pos = -1, envir=parent.frame(n = 1))
}
                      



if(n_loci<1){ print("No resistance in the population")
              dfgenotype <- data.frame(genotype="all", stringsAsFactors = TRUE)
              dfgenotype$resist <- 0
              xprobab <- NULL
              mf <- NULL
              cat("finished!\n")
              assign("dfgenotype", value=dfgenotype, pos = -1, envir=parent.frame(n = 1))        ##parent.frame(n = 1)??
              assign("xprobab", value=xprobab, pos = -1, envir=parent.frame(n = 1))
              assign("mf", value=mf, pos = -1, envir=parent.frame(n = 1))
              return(list("dfgenotype"=dfgenotype, "xprobab"=xprobab, "mf"=mf))
}else{

#------------------------------------------------
#-- probabilities for recombination --------------------------------------------
#------- probs for 1 locus ---------------------------------------
probabonelocus <- c(1.00, 0.50, 0.00,           #probabilities for the child for one locus
                    0.50, 0.25, 0.00,           
                    0.00, 0.00, 0.00,
                    0.00, 0.50, 1.00,
                    0.50, 0.50, 0.50,
                    1.00, 0.50, 0.00,
                    0.00, 0.00, 0.00,
                    0.00, 0.25, 0.50,
                    0.00, 0.50, 1.00)
#-------------------------------------------------------------------------------
#-- possible combinations are created -----------------------------------   
i       <- c(0,1,2)                                                             #number for one allel: none, hetero- and homozygot

onelocus1<- expand.grid(m = i, f = i, ch = i)                                   #all combinations for the orgigin of one locus: maternal and paternal "allel situation" and child

onelocus <- data.frame(tripel = paste(onelocus1$m, onelocus1$f, onelocus1$ch, sep = ""), probab = probabonelocus, stringsAsFactors = TRUE)       
            #triple: which combination, probab: probability for that combination

tmp_probab1 <- list()
for (i in seq(len=n_loci)){tmp_probab1[[i]] <- onelocus$tripel}         #tripple_tmp combinations for n genes.
probab1 <- expand.grid(tmp_probab1, KEEP.OUT.ATTRS = FALSE)
tmp_probab2 <- list()
for (i in seq(len=n_loci)){tmp_probab2[[i]] <- onelocus$probab}         #tripple_tmp combinations for n genes.
probab2 <- expand.grid(tmp_probab2, KEEP.OUT.ATTRS = FALSE)


mf_tmp <- c()
for (i in seq(len=n_loci)){mf_tmp <- paste(mf_tmp,substring(probab1[[i]],1,1), sep="")}
for (i in seq(len=n_loci)){mf_tmp <- paste(mf_tmp,substring(probab1[[i]],2,2), sep="")}

genotype_tmp <- c()
for (i in seq(len=n_loci)){genotype_tmp <- paste(genotype_tmp,substring(probab1[[i]],3,3), sep="")}


probab_tmp <-  apply(probab2,1,prod)                      # this doesnt work with n_loci>=5

probab3 <- data.frame(mf= mf_tmp, genotype = genotype_tmp, probab = probab_tmp, stringsAsFactors = TRUE)

probab  <- probab3[probab3$probab != 0,]                  # take just the possible combinations for further calculations
xprobab <- xtabs(probab ~ genotype + mf, data = probab)   # new table (cross table): the probability for a genotype of the actual individual with given parents

mf      <- data.frame(mf =levels(probab3$mf), stringsAsFactors = TRUE)             # list of all genetic situations of the parents
                 
### ----------------------------------------------------------------------------
#-- list of possible GT -------------------------------------------------------
dfgenotype <- data.frame(genotype = as.character(levels(probab3$genotype)), stringsAsFactors = TRUE)          #list of all genotypes for one indicidual

#-- resistances of the GT ------------------------------------------------------
dfgenotype$resist <- sel_resist( Rmx=Rmx, epis=epis, dom=dom)    #n_loci=n_loci,      #what resistance for what genotype
dfgenotype$genotype <- as.character(dfgenotype$genotype)  
                  

assign("dfgenotype", value=dfgenotype, pos = -1, envir=parent.frame(n = 1))
assign("xprobab", value=xprobab, pos = -1, envir=parent.frame(n = 1))
assign("mf", value=mf, pos = -1, envir=parent.frame(n = 1)) 
cat("finished!\n")


invisible(list("mf"=mf, "dfgenotype"=dfgenotype, "xprobab"=xprobab))
}#END if else(n_loci<1)
}
