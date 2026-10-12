#' @title generating the start values for PROSPER models
#' @seealso \code{\link{sel_resist}} \code{\link{struc_preparation2}}
#' @description \code{gen_freq} generates the numbers of genotypes at the beginning of the first simulated year.
#' @export
#' @param result names of the results columns. \code{character}.
#' @template af
#' @param n_seeds initial number of weed seeds. They will be allocated to the genotypes. \code{integer}. 
#' @param distribution defines the proportion with \code{n_seeds} is distributed among multiple cloumns, if result defines multiple columns. Has to be of the same length as \code{result}. \code{numeric} between 0 and 1 or "equal". See details.
#' @template max_vec_length


#' @details The start values for a model include the initial frequencies of alleles and the initial seedbank, i.e. the number of seeds in the soil. \code{gen_freq} allocates the \code{n_seeds} individuals to the different genotypes as provided by \code{dfgenotype}, which is created by \code{struc_preparation2()}. If \code{af == NULL} the function gives all seed to the only "genotype". The distribution can be splitted to multiple cohorts using \code{distribution}. 
#' If \code{distribution} is "equal", n_seeds are distributed equaly amoung the cohorts defindes with \code{result}.

#' @return Returns a \code{data.frame} containing the genotypes and their frequencies provided by \code{dfgenotype}.

#' @examples 
#' # generate a 'dfgenotype' data.frame:
#' struc_preparation2(Rmx=10, af=c(0.01,0.8), epis=0, dom=1)
#' #Distribute 10000 individuals of the starting population across the genotypes. 
#' #The two gene loci have initial frequencies of 0.01 and 0.8.
#' gen_freq(af=c(0.01,0.8), n_seeds=10000)
#' rm(dfgenotype, mf, xprobab)


gen_freq <- 
function(af, n_seeds, result="initialSB", distribution = NA,  max_vec_length=1e+07){
cat("gen_freq starts...")
#--- checks if it is necessary/genetics are included
dfgenotype <- get0("dfgenotype", envir = parent.frame(n = 1))

if(length(result)>1){ 
  if(distribution[1] == "equal") distribution <- rep(1/length(result), length(result))
  if(length(distribution) != length(result)) stop("Result and distribution has to be of the same length.")
  if(any(distribution > 1 | distribution < 0)) stop("Distribution must be between 1 and 0.")
  if(sum(distribution) > 1) stop("Distribution must not sum up to more the 1.")
  
  n_seeds <- round(distribution*n_seeds, digits=0) }

if(is.null(af)){
  
  #beak_col_sum <- sum(dfgenotype[which(names(dfgenotype) %in% break_col_names)]) 
               for(i in seq_along(result)){
                 dfgenotype[[result[i]]] <- n_seeds[i]
               }
               #dfgenotype[[result]] <- n_seeds
               assign("dfgenotype", value=dfgenotype, pos = -1, envir=parent.frame(n = 1))
               cat("finished!\n")
               return(dfgenotype) 
               }

#--- each GT has a certain probability to occure in the starting population
     #prob^amount_TRUE     *(1-prob)^(amount_of_FALSE)     * fff
                   #the first allel                                        #l(x)= 0 -> fff=1
#a probability tree  of 2 steps                                            #l(x)= 2 -> fff=1
#l(x) is always 0,1 or 2                                                   #l(x)= 1 -> fff=2
gt_probab <- rep(1, nrow(dfgenotype))
n_loci <- length(af)
locus_value <- data.frame(matrix(ncol=n_loci,as.numeric(unlist(strsplit(as.character(dfgenotype[[1]]), split=NULL))),byrow=TRUE), stringsAsFactors = TRUE)

for (i in seq(along=af)){   
    locus <- locus_value[,i]
    gt_probab <- gt_probab * af[i]^locus * (1-af[i])^abs(2-locus) *(-(locus-1)^2+2)
}#END for(i)

gtFreq  <- data.frame(genotype = dfgenotype$genotype, stringsAsFactors = TRUE)          #all genotypes listet and the actual frequence of it             


for(i in seq_along(result)){
gtFreq[[result[i]]] <- 0                             # for saving the result
i1 <- n_seeds[i] %/% max_vec_length                  #counter to cut vectors at pieces of 1e+07
i2 <- n_seeds[i] %% max_vec_length                   #the rest of the deviedet vector
if(i1 > 0){
  for(i in 1:i1){                                         #for every 1e+07-piece do the same as for the rest (i2, see below)
    tmp1 <- data.frame(table(sample(gtFreq$genotype, size = max_vec_length, replace = TRUE, prob = gt_probab)), stringsAsFactors = TRUE) #
    tmp2 <- merge(gtFreq, tmp1, by.x = "genotype", by.y = "Var1", all.x = TRUE)
    if(anyNA(tmp2$Freq)){
      warning("Frequencies of the genotype in the initial seed bank could not be calculated correctly")
      tmp2[is.na(tmp2$Freq)] <- 0 }  
    gtFreq[,2] <- gtFreq[,2] + tmp2$Freq
  }
}#END if(i1)

#1. sample the initial genotype distribution with all genotypes, sample number i2, and the probabilities gt_probab
#2. sum up the existing genotypes
tmp1 <- data.frame(table(sample(gtFreq$genotype, size = i2, replace = TRUE, prob = gt_probab)))
tmp2 <- merge(gtFreq, tmp1, by.x = "genotype", by.y = "Var1", all.x = TRUE)                     #combine the numbers of all GT with the existing frequency
if(anyNA(tmp2$Freq)){
      warning("Frequencies of the genotype in the initial seed bank could not be calculated correctly")
      tmp2$Freq[is.na(tmp2)] <- 0 }                                                 #if na was created, change to 0
gtFreq[,2] <- gtFreq[,2] + tmp2$Freq

dfgenotype[[result[i]]] <- gtFreq[,2]

}#END for(i)

cat("finished!\n")
assign("dfgenotype", value=dfgenotype, pos = -1, envir=parent.frame(n = 1))           
invisible(dfgenotype)
}
