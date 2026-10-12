#' @title Germination
#' @seealso \code{\link{pop_reprod}} \code{\link{pop_step}} \code{\link{quanti}}  
#' @description \code{pop_germc} describes germination as random event for individual seeds. The function considers different cohorts and dormancy.
#' @export
#' @param germ germination probabilities for the different cohorts. See details.
#' @template max_vec_length
#' @param init_sb column name of the initial seed bank in the data.frame dfgenotype that is delivered by struc_preparation. \code{character}. 



#' @details Each individual has a chance to germinate or to stay dormant. In case of germination, it emerges in one of the cohorts. The distribution of individual seeds to cohorts or dormancy is random. The function uses the columns \code{init_SB} of the data.frame \code{dfgenotype} as input. The output values are the numbers of seedlings of each genotype and each cohort.
#' 
#' \code{germ} must be given as a numeric vector or - in case of multiple columns in init_sb - data.frame or matrix. In that case the row number must fit to the length of \code{init_sb}. If the columns in init_sb represent cohorts, the rows of \code{germ} give the germination probabilities for these specific cohorts. The sum of one row of \code{germ} shall be \eqn{\ge 0} and \eqn{\le 1}. The difference of 1 and the sum of on row \code{germ} is the probability of dormancy. 



#' @return Columns are added to \code{dfgenotype}: "germ_dorm" contains the numbers of each genotype that remain dormant, "germ1" to "germX" contain the numbers for each of X cohorts.

#' @examples 
#' struc_preparation2(Rmx=10, af=c(0.01,0.8), epis=0, dom=1)
#' ls()
#' gen_freq( af=c(0.01,0.8), n_seeds=10000)

#' #Distribute the individuals to three cohorts with the germination 
#' #probabilities 0.2, 0.4 and 0.4.
#' pop_germc( init_sb="initialSB", germ=c(0.2,0.4,0.4))

#'rm(mf, dfgenotype, xprobab) 


pop_germc <-
function(init_sb, germ, max_vec_length=1e+07){
cat("pop_germc starts...")
if(sum(germ)>1|sum(germ)<0){warning("pop_germc(germ): germ should follow 0 <= germ <= 1.")}
dfgenotype <- get0("dfgenotype", envir = parent.frame(n = 1))


if(length(init_sb)>1){ 
  if(is.null(nrow(germ))) germ <- matrix(germ, nrow= length(germ))  
  if(nrow(germ) != length(init_sb)) {
    warning("length(init_sb) is not fitting to germ. Only first set of values is used")
    germ <- matrix(rep(germ[1,], length(init_sb)), ncol=length(germ), byrow=TRUE)
    }
}


if(length(init_sb)==1){
  if(is.null(nrow(germ))) germ <- matrix(germ, ncol=length(germ))
  if( nrow(germ)>1) {
    warning("germ  has to many values for one column named in init_sb. Only the first set is used")
    germ <- matrix(germ[1,], byrow=TRUE)
  }
} 


first_amount <- dfgenotype[which(names(dfgenotype) %in% init_sb)]


#####----- structure for all cohorts
#the start cohorts can germinate in all result cohorts. Every genotype sepately.
germ_tmp <- data.frame(matrix(nrow=nrow(dfgenotype), ncol=ncol(germ)+length(init_sb)))        #for saving the germination result
germ_tmp[is.na(germ_tmp)] <-0
names(germ_tmp)[seq_along(germ)] <- paste("germ",c(1:(ncol(germ))),sep="")
names(germ_tmp)[seq_along(init_sb)+ncol(germ)] <- paste("dorm",c(1:(length(init_sb))),sep="")

for(inco in seq_along(init_sb)){ #every start cohort

for(pres_gt in which(first_amount[,inco] > 0)){                                  #every genotype (just work with the genotypes that are present)
    i1 <- first_amount[,inco][pres_gt] %/% max_vec_length
    i2 <- first_amount[,inco][pres_gt] %% max_vec_length
    if(i1 > 0){     #for every i1 chunk the same like the i2 rest (see below)
      for(i in 1:i1){
            tmp <- 0
            for(cohort in seq_along(germ[inco,])){ # every result cohort
                       tmp0 <- rbinom(1, max_vec_length-tmp, prob = germ[inco,][cohort])
                       germ_tmp[,cohort][pres_gt] <- germ_tmp[,cohort][pres_gt] + tmp0
                       tmp <- tmp + tmp0                                       
                            }#END for(cohort)
           germ_tmp[,ncol(germ)+inco][pres_gt] <- germ_tmp[,cohort][pres_gt] + max_vec_length - tmp # the last cohort is just a difference
           
      }#END for(i)
    }#END if(i1)
  ###----- i2 part
    tmp <- 0
    for(cohort in seq_along(germ[inco,])){ # every result cohort
            tmp0 <- rbinom(1, i2-tmp, prob=germ[inco,][cohort])
            germ_tmp[,cohort][pres_gt] <- germ_tmp[,cohort][pres_gt] + tmp0
            tmp <- tmp + tmp0                                                  
            }#END for(cohort)
    germ_tmp[,ncol(germ)+inco][pres_gt] <- germ_tmp[,ncol(germ)+inco][pres_gt] + i2 - tmp # the last cohort is just a difference
}#END for(pres_gt)

}#for(inco)  
dfgenotype <- data.frame(dfgenotype,germ_tmp)

cat("finished!\n")
assign("dfgenotype", value=dfgenotype, pos = -1, envir=parent.frame(n = 1))            
return(dfgenotype)     
}
