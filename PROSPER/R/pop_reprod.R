#' @title Seed production and crop yield
#' @seealso \code{\link{pop_reprod}} \code{\link{pop_step}} 
#' @export
#' @description Calculates the produced seeds and optionally the proportion of crop yield that is realized relative to weed free yield (Renton at al. 2011).

#' @template pen_co
#' @template dc
#' @template kc
#' @template kw
#' @template crop_inr
#' @template SSmax
#' @template area
# @template dfgenotype
#' @template start
#' @param yield \code{logical}, whether the percentage of yield gained should be calculated.

#' @details The number of produced seeds is calculated for 1m^2 by the formula:
#' \cr
#' \deqn{over\_all  = 1 + kc*dc[crop\_inr] + sum(kw*dw*pen\_co)} 
#' \deqn{producedseeds  = round(sum((SSmax[crop\_inr] * kw * dw * pen_co)/over\_all),digits=0)}
#' \deqn{propyield = (1 + kc*dc[crop\_inr])/over\_all}
#' \cr
#' The weed density \code{dw} is calculated for each squaremeter derived from the current simulation run (\code{start}). The used parameters values apply to wheat and ryegrass (Pannell et al. 2004, cited in Renton et al. 2011).

#' @examples 
#' struc_preparation2(Rmx=10, af=c(0.01,0.8), epis=0, dom=1)
#' #Distribute 10000 individuals of the starting population across the genotypes provided by tmp. 
#' #The two gene loci have initial frequencies of 0.01 and 0.8.
#' gen_freq(af=c(0.01,0.8), n_seeds=10000, max_vec_length=1e+07)
#' pop_reprod("initialSB",  area=100, kw=0.5, pen_co=1, kc=0.05, dc=100, 
#'                          crop_inr="wheat", SSmax=3000, yield=TRUE)
#' rm(producedseeds, dfgenotype, mf, xprobab, propyield)

#' @references 
#' Renton, M.; Diggle, A.; Manalil, S. & Powles, S. (2011): Does cutting herbicide rates threaten the sustainability of weed management in cropping systems? Journal of Theoretical Biology, 283, 14-27.

#' Pannell, D. J.; Stewart, V.; Bennett, A.; Monjardino, M.; Schmidt, C. & Powles, S. B. (2004): RIM: a bioeconomic model for integrated weed management of Lolium rigidum in Western Australia Agricultural Systems, 79, 305-325

pop_reprod <-
function(start,  area, kw, pen_co, kc, dc, crop_inr, SSmax, yield=FALSE){
cat("pop_reprod starts...")

#--- check: pen_co  must have the same length as start
dfgenotype <- get0("dfgenotype", envir = parent.frame(n = 1))
if(length(start)!=length(pen_co)){warning("pop_reprod(start, pen_co): start and pen_co must have the same length.")}

first_amount        <- data.frame(matrix(nrow=nrow(dfgenotype),ncol=length(start)))
names(first_amount) <- start
dw <- rep(0,length(start))

for(cohort in seq_along(start)){ 
       first_amount[cohort] <- eval(parse(text=paste("dfgenotype$",start[cohort],sep="")))   #the amount that gives the start
       dw[cohort] <- sum(first_amount[cohort])/area  #density of the cohort surviver
       }#END for(cohort)
 
over_all  <- 1 + kc*dc[crop_inr] + sum(kw*dw*pen_co) 

newseeds  <- round(sum((SSmax[crop_inr] * kw * dw * pen_co)/over_all)*area,digits=0)
if(yield==TRUE){
    propyield <- (1 + kc*dc[crop_inr])/over_all  #propyield is a result variable
    cat("finished!\n")
    assign("propyield", value=propyield, pos = -1, envir=parent.frame(n = 1))  
    assign("producedseeds", value=newseeds, pos = -1, envir=parent.frame(n = 1))              
    invisible(list("newseeds"=newseeds, "propyield"=propyield))
    }else{
cat("finished!\n")
assign("newseeds", value=newseeds, pos = -1, envir=parent.frame(n = 1))            
invisible(newseeds)}
}
