#' @title Surviving the Herbicide
#' @seealso \code{\link{sel_herb}} \code{\link{sel_resist}}
#' @description Utility function internally used. It's used only by \code{sel_herb}, and usually there is no reason to change it. Calculates the surviving number of weeds according to the specific genotype.
#' @export

#' @param n_samples integer, the number of weeds with one specific genotype.
#' @param resist  numeric, value of resistance for the genotype (defined by Renton et al. 2011). Shall be \eqn{\le 1}.
#' @template rate
#' @template sdrate
#' @template put
#' @template thresh

#' @details \code{intern_herbicide} is used in \code{\link{sel_herb}}. Firstly, it calculates the number of weeds that are untouched by the herbicide by chance (probability=\code{put}). In the second step, the herbicide rate that reached an individual is compared with the resistance value (calculated after Renton et al. 2011, \code{\link{sel_resist}}). If the resistance value is lower than the dose, the weed dies. All surviving weeds are summed up.

#' @return The number of weeds surviving the herbicide.

#' @references Renton, M.; Diggle, A.; Manalil, S. & Powles, S. (2011): Does cutting herbicide rates threaten the sustainability of weed management in cropping systems? Journal of Theoretical Biology, 283, 14-27.

#' @examples #How many of 1000 weeds of a genotype with resistance value 5.5 survive a herbicide application 
#' #with full dose? 'It is assumed that weeds reseaving less than 20 \% of the full dose survive
#' #independently of their resistant value.
#' intern_herbicide(resist=5.5, n_samples=1000, put=0.04, rate=100, sdrate=0.4, thresh=20)



intern_herbicide <-
function(resist, n_samples, put, rate, sdrate,thresh){

  surv_ut <- rbinom(1, n_samples, prob = put)            #survived because the HC did not touch them (untouched)
  #surv_col <- surv + surv_ut + sum(rnorm((n_samples-surv_ut), mean = rate, sd = sdrate*rate) < rep((resist * thresh), (n_samples-surv_ut)))
  surv_col <- surv_ut + sum(rnorm((n_samples-surv_ut), mean = rate, sd = sdrate*rate) < rep((resist * thresh), (n_samples-surv_ut)))
  
  
  return(surv_col)
}
