#' @title Population dynamic model of \emph{Lolium multiflorum}
# @rdname prosper-models
#' @describeIn prosper-models Population dynamic model of \emph{Lolium multiflorum}

#' @description prosper.LOLRI performs a simulation of PROSPER using the setting presented by Renton at al. (2011). To manipulate the parameters see Details.
#' @export
#' @template param.weed 
#' @template area 
#' @template af  
#' @template dom 
#' @template epis 
#' @template put 
#' @template sdrate 
#' @template thresh  
#' @template Rmx  
#' @template dc 
#' @template kc  
#' @template kw  
#' @template SSmax  
#' @template rate 
#' @template pen_co 
#' @template duration 
#' @template repetitions 
#' @template crop_list
#' @template max_vec_length  

#' @author Christoph von Redwitz, \email{christoph.redwitz@uni-rostock.de}
#' @details 
#'\code{prosper.LOLRI()} performs a simulation of \emph{Lolium rigidum} similar to PERTH (Renton et al. 2011) when it is used with \code{param.LOLRI}.


#' @references Renton, M., Diggle, A., Manalil, S. & Powles, S. (2011): Does cutting herbicide rates threaten the sustainability of weed management in cropping systems? Journal of Theoretical Biology, Elsevier BV, 283, 14-27.

#' @examples
#' \donttest{
#' mod_lolri    <- prosper.LOLRI(param.weed=param.LOLRI, area=100, 
#'                                        duration=15, repetitions=3)}

prosper.LOLRI <-
function(
      param.weed     = PROSPER::param.LOLRI,                     #data table containing necessary information on the weed
      area           = 100,                             #area of simulation
      af             = c(0.005, 0.01, 0.015, 0.02),     #starting frequnce for 4 resistance allels for 4 genes
      dom            = 0.5,                                                        
      epis           = 0,                               #probabilitie of weeds of beeing untuched
      put            = 0.05,
      sdrate         = 0.4,
      thresh         = 20,                              #threshold herbicid level to kill fully susceptible weeds
      Rmx            = 10,                              #maximum resistance level
      dc             = 150,
      kc             = 1/11,
      kw             = 1/33,
      SSmax          = 30000,
      rate           = 100,             
      pen_co         = c(1,0.5),
      duration       = 10,
      repetitions    = 1,                               #repetitions of the simulations #hier vielleicht paralellisieren?
      crop_list      = c("wheat"),
      max_vec_length = 1e+07                            #technical term: R cant process very long vectors
)
{
#-----Simulation preparations --------------------------------------------------
#things that you do only want to do ONCE in your simulation run
cat("SIMULATION START\n\n")

#### --- Controll of the given data ----
crop.list <- NA # not ideal
mod_check(param.weed=param.weed, area=area, duration=duration, repetitions=repetitions, crop_list=crop_list, max_vec_length=max_vec_length)

#- generating necessary objects: dfgenotype, mf, xprobab -----------------------
struc_preparation2(Rmx=Rmx, af=af, epis=epis, dom=dom)                                              

cat("---------------------------------------------\n")
cat("---------------------------------------------\n")
cat("--------preparation ready------\n")
for(rep_counter in seq(repetitions)){
cat("---------------------------------------------\n")
cat("starting simulation run\t", rep_counter, "\n")
crop <- crop.list[1]                                                            #starting point in crop rotation
year <- 0

# -- the starting weed density per m^2
weed_den          <- quanti(step_name="den0", crop=crop)   #getting the density
gen_freq(result="SB_autumn", af=af, n_seeds=weed_den*area) 

#Here come the things that you want to do every year in duration
cat("---------------------------------------------\n")
cat("---------------------------------------------\n")
cat("---------------------------------------------\n")
cat("starting the timeloop\n")
repeat{
year <- year + 1
crop <- crop.list[year]
cat("time step", year, "starts\n")

### winter reduces the SB_autumn  -----------------------------------------------
# and forms the SB_spring
dieprob     <- quanti(step_name="Pbetweebdeath", crop=crop, area=area, res_max=1, res_min=0)
pop_step(start="SB_autumn", result="SB_spring",surv_prob=1-dieprob)

### germination of cohorts -----------------------------------------------------
# cohorts should be defined here
# calculates the probability of germination in the first and second cohort
germ1 <- quanti(crop=crop, area=area, step_name="germ1")  #calculates the probability of germination in the
germ2 <- quanti(crop=crop, area=area, step_name="germ2")  #calculates the probability of germination in the second cohort
pop_germc(init_sb="SB_spring", germ=c(germ1,germ2))  #defines which individual goes which way

### dieing beside herbicide ----------------------------------------------------
chsprem <- quanti(crop=crop, area=area, step_name="chsprem")  
pop_step(start="germ1", result="surv01", surv_prob=chsprem)          

### surviving of herbicide -----------------------------------------------------                        
sel_herb(start="surv01", result="survherb1", thresh=thresh, sdrate=sdrate, rate=rate, put=put)
sel_herb(start="germ2", result="survherb2", thresh=thresh, sdrate=sdrate, rate=rate, put=put)                        

### production of new seeds ----------------------------------------------------
pop_reprod(start=c("survherb1","survherb2"), area=area, kw=kw, pen_co=pen_co, 
                             kc=kc, dc=dc, crop_inr=which(crop_list==crop), SSmax=SSmax, yield=FALSE) 
                             
# -- Genotypes of new seeds  -------
gen_diploid(start=c("survherb1", "survherb2"), start_comb="weedatharvest", result="new_seeds")
                           
###--- surviving seed that did not germinate -----------------------------------
#thats another branch in the germination tree
Pindeath <- quanti(step_name="Pindeath", crop=crop, area=area, res_min = 0)
                                                           #the proportion of surviving seeds is taken as probability
pop_step(start="dorm1", result="SB_dorm_surv", surv_prob=1-Pindeath)

### SB_autumn_end -----------------------------------------------------------------
#the ending SB of one year (is the starting of the next year)
pop_step(start=c("SB_dorm_surv","new_seeds"), result="SB_autumn_end", surv_prob=1)

#-------------------------------------------------------------------------------
# Now all calculations for this year in duration is done.
# the result is saved in "sim_res", a new year starts
# "dfgenotype" is cleaned up and prepared for the next cycle
struc_saveSimData( rep_counter=rep_counter, simcycle=year, 
                               start_names="SB_autumn", end_names="SB_autumn_end",
                               #loci=n_loci,
                                simstruc=c(repetitions, duration))
                             
#-------------------------------------------------------------------------------
### some rules for ending the repetition.
simend <- struc_endSim(year,"SB_autumn")
if(simend==TRUE) {break}
}#END repeat (TIMELOOP)
}#END for(rep_counter) 
cat("\n\n\n")
invisible(get0("sim_result"))
}                                                                                                      
