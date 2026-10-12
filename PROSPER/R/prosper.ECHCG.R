#' @title Population dynamic model of \emph{Echinochloa crus-galli}
#' @describeIn prosper-models Population dynamic model of \emph{Echinochloa crus-galli}

# @template param.weed 
# @template area 
# @template af  
# @template dom 
# @template epis 
# @template put 
# @template sdrate 
# @template thresh  
# @template Rmx  
# @template rate 
# @template duration 
# @template repetitions 
# @template crop_list
# @template max_vec_length  


#' @description \code{prosper.ECHCG} provides the setting for a simulation of the population dynamic of \emph{Echinochloa crus-galli}. 

#' @export

#' @param undersowing Numerical vector with two values between 0 and 1. See details.
#' @details 
#' \code{prosper.ECHCG()} simulates originally the population dynamic of \emph{Echinochloa crus-galli} using the data \code{param.ECHCG}. Different cohorts of weed seedlings are the focus of this model. The focus of this model is the effect of weeds that escape the selection pressure of herbicide treatment. These weeds keep the unselected genetic. Can they buffer the selection process? \emph{E. crus-galli} is able to germinate over a long period after maize planting with decreasing reproductive success (Bagavathiannan, 2013). In the model all germinating individuals are represented by two cohorts; an early, major cohort with with high seed production, and a small late emerging with lower reproduction. Only the first cohort is controlled by a herbicide, which is a typical situation in Germany (Rossberg, 2016). The second cohort escapes the herbicide treatment unaffected. However, the second cohort can be suppressed, for example by an undersown crop. Three scenarios with different degrees of suppression, 0\%, 30\% and 100\%, were simulated (Redwitz, 2016).



#' The parameter \code{undersowing} describes the probability of surviving a second, not selective pressure on weed seedlings, which germinate after the selective herbicide was applied.



#' \code{prosper.ECHCG} provides the setting for a simulation of the population dynamic of \emph{Echinochloa crus-galli}. 

#' @references Redwitz, C von, Pannwitt H, Gerowitt B (2016): About the interplay of sensitive and resistant biotypes in weed populations - simulation exercises for Echinochloa crus-galli in maize crops. Proceedings - 28th German Conference on Weed Biology and Weed Control, Julius-Kuehn-Archiv, 93-99, 452.


#' @examples 
#' \dontrun{
#' mod_echcg <- prosper.ECHCG(param.weed = param.ECHCG, area=100, af=c(0.001), 
#'                              undersowing=0.2,dom=0.5,duration=7,repetitions=1)
#'
#' 
#' #The model call for Redwitz et al. (2015)
#' undersowing_prob <- c(1, 0.3, 0) #no undersowing, strong competition, complete dominance
#' years <- 20
#' reps  <- 4
#' ####------------------------
#' simu_collect <- list()
#' for(simu in 1:3){
#' simu_collect[[simu]] <- prosper.ECHCG(area          = 100,
#'                                       param.weed    = param.ECHCG,
#'                                       thresh        = 20,
#'                                       duration      = years,
#'                                       af            = 0.001,     
#'                                       dom           = 1,      
#'                                       undersowing   = undersowing_prob[simu],  
#'                                       repetitions   = reps
#'                                              )
#' }
#' }

prosper.ECHCG <-                                                                 
function(
### Parameter ------------------------------------------------------------------
      param.weed     = PROSPER::param.ECHCG,                  #data table containing necessary information on the weed
      area           = NA,                              #area of simulation
      af             = NA,                              #starting frequnce for 4 resistance allels for 4 genes
      dom            = NA,
      epis           = 0,                               #probabilitie of weeds of beeing untuched
      put            = 0.05,
      sdrate         = 0.4, 
      thresh         = 20,                              #threshold herbicid level to kill fully susceptible weeds
      Rmx            = 10,                              #maximum resistance level
      rate           = 100,
      duration       = NA,
      repetitions    = NA,                              #repetitions of the simulations
      crop_list      = "corn",
      max_vec_length = 1e+07,                          #technical term: R cant process very long vectors
      undersowing    = NA
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

cat("---------------------------------------------\n\n\n")
cat("---------------------------------------------\n\n")
cat("--------preparation ready------\n")
for(rep_counter in seq(repetitions)){
cat("---------------------------------------------\n\n\n")
cat("starting simulation run\t", rep_counter, "\n")
crop <- crop.list[1]                                             #starting point in crop rotation
year <- 0

# -- the starting weed density per m^2
weed_den          <- quanti(step_name="den0", crop=crop)          #getting the density
gen_freq(result="SB_autumn", af=af, n_seeds=weed_den*area) 

#Here come the things that you want to do every year in duration.
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
survprob     <- quanti(step_name="seed_surv_winter", crop=crop, area=area, res_max=1, res_min=0)
pop_step(start="SB_autumn", result="SB_spring",surv_prob=survprob)

### germination of cohorts -----------------------------------------------------
# cohorts should be defined here
# calculates the probability of germination in the first and second cohort
germ1 <- quanti(crop=crop, step_name="germ1", res_max=1, res_min=0)  
germ2 <- quanti(crop=crop, step_name="germ2", res_max=1, res_min=0)  
pop_germc(init_sb="SB_spring", germ=c(germ1,germ2))  #defines which individual goes which way

### surviving of herbicide -----------------------------------------------------
#here its an early herbicide. 
sel_herb(start="germ1", result="surv02", thresh=thresh, sdrate=sdrate, rate=rate, put=put)

### dieing beside herbicide ----------------------------------------------------
#weeds have an "natural" dieing rate, caused by other things than herbicide
surv_seedling <- quanti(origin="surv02",crop=crop, area=area, step_name="surv_seedling1",log_values=FALSE)
pop_step(start="surv02", result="surv02", surv_prob=surv_seedling)    

### dieing because of undersowing ----------------------------------------------
#weeds that germinate to late will have additional truble because of the undersowing
pop_step(start="germ2", result="surv12", surv_prob=undersowing)

surv_seedling <- quanti(origin="surv12", crop=crop, area=area, step_name="surv_seedling1",log_values=FALSE)
pop_step(start="surv12", result="surv12", surv_prob=surv_seedling)


### production of new seeds ----------------------------------------------------
num_first <- quanti(origin="surv02", crop=crop, equal_dis=TRUE, proportion=FALSE, area=area, step_name="seed_prod_first", log_values=FALSE, back_log=FALSE)
num_second <- quanti(origin="surv12", crop=crop, equal_dis=TRUE, proportion=FALSE, area=area, step_name="seed_prod_second", log_values=FALSE, back_log=FALSE) 

producedseeds <- round(num_first + num_second, digits=0)
producedseeds <- ifelse(length(producedseeds)==0, 0, producedseeds)

# -- Genotypes of new seeds  -------
gen_diploid(start=c("surv02", "surv12"), start_comb="weedatharvest", 
                           result="seed_shed", newseeds=producedseeds)


###--- surviving seed that did not germinate -----------------------------------
#thats another branch in the germination tree
prob_surv_summer <- quanti(step_name="seed_surv_summer", crop=crop) 
pop_step(start="dorm1", result="SB_summer", surv_prob=prob_surv_summer)

### Seed_predation -------------------------------------------------------------
seeds_pred <- quanti(step_name="seed_pred", crop=crop) 
pop_step(start="seed_shed", result="new_seeds", surv_prob=(1-seeds_pred))

### SB_autumn_end -----------------------------------------------------------------
#the ending SB of one year (is the starting of the next year)
pop_step(start=c("SB_summer", "new_seeds"), result="SB_autumn_end", surv_prob=1)

#-------------------------------------------------------------------------------
# Now all calculations for this year in duration is done.
# the result is saved in "sim_res", a new year starts
# "dfgenotype" is cleaned up and prepared for the next cycle
struc_saveSimData(rep_counter=rep_counter, simcycle=year,
                               start_names="SB_autumn", end_names="SB_autumn_end",
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
