#' @title Population dynamic model of \emph{Galium aparine}

#' @describeIn prosper-models Population dynamic model of \emph{Galium aparine}

#' @description \code{prosper.GALAP} provides the setting for a simulation of the population dynamic of \emph{Galium aparine}. No selection process is used.


#' @export
#' @param sens_seeds sensitive seeds added every year.

#' @details 
#' \code{prosper.GALAP()} simulates originally the population dynamic of \emph{Galium aparine} using the data \code{param.GALAP}. Whether sowing of susceptible weed seeds can restore an 'acceptable' resistance level of a population in the early stages of resistance development, is an extraordinary research question. The patchy occurrence of \emph{Galium aparine} and its large seeds result in highly variable population dynamic parameters. Modeling has to take into account this variability. We used a simple population dynamics model structure (Redwitz et al., 2015). A seedbank in spring provides seeds out of which one cohort is germinating. The weeds are selected by herbicides, produce seeds, which are affected by seed predation and return to the seedbank in autumn. Data of a long term field experiment were used for parametrization (Daedlow, 2015).

#' @references Redwitz, C von, Daedlow D, Gerowitt B (2015): Simulation exercises on long-term management of widespread herbicide resistance in a field weed population. Proceedings 17th Symposium of the European Weed Research Society, Montpellier, France, 108.


#' @examples
#' \donttest{
#' mod_galap <- prosper.GALAP(param.weed=param.GALAP, repetitions=2, duration=10) }

prosper.GALAP <-
  function(                                                                                         
    ### Parameter ------------------------------------------------------------------
    param.weed    = PROSPER::param.GALAP,                  #data table containing necessary information on the weed
    sens_seeds    = 400,                             #500 seeds added every year
    area          = 100,                             #area of simulation
    af            = c(0.03, 0.08, 0.02),             #starting frequnce for 4 resistance allels for 4 genes
    dom           = c(0.5, 0.5, 0.5),
    epis          = 0,                               
    put           = 0.05,                            #probabilitie of weeds of beeing untuched
    thresh        = 20,                              #threshold herbicid level to kill fully susceptible weeds
    Rmx           = 10,                              #maximum resistance level
    rate          = 100,
    sdrate        = 0.4,
    duration      = 15,
    repetitions   = 1,                              #repetitions of the simulations #hier vielleicht paralellisieren?
    crop_list     = "wheat", 
    max_vec_length = 1e+07                         #technical term: R cant process very long vectors
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
      crop <- crop.list[1]                                                         #starting point in crop rotation
      year <- 0
      
      # -- the starting weed density per m^2
      weed_den          <- quanti(step_name="den0", crop=crop)          #getting the density
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
        survprob     <- quanti(origin= "SB_autumn", step_name="germ_intercrop", area=area, crop=crop, res_max=1, log_values=TRUE)
        pop_step( start="SB_autumn", result="SB_spring",surv_prob=survprob)
        
        ### germination of cohorts -----------------------------------------------------
        # cohorts should be defined here
        germ1 <- quanti(origin="SB_spring", crop=crop, step_name="germincrop", area=area, log_values=TRUE, back_log=TRUE)
        pop_germc(init_sb="SB_spring", germ=c(germ1))  #defines which individual goes which way
        
        
        ### surviving of herbicide -----------------------------------------------------
        #Montpellier adding: the resistance in population is thined out with sensitiv seeds
        dfgenotype$sens <- dfgenotype$germ1
        dfgenotype$sens[1] <- dfgenotype$sens[1] + sens_seeds * area
        
        
        ### surviving of herbicide -----------------------------------------------------
        sel_herb( start="sens", result="surv01", thresh=thresh, sdrate=sdrate, rate=rate, put=put)
        
        ### dieing beside herbicide ----------------------------------------------------
        #weeds have an "natural" dieing rate, caused by other things than herbicide
        surv_seedling <- quanti(origin="surv01",crop=crop, area=area, equal_dis=TRUE, step_name="adults", log_values=TRUE) 
        pop_step( start="surv01", result="surv02", surv_prob=surv_seedling)
        
        ### production of new seeds ----------------------------------------------------
        producedseeds <- quanti(origin="surv02",crop=crop, proportion=FALSE, area=area, equal_dis=TRUE, step_name="seedset", log_values=TRUE, back_log=FALSE) 
        
        # -- exported seeds -----
        exportedseeds <- quanti(origin=producedseeds,crop=crop, proportion=FALSE, area=area, equal_dis=T, step_name="seedexport", res_min=0, res_max=producedseeds, log_values=TRUE, back_log=FALSE)
        
        # -- Genotypes of new seeds  -------
        gen_diploid(start="surv02",result="seed_shed", newseeds=producedseeds-exportedseeds)
        
        ###--- surviving seed that did not germinate -----------------------------------
        #thats another branch in the germination tree
        prob_surv_summer <- quanti(origin= "dorm1", step_name="Pindeath", crop=crop, area=area, res_max=1, res_min=0)
        pop_step(start="dorm1", result="SB_summer", surv_prob=prob_surv_summer)
        
        ### Seed_predation -------------------------------------------------------------
        seeds_pred <- quanti(origin= "seed_shed", area= area, step_name="seedpred", crop=crop, res_max=0.92, log_values=FALSE)
        pop_step(start="seed_shed", result="seed_surv_pred", surv_prob=(1-seeds_pred))
        
        ### SB_autumnn2 ----------------------------------------------------------------
        #the ending SB of one year (is the starting of the next year)
        germab <- quanti(step_name="germab", crop=crop, res_max=1, log_values=FALSE)
        pop_step(start=c("seed_surv_pred"), result="new_seeds_germab", surv_prob=germab)
        pop_step(start=c("SB_summer", "new_seeds_germab"), result="SB_autumn_end", surv_prob=1)
        
        #-------------------------------------------------------------------------------
        # Now all calculations for this year in duration is done.
        # the result is saved in "sim_res", a new year starts
        # "dfgenotype" is cleaned up and prepared for the next cycle
        struc_saveSimData( rep_counter=rep_counter, simcycle=year,                                                        
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
