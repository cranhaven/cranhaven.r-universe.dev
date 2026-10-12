#' @title Step by step: the model develops driven by field data
#' @seealso \code{\link{pop_step}} \code{\link{pop_germc}} \code{\link{weed-parameters}}
#' @description This is a working horse of PROSPER. It quantifies the number or the proportion of individuals entering the next development stage using predefined formulas (\code{formul}). Typically these formulas are the results of experiments.

#' @export
#' @param origin       numbers or log-numbers of individuals at the start for every genotype. \code{numeric}. Alternatively the columnaes of dfgenotype can be used.
#' @param step_name     step name in the database containing the model parameters. \code{character}.
# @param param.weed   data with population dynamic model parameters. \code{data.frame}.
#' @template crop     
#' @param proportion   \code{logical}, \code{TRUE} when the output should be a proportion, otherwise it will be an absolute number.
#' @param res_max      output maximum, ignored when \code{origin=NA}. \code{numeric}.
#' @param res_min      output minimum. \code{numeric}. 
#' @param equal_dis    \code{logical}, \code{TRUE} when the distribution of the weeds is spatially uniform across the \code{area}.
#' @param formul       \code{character}. See details.
#' @template area
#' @param addit_variables variables used in \code{formul} that are not predefined in \code{param.weed}. See details. \code{character vector}
#' @param log_values   logical, \code{TRUE} when \code{origin} in log-scale.
#' @param back_log     logical, \code{TRUE} when the output is in log-scale. Only used when \code{proportion==FALSE}. 



#' @importFrom stats median rbinom rnorm runif sd xtabs


#' @details Within PROSPER simulation models are build up with discrete simulation steps. These steps are conducted by functions like \code{pop_step} or \code{gen_reprod}. These functions affect the complete population and need a count of individuals or a proportion of the population that are affected of the specific simulation step. These numbers are calculated with \code{quanti}. The calculation is based on the data provided in \code{\link{weed-parameters}}. There a model is given for every parameter in the table. The parameter \code{formul} allows to use a different model if necessary. If no model is given at all, the simulation step is assumed to consist only of one value. The parameters of \code{param.weed} are normal distributed and the SE is used to draw them for the current calculation using \code{rnorm()}. These values are used to evaluate the model for simulation step. The resulting count or proportion can be used to perform the simulation step for the population.


#' @return Either a (log-)number of individuals or a proportion resp. a rate.

#' @examples 
#' #loads the example data for Echinochloa crus-galli
#' data(param.ECHCG)
#' param.weed <- param.ECHCG

#' #how many seeds (natural, not log-scale) prodused by 100 plants in a corn stand on 100 area units?
#' quanti(origin=100, step_name="seed_prod_first", crop="corn",  proportion=FALSE,
#'                  area=100, log_values=FALSE, back_log=FALSE)
#' rm(param.ECHCG)


quanti <-
function(origin=NA, step_name, crop, proportion=TRUE, equal_dis=TRUE, res_max=NA, res_min=0, formul= NA, area, addit_variables=NA, log_values=TRUE, back_log=TRUE){

param.weed <- get0("param.weed", envir = parent.frame(n = 1)) 
dfgenotype <- get0("dfgenotype", envir = parent.frame(n = 1)) 
      
      #----- checks for origin                
      if(!is.na(origin) & !(is.numeric(origin) | is.character(origin))) stop("quanti(): origin must be numeric or character.")
      if(!is.na(origin)){
      if(is.character(origin)) {
            if(length(origin)>1) {
            origin <- origin[1]
            warning("quanti(): origin has length > 1. Only the first element is used.")}
            
            if(origin %in% names(dfgenotype)) {
             origin <- sum(dfgenotype[[origin]])
             }else{stop("quanti() the name given as origin is not a column name of dfgenotype")}
      }#END if(is.character)
      if(log_values==TRUE) origin <- log(origin+1)
      }#END if(!is.na)
      
      #----- checks for param.weed   
      nam <- c("weed","crop","step","name","estimate","std.error","model", "range_low", "range_up", "source")
      if(any(names(param.weed) != nam)){
         errnam <- paste("quanti(): Column names of param.weed are not correct. They must be named: ", paste(nam, collapse=" "), sep="")
         stop(errnam)}

      #----- check for restrictions
      res_max <- ifelse(proportion==TRUE & is.na(res_max), 1, res_max)
      
      restriction_yes_no <- !is.na(res_max)|!is.na(res_min)
      crop_pos <- which(names(param.weed)=="crop")
      variable_pos <- which(names(param.weed)=="step")
      if(!any(param.weed[variable_pos]==step_name)) stop("The step_name passed to quanti() is not part of param.weed.")
      if(!any(param.weed[crop_pos]==crop)) stop("The crop passed to quanti() is not part of param.weed.")      
      parameters_all <- param.weed[(param.weed[crop_pos]==crop & param.weed[variable_pos]==step_name),]
      
###-----------------------------------------------------------------------------
# other model dependent variables such as "year" can be passed by "addit_variables".
      if(is.character(addit_variables)){
               varslist <- mget(addit_variables,envir=parent.frame(n = 1))       #mget() returns a list, the envir is the parent.frame
               varsnames <- names(varslist)
               eval(parse(text=paste(varsnames, "<-", varslist,";")))            #this assigns all the "addit_variables"
      }#END if(addit_variables)

###-----------------------------------------------------------------------------
###--- assumptions, shortcuts...
      if(is.na(origin)){               #if no origin is used, the estimate as returned as fixed value.
                        if(log_values==TRUE){
                                             res <- ifelse(back_log==TRUE, parameters_all$estimate, (exp(parameters_all$estimate)-1))
                                             }
                        if(log_values==FALSE){
                                             res <- ifelse(back_log==TRUE, log(parameters_all$estimate+1), parameters_all$estimate)
                                             }
                        return(ifelse(!is.na(step_name), res, "no parameters"))
                        }
      if(origin==0){return(0)}                   #since there is no death/production/growth without any starting level, 0 will return

###-----------------------------------------------------------------------------
###--- density dependent value calculation

#the variables are the same in all m^2
#the formula should be calculated for each m^2.
    if(equal_dis==TRUE){  
      if(log_values==TRUE){
                        x_all <- rep(log(((exp(origin)-1)/area) +1), area)         #density in the model-base unit
                        }
      if(log_values==FALSE){
                        x_all <- rep(origin/area, area)                          #density in the model-base unit
                        }
      }
    if(equal_dis==FALSE){  
      if(log_values==TRUE){
                        x_all <- log((as.numeric(table(round(runif(               #density in the model-base unit
                            exp(origin)-1,min=1, max=area), digits=0))))+1)       #runif is used to give every weed the a No of area
                        }
      if(log_values==FALSE){
                        x_all <- as.numeric(table(round(runif(origin,min=1, max=area), digits=0)))      #density in the model-base unit
                        }
      }#END if equal_dis

#---------------------------
###--- if proportion is necessary

if(proportion==TRUE){
###--- a proportion per m^2 is calculated and the overall mean returned
res <- rep(0,area)
for(area_ in seq_len(area)){
### -- the weed density on one specific unit area resp. m^2
      x <- x_all[area_] 

### -- range condition: formul selection ---------------------------------------
      if(!anyNA(parameters_all$range_low) | !anyNA(parameters_all$range_up)){
                      parameters <- parameters_all[x>=parameters_all$range_low & x < parameters_all$range_up,]     
                      }else{
                      parameters <- parameters_all
                      }#END if(!anyNA)
if(is.na(formul)){#if no formul is named, the model from param.weed is taken. if there is none, the "mean"-value from param.weed is returned
            formul<- ifelse(!is.na(parameters$model[1]) & parameters$model[1] != "",as.character(parameters$model[1]),NA)
                       }
if(is.na(formul)){               #if no formula or model is used, the mean as returned as fixed value.
  if(is.na(parameters$estimate)) stop("Function quanti() has NA estimates to work with.")
  if(log_values==TRUE){
    res <- ifelse(back_log==TRUE, parameters_all$estimate, (exp(parameters_all$estimate)-1))
    }
  if(log_values==FALSE){

    res <- ifelse(back_log==TRUE, log(parameters_all$estimate+1), parameters_all$estimate)
  }
  return(res)
}      
      
### -- all variables are calculated --------------------------------------------            
      vars <- all.vars(parse(text=formul))      #all variables in the formula listed
      vars <- vars[vars!="x" & !(vars %in% addit_variables)]                   #x is origin, not anything other
      vars_value <- c()
      
      for(i in 1:length(vars)){                                              #calculation of every other parameter
        current_estimate <- parameters$estimate[parameters$name == vars[i]]  
        current_sterr <- parameters$std.error[parameters$name == vars[i]]
        current_sterr <- ifelse(length(current_sterr)==0 | is.na(current_sterr),0,current_sterr)

        vars_value[i] <- rnorm(1,current_estimate, current_sterr)
        assign(vars[i], vars_value[i])
        assign("x", x_all[i])
      }#END for(i)
      
### -- the result for one area unit is calculated ------------------------------             
      res_tmp <- eval(parse(text=formul))

# --- proportion
      res_tmp <- ifelse(
              log_values==TRUE,((exp(res_tmp)-1))/(exp(x)-1),            
              sum(res_tmp)/x) 
                                                                         


# -- the result is summed over the area and returned ---------------------------
res[area_] <- res_tmp
}#END for(area_)

###--- restrictions for the result: min and max
     if(restriction_yes_no==TRUE){
                         res <- ifelse(!is.na(res_max) & res_max<res, res_max, res) #maximum restriction
                         res <- ifelse(!is.na(res_min) & res_min>res, res_min, res) #minimum restriction
      }#END if(restriction_yes_no)
res <- mean(res)

if(is.na(res)) stop("Function quanti() produced NA result.")
return(res)
}else{#END if(proportion)
res <- 0                  
for(area_ in seq_len(area)){
### -- the weed density on one specific unit area resp. m^2
      x <- x_all[area_] 

### -- range condition: formul selection ---------------------------------------
      if(!anyNA(parameters_all$range_low) | !anyNA(parameters_all$range_up)){
                      parameters <- parameters_all[x>=parameters_all$range_low & x < parameters_all$range_up,]    
                      }else{
                      parameters <- parameters_all
                      }#END if(!anyNA)

if(is.na(formul)){#if no formul is named, the model from param.weed is taken. if there is none, the estimate from param.weed is returned
            formul<- ifelse(!is.na(parameters$model[1]),as.character(parameters$model[1]),NA)
                       }
if(is.na(formul)){               #if no formula or model is used, the mean as returned as fixed value.
                        if(is.na(parameters$estimate)) stop("Function quanti() produced NA result.")
                        if(log_values==TRUE){
                                             res <- ifelse(back_log==TRUE, parameters_all$estimate, (exp(parameters_all$estimate)-1))
                                             }
                        if(log_values==FALSE){
                                             res <- ifelse(back_log==TRUE, log(parameters_all$estimate+1), parameters_all$estimate)
                                             }
                        return(res)
                        }      

### -- all variables are calculated --------------------------------------------            
      vars <- all.vars(parse(text=formul))      #all variables in the formula listed
      vars <- vars[vars!="x" & !(vars %in% addit_variables)]                   #x is origin, not anything other
      vars_value <- c()
      for(i in 1:length(vars)){                                              #calculation of every other parameter
        current_estimate <- parameters$estimate[parameters$name == vars[i]]  
        current_sterr <- parameters$std.error[parameters$name == vars[i]]
        current_sterr <- ifelse(length(current_sterr)==0 | is.na(current_sterr),0,current_sterr)

        
        vars_value[i] <- rnorm(1,current_estimate, current_sterr)
        assign(vars[i], vars_value[i])
        assign("x", x_all[i])
             }#END for(i)
      
### -- the result for one area unit is calculated ------------------------------                
      res_tmp <- eval(parse(text=formul))
      res <- res + ifelse(log_values==TRUE, exp(res_tmp)-1, res_tmp) 
}#END for(area_)

###--- restrictions: min and max
      if(restriction_yes_no==TRUE){
                         res <- ifelse(!is.na(res_max) & res_max<res, res_max, res) #maximum restriction
                         res <- ifelse(!is.na(res_min) & res_min>res, res_min, res) #minimum restriction
                         }#END if(restriction)
      if(is.na(res)) stop("Function quanti() produced NA result.")
      ifelse(back_log==TRUE, return(log(res+1)), return(res))
}#END else(proportion)
}
