#' Gradient boosted propensity score estimation
#'
#' `ps.xgb` calculates propensity scores using gradient boosted logistic
#' regression and diagnoses the resulting propensity scores using a variety of
#' methods
#' 
#' @param formula An object of class [formula]: a symbolic
#'   description of the propensity score model to be fit with the treatment
#'   indicator on the left side of the formula and the 
#'   variables to be balanced on the right side.
#' @param strata An optional factor variable identifying the strata. 
#'   If specified, balance is optimized within strata.
#' @param data A dataset.
#' @param params [xgboost] parameters. 
#' @param file An optional character string naming a file to save intermediate results.
#' @param max.steps An integer specifying the maximum number of steps to take.
#'   Note that `max.steps`*`iters.per.step` must be greater than or equal to `min.iter`. Default: Inf.
#' @param iters.per.step An integer specifying the number of iterations to add
#'   to the model at each step of algorithm. Note that `max.steps`*`iters.per.step` must be greater than or equal to `min.iter`. Default: 100.
#' @param id.var A variable that uniquely identifies observations.
#' @param min.iter An integer specifying the minimum number of iterations before checking for convergence. 
#'   Note that `max.steps`*`iters.per.step` must be greater than or equal to `min.iter`. Default: 1000.
#' @param min.width An integer specifying the minimum number of iterations between the current 
#'   number of iterations and the optimal value. Default: `5*iters.per.step`.
#' @param verbose A logical value indicating if the function should update 
#'   the user on its progres Default: TRUE.
#' @param save.model A logical value indicating if the xgboost model be saved as part of the output object. Default: FALSE.
#' @param weights An optional variable that identifies user defined weights to be incorporated into the optimization.
#' @param linkage An indicator of whether the weighting should be for linkage failure (or nonresponse) versus comparison group construction. 
#'   A value of TRUE requests weighting to account for linkage failure, while a value of FALSE 
#'   requests weighting for comparison group construction. Default: TRUE.
#' @return Returns an object of class `ps.xgb`, a list containing 
#'   * `bal.tab` A table summarizing the balance at the optimal number of iterations.
#'   * `es` A table summarizing the standardized differences within strata at the optimal number of iterations.
#'   * `es.max` A table summarizing the maximum absolute standardized difference by strata.
#'   * `es.mean`A table summarizing the mean absolute standardized difference by strata.
#'   * `iter.per.step` Saves the value of `iters.per.step` specified by the user.
#'   * `opt.iter` The optimal number of iterations.
#'   * `strata` A list of the strata used in the optimization.
#'   * `weight.data` A dataset containing the unique ID and the optimal weight for each observation.
#' @examples
#' # See the vignette for examples.
#' 
#' 
#' @seealso [twang::ps], [xgboost]
#' @keywords nonparametric
#' @concept twang
#'
#' @references Dan McCaffrey, G. Ridgeway, Andrew Morral (2004). "Propensity
#'   Score Estimation with Boosted Regression for Evaluating Adolescent
#'   Substance Abuse Treatment", *Psychological Methods* 9(4):403-425.
#'
#' @import xgboost data.table stats utils
#' @export
#'    
#'    

ps.xgb <- function(formula = formula(data), 
                   strata=NULL,
                   data,
                   params,
                   file=NULL,
                   max.steps=Inf,
                   iters.per.step=100,
                   id.var , 
                   min.iter=1000,
                   min.width=NULL,
                   verbose=TRUE,
                   save.model=FALSE,
                   weights=NULL,
                   linkage=TRUE){
   
   # make weights__temp and N1__temp null to avoid cran check warning about binding 
   weights__temp = N1__temp = NULL
   
   # call back for xgboost
   if (verbose){
      callback.list <- list(cb.print.evaluation(), cb.evaluation.log(n.keep=iters.per.step))
   }else{
      callback.list <- list(cb.evaluation.log(n.keep=iters.per.step))
   }
   
   # check the number of iterations makes sense
   if( max.steps*iters.per.step < min.iter){
      stop("max.steps*iters.per.step must be at least equal to min.iter")
   }
   
   # default min width
   if( is.null(min.width) ){
      min.width = 5*iters.per.step
   }
   
   # convert to data.frame b/c some of this code won't work with tibble, or data.table
   data = as.data.frame(data)
   
   # extract weights
   if (is.null(weights)){
      weights = rep(1 , nrow(data))
   }else{
      weights = data[,weights]
   }
   
   # the "treatment" variable
   treat.var <- as.character(formula[[2]])
   if(is.factor(data[,treat.var])) stop("Treatment indicator must be numeric, not a factor.")
   if( !all(unique(data[,treat.var])%in%c(0,1)) ) stop("Treatment indicator must only take values 0 or 1.")
   
   
   # all this is just to extract the variable names
   terms <- match.call()
   mf <- match.call(expand.dots = FALSE)
   m <- match(c("formula", "data"), names(mf), 0)
   mf <- mf[c(1, m)]
   mf[[1]] <- as.name("model.frame")
   mf$na.action <- na.pass
   mf$subset <- rep(FALSE, nrow(data)) # drop all the data
   mf <- eval(mf, parent.frame())
   Terms <- attr(mf, "terms")
   vars.bal <- attributes(Terms)$term.labels
   
   # create interaction factors
   for (v in grep(":",vars.bal , value = TRUE)){
      data[,gsub(":","..",v)] = with(data,factor(eval(parse(text=v))))
   }
   
   # rename factors
   vars.bal = gsub(":","..",vars.bal)

   # subset the data, expand interactions, etc
   #data = cbind(model.matrix( update(formula, ~ . - 1) , data=data , na.action=na.pass) , data[,c(strata,id.var,treat.var),drop=FALSE] )
   #vars.bal = setdiff(colnames(data),c(strata,id.var,treat.var))
   
   
   # we require 2 variables to be balanced
   if(length(vars.bal) < 2) stop("At least two variables are needed in the right-hand side of the formula.\n")
   
   # we require missing data to be in both groups
   for (v in vars.bal){
      Nmiss = tapply(is.na(data[,v]) , data[,treat.var] , any )
      if( xor(Nmiss[1],Nmiss[2]) ){
         stop(paste0("The covariate ", v, " has missing values that are unique to one level of " , treat.var,". At this time, we do not support this pattern of missingness. Please impute the missing values for ", v, ", drop observations with the problematic missingness, or remove ", v, " from the covariates to be balanced.\n"))
      }
   }
   
   # clean up
   rm(terms , mf , m , Terms , Nmiss)
   
   # only keep data that is used 
   vars.to.keep = c(treat.var , vars.bal , strata , id.var)
   data = subset(data , select=vars.to.keep)
   rm(vars.to.keep)
   
   # drop unused factor levels from raw data
   data = droplevels(data)
   
   # list of tracts including those with no pik variation
   if (is.null(strata)){
      strata = "strata__temp"
      data[,strata] = as.factor("Combined")
   }else{
      data[,strata] = factor(data[,strata])
   }
   
#   if (!is.null(strata)){
      all.strata = levels(data[,strata])
      
      # check for strata with no treatment variation
      all.treat = names(which(tapply(data[,treat.var],data[,strata],function(x) all(x==1))))
      no.treat = names(which(tapply(data[,treat.var],data[,strata],function(x) all(x==0))))
      
      # list of tracts excluding those with no variation
      strata.list = setdiff(all.strata,c(all.treat,no.treat))
      n.strata = length(strata.list)
#   }
   
   # run initial model
   init.time = Sys.time()
   
   # initial model fit
   if (verbose){
      cat("\nFitting initial GLM...\n")
   }
   m.glm = glm(as.formula(paste0(treat.var,"~", paste0(grep("\\.\\.",vars.bal,value=TRUE,invert=TRUE),collapse="+") )) , family='binomial' , data=data , weights=weights)
   initial.p = predict(m.glm , type="response" , newdata=data)
   initial.p = ifelse(is.na(initial.p) , mean(initial.p , na.rm=T) , initial.p )
   
   data$log.p = log(initial.p) - log(1-initial.p)
   
   rm(m.glm , initial.p)
   if (verbose){
      cat("\nTotal time needed to fit GLM...")
      print(Sys.time()-init.time)
   }
   
   # set up data for xgboost
   # include data with no pik variation
   if (n.strata>1){
      sparse.form = reformulate(c("-1",c(vars.bal , strata),"log.p"))
      sparse.data = MatrixModels::model.Matrix(sparse.form , model.frame(terms(sparse.form),data=data[,c(vars.bal,strata,"log.p")] , na.action=na.pass), drop.unused.levels=T , sparse = T)
   }else{
      sparse.form = reformulate(c("-1",c(vars.bal),"log.p"))
      sparse.data = MatrixModels::model.Matrix(sparse.form , model.frame(terms(sparse.form),data=data[,c(vars.bal,"log.p")] , na.action=na.pass), drop.unused.levels=T , sparse = T)
   }
   
   # initialize the xgboost model
   params = c(params,base_score=mean(data[,treat.var]))
   if (verbose){
      cat("\nSetting up gradient boosted model...\n")
   }
   gbm1 <- xgboost(data=sparse.data , label=data[,treat.var], params=params, 
                   feval=pred.xgboost , nrounds=1 , verbose=FALSE , weight = weights , 
                   callbacks= list(cb.evaluation.log(n.keep=iters.per.step) ) ) 
   if (verbose){
      cat("\nCalculating initial balance statistics...\n")
   }
   
   # objects to store initial balance stats
   m.pop = var.pop = list()
   
   bal.time = Sys.time()
   
   if (n.strata>1){
      pb = txtProgressBar(min=1,max=n.strata,char = "+",style=3)
   }
   
   #  loop over the unique strata
   es = list()
   for (i in 1:n.strata){
      stratum = strata.list[i]
      if (n.strata>1){
         setTxtProgressBar(pb, value=i)
      }
      
      # defines data to which we compare our weighted data
      if (linkage){
         # gets population means
         index = data[,strata]==stratum
      }else{
         # gets treatment means
         index = data[,strata]==stratum & data[,treat.var]==1
      }
      bal.data = gen.bal.data(data = data[index ,vars.bal] , var.names = vars.bal)
      bal.weights = weights[index]
      rm(index)

      # find numeric variables
      numeric.vars = bal.data$numeric.vars 
         
      # calculate mean and variance for population
      bal.data = as.matrix(bal.data$bal.data)
      N.pop = nrow(bal.data)
      
      m.pop[[stratum]]  = colSums(sweep(x = bal.data , MARGIN = 1 , STATS = bal.weights , FUN="*" )) / sum(bal.weights) # colMeans(bal.data)
      m2.pop = colSums(sweep(x = bal.data^2 , MARGIN = 1 , STATS = bal.weights , FUN="*" )) / sum(bal.weights) # colMeans(bal.data^2)
      
      var.pop[[stratum]] = m2.pop - (m.pop[[stratum]])^2
      var.pop[[stratum]][numeric.vars] =  var.pop[[stratum]][numeric.vars] * N.pop / (N.pop-1)
      
      if (any(var.pop[[stratum]]==0)){
         var.pop[[stratum]][var.pop[[stratum]]==0]=NA
      }
      
      # clean up for memory
      rm(bal.data , bal.weights)
      gc()
   
      # defines data that is to be weighted
      if (linkage){
         # gets mean with treat = 1
         index = data[,strata]==stratum & data[,treat.var]==1
      }else{
         # gets mean with treat = 0
         index = data[,strata]==stratum & data[,treat.var]==0
      }
      bal.data.pik = gen.bal.data(data = data[index ,vars.bal] , var.names = vars.bal)
      bal.weights = weights[index]
      rm(index)
      
      # calculate mean and variance for population
      bal.data.pik = as.matrix(bal.data.pik$bal.data)
      
      # need mean for pik
      m.w = colSums(sweep(x = bal.data.pik , MARGIN = 1 , STATS = bal.weights , FUN="*" )) / sum(bal.weights)
      
      # clean up
      rm(bal.data.pik , bal.weights)
      
      # standardize difference
      es[[stratum]] = (m.pop[[stratum]] - m.w) / sqrt(var.pop[[stratum]])
   }
   if (n.strata>1){
      close(pb)
   }
   rm(N.pop,m2.pop,m.w)
   
   if (verbose){
      cat("\nTotal time for initial balance calculations: ")
      print(Sys.time()-bal.time)
   }
   
   # keep adding to xgboost until optimal reached.
   flag = 0
   es.mean.out = es.max.out = NULL
   
   # add unweighted values
   es.mean.out = rbind(es.mean.out , sapply(es , function(x) mean(abs(x),na.rm=T)) )
   es.max.out = rbind(es.max.out , sapply(es , function(x) max(abs(x),na.rm=T)) )
   
   while (flag >= 0 & flag < max.steps){
      if (verbose){
         cat(paste0("\nCurrent number of iterations: ", iters.per.step*flag, "\n"))
         cat(paste0("\nAdding ",iters.per.step," iterations:\n"))
      }
      
      # add to the xgboost object (first time use one fewer iteration since model initiated with 1 iteration)
      if (flag==0){
         gbm1 <- xgboost(data=sparse.data , label=data[,treat.var], params=params, 
                         feval=pred.xgboost , nrounds=iters.per.step-1 , verbose=TRUE , weight = NULL, 
                         callbacks=callback.list , xgb_model = gbm1 )
      }else{
         gbm1 <- xgboost(data=sparse.data , label=data[,treat.var], params=params, 
                         feval=pred.xgboost , nrounds=iters.per.step , verbose=TRUE , weight = NULL, 
                         callbacks=callback.list , xgb_model = gbm1 )
      }
      
      # saveRDS(gbm1 , paste0(file.loc,"current_gbm.rds"))
      
      # extract predictions
      # p = plogis(as.matrix(gbm1$evaluation_log))
      p = as.matrix(predict(gbm1,newdata=sparse.data))

      if (verbose){
         cat("\nUpdating balance calculations...\n")
      }
      if (n.strata>1){
         pb = txtProgressBar(min=1,max=n.strata,char = "+",style=3)
      }
      
      # object to store the balance
      es = list()
      bal.time = Sys.time()
      for (i in 1:n.strata){
         stratum = strata.list[i]
         if (n.strata>1){
            setTxtProgressBar(pb, value=i)
         }
         
         # derive weights
         # if (linkage){
         #    index = data[,strata]==stratum & data[,treat.var]==1
         #    w = weights[index] / p[index,,drop=FALSE] 
         # }else{
         #    index = data[,strata]==stratum & data[,treat.var]==0
         #    w = weights * p[index,,drop=FALSE] / (1 - p[index,,drop=FALSE])
         # }
         
         # calculate data needed for balance statistics
         # defines data that is to be weighted
         if (linkage){
            # gets mean with treat = 1
            index = data[,strata]==stratum & data[,treat.var]==1
            bal.data.pik = gen.bal.data(data = data[ index ,vars.bal] , var.names = vars.bal)
            # define weights
            w = weights[index] / p[index,,drop=FALSE] 
         }else{
            # gets mean with treat = 0
            index = data[,strata]==stratum & data[,treat.var]==0
            bal.data.pik = gen.bal.data(data = data[ index ,vars.bal] , var.names = vars.bal)
            # define weights
            w = weights[index] * p[index,,drop=FALSE] / (1 - p[index,,drop=FALSE])
         }
         rm(index)
         
         numeric.vars = bal.data.pik$numeric.vars 
         
         # calculate mean and variance for population
         bal.data.pik = as.matrix(bal.data.pik$bal.data)
         
         # need mean for pik
         num = t(w) %*% bal.data.pik
         den = apply(w,2,sum)
         m.w = sweep(num , 1 , den , "/")
         
         # standardize difference
         es[[stratum]] =  sweep( -sweep( m.w , 2 , m.pop[[stratum]] , "-") , 2 , sqrt(var.pop[[stratum]]) , "/")
         
         # clean up
         rm(bal.data.pik, num, den, m.w, numeric.vars)
         gc()
      }
      
      if (n.strata>1){
         close(pb)
      }
      
      if (verbose){
         cat("\nTotal time updating balance calculations: ")
         print(Sys.time()-bal.time)
      
         cat("Total time: ")
         print(Sys.time()-init.time)
      }
      
      # weights to be exported
      if (linkage){
         w = weights / p 
         w[data[,treat.var]==0,]=0  
      }else{
         w = weights * p / (1-p)
         w[data[,treat.var]==1,]= weights[data[,treat.var]==1]
      }
      
      # recode weights for tracts with no pik variation
      # those with no pik already set to zero from above
      w[data[,strata]%in%all.treat , ] = weights[data[,strata]%in%all.treat]
      
      # find balance summaries
      es.mean = sapply(es , function(x) rowMeans(abs(x),na.rm=T))
      es.max = sapply(es , function(x) apply(abs(x),1,max,na.rm=T))
      
      # store balance summaries
      es.mean.out = rbind(es.mean.out , (es.mean))
      es.max.out = rbind(es.max.out , (es.max))
      
      # select optimal based on es.max
      #  -1 drops unweighted
      opt.iter = which.min(apply(es.max.out,1,mean)[-1])
      
      # store current optimal information
      #  +1 accounts counter starting at 0
      if (opt.iter==(flag+1)){
         es.hold = es
         w.hold = w
      }
      
      # at least 1000 iterations
      if ( (flag+1)*iters.per.step<min.iter){
         flag = flag + 1
      }else{
         # check if the optimal is within 500 iterations of current model
         flag = ifelse( (flag-opt.iter)*iters.per.step < min.width , flag+1 , -1)
         
         # +1 accounts for unweighted data
         if (verbose){
            cat(paste0("\nCurrent optimal balance statistic: " , round(mean(es.max.out[opt.iter+1,]),5) , "\n"))
         }
      }
      
      # rename columns
      colnames(es.mean.out) = strata.list 
      colnames(es.max.out) = strata.list 
      
      # save current results
      if (!is.null(file)){
         out = list(w=w , es.mean=es.mean.out , es.max=es.max.out , opt.iter=opt.iter*iters.per.step ,es=es.hold , strata=strata.list , iters.per.step=iters.per.step)
         if (save.model){
            out = c(out , list(xgb.model=gbm1))
         }
         class(out) = 'ps.xgb'
         saveRDS(out , paste0(file))
      }
   }
   
   # warning if we exited before reaching convergence requirements
   if (flag>=0){
      warning("Convergence criteria was not achieved. Consider increasing max.steps or iters.per.step.")
   }
   
   # clean up
   rm(sparse.data , sparse.form , p , callback.list)
   if (!save.model){
      rm(gbm1)
   }
   
   # create final output
   out = list(es.mean=es.mean.out , es.max=es.max.out , opt.iter=opt.iter*iters.per.step ,es=es.hold , strata=strata.list,iters.per.step=iters.per.step)
   
   # scale weights 
   data$w = as.vector(w.hold)
   rm(w,w.hold)
   
   data$weights__temp = weights
   data.table::setDT(data)
   if (linkage){
      data[ , w:= w / sum(w) * sum(weights__temp) , by=strata]
   }else{
      data[ , N1__temp := sum(get(treat.var)*w) , by=strata]
      data[ get(treat.var)==0 , w:= w / sum(w) * N1__temp , by=strata]
   }
   
   # if a tract has no pik, then w will be NA, so reset to 0
   data[is.na(w) , w:=0]
   
   data = as.data.frame(data)
   
   # replace w with the optimal value
   out$weight.data = data[,c(id.var,"w")]

   # calculate overall balance
   # run covariate by covariate to avoid memory issues
   bal.tab = NULL
   for (v in vars.bal){
      if (linkage){
         index = rep(TRUE,nrow(data))
         bal.data = gen.bal.data(data = data[,v,drop=FALSE] , var.names = v)
         bal.weights = out$weight.data[,"w"]
      }else{
         index = data[,treat.var]==1
         bal.data = gen.bal.data(data = data[index,v,drop=FALSE] , var.names = v)
         bal.weights = out$weight.data[data[index,treat.var]==1,"w"]
      }
      numeric.vars = bal.data$numeric.vars 
      
      # calculate mean and variance for population
      bal.data = as.matrix(bal.data$bal.data)
      N.pop = nrow(bal.data)
      
      m.pop  = colSums(sweep(x = bal.data , MARGIN = 1 , STATS = weights[index] , FUN="*" )) / sum(weights[index]) # colMeans(bal.data)
      m2.pop = colSums(sweep(x = bal.data^2 , MARGIN = 1 , STATS = weights[index] , FUN="*" )) / sum(weights[index]) # colMeans(bal.data^2)
      
      var.pop = m2.pop - (m.pop)^2
      var.pop[numeric.vars] =  var.pop[numeric.vars] * N.pop / (N.pop-1)
      
      if (any(var.pop==0)){
         var.pop[var.pop==0]=NA
      }
      
      # clean up
      rm(bal.data,bal.weights,index)
      
      # defines data that is to be weighted
      if (linkage){
         # gets mean with treat = 1
         index = data[,treat.var]==1
         bal.data.pik = gen.bal.data(data = data[ index ,v, drop=FALSE] , var.names = v)
         # define weights
         w = out$weight.data[index,'w',drop=FALSE] 
      }else{
         # gets mean with treat = 0
         index = data[,treat.var]==0
         bal.data.pik = gen.bal.data(data = data[ index ,v, drop=FALSE] , var.names = v)
         # define weights
         w =  out$weight.data[index,'w',drop=FALSE]
      }
      
      bal.data.pik = as.matrix(bal.data.pik$bal.data)
      
      # calculate weighted mean
      num = t(w) %*% bal.data.pik
      den = apply(w,2,sum)
      m.w =  num/den # colSums(sweep(x = bal.data.pik , MARGIN = 1 , STATS =w , FUN="*" )) / sum(w)
      
      # unweighted mean among pik
      m.pik = colSums(sweep(x = bal.data.pik , MARGIN = 1 , STATS = weights[index] , FUN="*" )) / sum(weights[index])
      #m.pik = colMeans(bal.data.pik)
      
      # clean up
      rm(bal.data.pik,index)
      
      # standardize difference
      es.overall =  sweep( -sweep( m.w , 2 , m.pop , "-") , 2 , sqrt(var.pop) , "/")
      es.unw =  (m.pop-m.pik)/sqrt(var.pop)
      
      bal.tab = rbind(bal.tab , t(rbind(m.pop , m.pik, m.w , es.unw , es.overall)))
   }
   
   # create a balance table
   #bal.tab = t(rbind(m.pop , m.w , es.overall))
   if (linkage){
      colnames(bal.tab) = c("Population Mean","Unadjusted Mean","Adjusted Mean","Unadjusted Standardized Difference","Adjusted Standardized Difference")
   }else{
      colnames(bal.tab) = c("Treatment Group Mean","Unadjusted Comparison Group Mean","Adjusted Comparison Group Mean","Unadjusted Standardized Difference","Adjusted Standardized Difference")
   }
   
   # save at the optimal for each covariate
   if (save.model){
      out = c(out , list(xgb.model=gbm1))
   }
   out = c(out,list(bal.tab=bal.tab))
   class(out) = "ps.xgb"
           
   return(out)
}

