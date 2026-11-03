#' Function performing the test of point identification on a validation sample.
#'
#' @param validation dataset containing the joint distribution (Y,Xnc,Xc) where Y is the outcome,  Xnc are the non commonly observed regressors,  Xc are potential common regressors.
#' @param Ldata dataset containing (Y,Xc) where Y is the outcome, Xc are potential common regressors. Default is NULL
#' @param Rdata dataset containing (Xnc,Xc) where Xnc are the non commonly observed regressors, Xc are potential common regressors. Default is NULL.
#' @param out_var label of the outcome variable Y.
#' @param nc_var label of the non commonly observed regressors Xnc.
#' @param c_var label of the commonly observed regressors Xc.
#' @param alpha the level of the confidence intervals. Default is 0.05.
#' @param constraint a vector indicating the different constraints in a vector of the size of X_c indicating the type of constraints, if any on f(X_c) : "concave", "concave", "nondecreasing", "nonincreasing", "nondecreasing_convex", "nondecreasing_concave",  "nonincreasing_convex", "nonincreasing_concave", or NULL for none. Default is NULL, no contraints at all.
#' @param nc_sign if sign restrictions on the non-commonly observed regressors Xnc: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
#' @param c_sign if sign restrictions on the commonly observed regressors: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
#' @param weights_validation the sampling weights for the full dataset (Y, Xnc,Xc). Default is NULL.
#' @param weights_x the sampling weights for the dataset (Xnc,Xc). Default is NULL.
#' @param weights_y  the sampling weights for the dataset (Y,Xc). Default is NULL.
#' @param nbCores number of cores for the parallel computation. Default is 1.
#' @param grid the number of points for the grid search on epsilon. Default is 30. If NULL, then epsilon is taken fixed equal to eps_default.
#' @param eps_default If grid =NULL, then epsilon is taken equal to eps_default.
#' @param R2bound the lower bound on the R2 of the long regression if any. Default is NULL.
#' @param unchanged Boolean indicating if the categories based on Xc must be kept unchanged (TRUE). Otherwise (FALSE), a thresholding approach is taken imposing that each value appears more than 10 times in both datasets and 0.01 per cent is the pooled one. Default is FALSE.
#' @param ties Boolean indicating if there are ties in the dataset. Default is FALSE.
#'
#' @return a list containing, in order:
#' - S: the point estimation used the statistic for the test
#'
#' - S_ci: the CI on the upper bound
#'
#' - stat: the statistic of the test
#'
#' - the critical value at level alpha
#'
#' - the p_value of the test
#'
#' - the fit with the OLS on this sample
#'
#' - n the sample size
#'
#' - epsilon, the choice of epsilon we made
#'
#' - r2long the r2 on the  long regression
#'
#'  -r2short the r2 on the short regression
#'
#' @export
#'
#' @examples
#'
#' ### Simulating joint distribution according to this DGP
#' n=200
#' Xnc = rnorm(n,0,1.5)
#' epsilon = rnorm(n,0,1)
#'
#' ## true value
#' beta0 =1
#' Y = Xnc*beta0 + epsilon
#' out_var = "Y"
#' nc_var = "Xnc"
#'
#' # create the datasets
#' validation<- as.data.frame(cbind(Y,Xnc))
#' colnames(validation) <- c(out_var,nc_var)
#'
#'
#' ############# Estimation #############
#' test = point_ident_test (validation, Ldata=NULL,Rdata=NULL,out_var,nc_var)
#'
#'
point_ident_test <- function(validation,Ldata =NULL ,Rdata =NULL, out_var,nc_var, c_var=NULL, alpha =0.05,
                             constraint = NULL,
                             nc_sign = NULL, c_sign = NULL, weights_validation = NULL,
                             weights_x = NULL,weights_y = NULL,
                             nbCores=1,
                             grid = 10,
                             eps_default = 0.5,
                             R2bound=NULL,
                             unchanged = FALSE,
                             ties = FALSE){

  version = "second"
  projections=FALSE
  if(is.null( Ldata) |is.null( Rdata)){
    Ldata<- as.data.frame(validation[,c( out_var,c_var)])
    colnames(Ldata) <- c( out_var,c_var)

    weights_x =weights_validation
    weights_y =weights_validation

    Rdata <- as.data.frame(validation[,c(nc_var,c_var)])
    colnames(Rdata) <- c(nc_var,c_var)
  }

  ###
  # form = paste0(out_var, " ~ ")
  # form1 = paste0(nc_var,collapse = "+")
  # form = as.formula(paste0(form,form1))
  # fit <- lm(  form , data = validation, weights=weights_validation)
  # betahat =  fit$coefficients[nc_var]


  #########
  validation2 <- validation
  if(length(c_var)==0){
    fm_OLS <- formula(paste0(out_var, " ~ ",nc_var ))
  }else{
    validation2[,c_var] <- as.factor(  validation2[,c_var])
    fm_OLS <- formula(paste0(out_var, " ~ ",nc_var ,"+  ", paste(c_var,collapse = "+")))
  }
  fit  <- lm(fm_OLS ,weights=weights_validation, data= validation2)
  betahat =  fit$coefficients[nc_var]

  # den =    wtd.var(validation2[,nc_var],weights_validation, normwt=TRUE)     #var(merge_cp[,out_var_l],na.rm=T)
  #
  # names_id = sort(unique( validation2[,c_var]))
  # ln =length(names_id )-1
  # rat = rep(0,ln)
  # for(k in 1:ln){
  #   rat[k] <- cov(validation2[!is.na(validation2[,nc_var]),nc_var],validation2[!is.na(validation2[nc_var]),c_var]==k)/  den
  # }
  # betahat =  fit_OLS$coefficients[nc_var]  + sum( fit_OLS$coefficients[-c(1,2)]*rat, na.rm=T)













  ###### compute stat #############################################"

  dimXc = length(c_var)
  dimXc_old= dimXc
  dimXnc = length(nc_var)

  meth = "adapt"

  # version= "second"
  # version_sel = "first"

  modeNA=FALSE
  winsor= FALSE
  trunc=2
  C0= 0.5
  C=1
  if(dimXnc >1){
    Bsamp=200
  }else{
    Bsamp=1000
  }


  weights_xs =  weights_x
  weights_ys =  weights_y
  output <- vector("list")

  idx_output = 1
  names_output = NULL

  values_sel = NULL
  ####
  projections0 = projections
  if(dimXc!=0){
    if(dimXc!=1){
      values = create_values(dimXc,c_var,Rdata)
      refs0=NULL
      for(j in 1:dim(values)[1]){
        if(sum(values[j,]==0)==(dim(values)[2]-1)){
          refs0 = c( refs0,j )
        }
      }
    }else{
      values = matrix(create_values(dimXc,c_var,Rdata))
      refs0 = (1:length(values))[values>0]
    }

    ####### if Xc
    # select pooled observations
    if(dimXc==1){
      Xc_pool <- c(Rdata[,c_var],Ldata[,c_var])
    }else{
      Xc_pool <- rbind(Rdata[,c_var],Ldata[,c_var])
    }
    ### preliminary screening on Xc ###

    ## tabulate values of initial Xc
    values_tab = matrix(0,dim(values)[1],3)
    for(k in 1:dim(values)[1]){
      values_tab[k,1] <- tabulate_values(k,values,Rdata[,c_var],dimXc)
      values_tab[k,2] <- tabulate_values(k,values,Ldata[,c_var],dimXc)
      values_tab[k,3] <- tabulate_values(k,values,Xc_pool,dimXc)
    }

    ## select values according to threshold and create new Xc
    select_values=NULL
    unselect_values=NULL

    # tomod_values=NULL
    if(unchanged==TRUE){
      select_values = 1:dim(values)[1]
      unselect_values = NULL
    }else{
      for(j in 1:dim(values)[1]){
        ### condition selection of Xc values ###
        if(  values_tab[j,1]>=10 &  values_tab[j,2]>=10 &   values_tab[j,3]>=(0.01/100*(dim(Rdata)[1] + dim(Ldata)[1]))){
          select_values = c(select_values, j)
        }else{
          unselect_values = c(unselect_values, j)
        }
      }
    }

    values_sel = vector("list")
    values_sel[["selected"]] <- values[select_values,]
    values_sel[["old"]] <- values

    c_sign_old= c_sign
    c_sign = c_sign[(select_values-1)[-c(1)]]

    ### V1: redefine first class as {1, select_values}
    Xc_x = matrix(0,dim(Ldata)[1],1)
    Xc_y = matrix(0,dim(Rdata)[1],1)
    ind=1
    for(j in select_values[-c(1)]){
      if(dimXc==1){
        val = values[j,]
        sel_x = (Ldata[,c_var]==val)
        sel_y = (Rdata[,c_var]==val)
      }else{
        val = t(as.matrix(values[j,]))
        sel_x = matrix(1,dim(Ldata)[1],1)
        sel_y = matrix(1,dim(Rdata)[1],1)
        for(ddd in 1:dimXc){
          sel_x =  sel_x & (Ldata[,ddd]==val[ddd])
          sel_y =  sel_y & (Rdata[,ddd]==val[ddd])
        }
        sel_x = matrix( sel_x,dim(Ldata)[1],1)
        sel_y = matrix( sel_y,dim(Rdata)[1],1)
      }
      Xc_x[sel_x]=ind
      Xc_y[sel_y]=ind
      ind= ind+1
    }

    table(Xc_y)
    colnames(Xc_y) <- "Xc"
    colnames(Xc_x) <- "Xc"
    Rdata0 <- Rdata
    Ldata0 <- Ldata
    Rdata <- as.data.frame(cbind(Rdata[,nc_var],   Xc_y))
    colnames(Rdata) <- c(nc_var,"Xc")
    Ldata <- as.data.frame(cbind(Ldata[,out_var],   Xc_x))
    colnames(Ldata) <- c(out_var,"Xc")
    dimXc_old = dimXc
    c_var_old = c_var
    values_old = values
    # c_sign_old = c_sign

    dimXc=1
    c_var = "Xc"
    ####
    groups=1
    values = matrix(create_values(dimXc,c_var,Rdata))
    refs0 = (1:length(values))[values>0]

    if( length(c_sign) < (length(values)-1)){
      c_sign = rep(0,(length(values)-1) )
    }

    # values_old[select_values,]
    # values_old[unselect_values,]
  }else{
    values = NULL
    s= NULL
    refs0 = NULL
  }


  if(dimXc==0){
    if(dimXnc>1){
      nb_pts=0.5
    }else{
      nb_pts=1
    }
  }else{
    if(dimXnc>1){
      nb_pts=0.5
    }else{
      nb_pts=1
    }
  }

  ##### fit long one #################
  if(length(c_var)>0){
    ###
    form = paste0(out_var, " ~ ")
    form1 = paste0(c(nc_var,"names"),collapse = "+")
    # form2 = paste0(,collapse = "+")
    form_ext = as.formula(paste0(form,form1))
    validation$names <- as.factor(validation$names)
    fit_long <- lm( form_ext , data = validation, weights=weights_validation)
    r2long = summary(fit_long)$r.squared


    form1 = paste0(c("names"),collapse = "+")
    form_sh = as.formula(paste0(form,form1))
    fit_sh <- lm(  form_sh , data = validation, weights=weights_validation)
    r2short  = summary(fit_sh)$r.squared

  }

  ######################################################


  # sam0 = fit_long$coefficients[nc_var]
  sam0 = betahat  #(fit$coefficients[nc_var])
  sam0 = rbind(sam0,sam0)

  # sam0 <- eye(dimXnc)
  # sam0 <- rbind(-sam0,sam0)

  out<-  DGM_bounds_test(Ldata, Rdata, values,
                     sam0,refs0,
                     out_var,  nc_var, c_var, constraint,
                     nc_sign, c_sign,
                     nbCores=nbCores,
                     eps_default= eps_default, nb_pts= nb_pts,Bsamp=Bsamp,grid =grid,
                     weights_x =weights_x,weights_y =weights_y,outside=FALSE,  meth=meth,
                     modeNA =modeNA,
                     version = version,
                     version_sel =version,
                     alpha=alpha,
                     projections = FALSE,
                     R2bound=    R2bound,
                     values_sel=values_sel,
                     ties = ties)

  # out <- out1
# out$epsilon
  mat_varb  = out$ci$upper_repli[,1]
  # attributes(out$point)

  n = dim(validation)[1]
  bs = sampling_rule(floor(n/2))
  q95 <-  function(x){quantile(x,1-alpha,na.rm=T)}
  dist_c = sqrt(bs)*(mat_varb -  out$point$upper[1])
  crit = quantile(dist_c ,1-alpha,na.rm=T) #apply(  dist_c ,2,q95)
  FF = ecdf(dist_c)
  # stat = sqrt(n)*(out$point$upper[1] -1)
  stat = sqrt(n)*(out$point$upper[1] - 1)
  p_value = 1-FF(stat)


  # T_reps = sort(dist_c)
  # B=length( T_reps)
  # p_index = 0
  # for (p_rep in 1:B) {
  #   if (T_reps[p_rep] < stat) {
  #     p_index = p_index + 1
  #   }
  # }
  # if (p_index == 0) {
  #   p_index = 1
  # }
  # p_value = 1 - (p_index - 1)/(B - 1)



  output = vector("list")
  output[["S"]] <- out$point$upper
  output[["S_ci"]] <- out$ci$upper
  output[["stat"]] <- stat
  output[["crit"]] <-  crit
  output[["p_value"]]<-p_value
  output[["ols"]] <- fit
  output[["n"]] <- n
  output[["epsilon"]] <- out$epsilon
  if(length(c_var)>0){
    output[["r2long"]] <-   r2long
    output[["r2short"]] <-   r2short
  }
  # output["FF"] <- FF
  return(output)
}
