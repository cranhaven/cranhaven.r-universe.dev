#' Computing the DGM bounds for different values of epsilon, proportional to the data-driven  selected one
#'
#' @param Ldata dataset containing (Y,Xc) where Y is the outcome, Xc are potential common regressors.
#' @param Rdata dataset containing (Xnc,Xc) where Xnc are the non commonly observed regressors, Xc are potential common regressors.
#' @param out_var label of the outcome variable Y.
#' @param nc_var label of the non commonly observed regressors Xnc.
#' @param c_var label of the commonly observed regressors Xc.
#' @param constraint a vector indicating the different constraints in a vector of the size of X_c indicating the type of constraints, if any on f(X_c) : "concave", "concave", "nondecreasing", "nonincreasing", "nondecreasing_convex", "nondecreasing_concave",  "nonincreasing_convex", "nonincreasing_concave", or NULL for none. Default is NULL, no contraints at all.
#' @param nc_sign if sign restrictions on the non-commonly observed regressors Xnc: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
#' @param c_sign if sign restrictions on the commonly observed regressors: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
#' @param weights_x the sampling weights for the dataset (Xnc,Xc). Default is NULL.
#' @param weights_y  the sampling weights for the dataset (Y,Xc). Default is NULL.
#' @param nbCores number of cores for the parallel computation. Default is 1.
#' @param methods method used for the bounds: "DGM" (Default) and/or "Variance".
#' @param grid the number of points for the grid search on epsilon. Default is 30. If NULL, then epsilon is taken fixed equal to eps_default.
#' @param alpha the level of the confidence intervals. Default is 0.05.
#' @param eps_default If grid =NULL, then epsilon is taken equal to eps_default.
#' @param R2bound the lower bound on the R2 of the long regression if any. Default is NULL.
#' @param projections if FALSE compute the identified set along some directions or the confidence regions. Default is FALSE
#' @param unchanged Boolean indicating if the categories based on Xc must be kept unchanged (TRUE). Otherwise (FALSE), a thresholding approach is taken imposing that each value appears more than 10 times in both datasets and 0.01 per cent is the pooled one. Default is FALSE.
#' @param ties Boolean indicating if there are ties in the dataset. Default is FALSE.
#' @param multipliers different multipliers of our selected epsilon to compute the bounds. Default is 0.25,0.5,1,1.5,2.
#'
#' @return a list containing, in order:
#' - details: a list with all the detailled results of the estimation for the different multipliers. see "regCombin".
#'
#' - Profile_point : a matrix with the profile of the bounds without constraints for different values of the multiplier.
#'
#' - Profile_point_sign : a matrix with the profile of the bounds with constraints for different values of the multiplier.
#' @export
#'
#'
#' @examples
#' ### Simulating according to this DGP
#' n=200
#' Xnc_x = rnorm(n,0,1.5)
#' Xnc_y = rnorm(n,0,1.5)
#' epsilon = rnorm(n,0,1)
#'
#' ## true value
#' beta0 =1
#' Y = Xnc_y*beta0 + epsilon
#' out_var = "Y"
#' nc_var = "Xnc"
#'
#' # create the datasets
#' Ldata<- as.data.frame(Y)
#' colnames(Ldata) <- c(out_var)
#' Rdata <- as.data.frame(Xnc_x)
#' colnames(Rdata) <- c(nc_var)
#'
#'
#' ############# Estimation #############
#' profile = regCombin_profile(Ldata,Rdata,out_var,nc_var, multipliers = seq(0.1,3,length.out=3))
#'

regCombin_profile <- function(Ldata, Rdata,
                              out_var, nc_var, c_var =NULL,
                              constraint = NULL,
                              nc_sign = NULL, c_sign = NULL,
                              weights_x = NULL,weights_y = NULL,
                              nbCores=1,
                              methods=c("DGM"),
                              grid = 10,
                              alpha=0.05,
                              eps_default = 0.5,
                              R2bound=NULL,
                              projections= FALSE,
                              unchanged=FALSE,
                              ties = FALSE,
                              multipliers = c(0.25,0.5,1,1.5,2)){

  ## if no weights, same data sizes, and no ties, faster estimation.
  if(is.null(weights_x) && is.null(weights_y) && (dim(Ldata)[1]==dim(Rdata)[2]) &&  (ties == FALSE) ){
    version= "first"
  }else{
    version= "second"
  }
  version_sel = version

  version_sel = version
  ## parameter for handling the Xc (either convert to one or keep )
  hold_specif_Xc = TRUE
  set = FALSE
  #### get sizes
  dimXc = length(c_var)
  dimXc_old= dimXc
  dimXnc = length(nc_var)

  # modeNA indicates if NA introduced if the interval is empty. Default is FALSE.
  # winsor indicates if winsorisation. Default is FALSE.
  # trunc equal to 2, for the definition of epsilon.
  # C0 the upper bound on the grid for epsilon. Default is 0.5.
  # C the constant C in DGM for the epsilon_0, the lower bound on the grid for epsilon, taken equal to C*ln(n)/n. Default is 1 without regressors Xc, 3 with Xc.
  # Bsamp the number of bootstrap/subsampling replications. Default is 1000.
  # meth the method for the choice of epsilon, either "adapt", i.e. adapted to the direction or "min" the minimum over the directions. Default is "adapt".
  # version version of the computation of the ratio, "first" is a degraded version but fast; "second" is a correct version but slower. Default is "second".
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
    if(!is.null(R2bound)){
      if(R2bound >1){
        Bsamp=1000
      }
    }else{
      Bsamp=1000
    }
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

  # for(  method in methods ){
  #   if( method=="DGM"){
  method="DGM"
  sam0 <- eye(dimXnc)
  sam0 <- rbind(-sam0,sam0)
  # if(dimXnc==1){projections0=FALSE}else{projections0=TRUE}
  eps0 = 0

  output_all = vector("list")
  output_pt = matrix(NA,length(multipliers),2)
  output_pt_sign = matrix(NA,length(multipliers),2)
  type0 = "first"
  # multipliers = c(1, multipliers)
  for(jj in 1:(1+length(multipliers))){
    ### first estimation, mult =1


    if(jj ==1){
      out <- DGM_bounds(Ldata, Rdata, values,
                        sam0,refs0,
                        out_var,  nc_var, c_var, constraint,
                        nc_sign, c_sign,
                        nbCores=nbCores,
                        eps_default= eps_default, nb_pts= nb_pts,Bsamp=0 ,grid =grid,
                   #     list_ex=list_ex,
                        weights_x =weights_x,weights_y =weights_y,outside=FALSE,  meth=meth,
                        modeNA =modeNA, version =version,
                        version_sel =  version_sel,
                        alpha = alpha , projections=projections, R2bound ,values_sel=  values_sel,
                        ties =  ties)
      mult_curr=1
      epsilon_reference = out$epsilon

    }else{

      mult_curr = multipliers[jj-1]
      out <- DGM_bounds(Ldata, Rdata, values,
                        sam0,refs0,
                        out_var,  nc_var, c_var, constraint,
                        nc_sign, c_sign,
                        nbCores=nbCores,
                       eps_default= epsilon_reference ,
                        nb_pts= nb_pts,Bsamp=0,
                        grid = NULL ,
                   #     list_ex=list_ex,
                        weights_x =weights_x,weights_y =weights_y,outside=FALSE,  meth=meth,
                        modeNA =modeNA, version =version,
                        version_sel =  version_sel,
                        alpha = alpha , projections=projections, R2bound ,values_sel=  values_sel,
                        ties =  ties,
                        mult =mult_curr )


    }

    output[[paste0(method,"_complete")]] <-  out


    # if(dimXnc==1){
    # mt_sharpCI_proj_sign <- out$ci$upper
    # mt_sharpCI_proj_sign_low <- out$ci$lower
    # mt_sharpCI_proj  <-  out$ci$unconstr

    mt_sharp0_proj_sign  <- out$point$upper
    mt_sharp0_proj_sign_low  <- out$point$lower
    mt_sharp0_proj  <- out$point$unconstr

    # hull0 <-  mt_sharpCI_proj_sign
    # hull0_low <-  mt_sharpCI_proj_sign_low


    if(!is.null(values)){
      # mt_sharpCI_proj_sign_agg <- out$ci$upper_agg
      # mt_sharpCI_proj_sign_low_agg <- out$ci$lower_agg
      # mt_sharpCI_proj_agg  <-  out$ci$unconstr_agg

      mt_sharp0_proj_sign_agg  <- out$point$upper_agg
      mt_sharp0_proj_sign_low_agg  <- out$point$lower_agg
      mt_sharp0_proj_agg  <- out$point$unconstr_agg
      #
      # hull0_agg <-  mt_sharpCI_proj_sign_agg
      # hull0_low_agg <-  mt_sharpCI_proj_sign_low_agg
      #
      #

    }


    # out00 = matrix(0,dimXnc,2)
    #
    # for(id0 in 1:dimXnc){
    #
    #   # out00[id0,1] = -mt_sharpCI_proj_sign_low[id0+1]
    #   # out00[id0,2] = mt_sharpCI_proj_sign[id0+1]
    #   #
    #
    # }
    # output[[paste0(method,"CI_sign")]] <- out00

    out00 = matrix(0,dimXnc,2)

    for(id0 in 1:dimXnc){

      out00[id0,1] = -mt_sharp0_proj_sign_low[id0+1]
      out00[id0,2] = mt_sharp0_proj_sign[id0+1]



    }
    output[[paste0(method,"pt_sign")]] <- out00


    ##################### aggregated values ####################################
    if(!is.null(values)){

      out00_agg = matrix(0,dimXnc,2)

      # for(id0 in 1:dimXnc){
      #
      #   out00_agg[id0,1] = -mt_sharpCI_proj_sign_low_agg[id0+1]
      #   out00_agg[id0,2] = mt_sharpCI_proj_sign_agg[id0+1]
      #
      #
      # }
      # output[[paste0(method,"CI_sign_agg")]] <- out00_agg

      for(id0 in 1:dimXnc){

        out00_agg[id0,1] = -mt_sharp0_proj_sign_low_agg[id0+1]
        out00_agg[id0,2] = mt_sharp0_proj_sign_agg[id0+1]



      }
      output[[paste0(method,"pt_sign_agg")]] <- out00_agg



    }


    output[[paste0(method,"kp_sign")]] <-   out$epsilon

    sam0 <- eye(dimXnc)
    sam0 <- rbind(-sam0,sam0)


    # out00 = matrix(0,dimXnc,2)
    #
    # for(id0 in 1:dimXnc){
    #   out00[id0,1] = mt_sharpCI_proj[id0]
    #   out00[id0,2] = mt_sharpCI_proj[dimXnc+id0]
    # }
    # output[[paste0(method,"CI")]] <- out00

    out00 = matrix(0,dimXnc,2)

    for(id0 in 1:dimXnc){
      out00[id0,1] = - mt_sharp0_proj[id0]
      out00[id0,2] = mt_sharp0_proj[dimXnc+id0]
    }
    output[[paste0(method,"pt")]] <- out00


    if(!is.null(values) & method!="Variance"){

      # out00_agg = matrix(0,dimXnc,2)
      #
      # for(id0 in 1:dimXnc){
      #   out00_agg[id0,1] = mt_sharpCI_proj_agg[id0]
      #   out00_agg[id0,2] = mt_sharpCI_proj_agg[dimXnc+id0]
      # }
      # output[[paste0(method,"CI_agg")]] <- out00_agg

      for(id0 in 1:dimXnc){
        out00_agg[id0,1] = - mt_sharp0_proj_agg[id0]
        out00_agg[id0,2] = mt_sharp0_proj_agg[dimXnc+id0]
      }
      output[[paste0(method,"pt_agg")]] <- out00_agg

    }


    ####
    output[["n_y"]] <- dim(Ldata)[1]
    output[["n_x"]] <- dim(Rdata)[1]
    output[["out_var"]] <- out_var
    output[["nc_var"]] <- nc_var
    output[["c_var"]] <- c_var
    output[["constraint"]] <- constraint
    output[["nc_sign"]] <- nc_sign
    output[["c_sign"]] <- c_sign
    output[["method"]] <- methods
    # output[["Opt"]] <- Opt
    output[["alpha"]] <- alpha
    output[["sam0"]] <- sam0
    output[["Bsamp"]] <- Bsamp
    output[["mult"]] <- mult_curr

    output_all[[jj]] <-  output_all
    if(jj!=1){
      output_pt[jj-1,] <- output[[paste0(method,"pt")]]
      output_pt_sign[jj-1,] <- output[[paste0(method,"pt_sign")]]
    }
    # outall_ci[jj,] <- output[[paste0(method,"CI")]]

  }

  all0 = vector("list")
  all0[["detail"]] <-  output_all

  output_pt <- cbind(multipliers,output_pt)
  colnames(output_pt) <- c("multiplier","Pt. Est. Lower Bnd." ,"Pt. Est. Upper Bnd.")

  output_pt_sign <- cbind(multipliers,output_pt_sign)
  colnames(output_pt_sign) <- c("multiplier","Pt. Est. Lower Bnd." ,"Pt. Est. Upper Bnd.")


  all0[["Profile_point"]] <-  output_pt
  all0[["Profile_point_sign"]] <-  output_pt_sign
  # all0[["Profile_CI"]] <-  outall_ci




  return( all0)




}
