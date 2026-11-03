#' Function computing all the different bounds : DGM and/or Variance
#'
#' @param Ldata a dataset including Y and possibly X_c=(X_c1,...,X_cq). X_c must be finitely supported.
#' @param Rdata a dataset including X_nc and the same variables X_c as in Ldata.
#' @param out_var the label of the outcome variable Y.
#' @param nc_var the labels of the regressors X_nc.
#' @param c_var the labels of the regressors X_c (if any).
#' @param constraint a vector of size q indicating the type of constraints (if any) on the function f(x_c1,...,x_cq) for k=1,...,q:  "convex", "concave", "nondecreasing", "nonincreasing", "nondecreasing_convex", "nondecreasing_concave", "nonincreasing_convex", "nonincreasing_concave", or NA for no constraint. Default is NULL, namely no constraints at all.
#' @param nc_sign a vector of size p indicating sign restrictions on each of the p coefficients of X_nc. For each component, -1 corresponds to a minus sign, 1 to a plus sign and 0 to no constraint. Default is NULL, namely no  constraints at all.
#' @param c_sign same as nc_sign but for X_c (accordingly, it is a vector of size q).
#' @param weights_x the sampling weights for the dataset Rdata. Default is NULL.
#' @param weights_y  the sampling weights for the dataset Ldata. Default is NULL.
#' @param nbCores number of cores for the parallel computation. Default is 1.
#' @param methods method used for the bounds: "DGM" (Default) and/or "Variance".
#' @param grid the number of points for the grid search on epsilon. If NULL, then grid search is not performed and epsilon is taken as eps_default. Default is 10.
#' @param alpha one minus the nominal coverage of the confidence intervals. Default is 0.05.
#' @param eps_default a pre-specified value of epsilon used only if the grid search for selecting the value of epsilon is not performed, i.e, when grid is NULL. Default is 0.5.
#' @param R2bound the lower bound on the R2 of the long regression if any. Default is NULL.
#' @param projections a boolean indicating if the identified set and confidence intervals on beta_0k for k=1,...,p are computed (TRUE), rather than the identified set and confidence region of beta_0 (FALSE). Default is FALSE.
#' @param unchanged a boolean indicating if the categories based on X_c must be kept unchanged (TRUE). Otherwise (FALSE), a thresholding approach is taken imposing that each value appears more than 10 times in both datasets and represents more than 0.01 per cent of the pooled dataset (of size n_X+n_Y). Default is FALSE.
#' @param ties a boolean indicating if there are ties in the dataset. If not (FALSE), computation is faster. Default is FALSE.
#' @param seed to avoid fixinx the seed for the subsampling, set to NULL. Otherwise 2131.
#' @param mult a list of multipliers of our selected epsilon to look at the robustness of the point estimates with respect to it. Default is NULL
#'
#' @return  Use summary_regCombin for a user-friendly print of the estimates. Returns a list containing, in order:
#' - DGM_complete or Variance_complete : the complete outputs of the functions DGM_bounds or Variance_bounds.
#'
#'  and additional pre-treated outputs, replace below "method" by either "DGM" or "Variance":
#'
#' - methodCI:  the confidence region on the betanc without sign constraints
#'
#' - methodpt:  the bounds point estimates on the betanc without sign constraints
#'
#' - methodCI_sign:  the confidence region on the betanc with sign constraints
#'
#' - methodpt_sign:  the bounds point estimates on the betanc with sign constraints
#'
#' - methodkp: the values of epsilon(q)
#'
#' - methodbeta1: the confidence region on the betac corresponding to the common regressors Xc without sign constraints
#'
#' - methodbeta1_pt: the bounds point estimates on the betac corresponding to the common regressors Xc without sign constraints
#'
#' - methodbeta1_sign: the confidence region on the betac corresponding to the common regressors Xc with sign constraints
#'
#' - methodbeta1_sign_pt: the bounds point estimates on the betac corresponding to the common regressors Xc with sign constraints
#'
#' @export
#'
#' @examples
#'
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
#' output <- regCombin(Ldata,Rdata,out_var,nc_var)


#'
#'
#'
regCombin <- function(Ldata, Rdata,
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
                      seed = 2131,
                      mult = NULL){


  ## if no weights, same data sizes, and no ties, faster estimation.
  if(is.null(weights_x) && is.null(weights_y) && (dim(Ldata)[1]==dim(Rdata)[2]) &&  (ties == FALSE) ){
    version= "first"
  }else{
    version= "second"
  }
  version_sel = version


  ## parameter for handling the Xc (either convert to one or keep )
  hold_specif_Xc = TRUE
  set = FALSE
  #### get sizes
  dimXc = length(c_var)
  dimXc_old= dimXc
  dimXnc = length(nc_var)
  # modeNA indicates if NA introduced if the interval is empty.
  modeNA=FALSE
  # if winsorisation
  meth = "adapt"
  winsor= FALSE
  trunc=2
  C0= 0.5
  C=1


  #### number of subsampling replications ###############
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

  ### save weigths
  weights_xs =  weights_x
  weights_ys =  weights_y
  output <- vector("list")

  idx_output = 1
  names_output = NULL

  ############ handling the Xc  #######################################################
  ####### reformating them if needded #################################################

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
    Xc_x = matrix(0,dim(Rdata)[1],1)
    Xc_y = matrix(0,dim(Ldata)[1],1)
    ind=1
    for(j in select_values[-c(1)]){
      if(dimXc==1){
        val = values[j,]
        sel_x = (Rdata[,c_var]==val)
        sel_y = (Ldata[,c_var]==val)
      }else{
        val = t(as.matrix(values[j,]))
        sel_x = matrix(1,dim(Rdata)[1],1)
        sel_y = matrix(1,dim(Ldata)[1],1)
        for(ddd in 1:dimXc){
          sel_x =  sel_x & (Rdata[,c_var[ddd]]==val[ddd])
          sel_y =  sel_y & (Ldata[,c_var[ddd]]==val[ddd])
        }
        sel_x = matrix( sel_x,dim(Rdata)[1],1)
        sel_y = matrix( sel_y,dim(Ldata)[1],1)
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
    Rdata <- as.data.frame(cbind(Rdata[,nc_var],   Xc_x))
    colnames(Rdata) <- c(nc_var,"Xc")
    Ldata <- as.data.frame(cbind(Ldata[,out_var],   Xc_y))
    colnames(Ldata) <- c(out_var,"Xc")
    dimXc_old = dimXc
    c_var_old = c_var
    values_old = values
    # c_sign_old = c_sign

    ##################################################################################
    ##################################################################################


    ################# handling the sign, after modifications #########################

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

  #
  #   if(dimXc==0){
  #     if(dimXnc>1){
  #       nb_pts=0.6
  #     }else{
  #       nb_pts=1
  #     }
  #   }else{
  if(dimXnc>1){
    nb_pts=0.6
  }else{
    nb_pts=1
  }
  # }

  #### iterate over the possibly selected methods ##############################

  for(  method in methods ){
    if( method=="DGM"){

      sam0 <- eye(dimXnc)
      sam0 <- rbind(-sam0,sam0)
      eps0 = 0


      out <- DGM_bounds(Ldata, Rdata, values,
                        sam0,refs0,
                        out_var,  nc_var, c_var, constraint,
                        nc_sign, c_sign,
                        nbCores=nbCores,
                        eps_default= eps_default, nb_pts= nb_pts,Bsamp=Bsamp,grid =grid,
                        #list_ex=list_ex,
                        weights_x =weights_x,weights_y =weights_y,outside=FALSE,  meth=meth,
                        modeNA =modeNA, version =version,
                        version_sel =  version_sel,
                        alpha = alpha , projections=projections, R2bound ,values_sel=  values_sel,
                        ties =  ties, mult=mult, seed = seed)



      output[[paste0(method,"_complete")]] <-  out


      if(dimXnc==1){
        mt_sharpCI_proj_sign <- out$ci$upper
        mt_sharpCI_proj_sign_low <- out$ci$lower
        mt_sharpCI_proj  <-  out$ci$unconstr

        mt_sharp0_proj_sign  <- out$point$upper
        mt_sharp0_proj_sign_low  <- out$point$lower
        mt_sharp0_proj  <- out$point$unconstr

        hull0 <-  mt_sharpCI_proj_sign
        hull0_low <-  mt_sharpCI_proj_sign_low


        ####" handling the aggregate values if any Xc ################
        if(!is.null(values)){
          mt_sharpCI_proj_sign_agg <- out$ci$upper_agg
          mt_sharpCI_proj_sign_low_agg <- out$ci$lower_agg
          mt_sharpCI_proj_agg  <-  out$ci$unconstr_agg

          mt_sharp0_proj_sign_agg  <- out$point$upper_agg
          mt_sharp0_proj_sign_low_agg  <- out$point$lower_agg
          mt_sharp0_proj_agg  <- out$point$unconstr_agg

          hull0_agg <-  mt_sharpCI_proj_sign_agg
          hull0_low_agg <-  mt_sharpCI_proj_sign_low_agg
        }


        out00 = matrix(0,dimXnc,2)

        for(id0 in 1:dimXnc){

          out00[id0,1] = -mt_sharpCI_proj_sign_low[id0+1]
          out00[id0,2] = mt_sharpCI_proj_sign[id0+1]


        }
        output[[paste0(method,"CI_sign")]] <- out00

        for(id0 in 1:dimXnc){

          out00[id0,1] = -mt_sharp0_proj_sign_low[id0+1]
          out00[id0,2] = mt_sharp0_proj_sign[id0+1]



        }
        output[[paste0(method,"pt_sign")]] <- out00


        ##################### aggregated values ####################################
        if(!is.null(values)){

          out00_agg = matrix(0,dimXnc,2)

          for(id0 in 1:dimXnc){

            out00_agg[id0,1] = -mt_sharpCI_proj_sign_low_agg[id0+1]
            out00_agg[id0,2] = mt_sharpCI_proj_sign_agg[id0+1]


          }
          output[[paste0(method,"CI_sign_agg")]] <- out00_agg

          for(id0 in 1:dimXnc){

            out00_agg[id0,1] = -mt_sharp0_proj_sign_low_agg[id0+1]
            out00_agg[id0,2] = mt_sharp0_proj_sign_agg[id0+1]



          }
          output[[paste0(method,"pt_sign_agg")]] <- out00_agg



        }

      }else{
        ### dimXnc >1


        if(projections==TRUE){
          ## support function
          ## sign constraints not implemented yet in dimXnc >1
          output[[paste0(method,"CI_sign")]] <- NULL
          output[[paste0(method,"pt_sign")]] <- NULL

          v0 = NULL
          for(d in 1:dimXnc){
            v0 <- c(v0,paste0("q_",d))
          }
          out_ci <-  out$ci$support
          colnames(out_ci) <- c(v0,"Support")
          out_pt <-  out$point$support
          colnames(out_pt) <- c(v0,"Support")

          res_ci <- matrix(NA,dimXnc,2)
          res_pt <- matrix(NA,dimXnc,2)
          for(d in 1:dimXnc){
            sub <- out_ci[out_ci[,d]!=0,c(d,dimXnc+1)]
            res_ci[d,] <- c(apply(sub,1,prod))
            sub <- out_pt[out_pt[,d]!=0,c(d,dimXnc+1)]
            res_pt[d,] <- c(apply(sub,1,prod))
          }


          output[[paste0(method,"support_CI")]] <- out_ci
          output[[paste0(method,"support_pt")]] <- out_pt
          output[[paste0(method,"CI")]] <- res_ci
          output[[paste0(method,"pt")]] <- res_pt

        }else{
          #### the status is different/ convex hull points.
          mt_sharpCI_sign <- out$ci$upper
          mt_sharp0_sign <- out$point$upper

          output[[paste0(method,"CI_all_sign")]] <-  mt_sharpCI_sign
          output[[paste0(method,"pt_all_sign")]] <-  mt_sharp0_sign
          # output[[paste0(method,"CI")]] <-  mt_sharpCI_sign
          # output[[paste0(method,"pt")]] <-  mt_sharp0_sign

          if(!is.null(nc_sign) | !is.null(c_sign)){
            mt_sharpCI_proj <- out$ci$unconstr
            mt_sharp0_proj  <- out$point$unconstr
          }else{
            mt_sharpCI_proj <- out$ci$upper
            mt_sharp0_proj  <- out$point$upper
          }


        }

      }

      output[[paste0(method,"kp_sign")]] <-   out$epsilon

      sam0 <- eye(dimXnc)
      sam0 <- rbind(-sam0,sam0)

      # bounds on betanc not yet implemented in dimXnc > 1
      if(dimXc>0 & dimXnc ==1){

        beta1K <-  out$ci$betac_ci
        beta1K_pt <-  out$point$betac_pt

      }else{
        beta1K = c(NA,NA)
        beta1K_pt = c(NA,NA)
      }

      output[[ paste0(method,"beta1_sign")]] <- beta1K
      output[[ paste0(method,"beta1_sign_pt")]] <- beta1K_pt
    }



    ##### For the variance bounds ####################################################
    if( method=="Variance"){

      sam0 <- eye(dimXnc)
      sam0 <- rbind(-sam0,sam0)

      if(dimXnc==1){projections0=FALSE}else{projections0=TRUE}

      out <- Variance_bounds(Ldata,Rdata,
                             out_var, c_var, nc_var, constraint,
                             c_sign, nc_sign, projections=projections0,
                             values,sam0, refs0,
                             nb_pts,eps_default,nbCores,Bsamp=1000,
                             weights_x =weights_x,weights_y =weights_y,
                             values_sel=  values_sel)

      output[[paste0(method,"_complete")]] <-  out
      mt_sharpCI_proj_sign <- out$ci$upper
      mt_sharpCI_proj_sign_low <- out$ci$lower
      mt_sharp0_proj_sign  <- out$point$upper
      mt_sharp0_proj_sign_low <- out$point$lower

      mt_sharpCI_proj <- out$ci$unconstr
      mt_sharp0_proj  <- out$point$unconstr

      # out00 = matrix(0,dimXnc,2)
      # output[[paste0(method,"CI_sign")]] <- mt_sharpCI_proj_sign
      # output[[paste0(method,"pt_sign")]] <-   mt_sharp0_proj_sign

      out00 = matrix(0,dimXnc,2)

      for(id0 in 1:dimXnc){

        out00[id0,1] = -mt_sharpCI_proj_sign_low[id0+1]
        out00[id0,2] = mt_sharpCI_proj_sign[id0+1]


      }
      output[[paste0(method,"CI_sign")]] <- out00

      for(id0 in 1:dimXnc){

        out00[id0,1] = -mt_sharp0_proj_sign_low[id0+1]
        out00[id0,2] = mt_sharp0_proj_sign[id0+1]



      }
      output[[paste0(method,"pt_sign")]] <- out00

    }


    #### 1) bounds on beta_nc without any sign constraints
    if(projections==FALSE | dimXnc==1){



      out00 = matrix(0,dimXnc,2)

      for(id0 in 1:dimXnc){
        if(method!="HP"){
          out00[id0,1] = -mt_sharpCI_proj[id0]
        }else{
          out00[id0,1] = mt_sharpCI_proj[id0]
        }
        out00[id0,2] = mt_sharpCI_proj[dimXnc+id0]
      }
      output[[paste0(method,"CI")]] <- out00

      for(id0 in 1:dimXnc){
        if(method!="HP"){
          out00[id0,1] = - mt_sharp0_proj[id0]
        }else{
          out00[id0,1] = mt_sharp0_proj[id0]
        }
        out00[id0,2] = mt_sharp0_proj[dimXnc+id0]
      }
      output[[paste0(method,"pt")]] <- out00
    }


    if(!is.null(values) & method!="Variance" & dimXnc==1){


      out00_agg = matrix(0,dimXnc,2)

      for(id0 in 1:dimXnc){
        if(method!="HP"){
          out00_agg[id0,1] = -mt_sharpCI_proj_agg[id0]
        }else{
          out00_agg[id0,1] = mt_sharpCI_proj_agg[id0]
        }
        out00_agg[id0,2] = mt_sharpCI_proj_agg[dimXnc+id0]
      }
      output[[paste0(method,"CI_agg")]] <- out00_agg

      for(id0 in 1:dimXnc){
        if(method!="HP"){
          out00_agg[id0,1] = - mt_sharp0_proj_agg[id0]
        }else{
          out00_agg[id0,1] = mt_sharp0_proj_agg[id0]
        }
        out00_agg[id0,2] = mt_sharp0_proj_agg[dimXnc+id0]
      }
      output[[paste0(method,"pt_agg")]] <- out00_agg

    }

    ######################### bounds on betac, the coef of Xc ############################"

    if(dimXc>0 & dimXnc==1){

      if(method=="Variance"){

        beta1K <- out$ci$betac_ci_unc
        beta1K0 <- out$point$betac_pt_unc
        output[[paste0(method,"beta1_sign")]] <- out$ci$betac_ci
        output[[paste0(method,"beta1_sign_pt")]] <- out$point$betac_pt

      }else{

        beta1K <-  out$ci$betac_ci_unc
        beta1K0 <-  out$point$betac_pt_unc

      }

    }else{
      beta1K = c(NA,NA)
      beta1K0 =c(NA,NA)
    }

    output[[paste0(method,"beta1")]] <- beta1K
    output[[paste0(method,"beta1_pt")]] <- beta1K0
  }


  ###############################################################################


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

  if(is.null(R2bound)){
    output[["R2bound"]] <- NA
  }else{
    output[["R2bound"]] <- R2bound
  }

  if(dimXc_old>=1){
    output[["select_values"]] <-select_values
    output[["unselect_values"]] <-unselect_values
    output[["values"]] <-values
    output[["values_old"]] <-values_old
  }
  if(dimXc_old>1){
    output[["dimXc_old"]] <-dimXc_old
    output[["c_var_old"]] <-c_var_old
  }

  return(output)




}
