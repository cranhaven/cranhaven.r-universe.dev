#' This function compute the DGM bounds for all the different coefficients, adapted to the point identification test.
#'
#' @param Ldata dataset containing (Y,Xc) where Y is the outcome, Xc are potential common regressors.
#' @param Rdata dataset containing (Xnc,Xc) where Xnc are the non commonly observed regressors, Xc are potential common regressors.
#' @param values the different unique points of support of the common regressor Xc.
#' @param sam0 the directions q to compute the radial function.
#' @param refs0 indicating the positions in the vector values corresponding to the components of betac.
#' @param out_var label of the outcome variable Y.
#' @param nc_var label of the non commonly observed regressors Xnc.
#' @param c_var label of the commonly observed regressors Xc.
#' @param constraint a vector indicating the different constraints in a vector of the size of X_c indicating the type of constraints, if any on f(X_c) : "concave", "concave", "nondecreasing", "nonincreasing", "nondecreasing_convex", "nondecreasing_concave",  "nonincreasing_convex", "nonincreasing_concave", or NULL for none. Default is NULL, no contraints at all.
#' @param nc_sign sign restrictions on the non-commonly observed regressors Xnc: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
#' @param c_sign sign restrictions on the commonly observed regressors: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
#' @param nbCores number of cores for the parallel computation. Default is 1.
#' @param eps_default If grid =NULL, then epsilon is taken equal to eps_default.
#' @param nb_pts the constant C in DGM for the epsilon_0, the lower bound on the grid for epsilon, taken equal to nb_pts*ln(n)/n. Default is 1 without regressors Xc, 3 with Xc.
#' @param Bsamp the number of bootstrap/subsampling replications. Default is 1000.
#' @param grid the number of points for the grid search on epsilon. Default is 30. If NULL, then epsilon is taken fixed equal to kp.
#' @param weights_x the sampling weights for the dataset (Xnc,Xc). Default is NULL.
#' @param weights_y  the sampling weights for the dataset (Y,Xc). Default is NULL.
#' @param outside if TRUE indicates that the parallel computing has been launched outside of the function. Default is FALSE.
#' @param meth the method for the choice of epsilon, either "adapt", i.e. adapted to the direction or "min" the minimum over the directions. Default is "adapt".
#' @param modeNA indicates if NA introduced if the interval is empty. Default is FALSE.
#' @param version version of the computation of the ratio, "first" indicates no weights, no ties, same sizes of the two datasets; "second" otherwise. Default is "second".
#' @param version_sel version of the selection of the epsilon, "first" indicates no weights, no ties, same sizes of the two datasets; "second" otherwise. Default is "second".
#' @param alpha for the level of the confidence region. Default is 0.05.
#' @param projections if FALSE compute the identified set along some directions or the confidence regions. Default is FALSE
#' @param R2bound the lower bound on the R2 of the long regression if any. Default is NULL.
#' @param values_sel the selected values of Xc for the conditioning. Default is NULL.
#' @param ties Boolean indicating if there are ties in the dataset. Default is FALSE.
#' @param seed set a seed to fix the subsampling replications.
#'
#'
#' @return a list containing, in order:
#'  - ci : a list with all the information on the confidence intervals
#'
#'     * upper: upper bound of the confidence interval on the radial function S in the specified direction at level alpha, possibly with sign constraints
#'
#'     * lower: lower bound upper bound of the confidence interval on the radial function S, possibly with sign constraints
#'
#'     * unconstr: confidence interval on the radial function S, without sign constraints
#'
#'     * If common regressors, upper_agg, lower_agg, and unconstr_agg reports the same values but aggregated over the values of Xc (see the parameter theta0 in the paper)
#'
#'     * betac_ci: confidence intervals on each coefficients related to the common regressor, possibly with sign constraints
#'
#'     * betac_ci_unc: confidence intervals on each coefficients related to the common regressor without sign constraints
#'
#'     If projection is TRUE:
#'
#'        * support: confidence bound on the support function in each specified direction
#'
#'  - point : a list with all the information on the point estimates
#'
#'     * upper: the upper bounds on betanc, possibly with sign constraints
#'
#'     * lower: the lower bounds on betanc, possibly with sign constraints
#'
#'     * unconstr: bounds on betanc without sign constraints
#'
#'     * If common regressors, upper_agg, lower_agg, and unconstr_agg reports the same values but aggregated over the values of Xc (see the parameter theta0 in the paper)
#'
#'     * betac_pt: bounds on betanc, possibly with sign constraints
#'
#'     * betac_pt_unc: bounds on betanc without sign constraints
#'      If projection ==TRUE:
#'
#'     * support: point estimate of the support function in each specified direction
#'
#'  - epsilon : the values of the selected epsilon(q)

DGM_bounds_test <- function(Ldata, Rdata,values,
                       sam0,
                       refs0,
                       out_var,  nc_var, c_var =NULL,
                       constraint = NULL,
                       nc_sign  = NULL, c_sign = NULL,
                       nbCores=1,
                       eps_default =0.5,nb_pts=1,Bsamp=1000,grid=30,
                       weights_x = NULL, weights_y = NULL,
                       outside = FALSE,
                       meth="adapt",
                       modeNA =FALSE,
                       version = "first",
                       version_sel = "first",
                       alpha=0.05,
                       projections = FALSE,
                       R2bound=NULL,
                       values_sel=NULL,
                       ties = FALSE,
                       seed = 2131){


  ###
  # change kp=0.5 => eps_default = 0.5
  # Suppr. ,C0=0.5,trunc=2,
  # winsor = FALSE, laisser le code, supprimer l'appel.
  #

  ######
  kp=eps_default
  Rs=0
  ## unused
  trunc=2
  ## nb of obs min to use the conditioning.
  limit = 1
  # lim = limit
  #### get sizes
  dimXc = length(c_var)
  dimXnc = length(nc_var)
  q95 <-  function(x){quantile(x,1-alpha,na.rm=T)}
  q05 <-  function(x){quantile(x,alpha,na.rm=T)}

  # if(dimXc >=1 & nb_pts <3){
  #   nb_pts =3
  # }
  # if(dimXnc>1 & Bsamp>=1000){
  #   Bsamp=200
  # }
  # if(dimXnc>1 & grid>15){
  #   grid=15
  # }

  if(is.null(values_sel) & (dimXc>0)){
    values_sel= vector("list")
    values_sel[["selected"]] = values
    values_sel[["old"]] = values
  }


  #####################
  ## grid is the number of points on the grid of the selection rule of epsilon.



    if( dimXc!=0){
      ### dataset 1
      Xc_x = as.matrix(Rdata[,c_var],dim(Rdata)[1],dimXc)
      Xnc = as.matrix(Rdata[,nc_var],dim(Rdata)[1],dimXnc)

      ### dataset 2
      Xc_y = as.matrix(Ldata[,c_var],dim(Ldata)[1],dimXc)
      Y = as.matrix(Ldata[,out_var],dim(Ldata)[1],1)

    }else{


      ### dataset 1
      Xc_x = NULL
      Xnc = as.matrix(Rdata[,nc_var],dim(Rdata)[1],dimXnc)
      ### dataset 2
      Xc_y = NULL
      Y = as.matrix(Ldata[,out_var],dim(Ldata)[1],1)

    }

  if(outside ==FALSE & nbCores>1){
    ### subsampling (B samples) parallel nbCores=10
    sfInit(parallel = TRUE, cpus = nbCores, type = "SOCK")

    sfExport( "Xc_x","Xnc", "Xc_y" ,"Y",
    "values", "sam0","refs0",
    "out_var",  "nc_var", "c_var", "constraint",
    "nc_sign", "c_sign",'compute_ratio',
    "nbCores","sampling_rule",
    "eps_default", "nb_pts","Bsamp" ,"grid",
    "weights_x","weights_y","outside",  "meth",
    "modeNA", "version" ,
    "version_sel",
    "alpha" , "projections", "R2bound" ,"values_sel",
    "ties","dimXc","dimXnc","limit")

    # sfExportAll( )
    # sfLibrary(R.matlab)
    # sfLibrary(pracma)
    # sfLibrary(Hmisc)
  }

  output  <- vector("list")
  hull_point=NULL
  hull_sharp=NULL
  seed0 = floor(seed/100)

  #################################################################################################################
  ##########  epsilon selection   #################################################################################
  if(!is.null(grid)){


    if( projections == FALSE){
      if(dimXnc==1){
        sam1 = sam0
      }else{
        sam00 <- eye(dimXnc)
        sam00 <- rbind(-sam00,sam00)
        sam1 = sam00
      }
    }else{
      sam00 <- eye(dimXnc)
      sam00 <- rbind(-sam00,sam00)
      sam1 = sam00
    }

    set.seed(seed)

      eps_default0 =  select_epsilon_test(sam1,eps_default,  Xc_x,Xnc,Xc_y,Y,
                                     values,dimXc,dimXnc, nb_pts,lim =limit ,
                                     weights_x,weights_y, refs0, grid, constraint, c_sign, nc_sign, meth=meth,
                                     nbCores=nbCores,  version_sel = version_sel, alpha=alpha ,ties =ties)

    set.seed(NULL)

  }else{

    if(meth=="min"){
      eps_default0 = eps_default
    }else{
      eps_default0 = matrix(eps_default,dim(sam0)[1],1)
    }
  }



  #################################################################################################################
  #################################################################################################################

  #### compute the radial function
  if(projections == FALSE){
    # limit = 100
    sample1 =NULL
    minsel="normal"
    # lim =limit

    ### compute point estimate
    type="both"
    pt =FALSE
    # version ="first"

    # eps_default1 <- eps_default0*0.5
    # eps_default1[ eps_default1 >0.5] <- 0.5
    mat_var_out1 <- compute_radial(sample1 =sample1,Xc_x,Xnc,Xc_y,Y,
                                   values,dimXc,dimXnc,
                                   nb_pts, sam0, eps_default0 ,grid,lim =limit,
                                   weights_x,weights_y, constraint,
                                   c_sign, nc_sign,refs0,type="both",meth=meth, version = version,
                                   R2bound,values_sel,ties)

    # mat_var_out1$upper
    # pt =FALSE
    if(!is.null(R2bound)){
      Rs <-  mat_var_out1$Rs
    }

    # hull_point$upper

    hull_point <-    mat_var_out1
    if(modeNA ==TRUE){
      no_inter =  hull_point [["upper"]] <  -hull_point[["lower"]]
      hull_point [["upper"]][no_inter] <- NA
      hull_point [["lower"]][no_inter] <- NA

      # if(dimXnc>1 & length(c_sign)>0){
      #   if(sum(abs(c_sign))>0){
      #     no_inter = apply(matrix((hull_point [["tests"]][,-c(1)]< 10^(-5)), dim(hull_point [["tests"]])[1],  dim(hull_point [["tests"]])[2]-1),1,prod)*TRUE
      #     hull_point [["upper"]][!no_inter] <- NA
      #     hull_point [["lower"]][!no_inter] <- NA
      #   }
      # }
    }


    ##### compute point estimate of betac if common regressors Xc ###################################################################################""
    if(!is.null(values)){

      beta1_pt <- compute_bnds_betac(sample1 =NULL, info0 = mat_var_out1, values,  constraint, c_sign0 = c_sign, nc_sign0=nc_sign ,  refs0,  c_var, nc_var,sam0,
                                     info1=NULL , constr=TRUE,R2bound,values_sel)

      mat_beta1_l = matrix(0,Bsamp,length(refs0))
      mat_beta1_u = matrix(0,Bsamp,length(refs0))
      hull_point[["betac_pt"]] <-  beta1_pt
    }


    #### replications numerical bootstrap or subsampling ############################################################################################
    if(Bsamp >0){
      set.seed(seed0)
      if(nbCores>1){
        res0 <- sfLapply(1:Bsamp, compute_radial_test, Xc_x,Xnc,Xc_y,Y,values,dimXc,dimXnc,nb_pts,
                         sam0, eps_default0,grid,lim =limit ,
                         weights_x,   weights_y, constraint ,c_sign , nc_sign,refs0,type="both",meth=meth,
                         version = version, R2bound,values_sel,ties )

      }else{

        res0 <- lapply(1:Bsamp, compute_radial_test, Xc_x,Xnc,Xc_y,Y,
                       values,dimXc,dimXnc,nb_pts,sam0, eps_default0,grid,lim =limit ,
                       weights_x,   weights_y, constraint ,c_sign , nc_sign,refs0,type="both",meth=meth,
                       version = version, R2bound,values_sel,ties)
      }
      set.seed(NULL)


      mat_varb = matrix(0,Bsamp,dim(sam0)[1])
      mat_varb0 = matrix(0,Bsamp,dim(sam0)[1])
      mat_varb_unc = matrix(0,Bsamp,dim(sam0)[1])

      for(b in 1:Bsamp){
        mat_varb[b,] = res0[[b]][["upper"]]
        mat_varb0[b,] = res0[[b]][["lower"]]
        mat_varb_unc[b,] = res0[[b]][["unconstr"]]
      }

      n_x = dim(Xnc)[1]
      n_y = dim(Y)[1]
      # cbind(-mat_varb_unc[,1],mat_varb0[,2])
      # -mat_varb_out_unc[1]


      # cbind(mat_varb[,2],mat_varb_unc[,2])
      # cbind(-mat_varb_unc[,1],-mat_varb0[,2],mat_varb[,2],mat_varb_unc[,2])
      # cbind(-mat_varb_unc[,1],-mat_varb[,1],-mat_varb0[,1],mat_varb_unc[,2])
      # no_inter =    mat_varb <  mat_varb0
      # mat_varb[no_inter] <- NA
      # mat_varb0[no_inter] <- NA
      # cbind(-mat_varb_unc[,1],-mat_varb[,1])
      T_xy = (n_y/(n_x+n_y))*n_x
      n_xy = min(n_x,n_y)
      bs = ceil(sampling_rule(T_xy))
      bs0 = sqrt(bs/T_xy)

      mat_varb_out_unc  =   hull_point[["unconstr"]] - apply((mat_varb_unc -   matrix(1,dim(mat_varb_unc)[1],1)%*% hull_point[["unconstr"]])*bs0  ,2,q05)
      # cbind(-mat_varb0[,2],mat_varb_unc[,1])

      mat_var_out= hull_point[["upper"]] - apply((mat_varb -  matrix(1,dim(mat_varb)[1],1)%*% hull_point[["upper"]])*bs0,2,q05)
      # mat_var_in=  hull_point[["lower"]] - apply((mat_varb0 -  matrix(1,dim(mat_varb0)[1],1)%*%hull_point[["lower"]])*bs0,2,q05)
      # mat_var_in=  -(-hull_point[["lower"]] - apply((mat_varb0  - matrix(1,dim(mat_varb0)[1],1)%*%hull_point[["lower"]])*bs0,2,q95))

      # inf is stacked as -S, hence the minus. Otherwise, its  Sh -q_{1-a}(Sh - S)
      mat_var_in=  -( -hull_point[["lower"]] - apply((matrix(1,dim(mat_varb0)[1],1)%*%hull_point[["lower"]]-  mat_varb0  )*bs0,2,q95) )

      ##
      hull_sharp[["upper"]] <-  mat_var_out
      hull_sharp[["lower"]] <-  mat_var_in
      hull_sharp[["unconstr"]] <-  mat_varb_out_unc

      ## stock replications
      hull_sharp[["upper_repli"]] <-  mat_varb
      hull_sharp[["lower_repli"]] <-  mat_varb0
      hull_sharp[["unconstr_repli"]] <-  mat_varb_unc

      #### compute the replications for beta_c ###################################################################################

      if(!is.null(values)){

        ### with sign constraints ######################################################################
        if(nbCores>1){
          res0b <- sfLapply(1:Bsamp,compute_bnds_betac, info0 = res0, values, constraint,  c_sign0 = c_sign, nc_sign0=nc_sign ,
                            refs0, c_var, nc_var,sam0, info1=mat_var_out1, constr=TRUE,R2bound,values_sel)
        }else{
          res0b <- lapply(1:Bsamp,compute_bnds_betac, info0 = res0, values,  constraint,   c_sign0 = c_sign, nc_sign0=nc_sign ,
                          refs0, c_var, nc_var,sam0, info1=mat_var_out1, constr=TRUE,R2bound,values_sel)
        }



        for(b in 1:Bsamp){
          mat_beta1_l[b,] = res0b[[b]][,1]
          mat_beta1_u[b,] = res0b[[b]][,2]
        }
        # bs = floor(sampling_rule(n_x*(n_y/(n_x+n_y))))
        # term = sqrt(bs/T_xy)

        # beta1_l_ci =  beta1_pt[,1] +   apply( (mat_beta1_l -  matrix(1,dim(mat_beta1_l)[1],1)%*% beta1_pt[,1])* bs0,2,q05)
        # beta1_u_ci   = beta1_pt[,2] +   apply( (mat_beta1_u -  matrix(1,dim(mat_beta1_l)[1],1)%*% beta1_pt[,2])* bs0,2,q95)

        beta1_l_ci =  beta1_pt[,1] -   apply( (mat_beta1_l -  matrix(1,dim(mat_beta1_l)[1],1)%*% beta1_pt[,1])* bs0,2,q95)
        beta1_u_ci   = beta1_pt[,2] -   apply( (mat_beta1_u -  matrix(1,dim(mat_beta1_l)[1],1)%*% beta1_pt[,2])* bs0,2,q05)

        beta1_ci <- cbind(beta1_l_ci,  beta1_u_ci )
        hull_sharp[["betac_ci"]] <-  beta1_ci
        hull_point[["betac_pt"]] <-  beta1_pt


        ### without sign constraints ######################################################################"
        beta1_pt <- compute_bnds_betac(sample1 =NULL, info0 = mat_var_out1, values,
                                       constraint , c_sign0 = c_sign, nc_sign0=nc_sign , refs0, c_var, nc_var,  sam0, info1=NULL ,constr=FALSE)




        mat_beta1_l = matrix(0,Bsamp,length(refs0))
        mat_beta1_u = matrix(0,Bsamp,length(refs0))

        if(nbCores>1){
          res0b <- sfLapply(1:Bsamp,compute_bnds_betac, info0 = res0, values, constraint ,  c_sign0 = c_sign, nc_sign0=nc_sign ,  refs0, c_var,
                            nc_var, sam0, info1=NULL , constr=FALSE)
        }else{
          res0b <- lapply(1:Bsamp,compute_bnds_betac,info = res0, values,  constraint ,  c_sign0 = c_sign, nc_sign0=nc_sign ,  refs0, c_var,
                          nc_var, sam0 , info1=NULL, constr=FALSE)
        }


        for(b in 1:Bsamp){
          mat_beta1_l[b,] = res0b[[b]][,1]
          mat_beta1_u[b,] = res0b[[b]][,2]
        }
        # beta1_l_ci   =  beta1_pt[,1] +   apply((mat_beta1_l -  matrix(1,dim(mat_beta1_l)[1],1)%*% beta1_pt[,1])* bs0,2,q05)
        # beta1_u_ci   =  beta1_pt[,2] +   apply((mat_beta1_u -  matrix(1,dim(mat_beta1_l)[1],1)%*% beta1_pt[,2])* bs0,2,q95)

        beta1_l_ci   =  beta1_pt[,1] -   apply((mat_beta1_l -  matrix(1,dim(mat_beta1_l)[1],1)%*% beta1_pt[,1])* bs0,2,q95)
        beta1_u_ci   =  beta1_pt[,2] -   apply((mat_beta1_u -  matrix(1,dim(mat_beta1_l)[1],1)%*% beta1_pt[,2])* bs0,2,q05)
        beta1_ci <- cbind(beta1_l_ci,  beta1_u_ci )
        hull_sharp[["betac_ci_unc"]] <-  beta1_ci
        hull_point[["betac_pt_unc"]] <-  beta1_pt
      }



      if(modeNA ==TRUE){
        no_inter =   hull_sharp[["upper"]] <  hull_sharp[["lower"]]
        hull_sharp[["upper"]][no_inter] <- NA
        hull_sharp[["lower"]][no_inter] <- NA
      }
    }else{
      hull_sharp <- matrix(NA,1,1)

    }

    if(outside ==FALSE & nbCores>1){
      sfStop()
    }

    ###########################################################################################################################################################
    ########################################### Compute projections (Support function) ########################################################################
    ###########################################################################################################################################################
  }else{

    ## do not use c_sign
    if(!is.null(c_sign)){
      c_sign= 0*c_sign
    }

    set.seed(seed)
    minsel="normal"
    mat_var_out1 <- compute_support(sample1 =NULL,Xc_x,Xnc,Xc_y,Y,
                                    values,dimXc,dimXnc,
                                    nb_pts, sam0, eps_default0, grid,lim =limit,
                                    weights_x,weights_y, constraint,
                                    c_sign =c_sign, nc_sign,refs0,type="both",meth=meth, bc=TRUE,
                                    version = version, R2bound,values_sel,ties)

    hull_point[["support"]] <-      mat_var_out1
    set.seed(NULL)

    if(Bsamp!=0){
      # mat_var_out1
      Bsamp1 = Bsamp
      # start_time <- Sys.time()
      if(nbCores>1){
        res0 <- sfLapply(1:Bsamp1, compute_support,Xc_x,Xnc,Xc_y,Y,values,dimXc,dimXnc,nb_pts,sam0, eps_default0,
                         grid,lim =limit ,
                         weights_x,   weights_y, constraint,
                         c_sign = c_sign, nc_sign,refs0,type="both",meth=meth,
                         bc=TRUE, version = version, R2bound,values_sel,ties)


      }else{
        res0 <- lapply(1:Bsamp1, compute_support, Xc_x,Xnc,Xc_y,Y,values,dimXc,dimXnc,nb_pts,sam0, eps_default0,
                       grid,lim =limit ,
                       weights_x,   weights_y, constraint,
                       c_sign = c_sign, nc_sign,refs0,type="both",meth=meth,
                       bc=TRUE, version = version,R2bound,values_sel,ties)
      }
      mat_varb = matrix(0,Bsamp1,dim(sam0)[1])
      for(b in 1:Bsamp1){
        mat_varb[b,] = res0[[b]][,3]
      }

      n_x = dim(Xnc)[1]
      n_y = dim(Y)[1]
      #### subsampling
      T_xy=n_x*(n_y/(n_x+n_y))
      n_xy = min(n_x,n_y)

      bs = ceil(sampling_rule(T_xy))
      bs0 = sqrt(bs/T_xy)
      # mat_var_out = pmax(0, mat_var_out1[,3] -   apply(mat_varb -  matrix(1,dim(mat_varb)[1],1)%*% mat_var_out1[,3],2,q05)* bs0)
      mat_var_out =  mat_var_out1[,3] -   apply(mat_varb -  matrix(1,dim(mat_varb)[1],1)%*% mat_var_out1[,3],2,q05)* bs0


      hull_sharp[["support"]] <-  cbind(sam0,mat_var_out)
    }else{
      hull_sharp[["support"]] <-  matrix(NA,1,1)
    }

    if(outside ==FALSE & nbCores>1){
      sfStop()
    }

  } ###########################

  output[["ci"]] <- hull_sharp
  output[["point"]] <- hull_point
  output[["epsilon"]] <- eps_default0
  if(!is.null(R2bound)){
    output[["Rs"]] <-  Rs
  }
  return(output)
}
