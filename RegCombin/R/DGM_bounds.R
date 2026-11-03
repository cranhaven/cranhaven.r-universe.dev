#' This function compute the DGM bounds for all the different coefficients.
#'
#' @param Ldata dataset containing (Y,Xc) where Y is the outcome, Xc are potential common regressors.
#' @param Rdata dataset containing (Xnc,Xc) where Xnc are the non commonly observed regressors, Xc are potential common regressors.
#' @param values the different unique points of support of the common regressor Xc.
#' @param sam0 the directions q to compute the radial function.
#' @param refs0 indicating the positions in the vector values corresponding to the components of betac.
#' @param out_var label of the outcome variable Y.
#' @param nc_var label of the non commonly observed regressors Xnc.
#' @param c_var label of the commonly observed regressors Xc.
#' @param constraint a vector indicating the different constraints in a vector of the size of X_c indicating the type of constraints, if any on f(X_c) : "concave", "concave", "nondecreasing", "nonincreasing", "nondecreasing_convex", "nondecreasing_concave",  "nonincreasing_convex", "nonincreasing_concave", or NULL for none. Default is NULL, no contraints at all.#' @param nc_sign if sign restrictions on the non-commonly observed regressors Xnc: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
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
#' @param mult a list of multipliers of our selected epsilon to look at the robustness of the point estimates with respect to it. Default is NULL
#' @param seed set a seed to fix the subsampling replications
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
#'
#' @export
#'
#' @examples
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
#'  values = NULL
#'s= NULL
#' refs0 = NULL
#'
#' sam0 <- rbind(-1,1)
#' eps0 = 0
#' ############# Estimation #############
#' output <- DGM_bounds(Ldata,Rdata,values,sam0,refs0,out_var,nc_var)

DGM_bounds <- function(Ldata, Rdata,
                       values,
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
                       version = "second",
                       version_sel = "second",
                       alpha=0.05,
                       projections = FALSE,
                       R2bound=NULL,
                       values_sel=NULL,
                       ties = FALSE,
                       mult = NULL,
                       seed =  2131){


  #### to draw the profile of epsilon
  # if(!is.null(mult)){ ## multiplier
  #   Bsamp=0
  # }

  ######
  kp=eps_default
  Rs=0
  ## nb of obs min to use the conditioning.
  limit = 1
  # lim = limit
  #### get sizes
  dimXc = length(c_var)
  dimXnc = length(nc_var)
  q95 <-  function(x){quantile(x,1-alpha,na.rm=T)}
  q05 <-  function(x){quantile(x,alpha,na.rm=T)}

  q95_2 <-  function(x){quantile(x,1-alpha/2,na.rm=T)}
  q05_2 <-  function(x){quantile(x,alpha/2,na.rm=T)}

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

  if(!is.null(seed)){
    # seed1 = 2131
    seed0 = floor(seed/100)
  }
  # ks=rep(0,dim(sam0)[1],1)

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

    if(!is.null(seed)){
      set.seed(seed)
    }
    eps_default0 =  select_epsilon(sam1,eps_default,  Xc_x,Xnc,Xc_y,Y,
                                   values,dimXc,dimXnc, nb_pts,lim =limit ,
                                   weights_x,weights_y, refs0, grid, constraint, c_sign, nc_sign, meth=meth,
                                   nbCores=nbCores,  version_sel = version_sel, alpha=alpha ,ties =ties)

    if(!is.null(seed)){
      set.seed(NULL)
    }

  }else{

    if(meth=="min"){
      eps_default0 = eps_default
    }else{
      eps_default0 = matrix(eps_default,dim(sam0)[1],1)
    }
  }

  ####
  if(!is.null(mult)){ ## multiplier
    eps_default0 = eps_default0*mult
    eps_default0[eps_default0 >0.5] <- 0.5
  }
  #


  #################################################################################################################
  #################################################################################################################

  #### compute the radial function
  if(projections == FALSE){
    # limit = 100
    sample1 =NULL
    minsel="normal"
    ### compute point estimate
    type="both"

    mat_var_out1 <- compute_radial(sample1 =sample1,Xc_x,Xnc,Xc_y,Y,
                                   values,dimXc,dimXnc,
                                   nb_pts, sam0, eps_default0 ,grid,lim =limit,
                                   weights_x,weights_y, constraint,
                                   c_sign, nc_sign,refs0,type="both",meth=meth, version = version,
                                   R2bound,values_sel,ties,modeNA)

    if(!is.null(R2bound)){
      Rs <-  mat_var_out1$Rs
    }

    hull_point <-    mat_var_out1
    if(modeNA ==TRUE){
      no_inter =  hull_point [["upper"]] <  -hull_point[["lower"]]
      hull_point [["upper"]][no_inter] <- NA
      hull_point [["lower"]][no_inter] <- NA



    }

    ##### compute point estimate of betac if common regressors Xc ###################################################################################""
    if(!is.null(values)){

      beta1_pt <- compute_bnds_betac(sample1 =NULL, info0 = mat_var_out1, values,  constraint, c_sign0 = c_sign, nc_sign0=nc_sign ,  refs0,  c_var, nc_var,sam0,
                                     info1=NULL , constr=TRUE,R2bound,values_sel)

      mat_beta1_l = matrix(0,Bsamp,length(refs0))
      mat_beta1_u = matrix(0,Bsamp,length(refs0))
      hull_point[["betac_pt"]] <-  beta1_pt
    }



    # Bsamp=2000
    #### replications numerical bootstrap or subsampling ############################################################################################
    if(Bsamp >0){
      if(!is.null(seed)){
        set.seed(seed0)
      }

      if(nbCores>1){
        res0 <- sfLapply(1:Bsamp, compute_radial, Xc_x,Xnc,Xc_y,Y,values,dimXc,dimXnc,nb_pts,
                         sam0, eps_default0,grid,lim =limit ,
                         weights_x,   weights_y, constraint ,c_sign , nc_sign,refs0,type="both",meth=meth,
                         version = version, R2bound,values_sel,ties,modeNA  )

      }else{

        res0 <- lapply(1:Bsamp, compute_radial, Xc_x,Xnc,Xc_y,Y,
                       values,dimXc,dimXnc,nb_pts,sam0, eps_default0,grid,lim =limit ,
                       weights_x,   weights_y, constraint ,c_sign , nc_sign,refs0,type="both",meth=meth,
                       version = version, R2bound,values_sel,ties,modeNA  )
      }

      # compute_radial(1, Xc_x,Xnc,Xc_y,Y,
      # values,dimXc,dimXnc,nb_pts,sam0, eps_default0,grid,lim =limit ,
      # weights_x,   weights_y, constraint ,c_sign , nc_sign,refs0,type="both",meth=meth,
      # version = version, R2bound,values_sel,ties  )
      if(!is.null(seed)){
         set.seed(NULL)
      }


      mat_varb = matrix(0,Bsamp,dim(sam0)[1])
      mat_varb0 = matrix(0,Bsamp,dim(sam0)[1])
      mat_varb_unc = matrix(0,Bsamp,dim(sam0)[1])

      ##### handling the aggregate values if Xc ###############################
      if(dimXnc==1 & !is.null(values)){
        mat_varb_agg = matrix(0,Bsamp,dim(sam0)[1])
        mat_varb0_agg = matrix(0,Bsamp,dim(sam0)[1])
        mat_varb_unc_agg = matrix(0,Bsamp,dim(sam0)[1])
      }

      for(b in 1:Bsamp){
        mat_varb[b,] = res0[[b]][["upper"]]
        mat_varb0[b,] = res0[[b]][["lower"]]
        mat_varb_unc[b,] = res0[[b]][["unconstr"]]

        ##### handling the aggregate values if Xc ###############################
        if(dimXnc==1 && !is.null(values)){
          mat_varb_agg[b,] =  res0[[b]][["upper_agg"]]
          mat_varb0_agg[b,] = res0[[b]][["lower_agg"]]
          mat_varb_unc_agg[b,] =  res0[[b]][["unconstr_agg"]]
        }

      }
      n_x = dim(Xnc)[1]
      n_y = dim(Y)[1]

      T_xy = (n_y/(n_x+n_y))*n_x
      n_xy = min(n_x,n_y)
      bs = floor(sampling_rule(T_xy))+1
      bs0 = sqrt(bs/T_xy)

      #### upper bound without constraints
      mat_varb_out_unc  =   hull_point[["unconstr"]] - apply((mat_varb_unc -   matrix(1,dim(mat_varb_unc)[1],1)%*% hull_point[["unconstr"]])*bs0  ,2,q05)
      #### upper bound with constraints
      mat_var_out= hull_point[["upper"]] - apply((mat_varb -  matrix(1,dim(mat_varb)[1],1)%*% hull_point[["upper"]])*bs0,2,q05_2)

      ### lower bound
      ################ inf is stacked as -S, hence the minus. Otherwise, its  Sh + q_{1-a}(S - Sh)
      mat_var_in=  -( -hull_point[["lower"]] - apply((matrix(1,dim(mat_varb0)[1],1)%*%hull_point[["lower"]]-  mat_varb0  )*bs0,2,q95_2) )



      ######################################
      hull_sharp[["upper"]] <-  mat_var_out
      hull_sharp[["lower"]] <-  mat_var_in
      hull_sharp[["unconstr"]] <-  mat_varb_out_unc

      ## stock replications
      hull_sharp[["upper_repli"]] <-  mat_varb
      hull_sharp[["lower_repli"]] <-  mat_varb0
      hull_sharp[["unconstr_repli"]] <-  mat_varb_unc

      if(dimXnc==1 && !is.null(values)){
        #### aggregate upper bound without constraints
        mat_varb_out_unc_agg  =   hull_point[["unconstr_agg"]] - apply((mat_varb_unc_agg -   matrix(1,dim(mat_varb_unc_agg)[1],1)%*% hull_point[["unconstr_agg"]])*bs0  ,2,q05)
        #### aggregate upper bound with constraints
        mat_var_out_agg = hull_point[["upper_agg"]] - apply((mat_varb_agg -  matrix(1,dim(mat_varb_agg)[1],1)%*% hull_point[["upper_agg"]])*bs0,2,q05_2)

        ### aggregate lower bound
        #### inf is stacked as -S, hence the minus. Otherwise, its  Sh + q_{1-a}(S - Sh)
        mat_var_in_agg =  -( -hull_point[["lower_agg"]] - apply((matrix(1,dim(mat_varb0_agg)[1],1)%*%hull_point[["lower_agg"]]-  mat_varb0_agg)*bs0,2,q95_2) )

        hull_sharp[["upper_agg"]] <-  mat_var_out_agg
        hull_sharp[["lower_agg"]] <-  mat_var_in_agg
        hull_sharp[["unconstr_agg"]] <-  mat_varb_out_unc_agg
      }

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

        beta1_l_ci =  beta1_pt[,1] -   apply( (mat_beta1_l -  matrix(1,dim(mat_beta1_l)[1],1)%*% beta1_pt[,1])* bs0,2,q95_2)
        beta1_u_ci   = beta1_pt[,2] -   apply( (mat_beta1_u -  matrix(1,dim(mat_beta1_l)[1],1)%*% beta1_pt[,2])* bs0,2,q05_2)


        ######################### enforce the constraints on the CI #############################################################

        if(!is.null(constraint)){
          nbV = dim(values)[1]

          if(length(constraint)==1){
            if(length(values_sel$selected)< length(values_sel$old)){
              grouped0 = TRUE # for monotone, convex, do not consider 0.
            }else{
              grouped0 = FALSE
            }
          }else{
            if(dim(values_sel$selected)[1]< dim(values_sel$old)[1]){
              grouped0 = TRUE # for monotone, convex, do not consider 0.
            }else{
              grouped0 = FALSE
            }
          }
          # compute matrix R (put as a function)

          selected <- as.numeric(rownames(values_sel$selected))

          if(length(constraint)==1){ ### only 1 Xc
            cptR <- compute_constraints(constraint,values,values_sel,indexes_k=NULL,nbV, grouped0,ind=NULL,c_sign)
            R <- cptR$R
            pp0 <- cptR$pp0
            pp01 <- cptR$pp01
            if(is.null(dim(R))){
              indR  = 1
            }else{
              indR  = length(R[,1])
            }

            non_na_indexes=1
            R_all=vector("list")
            pp0_all=vector("list")
            R_all[[1]] <-R
            pp0_all[[1]] <- pp0
          }else{
            # get the indices of non null elements in constraints
            non_na_indexes <- (1:length(constraint))[!is.na(constraint)]
            # j=1

            grouped0 = FALSE
            R_all=vector("list")
            pp0_all=vector("list")
            indR = 0
            for(j in non_na_indexes){

              R=NULL
              pp0=NULL
              # for all them k
              # get the values of values-k
              values_selected_k <- as.matrix(values_sel$selected[,-c(j)])
              values_selected_k1 <- as.matrix(values_selected_k[!duplicated(values_selected_k),])
              #jj=1

              for(jj in 1:length(values_selected_k1[,1])){
                curr_k = values_selected_k1[jj,]
                indexes_k = matrix(1,dim(values_selected_k)[1],1)
                for(ddd in 1:dim(values_selected_k)[2]){
                  indexes_k =     indexes_k & (values_selected_k[,ddd]== curr_k[ddd])
                }

                # -> liste of valuesk on which we compute the constraints
                values_k <- values_sel$selected[ indexes_k,]
                nbV_k = dim( values_k)[1]
                if(  nbV_k>1){
                  cptR <- compute_constraints(constraint[j],values,values_sel,indexes_k,nbV, grouped0,ind=j,c_sign) # modify for c_sign
                  # R_k  <- cptR$R
                  # pp0_k <- ((1:length(values[,1]))[indexes_k])[as.matrix(cptR$pp0)]
                  # # dim(pp0_k )
                  R_k  <- cptR$R
                  pp0_k =NULL
                  for(ko in 1:dim(cptR$pp0)[1]){
                    pp0_k <- rbind(pp0_k,  ((1:length(values[,1]))[indexes_k])[as.matrix(cptR$pp0)[ko,]])
                  }
                  # length(pp0_k )
                  R = rbind(R,R_k)
                  pp0 = rbind(pp0,pp0_k)
                }
              }

              inna <- rowMeans(is.na(R)) >0
              R <- R[!inna,]
              pp0 <- pp0[!inna,]


              if(is.null(dim(R))){
                indR =1 + indR
              }else{
                indR = length(R[,1])+ indR
              }

              # R_k <- na.omit(R_k)
              R_all[[j]] <-R
              pp0_all[[j]] <- pp0
            }

          }

          indic =1
          # j =1
          for(j in non_na_indexes){
            constraint1 = constraint[j]
            R <- R_all[[j]]
            pp0 <- pp0_all[[j]]



            if(is.null(dim(R))){
              lR =1
            }else{
              lR= length(R[,1])
            }

            # k=1
            for(k in 1:lR){ ## for all the constraints on Xc


              if(constraint1=="convex" || constraint1=="concave"){





              }else if(constraint1 =="sign" || constraint1 =="IV" ){


                # beta1_l_ci =
                # beta1_u_ci =
                if(lR==1){
                  cc =  max(1,as.numeric(pp0[2]))
                  refR = R[as.numeric( cc)]
                }else{
                  cc =  max(1,as.numeric(pp0[k,2]))
                  refR = R[k,as.numeric(cc)]
                }

                if( refR==1){
                  beta1_l_ci[cc -1] = max( beta1_l_ci[cc -1],0)
                  beta1_u_ci[cc -1] = max( beta1_u_ci[cc -1],0)
                  beta1_pt[cc-1,1] = max(  beta1_pt[cc -1,1],0)
                  beta1_pt[cc -1,2] = max(  beta1_pt[cc -1,2],0)
                }else{
                  beta1_l_ci[cc -1] = min( beta1_l_ci[cc -1],0)
                  beta1_u_ci[cc -1] = min( beta1_u_ci[cc -1],0)
                  beta1_pt[cc -1,1] = min(  beta1_pt[cc -1,1],0)
                  beta1_pt[cc -1,2] = min(  beta1_pt[cc -1,2],0)
                }

              }else if(constraint1 =="nondecreasing" || constraint1 =="nonincreasing" ){

                # beta1_u_ci =
                if(lR==1){
                  cc =  max(1,as.numeric(pp0[2]))
                  refR = R[as.numeric( cc)]
                }else{
                  cc =  max(1,as.numeric(pp0[k,2]))
                  refR = R[k,as.numeric(cc)]
                }

                if( refR==1){
                  beta1_l_ci[cc -1] = max( beta1_l_ci[cc -1],0)
                  beta1_u_ci[cc -1] = max( beta1_u_ci[cc -1],0)
                  beta1_pt[cc-1,1] = max(  beta1_pt[cc -1,1],0)
                  beta1_pt[cc -1,2] = max(  beta1_pt[cc -1,2],0)
                }else{
                  beta1_l_ci[cc -1] = min( beta1_l_ci[cc -1],0)
                  beta1_u_ci[cc -1] = min( beta1_u_ci[cc -1],0)
                  beta1_pt[cc -1,1] = min(  beta1_pt[cc -1,1],0)
                  beta1_pt[cc -1,2] = min(  beta1_pt[cc -1,2],0)
                }



              }


              # indic= indic+1

            }
          }



        }


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

    minsel="normal"
    mat_var_out1 <- compute_support(sample1 =NULL,Xc_x,Xnc,Xc_y,Y,
                                    values,dimXc,dimXnc,
                                    nb_pts, sam0, eps_default0, grid,lim =limit,
                                    weights_x,weights_y, constraint,
                                    c_sign =c_sign, nc_sign,refs0,type="both",meth=meth, bc=TRUE,
                                    version = version, R2bound,values_sel,ties,modeNA )

    hull_point[["support"]] <-      mat_var_out1

    if(Bsamp!=0){
      # mat_var_out1
      Bsamp1 = Bsamp

      if(!is.null(seed)){
        set.seed(seed0)
      }
      # start_time <- Sys.time()
      if(nbCores>1){
        res0 <- sfLapply(1:Bsamp1, compute_support,Xc_x,Xnc,Xc_y,Y,values,dimXc,dimXnc,nb_pts,sam0, eps_default0,
                         grid,lim =limit ,
                         weights_x,   weights_y, constraint,
                         c_sign = c_sign, nc_sign,refs0,type="both",meth=meth,
                         bc=TRUE, version = version, R2bound,values_sel,ties,modeNA )


      }else{
        res0 <- lapply(1:Bsamp1, compute_support, Xc_x,Xnc,Xc_y,Y,values,dimXc,dimXnc,nb_pts,sam0, eps_default0,
                       grid,lim =limit ,
                       weights_x,   weights_y, constraint,
                       c_sign = c_sign, nc_sign,refs0,type="both",meth=meth,
                       bc=TRUE, version = version,R2bound,values_sel,ties,modeNA )
      }
      mat_varb = matrix(0,Bsamp1,dim(sam0)[1])
      for(b in 1:Bsamp1){
        mat_varb[b,] = res0[[b]][,3]
      }
      if(!is.null(seed)){
        set.seed(NULL)
      }
      n_x = dim(Xnc)[1]
      n_y = dim(Y)[1]
      #### subsampling
      T_xy=n_x*(n_y/(n_x+n_y))
      n_xy = min(n_x,n_y)

      bs = floor(sampling_rule(T_xy))+1
      bs0 = sqrt(bs/T_xy)
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
