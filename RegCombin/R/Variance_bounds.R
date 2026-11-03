#' Function to compute the variance bounds for Xnc
#'
#' @param Ldata dataset containing (Y,Xc) where Y is the outcome, Xc are potential common regressors
#' @param Rdata dataset containing (Xnc,Xc) where Xnc are the non commonly observed regressors, Xc are potential common regressors
#' @param out_var label of the outcome variable Y.
#' @param c_var label of the commonly observed regressors Xc.
#' @param nc_var label of the non commonly observed regressors Xnc.
#' @param constraint vector of the size of X_c indicating the type of constraint if any on f(X_c) : "monotone", "convex", "sign", or "none". Default is NULL, no contraints at all.
#' @param c_sign sign restrictions on the commonly observed regressors: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
#' @param nc_sign sign restrictions on the non-commonly observed regressors Xnc: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
#' @param projections if FALSE compute the identified set along some directions or the confidence regions. Default is FALSE.
#' @param values the different unique points of support of the common regressor Xc.
#' @param sam0 the directions q to compute the radial function.
#' @param refs0 indicating the positions in the vector values corresponding to the components of betac.
#' @param nb_pts the constant C in DGM for the epsilon_0, the lower bound on the grid for epsilon, taken equal to nb_pts*ln(n)/n. Default is 1 without regressors Xc, 3 with Xc.
#' @param eps_default If data_k =NULL, then epsilon is taken equal to eps_default.
#' @param nbCores number of cores for the parallel computation. Default is 1.
#' @param Bsamp the number of bootstrap/subsampling replications. Default is 1000.
#' @param weights_x the sampling weights for the dataset (Xnc,Xc).
#' @param weights_y  the sampling weights for the dataset (Y,Xc).
#' @param outside if TRUE indicates that the parallel computing has been launched outside of the function. Default is FALSE.
#' @param alpha for the level of the confidence region. Default is 0.05.
#' @param values_sel the selected values of Xc for the conditioning. Default is NULL.
#' @param seed set a seed to fix the subsampling replications
#'
#' @return a list containing, in order:
#'     - ci : a list with all the information on the confidence intervals
#'
#'     - upper: upper bound of the confidence interval on betanc at level alpha, possibly with sign constraints
#'
#'     - lower: lower bound upper bound of the confidence interval on betanc, possibly with sign constraints
#'
#'     - unconstr: confidence interval on betanc, without sign constraints
#'
#'     - betac_ci: confidence intervals on each coefficients related to the common regressor, possibly with sign constraints
#'
#'     - betac_ci_unc: confidence intervals on each coefficients related to the common regressor without sign constraints
#'
#'  - point : a list with all the information on the point estimates
#'
#'      - upper: the upper bounds on betanc, possibly with sign constraints
#'
#'      - lower: the lower bounds on betanc, possibly with sign constraints
#'
#'      -unconstr: bounds on betanc without sign constraints
#'
#'      -betac_pt: bounds on betanc, possibly with sign constraints
#'
#'      -betac_pt_unc: bounds on betanc without sign constraints
#'

Variance_bounds <- function(Ldata,Rdata,
                            out_var, c_var, nc_var, constraint =NULL,
                            c_sign=NULL, nc_sign=NULL,
                            projections=TRUE,
                            values, sam0, refs0,nb_pts,eps_default, nbCores,Bsamp=2000,
                            weights_x = NULL,weights_y = NULL, outside=FALSE,alpha=0.05,
                            values_sel=NULL,seed =21){


  #### get sizes
  dimXc = length(c_var)
  dimXnc = length(nc_var)

  if( dimXc!=0){
    ### dataset 1
    Xc_x = as.matrix(Rdata[,c_var],dim(Rdata)[1],dimXc)
    Xnc = as.matrix(Rdata[,nc_var],dim(Rdata)[1],dimXnc)

    ### dataset 2
    Xc_y = as.matrix(Ldata[,c_var],dim(Ldata)[1],dimXc)
    Y = as.matrix(Ldata[,out_var],dim(Ldata)[1],1)
  }else{

    Xc_x = NULL
    Xnc =  as.matrix(Rdata[,nc_var],dim(Rdata)[1],dimXnc)

    ### dataset 2
    Xc_y = NULL
    Y = as.matrix(Ldata[,out_var],dim(Ldata)[1],1)

  }

  # n_x = dim( Xc_x)[1]
  # n_y = dim( Xc_y)[1]
  # n_xy = min(n_x,n_y)
  # T_xy =   n_xy

  output  <- vector("list")
  hull_point=NULL
  hull_sharp=NULL

  #####################################################################
  if(projections==FALSE){


    if(dim(sam0)[1]==1){
      sam1 = rbind(sam0,sam0)
    }else{
      sam1= sam0
    }

    if(outside ==FALSE & nbCores >1){
      ### subsampling (B samples) parallel nbCores=4
      sfInit(parallel = TRUE, cpus = nbCores, type = "SOCK")
      # sfExportAll( except = list_ex)

      sfExport( "Xc_x","Xnc", "Xc_y" ,"Y",
                "values", "sam0","refs0","sam1",
                "out_var",  "nc_var", "c_var", "constraint",
                "nc_sign", "c_sign",
                "nbCores",
                "eps_default", "nb_pts","Bsamp",
                "weights_x","weights_y","outside",
                "alpha" , "projections",
                "dimXc","dimXnc")

      # sfExport('wtd.var','Norm','ceil','eye')
      # sfLibrary(R.matlab)
      # sfLibrary(pracma)
      # sfLibrary(Hmisc)
    }


    mat_var_out1<- compute_stat_variance(sample1 =NULL, Xc_x,Xnc,Xc_y,Y,values, refs0, dimXc, dimXnc,nb_pts,sam1,lim =1,
                                      weights_x = weights_x,weights_y = weights_y,constraint=constraint,c_sign=c_sign,nc_sign=nc_sign,
                                      values_sel=values_sel)
    # hull_point <- mat_var_out1

    hull_point <-    mat_var_out1
    # if(modeNA ==TRUE){
    #   no_inter =  hull_point [["upper"]] <  hull_point[["lower"]]
    #   hull_point [["upper"]][no_inter] <- NA
    #   hull_point [["lower"]][no_inter] <- NA
    #
    #   # if(dimXnc>1 & length(c_sign)>0){
    #   #   if(sum(abs(c_sign))>0){
    #   #     no_inter = apply(matrix((hull_point [["tests"]][,-c(1)]< 10^(-5)), dim(hull_point [["tests"]])[1],  dim(hull_point [["tests"]])[2]-1),1,prod)*TRUE
    #   #     hull_point [["upper"]][!no_inter] <- NA
    #   #     hull_point [["lower"]][!no_inter] <- NA
    #   #   }
    #   # }
    # }

    # if(length(c_sign)>0){
    #   if(sum(abs(c_sign))>0){
    #     effective_c_sign = c_sign
    #     for(j in 1:length(c_sign)){
    #       tests=matrix(hull_point$tests[,-c(1)],dim(sam0)[1],dim(values)[1]-1)
    #       if(dimXc >1){
    #         if(prod(!is.na(tests[,j])) ==0){
    #           effective_c_sign[j] = 0
    #         }else{
    #           if( prod(tests[,j]< 10^(-2))==0 & c_sign[j]==1 ){
    #             effective_c_sign[j] = 0
    #           }
    #         }
    #       }else{
    #         if(prod(!is.na(tests[j])) ==0){
    #           effective_c_sign[j] = 0
    #         }else{
    #           if( prod(tests[j]< 10^(-2))==0 & c_sign[j]==1 ){
    #             effective_c_sign[j] = 0
    #           }
    #         }
    #       }
    #     }
    #     hull_point [["effective_c_sign"]] <-  effective_c_sign
    #     c_sign =  effective_c_sign
    #   }
    # }
    ##### compute point estimate of betac if common regressors Xc ###################################################################################""
    if(!is.null(values)){

      beta1_pt <- compute_bnds_betac(sample1 =NULL, info0 = mat_var_out1, values,  constraint, c_sign0 = c_sign, nc_sign0=nc_sign ,  refs0,  c_var, nc_var,sam0,
                                     info1=NULL , constr=TRUE,R2bound=NULL,values_sel)


      mat_beta1_l = matrix(0,Bsamp,length(refs0))
      mat_beta1_u = matrix(0,Bsamp,length(refs0))
      hull_point[["betac_pt"]] <-  beta1_pt
    }



    if(Bsamp>0){
      set.seed(seed)
      ##### for subsampling
      if(nbCores>1){
        res0 <- sfLapply(1:Bsamp, compute_stat_variance,X1_x=Xc_x,X2=Xnc,X1_y=Xc_y,Y,values, refs0,dimXc,
                         dimXnc,nb_pts,sam1,lim =1,weights_x = weights_x,weights_y = weights_y,constraint = constraint, c_sign=c_sign,nc_sign=nc_sign,
                         values_sel=values_sel)

      }else{
        res0 <- lapply(1:Bsamp, compute_stat_variance,X1_x=Xc_x,X2=Xnc,X1_y=Xc_y,Y,values, refs0,dimXc,
                       dimXnc,nb_pts,sam0=sam1,lim =1,weights_x = weights_x,weights_y= weights_y,constraint = constraint,c_sign=c_sign,nc_sign=nc_sign,
                       values_sel=values_sel)


      }
      #
      set.seed(NULL)
      mat_varb = matrix(0,Bsamp,dim(sam0)[1])
      mat_varb0 = matrix(0,Bsamp,dim(sam0)[1])
      mat_varb_unc = matrix(0,Bsamp,dim(sam0)[1])


      for(b in 1:Bsamp){
        mat_varb[b,] = res0[[b]][["upper"]]
        mat_varb0[b,] = res0[[b]][["lower"]]
        mat_varb_unc[b,] = res0[[b]][["unconstr"]]
      }


      q95 <-  function(x){quantile(x,1-alpha,na.rm=T)}
      q05 <-  function(x){quantile(x,alpha,na.rm=T)}
      q95_2 <-  function(x){quantile(x,1-alpha/2,na.rm=T)}
      q05_2 <-  function(x){quantile(x,alpha/2,na.rm=T)}

      n_x = dim(Xnc)[1]
      n_y = dim(Y)[1]


      T_xy = (n_y/(n_x+n_y))*n_x
      n_xy = min(n_x,n_y)
      bs = ceil(sampling_rule(T_xy))
      bs0 = sqrt(bs/T_xy)

      #### upper bound without constraints
      mat_varb_out_unc  =   hull_point[["unconstr"]] - apply((mat_varb_unc -   matrix(1,dim(mat_varb_unc)[1],1)%*% hull_point[["unconstr"]])*bs0  ,2,q05)
      #### upper bound with constraints
      mat_var_out= hull_point[["upper"]] - apply((mat_varb -  matrix(1,dim(mat_varb)[1],1)%*% hull_point[["upper"]])*bs0,2,q05_2)

      ### lower bound
      ################ inf is stacked as -S, hence the minus. Otherwise, its  Sh + q_{1-a}(S - Sh)
      mat_var_in=  -( -hull_point[["lower"]] - apply((matrix(1,dim(mat_varb0)[1],1)%*%hull_point[["lower"]]-  mat_varb0  )*bs0,2,q95_2) )


      ##
      hull_sharp[["upper"]] <-  mat_var_out
      hull_sharp[["lower"]] <-  mat_var_in
      hull_sharp[["unconstr"]] <-  mat_varb_out_unc

      ## stock replications
      hull_sharp[["upper_repli"]] <-  mat_varb
      hull_sharp[["lower_repli"]] <-  mat_varb0
      hull_sharp[["unconstr_repli"]] <-  mat_varb_unc

      #### compute the projections for beta_c
        ### with sign constraints #############################################################
        if(!is.null(values)){


            ### with sign constraints ######################################################################
            if(nbCores>1){
              res0b <- sfLapply(1:Bsamp,compute_bnds_betac, info0 = res0, values, constraint,  c_sign0 = c_sign, nc_sign0=nc_sign ,
                                refs0, c_var, nc_var,sam0, info1=mat_var_out1, constr=TRUE,R2bound=NULL,values_sel)
            }else{
              res0b <- lapply(1:Bsamp,compute_bnds_betac, info0 = res0, values,  constraint,   c_sign0 = c_sign, nc_sign0=nc_sign ,
                              refs0, c_var, nc_var,sam0, info1=mat_var_out1, constr=TRUE,R2bound=NULL,values_sel)
            }

            for(b in 1:Bsamp){
              mat_beta1_l[b,] = res0b[[b]][,1]
              mat_beta1_u[b,] = res0b[[b]][,2]
            }

            beta1_l_ci =  beta1_pt[,1] -   apply( (mat_beta1_l -  matrix(1,dim(mat_beta1_l)[1],1)%*% beta1_pt[,1])* bs0,2,q95_2)
            beta1_u_ci   = beta1_pt[,2] -   apply( (mat_beta1_u -  matrix(1,dim(mat_beta1_l)[1],1)%*% beta1_pt[,2])* bs0,2,q05_2)



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

            ######################### enforce the constraints on the CI #############################################################
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



      # if(modeNA ==TRUE){
      #   no_inter =   hull_sharp[["upper"]] <  hull_sharp[["lower"]]
      #   hull_sharp[["upper"]][no_inter] <- NA
      #   hull_sharp[["lower"]][no_inter] <- NA
      # }

    }else{
      hull_sharp <- matrix(NA,1,1)
    }

    if(outside ==FALSE){
      sfStop()
    }

    ########################################### dim(X) >1, compute the convex Hull using sampled or fixed directions ########################################################################""
  }else{


    if(outside ==FALSE){
      sfInit(parallel = TRUE, cpus = nbCores, type = "SOCK")
      sfExportAll()
      # sfExport('wtd.var','Norm','ceil','eye')
      # sfLibrary(R.matlab)
      # sfLibrary(pracma)
      # sfLibrary(Hmisc)
    }


    sam = NULL
    for(k in 1:dimXnc){
      sam = cbind(sam,runif(nb_pts,-10,10))
    }
    sam1 <- t(apply(sam,1,Norm))


    ### compute point estimate
    mat_var0<-  compute_stat_variance(sample1 =NULL, Xc_x,Xnc,Xc_y,Y,values, refs0,dimXc,dimXnc,nb_pts,sam1,weights_x = weights_x,weights_y = weights_y,
                                      values_sel=values_sel)
    mat_var= mat_var0[["upper"]]*sam1
    pp <-  convhulln(mat_var, options = "Tv", output.options = "FA",return.non.triangulated.facets = FALSE)
    p0 = NULL
    for( j in 1: dimXnc){
      p0  <- cbind(p0,pp$p[pp$hull[,j],j])
    }

    hull_point <-   matrix(0,1,dim(sam0)[1])

    for(kk in 1:dim(sam0)[1]){
      hull_point [1,kk] <- max(sam0[kk,]%*%t(p0))
    }

    ##### bootstrap or subsampling
    res0 <- sfLapply(1:Bsamp, compute_stat_variance,Xc_x,Xnc,Xc_y,Y,values, refs0,dimXc,dimXnc,nb_pts,sam1,weights_x = weights_x,weights_y = weights_y,
                     values_sel=values_sel)
    mat_varb = matrix(0,Bsamp,dim(sam1)[1])
    for(b in 1:Bsamp){
      mat_varb[b,] = res0[[b]][[1]]
    }

    q95 <-  function(x){quantile(x,0.95,na.rm=T)}
    mat_var_out =   apply(mat_varb,2,q95)
    mat_var = sam1* mat_var_out

    pp <-  convhulln(mat_var, options = "Tv", output.options = "FA",return.non.triangulated.facets = FALSE)
    p1 = NULL
    for( j in 1: dimXnc){
      p1  <- cbind(p1,pp$p[pp$hull[,j],j])
    }
    # compute the bounds taking quantiles
    hull_sharp  <-   matrix(0,1,dim(sam0)[1])

    for(kk in 1:dim(sam0)[1]){
      hull_sharp  [1,kk] <- max(sam0[kk,]%*%t(p1))
    }

    if(outside ==FALSE){
      sfStop()
    }


  } #################################################################### end of if dimXnc ==1 or >1

  output[["ci"]] <- hull_sharp
  output[["point"]] <- hull_point


  return(output)
}
