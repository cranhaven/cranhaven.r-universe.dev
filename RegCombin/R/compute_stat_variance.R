#' Function to compute the Variance bounds on the noncommon regressor Xnc
#'
#' @param sample1 if NULL compute the point estimate, if a natural number then evaluate a bootstrap or subsampling replication.
#' @param X1_x the common regressor on the dataset  (Xnc,Xc). Default is NULL.
#' @param X2 the noncommon regressor on the dataset  (Xnc,Xc). No default.
#' @param X1_y the common regressor on the dataset  (Y,Xc). Default is NULL.
#' @param Y the outcome variable. No default.
#' @param values the different unique points of support of the common regressor Xc.
#' @param refs0 indicating the positions in the vector values corresponding to the components of betac.
#' @param dimX1 the dimension of the common regressors Xc.
#' @param dimX2 the dimension of the noncommon regressors Xnc.
#' @param nb_pts the constant C in DGM for the epsilon_0, the lower bound on the grid for epsilon, taken equal to nb_pts*ln(n)/n. Default is 1 without regressors Xc, 3 with Xc.
#' @param sam0 the directions q to compute the variance bounds on the radial function.
#' @param lim the limit number of observations under which we do no compute the conditional variance.
#' @param weights_x the sampling weights for the dataset (Xnc,Xc).
#' @param weights_y  the sampling weights for the dataset (Y,Xc).
#' @param constraint a vector indicating the different constraints in a vector of the size of X_c indicating the type of constraints, if any on f(X_c) : "concave", "concave", "nondecreasing", "nonincreasing", "nondecreasing_convex", "nondecreasing_concave",  "nonincreasing_convex", "nonincreasing_concave", or NULL for none. Default is NULL, no contraints at all.
#' @param c_sign sign restrictions on the commonly observed regressors: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
#' @param nc_sign sign restrictions on the non-commonly observed regressors Xnc: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
#' @param values_sel the selected values of Xc for the conditioning. Default is NULL.

#' @return a list containing:
#'
#'  - upper: the upper bound in the specified directions, possibly with sign constraints
#'
#'  - lower: the lower bound in the specified directions, possibly with sign constraints
#'
#'  - unconstr: the bounds without sign constraints in the specified directions
#'
#'  - Ykmean: the means of Y|Xc for the considered sample
#'
#'  - Xkmean: the means of Xnc|Xc for the considered sample
#'
#'  - DYk: the difference of means of Y|Xc =k -  Y|Xc =0 for the considered sample
#'
#'  - DXk: the difference of means of Xnc|Xc =k -  Xnc|Xc =0 for the considered sample
#'
#'  - tests: the pvalues of the tests H0 : DXk =0
#'
#'  - ratio_ref: the ratio R in the radial function computed for the initial sample
#'
#'
compute_stat_variance <- function(sample1 = NULL,X1_x,X2,X1_y,Y,values,
                            refs0,dimX1,dimX2,
                            nb_pts,sam0,
                            lim = 1,
                            weights_x = NULL,weights_y = NULL,
                            constraint=NULL,c_sign= NULL,nc_sign= NULL, values_sel=NULL){



  R2bound=NULL

  if( dimX1==0){
    X1_x = NULL
    # X2 =X2

    ### dataset 2
    X1_y = NULL
    # Y = Y

  }

  n_x = dim(X2)[1]
  n_y = dim(Y)[1]
  n_xy= min(n_x,n_y)
  T_xy= n_xy

  if(is.null(weights_x)){
    weights_x= rep(1/dim(X2)[1],dim(X2)[1])
  }
  if(is.null(weights_y)){
    weights_y= rep(1/length(Y),length(Y))
  }

  # save original weights
  weights_xs <-  weights_x
  weights_ys <-  weights_y


  dimXnc =   dimX2
  dimXc =   dimX1

  if(!is.null(sample1)){

    n_x = dim(X2)[1]
    n_y = dim(Y)[1]
    n_xy = min(n_x,n_y)
    T_xy  = (n_y/(n_x+n_y))*n_x

    bs = floor(sampling_rule(T_xy))

    bb = sample(1:n_x,bs, replace=FALSE)
    if(!is.null(X1_x)){
      Xc_xb = matrix(X1_x[bb,],bs,dimX1)
    }
    Xncb = matrix(X2[bb,],bs,dimX2)
    weights_x =  matrix(weights_x[bb],bs,1)
    weights_x = weights_x/sum(weights_x)
    n_x = dim(Xncb)[1]

    bby = sample(1:n_y,bs, replace=FALSE)
    if(!is.null(X1_y)){
      Xc_yb = matrix(X1_y[bby,],bs,dimX1)
    }
    Yb = matrix(Y[bby],bs,1)
    weights_y =  matrix(weights_y[bby],bs,1)
    weights_y = weights_y/sum(weights_y)
    n_y = dim(Yb)[1]

  }else{
    ## point estimate
    Xc_xb =X1_x
    Xncb = X2
    Xc_yb =   X1_y
    Yb = Y

  }


  if(!is.null(values)){### if there is a discrete common regressor X1
    # mat_var = NULL
    n_x_all = dim(Xc_xb)[1]
    n_y_all = dim(Xc_yb)[1]

    mat_Yk= matrix(NA,dim(values)[1],1)
    mat_Xk= matrix(NA,dim(values)[1],dimXnc)
    Dmat_Yk= matrix(NA,dim(values)[1],1)
    Dmat_Xk=matrix(NA,dim(values)[1],dimXnc)

    mat_var_unc= matrix(0,dim(values)[1],dim(sam0)[1])
    mat_var= matrix(0,dim(values)[1],dim(sam0)[1])
    mat_var_low= matrix(0,dim(values)[1],dim(sam0)[1])


    nbV = dim(values)[1]
    ##########
    ########## compute the shape constraint if any ###################
    ###########################
    # constraint= c("convex",NA)

    if(!is.null(constraint)){

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

      if(length(constraint)==1){
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


            values_k <- values_sel$selected[ indexes_k,]
            nbV_k = dim( values_k)[1]
            if(  nbV_k>1){
              cptR <- compute_constraints(constraint[j],values,values_sel,indexes_k,nbV, grouped0,ind=j,c_sign) # modify for c_sign
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

          # values_sel
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


      refsXcx = matrix(NA,dim(Xncb)[1],dim(values)[1])
      refsXcy = matrix(NA,dim(Yb)[1],dim(values)[1])
      for( k in 1:dim(values)[1]){
        if(dimXc==1){
          val =values[k,]
          sel_x = (Xc_xb==val)
          sel_y = (Xc_yb==val)
        }else{
          val = t(as.matrix(values[k,]))
          sel_x = matrix(1,dim(Xc_xb)[1],1)
          sel_y = matrix(1,dim(Xc_yb)[1],1)
          for(ddd in 1:dimXc){
            sel_x =  sel_x & (Xc_xb[,ddd]==val[ddd])
            sel_y =  sel_y & (Xc_yb[,ddd]==val[ddd])
          }
          sel_x = matrix( sel_x,dim(Xc_xb)[1],1)
          sel_y = matrix( sel_y,dim(Xc_yb)[1],1)
        }
        refsXcx[,k] <- sel_x/sum(sel_x*weights_x)
        refsXcy[,k] <- sel_y/sum(sel_y*weights_y)
      }


      ############################################################################################
      ################ conditional means & variance computation of the constraint ################
      s0=  (sam0%*%t(Xncb))


      mat_Yk= matrix(NA,indR,1)
      var_Yk= matrix(NA,indR,1)
      mat_Xk= matrix(NA,indR,dim(s0)[1])
      var_Xk= matrix(NA,indR,dim(s0)[1])
      # k=1
      indic =1
      # j=1
      for(j in non_na_indexes){
        constraint1 = constraint[j]
        R <- R_all[[j]]
        pp0 <- pp0_all[[j]]



        if(is.null(dim(R))){
          lR =1
        }else{
          lR= length(R[,1])
        }

        for(k in 1:lR){ ## for all the constraints on Xc
          # jj=1
          interx=matrix(0,dim(Xncb)[1],1)
          intery=matrix(0,dim(Yb)[1],1)

          if(constraint1=="convex" || constraint1=="concave"){

            if(lR==1){
              r =  R[as.numeric(pp0)]
            }else{
              r =  R[k,as.numeric(pp0[k,])]
            }



            for( jj in 1:3){
              if(lR==1){
                ref_p = pp0[jj]
              }else{
                ref_p = pp0[k,jj]
              }
              interx=  interx + refsXcx[,as.numeric(ref_p)]*r[jj]
              intery=  intery + refsXcy[,as.numeric(ref_p)]*r[jj]
            }

          }else if(constraint1 =="nondecreasing" || constraint1 =="nonincreasing" || constraint1 =="sign" || constraint1 =="IV"){
            if(lR==1){
              r =  R[as.numeric(pp0)]
            }else{
              r =  R[k,as.numeric(pp0[k,])]
            }

            for( jj in 1:2){
              if(lR==1){
                ref_p = pp0[jj]
              }else{
                ref_p = pp0[k,jj]
              }

              interx=  interx + refsXcx[,as.numeric(ref_p)]*r[jj]
              intery=  intery + refsXcy[,as.numeric(ref_p)]*r[jj]
            }

          }else if(constraint1 =="nondecreasing_convex" || constraint1 =="nondecreasing_concave" || constraint1 =="nonincreasing_convex" || constraint1 =="nonincreasing_concave"){

            if(k <= dim(pp0)[1]){

              r =  R[k,as.numeric(pp0[k,])]
              for( jj in 1:3){
                interx=  interx + refsXcx[,as.numeric(pp0[k,jj])]*r[jj]
                intery=  intery + refsXcy[,as.numeric(pp0[k,jj])]*r[jj]
              }
            }else{
              k0 = k -  dim(pp0)[1]
              r =  R[k,as.numeric(pp01[k0,])]
              for( jj in 1:2){
                interx=  interx + refsXcx[,as.numeric(pp01[ k0,jj])]*r[jj]
                intery=  intery + refsXcy[,as.numeric(pp01[ k0,jj])]*r[jj]
              }
            }
          }

          # if(dimXnc==1){
          inter1 = matrix(1,dim(s0)[1],1)%*%t(interx)
          inter1 = s0*inter1
          # }else{
          #   inter1 = s0*inter1
          # }


          mat_Xk[indic,] <- apply(inter1*(matrix(1,dim(s0)[1],1)%*%t(as.matrix(weights_x))),1,sum)
          var_Xk[indic,] <- apply((inter1 -  matrix(mat_Xk[indic,],dim(s0)[1],1)%*%matrix(1,1,dim(Xncb)[1]))^2*(matrix(1,dim(s0)[1],1)%*%t(as.matrix(weights_x))),1,sum)
          ## Rm_Y
          Ybarre = sum(intery*Yb*weights_y);
          mat_Yk[indic,1] <- Ybarre
          var_Yk[indic,1] <- sum(weights_y*(intery*Yb-Ybarre)^2)

          indic= indic+1


        }
      }

    }


    ind = NULL
    inds=0
    if(dimXc==1){
      val0 =values[1,]
      sel0_x =  (Xc_xb==val0)
      sel0_y =  (Xc_yb==val0)
    }else{
      val0 = t(as.matrix(values[1,]))
      sel0_x = matrix(1,dim(Xc_xb)[1],1)
      sel0_y = matrix(1,dim(Xc_yb)[1],1)
      for(ddd in 1:dimXc){
        sel0_x =  sel0_x & (Xc_xb[,ddd]==val0[ddd])
        sel0_y =  sel0_y & (Xc_yb[,ddd]==val0[ddd])
      }
      sel0_x = matrix( sel0_x,dim(Xc_xb)[1],1)
      sel0_y = matrix( sel0_y,dim(Xc_yb)[1],1)
    }

    weights_xp0 =  matrix(weights_x[sel0_x],sum(sel0_x),1)
    weights_xp0 = weights_xp0/sum(weights_xp0)
    weights_yp0 =  matrix(weights_y[sel0_y],sum(sel0_y),1)
    weights_yp0 = weights_yp0/sum(weights_yp0)
    Xp0 = matrix(Xncb[sel0_x,],sum(sel0_x),dimXnc)
    if(dimXnc>1){
      mat_X0 <- colSums(Xp0*(weights_xp0%*%matrix(1,1,dim(Xp0)[2])))
    }else{
      mat_X0 <- sum(Xp0*weights_xp0)
    }


    Yp0 = Yb[sel0_y]
    Y0 = sum(Yp0*weights_yp0)
    grid_I = vector("list")
    #### vector of matrices of point estimate ratios.
    T_n = matrix(NA,1,dim(values)[1])
    cond_w = matrix(NA,1,dim(values)[1])


    for(k in 1:dim(values)[1]){


      short=FALSE

      ###################################################### select the sample conditional on the values of Xc
      if(dimXc==1){
        val =values[k,]
        sel_x = (Xc_xb==val)
        sel_y = (Xc_yb==val)
      }else{
        val = t(as.matrix(values[k,]))
        sel_x = matrix(1,dim(Xc_xb)[1],1)
        sel_y = matrix(1,dim(Xc_yb)[1],1)
        for(ddd in 1:dimXc){
          sel_x =  sel_x & (Xc_xb[,ddd]==val[ddd])
          sel_y =  sel_y & (Xc_yb[,ddd]==val[ddd])
        }
        sel_x = matrix( sel_x,dim(Xc_xb)[1],1)
        sel_y = matrix( sel_y,dim(Xc_yb)[1],1)
      }
      Xp= matrix(Xncb[sel_x,],sum(sel_x),dimXnc);
      Yp = Yb[sel_y]
      weights_yp =   weights_y[sel_y]
      if(sum(weights_yp!=0)<=1){
        weights_yp=    weights_yp + 1/length( weights_yp)
        weights_yp = weights_yp /sum( weights_yp )
      }
      weights_yp  =  weights_yp /sum( weights_yp )
      weights_xp =   weights_x[sel_x]

      cond_w[1,k] <- sum(weights_xp)
      if(sum(weights_xp!=0)<=1){
        weights_xp=    weights_xp + 1/length( weights_xp)
        weights_xp = weights_xp /sum( weights_xp )
      }
      weights_xp  =  weights_xp /sum( weights_xp )
      n_x = sum(sel_x)
      n_y = sum(sel_y)
      T_xy1 = (n_y/(n_x+n_y))*n_x
      T_n[1,k] =  T_xy1

      ######################################################
      #####################################################

      Xp_s = Xp
      weights_xp_s = weights_xp
      weights_yp_s = weights_yp


      # if(is.null(grid) | is.null(eps_default0)){
        test0 = TRUE
      # }else{
      #   if(meth=="adapt"){
      #     test0 = sum(is.na(eps_default0[,k]))==0
      #   }else{
      #     test0 = sum(is.na(eps_default0[k]))==0
      #   }
      # }

      if(dimXnc>1){
        mat_Xp <- colSums( Xp*(weights_xp%*%matrix(1,1,dim(Xp)[2])))
      }else{
        mat_Xp <- sum( Xp*weights_xp)
      }
      # Xk[k,]<-  mat_Xp

      Dmat_Xk[k,] <- mat_Xp- mat_X0

      Ybarre =  sum(Yp*weights_yp)
      Dmat_Yk[k,1] <- Ybarre- Y0
      # Yk[k] =   Ybarre

      ##########  If checks passed, then compute the stat S_{eps} | Xc ##################################################################
      if(sum(sel_x)> lim & sum(sel_y) >  lim & test0){

        # lim=5
        Xp= matrix(Xncb[sel_x,],sum(sel_x),dimXnc);
        Yp = Yb[sel_y] #- mean(Yb[sel_y]);
        weights_yp =   weights_y[sel_y]
        # if(sum(sel_y)< 20){
        if(sum(weights_yp!=0)<=1){
          weights_yp=    weights_yp + 1/length( weights_yp)
          weights_yp = weights_yp /sum( weights_yp )
        }
        # }
        weights_yp  =  weights_yp /sum( weights_yp )
        weights_xp =   weights_x[sel_x]
        # if(sum(sel_x)< 20){
        if(sum(weights_xp!=0)<=1){
          weights_xp=    weights_xp + 1/length( weights_xp)
          weights_xp = weights_xp /sum( weights_xp )
        }
        # }
        weights_xp  =  weights_xp /sum( weights_xp )
        n_x = sum(sel_x)
        n_y = sum(sel_y)
        n_xy = min(n_x,n_y)
        T_xy=   n_xy
        T_n[1,k] =  T_xy
        sam1 = cbind(1:dim(sam0)[1],sam0)

        bsharp_beta2 <- na.omit(t(apply(sam1,1,compute_ratio_variance,Xp=Xp,Yp=Yp,dimX2=dimXnc,
                                          weights_xp ,weights_yp )))

        # mat_Yk[k,1] <- sum(Yp*weights_yp)
        # mat_Xk[k,] <- sum(Xp*weights_xp)

        # if(dimX2==1){
        mat_var[k,] <- bsharp_beta2
        mat_var_unc[k,] <-mat_var[k,]
        mat_var_low[k,] <- rev(mat_var[k,])
        bsharp_beta2_m  = rev(bsharp_beta2)

        EU = FALSE
        EL=  FALSE

        Ybarre =  sum(Yp*weights_yp)
        Dmat_Yk[k,1] <- Ybarre- Y0
        if(dimXnc>1){
          mat_Xp <- colSums( Xp*(weights_xp%*%matrix(1,1,dim(Xp)[2])))
        }else{
          mat_Xp <- sum( Xp*weights_xp)
        }
        Dmat_Xk[k,] <- mat_Xp- mat_X0


        if(is.null(sample1)){
          ind = c(ind,k)
        }else{
          if(sum(mat_var_unc[k,]==0)==0){
            ## add signal if not, raise
            ind = c(ind,k)
          }
        }



        ind = c(ind,k)
        mat_var[k,] <- bsharp_beta2

      }
    }

    mat_var_unc1<- mat_var_unc[ind ,]
    mat_var_unc1 <- matrix(mat_var_unc1,length(ind),dim( mat_var_unc)[2])
    mat_var_low1 <-   mat_var_low[ind,]
    mat_var_low1 <- matrix(mat_var_low1,length(ind),dim( mat_var_low)[2])
    mat_var_low <- apply(  mat_var_low1,2,min)
    mat_var_unc <- apply(  mat_var_unc1,2,min)
    mat_var = mat_var_unc

    ###############################################################################################

    if(!is.null(constraint)){

      nb_eff =  dim(Xncb)[1] #floor(cond_w*dim(Xnc)[1])

      # jj=1
      # if(!is.null(eps_default0)){

        for(jj in 1:dim(sam0)[1]){

          u_n=1e-07
          den = mat_Xk[,jj] +  sign(mat_Xk[,jj])*u_n^2
          ratios = (mat_Yk + u_n)/   den

          for(kk in 1:length(ratios)){

            if(!is.na(mat_Xk[kk,jj] )){
              ## possibly modify the upper bound
              if( mat_Xk[kk,jj] >=0){
                ratios_plus <- ratios[kk]
                cond = (min(mat_var[jj],ratios_plus) >= - mat_var_low[jj])
                cond[is.na(cond)] = FALSE
                if( cond ){
                  mat_var[jj] <- min(  mat_var[jj] ,   ratios_plus )
                }
              }

              ## possibly modify the lower bound
              if( mat_Xk[kk,jj] <=0){
                ratios_minus <- ratios[kk]
                cond = (- min(mat_var_low[jj] , - ratios_minus) <=  mat_var[jj])
                cond[is.na(cond)] = FALSE
                if( cond  ){
                  mat_var_low[jj] <- min(   mat_var_low[jj]  , -  ratios_minus)
                }
              }
            }
          }
        }
      }


    # }



    if(!is.null(nc_sign)){
      ee = eye(dimXnc)
      for(jj in 1:dimXnc){
        if(nc_sign[jj]!=0){
          cond = (matrix(ee[jj,]*nc_sign[jj],1,dimXnc)%*%t(sam0))<0
          if(mat_var[!cond]>0 ){# existe une intersection
            mat_var[cond]<- pmin(mat_var[cond],0)
            mat_var_low[!cond]<- - pmax(-mat_var_low[!cond],0)
          }else{
            if(mat_var[!cond]>0){ # existe une intersection
              mat_var1 = mat_var
              mat_var_low1 = mat_var_low
              mat_var[!cond]<- pmax(0, mat_var[!cond], - mat_var_low[!cond])
              # mat_var[!cond]<- pmax(0, mat_var[!cond])
              mat_var_low[!cond]<- - pmax(0, - mat_var_low[!cond])
              mat_var[cond]<-    mat_var_low[!cond]
              mat_var_low[cond]<-  mat_var[!cond]
            }else{
              if(is.null(sample1)){ # si le point estimate, renvoit NA
                mat_var =matrix(NA,dim(mat_var)[1],dim(mat_var)[2])
                mat_var_low =matrix(NA,dim(mat_var_low)[1],dim(mat_var_low)[2])
              }else{  # si subsmapling, met un estimate ? 0
                mat_var =0*mat_var
                mat_var_low =0* mat_var_low
              }
            }
          }
        }
      }
    }

    # mat_var_low2 <-  mat_var_low
    # mat_var2 <- mat_var


  }else{

    mat_var= matrix(0,1,dim(sam0)[1])
    mat_var_unc = matrix(0,1,dim(sam0)[1])
    Xp= Xncb
    Yp = Yb
    weights_yp =   weights_y/sum( weights_y)
    weights_xp =   weights_x/sum( weights_x )

    n_x = dim(  Xp)[1]
    n_y = dim(Yp)[1]
    n_xy = min(n_x,n_y)

    sam1 = cbind(1:dim(sam0)[1],sam0)

      bsharp_beta2 <- na.omit(t(apply(sam1,1,compute_ratio_variance,Xp=Xp,Yp=Yp,dimX2=dimXnc,
                                      weights_xp ,weights_yp)))

    mat_var<- bsharp_beta2
    mat_var_unc <- bsharp_beta2
    mat_var_low <- rev( mat_var)

  }


  output <- vector("list")
  # if(dimX1==0){
  #   mat_var_unc <- mat_var
  # }

  output[["upper"]] <- mat_var
  output[["unconstr"]] <-mat_var_unc
  output[["lower"]] <- mat_var_low
  if(!is.null(values)){
    output[["Ykmean"]] <- mat_Yk
    output[["Xkmean"]] <- mat_Xk
    output[["DYk"]] <- Dmat_Yk
    output[["DXk"]] <- Dmat_Xk
    # output[["tests"]] <- tests
    output[["T_n"]] <- T_n
  }

  return(output)

}
