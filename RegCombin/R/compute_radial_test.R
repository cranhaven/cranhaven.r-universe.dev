#' Function to compute the DGM bounds on the noncommon regressor Xnc,  adapted to the point identification test.
#'
#'
#' @param sample1  if NULL compute the point estimate, if a natural number then evaluate a bootstrap or subsampling replication.
#' @param Xc_x the common regressor on the dataset  (Xnc,Xc). Default is NULL.
#' @param Xnc the noncommon regressor on the dataset  (Xnc,Xc). No default.
#' @param Xc_y the common regressor on the dataset  (Y,Xc). Default is NULL.
#' @param Y  the outcome variable. No default.
#' @param values  the different unique points of support of the common regressor Xc.
#' @param dimXc the dimension of the common regressors Xc.
#' @param dimXnc the dimension of the noncommon regressors Xnc.
#' @param nb_pts the constant C in DGM for the epsilon_0, the lower bound on the grid for epsilon, taken equal to nb_pts*ln(n)/n. Default is 1 without regressors Xc, 3 with Xc.
#' @param sam0 the directions q to compute the variance bounds on the radial function.
#' @param eps_default0 the matrix containing the directions q and the selected epsilon(q).
#' @param grid the number of points for the grid search on epsilon. Default is 30. If NULL, then epsilon is taken fixed equal to kp.
#' @param lim the limit number of observations under which we do no compute the conditional variance.
#' @param weights_x the sampling weights for the dataset (Xnc,Xc).
#' @param weights_y  the sampling weights for the dataset (Y,Xc).
#' @param constraint a vector indicating the different constraints in a vector of the size of X_c indicating the type of constraints, if any on f(X_c) : "concave", "concave", "nondecreasing", "nonincreasing", "nondecreasing_convex", "nondecreasing_concave",  "nonincreasing_convex", "nonincreasing_concave", or NULL for none. Default is NULL, no contraints at all.
#' @param c_sign sign restrictions on the commonly observed regressors: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no contraints.
#' @param nc_sign sign restrictions on the non-commonly observed regressors Xnc: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no contraints.
#' @param refs0  indicating the positions in the vector values corresponding to the components of betac.
#' @param type equal to "both", "up", or "low".
#' @param meth the method for the choice of epsilon, either "adapt", i.e. adapted to the direction or "min" the minimum over the directions. Default is "adapt".
#' @param version version of the computation of the ratio, "first" indicates no weights, no ties, same sizes of the two datasets; "second" otherwise. Default is "second".
#' @param R2bound the lower bound on the R2 of the long regression if any. Default is NULL.
#' @param values_sel the selected values of Xc for the conditioning. Default is NULL.
#' @param ties Boolean indicating if there are ties in the dataset. Default is FALSE.
#'
#'
#' @return  a list contaning:
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
#'
compute_radial_test <- function(sample1 = NULL,Xc_x,Xnc,Xc_y,Y,values,
                                dimXc,dimXnc,nb_pts,
                                sam0,eps_default0,grid = NULL,
                                lim = 10,weights_x = NULL,weights_y = NULL,
                                constraint =NULL,
                                c_sign = NULL, nc_sign= NULL,
                                refs0=NULL,type="both",meth="adapt",
                                version = "first", R2bound=NULL,
                                values_sel = NULL,
                                ties = FALSE ){
  winsor= FALSE
  Rs = 0


  # sample1 = NULL
  if(is.null(eps_default0)){
    learn_eps = TRUE
  }else{
    learn_eps = FALSE
  }

  mat_var_low= NULL
  if(is.null(weights_x)){
    weights_x= rep(1/dim(Xnc)[1],dim(Xnc)[1])
  }
  if(is.null(weights_y)){
    weights_y= rep(1/length(Y),length(Y))
  }

  # save original weights
  weights_xs <-  weights_x
  weights_ys <-  weights_y

  # mn =0.5
  if(!is.null(sample1)){

    n_x = dim(Xnc)[1]
    n_y = dim(Y)[1]
    T_xy = n_x*(n_y/(n_x+n_y))

    bs = floor(sampling_rule(T_xy))

    # if(version !="first"){
    #
    #   ##
    #   # n_x = dim(Xnc)[1]
    #   bb = sample(1:n_x,n_x-bs, replace=FALSE)
    #   # bb = 3:4
    #   if(!is.null(Xc_x)){
    #     Xc_xb = matrix(Xc_x,n_x,dimXc)
    #   }
    #   Xncb = matrix(Xnc,n_x,dimXnc)
    #   weights_x =  matrix(weights_x,n_x,1)
    #   weights_x[bb] <-0
    #   weights_x = weights_x/sum(weights_x)
    #
    #   # n_y = dim(Y)[1]
    #   bby = sample(1:n_y,n_y-bs, replace=FALSE)
    #   # bby=1:2
    #   if(!is.null(Xc_y)){
    #     Xc_yb = matrix(Xc_y,n_y,dimXc)
    #   }
    #   Yb = matrix(Y,n_y,1)
    #   weights_y =  matrix(weights_y,n_y,1)
    #   weights_y[bby] <-0
    #   weights_y = weights_y/sum(weights_y)
    #
    # }else{
    #
    #
    # n_x = dim(Xnc)[1]
    bb = sample(1:n_x,bs, replace=FALSE)
    if(!is.null(Xc_x)){
      Xc_xb = matrix(Xc_x[bb,],bs,dimXc)
    }
    Xncb = matrix(Xnc[bb,],bs,dimXnc)
    weights_x =  matrix(weights_x[bb],bs,1)
    # weights_x[bb] <-0
    weights_x = weights_x/sum(weights_x)
    n_x = dim(Xncb)[1]


    # n_y = dim(Y)[1]
    # bby = sample(1:n_y,bs, replace=FALSE)
    #  bby = 3:n_y
    if(!is.null(Xc_y)){
      Xc_yb = matrix(Xc_y[bb,],bs,dimXc)
    }
    Yb = matrix(Y[bb],bs,1)
    weights_y =  matrix(weights_y[bb],bs,1)
    # weights_y[bby] <-0
    weights_y = weights_y/sum(weights_y)
    n_y = dim(Yb)[1]


    # n_x = dim(Xnc)[1]
    # bb = sample(1:n_x,bs, replace=FALSE)
    # if(!is.null(Xc_x)){
    #   Xc_xb = matrix(Xc_x[bb,],bs,dimXc)
    # }
    # Xncb = matrix(Xnc[bb,],bs,dimXnc)
    # weights_x =  matrix(weights_x[bb],bs,1)
    # # weights_x[bb] <-0
    # weights_x = weights_x/sum(weights_x)
    # n_x = dim(Xncb)[1]
    #
    #
    # n_y = dim(Y)[1]
    # bby = sample(1:n_y,bs, replace=FALSE)
    # if(!is.null(Xc_y)){
    #   Xc_yb = matrix(Xc_y[bby,],bs,dimXc)
    # }
    # Yb = matrix(Y[bby],bs,1)
    # weights_y =  matrix(weights_y[bby],bs,1)
    # # weights_y[bby] <-0
    # weights_y = weights_y/sum(weights_y)
    # n_y = dim(Yb)[1]


    if(!is.null(eps_default0)){
      ### computation of beta_hat in OLS
      ###
      # form = paste0(out_var, " ~ ")
      # form1 = paste0(nc_var,collapse = "+")
      # # form2 = paste0(c_var,collapse = "+")
      # form = as.formula(paste0(form,form1))

      # validation_rep = data.frame(cbind(Yb,Xncb))
      # colnames(validation_rep ) <- c("Y","X")
      # fit <- lm(as.formula("Y ~ X"), data = validation_rep, weights=weights_x)
      # betahat= fit$coefficients["X"]

      if(dimXc>0){
        validation_rep = data.frame(cbind(Yb,Xncb,Xc_xb))
        colnames(validation_rep ) <- c("Y","X","Xc")
        validation_rep$Xc <- as.factor(validation_rep$Xc )
        fit <- lm(as.formula("Y ~ X + Xc"), data = validation_rep, weights=weights_x)
        betahat =  fit$coefficients["X"]
      }else{
        validation_rep = data.frame(cbind(Yb,Xncb))
        colnames(validation_rep ) <- c("Y","X")
        # validation_rep$Xc <- as.factor(validation_rep$Xc )
        fit <- lm(as.formula("Y ~ X "), data = validation_rep, weights=weights_x)
        betahat =  fit$coefficients["X"]
      }

      # den = wtd.var(validation_rep[,nc_var],weights_validation, normwt=TRUE)     #var(merge_cp[,out_var_l],na.rm=T)
      # names_id = sort(unique(validation_rep$Xc))
      # ln =length(names_id )-1
      # rat = rep(0,ln)
      # for(k in 1:ln){
      #   rat[k] <- cov( validation_rep[!is.na( validation_rep[,"X"]),"X"], validation_rep[!is.na( validation_rep["X"]),"Xc"]==k)/  den
      # }
      # betahat =  fit$coefficients["X"]  + sum( fit$coefficients[-c(1,2)]*rat, na.rm=T)

      sam0 =     betahat
      sam0 = rbind(sam0,sam0)



      # validation2 <- validation
      # validation2[,c_var] <- as.factor(  validation2[,c_var])
      # fm_OLS <- formula(paste0(out_var, "~",nc_var ,"+  ", paste(c_var,collapse = "+")))
      # fit_OLS  <- lm(fm_OLS ,weights=weights_validation, data= validation2)
      # den =    wtd.var(validation2[,nc_var],weights_validation, normwt=TRUE)     #var(merge_cp[,out_var_l],na.rm=T)
      #
      # names_id = sort(unique(validation2$names))
      # ln =length(names_id )-1
      # rat = rep(0,ln)
      # for(k in 1:ln){
      #   rat[k] <- cov(validation2[!is.na(validation2[,nc_var]),nc_var],validation2[!is.na(validation2[nc_var]),c_var]==k)/  den
      # }
      # betahat =  fit_OLS$coefficients[nc_var]  + sum( fit_OLS$coefficients[-c(1,2)]*rat, na.rm=T)





    }




  }else{
    ## point estimate
    Xc_xb =Xc_x
    Xncb = Xnc
    Xc_yb = Xc_y
    Yb = Y

  }

  if(!is.null(R2bound)){

    ## compute short regression
    datay <- cbind(Yb, Xc_yb)
    datay <- as.data.frame(datay)
    for(j in 1:(dim(datay)[2]-1)){
      datay[,j+1] <- as.factor( datay[,j+1])
    }
    # colnames(datay) <- c("Y",seq())
    form = paste0(colnames(datay)[1],"~ ")
    form = paste0(form,paste0(colnames(datay)[-c(1)],collapse="+"))

    datay$weights_y <- weights_y
    short_reg <- lm(form, data = datay, weights= weights_y )
    short_reg_s <- summary(short_reg)
    Rs = short_reg_s$r.squared

    # rm(datay)
  }





  #######################################################################################################################################
  if(!is.null(values)){ ### if there is a discrete common regressor Xc

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

    if(!is.null(R2bound)){
      r2bound_M= matrix(0,dim(values)[1],dim(sam0)[1])
    }
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
            # if( (nbV_k>1 & grouped0==FALSE) | (nbV_k>2 & grouped0==TRUE)){
            if(  nbV_k>1){
              cptR <- compute_constraints(constraint[j],values,values_sel,indexes_k,nbV, grouped0,ind=j,c_sign) # modify for c_sign
              R_k  <- cptR$R
              pp0_k <- ((1:length(values[,1]))[indexes_k])[as.matrix(cptR$pp0)]
              # dim(pp0_k )
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

          # if(dimXnc>1){
          #   mat_Xk[k,] <- colSums(Xp*(weights_xp%*%matrix(1,1,dim(Xp)[2])))
          # }else{
          #   mat_Xk[k,] <- sum(Xp*weights_xp)
          # }
        }
      }

    }

    #############################################################################################

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

    # if(!is.null(eps_default0)){
    #   # X_0 =((sam0%*%t(Xp0))*(matrix(1,dim(sam0)[1],1)%*%matrix(weights_xp0,1,sum(sel0_x))))%*%matrix(1,dim(Xp0)[1],1)
    #   X_kk = sam0%*%matrix(mat_X0[k,],dim(sam0)[2],1)
    # }


    Yp0 = Yb[sel0_y]
    Y0 = sum(Yp0*weights_yp0)
    grid_I = vector("list")
    #### vector of matrices of point estimate ratios.
    T_n = matrix(NA,1,dim(values)[1])
    cond_w = matrix(NA,1,dim(values)[1])



    # Yk = rep(NA,length(names))
    # Xk = matrix(NA,length(names), dimXnc)



    # k=18
    # lim = 1
    # start_time <- Sys.time()
    ##################################################################################################################################################################################
    ## statistic S bar k=1
    for(k in 1:dim(values)[1]){ ## for all the points of support of Xc

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
      # T_xy1 = min(n_x,n_y)
      T_n[1,k] =  T_xy1
      # T_n[1,k] =  T_xy1

      ######################################################
      #####################################################





      # Ybarre =  sum(Yb*weights_y)
      Xp_s = Xp
      weights_xp_s = weights_xp
      weights_yp_s = weights_yp

      if(version == "first"){
        ### handle the potentially different size
        if(n_x > n_y){
          sit = sample(1:n_x,n_y,replace = F)
          Xp = matrix(Xp[sit,],n_y,dimXnc)
          weights_xp =   weights_xp[sit]
          weights_xp  =  weights_xp /sum( weights_xp )
        }else if( n_y > n_x){
          sit = sample(1:n_y,n_x,replace = F)
          Yp =Yp[sit]
          weights_yp =   weights_yp[sit]
          weights_yp  =  weights_yp /sum( weights_yp )
        }
      }

      if(is.null(grid) | is.null(eps_default0)){
        test0 = TRUE
      }else{
        if(meth=="adapt"){
          test0 = sum(is.na(eps_default0[,k]))==0
        }else{
          test0 = sum(is.na(eps_default0[k]))==0
        }
      }

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








        if(version == "second"){

          if(ties==FALSE){

            Ys0 = cbind(weights_yp,Yp-Ybarre)
            Ys1 = Ys0[order(Ys0[,2], decreasing = F),]
            indexes0 = Ys1[,1]>0
            Ysort = Ys1[ indexes0 ,2]
            weights_yp = Ys1[ indexes0 ,1]
            Iwy = cumsum(weights_yp)

            if(length(Ysort)>1 & length(Iwy) > 2){
              for_critY = approxfun( c(0,Iwy) ,  c(0,cumsum(weights_yp*Ysort)), method = "linear" ,yleft = 0, yright=0 )
              grid_I_k =  sort(unique(Iwy), decreasing = F)

              grid_I[[k]] <- grid_I_k

            }else{
              short = TRUE
            }

          }else{


            ##### alternative
            if(length(unique(Yp))>1){
              Ysort = Yp
              FY0 = ewcdf(Yp ,  weights_yp)
              FY = FY0[[1]]
              I_Wy = unique(as.numeric(FY0[[2]]))
              if(length(I_Wy)>2){

                resY = matrix(NA,length(I_Wy),1)
                for(i in 1:length(I_Wy)){
                  resY[i] = sum( (Yp-Ybarre)*weights_yp*(FY(Yp)> I_Wy[i]  ))
                }
                for_critY = approxfun(c(0,I_Wy),c(0,resY) , method = "linear", yleft =0, yright=0)

                grid_I_k =  sort(I_Wy, decreasing = F)
                # grid_I <- grid_I_k
                grid_I[[k]] <- grid_I_k
              }else{
                short= TRUE
              }
            }else{
              short= TRUE
            }
          }

          # cbind(resY,resY1)

        }else{


          Ysort = sort(Yp-Ybarre)
          for_critY= cumsum(Ysort);

          grid_I=NULL

          grid_I =NULL
          grid_I_k =NULL

          short= FALSE
        }



        if(!short){
          if(meth=="min"){
            if(is.null(eps_default0)){
              sam0_eps_default0=sam0
            }else{
              if(is.null(grid)){
                sam0_eps_default0= cbind(rep(eps_default0,dim(sam0)[1]),sam0)
              }else{
                sam0_eps_default0= cbind(rep(eps_default0[k],dim(sam0)[1]),sam0)
              }
            }
          }else{
            #adapt
            if(is.null(grid)){
              sam0_eps_default0= cbind(rep(eps_default0,dim(sam0)[1]),sam0)
            }else{
              sam0_eps_default0= cbind(eps_default0[,k],sam0)
            }
          }

          sam0_eps_default0 = cbind(1:dim(sam0_eps_default0)[1],sam0_eps_default0)



          # x_eps0= sam0_eps_default0[2,]
          bsharp_beta2 <- na.omit(t(apply(sam0_eps_default0,1,compute_ratio,Xp=Xp,Yp= Ysort,for_critY=for_critY,
                                          dimXnc=dimXnc,weights_xp=weights_xp,weights_yp=weights_yp,version,
                                          grid_I = grid_I_k, ties = ties)))

          if(dimXnc==1){
            mat_var_unc[k,] <- bsharp_beta2
            mat_var_low[k,] <- - rev(mat_var_unc[k,])
            bsharp_beta2_m  = rev(bsharp_beta2)

            if(!is.null(R2bound)){ ##### to complete in dim  1.


              sam1 = cbind(1:dim(sam0)[1],-sam0)


              normY = sqrt(wtd.var(c(Ysort), weights_yp, normwt=TRUE))
              if(normY!=0){
                r2bound_m <- na.omit(t(apply(sam1,1,compute_ratio_variance,Xp=Xp,Yp=Ysort/normY ,dimX2=dimXnc,
                                             weights_xp,weights_yp)))
                # r2bound_m
                r2bound_M[k,] <- 1/r2bound_m^2
              }
            }
          }else{
            #### compute S(-q)
            #min
            if(meth=="min"){
              if(is.null(eps_default0)){
                sam0_eps_default0_m= cbind(sam0_eps_default0[,1],-sam0_eps_default0[,2])
              }else{
                if(is.null(grid)){
                  sam0_eps_default0_m= cbind(rep(eps_default0,dim(sam0)[1]),-sam0)
                }else{
                  sam0_eps_default0_m= cbind(rep(eps_default0[k],dim(sam0)[1]),-sam0)
                }
              }
            }else{
              #adapt
              if(is.null(grid)){
                sam0_eps_default0_m= cbind(rep(eps_default0,dim(sam0)[1]),-sam0)
              }else{
                sam0_eps_default0_m= cbind(eps_default0[,k],-sam0)
              }
            }
            sam0_eps_default0_m = cbind(1:dim(sam0_eps_default0_m)[1],sam0_eps_default0_m)

            bsharp_beta2_m <- na.omit(t(apply(sam0_eps_default0_m,1,compute_ratio,Xp=Xp,Yp= Ysort,for_critY=for_critY,
                                              dimXnc=dimXnc,weights_xp=weights_xp,weights_yp=weights_yp, version,
                                              grid_I =  grid_I_k, ties=ties)))

            if(!is.null(R2bound)){

              sam1 = cbind(1:dim(sam0)[1],-sam0)

              normY = sqrt(wtd.var(c(Ysort), weights_yp, normwt=TRUE))
              # length(weights_yp)
              if(!is.na(normY))
                if(normY!=0){
                  r2bound_m <- na.omit(t(apply(sam1,1,compute_ratio_variance,Xp=Xp,Yp=Ysort/normY ,dimX2=dimXnc,
                                               weights_xp,weights_yp)))
                  r2bound_M[k,] <- 1/r2bound_m^2
                }
            }

          }

          ######################"
          # if(sum(bsharp_beta2==Inf))
          ind = c(ind,k)
          ### save the unconstrained value of  S(q) and S(-q) (both  positive)
          mat_var_unc[k,] <- bsharp_beta2
          mat_var_low[k,] <- bsharp_beta2_m
          EU = FALSE
          EL=  FALSE
        }
      }
    }
    # end_time <- Sys.time()
    # end_time - start_time
    # cbind(1:dim(mat_var_unc)[1],mat_var_unc)[ind,]

    # length(ind )
    # length(values)
    mat_var_unc1<- mat_var_unc[ind ,]
    mat_var_unc1 <- matrix(mat_var_unc1,length(ind),dim(sam0_eps_default0)[1])
    # mat_var1 <-   mat_var[ind,]
    # mat_var1 <- matrix(mat_var1,length(ind),dim(sam0_eps_default0)[1])
    mat_var_low1 <-   mat_var_low[ind,]
    mat_var_low1 <- matrix(mat_var_low1,length(ind),dim(sam0_eps_default0)[1])


    # mat_var <- apply(  mat_var1,2,min)
    mat_var_low <- apply(  mat_var_low1,2,min)
    mat_var_unc <- apply(  mat_var_unc1,2,min)
    mat_var = mat_var_unc

    # (test- test_1)
    # sort(mat_var_unc1[,2])
    # mat_var_unc
    # test = mat_var_unc1[,2]
    # which.min(  test0 )
    # quantile(mat_var_unc1[,2],c(0,1/length(test ),2/length(test ),3/length(test ),4/length(test )))
    # # # test[222]
    # # mat_var_unc1[129,2]
    #

    # mean((mat_var_unc - mat_var_low)>0)
    ###############################################################################################

    if(!is.null(constraint)){

      nb_eff =  dim(Xnc)[1] #floor(cond_w*dim(Xnc)[1])

      # jj=1
      if(!is.null(eps_default0)){

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


    }



    if(!is.null(nc_sign)){
      ee = eye(dimXnc)
      for(jj in 1:dimXnc){
        if(nc_sign[jj]!=0){
          cond = (matrix(ee[jj,]*nc_sign[jj],1,dimXnc)%*%t(sam0))<0
          if(mat_var[!cond]>0 & is.null(R2bound)){# existe une intersection
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

    mat_var_low2 <-  mat_var_low
    mat_var2 <- mat_var

    if(!is.null(R2bound)){
      ee = eye(dimXnc)

      r2bound_M1 <-  r2bound_M[ind,]
      # r2bound_M1[abs(r2bound_M1)==Inf]=10^8
      cond_w1 <- cond_w[,ind]
      ind0 = (rowSums(r2bound_M1)!=Inf)
      cond_w1 <- cond_w1[ind0]
      cond_w1 <- cond_w1/sum(cond_w1)
      r2bound_M1 <-  r2bound_M1[ind0,]
      EVX = cond_w1%*%( r2bound_M1)
      normY = sqrt(wtd.var(c(Yb), weights_y, normwt=TRUE))

      if( R2bound >1){
        val =  sqrt((R2bound-1)*Rs*normY^2/EVX[1])

        cond = (matrix(ee*nc_sign,1,dimXnc)%*%t(sam0))<0

        # jj=1
        for(jj in 1:dim(sam0)[1]){
          if(!is.na( mat_var[jj]) & !is.na( mat_var_low[jj]) ){
            if((val >  mat_var[jj]) & (-val < - mat_var_low[jj])){
              if(is.null(sample1)){
                mat_var2[jj] =  NA
                mat_var_low2[jj] = NA
              }else{ ##

                if(cond[jj]){ # negative side
                  mat_var2[jj] =   - (val + max(mat_var[jj],mat_var_low[jj]))/2
                  mat_var_low2[jj] =  (val + max(mat_var[jj],mat_var_low[jj]))/2
                }else{
                  mat_var2[jj] =  (val + max(mat_var[jj],mat_var_low[jj]))/2
                  mat_var_low2[jj] = - (val + max(mat_var[jj],mat_var_low[jj]))/2
                }
              }
            }else if( (val <=  mat_var[jj]) & (-val < - mat_var_low[jj])){
              mat_var_low2[jj] = -val
            }else if( (val >  mat_var[jj]) & (-val >= - mat_var_low[jj])){
              mat_var2[jj] = -val
            }else{
              mat_var_low2[jj] = -val
            }
          }
        }


      }

    }


    mat_var_low = mat_var_low2
    mat_var = mat_var2

    # mean((mat_var_unc - mat_var)>0)
    # mean((mat_var_unc - mat_var_low)>0)
    # mat_var- mat_var_low
    ############################################################################################################################
    # jj=1
    ## handle the sign constraint on Xnc.

    #####################################################################################################################
    #### without Xc #####################################################################################################
  }else{

    short=FALSE
    mat_var= matrix(NA,1,dim(sam0)[1])
    mat_var_unc= matrix(NA,1,dim(sam0)[1])
    mat_var_low= matrix(NA,1,dim(sam0)[1])
    if(!is.null(values)){
      mat_Yk= matrix(NA,dim(values)[1],1)
      mat_Xk= matrix(NA,dim(values)[1],dimXnc)
    }

    Xp=matrix(Xncb, dim(Xncb)[1] ,dimXnc )
    Yp = matrix(Yb, dim(Yb)[1],1)
    weights_yp =   weights_y/sum( weights_y)
    weights_xp =   weights_x/sum( weights_x )

    n_x = dim(Xp)[1]
    n_y = dim(Yp)[1]

    if(version=="first"){

      ### handle the potentially different size
      if(n_x > n_y){
        sit = sample(1:n_x,n_y,replace = F)
        Xp = matrix(Xp[sit,],n_y,dimXnc)
        weights_xp =   weights_x[sit]
        weights_xp  =  weights_xp /sum( weights_xp )

      }else if( n_y > n_x){

        sit = sample(1:n_y,n_x,replace = F)
        Yp =Yp[sit]
        weights_yp =   weights_y[sit]
        weights_yp  =  weights_yp /sum( weights_yp )
      }
    }

    # if(winsor ==TRUE){
    # qu=quantile(Yp,   max( 0.9, 1-2*log(length(Yp))/length(Yp)) )
    # Yp[Yp> qu] <- qu
    # }

    if(version == "second"){

      Ybarre = sum(Yp*weights_yp);

      # ties = FALSE
      if(ties==FALSE){


        # Ybarre = sum(Yp*weights_yp);
        Ys0 = cbind(weights_yp,Yp-Ybarre)
        Ys1 = Ys0[order(Ys0[,2], decreasing = F),]
        indexes0 = Ys1[,1]>0
        Ysort = Ys1[ indexes0 ,2]
        weights_yp = Ys1[ indexes0 ,1]
        # weights_yp = weights_yp /sum( weights_yp )
        # for_critY = cumsum(weights_yp*Ysort)
        # grid_I =  cumsum(weights_yp)
        for_critY = approxfun( c(0,cumsum(weights_yp)), c(0,cumsum(weights_yp*Ysort)) , method = "linear",yleft = 0, yright=0 )
        # for_critY = approxfun( cumsum(weights_yp), cumsum(weights_yp*Ysort), method = "linear",yleft = 0, yright=0 )

        grid_I =  cumsum(weights_yp)
        # for_critY = approxfun( cumsum(weights_yp) ,c(0,rev(cumsum(rev(weights_yp*Ysort)))), method = "linear",yleft = 0, yright=0)


      }else{

        # Ys0 = as.data.frame(cbind(weights_yp, Yp-Ybarre))
        # colnames(Ys0) <- c("weights_yp","Yp_b")
        # Ys1 = Ys0 %>% filter(weights_yp>0) %>% group_by(Yp_b) %>% summarise(weights_yp = sum(weights_yp))
        #
        # # Ys1 = Ys0 %>% group_by(Yp_b) %>% summarise(weights_yp = sum(weights_yp))
        # weights_yp = Ys1$weights_yp # /sum(Ys1$weights_yp)
        # Ysort =Ys1$Yp_b
        # for_critY = approxfun( cumsum(weights_yp) ,cumsum(weights_yp*Ysort), method = "constant" ,yleft = 0, yright=0,f =0 )
        # grid_I =  cumsum(  weights_yp)

        ##### alternative
        if(length(unique(Yp))>1){
          Ysort = Yp
          FY0 = ewcdf(Yp ,  weights_yp)
          FY = FY0[[1]]
          Iwy = unique(as.numeric(FY0[[2]]))
          if(length(Iwy)>2){
            resY = matrix(NA,length(Iwy),1)
            for(i in 1:length(Iwy)){
              resY[i] = sum( (Yp-Ybarre)*weights_yp*(FY(Yp)> Iwy[i]  ))
            }
            for_critY = approxfun(c(0,Iwy),pmax(0,c(0,resY)), method = "linear", yleft =0 , yright=0 )
            # for_critY = approxfun(Iwy,resY, method = "linear", yleft =0 , yright=0 )

            grid_I =  sort(Iwy, decreasing = F)

          }else{
            short= TRUE
          }


        }else{
          short= TRUE
        }
      }


      # cbind(for_critY(Iwy),sav,for_critY0,for_critY0/sav,for_critY0/for_critY(Iwy))


    }else{

      Ybarre = mean(Yp);
      Ysort = sort(Yp-Ybarre)
      for_critY= cumsum(Ysort);
      grid_I=NULL
    }





    if(!is.null(values)){
      mat_Yk[k,1] <- Ybarre
      mat_Xk[k,] <- sum(Xp*weights_xp)
    }

    if(!short){
      if(meth=="min"){
        # compute S(q)
        if(is.null(eps_default0)){
          ## when select epsilon
          sam0_eps_default0= sam0
        }else{
          sam0_eps_default0= cbind(rep(eps_default0,dim(sam0)[1]),sam0)
        }
      }else{
        # adapt
        # # compute S(q)
        if(is.null(eps_default0)){
          ## when select epsilon
          sam0_eps_default0= sam0
        }else{
          sam0_eps_default0= cbind(eps_default0,sam0)
        }
        #
      }

      sam0_eps_default0 = cbind(1:dim(sam0_eps_default0)[1],sam0_eps_default0)

      mat_var <- na.omit(t(apply(sam0_eps_default0,1,compute_ratio,Xp=Xp,Yp= Ysort,for_critY=for_critY,
                                 dimXnc=dimXnc,weights_xp=weights_xp,weights_yp=weights_yp, version,
                                 grid_I = grid_I, ties = ties)))

      mat_var_unc <-    mat_var

      if(type=="both" | type=="low"){
        if(dimXnc==1){
          mat_var_low <-  - rev( mat_var)
          # bsharp_beta2_m  = rev(bsharp_beta2)

        }else{
          # compute S(-q)
          if(is.null(eps_default0)){
            # min
            sam0_eps_default0_m=  sam0
          }else{
            # min
            if(meth=="min"){
              sam0_eps_default0_m= cbind(rep(eps_default0,dim(sam0)[1]),-sam0)
            }else{
              # adapt
              sam0_eps_default0_m= cbind(eps_default0,-sam0)
            }
          }
          sam0_eps_default0_m = cbind(1:dim(sam0_eps_default0_m)[1],sam0_eps_default0_m)

          mat_var_low <- - na.omit(t(apply(sam0_eps_default0_m,1,compute_ratio,Xp=Xp,Yp= Ysort,for_critY=for_critY,
                                           dimXnc=dimXnc,weights_xp=weights_xp,weights_yp=weights_yp, version,
                                           grid_I = grid_I , ties = ties)))

          # mat_var_low = matrix(NA,1,dim(sam0_eps_default0)[1])

        }
      }

      ind = 1

      if(!is.null(nc_sign)){
        ee = eye(dimXnc)
        for(jj in 1:dimXnc){
          if(nc_sign[jj]!=0){
            cond = (matrix(ee[jj,]*nc_sign[jj],1,dimXnc)%*%t(sam0))<0
            mat_var[ cond]<- 0
          }
        }
      }
    }
  }


  #### return the values of the stats Tinf/Tsup and S(q) for each q
  if(type=="both"){
    output <- vector("list")
    output[["upper"]] <- mat_var
    output[["lower"]] <- mat_var_low

    # if(!is.null(R2bound)){
    #   output[["upper_r2"]] <- mat_var2
    #   output[["lower_r2"]] <- mat_var_low2
    # }

    output[["unconstr"]] <- mat_var_unc
    if(!is.null(values)){
      output[["Ykmean"]] <- mat_Yk
      output[["Xkmean"]] <- mat_Xk

      if(dimXnc==1){
        den =  wtd.var(Xncb, weights_x, normwt=TRUE)
        Yk = rep(0,dim(values)[1]-1)
        Xk = rep(0,dim(values)[1]-1)
        rat =rep(0,dim(values)[1]-1)
        weights_yk = weights_y[Xc_yb==0]/sum(weights_y[Xc_yb==0], na.rm=T)
        weights_xk = weights_x[Xc_xb==0]/sum(weights_x[Xc_xb==0], na.rm=T)
        Y0 <- sum(Yb[Xc_yb==0]* weights_yk, na.rm=T)
        X0 <- sum(Xncb[Xc_xb==0]*weights_xk, na.rm=T)
        k=1
        for(k in 1:dim(values)[1]){
          rat[k] <- as.numeric(cov.wt(cbind(Xncb,Xc_xb==k),wt=c(weights_x) )$cov[1,2])/  den
          # rat[k] <- cov(Xncb,Xc_xb==k)/  den
          weights_yk = weights_y[Xc_yb==k]/sum(weights_y[Xc_yb==k], na.rm=T)
          weights_xk = weights_x[Xc_xb==k]/sum(weights_x[Xc_xb==k], na.rm=T)
          Yk[k] <- sum(Yb[Xc_yb==k]* weights_yk, na.rm=T)
          Xk[k] <- sum(Xncb[Xc_xb==k]*weights_xk, na.rm=T)
        }

        # den =  var(Xncb)
        # Yk = rep(0,dim(values)[1]-1)
        # Xk = rep(0,dim(values)[1]-1)
        # rat =rep(0,dim(values)[1]-1)
        # # k=2
        #
        # Y0 =  mean(Yb[Xc_yb==0], na.rm=T)
        # X0 =  mean(Xncb[Xc_xb==0], na.rm=T)
        # for(k in 1:(dim(values)[1]-1)){
        #   rat[k] <- cov(Xncb,Xc_xb==k)/den
        #   Yk[k] <- mean(Yb[Xc_yb==k], na.rm=T)
        #   Xk[k] <- mean(Xncb[Xc_xb==k], na.rm=T)
        # }
        # # table(Xc_yb)

        output[["Ykmean2"]] <- Yk
        output[["Xkmean2"]] <-  Xk
        output[["cov_ratio"]] <- rat

        term1 = sum(rat*(Yk-Y0), na.rm=T)
        if(dimXnc==1){
          term2 = 1- sum(rat*(Xk-X0), na.rm=T)
        }### complete

        output[["upper_agg"]] <-  term1 +  term2*mat_var
        output[["lower_agg"]] <-  term1 +  term2*mat_var_low
        output[["unconstr_agg"]] <-  term1 +  term2*mat_var_unc
      }

      # output[["Xkvar"]] <- var_Xk
      output[["DYk"]] <- Dmat_Yk
      output[["DXk"]] <- Dmat_Xk

      # output[["tests"]] <- tests
      output[["T_n"]] <- T_n

    }
    if(!is.null(R2bound)){
      output[["Rs"]] <- Rs
    }
    # if(!is.null(constraint)){
    #   output[["ratio"]] <- ratios0
    # }


    #### return only the values of the stat Tinf for each q
  }else if(type=="low"){
    output <- mat_var_low
  }else{
    #### return only the values of the stat S(q) for each q
    output <- mat_var
  }

  return(output)

}
