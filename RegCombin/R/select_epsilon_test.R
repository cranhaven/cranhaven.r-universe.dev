#' Function for the data-driven selection of the epsilon tuning parameter,  adapted to the point identification test.
#'
#' @param sam1 the matrix containing the directions q on which to compute the selected rule for epsilon(q)
#' @param eps_default If grid =NULL, then epsilon is taken equal to eps_default.
#' @param Xc_x the common regressor on the dataset  (Xnc,Xc). Default is NULL.
#' @param Xnc the noncommon regressor on the dataset  (Xnc,Xc). No default.
#' @param Xc_y the common regressor on the dataset  (Y,Xc). Default is NULL.
#' @param Y the outcome variable. No default.
#' @param values  the different unique points of support of the common regressor Xc.
#' @param dimXc the dimension of the common regressors Xc.
#' @param dimXnc the dimension of the noncommon regressors Xnc.
#' @param nb_pts  the constant C in DGM for the epsilon_0, the lower bound on the grid for epsilon, taken equal to nb_pts*ln(n)/n. Default is 1 without regressors Xc, 3 with Xc.
#' @param lim the lim number of observations under which we do no compute the conditional variance.
#' @param weights_x the sampling weights for the dataset (Xnc,Xc).
#' @param weights_y the sampling weights for the dataset (Y,Xc).
#' @param refs0 indicating the positions in the vector values corresponding to the components of betac.
#' @param grid the number of points for the grid search on epsilon. Default is 30. If NULL, then epsilon is taken fixed equal to eps_default.
#' @param constraint a vector indicating the different constraints in a vector of the size of X_c indicating the type of constraints, if any on f(X_c) : "concave", "concave", "nondecreasing", "nonincreasing", "nondecreasing_convex", "nondecreasing_concave",  "nonincreasing_convex", "nonincreasing_concave", or NULL for none. Default is NULL, no contraints at all.
#' @param c_sign sign restrictions on the commonly observed regressors: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
#' @param nc_sign sign restrictions on the non-commonly observed regressors Xnc: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
#' @param meth the method for the choice of epsilon, either "adapt", i.e. adapted to the direction or "min" the minimum over the directions. Default is "adapt".
#' @param nbCores number of cores for the parallel computation. Default is 1.
#' @param version_sel version of the selection of the epsilon, "first" indicates no weights, no ties, same sizes of the two datasets; "second" otherwise. Default is "second".
#' @param alpha the level for the confidence regions. Default is 0.05.
#' @param ties Boolean indicating if there are ties in the dataset. Default is FALSE.
#'
#' @return
#' a matrix containing the values of the selected epsilon(q) for q directions in sam1.
#'

select_epsilon_test <- function(sam1,eps_default, Xc_x,Xnc,Xc_y,Y,
                                values,dimXc,dimXnc,
                                nb_pts,lim ,weights_x,weights_y,
                                refs0,
                                grid=30,constraint =NULL, c_sign=NULL,nc_sign=NULL, meth="adapt",
                                nbCores=1,  version_sel = "first", alpha=0.05, ties =FALSE){


  version0 = version_sel
  if(is.null(weights_x)){
    weights_x= rep(1/dim(Xnc)[1],dim(Xnc)[1])
  }
  if(is.null(weights_y)){
    weights_y= rep(1/length(Y),length(Y))
  }
  q95 <-  function(x){quantile(x,1-alpha,na.rm=T)}
  q05 <-  function(x){quantile(x,alpha,na.rm=T)}
  Bsamp0 = 100
  vart2_v = NULL
  C0=0.5


  # ## point estimate
  n_x = dim(Xnc)[1]
  n_y = dim(Y)[1]

  hard = FALSE
  pt= FALSE

  if(is.null(values)){

    n_xy = min(n_x,n_y)
    T_xy  = (n_y/(n_x+n_y))*n_x

    # eps0 =2*log(T_xy)
    # eps_grid = seq(min(C0,eps0/T_xy^(3/4)),C0,length.out=grid)

    ## OK
    eps0 = nb_pts*pmax(9,log(T_xy))
    eps_grid = seq(min(C0,eps0/T_xy^(3/4)),C0,length.out=grid)

    # eps0 =  nb_pts*7.5*log(T_xy)
    # eps_grid = seq(min(C0,eps0/T_xy),C0,length.out=grid)

    # nb_pts=1
    # eps0 = nb_pts*0.3*log(n_xy)
    # eps_grid = seq(min(C0,eps0/n_xy^(2/3)),C0,length.out=grid)

    # eps0 = nb_pts*log(T_xy)
    # eps_grid = seq(min(C0,eps0/T_xy),C0,length.out=grid)

    # T_xy =200
    #
    # 5*log(T_xy)/T_xy
    # 0.8*log(T_xy)/T_xy^(2/3)

    # eps0 = 1.3*log(T_xy)
    # eps_grid = seq(min(C0,eps0/T_xy^(0.75)),C0,length.out=grid)

    eps1= matrix(0.5,dim(sam1)[1],1)
    ##################################################################
    sam0_eps_default0 =matrix(NA,dim(sam1)[1]*length(eps_grid),dimXnc+1)
    for(j0 in 1:dim(sam1)[1]){
      cur_ind = ((j0-1)*length(eps_grid)+1):(j0*length(eps_grid))
      sam0_eps_default0[ cur_ind,1] <- eps_grid
      sam0_eps_default0[ cur_ind,2:(dimXnc+1)] <- matrix(1,length(eps_grid),1)%*%sam1[j0,]
    }


    ### compute point estimate
    mat_var_out <- compute_radial(sample1 =NULL,Xc_x,Xnc,Xc_y,Y,
                                  values=NULL,dimXc,dimXnc,
                                  nb_pts, sam0= sam0_eps_default0, eps_default0=NULL, grid ,lim,
                                  weights_x,weights_y, constraint =NULL,
                                  c_sign = NULL, nc_sign  =NULL,refs0,type="both",meth=meth,
                                  version  = version0,ties =ties)

    # pt=FALSE
    # set.seed(1112)
    if(nbCores>1){
      res0 <- sfLapply(1:Bsamp0, compute_radial_test,Xc_x,Xnc,Xc_y,Y,values=NULL,dimXc,dimXnc,nb_pts,
                       sam0= sam0_eps_default0, eps_default0=NULL, grid ,lim ,weights_x,weights_y, constraint =NULL,
                       c_sign = NULL, nc_sign  =NULL,
                       refs0=NULL,type="up",meth=meth, version =version0,ties =ties)

    }else{
      res0 <- lapply(1:Bsamp0, compute_radial_test,Xc_x,Xnc,Xc_y,Y,values=NULL,dimXc,dimXnc,nb_pts,
                     sam0= sam0_eps_default0, eps_default0=NULL, grid ,lim  ,weights_x,weights_y, constraint =NULL,
                     c_sign = NULL, nc_sign  =NULL,
                     refs0=NULL,type="up",meth=meth, version =version0,ties =ties)
    }

    # compute_radial_test(1, Xc_x,Xnc,Xc_y,Y,values=NULL,dimXc,dimXnc,nb_pts,
    # sam0= sam0_eps_default0, eps_default0=NULL, grid ,lim  ,weights_x,weights_y, constraint =NULL,
    # c_sign = NULL, nc_sign  =NULL,
    # refs0=NULL,type="up",meth=meth, version =version0,ties =ties,  pt=pt)


    mat_varb= matrix(0,Bsamp0,dim(sam0_eps_default0)[1])
    for(b in 1:Bsamp0){
      mat_varb[b,] <-  res0[[b]]
    }
    S_e = matrix(1,Bsamp0,1)%*%mat_var_out$upper

    b0 = ceil(sampling_rule(T_xy))
    bs0 = sqrt( b0/T_xy)
    vart2_v=mat_var_out$upper - apply((mat_varb -  S_e)*  bs0,2,q05)

    for(j0 in 1:dim(sam1)[1]){
      tryCatch({

        cur_ind = ((j0-1)*length(eps_grid)+1):(j0*length(eps_grid))
        out_sim_p = vart2_v[cur_ind]
        eps1[j0,1] = eps_grid[which.min(out_sim_p)]
        # }


      },error=function(cond) {
        eps1[j0,1] = 0.5
      })
    }


    if(meth=="min"){
      eps_fin = min(eps1)
    }else{
      # adapt
      eps_fin = eps1 #pmin(0.5,2*eps1)  #eps1
    }

    ##############################################################################################################
  }else{

    if(meth=="min"){
      # min
      eps_fin = matrix(NA,dim(values)[1],1)
    }else{
      # #adapt
      eps_fin = matrix(NA,dim(sam1)[1],dim(values)[1])
    }


    n_x_all =  dim(Xnc)[1]
    n_y_all = dim(Y)[1]
    n_xy_all= min(n_x_all,n_y_all)
    T_xy_all  =  (n_y_all/(n_x_all+n_y_all))*n_x_all


    for(k in 1:dim(values)[1]){

      if(dimXc==1){
        val = values[k,]
        sel_x =   (Xc_x ==val)
        sel_y =   (Xc_y ==val)
      }else{
        val = t(as.matrix(values[k,]))
        sel_x = matrix(1,dim(Xc_x)[1],1)
        sel_y = matrix(1,dim(Xc_y)[1],1)
        for(ddd in 1:dimXc){
          sel_x =  sel_x & (Xc_x[,ddd]==val[ddd])
          sel_y =  sel_y & (Xc_y[,ddd]==val[ddd])
        }
      }

      if(sum(sel_x)> lim & sum(sel_y) >  lim){
        Xp =  matrix(Xnc[sel_x,],sum(sel_x),dimXnc);
        Yp =  matrix(Y[sel_y],sum(sel_y),1)

        weights_yp =   weights_y[sel_y]
        weights_yp  =  weights_yp /sum( weights_yp )
        weights_xp =   weights_x[sel_x]
        weights_xp  =  weights_xp /sum( weights_xp )

        n_x = sum(sel_x)
        n_y = sum(sel_y)
        n_xy= min(n_x,n_y)
        T_xy=  (n_y/(n_x+n_y))*n_x

        # eps0 = 7*log(T_xy)
        # eps_grid = seq(min(C0,eps0/T_xy),C0,length.out=grid)

        # eps0 = 3.5*log(T_xy)
        # eps_grid = seq(min(C0,eps0/T_xy^(3/4)),C0,length.out=grid)

        # OK
        # eps0 = 3*log(T_xy)
        # eps_grid = seq(min(C0,eps0/T_xy^(3/4)),C0,length.out=grid)

        eps0 = nb_pts*pmax(9,5*log(T_xy))
        eps_grid = seq(min(C0,eps0/T_xy^(3/4)),C0,length.out=grid)


        # eps0 = nb_pts*pmax(2,log(T_xy))
        # eps_grid = seq(min(C0,eps0/T_xy^(3/4)),C0,length.out=grid)

        eps1= matrix(0.5,dim(sam1)[1],1)


        sam0_eps_default0 =matrix(NA,dim(sam1)[1]*length(eps_grid),dimXnc+1)
        for(j0 in 1:dim(sam1)[1]){
          cur_ind = ((j0-1)*length(eps_grid)+1):(j0*length(eps_grid))
          sam0_eps_default0[ cur_ind,1] <- eps_grid
          sam0_eps_default0[ cur_ind,2:(dimXnc+1)] <- matrix(1,length(eps_grid),1)%*%sam1[j0,]
        }

        mat_var_out <- compute_radial(sample1 =NULL,Xc_x=NULL,Xnc=Xp,Xc_y=NULL,Y=Yp,
                                      values=NULL,dimXc=0,dimXnc,
                                      nb_pts, sam0= sam0_eps_default0, eps_default0 = NULL, grid,lim,
                                      weights_xp,weights_yp, constraint =NULL,
                                      c_sign = NULL, nc_sign  =NULL,refs0,type="both",meth=meth, version =version0,ties =ties)

        if(nbCores>1){
          res0 <- sfLapply(1:Bsamp0, compute_radial_test,Xc_x=NULL,Xnc=Xp,Xc_y=NULL,Y=Yp,values=NULL,dimXc=0,dimXnc,nb_pts,
                           sam0=  sam0_eps_default0 , eps_default0 =NULL, grid,lim,weights_x=weights_xp,weights_y=weights_yp,constraint =NULL,c_sign = NULL, nc_sign  =NULL,
                           refs0=NULL, type="up",meth=meth, version =version0,ties =ties)

        }else{
          res0 <- lapply(1:Bsamp0, compute_radial_test,Xc_x=NULL,Xnc=Xp,Xc_y=NULL,Y=Yp,values=NULL,dimXc=0,dimXnc,nb_pts,
                         sam0=  sam0_eps_default0 , eps_default0 =NULL, grid,lim,weights_x=weights_xp,weights_y=weights_yp,constraint =NULL,c_sign = NULL, nc_sign  =NULL,
                         refs0=NULL,type="up",meth=meth,
                         version =version0,ties =ties)
        }

        # set.seed(22)
        # compute_radial_test(1,Xc_x=NULL,Xnc=Xp,Xc_y=NULL,Y=Yp,values=NULL,dimXc=0,dimXnc,nb_pts,
        # sam0=  sam0_eps_default0 , eps_default0 =NULL, grid,lim,weights_x=weights_xp,weights_y=weights_yp,constraint =NULL,c_sign = NULL, nc_sign  =NULL,
        # refs0=NULL,type="up",meth=meth,
        # version =version0,ties =ties)


        mat_varb= matrix(0,Bsamp0,dim(sam0_eps_default0)[1])
        for(b in 1:Bsamp0){
          mat_varb[b,] <-  res0[[b]]
        }

        S_e = matrix(1,Bsamp0,1)%*%mat_var_out$upper

        b0 = ceil(sampling_rule(T_xy))
        bs0 = sqrt( b0/ T_xy)
        vart2_v=mat_var_out$upper - apply((mat_varb -  S_e)*  bs0,2,q05)

        eps1= matrix(0.5,dim(sam1)[1],1)

        for(j0 in 1:dim(sam1)[1]){
          cur_ind = ((j0-1)*length(eps_grid)+1):(j0*length(eps_grid))
          out_sim_p =vart2_v[cur_ind]
          ind0 = which.min(out_sim_p)
          if(length(ind0)>0){
            eps1[j0,1] = eps_grid[ind0]
          }
        }
      }


      if(meth=="min"){
        # min
        eps_fin[,1] = min(eps1)
      }else{
        # adapt
        eps_fin[,k] = eps1 # pmin(0.5,2*eps1) #eps1
      }

    }

  }


  return(eps_fin)
}
