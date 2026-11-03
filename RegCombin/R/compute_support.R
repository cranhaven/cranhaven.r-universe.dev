#' Compute the support function for the projections of the identified set
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
#' @param constraint a vector indicating the different constraints in a vector of the size of X_c indicating the type of constraints, if any on f(X_c) : "concave", "concave", "nondecreasing", "nonincreasing", "nondecreasing_convex", "nondecreasing_concave",  "nonincreasing_convex", "nonincreasing_concave", or NULL for none. Default is NULL, no contraints at all.#' @param nc_sign if sign restrictions on the non-commonly observed regressors Xnc: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
#' @param c_sign sign restrictions on the commonly observed regressors: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
#' @param nc_sign sign restrictions on the non-commonly observed regressors Xnc: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
#' @param refs0 indicating the positions in the vector values corresponding to the components of betac.
#' @param type Equal to "both".
#' @param meth  the method for the choice of epsilon, either "adapt", i.e. adapted to the direction or "min" the minimum over the directions. Default is "adapt".
#' @param bc  if TRUE compute also the bounds on betac. Default is FALSE.
#' @param version version of the computation of the ratio, "first" indicates no weights, no ties, same sizes of the two datasets; "second" otherwise. Default is "second".
#' @param R2bound the lower bound on the R2 of the long regression if any. Default is NULL.
#' @param values_sel the selected values of Xc for the conditioning. Default is NULL.
#' @param ties Boolean indicating if there are ties in the dataset. Default is FALSE.
#' @param modeNA indicates if NA introduced if the interval is empty. Default is FALSE.
#'
#' @return
#' a matrix containing the considered directions and the computed value of the support function.
#'
compute_support <- function(sample1 = NULL,Xc_x,Xnc,Xc_y,Y,
                            values ,dimXc,dimXnc,nb_pts,
                            sam0, eps_default0,grid,
                            lim = 30,weights_x = NULL,weights_y = NULL,
                            constraint = NULL,
                            c_sign = NULL, nc_sign= NULL,
                            refs0=NULL,type="both",meth="adapt", bc = FALSE,
                            version="first",
                            R2bound=NULL,  values_sel=NULL,
                            ties=FALSE,modeNA=FALSE){
  mat_var_low= NULL
  if(is.null(weights_x)){
    weights_x= rep(1/dim(Xnc)[1],dim(Xnc)[1])
  }
  if(is.null(weights_y)){
    weights_y= rep(1/length(Y),length(Y))
  }

  weights_xs <-  weights_x
  weights_ys <-  weights_y


  if(!is.null(sample1)){
    n_x = dim(Xnc)[1]
    n_y = dim(Y)[1]
    n_xy = min(n_x,n_y)
    T_xy  = (n_y/(n_x+n_y))*n_x

    bs = floor(sampling_rule(T_xy))+1

    if(version !="first"){

      ##
      # n_x = dim(Xnc)[1]
      bb = sample(1:n_x,n_x-bs, replace=FALSE)
      # bb = 3:4
      if(!is.null(Xc_x)){
        Xc_xb = matrix(Xc_x,n_x,dimXc)
      }
      Xncb = matrix(Xnc,n_x,dimXnc)
      weights_x =  matrix(weights_x,n_x,1)
      weights_x[bb] <-0
      weights_x = weights_x/sum(weights_x)

      # n_y = dim(Y)[1]
      bby = sample(1:n_y,n_y-bs, replace=FALSE)
      # bby=1:2
      if(!is.null(Xc_y)){
        Xc_yb = matrix(Xc_y,n_y,dimXc)
      }
      Yb = matrix(Y,n_y,1)
      weights_y =  matrix(weights_y,n_y,1)
      weights_y[bby] <-0
      weights_y = weights_y/sum(weights_y)

    }else{
      #
      #
      # n_x = dim(Xnc)[1]
      bb = sample(1:n_x,bs, replace=FALSE)
      # bb = c(1:2,5:n_x)
      if(!is.null(Xc_x)){
        Xc_xb = matrix(Xc_x[bb,],bs,dimXc)
      }
      Xncb = matrix(Xnc[bb,],bs,dimXnc)
      weights_x =  matrix(weights_x[bb],bs,1)
      # weights_x[bb] <-0
      weights_x = weights_x/sum(weights_x)
      n_x = dim(Xncb)[1]


      # n_y = dim(Y)[1]
      bby = sample(1:n_y,bs, replace=FALSE)
      #  bby = 3:n_y
      if(!is.null(Xc_y)){
        Xc_yb = matrix(Xc_y[bby,],bs,dimXc)
      }
      Yb = matrix(Y[bby],bs,1)
      weights_y =  matrix(weights_y[bby],bs,1)
      # weights_y[bby] <-0
      weights_y = weights_y/sum(weights_y)
      n_y = dim(Yb)[1]
    }

  }else{
    ## point estimate
    weights_xs = NULL
    weights_ys = NULL
    Xc_xb =Xc_x
    Xncb = Xnc
    Xc_yb =   Xc_y
    Yb = Y

  }

  dir_nb=1
  sam1 = matrix(NA,dim(sam0)[1],1)
  varn = function(x){var(x,na.rm=TRUE)}
  bnd =1.5*sqrt( var(Yb,na.rm=TRUE)/min(apply(Xncb,2,varn)))

  n_x = dim(Xnc)[1]
  n_y = dim(Y)[1]
  T_xy=n_x*(n_y/(n_x+n_y))

  nbCores_dir = 1


  if(nbCores_dir==1){
    out1 = lapply(1:dim(sam0)[1],compute_support_paral,sam0,Xnc,  eps_default0, grid,dimXc,dimXnc,Xc_xb= Xc_xb ,Xncb,Xc_yb= Xc_yb,Yb,
                  values, weights_x,weights_y, constraint , c_sign,
                  nc_sign,refs0,meth,   T_xy  ,
                  bc, version, R2bound,  values_sel,ties,modeNA)

  }else{
    out1 =  sfLapply(1:dim(sam0)[1],compute_support_paral,sam0,Xnc,  eps_default0, grid,dimXc,dimXnc,Xc_xb= Xc_xb ,Xncb,Xc_yb= Xc_yb,Yb,
                     values,weights_x,weights_y, constraint , c_sign,
                     nc_sign,refs0,meth, T_xy ,
                     bc,  version, R2bound,  values_sel,ties,modeNA)
  }
  out11 <- unlist(out1)

  sam1 = cbind(sam0, out11)

  return(sam1)

}
