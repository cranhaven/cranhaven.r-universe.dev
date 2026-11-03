#' Function to minimize to compute the function sigma for the projections of the identified set
#'
#' @param dir_nb the reference for the considered direction e in sam0
#' @param sam0 the directions q to compute the radial function.
#' @param Xnc the noncommon regressor on the dataset  (Xnc,Xc). No default
#' @param eps_default0 the matrix containing the directions q and the selected epsilon(q)
#' @param grid the number of points for the grid search on epsilon. Default is 30. If NULL, then epsilon is taken fixed equal to kp.
#' @param dimXc the dimension of the common regressors Xc.
#' @param dimXnc the dimension of the noncommon regressors Xnc.
#' @param Xc_xb the possibly bootstraped/subsampled common regressor on the dataset  (Xnc,Xc). Default is NULL.
#' @param Xncb the possibly bootstraped/subsampled noncommon regressor on the dataset (Xnc,Xc). No default.
#' @param Xc_yb the possibly bootstraped/subsampled common regressor on the dataset  (Y,Xc). Default is NULL.
#' @param Yb the possibly bootstraped/subsampled outcome variable  on the dataset  (Y,Xc). No default.
#' @param values the different unique points of support of the common regressor Xc.
#' @param weights_x the bootstrap or sampling weights for the dataset (Xnc,Xc).
#' @param weights_y  the bootstrap or sampling weights for the dataset (Y,Xc).
#' @param constraint a vector indicating the different constraints in a vector of the size of X_c indicating the type of constraints, if any on f(X_c) : "concave", "concave", "nondecreasing", "nonincreasing", "nondecreasing_convex", "nondecreasing_concave",  "nonincreasing_convex", "nonincreasing_concave", or NULL for none. Default is NULL, no contraints at all.#' @param nc_sign if sign restrictions on the non-commonly observed regressors Xnc: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
#' @param c_sign sign restrictions on the commonly observed regressors: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
#' @param nc_sign sign restrictions on the non-commonly observed regressors Xnc: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
#' @param refs0 indicating the positions in the vector values corresponding to the components of betac.
#' @param meth the method for the choice of epsilon, either "adapt", i.e. adapted to the direction or "min" the minimum over the directions. Default is "adapt".
#' @param T_xy the apparent sample size the taking into account the difference in the two datasets.
#' @param bc if TRUE compute also the bounds on betac. Default is FALSE.
#' @param version version of the computation of the ratio, "first" indicates no weights, no ties, same sizes of the two datasets; "second" otherwise. Default is "second".
#' @param R2bound the lower bound on the R2 of the long regression if any. Default is NULL.
#' @param values_sel the selected values of Xc for the conditioning. Default is NULL.
#' @param ties Boolean indicating if there are ties in the dataset. Default is FALSE.
#' @param modeNA indicates if NA introduced if the interval is empty. Default is FALSE.
#'
#' @return
#' the value of the support function in the specifed direction dir_nb.
#'
compute_support_paral <- function(dir_nb , sam0,Xnc,  eps_default0, grid,dimXc,dimXnc,Xc_xb=NULL ,Xncb,Xc_yb=NULL,Yb,
                                  values,weights_x,weights_y, constraint =NULL, c_sign,
                                  nc_sign,refs0,meth,  T_xy ,
                                  bc,  version,
                                  R2bound=NULL,  values_sel=NULL,
                                  ties= FALSE,modeNA=FALSE){
  sam11 = sam0[dir_nb,]
  sam11[sam0[dir_nb,]==0] <- 1
  sam11[sam0[dir_nb,]!=0] <- 0

  ### compute starting point.
  v12 = cov(Xncb%*%sam0[dir_nb,],Xncb%*% sam11)
  v1= var(Xncb%*% sam11)
  if(dimXnc==2){
    start= -v12/v1
  }else{
    start= c(-v12/v1, rep(1,dimXnc-2))
  }


  if(dimXc==0){

    if(meth=="min"){
      eps1= eps_default0
    }else{
      eps1= eps_default0[dir_nb,1]
    }
    optf <-  optim(par= start ,  fn=objective_support, gr=NULL,   dir_nb=dir_nb,  sam0 = sam0, eps1= eps1,grid = grid, Xc_xb=NULL ,Xncb=Xncb,Xc_yb=NULL,Yb=Yb,
                   values=values, weights_x=weights_x,weights_y=weights_y, constraint= constraint, c_sign= c_sign,
                   nc_sign=nc_sign,refs0=refs0,meth=meth,  T_xy = T_xy ,
                   bc=bc, version=version, R2bound= R2bound,  values_sel=values_sel,ties = ties,modeNA=modeNA,
                   method="BFGS")
  }else{

    if(meth=="min"){
      eps1= eps_default0
    }else{
      eps1= eps_default0[dir_nb,1]
    }

    optf <- optim(par= start, objective_support, gr=NULL,   dir_nb=dir_nb,  sam0 = sam0, eps1= eps1,grid = grid,Xc_xb=Xc_xb ,Xncb=Xncb,Xc_yb=Xc_yb,Yb=Yb,
                  values=values, weights_x=weights_x,weights_y=weights_y, constraint= constraint,  c_sign= c_sign,
                  nc_sign=nc_sign,refs0=refs0,meth=meth, T_xy = T_xy ,
                  bc=bc, version=version,R2bound= R2bound,  values_sel=values_sel,ties = ties,modeNA=modeNA,
                  method="BFGS")

  }

  return(1/optf$value)
}
