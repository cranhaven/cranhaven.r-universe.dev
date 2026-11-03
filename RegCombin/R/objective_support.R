#' Internal function to minimize to compute the function sigma for the projections of the identified set
#'
#' @param x value at which the function is evaluated.
#' @param dir_nb the index of the considered direction.
#' @param sam0 the set of directions e where to compute the support function
#' @param eps1 the matrix of directions q, along the canonical axis, and the selected epsilon(q)
#' @param Xc_xb the possibly bootstraped/subsampled common regressor on the dataset  (Xnc,Xc). Default is NULL.
#' @param Xncb the possibly bootstraped/subsampled noncommon regressor on the dataset (Xnc,Xc). No default.
#' @param Xc_yb the possibly bootstraped/subsampled common regressor on the dataset  (Y,Xc). Default is NULL.
#' @param Yb the possibly bootstraped/subsampled outcome variable  on the dataset  (Y,Xc). No default.
#' @param values the different unique points of support of the common regressor Xc.
#' @param grid the number of points for the grid search on epsilon. Default is 30. If NULL, then epsilon is taken fixed equal to kp.
#' @param weights_x the bootstrap or sampling weights for the dataset (Xnc,Xc).
#' @param weights_y  the bootstrap or sampling weights for the dataset (Y,Xc).
#' @param constraint a vector indicating the different constraints in a vector of the size of X_c indicating the type of constraints, if any on f(X_c) : "concave", "concave", "nondecreasing", "nonincreasing", "nondecreasing_convex", "nondecreasing_concave",  "nonincreasing_convex", "nonincreasing_concave", or NULL for none. Default is NULL, no contraints at all.
#' @param c_sign sign restrictions on the commonly observed regressors: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
#' @param nc_sign sign restrictions on the non-commonly observed regressors Xnc: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
#' @param refs0 indicating the positions in the vector values corresponding to the components of betac.
#' @param meth  the method for the choice of epsilon, either "adapt", i.e. adapted to the direction or "min" the minimum over the directions. Default is "adapt".
#' @param T_xy the apparent sample size the taking into account the difference in the two datasets.
#' @param bc  if TRUE compute also the bounds on betac. Default is FALSE.
#' @param version version of the computation of the ratio, "first" is a degraded version but fast; "second" is a correct version but slower. Default is "second".
#' @param R2bound the lower bound on the R2 of the long regression if any. Default is NULL.
#' @param values_sel the selected values of Xc for the conditioning. Default is NULL.
#' @param ties Boolean indicating if there are ties in the dataset. Default is FALSE.
#' @param modeNA indicates if NA introduced if the interval is empty. Default is FALSE.
#'
#' @return
#' the value the support function
#'
#' @export
#'
objective_support <- function(x,dir_nb,sam0, eps1,
                              Xc_xb ,Xncb,Xc_yb,Yb,
                              values, grid, weights_x,weights_y, constraint,
                              c_sign, nc_sign,refs0,meth="adapt", T_xy ,bc=FALSE,
                              version="first",
                              R2bound=NULL,  values_sel=NULL,
                              ties = FALSE,modeNA=FALSE){

  # enforce the constraint q'e=1
  sam1 = sam0[dir_nb,]
  if(sum(sam1==0)>0){
    sam1[ sam1==0] <- x
  }else{
    dd = (1-sam1[1]*x)/sam1[2]
    sam1[1] <- x
    sam1[2] <- dd
  }
  sam1 <- matrix(sam1,1, dim(Xncb)[2])
  XX = Xncb%*%t(sam1)
  n_x = 1
  if(!is.null( values)){
    n_c=  dim(Xc_xb )[2]
  }else{
    n_c=0
  }
  lim=1
  # sample1 =NULL
  nb_pts=0.5

  # pt = FALSE
  ### compute point estimate
  if(is.null(values)){
    mat_var_out1 <- compute_radial(sample1 =NULL,Xc_x=NULL ,Xnc= XX,Xc_y=NULL,Y=Yb,
                                   values,n_c,n_x,
                                   nb_pts,sam0= matrix(1,1,1), eps_default0=eps1, grid,lim,
                                   weights_x,weights_y,constraint,
                                   c_sign, nc_sign,refs0,type="both",meth=meth, version =version,R2bound=R2bound,
                                   values_sel=values_sel, ties =ties,modeNA=modeNA)


  }else{

    mat_var_out1 <- compute_radial(sample1 =NULL,Xc_x=Xc_xb ,Xnc= XX,Xc_y=Xc_xb,Y=Yb,
                                   values,n_c,n_x,
                                   nb_pts,sam0= matrix(1,1,1), eps_default0=eps1, grid ,lim,
                                   weights_x,weights_y,constraint,
                                   c_sign, nc_sign,refs0,type="both",meth=meth, version =version,R2bound=R2bound,
                                   values_sel=values_sel, ties =ties,modeNA=modeNA )

  }

  #### changer en fonction de ce qu'on veut S, Sc, Scon.

  # es =   T_xy ^(-boot_par)
  if(bc ==TRUE){

    res = 1/mat_var_out1$upper

  }else{

    res = 1/mat_var_out1$unconstr


  }
  if(is.na(res)){
    res=100000000
  }else{
    if(abs(res)==Inf){
      res=100000000
    }
  }
  return(res)

}
#
