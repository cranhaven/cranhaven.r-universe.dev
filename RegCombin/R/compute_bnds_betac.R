#' Function to compute the bounds on the coefficients of the common regressors.
#'
#' @param sample1  if NULL compute the point estimate, if a natural number then evaluate a bootstrap or subsampling replication.
#' @param info0 the results of the estimates (point and bootstrap/subsampling replications) for betanc. No default.
#' @param values the different unique points of support of the common regressor Xc.
#' @param constraint a vector indicating the different constraints in a vector of the size of X_c indicating the type of constraints, if any on f(X_c) : "concave", "concave", "nondecreasing", "nonincreasing", "nondecreasing_convex", "nondecreasing_concave",  "nonincreasing_convex", "nonincreasing_concave", or NULL for none. Default is NULL, no contraints at all.#'
#' @param c_sign0 sign restrictions on the commonly observed regressors: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
#' @param nc_sign0 sign restrictions on the non-commonly observed regressors Xnc: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
#' @param refs0 indicating the positions in the vector values corresponding to the components of betac.
#' @param c_var label of the commonly observed regressors Xc.
#' @param nc_var label of the non commonly observed regressors Xnc.
#' @param sam0 the directions q where the radial function has been computed.
#' @param info1 the results of the point estimates for betac. Default is NULL.
#' @param constr if sign constraints imposed. Default is TRUE.
#' @param R2bound the lower bound on the R2 of the long regression if any. Default is NULL.
#' @param values_sel the selected values of Xc for the conditioning. Default is NULL.
#'
#' @return
#' a matrix containing the bounds on the coefficients associated to the common regressor.
#' @export
#'
compute_bnds_betac <- function(sample1 =NULL, info0, values,
                         constraint = NULL,
                         c_sign0, nc_sign0,
                         refs0,
                         c_var, nc_var, sam0 ,info1=NULL ,
                        constr=TRUE,  R2bound=NULL,
                        values_sel=NULL){


  if(!is.null(sample1)){
    info=info0[[sample1]]
  }else{
    info=info0
  }


  #### get sizes
  dimXc = length(c_var)
  dimXnc = length(nc_var)

  beta1K <- matrix(0,length(refs0),2)
  beta1K0 <- matrix(0,length(refs0),2)

  inds=0
# k=1
  for(k in 1:length(refs0 )){

    if(dimXnc==1){
      if(is.null(info1)){

        Y_1 = info[["DYk"]][refs0[k],1]
        Xq1 = info[["DXk"]][[refs0[k]]]

      }else{

        Y_1 = info[["DYk"]][refs0[k],1]
        Xq1 = info[["DXk"]][[refs0[k]]]


      }
      if( constr==TRUE){
        if(is.null(info1)){
#
#           if(!is.null(R2bound)){
#             hull0_low =  info[["lower"]]*0
#             hull0 =  info[["upper"]]*sam0
#
#           }else{

            hull0_low =  info[["upper"]]*sam0
            hull0 =  info[["upper"]]*sam0
          # }

        }else{

          # hull0_low =  info[["upper"]]*0
          # hull0 =  info[["upper"]]*sam0

          hull0_low =  info[["upper"]]*sam0
          hull0 =  info[["upper"]]*sam0

        }
      }else{

        if(is.null(info1)){

          hull0_low =  info[["unconstr"]]*0
          hull0 =  info[["unconstr"]]*sam0

        }else{
          hull0_low =  info[["unconstr"]]*0
          hull0 =  info[["unconstr"]]*sam0

        }

      }

      if(sum(is.na(hull0_low))==0){
        if(sum(abs(hull0_low))==0){
            # without constraints, in dimension 1

            b1_inf =  min(Y_1- hull0*Xq1)
            b1_sup =  max(Y_1- hull0*Xq1)

        }else{

            b1_inf =   min( min(Y_1 - hull0*Xq1),min(Y_1 - hull0_low*Xq1)  )
            b1_sup =   max( max(Y_1 - hull0*Xq1),max(Y_1 - hull0_low*Xq1)  )

        }
      }else{
        b1_inf = NA
        b1_sup = NA
      }

      beta1K[k,] <- c(b1_inf,b1_sup)

      if(!is.null(c_sign0) & constr==TRUE){
        if(c_sign0[k]>0){
          beta1K[k,] <- c(max(b1_inf,0),b1_sup)
        }else if(c_sign0[k]<0){
          beta1K[k,] <- c(b1_inf,min(b1_sup,0))
        }else{
          beta1K[k,] <- c(b1_inf,b1_sup)
        }
      }

    }else{
      ########   dimXnc > 1 ##############

    }

  }


  return(beta1K)
}
