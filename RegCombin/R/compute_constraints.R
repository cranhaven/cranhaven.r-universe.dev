#' Compute the indexes of the values of the common regressors Xc used in the various shape constraints
#'
#' @param constraint the current shape constraint
#' @param values  the different unique points of support of the common regressor Xc.
#' @param values_sel the selected values of Xc for the conditioning. Default is NULL.
#' @param indexes_k indexes of the constraints
#' @param nbV indexes of the constraints
#' @param grouped0 boolean indexing if the values of Xc have been changed
#' @param ind index
#' @param c_sign sign restrictions on the commonly observed regressors: -1 for a minus sign, 1 for a plus sign, 0 otherwise. Default is NULL, i.e. no constraints.
#'
#' @return
#' a vector containing:
#'
#' - the matrix R where each line is a constraint
#'
#' - the matrices pp0 and pp1, which contains the indexes of the values of Xc in values_sel which enters the various constraints.
#'


compute_constraints <- function(constraint,values, values_sel, indexes_k=NULL,nbV, grouped0,ind=NULL,c_sign=NULL){


  pp01 = NULL
  if(is.null(indexes_k)){ ### 1 Xc
    nbV_k = nbV
    values1 = values
    pp00 = (1:nbV)
  }else{  # more than 1.
    nbV_k = sum(indexes_k)
    values1 = values_sel$selected[indexes_k,ind]
    pp00 = (1:nbV)[indexes_k]
  }

  if((constraint=="convex" || constraint=="concave"  )&& ( (nbV_k>2 & grouped0==FALSE) || (nbV_k>3 & grouped0==TRUE) ) ){

    if(grouped0){
      pp0 = cbind(2:(nbV_k-2), 3:(nbV_k-1), 4:(nbV_k))
      dd0 = length(pp0[,1])
    }else{
      pp0 = cbind(1:(nbV_k-2), 2:(nbV_k-1), 3:(nbV_k))
      dd0 = length(pp0[,1])
    }
    # pp0 = rbind( pp0,pp0)

    R = matrix(0, dd0,nbV)

    for(k in 1: dd0){
      if(k <=  dd0){
        R[k,pp00[pp0[k,1]]] <- 1/(values1[pp0[k,2]]-values1[pp0[k,1]]) - 1/(values1[pp0[k,3]]-values1[pp0[k,1]])
        R[k,pp00[pp0[k,2]]] <- -1/(values1[pp0[k,2]]-values1[pp0[k,1]])
        R[k,pp00[pp0[k,3]]]<- 1/(values1[pp0[k,3]]-values1[pp0[k,1]])
      }else{
        R[k,pp00[pp0[k,1]]] <- 1/(values1[pp0[k,3]]-values1[pp0[k,1]])
        R[k,pp00[pp0[k,2]]]<- -1/(values1[pp0[k,3]]-values1[pp0[k,1]])
        R[k,pp00[pp0[k,3]]] <- 1/(values1[pp0[k,3]]-values1[pp0[k,2]]) -  1/(values1[pp0[k,3]]-values1[pp0[k,1]])
      }
    }

    if(constraint=="concave" ){
      R= -R
    }

  }else if((constraint =="nondecreasing" || constraint =="nonincreasing" || constraint == "IV") && ( (nbV_k>1 & grouped0==FALSE)|| (nbV_k>2 & grouped0==TRUE) )){



    if(grouped0){
      pp = expand.grid(2:nbV_k, 2:nbV_k)
      pp <-  pp[( pp[,1] <  pp[,2]) ,]
      pp0 = pp
    }else{
      pp = expand.grid(1:nbV_k, 1:nbV_k)
      pp <-  pp[( pp[,1] <  pp[,2]) ,]
      pp0 = pp
    }

    R = matrix(0,length(pp0[,1]),nbV)
    for(k in 1:length(pp0[,1])){
      R[k,pp00[pp0[k,1]]] <- -1
      R[k,pp00[pp0[k,2]]] <- 1
    }

    if(constraint=="nonincreasing" ){
      R= -R
    }

    if(constraint == "IV"){
      R= rbind(R,-R)
      pp0 = rbind(pp0,pp0)
    }



  }else if( constraint =="nondecreasing_convex" || constraint =="nondecreasing_concave" || constraint =="nonincreasing_convex" || constraint =="nonincreasing_concave" ){

    trig = FALSE
    if( ((nbV_k>2 && grouped0==FALSE) || (nbV_k>3 && grouped0==TRUE))){
      if(grouped0){
        pp0 = cbind(2:(nbV_k-2), 3:(nbV_k-1), 4:(nbV_k))
        dd0 = length(pp0[,1])
      }else{
        pp0 = cbind(1:(nbV_k-2), 2:(nbV_k-1), 3:(nbV_k))
        dd0 = length(pp0[,1])
      }
      # pp0 = rbind( pp0,pp0)

      R = matrix(0,dd0,nbV)
      for(k in 1:dd0){
        if(k <=  dd0){
          R[k,pp00[pp0[k,1]]] <- 1/(values1[pp0[k,2]]-values1[pp0[k,1]]) - 1/(values1[pp0[k,3]]-values1[pp0[k,1]])
          R[k,pp00[pp0[k,2]]] <- -1/(values1[pp0[k,2]]-values1[pp0[k,1]])
          R[k,pp00[pp0[k,3]]]<- 1/(values1[pp0[k,3]]-values1[pp0[k,1]])
        }else{
          R[k,pp00[pp0[k,1]]] <- 1/(values1[pp0[k,3]]-values1[pp0[k,1]])
          R[k,pp00[pp0[k,2]]]<- -1/(values1[pp0[k,3]]-values1[pp0[k,1]])
          R[k,pp00[pp0[k,3]]] <- 1/(values1[pp0[k,3]]-values1[pp0[k,2]]) -  1/(values1[pp0[k,3]]-values1[pp0[k,1]])
        }
      }
      trig = TRUE

      if(constraint =="nondecreasing_concave" || constraint =="nonincreasing_concave" ){
        R= -R
      }


    }

    if( ((nbV_k>1 & grouped0==FALSE) | (nbV_k>2 & grouped0==TRUE)) ){
      ####
      if(grouped0){
        pp1 = expand.grid(2:nbV_k, 2:nbV_k)
        pp1 <-  pp1[( pp1[,1] <  pp1[,2]) ,]
        pp01 = pp1
      }else{
        pp1 = expand.grid(1:nbV_k, 1:nbV_k)
        pp1 <-  pp1[( pp1[,1] <  pp1[,2]) ,]
        pp01 = pp1
      }
      # pp01 = cbind(1:(nbV-1), 2:nbV)

      R1 = matrix(0,length(pp01[,1]),nbV)
      for(k in 1:length(pp01[,1])){
        R1[k,pp00[pp01[k,1]]]<- -1
        R1[k,pp00[pp01[k,2]]]<- 1
      }


      if(constraint =="nonincreasing_concave" || constraint =="nonincreasing_convex" ){
        R1= -R1
      }

      if( trig){
        R = rbind(R,R1)
      }else{
        R=R1
      }

    }
    # pp0 = rbind(pp0,pp01)


  }else if(constraint =="sign" && ((nbV_k>1 & grouped0==FALSE) || (nbV_k>2 & grouped0==TRUE)) ){

    nb0 = sum(c_sign!=0)
    R = NULL
    pp0 = NULL
    if(!is.na(nb0)){
      if(nb0>0){
          for(jj in 1:length(c_sign)){
            if(c_sign[jj]!=0){
              p0 = rep(0,length(values))
              p0[1] <- - 1*(c_sign[jj] >0) + 1*(c_sign[jj] <0)
              p0[jj+1] <-  1*(c_sign[jj] >0) - 1*(c_sign[jj] <0)
              pp0 = rbind(pp0, c(1,jj+1))
              R = rbind(R, p0)
            }
          }
        }
    }

  }


  output = vector("list")
  output[["R"]] <- R
  output[["pp0"]] <- pp0
  output[["pp01"]] <- pp01
  return(output)
}
