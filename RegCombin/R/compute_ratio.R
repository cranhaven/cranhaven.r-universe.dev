#' Function to compute the main statistic for the point estimate
#'
#' @param x_eps0 a matrix containing the directions to compute the radial function, and the associated choice epsilon(q).
#' @param Xp the observations of the noncommon regressor (possibly conditional on Xc).
#' @param Yp the observations of the outcome variable.
#' @param for_critY the numerator of the ratio R for the point estimate of the radial function, on the grid grid_I;
#' @param dimXnc the dimension of the noncommon regressors
#' @param weights_xp the sampling or bootstrap weights for the dataset (Xnc,Xc).
#' @param weights_yp the sampling or bootstrap weights for the dataset (Y,Xc).
#' @param version version of the computation of the ratio, "first" indicates no weights, no ties, same sizes of the two datasets; "second" otherwise. Default is "second".
#' @param grid_I the grid of alpha on which we evaluate the ratio R to compute the point estimate of the radial function.
#' @param ties binary value handling the ties, default is FALSE.
#'
#' @return
#'  the value of the point estimate of the radial function using the DGM method.

compute_ratio <- function(x_eps0,Xp,Yp,
                          for_critY,dimXnc,
                          weights_xp,weights_yp,
                          version = "first",
                          grid_I = NULL,
                          ties = FALSE){


  # x_eps0= sam0_eps_default0[2,]
  winsor = FALSE
  nb =  x_eps0[c(1)]
  q = x_eps0[-c(1,2)]
  eps = x_eps0[c(2)]

  if(dimXnc==1){
    XX = Xp%*%q
  }else{
    XX =as.matrix(Xp,dim(Xp)[1],dimXnc)%*%matrix(q,dimXnc,1)
  }

  short= FALSE

  if(version=="second"){

    indexbarre=sum(XX*weights_xp);

    if(ties==FALSE){

      indexbarre=sum(XX*weights_xp);
      Xs0 = cbind(weights_xp,XX-  indexbarre)
      Xs1 = Xs0[order(Xs0[,2], decreasing = F),]
      indexes0 = Xs1[,1]>0
      index = Xs1[  indexes0,2]
      weights_xp0 = Xs1[  indexes0,1]
      Iwy = grid_I
      Iwx = cumsum(weights_xp0)

      if(length(index)>1 & length(Iwx)>2){
        for_critind=cumsum(weights_xp0*index);

        ### define the grid and ad the epsilon, 1- epsilon
        n_xy0 = min(length(Iwx),length(Iwy))
        kp_d = min(0.5, max(eps, 2/n_xy0))
        kp_u = max(0.5, 1- max(eps, 2/n_xy0))

        grid_I =  sort(unique(c(kp_d,Iwy,Iwx,kp_u)), decreasing = F)
        denX = approx(c(0,Iwx), c(0,for_critind), xout = c(eps,grid_I) ,  method ="linear"  ,yleft = 0, yright=0)$y

        ### compute the ratio
        # ratio =  for_critY(grid_I)/ denX(grid_I)
        ratio =  for_critY(grid_I)/ denX[-c(1)]
        ratio[is.na(ratio) ] = 10^6

      }else{
        short= TRUE
      }


      if(!short){
        ### compute the ratio
        select0 =  (grid_I>= kp_d) & (grid_I<= kp_u)
        if(sum(select0)==0){
          # ratio =  for_critY(eps)/denX(eps)
          ratio =  for_critY(eps)/denX[1]
        }else{
          ratio = ratio[select0]
        }
      }

    }else{

      if(length(unique(XX))>1){
        ##### alternative
        fX0 = ewcdf(XX ,  weights_xp)
        FX = fX0[[1]]
        Iwx = unique(as.numeric(fX0[[2]]))

        if(length(Iwx)>2 & !short){

          resX = matrix(NA,length(Iwx),1)
          for(i in 1:length(Iwx)){
            resX[i] = sum( (XX-indexbarre)*weights_xp*(FX(XX) >Iwx[i]  ))
            if(is.na(    resX[i])){
              resX[i] = 0
            }
          }


          ##### define the grid
          Iwy = grid_I
          n_xy0 = min(length(Iwy),length(Iwx))
          kp_d = min(0.5, max(eps, 2/n_xy0))
          kp_u = max(0.5, 1- max(eps, 2/n_xy0))
          grid_I =sort(unique(c(kp_d,Iwy, Iwx,kp_u)), decreasing = F)
          denX = approx( c(0,Iwx),c(0,resX) ,  method ="linear", xout = c(eps,grid_I), yleft = 0, yright=0)$y
          ratio =  for_critY(grid_I)/denX[-c(1)]


        }else{
          short= TRUE
        }
      }else{
        short= TRUE
      }

      # }
      # x11()
      # plot( grid_I, ratio,type='l')
      # lines( grid_I, ratio,col=2)

      if(!short){
        ### compute the ratio
        ratio[is.na(ratio)] = 10^6
        select0 =  (grid_I>= kp_d) & (grid_I<= kp_u)
        if(sum(select0)==0){
          ratio =  for_critY(eps)/denX[1]
        }else{
          ratio = ratio[select0]
        }

      }

    }

  }else{

    indexbarre=mean(XX);
    index = sort(XX-indexbarre)
    for_critind=cumsum(index);
    ratio = for_critY /for_critind
    ratio[is.na(  ratio)] = 10^6

    kp_d = max(floor(length(ratio)*eps)+1,2)
    kp_u = length(ratio)- max(floor(length(ratio)*eps),2)
    grid_I1 = 1:length(ratio)
    select0 = (grid_I1 >= kp_d) & (grid_I1<= kp_u)

    if(sum(select0)==0){
      select0[floor(length(ratio)/2)+1] = TRUE
    }
    ratio = ratio[select0]

    short =FALSE
  }
  # sum(is.na( ratio))

  if(!short){

    lambda=  pmax(min(ratio, na.rm=TRUE),0)

  }else{
    lambda = 10^6
  }

  return(lambda)
}

