#' Calculate QHampel
#'
#' @param y vector of data values.
#' @param lab corresponding lab numbers.
#' @param tol.G1 decimal place accuracy of the numerator of s.star.
#'
#' @importFrom stats median qnorm
#' @export
#' @example man/examples/QHampel_example.R
#' @return Numeric value of zscore using QHampel algorithm.

QHampel<- function(y, lab, tol.G1=0.000001){

  # written by Bret A. Holladay
  # Q/Hampel method for robust estimation of population mean and standard deviation
  # Takes as input vector of data values, 'y' and corresponding lab numbers, 'lab'.
  # Allows for replicates.
  # There is one accuracy tolerance level in the code tol.G1 which can be made
  # smaller to yield slower but more accurate results.
  # tol.G1 corresponds to the decimal place accuracy of the numerator of s.star

  #C.5.2.2 - Q method for robust estimation of standard deviation
  if(length(y)< 2){
    showModal(modalDialog(
      title = "Error",
      "The QHampel method cannot be applied in certain cases due to insufficient data.",
      footer = modalButton("Close")
    ))
  }else{

    #p: number of labs
    p=length(unique(lab));p

    #n[i]: number of replicates from lab i
    n=NA
    for(i in 1:p){
      n[i]=sum(lab==i)
    }


    #C.22 - Define H1, the cumulative distribution function of all absolute
    #between-labratory differences

    #H1 function
    H1 <-function(x){
      sum=0
      for(i in 1:(p-1)){
        for(j in (i+1):p){
          for(k in 1:n[i]){
            sum=sum+1/(n[i]*n[j])*sum(abs(y[lab==j]-(y[lab==i])[k])<=x)
          }
        }
      }
      return(2/(p*(p-1))*sum)
    }

    # find discontinuity points, x.discont[1],...,x.discont[r] of H1(x),
    # where, x.discont[1]<...<x.discont[r]
    # dicontinuites occur at each unique between-labratory difference

    #create vector of all between-labratory differences
    x.discont=vector()
    for(i in 1:(p-1)){
      for(j in (i+1):p){
        for(k in 1:n[i]){
          x.discont=append(x.discont,abs(y[lab==j]-(y[lab==i])[k]))
        }
      }
    }
    #sort & keep only 1 of each difference (i.e. delete duplicates)
    x.discont=sort(unique(x.discont));x.discont

    #keep only positive values
    if(x.discont[1]==0){x.discont=x.discont[-1]}

    #C.23 - Calculate G1(0) and G1 at discontinuity points of H1

    #Evaluate H1 on discontinuity points
    H1.discont=sapply(x.discont, H1)

    #calculate G1(0) and G1(x1),...,G1(xr)
    G1.discont=c(0,.5*H1.discont[1], .5*(H1.discont[-1]+H1.discont[-length(H1.discont)]))

    #linearly interpolate G1 between discontinuity points 0, x.discont[1],...,x.discont[r]

    # tolerance level for G1 inverse which is part of the numerator for calculation of s*
    tol.G1=tol.G1

    #linearly interpolate G1 between discontinuity points 0,x1,...,xr
    x.int=vector()
    G1.int=vector()
    for(i in 1:length(x.discont)){
      #for each i define two points of discontinuity  (x1,y1) and (x2,y2)
      x1=c(0,x.discont)[i]
      x2=c(0,x.discont)[i+1]
      y1=G1.discont[i]
      y2=G1.discont[i+1]

      #creates sequence of points between x1 and x2
      x.temp=seq(x1,x2,by=tol.G1)

      #linearly interpolates between the two points of discontinuity
      y.temp=((y2-y1)/(x2-x1))*(x.temp-x1)+y1

      #append interpolated values
      x.int=c(x.int,x.temp[-length(x.temp)])
      G1.int=c(G1.int,y.temp[-length(y.temp)])
    }

    #numerator of s* :
    q.G1=0.25+0.75*H1(0)

    #x value that results in smallest value of |G1-q.G1|
    numerator=x.int[which.min(abs(G1.int-q.G1))]

    #denominator of s*
    p.z=0.625+0.375*H1(0)
    denominator=sqrt(2)*qnorm(p.z, mean = 0, sd = 1, lower.tail = TRUE)

    #C.24 - Calculate robust standard deviation s*
    s.star=numerator/denominator

    #C.5.3.3 - Hampel estimate of robust mean without iterative reweighting
    # uses s.star, value of robust standard deviation calculate with above
    # Q method

    #calculate arithmetic mean for each lab
    y.mean=NA
    for(i in 1:max(lab)){
      y.mean[i]=mean(y[lab==i])
    }
    #redefine y as y.mean
    y=y.mean

    #C.26 - Psi function
    Psi <- function(q){
      res=NA
      if(q<=-4.5){
        res=0
      } else if(-4.5<q && q<=-3){
        res=-4.5-q
      } else if(-3<q && q<=-1.5){
        res=-1.5
      } else if(-1.5<q && q<=1.5){
        res=q
      } else if(1.5<q && q<=3){
        res=1.5
      } else if(3<q && q<=4.5){
        res=4.5-q
      } else if(q>4.5){
        res=0
      }
      return(res)
    }

    #Calculate all interpolation nodes
    d=vector()
    for(i in 1:length(y)){
      d=c(
        d,
        y[i]-4.5*s.star,
        y[i]-3.0*s.star,
        y[i]-1.5*s.star,
        y[i]+1.5*s.star,
        y[i]+3.0*s.star,
        y[i]+4.5*s.star
      )
    }

    #sort in ascending order
    d=sort(d,decreasing=FALSE)

    #calculate p_m values
    p.vec=vector()
    for(m in 1:(6*p)){
      p.vec[m]=sum(sapply((y-d[m])/s.star, Psi))
    }

    #Find all solutions to equation C.25
    S=vector()
    for(m in 1:(6*p-1)){
      if(p.vec[m]==0){S=c(S,d[m])}
      if(p.vec[m+1]==0){S=c(S,d[m+1])}
      if(p.vec[m]*p.vec[m+1]<0){S=c(S,d[m]-p.vec[m]/((p.vec[m+1]-p.vec[m])/(d[m+1]-d[m])))}
    }

    #determine x.star
    x.star=NA
    #if no solutions to C.25 s.star is the median (median of vector of lab means)
    if(length(S)==0){
      x.star=median(y)
    } else {
      #Else s.star is the solution closest to median
      x.star=S[abs(S-median(y))==min(abs(S-median(y)))]
    }
    #if there are two solutions nearest the median set s.star to be the median
    if(length(x.star)>=2){x.star=median(y)}

    #return Q/Hampel values of robust mean and standard deviation
    return(data.frame(x.star=x.star,s.star=s.star))
}}



#' Calculate algB
#'
#' @param x vector of n observations.
#' @param iter_loc number of iteration steps for location estimate (default=50).
#' @param iter_scale number of iteration steps for scale estimate  (default=1000).
#'
#' @importFrom stats median mad
#' @export
#' @example man/examples/M_estimate_example.R
#' @return Numeric value of zscore using algB.


M_estimate <-function(x, iter_loc=50, iter_scale=1000){# Logistic M-estimate , algorithm B #

  # This code was translated from the Matlab functions 'mloclogist' and 'mscalelogist'
  # which are part of LIBRA: the Matlab Library for Robust Analysis, available at:
  #   http://wis.kuleuven.be/stat/robust.html
  # Written by S. Verboven with Revisions by N. Smets (Last update 28/08/03)
  if(length(x)< 2){
    showModal(modalDialog(
      title = "Error",
      "The algB method cannot be applied in certain cases due to insufficient data.",
      footer = modalButton("Close")
    ))
  }else{
    n=length(x)

    #starting value for the location estimate
    t_0=median(x)

    #starting value for the scale estimate
    s_0=sqrt((length(x)-1)/(length(x)-1.5))*mad(x);s_0

    #compute M-estimate of location
    if(n==1){
      loc_estimate=x #n=1 implies location estimate equals x
    }else if(n==2){
      loc_estimate=mean(x) #n=2 implies location estimate equals the mean
    }else{
      #see page 752 Rousseeuw, P.J. and Verboven, S. (2002) for
      #why denominator can be replaced by 0.4132
      alpha=0.413241928283814 #integrate(1/2*sech(x/2)^2*normpdf(x,0,1),-10,10)

      tstep=t_0

      if (s_0!=0){
        j=1
        while(j<=iter_loc){
          z=(x-tstep)/s_0;
          y=tanh(z/2);
          tstep=tstep+s_0*(sum(y)/(n*alpha)); #updating location estimate
          j=j+1;
        }
        loc_estimate=tstep
      }
    }

    #Compute M-estimate of scale
    if(n==1){
      #when x is of length 1, all scale estimators must equal to 0
      scale_estimate=0
    }else{

      # b=0.3739 leads to a 50% breakdown
      # (see page 754 Rousseeuw, P.J. and Verboven, S. (2002))
      b=0.3739;
      beta=0.500038854875226 #integrate((tanh(x/(2*b))^2)*normpdf(x,0,1),-5,5)
      sstep=s_0

      if (s_0!=0){
        j=1
        while(j<=iter_scale){
          u=(x-t_0)/sstep;u
          uu=tanh(u/(2*b))^2;uu
          sstep=sstep*sqrt(sum(uu)/(n*beta));sstep
          j=j+1;
        }
        scale_estimate=sstep
      }
    }

    return(data.frame(location=loc_estimate, scale=scale_estimate))
}}


#' Calculate ZScore
#'
#' @param X vector of estimated doses.
#' @param type dose or freq.
#' @param alg algorithm. Choose between algA, algB or QHampel
#' @param c value for reference dose.
#'
#' @importFrom MASS hubers
#' @export
#' @example man/examples/calc.zValue.new_example.R
#' @return Numeric value of zscore.

calc.zValue.new <- function(X, type, alg , c){
  req(X)
  if(alg == "algA"){
    if(sum(!is.na(X))< 2){
      showModal(modalDialog(
        title = "Error",
        "This algorithm cannot be applied in one or more samples due to insufficient data.",
        footer = modalButton("Close")
      ))
      return(NULL)
    }else{
      aus=hubers(X)
      s = aus$s
      mu = aus$mu
    }

  }else if (alg == "algB"){
    if(sum(!is.na(X))< 2){
      showModal(modalDialog(
        title = "Error",
        "This algorithm cannot be applied in one or more samples due to insufficient data.",
        footer = modalButton("Close")
      ))
      result <- rep(NA, length(X))
      return(result)
    }else{
      aus = M_estimate(X)
      s = aus$scale
      mu = aus$location
    }
  }else if (alg == "QHampel"){
    if(sum(!is.na(X))< 2){
      showModal(modalDialog(
        title = "Error",
        "This algorithm cannot be applied in one or more samples due to insufficient data.",
        footer = modalButton("Close")
      ))
      result <- rep(NA, length(X))
      return(result)
    }else{
      aus = QHampel(y=as.numeric(X),lab=1:length(X))
      s = aus$s.star
      mu = aus$x.star
    }
  }
  uref=1.25*s/sqrt(length(X))
  if(type=='dose'){
    mu=c
    uref=0
  }
  z=(X-mu)/sqrt(s^2+uref^2)

  return(z)
}


#' Calculate curve
#'
#' @param d dose
#' @param coef curve coefficients
#' @param curve_type type of curve: lin_quad, lin
#'
#' @return Numeric value.

fun.curve <- function(d, coef, curve_type){
  if(curve_type == "lin_quad")
    return(coef[1]+coef[2]*d+ coef[3]*d^2)
  else if(curve_type == "lin")
    return(coef[1]+coef[2]*d)
}

