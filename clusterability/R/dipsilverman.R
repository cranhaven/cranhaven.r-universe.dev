# Performs Dip Test of Unimodality and Silverman's Critical Bandwidth Test, for use in the clusterability R package.

# Copyright (C) 2020  Zachariah Neville, Naomi Brownstein, Andreas Adolfsson, Margareta Ackerman

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.


# Perform the Dip Test of Unimodality
dip <- function(dipdata, simulatepvalue = FALSE, B = 2000) {
  dipresult <- diptest::dip.test(dipdata, simulatepvalue, B)
  return(dipresult)
}

# From the silvermantest package cited in the paper, we extracted the three main functions necessary to perform
# the test. We also modified so that k = 1 is the default, and the returned
# value of the silverman.test is a list containing the p-value and saved seed.
# The method used in the bootstrap is the one described in Silverman 1981, which is slightly different
# from the method used in the original silvermantest package
# The original behavior of rounding p-values to 0 if they are below 0.005 was also removed.
# Source originally obtained from: https://www.mathematik.uni-marburg.de/~stochastik/R_packages/

silverman <-
  function(x,k=1,M=999,adjust=FALSE,digits=6,seed=NULL){
    # x: data
    # k: number of modes to be tested
    # M: number of bootstrap replications

    #check if seed is available (as done in boot package)
    #if so save it
    seedAvailable = exists(x=".Random.seed",envir=.GlobalEnv,inherits=FALSE)
    if(seedAvailable)
      saved_seed = .Random.seed
    else{
      stats::rnorm(1)
      saved_seed = .Random.seed
    }

    if(!is.null(seed)) {
      set.seed(seed)
    }

    # temp function for bootstrapping
    y.obs <- function(x,h,sig = stats::sd(x)){
      #mean(x) + (x-mean(x)+h*rnorm(length(x),0,1))/((1+h^2/sig^2)^(1/2))
      (x+h*stats::rnorm(length(x),0,1))/((1+h^2/sig^2)^(1/2))
    }

    # temp function for density calculation
    nor.kernel <- function(x,h){
      stats::density(x,bw=h,kernel ="gaussian")$y
    }

    #start of the test
    h0 <- h.crit(x, k)
    n <- 0


    for (i in 1:M) {
      x.boot <- sort(y.obs(sample(x, replace=TRUE),h0))
      mod.temp <- nr.modes(nor.kernel(x.boot,h0))
      if (mod.temp > k){
        n <- n+1
      }
    }
    p <- n/M
    ptemp=p

    if(adjust){
      if(k==1){
        #asymptotic levels of silvermantest by Hall/York
        x=c(0,0.005,0.010,0.020,0.030,0.040,0.050,0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,0.2,0.25,0.30,0.35,0.40,0.50)
        y=c(0,0,0,0.002,0.004,0.006,0.010,0.012,0.016,0.021,0.025,0.032,0.038,0.043,0.050,0.057,0.062,0.07,0.079,0.088,0.094,0.102,0.149,0.202,0.252,0.308,0.423)
        sp = splines::interpSpline(x,y)
        #adjusting the p-value
        #if(p<0.005)
        #  p=0
       #else{
          p = stats::predict(sp,p)$y
          p = round(p,digits)

          # in certain cases, spline interpolation gives a negative p-value.
          if(p < 0){
            p <- 0
          }
       # }
      }
    }

    return(list(saved_seed = saved_seed, p_value = p, k = k, hcrit = h0))
  }

nr.modes <-
  function(y){

    d1 <- diff(y)
    signs <- diff(d1/abs(d1))
    length(signs[signs==-2])

  }

h.crit <-
  function(x,k,prec=6){

    #temp function
    nor.kernel <- function(x,h){
      stats::density(x,bw=h,kernel ="gaussian")$y
    }

    digits=prec
    prec=10^(-prec)
    x <- sort(x)
    minh <- min(diff(x))		#minimal possible h
    maxh <- diff(range(x))/2	#maximal possible h
    a <- maxh
    b <- minh

    while (abs(b-a)>prec){
      m <- nr.modes(nor.kernel(x,a))

      b <- a
      if (m > k){
        minh <- a
        a <- (a + maxh)/2
      }
      else {
        maxh <- a
        a <- (a - minh)/2
      }
    }

    a=round(a,digits)


    if(nr.modes( nor.kernel(x,a) ) <= k){
      # subtract until more than k modes
      while(nr.modes( nor.kernel(x,a) ) <= k){
        a = a - prec
      }
      a=a+prec
    }

    if(nr.modes( nor.kernel(x,a) ) > k){
      # add until nr. of modes correct
      while(nr.modes( nor.kernel(x,a) ) > k){
        a = a + prec
      }
    }

    a
  }
