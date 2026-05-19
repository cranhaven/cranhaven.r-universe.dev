Get_Segment = function(xVec, dGap=0.01){
  
  xVec = sort(xVec)
  n = length(xVec)
  
  
  idxVec = c()
  for(i in 2:n){
    
    diff = xVec[i]-xVec[i-1]
    if(diff<dGap){
      idxVec = c(idxVec, (i-1))
    }
  }
  
  NewXVec = c(xVec[1])
  for(i in 1:(n-1)){
    
    xi = xVec[i]
    xi1 = xVec[i+1]
    
    wh = which(i==idxVec)
    
    if(length(wh)==0){
      
      diff = xi1-xi
      nNum = diff%/%dGap
      
      for(j in 1:nNum){
        xVal = xi+dGap*j
        NewXVec = c(NewXVec, xVal)
      }
      
      if(xVal != xi1){
        NewXVec = c(NewXVec, xi1)
      }
      
    }else{
      
      diff = xi1-xi
      dGap0 = diff/2
      
      for(j in 1:2){
        xVal = xi+dGap0*j
        NewXVec = c(NewXVec, xVal)
      }
      
    }
    
  }
  
  
  return(NewXVec)
  
  
}




#' Implementing visual inspection of the KMT test statistic
#'
#' Draw the graph of |\eqn{U[n](z)}|.
#' 
#'@param X  a random sample of n observations
#'@param strDistr  a null distribution for the hypothesis test: Normal, Cauchy, Logistic, or Gumbel.
#'@param type  a type of  plot. A default is a line. 
#'@param lty  a line type. The default value is a solid.
#'@param lwd a line width.
#'@param col a line color.
#'@param xlim a limit for the x-axis.
#'@param ylim a limit for the y-axis.
#'@param margin_x a margin of graph in the x-axis.
#'@param margin_y a margin of graph in the y-axis.
#'@param dGap a length of subintervals of the x-axis.
#'@param b_abline a logical value for drawing vertical lines where the discontinuities of the graph happen. 

#'@return A list of the following values:
#'\describe{
#'\item{gObj}{plot of the graph of the supremand of the KMT test statistic, that is, |\eqn{U[n](z)}|  }
#'}
#'
#'@examples
#'####################

#'n=20
#'mu0=2
#'sigma0=1
#'X = rnorm(n, mu0, sigma0)
#'strDistr="Normal"


#'DrawUnz(X, strDistr, type="l", lty=1, lwd=1.5, col="red",
#'                    xlim = c(-5,5), ylim = c(NA,NA),
#'                    margin_x=0.1, margin_y=0.5, dGap=0.01, b_abline=TRUE)
#'


#'@references
#'[1] Khmaladze, E.V., Koul, H.L. (2004). Martingale transforms goodness-of-fit tests in regression models. Ann. Statist., 32. 995-1034
#'@references
#'[2] E.V. Khmaladze, H.L. Koul (2009). Goodness-of-fit problem for errors in nonparametric regression: distribution free approach. Ann. Statist., 37(6A) 3165-3185.
#'@references
#'[3] Kim, Jiwoong (2020). Implementation of a goodness-of-fit test through Khmaladze martingale transformation. Comp. Stat., 35(4): 1993-2017
#'@export
#'@useDynLib GofKmt




DrawUnz = function(X, strDistr, type="l", lty=1, lwd=1.5, col="red",
                               xlim = c(NA,NA), ylim = c(NA,NA),
                               margin_x=0.1, margin_y=0.5, dGap=0.01, b_abline=FALSE){
  
  
  spt_x=xlim[1]
  ept_x=xlim[2]
  spt_y=ylim[1]
  ept_y=ylim[2]
  
  
  tmp_txt = paste0(strDistr, "_table$SMat")
  SMat = eval(parse(text=tmp_txt))
  
  tmp_txt = paste0(strDistr, "_table$xVec")
  xVec = eval(parse(text=tmp_txt))
  
  if(strDistr=="Logistic"){
    tmp_txt = paste0(strDistr, "_table$ReVec")
    ReVec = eval(parse(text=tmp_txt))
    
  }else{
    ReVec = c(1)
  }
  
  if(strDistr=="Gumbel"){
    
    tmp_txt = paste0(strDistr, "_table$VMat")
    VMat = eval(parse(text=tmp_txt))
    
  }else{
    VMat = matrix(1, 3,3)
  }
  
  
  
  n = length(X)
  
  lst = SortX(X, strDistr)
  sorted_X = lst$Sorted_X
  
  Normed_X = sorted_X
  #print(sorted_X)
  
  if(is.na(spt_x)==TRUE){
    spt_x = sorted_X[1]-2*margin_x
  }
  
  if(is.na(ept_x)==TRUE){
    ept_x = sorted_X[n]+2*margin_x
  }
  
  
  wh = which(sorted_X>spt_x & sorted_X<ept_x)
  sidx = wh[1]
  eidx = wh[length(wh)]
  
  
  sorted_X = c(spt_x, sorted_X[sidx:eidx], ept_x)
  
  xLineVec = Get_Segment(sorted_X, dGap=dGap)
  
  nxLine = length(xLineVec)
  
  yLineVec = rep(0, times=nxLine)
  
  for(i in 1:nxLine){
    
    xi = xLineVec[i]
    yLineVec[i] = Unz(xi, Normed_X, strDistr, xVec, SMat, VMat, ReVec)
    
  }
  
  #plot(xLineVec, yLineVec, type="l", lty=2, col="red")
  
  Un_at_Zi = abs(yLineVec )
  
  max_Un = max(Un_at_Zi)
  min_Un = min(Un_at_Zi)
  
  
  if(is.na(spt_y)==TRUE){
    spt_y = min_Un
  }
  
  if(is.na(ept_y)==TRUE){
    ept_y = max_Un
  }
  
  jump_idx = c()
  
  for(i in 1:nxLine){
    
    xi = xLineVec[i]
    
    wh = which(Normed_X==xi)
    if(length(wh)!=0){
      jump_idx = c(jump_idx, i)
      
    }
    
  }
  
  nJump = length(jump_idx)
  
  sidx = 1
  eidx = jump_idx[1]-1
  
  #print(sidx)
  #print(eidx)
  #print(jump_idx)
  
  Intervals = xLineVec[sidx:eidx]
  Un_vec = yLineVec[sidx:eidx]
  
  
  plot(Intervals, Un_vec, xlim=c(xLineVec[1]-margin_x, xLineVec[nxLine]+margin_x), ylim=c(spt_y-margin_y, ept_y+margin_y),
       xlab="z", ylab=expression('|'* hat('U')[n]*'(z)|'),
       type=type, lty=lty, lwd=lwd, col=col)
  
  
  for(i in 2:nJump){
    sidx = jump_idx[i-1]
    
    if(i==n){
      eidx = nxLine
    }else{
      eidx = jump_idx[i]-1
    }
    
    Intervals = xLineVec[sidx:eidx]
    Un_vec = yLineVec[sidx:eidx]
    
    lines(Intervals, Un_vec, type=type, lty=lty, lwd=lwd, col=col)
  }
  
  if(b_abline==TRUE){
    abline(v = Normed_X, col="blue", lwd=lwd, lty=2)
  }
  
  
  
}
