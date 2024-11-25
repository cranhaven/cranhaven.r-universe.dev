carbonTurnover=function(tt,clay,C0,In, Dr=1.44, effcts, solver="lsoda"){
    func0=function(Clay, Dr){
      k=c(10,0.3,0.66,0.02,0.0)
      o=Dr/(1+Dr)
      f = 1.67 * (1.85 + 1.6 * exp(-0.0786 * clay))
      aa=c(k[1],k[2],k[3],k[4])
      a3=0.46*aa/(f+1)
      a4=0.54*aa/(f+1)
    c(o=o,a31=a3[1],a32=a3[2],a33=a3[3],a34=a3[4],a41=a4[1],a42=a4[2],a43=a4[3],a44=a4[4])
  }
  pars1=func0(clay,Dr)
  if (length(C0) != 5) {
    stop("check the number of compartments for initial carbon pools MUST 5")}
   else {
     stat1=c(C1=C0[1],C2=C0[2],C3=C0[3],C4=C0[4],C5=C0[5])}
    if(length(effcts)==1)
      func1=function(tt){effcts}
    if(is(effcts,"data.frame")){
      uu1=effcts[,1]
      uu2=effcts[,2]
      func1=splinefun(uu1,uu2)
      }

   if (length(In) == 1)
     func2=function(tt){In}
    if (is(In,"data.frame")){
      uu3=In[,1]
      uu4=In[,2]
      func2=splinefun(uu3,uu4)
      }
    func3=function(tt,stat1,pars1){
    with(as.list(c(stat1,pars1)),{
      rr=func1(tt)
      pp=func2(tt)
      dC1=o*pp-(C1*10)*rr
      dC2=(1-o)*pp-(C2*0.3)*rr
      dC3=(a31*C1+a32*C2+(-0.66+a33)*C3+a34*C4)*rr
      dC4=(a41*C1+a42*C2+a43*C3+(-0.02+a44)*C4)*rr
      dC5=0.0
      return(list(c(dC1,dC2,dC3,dC4,dC5)))
    })
  }
  pools <- ode(y = stat1, times = tt, func = func3, parms = pars1, method = solver)
  return(pools)
}
