calculate_PCC_by_DS<-function(n,p,m,delta,sigma,alpha.grid, eigen.val,
                                class.prob)
{
  PCC.n.alpha<-vector(length = length(alpha.grid))
  for(i in 1: length(alpha.grid))
  {
    tstat = qt(alpha.grid[i]/2,n-2,lower.tail = FALSE)
    cum.t = pt(((delta/sigma)*sqrt(n)) + tstat, n-2,lower.tail = TRUE)
    num=(delta/sigma)*m*cum.t
    denom = sqrt((m*cum.t) + (alpha.grid[i]*(p-m)))
    PCC.n.alpha[i] = pnorm(num/denom , 0,1,lower.tail = TRUE) 
  }
  opt.alpha = alpha.grid[which.max(PCC.n.alpha)]
  val=((delta/sigma)*sqrt(n)) + qt((opt.alpha/2),n-2,lower.tail = FALSE)
  beta = 1 - pt( val, n - 2, lower.tail = TRUE)
  num1 = (delta*m*(1-beta)) - (0.5*log((1-class.prob)/(class.prob)))
  denom1 = sigma*sqrt(eigen.val)* sqrt((m*(1-beta)) + ((p-m)*opt.alpha))
  num2 = (delta*m*(1-beta)) + (0.5*log((1-class.prob)/(class.prob)))
  PCC.n = (class.prob * pnorm((num1/denom1),0,1, lower.tail = TRUE)) +
   ((1-class.prob)*pnorm((num2/denom1),0,1, lower.tail = TRUE))
  num3 = delta*sqrt(m)*sqrt(1-beta)*sqrt(m*(1-beta))
  denom2 = sigma*sqrt(eigen.val)*sqrt((m*(1-beta)) + ((p-m)*opt.alpha))
  PCC.n.balanced = pnorm(num3/denom2 , 0,1,lower.tail = TRUE)
  return(c(optimal.alpha=opt.alpha, PCC.n=PCC.n, PCC.n.balanced = PCC.n.balanced))
}

calculate_FScore_by_DS<-function(n,p,m,delta,sigma,alpha.grid, eigen.val,
                              class.prob)
{
  tpr<-vector(length = length(alpha.grid))
  tnr<-vector(length = length(alpha.grid))
  ppv<-vector(length = length(alpha.grid))
  FSCore.n.alpha<-vector(length = length(alpha.grid))
  k = 0.5*log((1-class.prob)/class.prob)
  for(i in 1: length(alpha.grid))
  {
    tstat = qt(alpha.grid[i]/2,n-2,lower.tail = TRUE)
    cum.t = pt(((delta/sigma)*sqrt(n)) + tstat, n-2,lower.tail = TRUE)
    tpr[i] = ((delta*m*cum.t) - k) / ((sigma*eigen.val)* 
                                        sqrt((m*cum.t)+ ((p-m)*alpha.grid[i])))
    tnr[i] = ((delta*m*cum.t) + k) / ((sigma*eigen.val)* 
                        sqrt((m*cum.t)+ ((p-m)*alpha.grid[i])))
    ppv[i] = (class.prob*tpr[i]) / (( class.prob*tpr[i]) + ((1-class.prob)*(1-tnr[i])))
    FSCore.n.alpha[i] = (2*pnorm(ppv[i] , 0,1,lower.tail = TRUE) *
                           (pnorm(tpr[i], 0,1,lower.tail = TRUE)))/ 
      ((pnorm(ppv[i] , 0,1,lower.tail = TRUE) + (pnorm(tpr[i], 0,1,lower.tail = TRUE))))
  }
  opt.alpha = alpha.grid[which.max( FSCore.n.alpha)]
  #opt.alpha = 0.01
  val=((delta/sigma)*sqrt(n)) + qt(opt.alpha/2,n-2,lower.tail = TRUE)
  beta = 1 - pt( val, n - 2, lower.tail = TRUE)
  num1 = (delta*m*(1-beta)) - (0.5*log((1-class.prob)/(class.prob)))
  denom1 = sigma*sqrt(eigen.val)* sqrt((m*(1-beta)) + ((p-m)*opt.alpha))
  num2 = (delta*m*(1-beta)) + (0.5*log((1-class.prob)/(class.prob)))
  TPR = pnorm(num1/denom1,0,1, lower.tail = TRUE)
  TNR = pnorm(num2/denom1,0,1, lower.tail = TRUE)
  PPV = (class.prob * TPR ) /(( class.prob*TPR) + ((1-class.prob)*(1-TNR)))
  FScore.n = (2* PPV*TPR)/(PPV + TPR)
  return(c(opt.alpha=opt.alpha,FScore.n=FScore.n))
}

calculate_MCC_by_DS<-function(n,p,m,delta,sigma,alpha.grid, eigen.val,
                                 class.prob)
{
  tpr<-vector(length = length(alpha.grid))
  tnr<-vector(length = length(alpha.grid))
  ppv<-vector(length = length(alpha.grid))
  npv<-vector(length = length(alpha.grid))
  MCC.n.alpha<-vector(length = length(alpha.grid))
  k = 0.5*log((1-class.prob)/class.prob)
  for(i in 1: length(alpha.grid))
  {
    tstat = qt(alpha.grid[i]/2,n-2,lower.tail = TRUE)
    cum.t = pt(((delta/sigma)*sqrt(n)) + tstat, n-2,lower.tail = TRUE)
    tpr[i] = ((delta*m*cum.t) - k) / ((sigma*eigen.val)* 
                                        sqrt((m*cum.t)+ ((p-m)*alpha.grid[i])))
    tnr[i] = ((delta*m*cum.t) + k) / ((sigma*eigen.val)* 
                                        sqrt((m*cum.t)+ ((p-m)*alpha.grid[i])))
    ppv[i] = (class.prob*tpr[i]) / (( class.prob*tpr[i]) + ((1-class.prob)*(1-tnr[i])))
    npv[i] = ((1-class.prob)*tnr[i]) / (( (1-class.prob)*tnr[i]) + ((class.prob)*(1-tpr[i])))
    
    MCC.n.alpha[i] = (sqrt(pnorm(ppv[i] , 0,1,lower.tail = TRUE) *
                           pnorm(tpr[i], 0,1,lower.tail = TRUE)*
      pnorm(npv[i], 0,1,lower.tail = TRUE) * pnorm(tnr[i], 0,1,lower.tail = TRUE))) - 
      (sqrt((1-pnorm(-ppv[i] , 0,1,lower.tail = TRUE)) *
             (1-pnorm(tpr[i], 0,1,lower.tail = TRUE))*
             (1-pnorm(npv[i], 0,1,lower.tail = TRUE)) * (1-pnorm(tnr[i], 0,1,lower.tail = TRUE))))
      
  }
  opt.alpha = alpha.grid[which.max( MCC.n.alpha)]
  #opt.alpha = 0.01
  val=((delta/sigma)*sqrt(n)) + qt(opt.alpha/2,n-2,lower.tail = TRUE)
  beta = 1 - pt( val, n - 2, lower.tail = TRUE)
  num1 = (delta*m*(1-beta)) - (0.5*log((1-class.prob)/(class.prob)))
  denom1 = sigma*sqrt(eigen.val)* sqrt((m*(1-beta)) + ((p-m)*opt.alpha))
  num2 = (delta*m*(1-beta)) + (0.5*log((1-class.prob)/(class.prob)))
  TPR = pnorm(num1/denom1,0,1, lower.tail = TRUE)
  TNR = pnorm(num2/denom1,0,1, lower.tail = TRUE)
  PPV = (class.prob * TPR ) /(( class.prob*TPR) + ((1-class.prob)*(1-TNR)))
  NPV = ((1-class.prob) * TNR ) /(( (1-class.prob)*TNR) + ((class.prob)*(1-TPR)))
  MCC.n = sqrt(TPR*TNR*PPV*NPV) - sqrt((1-TPR)*(1-TNR)*(1-PPV)*(1-NPV))
  return(c(opt.alpha=opt.alpha,MCC.n=MCC.n))
}
  
calculate_AUC_by_DS<-function(n,p,m,delta,sigma,alpha.grid=seq(0.01,0.5,0.01), eigen.val,
                                 class.prob.grid= class.prob.grid)
{
  num1.AUROC<-vector(length = length(class.prob.grid))
  num1.AUPRC<-vector(length = length(class.prob.grid))
  num2.AUROC<-vector(length = length(class.prob.grid))
  num2.AUPRC<-vector(length = length(class.prob.grid))
  TPR.AUROC <-vector(length = length(class.prob.grid))
  TPR.AUPRC <-vector(length = length(class.prob.grid))
  TNR.AUROC <-vector(length = length(class.prob.grid))
  TNR.AUPRC <-vector(length = length(class.prob.grid))
  PPV.AUROC <-vector(length = length(class.prob.grid))
  PPV.AUPRC <-vector(length = length(class.prob.grid))
  kappa = 0.5*log((1-class.prob.grid)/class.prob.grid)
  tpr<-matrix(NA,nrow = length(class.prob.grid), ncol=length(alpha.grid), byrow = TRUE)
  tnr<-matrix(NA,nrow = length(class.prob.grid), ncol=length(alpha.grid), byrow = TRUE)
  ppv<-matrix(NA,nrow = length(class.prob.grid), ncol=length(alpha.grid), byrow = TRUE)
  AUROC.n.alpha<-vector(length = length(alpha.grid))
  AUPRC.n.alpha<-vector(length = length(alpha.grid))
  for(j in 1: length(alpha.grid))
  {
    for(k in 1: length(class.prob.grid))
    {
      tstat = qt(alpha.grid[j]/2,n-2,lower.tail = TRUE)
      cum.t = pt(((delta/sigma)*sqrt(n)) + tstat, n-2,lower.tail = TRUE)
      denom =  ((sigma*sqrt(eigen.val))* sqrt((m*cum.t)+ ((p-m)*alpha.grid[j])))
      tpr[k,j] = ((delta*m*cum.t) - kappa[k]) / denom
      tpr[k,j] = pnorm(tpr[k,j],0,1,lower.tail = TRUE)
      tnr[k,j] = ((delta*m*cum.t) + kappa[k]) / denom
      tnr[k,j] = pnorm(tnr[k,j],0,1,lower.tail = TRUE)
    }
    ppv[,j] = (class.prob.grid*tpr[,j])/ ((class.prob.grid*tpr[,j]) + ((1-class.prob.grid)*(1-tnr[,j])))
    #ppv[1,]<-1
    AUROC.n.alpha[j] = simple_auc(TPR=tpr[,j], FPR=1-tnr[,j])
    AUPRC.n.alpha[j] = simple_auc(TPR=ppv[,j], FPR=tpr[,j])
    
  }
  opt.alpha.AUROC = alpha.grid[which.max( AUROC.n.alpha)]
  opt.alpha.AUPRC = alpha.grid[which.max( AUPRC.n.alpha)]
  #opt.alpha = 0.01
  val.AUROC=((delta/sigma)*sqrt(n)) + qt(opt.alpha.AUROC/2,n-2,lower.tail = TRUE)
  val.AUPRC=((delta/sigma)*sqrt(n)) + qt(opt.alpha.AUPRC/2,n-2,lower.tail = TRUE)
  beta.AUROC = 1 - pt( val.AUROC, n - 2, lower.tail = TRUE)
  beta.AUPRC = 1 - pt( val.AUPRC, n - 2, lower.tail = TRUE)
  denom1.AUROC = sigma*sqrt(eigen.val)* sqrt((m*(1-beta.AUROC)) + ((p-m)*opt.alpha.AUROC))
  denom1.AUPRC = sigma*sqrt(eigen.val)* sqrt((m*(1-beta.AUPRC)) + ((p-m)*opt.alpha.AUPRC))
  for(i in 1: length(class.prob.grid))
  {
    num1.AUROC[i] = (delta*m*(1-beta.AUROC)) - kappa[i]
    num1.AUPRC[i] = (delta*m*(1-beta.AUPRC)) - kappa[i]
    num2.AUROC[i] = (delta*m*(1-beta.AUROC)) + kappa[i]
    num2.AUPRC[i] = (delta*m*(1-beta.AUPRC)) + kappa[i]
    TPR.AUROC[i] = pnorm(num1.AUROC[i]/denom1.AUROC,0,1, lower.tail = TRUE)
    TPR.AUPRC[i] = pnorm(num1.AUPRC[i]/denom1.AUPRC,0,1, lower.tail = TRUE)
    TNR.AUROC[i] = pnorm(num2.AUROC[i]/denom1.AUROC,0,1, lower.tail = TRUE)
    TNR.AUPRC[i] = pnorm(num2.AUPRC[i]/denom1.AUPRC,0,1, lower.tail = TRUE)
    PPV.AUROC[i] = (class.prob.grid[i] * TPR.AUROC[i] ) /(( class.prob.grid[i]*TPR.AUROC[i]) + ((1-class.prob.grid[i])*(1-TNR.AUROC[i])))
    #PPV.AUPRC[i] = (class.prob.grid[i] * TPR.AUPRC[i] ) /(( class.prob.grid[i]*TPR.AUPRC[i]) + ((1-class.prob.grid[i])*(1-TNR.AUPRC[i])))
  }
  PPV.AUPRC = (class.prob.grid * TPR.AUPRC)/ ((class.prob.grid * TPR.AUPRC) + ((1-class.prob.grid)*(1-TNR.AUPRC)))
  #PPV.AUPRC[1] = 1
  # dd<-data.frame(TPR.AUROC=TPR.AUROC,FPR.AUROC = 1-TNR.AUROC)
  # dd<-dd[order(dd$TPR.AUROC,decreasing = FALSE),]
  # ddd<-data.frame(TPR.AUPRC=TPR.AUPRC,PPV.AUPRC = PPV.AUPRC)
  # ddd<-ddd[order(ddd$TPR.AUPRC,decreasing = FALSE),]
  AUROC.n = MESS::auc(y=TPR.AUROC, x= 1-TNR.AUROC)
  AUPRC.n = MESS::auc(y=PPV.AUPRC, x=TPR.AUPRC,type="spline")
  return(c(optimal.alpha.AUROC =  opt.alpha.AUROC,
           optimal.alpha.AUPRC = opt.alpha.AUPRC,
           AUROC.n=AUROC.n, AUPRC.n=AUPRC.n))
}

simple_auc <- function(TPR, FPR){
  # inputs already sorted, best scores first 
  dFPR <- c(diff(FPR), 0)
  dTPR <- c(diff(TPR), 0)
  sum(TPR * dFPR, na.rm = TRUE) + sum(dTPR * dFPR, na.rm = TRUE)/2
}


