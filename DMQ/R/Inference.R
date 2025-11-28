significanceFun <- function(Hessian, pn, iT){
  
  names_pn = names(pn)
  
  if(!is.null(Hessian)) {
  inv.hessian=ginv(Hessian)
  var=diag(inv.hessian)
  
  se = sqrt(var)/sqrt(iT)
  test = pn/se
  p.values=numeric(length(pn))
  
  p.values[pn>0]=pt(q=test[pn>0], df=iT-length(pn), lower.tail = FALSE)
  p.values[pn<0]=pt(q=test[pn<0], df=iT-length(pn), lower.tail = TRUE)
  } else {
    se = rep(NA, length(pn))
    test = rep(NA, length(pn))
    p.values = rep(NA, length(pn))
    inv.hessian = NULL
  }
  
  matcoef=base::matrix(0,length(pn),4,dimnames=list(names_pn,c("Estimate", 
                                                               "Std.Error", "t.test","p-value")))
  matcoef[,1]=pn
  matcoef[,2]=se
  matcoef[,3]=test
  matcoef[,4]=p.values
  
  out=list(pn=pn,se=se,test=test,p.values=p.values,matcoef=matcoef, 
           inv.hessian=inv.hessian)
  return(out)
}
