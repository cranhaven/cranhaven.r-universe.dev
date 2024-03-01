# Carrier probability
carrierprob <- function(condition="geno", method="data", fit=NULL, data, mode="dominant", q=0.02)
{
  
  if(sum(is.na(data$mgene))==0) stop("Mutatioin carrier statuses are all known")

  
  if(condition=="geno+pheno"){
    if(method=="model" | method =="data") {
    data<-carrierprobpheno(method=method, fit=fit, data=data, mode=mode, q=q)
    }
    else stop("method should be either model or data.")
  } #close cond
  else if(condition=="geno"){
    data<-carrierprobgeno(method=ifelse(method=="data","data","mendelian"), data=data, mode=mode, q=q)
  }
  else stop("Argument conditon should be either geno or geno+pheno.")
  

  return(data)
  
}