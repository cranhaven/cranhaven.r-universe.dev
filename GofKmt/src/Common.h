
#ifndef __COMMON_H
#define __COMMON_H


arma::vec GetLineVec(arma::vec X, int nNum, double sighat);

arma::vec GetLineVec(arma::vec X, int nNum=10, double sighat=0){
  
  double dAdd = 2.5;
  
  if(sighat>0){
    dAdd = 2*sighat;
  }
  
  int n=X.n_elem;
  int nLen = (n+1)*nNum;
  
  arma::vec out(nLen);
  
  int nIndex=0;
  
  double SP=0;
  double EP=0;
  double del = 1e-3;
  
  double dGap = 0;
  for(int i=1;i<=n;i++){
    
    nIndex = (i-1)*nNum;
    
    if(i==1){
      EP = X[i-1];
      SP = EP - dAdd;
    }else{
      EP = X[i-1];
      SP = X[i-2];
    }
    dGap = (EP-SP)/nNum;
    
    for(int j=1;j<=nNum;j++){
      if(j<nNum){
        out[nIndex+j-1] = SP + dGap*(j-1);
      }else{
        out[nIndex+j-1] = EP-del;
      }
      
    }
    
  }
  
  
  ////////////// After Xn
  nIndex = n*nNum;
  
  SP = X[n-1];
  EP = SP + dAdd;
  
  dGap = (EP-SP)/nNum;
  
  for(int j=1;j<=nNum;j++){
    
    if(j<nNum){
      out[nIndex+j-1] = SP + dGap*(j-1);
    }else{
      out[nIndex+j-1] = EP-del;
    }
    
  }
  
  
  return out;
}






#endif



















