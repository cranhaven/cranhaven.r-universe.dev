marginal_contribution_mean <-
function(permute,costs){
    n<-max(permute)
    coa<-coalitions(n)[[1]]
    cc<-costs
    if (is.vector(permute)==T){
        phi<-c()
        
        for (k in 1:n){
          permutek<-permute[1:which(permute==k)]
          permute_aux<-permute[1:(which(permute==k)-1)]
          ipermutek<-rep(0,n);ipermutek[permutek]<-1
          ipermutek_aux<-rep(0,n)
          
          if (length(permutek)!=length(permute_aux)){ipermutek_aux[permute_aux]<-1}
          
          for (i in 1:2^n){
            if (sum(coa[i,]==ipermutek)==n){
                costek<-cc[i]
            }
            if (sum(coa[i,]==ipermutek_aux)==n){
              costemenosk<-cc[i]
            }
          }
          ck<-costek-costemenosk
          phi[k]<-ck
        }
     }
    if(is.vector(permute)==F){
      cmarginales<-matrix(0,ncol=n,nrow=nrow(permute))
      phi<-rep(0,n)
      for (l in 1:nrow(permute)){
        permutel<-permute[l,]
        for (k in 1:n){
          permutek<-permutel[1:which(permutel==k)]
          permute_aux<-permutel[1:(which(permutel==k)-1)]
          ipermutek<-rep(0,n);ipermutek[permutek]<-1
          ipermutek_aux<-rep(0,n)
          
          if (length(permutek)!=length(permute_aux)){ipermutek_aux[permute_aux]<-1}
              
          costek<-0;costemenosk<-0
          for (i in 1:2^n){
            if (sum(coa[i,]==ipermutek)==n){
              costek<-cc[i]
            }
            if (sum(coa[i,]==ipermutek_aux)==n){
              costemenosk<-cc[i]
            }
          }
          ck<-costek-costemenosk
          cmarginales[l,k]<-ck
        }
      }
    phi<-apply(cmarginales,2,mean)}
return(phi)}
