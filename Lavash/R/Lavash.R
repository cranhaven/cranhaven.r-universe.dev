Lavash<-function(X, Y, K, Lambda1, Lambda2, method=c("profile","iteration"), Maxiter=50){
  if(is.matrix(X)< 1){stop("x should be a matrix with 2 or more columns")}
  dimX = dim(X)
  if(is.null(dimX) | (dimX[2] <= 1)){stop("X should be a matrix style with 2 or more columns")}
  
  if(is.matrix(Y)< 1){stop("Y should be a n by 1 matrix style")}
  dimY = dim(Y)
  if(dimX[1] != dimY[1]){stop("number of observations in Y is not equal to the number of rows of X")}
  
  if((dimY[1]%%K) != 0 | K< 1){stop("K should be a natural number which divides the number of observations in Y without leaving a remainder")}
  
  if(is.vector(Lambda1)< 1){stop("Lambda1 should be a vector style")}
  
  if(is.vector(Lambda2)< 1){stop("Lambda1 should be a vector style")}
  
  if(method != "profile" & method != "iteration"){stop("method should be either profile or iteration")}
  
  if((Maxiter%%1) != 0 | Maxiter< 1){stop("Maxiter should be a natural number")}
  
  
  if(method=="iteration"){ #lava_iteration
    n <- length(Y)
    p <- length(X[1,])
    Mitera <- Maxiter
    S=t(X)%*%X/n
    temp_svd<-svd(X,nu=n,nv=p)
    U<-temp_svd$u; M<-diag(temp_svd$d,n,p); V<-temp_svd$v
    
    elava_array<-array(dim=c(K,length(Lambda1),length(Lambda2)))
    elavapost<-array(dim=c(K,length(Lambda1),length(Lambda2)))
    ridge<-matrix(0,p,1)
    
    #begin lava and postlava CV process
    for(k in 1:K){
      #training data: size n-n/K
      Y1<-as.matrix(Y[-c(((k-1)*n/K+1):(k*n/K)),1])
      X1<-X[-c(((k-1)*n/K+1):(k*n/K)),]
      S1<-t(X1)%*%X1/(n-n/K)
      X1_svd<-svd(X1,nu=nrow(X1),nv=ncol(X1))
      U1<-X1_svd$u; M1<-diag(X1_svd$d,nrow(X1),ncol(X1)); V1<-X1_svd$v
      
      #validation data    n/K
      vY<-as.matrix(Y[((k-1)*n/K+1):((k-1)*n/K+n/K),1])
      vX<-X[((k-1)*n/K+1):((k-1)*n/K+n/K),]
      
      for(l in 1:length(Lambda2)){ #trying different values for lambda2 in the range of Lambda2
        
        #ridge<-pracma::mldivide((S1+Lambda2[l]*diag(p)),t(X1))%*%Y1/(n-n/K)
        ridge_temp<-glmnet::glmnet(X1,Y1,lambda = Lambda2[l],alpha = 0)
        ridge<-as.matrix(ridge_temp$beta)
        
        BETAlava<-matrix(0,p,length(Lambda1))
        THETA<-matrix(0,p,length(Lambda1))
        DELTA<-matrix(0,p,length(Lambda1))
        respost<-matrix(0,nrow(vX),length(Lambda1))
        
        for(mm in 1:length(Lambda1)){  
          #training for lava
          iterar<-1
          BETAlava[,mm]<-ridge # p by 1 dense part
          
          THETA[,mm]<-pracma::ones(p,1); tole<-100 # a random initial, unimportant
          while(iterar<Mitera && tole>1e-7){
            THETAold<-THETA[,mm] # the entire estimator, sparse+dense
            Z1<-Y1-X1%*%BETAlava[,mm] # only sparse left
            DELTA_temp<-glmnet::glmnet(X1,Z1,lambda = Lambda1[mm]) #returns a matrix of vectors of "sparse estimators" for different values of lambda1, chosen from the range Lambda1.
            DELTA[,mm]<-as.matrix(DELTA_temp$beta[,ncol(DELTA_temp$beta):1])
            
            #BETAlava[,mm]<-pracma::mldivide((S1+Lambda2[l]*diag(p)),t(X1))%*%(Y1-X1%*%DELTA[,mm])/(n-n/K)
            BETAlave_temp<-glmnet::glmnet(X1,(Y1-X1%*%DELTA[,mm]),lambda = Lambda2[l],alpha = 0)
            BETAlava[,mm]<-as.matrix(BETAlave_temp$beta)
            
            THETA[,mm]<-DELTA[,mm]+BETAlava[,mm]
            tole<-norm(as.matrix(THETA[,mm]-THETAold),"E")/sqrt(p)
            iterar<-iterar+1;
          }#end while
        
        # validation for post lava
        # validation for post lava is within the mm loop;
        # validation for lava is outside of the mm loop.
        
          use<-abs(DELTA[,mm])>1e-7
          if(sum(use)>0){
            XJ1<-X1[,use]
            ww<-Y1-X1%*%BETAlava[,mm]
            # In below,pracma::mldivide((t(XJ1)%*%XJ1),XJ1)%*%ww  represents the estimated nonzero elements of the sparse components,using post lava
            # vX*BETAlava(:,mm) represents the fitted dense part on the validation data
            respost[,mm]<-vX[,use]%*%(pracma::mldivide((t(XJ1)%*%XJ1),t(XJ1))%*%ww)+vX%*%BETAlava[,mm]-vY # sample size of validation by 1
            rm(use,XJ1,ww)
          } else{
            respost[,mm]<-vX%*%BETAlava[,mm]-vY
          }
        }#end mm
        elavapost[k,,l]<-colSums((respost^2)) # sum of residual squares
            
        #validation for lava
        residual<-vX%*%THETA-vY%*%matrix(1,1,length(Lambda1))   #each column is a vector of LAVA residuals fitted on the validation data. (again, there are L columns,                                                            #where L=length(Lambda1) represents the number of tried lambda1 values in the range of Lambda1)
        elava_array[k,,l]<-colSums((residual^2))  #sum of residual squares # 1 by L; elava_array(k,b,l)  k: K fold, b: Lambda1's choice, l: Lambda2's choice
        #rm(residual)
      }#end l
    }#end k
    
    #By now we have evaluated all the lambda1 and lambda2 for all the K possible splitted data sets        
    #lava fit by choosing the best lambda1 and lambda2, and then refitting the entire data set. 
    
    CVRlava<-matrix(0,length(Lambda2),ncol(THETA))
    for(l in 1:length(Lambda2)){
      CVRlava[l,]<-colMeans(elava_array[,,l]) #CVRlava(l,g): l: Lambda2(l)   g:  Lambda1(g) 
    }#end l
    a<-matrix(0,1,ncol(CVRlava)); b<-matrix(0,1,ncol(CVRlava))
    for(i in 1:ncol(CVRlava)){
      a[1,i]<-min(CVRlava[,i])
      b[1,i]<-which.min(CVRlava[,i])
    }
    c<-min(a); d<-which.min(a)
    lambda2<-Lambda2[b[1,d]]; lambda1<-Lambda1[d] #optimal choice of lambda1 and lambda2 for lava
    lambda1lava<-lambda1; lambda2lava<-lambda2;
    
    iterar<-1
    dense<-ridge
    lavatheta<-pracma::ones(p,1); tole<-100
    while(iterar<Mitera && tole>1e-7){
      THETAold<-lavatheta
      Z<-Y-X%*%dense
      deltahat_temp<-glmnet::glmnet(X,Z,lambda = lambda1) #sparse part
      deltahat<-as.matrix(deltahat_temp$beta[,ncol(deltahat_temp$beta):1])
      #dense<-pracma::mldivide((S+lambda2*diag(p)),t(X))%*%(Y-X%*%deltahat)/n
      dense_temp<-glmnet::glmnet(X,(Y-X%*%deltahat),lambda = lambda2,alpha = 0)
      dense<-as.matrix(dense_temp$beta)
        
      sparse<-deltahat
      lavatheta<-sparse+dense
      tole<-norm((lavatheta-THETAold),"E")/sqrt(p)
      iterar<-iterar+1
    }#end while  
    
    #Post-lava fit by choosing the best lambda1 and lambda2, and then refitting the entire data set. 
    CVRpostlava<-matrix(0,length(Lambda2),ncol(THETA))
    for(l in 1:length(Lambda2)){
      CVRpostlava[l,]<-colMeans(elavapost[,,l]) # CVRpostlava: l: Lambda2(l)   g: Lambda1(g)
    }
    a<-matrix(0,1,ncol(CVRpostlava)); b<-matrix(0,1,ncol(CVRpostlava))
    for(i in 1:ncol(CVRpostlava)){
      a[1,i]<-min(CVRpostlava[,i])
      b[1,i]<-which.min(CVRpostlava[,i])
    }
    c<-min(a); d<-which.min(a)
    lambda2<-Lambda2[b[1,d]]; lambda1<-Lambda1[d] # optimal choice of lambda1 and lambda2 for lava
    lambda1post<-lambda1; lambda2post<-lambda2
    
    iterar<-1
    post_dense<-dense
    post_lavatheta<-lavatheta; tole<-100
    while(iterar<Mitera && tole>1e-7){
      THETAold<-post_lavatheta
      Z<-Y-X%*%post_dense
      post_sparse_temp<-glmnet::glmnet(X,Z,lambda = lambda1) #sparse part
      post_sparse<-as.matrix(post_sparse_temp$beta[,ncol(post_sparse_temp$beta):1])
      #post_dense<-pracma::mldivide((S+lambda2*diag(p)),t(X))%*%(Y-X%*%post_sparse)/n
      post_dense_temp<-glmnet::glmnet(X,(Y-X%*%post_sparse),lambda = lambda2,alpha = 0)
      post_dense<-as.matrix(post_dense_temp$beta)
      post_lavatheta<-post_sparse+post_dense
      tole<-norm((post_lavatheta-THETAold),"E")/sqrt(p)
      iterar<-iterar+1;
    }  
    use<-abs(post_sparse)>1e-7
    post_sparse<-matrix(0,p,1)
    if(sum(use)>0){
      XJ<-X[,use]
      post_sparse[use]<-pracma::pinv(t(XJ)%*%XJ)%*%t(XJ)%*%(Y-X%*%post_dense) # post-lava estimator of the sparse component
    }  
    post_lavatheta<-post_dense+post_sparse # final estimator of post-lava 
    
    #clear row and column names
    rownames(dense) <- NULL
    colnames(dense) <- NULL
    
    rownames(deltahat) <- NULL
    colnames(deltahat) <- NULL
    
    rownames(lavatheta) <- NULL
    colnames(lavatheta) <- NULL
    
    rownames(post_dense) <- NULL
    colnames(post_dense) <- NULL
    
    rownames(post_sparse) <- NULL
    colnames(post_sparse) <- NULL
    
    rownames(post_lavatheta) <- NULL
    colnames(post_lavatheta) <- NULL
    
    #outputs:
    LAMBDA<-c(lambda1lava, lambda2lava, lambda1post, lambda2post) 
    output_list<-list("lava_dense"=dense,"lava_sparse"=deltahat,"lava_estimate"=lavatheta,"postlava_dense"=post_dense,"postlava_sparse"=post_sparse,"post_lava"=post_lavatheta,"LAMBDA"=LAMBDA, "Iteration"=iterar)
  } else if(method=="profile"){ #lava_profile
    n <- length(Y)
    p <- length(X[1,])
    S=t(X)%*%X/n
    temp_svd<-svd(X,nu=n,nv=p)
    U<-temp_svd$u; M<-diag(temp_svd$d,n,p); V<-temp_svd$v
    elava_array<-array(dim=c(K,length(Lambda1),length(Lambda2)))
    elavapost<-array(dim=c(K,length(Lambda1),length(Lambda2)))
    # begin lava CV process
    for(k in 1:K){
      # training data: size n-n/K
      Y1<-as.matrix(Y[-c(((k-1)*n/K+1):(k*n/K)),1])
      X1<-X[-c(((k-1)*n/K+1):(k*n/K)),]
      S1<-t(X1)%*%X1/(n-n/K)
      X1_svd<-svd(X1,nu=nrow(X1),nv=ncol(X1))
      U1<-X1_svd$u; M1<-diag(X1_svd$d,nrow(X1),ncol(X1)); V1<-X1_svd$v
      
      # validation data n/K
      vY<-as.matrix(Y[((k-1)*n/K+1):((k-1)*n/K+n/K),1])
      vX<-X[((k-1)*n/K+1):((k-1)*n/K+n/K),]
      
      for(l in 1:length(Lambda2)){ # trying different values for lambda2 in the range of Lambda2
        # training for lava
        H1<-M1%*%MASS::ginv(t(M1)%*%M1+(n-n/K)*Lambda2[l]*diag(p))%*%t(M1)
        Khalf1<-U1%*%pracma::sqrtm(diag(n-n/K)-H1)$B%*%t(U1)  # square root of (I-the ridge-projected matrix), which is square root of \tilde K in the paper's notation
        tY1=Khalf1%*%Y1; tX1=Khalf1%*%X1; # these are transformed data
        DELTA_temp<-glmnet::glmnet(tX1,tY1,lambda = Lambda1) # returns a matrix of vectors of "sparse estimators" for different values of lambda1, chosen from the range Lambda1.
        DELTA<-as.matrix(DELTA_temp$beta[,ncol(DELTA_temp$beta):1])
        BETAlava<-pracma::mldivide((S1+Lambda2[l]*diag(p)),t(X1))%*%(Y1%*%matrix(1,1,length(Lambda1))-X1%*%DELTA)/(n-n/K) # each column is a vector of "dense estimators".
        THETA<-DELTA+BETAlava  # p by L each column is a vector of "LAVA estimators".
        
        # validation for lava
        residual<-vX%*%THETA-vY%*%matrix(1,1,length(Lambda1))  # each column is a vector of LAVA residuals fitted on the validation data. (again, there are L columns, 
        # where L=length(Lambda1) represents the number of tried lambda1 values in the range of Lambda1)
        elava_array[k,,l]<-colSums((residual^2))  # sum of residual squares # 1 by L; elava_array(k,b,l)  k: K fold, b: Lambda1's choice, l: Lambda2's choice
        rm(residual)
        
        # post lava
        respost<-matrix(0,nrow(vX),length(Lambda1))
        for(j in 1:length(Lambda1)){
          use<-abs(DELTA[,j])>1e-7
          if(sum(use)>0){
            XJ1<-X1[,use];
            ww<-Y1-X1%*%BETAlava[,j]
            # In below,pracma::mldivide((t(XJ1)%*%XJ1),XJ1)%*%ww  represents the estimated nonzero elements of the sparse components,using post lava
            # vX*BETAlava(:,j) represents the fitted dense part on the validation data
            respost[,j]<-vX[,use]%*%(pracma::mldivide((t(XJ1)%*%XJ1),t(XJ1))%*%ww)+vX%*%BETAlava[,j]-vY # each column is a vector of post-lava residuals fitted on the validation data.
            rm(use,XJ1,ww)
          } else{
            respost[,j]<-vX%*%BETAlava[,j]-vY
          }
        }
        elavapost[k,,l]<-colSums((respost^2)) # sum of residual squares
        rm(respost)
      }#end l
    }#end k
    
    # By now we have evaluated all the lambda1 and lambda2 for all the K possible splitted data sets        
    # lava fit by choosing the best lambda1 and lambda2, and then refitting the entire data set. 
    CVRlava<-matrix(0,length(Lambda2),ncol(THETA))
    for(l in 1:length(Lambda2)){
      CVRlava[l,]<-colMeans(elava_array[,,l]) # CVRlava(l,g): l: Lambda2(l)   g:  Lambda1(g) 
    }#end l
    a<-matrix(0,1,ncol(CVRlava)); b<-matrix(0,1,ncol(CVRlava))
    for(i in 1:ncol(CVRlava)){
      a[1,i]<-min(CVRlava[,i])
      b[1,i]<-which.min(CVRlava[,i])
    }
    c<-min(a); d<-which.min(a)
    lambda2<-Lambda2[b[1,d]]; lambda1<-Lambda1[d] # optimal choice of lambda1 and lambda2 for lava
    lambda1lava<-lambda1; lambda2lava<-lambda2
    P<-pracma::mrdivide(X,(S+lambda2*diag(p)))%*%t(X)/n; Kmatrix<-diag(n)-P
    H<-M%*%MASS::ginv(t(M)%*%M+n*lambda2*diag(p))%*%t(M)
    Khalf<-U%*%pracma::sqrtm(diag(n)-H)$B%*%t(U) # square root of K matrix
    tY<-Khalf%*%Y; tX<-Khalf%*%X  # transfored data
    deltahat_temp<-glmnet::glmnet(tX,tY,lambda=lambda1) # deltahat is the final estimator of the sparse component
    deltahat<-as.matrix(deltahat_temp$beta[,ncol(deltahat_temp$beta):1])
    dense<-pracma::mldivide((S+lambda2*diag(p)),t(X))%*%(Y-X%*%deltahat)/n  # final estimator of the dense component
    lavatheta<-deltahat+dense # final estimator of lava 
    # Prelava<-P*Y+Kmatrix*X* deltahat; this is just an equivalent expression for X*lavatheta. One can also check if Prelava is almost the same as X*lavatheta, making sure the code is correct
    
    # Post-lava fit by choosing the best lambda1 and lambda2, and then refitting the entire data set. 
    CVRpostlava<-matrix(0,length(Lambda2),ncol(THETA))
    for(l in 1:length(Lambda2)){
      CVRpostlava[l,]<-colMeans(elavapost[,,l]) # CVRpostlava: l: Lambda2(l)   g:  Lambda1(g)
    }
    a<-matrix(0,1,ncol(CVRpostlava)); b<-matrix(0,1,ncol(CVRpostlava))
    for(i in 1:ncol(CVRpostlava)){
      a[1,i]<-min(CVRpostlava[,i])
      b[1,i]<-which.min(CVRpostlava[,i])
    }
    c<-min(a); d<-which.min(a)
    lambda2<-Lambda2[b[1,d]]; lambda1<-Lambda1[d] # optimal choice of lambda1 and lambda2 for lava
    lambda1post<-lambda1; lambda2post<-lambda2
    P<-pracma::mrdivide(X,(S+lambda2*diag(p)))%*%t(X)/n; # Kmatrix<-diag(n)-P
    H<-M%*%MASS::ginv(t(M)%*%M+n*lambda2*diag(p))%*%t(M)
    Khalf<-U%*%pracma::sqrtm(diag(n)-H)$B%*%t(U) # square root of K matrix
    tY<-Khalf%*%Y; tX<-Khalf%*%X  # transfored data
    deltahat_post_temp<-glmnet::glmnet(tX,tY,lambda=lambda1) # deltahat is the final estimator of the sparse component
    deltahat_post<-as.matrix(deltahat_post_temp$beta[,ncol(deltahat_post_temp$beta):1])
    post_dense<-pracma::mldivide((S+lambda2*diag(p)),t(X))%*%(Y-X%*%deltahat_post)/n  # post-lava estimator of the dense component
    use<-abs(deltahat_post)>1e-7
    post_sparse<-matrix(0,p,1)
    if(sum(use)>0){
      XJ<-X[,use]
      post_sparse[use]<-pracma::pinv(t(XJ)%*%XJ)%*%t(XJ)%*%(Y-X%*%post_dense) # post-lava estimator of the sparse component
    }  
    post_lavatheta<-post_dense+post_sparse # final estimator of post-lava 
    
    #clear row and column names
    rownames(dense) <- NULL
    colnames(dense) <- NULL
    
    rownames(deltahat) <- NULL
    colnames(deltahat) <- NULL
    
    rownames(lavatheta) <- NULL
    colnames(lavatheta) <- NULL
    
    rownames(post_dense) <- NULL
    colnames(post_dense) <- NULL
    
    rownames(post_sparse) <- NULL
    colnames(post_sparse) <- NULL
    
    rownames(post_lavatheta) <- NULL
    colnames(post_lavatheta) <- NULL
      
    #outputs:
    LAMBDA<-c(lambda1lava, lambda2lava, lambda1post, lambda2post) 
    output_list<-list("lava_dense"=dense,"lava_sparse"=deltahat,"lava_estimate"=lavatheta,"postlava_dense"=post_dense,"postlava_sparse"=post_sparse,"post_lava"=post_lavatheta,"LAMBDA"=LAMBDA)
    rm(list=setdiff(ls(), "output_list"))
  }#end lava_prof
  return(output_list)
}

