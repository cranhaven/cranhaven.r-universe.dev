digamma <- function(x){

  #Calculates the digamma function.
  #
  #Multiple evaluations should enter as a row vector.
  #
  #Thanks to Zoubin Ghahramani and Yw Teh for helping put this fast
  #version together.
  #
  #Matthew J. Beal GCNU 06/02/01

  coef=matrix(c(-1/12,1/120,-1/252,1/240,-1/132,691/32760,-1/12),nrow = 1)
  krange = matrix(1:7,ncol = 1);

  y = ceiling(pmax(0,6-x));

  z = x + y;

  logz = log(z);

  res = logz - .5/z + coef%*%exp(-2*krange%*%logz) -
    (y>=1)/x - (y>=2)/(x+1) - (y>=3)/(x+2) - (y>=4)/(x+3) -
    (y>=5)/(x+4) - (y>=6)/(x+5);

  return(res);
}

dobirth <- function(Y,Lm,psii,parent,Qns,Lcov,b,u,pu,alpha, Xcov, trXm, Xm, mean_mcl,nu_mcl,a,num, verbose){

  # dobirth.m : This script performs a birth operation. It has
  # considerable flexibility in the way it does this, along with a few
  # tweakables, but here the operation is to split the
  # *responsibilities* of the parent component.

  # Please email m.beal@gatsby.ucl.ac.uk for more information.

  s = length(Lm);
  n = dim(Y)[2];
  p = dim(Y)[1]

  s = s+1;
  t = s; # t is the identity of the newborn.

  hardness = 1; # 1 means hard, 0.5 means soft, 0 is equiv to 1 (opposite direction)
  
  if(verbose){
  print(paste('Creating component ', t , ' as a hybrid of component ', parent ))}
  
  # Sample from the full covariance ellipsoid of the component
  Lm[[t]] = Lm[[parent]];
  delta_vector = matrix(MASS::mvrnorm(1,matrix(0,nrow = 1, ncol = p),Lm[[t]][,-1] %*% t(Lm[[t]][,-1]) +diag(1/psii),1),ncol = 1);
  #delta_vector = matrix((1/p)*(1:p),ncol = 1)
  # Qns birth
  assign = sign( t(delta_vector)%*%(Y- matrix(rep(Lm[[parent]][,1],n), ncol = n)  ) ); # size 1 x n
  pos_ind = matrix(assign == 1,ncol = 1);
  neg_ind = matrix(assign == -1, ncol = 1);
  # Reassign those one side of vector to the child, t,
  # whilst the rest remain untouched. Positive are sent to child
  Qns = cbind(Qns, matrix(Qns[,1]))
  Qns[pos_ind,t] = hardness*Qns[pos_ind,parent];
  Qns[pos_ind,parent] = (1-hardness)*Qns[t(pos_ind),parent];
  Qns[neg_ind,t] = (1-hardness)*Qns[t(neg_ind),parent];
  Qns[neg_ind,parent] = hardness*Qns[t(neg_ind),parent];
  # set all features of t to those of t_parent
  Lcov[[t]] = Lcov[[parent]];
  Lm[[t]][,1]      = Lm[[t]][,1]      + delta_vector;
  Lm[[parent]][,1] = Lm[[parent]][,1] - delta_vector;
  b[[t]] = b[[parent]];
  u[parent] = u[parent]/2; u[t] = u[parent];
  pu = alpha/s * matrix(1,nrow=1,ncol = s);

  # Update Q(X)Q(L) posterior (twice?)
  tempOut <- inferQX(Y,Lm,Lcov,psii,Xcov,trXm,Xm)
  Xcov = tempOut$Xcov
  trXm = tempOut$trXm
  Xm = tempOut$Xm
  tempOut <- inferQL(Y,Lm,mean_mcl,nu_mcl,a,b,Xm,Qns,Xcov,psii,Lcov,num)
  mean_Lambda = tempOut$mean_Lambda
  num = tempOut$num
  Lcov = tempOut$Lcov
  Lm = tempOut$Lm
  tempOut <- inferQX(Y,Lm,Lcov,psii,Xcov,trXm,Xm)
  Xcov = tempOut$Xcov
  trXm = tempOut$trXm
  Xm = tempOut$Xm
  tempOut <- inferQL(Y,Lm,mean_mcl,nu_mcl,a,b,Xm,Qns,Xcov,psii,Lcov,num)
  mean_Lambda = tempOut$mean_Lambda
  num = tempOut$num
  Lcov = tempOut$Lcov
  Lm = tempOut$Lm


  components = 1:length(Lm);
  cophd = 0;

  return(list(Lcov = Lcov, Lm = Lm, b=b, u=u, pu = pu,
              mean_Lambda = mean_Lambda, num = num,
              Xcov = Xcov, trXm = trXm, Xm = Xm, Qns = Qns))

}

Fcalc <- function(Lm,Y,Qns,F_,a,u,pu,Xm,Xcov,Ps,psii,pi,b,pa,pb,nu_mcl,mean_mcl,it,alpha, Lcov, Fhist){

  # Fcalc.m : This script calculates the lower bound on the log evidence
  # using the formula for $\cF$.
  #browser()
  s = length(Lm);
  p = dim(Y)[1];
  n = dim(Y)[2];



  Ps = matrix(colSums(Qns),nrow = 1)/n;
  F_old = F_;

  Fmatrix = matrix(0,nrow = 6, ncol = s)
  tempdig_a = digamma(a);
  tempdig_u = digamma(u);

  if(is.null(dim(u))){
    u_sum = u #In case u is a scalar because this causes issues
  } else { u_sum = rowSums(u) }
  tempdig_usum = digamma(matrix(u_sum,ncol = 1)); 
  pu = alpha/s *matrix(1,nrow = 1, ncol = s);
  Qnsmod = Qns;
  Qnsmod[Qnsmod==0] = 1;
  
  FmatrixKLpi = -kldirichlet(as.matrix(u,nrow = 1),as.matrix(pu,nrow = 1));

  for(t in 1:s){
    kt = dim(Lm[[t]])[2];
    temp_alt = Xm[[t]]*matrix(rep(matrix(Qns[,t],nrow=1),kt), nrow = kt, byrow = TRUE)
    Xcor_t_Qns_weighted = Xcov[[t]]*sum(Qns[,t])  +  Xm[[t]]%*%t(temp_alt);
    Fmatrix[2,t] = sum( matrix(Qns[,t],ncol=1)*( -log(matrix(Qnsmod[,t],ncol= 1)) + matrix(1,nrow = n, ncol = 1)%*%(tempdig_u[t]-tempdig_usum) ) );
    Fmatrix[3,t] = .5*n*(kt-1)*Ps[t] -
      .5*sum(diag(  Xcor_t_Qns_weighted[-1, -1]  )) +
      .5*n*Ps[1,t]*(
        2*sum(log(diag(chol(Xcov[[t]][-1,-1])))) );

    Fmatrix[4,t] = -.5*n*Ps[t]*( -log(det(diag(as.vector(psii)))) +p*log(2*pi) ) -
      .5*sum(diag( t(Lm[[t]]) %*% diag(as.vector(psii)) %*% Lm[[t]] %*% Xcor_t_Qns_weighted )) -
      .5*sum(diag( matrix(matrix(Lcov[[t]],nrow = kt*kt,ncol = p)%*%psii,nrow = kt,ncol = kt) %*% Xcor_t_Qns_weighted ) ) -
      .5*sum(diag( diag(as.vector(psii)) %*% (matrix(1,nrow = p, ncol = 1)%*%matrix(Qns[,t],nrow = 1) * Y ) %*% t(Y-2*Lm[[t]]%*%Xm[[t]]) ));
    Fmatrix[6,t] = -klgamma(matrix(a),matrix(b[[t]],nrow = 1),pa,pb);
  }
  priorlnnum = list()
  priornum = list()

  for(t in 1:s){
    f1 = 0;
    priorlnnum[[t]] = cbind(log(nu_mcl),matrix(rep(matrix(rep(tempdig_a, dim(b[[t]])[2]), ncol = dim(b[[t]])[2]) -log(b[[t]]),p),nrow = p, byrow = TRUE)) ;
    priornum[[t]] = cbind(nu_mcl, matrix(rep(a/b[[t]],p),nrow = p, byrow = TRUE));
    f1 = f1 + sum(priorlnnum[[t]]);
    for(q in 1:p){
      f1 = f1 + log(det(Lcov[[t]][,,q])) - dim(Lcov[[t]][,,q])[1];
      f1 = f1 - ( t(diag(Lcov[[t]][,,q])) +Lm[[t]][q,]^2 ) %*% matrix(priornum[[t]][q,],ncol = 1);
      f1 = f1 - priornum[[t]][q,1]*( -2*Lm[[t]][q,1]*mean_mcl[q,1] + mean_mcl[q,1]^2);
    }
    Fmatrix[1,t] = f1/2;
  }

  F_ = sum(Fmatrix) + FmatrixKLpi;
  dF = F_-F_old;
  Fhist = rbind(Fhist,cbind(it,F_));

  return(list(Fmatrix = Fmatrix, pu = pu, Ps = Ps, F_old = F_old,
              Qnsmod = Qnsmod, FmatrixKLpi = FmatrixKLpi, F_ = F_,
              dF = dF, Fhist = Fhist))

}

infermcl <- function(Lm,Lcov){


  # infermcl : Infers the hyperparameters governing the mean
  # (mean_mcl) and the precisions on the mean (nu_mcl) of all the factor
  # loading matrices.

  s = length(Lm);

  if(s>1){
    temp_mcl =  abind::abind(Lm, along=3)
    temp_mcl = drop(temp_mcl[,1,]); # now p x s
    temp_Lcov = abind::abind(Lcov, along=4);

    mean_mcl = matrix(rowMeans(temp_mcl),ncol=1);
    nu_mcl = s/( matrix(rowSums(drop(temp_Lcov[1,1,,])),ncol = 1) + #squeeze gives p x s
                   matrix(rowSums(temp_mcl^2),ncol = 1)  -
                   2*mean_mcl*matrix(rowSums(temp_mcl),ncol = 1) +
                   s*mean_mcl^2 )
    return(list(mean_mcl = mean_mcl, nu_mcl = nu_mcl))
  } else {
    return("no change")
  }

}

inferpsii <- function(Y,Lm,Xm,Qns,Xcov,Lcov,pcaflag,psimin){

  # inferpsii : This function calculates the ML estimate for the
  # hyperparameters of the sensor noise in the $\Psi$ matrix.

  n = dim(Y)[2];
  p = dim(Y)[1];
  s = length(Lm);

  psi = matrix(0,nrow = p, ncol = p); zeta = 0;

  for(t in 1:s){
    kt = dim(Lm[[t]])[2]
    temp_alt = Xm[[t]]*matrix(rep(matrix(Qns[,t],nrow = 1),kt),nrow = kt, byrow = TRUE);
    temp = Xcov[[t]]*sum(Qns[,t])  +  Xm[[t]]%*%t(temp_alt);
    if(pcaflag == 0){
      psi = psi + (matrix(rep(matrix(Qns[,t],nrow = 1),p), nrow = p, byrow = TRUE)*Y) %*% t(Y-2*Lm[[t]]%*%Xm[[t]]) +
        Lm[[t]]%*%temp%*%t(Lm[[t]])
      for(q in 1:p){
        psi[q,q] = psi[q,q] + sum(diag( Lcov[[t]][,,q]%*%temp ));
      }
    } 
  }

  # psii is the inverse of the noise variances, and is a column vector.
  # Note the PCA analogue produces isotropic noise with the averaged
  # variance of the sensors in FA.

  if(pcaflag == 0){
    psi = 1/n * psi;
    psii = matrix(1/diag(psi),ncol = 1);
    if(sum(1/psii < psimin) > 0){ #Check this condition
      psii[pracma::finds(psii>(1/psimin))] = 1/psimin;
    }
  } else {
    psii = n*p*matrix(1,nrow = 1, ncol = p)*zeta^-1;
  }

  return(list(psi = psi, psii = psii, zeta = zeta))

}

inferQL <- function(Y,Lm,mean_mcl,nu_mcl,a,b,Xm,Qns,Xcov,psii,Lcov,num){

  # inferQL.m : This script calculates the sufficient statistics for the
  # variational posterior over factor loading matrix entries.

  n = dim(Y)[2];
  p = dim(Y)[1];
  s = length(Lm);

  for(t in 1:s){
    kt = dim(Lm[[t]])[2];
    mean_Lambda = cbind(mean_mcl,matrix(0,nrow = p, ncol = kt-1));
    num[[t]] = cbind(nu_mcl,matrix(rep(a/b[[t]],p),nrow = p, byrow = TRUE));
    temp = Xm[[t]]*matrix(rep(matrix(Qns[,t],nrow = 1),kt),nrow = kt, byrow = TRUE);
    T2 = Xcov[[t]]*sum(Qns[,t]) + Xm[[t]]%*%t(temp);
    T3 = diag(as.vector(psii))%*%Y%*%t(temp);
    for(q in 1:p){
      Lcov[[t]][,,q] = solve( diag(as.vector(num[[t]][q,])) + psii[q]*T2);
      Lm[[t]][q,] = ( matrix(T3[q,],nrow = 1) + matrix(mean_Lambda[q,],nrow = 1)%*%diag(as.vector(num[[t]][q,]))) %*%
        Lcov[[t]][,,q];
    }
  }

  return(list(mean_Lambda = mean_Lambda, num = num, Lcov = Lcov, Lm = Lm))

}


inferQns <- function(Y,Lm,Qns,psii,Lcov,Xm,Xcov,candorder,b,u,pu,removal,parent,pos,cophd, verbose){
  # inferQns : This function calculates the variational posterior for
  # the class-conditional responsibilities for the data. It also removes
  # from the model any components which have zero (or below threshold)
  # total responsibility.

  n = dim(Y)[2];
  p = dim(Y)[1];
  s = length(Lm);

  allocprobs = matrix(rowSums(Qns),ncol=1);
  Qns_old = Qns;
  logQns = matrix(nrow=n);
  for(t in 1:s){
    kt = dim(Lm[[t]])[2];
    LmpsiiLm = t(Lm[[t]])%*%diag(as.vector(psii))%*%Lm[[t]];
    temp = LmpsiiLm + matrix(matrix(Lcov[[t]],nrow=kt*kt,ncol=p)%*%psii,nrow=kt,ncol=kt);

    logQns = cbind(logQns,-.5*(  matrix(colSums(Y*(diag(as.vector(psii))%*%(Y-2*Lm[[t]]%*%Xm[[t]]))),ncol=1) +
                                   as.numeric(t(matrix(temp,nrow=kt*kt,ncol=1))%*%matrix(Xcov[[t]],nrow = kt*kt,ncol = 1))+
                                   matrix(colSums( Xm[[t]]*(temp%*%Xm[[t]])),ncol=1) +
                                   sum(diag(Xcov[[t]][-1,-1]), na.rm = TRUE)*matrix(1,ncol=1,nrow=n) + #TRACE
                                   matrix(colSums( Xm[[t]][-1,]*Xm[[t]][-1,]),ncol = 1) -
                                   2*sum(log(diag(chol(Xcov[[t]][-1,-1]))))*matrix(1,ncol=1,nrow=n))); #Check chol results for consistency
  } #t
  logQns = matrix(logQns[,-1],nrow=n)
  logQns = logQns + matrix(rep(as.numeric(digamma(u)), nrow(logQns)), nrow = nrow(logQns), byrow = TRUE);
  logQns = logQns - Rfast::rowMaxs(logQns,value=TRUE)%*%matrix(1,nrow = 1, ncol = s);
  Qns[,1:s] = exp(logQns);
  Qns[,1:s] = Qns * (  matrix((allocprobs/rowSums(Qns)),ncol=1) %*% matrix(1,nrow = 1, ncol = s)  );

  # check for any empty components, and remove them if allowed
  if(removal==1){
    empty_t = which(colSums(Qns) < 1)
    num_died = length(empty_t);
    # if there exist components to remove, do so
    if(num_died > 0){
      remain = 1:length(Lm); remain = remain[-empty_t];
      if(verbose){
      print(paste('Removing component: ', empty_t))};
      # ascertain if a cophd
      num_children = length(intersect(empty_t,cbind(parent,length(Lm))));
      if(num_children!=num_died){
        cophd=0; # i.e. require reordering
      } else {
        if(num_children == 1){
          if(verbose){
          print('Child of parent has died')}
          cophd = 1;
          decleft = candorder[1:(pos-1)]>parent; 
          decright = candorder[(pos+1):dim(candorder)[2]]>parent;
          if(empty_t == parent){
            if(verbose){
            print('Dead component is original parent - need to change ordering');
            print(paste('Old order was ', toString(candorder)))}
            decrement = candorder[1:(pos-1)]>parent;
            candorder = matrix(c(candorder[1:(pos-1)]-decleft,length(Lm)-1,candorder[(pos+1):dim(candorder)[2]]-decright),nrow = 1);
            if(verbose){
            print(paste('New order is ', toString(candorder)))};
          } else {
            if(verbose){
            print('Dead component is newly born - no change to component ordering')};
          }
        }
        if(num_children == 2){
          cophd = 1;
          decleft = candorder[1:(pos-1)]>parent;
          decright = candorder[(pos+1):length(candorder)]>parent;
          if(verbose){
          print('Both children died - need to change ordering');
          print(cat('Old order was ', toString(candorder)))}
          candorder = matrix(c(candorder[1:(pos-1)]-decleft,candorder[(pos+1):length(candorder)]-decright),nrow = 1); 
          if(verbose){
          print(cat('New order is', toString(candorder)))};
          pos = pos-1; # because 2 components died.
        }
      }

      Lm = Lm[remain];
      Lcov = Lcov[remain];
      Xm = Xm[remain];
      Xcov = Xcov[remain];
      b = b[remain];
      u = u[remain];
      pu = pu[remain];
      s = length(remain);
      if(length(remain) == 1){
        Qns = matrix(Qns[,remain], ncol = 1)
      } else {
      Qns = Qns[,remain]};
      tempOut <- inferQns(Y,Lm,Qns,psii,Lcov,Xm,Xcov,candorder,b,u,pu,removal,parent,pos, cophd,verbose) # N.B. this may cause stacking.
      Qns = tempOut$Qns; Ps = tempOut$Ps; Lm = tempOut$Lm; Lcov = tempOut$Lcov;
      Xm = tempOut$Xm; Xcov = tempOut$Xcov; b = tempOut$b; u = tempOut$u;
      pu = tempOut$pu; s = tempOut$s; dQns_sagit = tempOut$dQns_sagit; Qns_old = tempOut$Qns_old;
      candorder = tempOut$candorder; cophd = tempOut$cophd
    }
  }

  if(dim(Qns_old)[1] == dim(Qns)[1] & dim(Qns_old)[2] == dim(Qns)[2]){
    dQns = abs(Qns-Qns_old);
    dQns_sagit = (colSums(dQns)/colSums(Qns)); # The percentage absolute movement
  } else {
    dQns_sagit = matrix(1,nrow=1,ncol=dim(Qns)[2]); # i.e. 100% agitated
  }

  Qns = Qns/matrix(rep(rowSums(Qns),dim(Qns)[2]),ncol = dim(Qns)[2]);
  Ps = colSums(Qns)/dim(Qns)[1];

  return(list(Qns = Qns, Ps = Ps, Lm = Lm, Lcov = Lcov, Xm = Xm, Xcov = Xcov,
              b = b, u = u, pu = pu, s = s, dQns_sagit = dQns_sagit, Qns_old = Qns_old,
              candorder = candorder, cophd = cophd))

}


inferQnu <- function(Lm,pa,pb,Lcov,b){

  # inferQnu : This script calculates the sufficient statistics for
  # the variational posterior over precision parameters for each column
  # of each factor loading matrix.

  s = length(Lm);

  a = pa + .5*dim(Lm[[1]])[1];
  for(t in 1:s){
    kt = dim(Lm[[t]])[2];
    b[[t]] = pb*matrix(1,nrow = 1, ncol = kt - 1) +
      .5*( matrix(diag(rowSums(Lcov[[t]][-1,-1,],dims  = 2)),nrow = 1) + matrix(colSums(Lm[[t]][,-1]^2),nrow=1) );
  }
  return(list(s = s, b = b , a = a))
}

inferQpi <- function(Lm,alpha,Qns){

  # inferQp : This function infers the sufficient statistics for the
  # variational posterior for the Dirichlet parameter $\bpi$.

  s = length(Lm);
  pu = alpha/s * matrix(1,nrow = 1, ncol = s);

  u = pu + matrix(colSums(Qns), nrow = 1);
  return(list(s = s, pu = pu, u = u))
}

inferQX <- function(Y,Lm,Lcov,psii,Xcov,trXm,Xm){

  #inferQX: This script calculates the sufficient statistics for the
  # variational posterior over hidden factors.

  n = dim(Y)[2];
  p = dim(Y)[1];
  s = length(Lm);

  for(t in 1:s){
    kt = dim(Lm[[t]])[2];
    T1 = matrix(matrix(Lcov[[t]][-1,-1,],nrow = (kt-1)*(kt-1),ncol = p)%*%psii,nrow = kt-1,ncol = kt-1) +
      t(Lm[[t]][,-1])%*%diag(as.vector(psii))%*%Lm[[t]][,-1];
    Xcov[[t]]              = matrix(0,nrow = kt, ncol = kt);
    Xcov[[t]][-1,-1] = solve( diag(kt-1)+T1 );
    trXm[[t]]              = Xcov[[t]][-1,-1]%*%t(Lm[[t]][,-1])%*%diag(as.vector(psii))%*%(Y-matrix(rep(Lm[[t]][,1],n),nrow = p));
    Xm[[t]]                = rbind(matrix(1,nrow = 1, ncol = n),trXm[[t]]);
  }
  out = list(Xcov = Xcov, trXm = trXm, Xm = Xm)
}

kldirichlet <- function(vecP,vecQ){

  #res = kldirichlet(vecP,vecQ)
  #
  #Calculates KL(P||Q) where P and Q are Dirichlet distributions with
  #parameters 'vecP' and 'vecQ', which are row vectors, not
  #necessarily normalised.
  #
  # KL(P||Q) = \int d\pi P(\pi) ln { P(\pi) / Q(\pi) }.
  #
  #Matthew J. Beal GCNU 06/02/01
  
  alphaP = rowSums(vecP)
  alphaQ = rowSums(vecQ)


  kl = lgamma(alphaP)-lgamma(alphaQ) -
    rowSums(lgamma(vecP)-lgamma(vecQ)) +
    rowSums( (vecP-vecQ) * (digamma(vecP)-as.numeric(digamma(alphaP))));
  return(kl)
}


klgamma <- function(pa,pb,qa,qb){

  #kl = klgamma(pa,pb,qa,qb);
  #
  #Calculates KL(P||Q) where P and Q are Gamma distributions with
  #parameters {pa,pb} and {qa,qb}.
  #
  # KL(P||Q) = \int d\pi P(\pi) ln { P(\pi) / Q(\pi) }.
  #
  #This routine handles factorised P distributions, if their parameters
  #are specified multiply in either 'pa' or 'pb', as elements of a row
  #vector.
  #
  #Matthew J. Beal GCNU 06/02/01

  #pa, pb should be matrix objects. I.e.
  #pa <- matrix(2); pb <- matrix(3) would be used if pa = 2 & pb=3
  #If multiple pa's or pb's is to be defined, they should be given as
  #row vectors using matrix(..., nrow = 1).

  #qa,qb should be scalars

  n = max(dim(pb)[2], dim(pa)[2]);

  if(dim(pa)[2] == 1){pa = as.numeric(pa)*matrix(rep(1,n),nrow=1)}
  if(dim(pb)[2] == 1){pb = as.numeric(pb)*matrix(rep(1,n),nrow=1)}
  qa = qa*matrix(rep(1,n),nrow=1); qb = qb*matrix(rep(1,n),nrow=1);

  kl = rowSums( pa*log(pb)-log(gamma(pa)) -
                  qa*log(qb)+log(gamma(qa)) +
                  (pa-qa)*(digamma(pa)-log(pb)) -
                  (pb-qb)*pa/pb);
}

learn <- function(it, Y,Lm,Lcov,psii,Xcov,trXm,Xm,
                  mean_mcl,nu_mcl,a,b,Qns,candorder,u,pu,
                  removal, alpha, pa, pb, pcaflag, psimin, num, parent, pos,cophd, verbose){
  it = it + 1

  tempOut <- inferQX(Y,Lm,Lcov,psii,Xcov,trXm,Xm)
  Xcov = tempOut$Xcov; trXm = tempOut$trXm; Xm = tempOut$Xm

  tempOut <- inferQL(Y,Lm,mean_mcl,nu_mcl,a,b,Xm,Qns,Xcov,psii,Lcov,num)
  mean_Lambda = tempOut$mean_Lambda; num = tempOut$num;
  Lcov = tempOut$Lcov; Lm = tempOut$Lm;
  #problem here on second loop through <---
  tempOut <- inferQns(Y,Lm,Qns,psii,Lcov,Xm,Xcov,candorder,b,u,pu,removal,parent,pos,cophd,verbose)
  Qns = tempOut$Qns; Ps = tempOut$Ps; Lm = tempOut$Lm; Lcov = tempOut$Lcov;
  Xm = tempOut$Xm; Xcov = tempOut$Xcov;
  b = tempOut$b; u = tempOut$u; pu = tempOut$pu; s = tempOut$s;
  dQns_sagit = tempOut$dQns_sagit; candorder = tempOut$candorder;
  cophd = tempOut$cophd;

  tempOut <- inferQpi(Lm,alpha,Qns)
  s = tempOut$s; pu = tempOut$pu; u = tempOut$u;

  tempOut <- inferQnu(Lm,pa,pb,Lcov,b)
  s = tempOut$s; b = tempOut$b; a = tempOut$a;

  tempOut <- inferpsii(Y,Lm,Xm,Qns,Xcov,Lcov,pcaflag,psimin)
  psi = tempOut$psi; psii = tempOut$psii; zeta = tempOut$zeta;

  if(length(Lm) > 1){
    tempOut <- infermcl(Lm,Lcov)
    mean_mcl = tempOut$mean_mcl; nu_mcl = tempOut$nu_mcl
  }


  return(list(it = it, Xm=Xm, trXm= trXm, Xcov = Xcov, mean_Lambda = mean_Lambda,
              num = num, Lcov = Lcov, Lm = Lm, Qns = Qns, Ps = Ps, b = b, u = u,
              pu = pu, s = s, dQns_sagit = dQns_sagit, a = a, psi = psi, psii = psii,
              zeta = zeta, mean_mcl = mean_mcl, nu_mcl = nu_mcl, candorder = candorder,
              cophd = cophd))

}


ordercands <- function(Y,Lm,Fmatrix,Qns,getbeta,selection_method, verbose){

  # ordercands. This script orders the candidates according to
  # a selection method.
  #
  # Please email m.beal@gatsby.ucl.ac.uk for more information.
  #
  # Matthew J. Beal

  s = length(Lm);
  n = dim(Y)[2];

  s = s+1;
  t = s;

  selection_method = 4;

  if (selection_method == 1){
    parent = ceiling(rnorm(1)*(s-1))

  } else if (selection_method == 2){
    #If this was used a rej_count variable would need to be specified.
    #We are only concerned with case 4 so this doesnt matter

  } else if (selection_method == 3) {
    if(length(Lm) == 1){
      parent = t-1;
      return(parent)
    } else {
      free_energy = -( matrix(colSums(matrix(Fmatrix[c(2,3,4),],ncol = ncol(Fmatrix))), nrow = 1)/matrix(colSums(Qns),nrow = 1) +
                         matrix(colSums(matrix(Fmatrix[c(6,1),], ncol = ncol(Fmatrix))), nrow = 1) );
      beta = getbeta[free_energy];
      probs = exp[beta*free_energy];
      probs = probs/matrix(rowSums(probs),ncol = 1);
      parent = which.max(cumsum(probs)>rnorm(1))
      
      order_ = order(probs)
      dummy <- probs[order_]
      if(verbose){
      print(paste("Creation order: ", order_[length(order_):1],
                  ' (', dummy[length(dummy):1] , ' )' ))}

    }
  } else if (selection_method == 4) {

    free_energy = -( matrix(colSums(matrix(Fmatrix[c(2,3,4),],ncol = ncol(Fmatrix))), nrow = 1)/matrix(colSums(Qns),nrow = 1) +
                       matrix(colSums(matrix(Fmatrix[c(6,1),],ncol = ncol(Fmatrix))), nrow = 1) );
    candorder = order(free_energy);
    candorder = candorder[length(candorder):1];
    if(verbose){
    print(paste('Creation order: ', toString(candorder)))}

  } else {
    stop("Error in ordercands: Invalid selection_method value.")
  }


  candorder = matrix(c(candorder,0),nrow = 1)

  pos = 1;

  return(list(candorder = candorder, pos = pos))

}

