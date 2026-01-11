

#################### sub functions for smooth tensor estimations ####################
# polynomial approximation function
polytensor = function(tensor, l, kvec){
  d=dim(tensor)
  est=array(dim=d)
  z1=rep(1:kvec[1],rep(floor(d[1]/kvec[1]),kvec[1]))
  z1=c(z1,rep(kvec[1],d[1]-length(z1)))
  z2=rep(1:kvec[2],rep(floor(d[2]/kvec[2]),kvec[2]))
  z2=c(z2,rep(kvec[2],d[2]-length(z2)))
  z3=rep(1:kvec[3],rep(floor(d[3]/kvec[3]),kvec[3]))
  z3=c(z3,rep(kvec[3],d[3]-length(z3)))

  for(i in 1:kvec[1]){
    for(j in 1:kvec[2]){
      for(q in 1:kvec[3]){
        subtensor=tensor[which(z1==i),which(z2==j),which(z3==q),drop = F]
        X1=c(slice.index(subtensor,1))
        X2=c(slice.index(subtensor,2))
        X3=c(slice.index(subtensor,3))
        if(l==0){
          fit=lm(c(subtensor)~1)
          est[which(z1==i),which(z2==j),which(z3==q)]=predict(fit,as.data.frame(cbind(X1,X2,X3)))
        }else{
          fit=lm(c(subtensor)~polym(X1,X2,X3,degree=l,raw=TRUE))
          est[which(z1==i),which(z2==j),which(z3==q)]=predict(fit,polym(X1,X2,X3,degree=l,raw=TRUE))
        }
      }
    }
  }
  return(est)
}


# functions for LSE methods


tensor_unfold = function(tensor,dim=1){
  if (dim == 1) unfold = aperm(tensor,c(3,2,1))
  if (dim == 2) unfold = aperm(tensor,c(1,3,2))
  if (dim == 3) unfold = tensor
  unfold = apply(unfold,3,c)
  if (is.vector(unfold)) return(as.matrix(unfold)) else return(t(unfold))
}

ReNumber = function (Cs,sort = T){
  newCs <- rep(NA, length(Cs))
  if(sort==T){
    uniq <- unique(Cs)
  }else{
    uniq <- sort(unique(Cs))
  }
  for (i in 1:length(uniq)) {
    newCs[Cs == uniq[i]] <- i
  }
  return(newCs)
}

# a function estimating mean tensor given clustering functions for each tensor mode.

UpdateMus_tensor = function (x, Cs, Ds, Es) {
  Cs = ReNumber(Cs,sort = F); Ds = ReNumber(Ds,sort = F); Es = ReNumber(Es,sort = F)

  mus = array(NA, c(length(unique(Cs)), length(unique(Ds)), length(unique(Es))))
  d = dim(mus)
  for (k in 1:d[1]){
    for (r in 1:d[2]){
      for (l in 1:d[3]){
        mus[k,r,l] = mean(x[Cs==k,Ds==r,Es==l],na.rm = T)
      }
    }
  }
  return(mus)
}



# functions for estimating clustering functions:
# high-order spectral clustering method.
HSC = function(x,k,l,r,nstart = 40,sym = F){
  result = list()
  u1 = svd(tensor_unfold(x,1))$u[,1:k,drop = F]
  u2 = svd(tensor_unfold(x,2))$u[,1:l,drop = F]
  u3 = svd(tensor_unfold(x,3))$u[,1:r,drop = F]
  hu1 = svd(tensor_unfold(ttl(as.tensor(x),list(t(u2),t(u3)),ms = c(2,3))@data,1))$u[,1:k]
  hu2 = svd(tensor_unfold(ttl(as.tensor(x),list(t(u1),t(u3)),ms = c(1,3))@data,2))$u[,1:l]
  hu3 = svd(tensor_unfold(ttl(as.tensor(x),list(t(u1),t(u2)),ms = c(1,2))@data,3))$u[,1:r]
  Y1 = hu1%*%t(hu1)%*%tensor_unfold(ttl(as.tensor(x),list(t(hu2),t(hu3)),ms = c(2,3))@data,1)
  Y2 = hu2%*%t(hu2)%*%tensor_unfold(ttl(as.tensor(x),list(t(hu1),t(hu3)),ms = c(1,3))@data,2)
  Y3 = hu3%*%t(hu3)%*%tensor_unfold(ttl(as.tensor(x),list(t(hu1),t(hu2)),ms = c(1,2))@data,3)
  if(sym ==F){
    result$z1  = kmeans(Y1,k,nstart = nstart)$cluster
    result$z2  = kmeans(Y2,l,nstart = nstart)$cluster
    result$z3  = kmeans(Y3,r,nstart = nstart)$cluster
  }else{
    result$z1 =result$z2 =result$z3 = kmeans(Y1,k,nstart = nstart)$cluster
  }
  return(result)
}


# k-means type algorithm for community detection.
# Bal is for symmetric tensor input while Bal_asym for asymmetric tensor input.
Bal = function(A,k){
  n = dim(A)[1]
  z = kmeans(tensor_unfold(A,1),k)$cluster
  E = matrix(nrow = n,ncol = k)
  prob=array(0,dim=rep(k,3))
  for(a in 1:k){
    for(b in 1:k){
      for(c in 1:k){
        prob[a,b,c]=mean(A[which(z==a),which(z==b),which(z==c)])
      }
    }
  }

  for(i in 1:n){
    for(a in 1:k){
      E[i,a]=sum(dbinom(A[i,,],1, prob[a,z,z],log=TRUE))
    }
  }
  z=apply(E,1,which.max)

  return(z)
}


Bal_asym = function(A,kvec){
  d = dim(A)
  z1 = kmeans(tensor_unfold(A,1),kvec[1])$cluster
  z2 = kmeans(tensor_unfold(A,2),kvec[2])$cluster
  z3 = kmeans(tensor_unfold(A,3),kvec[3])$cluster
  E1 = matrix(nrow = d[1],ncol = kvec[1])
  E2 = matrix(nrow = d[2],ncol = kvec[2])
  E3 = matrix(nrow = d[3],ncol = kvec[3])
  prob=array(0,dim=kvec)
  for(a in 1:kvec[1]){
    for(b in 1:kvec[2]){
      for(c in 1:kvec[3]){
        prob[a,b,c]=mean(A[which(z1==a),which(z2==b),which(z3==c)])
      }
    }
  }

  for(i in 1:d[1]){
    for(a in 1:kvec[1]){
      E1[i,a]=sum(dbinom(A[i,,],1, prob[a,z2,z3],log=TRUE))
    }
  }
  z1=apply(E1,1,which.max)

  for(i in 1:d[2]){
    for(a in 1:kvec[2]){
      E2[i,a]=sum(dbinom(A[,i,],1, prob[z1,a,z3],log=TRUE))
    }
  }
  z2 =apply(E2,1,which.max)

  for(i in 1:d[3]){
    for(a in 1:kvec[3]){
      E3[i,a]=sum(dbinom(A[,,i],1, prob[z1,z2,a],log=TRUE))
    }
  }
  z3=apply(E3,1,which.max)
  result = list(z1 = z1,z2 = z2, z3 = z3)
  return(result)
}



#################### main functions for nonparametric tensor estimation ####################



#' Borda count algorithm for nonparametric tensor estimation with unknown permutation.
#'
#' Estimate a signal tensor and permutation from a noisy and incomplete data tensor using Borda count estimation method.
#' @param A A given (possibly noisy and incomplete) data tensor. Missing value should be encoded as \code{NA}.
#' @param l Degree of polynomial approximation.
#' @param kvec A vector of the number of groups for each mode.
#' @param sym Boolean variables representing symmetricity of the signal tensor. Non-symmetric tensor (\code{sym = FALSE}) is default.
#' @return The returned object is a list of components.
#' @return \code{Theta} - An estimated signal tensor based on Borda count estimation.
#' @return \code{permutation} - An estimated permutation based on Borda count estimation.
#' @usage Borda_count(A, l, kvec, sym = FALSE)
#' @references C. Lee and M. Wang. Smooth tensor estimation with unknown permutations. arXiv:2111.04681, 2021.
#' @examples
#'
#' # Generate the noisy observation from smooth tensor and permutation
#' d = 20
#' sim1 = simulation(d,mode = 1)
#' signal_T = sim1$signal
#' observe_T = sim1$observe
#' permutation = sim1$permutation
#'
#' # Estimate signal tensor and permutation
#' kvec = c(3,3,3)
#' result = Borda_count(observe_T,2,kvec,sym = TRUE)
#'
#' # Calculate MSE
#' hatTheta = result$Theta
#' mean((hatTheta-signal_T)^2)
#'
#' @export
#' @import rTensor
#' @importFrom Matrix "invPerm"
#' @import stats

Borda_count = function(A, l, kvec, sym = FALSE){
  d = dim(A)
  #sorting
  if(sym  == T){
    o1 = order(sapply(1:d[1], function(x) sum(A[x,,],na.rm = T)))
    As = A[o1,o1,o1]

    #polynomial block approximation
    est = polytensor(As,l,kvec)

    #sorting back
    invo1 = invPerm(o1)
    Theta = est
    o = list(invo1,invo1,invo1)
    result = list(Theta = Theta,permutation = o)

  }else{
    o1 = order(sapply(1:d[1], function(x) sum(A[x,,],na.rm = T)))
    o2 = order(sapply(1:d[2], function(x) sum(A[,x,],na.rm = T)))
    o3 = order(sapply(1:d[3], function(x) sum(A[,,x],na.rm = T)))
    As = A[o1,o2,o3]

    #polynomial block approximation
    est = polytensor(As,l,kvec)

    #sorting back
    invo1 = invPerm(o1);invo2 = invPerm(o2);invo3 = invPerm(o3)

    Theta = est
    o = list(invo1,invo2,invo3)
    result = list(Theta = Theta, permutation = o)
  }

  return(result)
}




#' Spectral method for nonparametric tensor estimation with unknown permutation.
#'
#' Estimate a permuted signal tensor from a noisy data tensor using spectral method, which performs universal singualr value thresholding on the unfolded tensor.
#' @param A A given noisy data tensor.
#' @param row_idx The indices of the modes that map onto the row space
#' @param col_idx The indices of the modes that map onto the column space
#' @param threshold A threshold to disregard singular values. Default value is the square root of unfolded matrix dimension.
#' @return An estimated permuted signal tensor based on Spectral method.
#' @usage Spectral(A, row_idx, col_idx, threshold = NULL)
#' @references J. Xu. Rates of convergence of spectral methods for graphon estimation. International Conference on Machine Learning, 2018. \cr C. Lee and M. Wang. Smooth tensor estimation with unknown permutations. arXiv:2111.04681, 2021.
#' @examples
#'
#' # Generate the noisy observation from smooth tensor and permutation
#' d = 20
#' sim1 = simulation(d,mode = 1)
#' signal_T = sim1$signal
#' observe_T = sim1$observe
#' permutation = sim1$permutation
#' psignal_T = signal_T[permutation,permutation,permutation]
#'
#' # Estimate permuted signal tensor
#' hatpTheta = Spectral(observe_T,1,c(2,3))
#'
#' # Calculate MSE
#' mean((hatpTheta-psignal_T)^2)
#'
#' @export
#' @import rTensor


Spectral = function(A, row_idx, col_idx, threshold = NULL){
  d = dim(A)[1]
  A_unfolded = unfold(as.tensor(A),row_idx = row_idx,col_idx = col_idx)@data
  if(is.null(threshold)){
    threshold = sqrt(max(dim(A_unfolded)))
  }

  Decomp = svd(A_unfolded)
  s = max(length(which(ifelse(Decomp$d>threshold,Decomp$d,0)>0)),1)



  D = diag(Decomp$d,s)
  Theta = Decomp$u[,1:s,drop = F] %*% D %*% t(Decomp$v[,1:s,drop = F])
  Theta = fold(Theta,row_idx = row_idx,col_idx = col_idx,dim(A))@data
  return(Theta)
}





#' The least squares estimation for nonparametric tensor estimation with unknown permutation.
#'
#' Estimate a permuted signal tensor from a noisy data tensor based on  the least squares estimation with constant block approximation.
#' @param A A given noisy data tensor.
#' @param kvec A vector of the number of groups for each mode.
#' @param sym Boolean variables representing symmetricity of the signal tensor.  Non-symmetric tensor (\code{sym = FALSE}) is default.
#' @param mode An integer from 1 to 3 representing a type of methods for estimating the clustering functions. Higher-order spectral clustering method is default. \cr\code{mode = 1}: k-means algorithm applied on unfolded matrices. \cr\code{mode = 2}: k-means algorithm for community detection in stocahstic block model (only availble on binary observation). \cr\code{mode = 3}: higher-order spectral clustering algorithm.
#' @return An estimated permuted signal tensor based on the least squares estimation.
#' @usage LSE(A, kvec, sym = FALSE, mode = 3)
#' @references  C. Gao, Y. Lu, and H. H. Zhou. Rate-optimal graphon estimation. The Annals of Statistics, 2015. \cr  K. Balasubramanian. Nonparametric modeling of higher-order interactions via hypergraphons. Journal of Machine Learning Research, 2021. \cr R. Han, Y. Luo, M. Wang, and A. R. Zhang. Exact clustering in tensor block model: Statistical optimality and computational limit. arXiv:2012.09996, 2020.
#' @examples
#'
#' # Generate the noisy observation from smooth tensor and permutation
#' d = 20
#' sim1 = simulation(d, mode = 1)
#' signal_T = sim1$signal
#' observe_T = sim1$observe
#' permutation = sim1$permutation
#' psignal_T = signal_T[permutation,permutation,permutation]
#'
#' # Estimate permuted signal tensor
#' kvec = c(10,10,10)
#' hatpTheta = LSE(observe_T,kvec,sym = TRUE)
#'
#' # Calculate MSE
#' mean((hatpTheta-psignal_T)^2)
#'
#' @export
#' @import rTensor
#' @import stats

LSE = function(A, kvec, sym = FALSE, mode = 3){

  if(mode==1){
    #Spectral membership estimation
    if(sym == T){
      z1 = z2 = z3 = kmeans(tensor_unfold(A,1),kvec[1],nstart = 100)$cluster
    }else{
      z1 = kmeans(tensor_unfold(A,1),kvec[1],nstart = 100)$cluster
      z2 = kmeans(tensor_unfold(A,2),kvec[2],nstart = 100)$cluster
      z3 = kmeans(tensor_unfold(A,3),kvec[3],nstart = 100)$cluster
    }

  }else if (mode==2){
    if(sym == T){
      z = Bal(A,kvec[1])
      z1 = z2 = z3 = z
    }else{
      result = Bal_asym(A,kvec)
      z1 =  result$z1; z2 = result$z2; z3 = result$z3
    }
    # Balasubramanian estimation

  }else if (mode==3){
    if(sym == T){
      result = HSC(A,kvec[1],kvec[1],kvec[1],sym = T)
      z1 =  result$z1; z2 = result$z2; z3 = result$z3
    }else{
      result = HSC(A,kvec[1],kvec[2],kvec[3])
      z1 =  result$z1; z2 = result$z2; z3 = result$z3
    }
    # HSC membership estimation

  }
  z1 = ReNumber(z1); z2 = ReNumber(z2); z3 = ReNumber(z3)

  mu.array = UpdateMus_tensor(A,z1,z2,z3)
  # this is for the technical error

  Theta = mu.array[z1,z2,z3, drop=FALSE]
  return(Theta)
}









##################### Simulation functions ############################################

f1 = function(a){
  return(a[1]*a[2]*a[3])
}
f2 = function(a){
  return(mean(a))
}
f3 = function(a){
  return(1/(1+exp(-3*sum(a^2))))
}
f4 = function(a){
  return(log(1+max(a)))
}
# f5=function(a){
#   return(min(a)/exp(-max(a)-sqrt(a[1])-sqrt(a[2])-sqrt(a[3])))
# }
f5=function(a){
  return(exp(-min(a)-sqrt(a[1])-sqrt(a[2])-sqrt(a[3])))
}




symnoise = function(d,sigma=0.5){
  noise = array(NA,dim = c(d,d,d))
  for (i in 1:d){
    for (j in i:d){
      for(k in j:d){
        noise[i,j,k] = noise[i,k,j] = noise[j,i,k]  = noise[j,k,i] = noise[k,i,j] = noise[k,j,i]=rnorm(1,0,sigma)
      }
    }
  }
  return(noise)
}



#' Generate a symmetric tensor observation from the smooth signal tensor, Gaussian noise tensor, and permutation.
#'
#' Generate a symmetric tensor observation from the smooth signal tensor, Gaussian noise tensor, and permutation. Users can select one of 5 different smooth signal tensors generated from functions specified in Table 4 of the reference given below.
#' @param d Dimension of a tensor to be generated.
#' @param mode An integer from 1 to 5 corresponding to models specified. Default model is 1.
#' @param sigma Standard deviation of the Gaussian noise tensor. Default value is 0.5.
#' @param signal_level A scale of the magnitude of the signal tensor to be generated.
#' @return The returned object is a list of components.
#' @return \code{signal} - A true signal tensor generated from a function specified.
#' @return \code{observe} - A noisy observation  generated from the smooth signal tensor, Gaussian noise tensor, and permutation.
#' @return \code{permutation} - A true permutation.
#' @usage simulation(d, mode = 1, sigma = 0.5, signal_level=5)
#' @references C. Lee and M. Wang. Smooth tensor estimation with unknown permutations. arXiv:2111.04681, 2021.
#' @examples
#' d = 20
#' # Generate 20 by 20 by 20 observed tesnor generated from model 1
#' sim1 = simulation(d,mode = 1)
#' observed_tensor = sim1$observe
#' signal_tensor = sim1$signal
#' permutation = sim1$permutation
#' @export
#' @importFrom stats "rnorm"

simulation = function(d, mode = 1, sigma = 0.5, signal_level=5){
  tensor=array(dim=c(d,d,d))
  X1=c(slice.index(tensor,1))/d
  X2=c(slice.index(tensor,2))/d
  X3=c(slice.index(tensor,3))/d
  if(mode==1){
    signal = array(apply(cbind(X1,X2,X3),1,f1),dim=rep(d,3))
  }else if(mode==2){
    signal = array(apply(cbind(X1,X2,X3),1,f2),dim=rep(d,3))
  }else if(mode==3){
    signal = array(apply(cbind(X1,X2,X3),1,f3),dim=rep(d,3))
  }else if(mode==4){
    signal = array(apply(cbind(X1,X2,X3),1,f4),dim=rep(d,3))
  }else if(mode==5){
    signal = array(apply(cbind(X1,X2,X3),1,f5),dim=rep(d,3))
  }

  signal=signal_level*signal/sqrt(mean(signal^2))

  observe = signal+symnoise(d,sigma = sigma)


  ### Permutation for the indices
  o = sample(1:d,d)

  return(list(signal= signal,observe=observe[o,o,o], permutation = o))
}


Observe_A = function(P){
  n = dim(P)[1]; m = length(dim(P))
  U=array(rnorm(length(P),0,1),dim(P))
  tempA=1*(U<qnorm(P,0,1))
  A = array(0,dim(P))
  for (i in 1:n){
    for (j in i:n){
      for(k in j:n){
        A[i,j,k] = A[i,k,j] = A[j,i,k]  = A[j,k,i] = A[k,i,j] = A[k,j,i]= tempA[i,j,k]
      }
    }
  }
  return(A)
}



#' Generate a symmetric binary tensor from the probability tensor and permutation.
#'
#' Generate a symmetric binary tensor from the probability tensor and permutation. Users can select one of 5 different smooth probability tensor generated from functions specified in Table 4 of the reference given below.
#' @param d Dimension of a tensor to be generated.
#' @param mode An integer from 1 to 5 corresponding to models specified. Default model is 1.
#' @return The returned object is a list of components.
#' @return \code{signal} - A true probability tensor generated from a function specified.
#' @return \code{observe} - A binary tensor generated by Bernoulli trials given the probability tensor and permutation.
#' @return \code{permutation} - A true permutation.
#' @usage simulation_bin(d, mode = 1)
#' @references C. Lee and M. Wang. Smooth tensor estimation with unknown permutations. arXiv:2111.04681, 2021.
#' @examples
#' d = 20
#' # Generate 20 by 20 by 20 binary-valued tensor generated from model 1
#' sim1 = simulation_bin(d, mode = 1)
#' observed_tensor = sim1$observe
#' signal_tensor = sim1$signal
#' permutation = sim1$permutation
#' @export
#' @importFrom stats "rnorm" "qnorm"

simulation_bin = function(d, mode = 1){
  tensor=array(dim=c(d,d,d))
  X1=c(slice.index(tensor,1))/d
  X2=c(slice.index(tensor,2))/d
  X3=c(slice.index(tensor,3))/d
  if(mode==1){
    signal = array(apply(cbind(X1,X2,X3),1,f1),dim=rep(d,3))
  }else if(mode==2){
    signal = array(apply(cbind(X1,X2,X3),1,f2),dim=rep(d,3))
  }else if(mode==3){
    signal = array(apply(cbind(X1,X2,X3),1,f3),dim=rep(d,3))
  }else if(mode==4){
    signal = array(apply(cbind(X1,X2,X3),1,f4),dim=rep(d,3))
  }else if(mode==5){
    signal = array(apply(cbind(X1,X2,X3),1,f5),dim=rep(d,3))
  }

  observe = Observe_A(signal)

  ### Permutation for the indices
  o = sample(1:d,d)
  return(list(signal = signal, observe = observe[o,o,o],permutation = o))
}





# Non-symmetirc simulation
af1 = function(a){
  return(a[1]*a[2]+a[3])
}
af2 = function(a){
  return(a[1]^2+a[2]+a[2]*a[3]^2)
}
af3 = function(a){
  return(a[1]/(1+exp(-3*sum(a^2))))
}
af4 = function(a){
  return(log(1+max(a)+a[1]^2+a[2]*a[3]))
}
af5=function(a){
  return(exp(-a[1]-sqrt(a[2])-a[3]^2))
}



#' Generate a non-symmetric tensor observation from the smooth signal tensor, Gaussian noise tensor, and permutation.
#'
#' Generate a non-symmetric tensor observation from the smooth signal tensor, Gaussian noise tensor, and permutation. Users can select one of 5 different smooth signal tensors generated from functions specified in Table 5 of the reference given below.
#' @param d A vector of dimensions of a tensor to be generated.
#' @param mode An integer from 1 to 5 corresponding to models specified. Default model is 1.
#' @param sigma Standard deviation of the Gaussian noise tensor. Default value is 0.5.
#' @param signal_level A scale of the magnitude of the signal tensor to be generated.
#' @return The returned object is a list of components.
#' @return \code{signal} - A true non-symmetric signal tensor generated from a function specified.
#' @return \code{observe} - A noisy observation generated from the smooth signal tensor, Gaussian noise tensor, and permutation.
#' @return \code{permutation} - A list of true permutation for each mode.
#' @usage simulation_asym(d, mode = 1, sigma = 0.5, signal_level=5)
#' @references C. Lee and M. Wang. Smooth tensor estimation with unknown permutations. arXiv:2111.04681, 2021.
#' @examples
#' d = c(10,20,30)
#' # Generate 10 by 20 by 30 observed tesnor generated from model 1
#' sim1 = simulation_asym(d,mode = 1)
#' observed_tensor = sim1$observe
#' signal_tensor = sim1$signal
#' permutation = sim1$permutation
#' @export
#' @importFrom stats "rnorm"


simulation_asym = function(d, mode = 1, sigma = 0.5, signal_level=5){
  d1 = d[1]; d2 = d[2]; d3 = d[3]
  tensor=array(dim=c(d1,d2,d3))
  X1=c(slice.index(tensor,1))/d1
  X2=c(slice.index(tensor,2))/d2
  X3=c(slice.index(tensor,3))/d3
  if(mode==1){
    signal = array(apply(cbind(X1,X2,X3),1,af1),dim=c(d1,d2,d3))
  }else if(mode==2){
    signal = array(apply(cbind(X1,X2,X3),1,af2),dim=c(d1,d2,d3))
  }else if(mode==3){
    signal = array(apply(cbind(X1,X2,X3),1,af3),dim=c(d1,d2,d3))
  }else if(mode==4){
    signal = array(apply(cbind(X1,X2,X3),1,af4),dim=c(d1,d2,d3))
  }else if(mode==5){
    signal = array(apply(cbind(X1,X2,X3),1,af5),dim=c(d1,d2,d3))
  }

  signal=signal_level*signal/sqrt(mean(signal^2))
  observe = signal+array(rnorm(d1*d2*d3,0,sigma),dim = c(d1,d2,d3))

  ### Permutation for the indices
  o1 = sample(1:d1,d1); o2 = sample(1:d2,d2); o3 = sample(1:d3,d3)

  return(list(signal= signal,observe=observe[o1,o2,o3], permutation = list(o1,o2,o3)))
}
















