#' @noRd
#' @importFrom Rglpk Rglpk_solve_LP
balseq_onestep <- function(Xaux,pik,pikInit,index,deg){
   
  n <- ncol(Xaux)- 1
  p <- ncol(Xaux) -1
  if(n == 0){
    n <- 1
  }
  status <- 1
  sol <- 0
  # loop on the linear programming
  while( status == 1){
    # print(n)
    n = n  + 1
    if(is.na(index[n])){
      break;
    }
    
    pik1 <- pik[index[1:n]]
    pik2 <- pik[index[2:n]]
    
    bmin <- -pmin(pik2,(1 - pik2) * pik1[1]/(1-pik1[1]))
    bmax <- pmin(1 - pik2,pik2 * pik1[1]/(1-pik1[1]))
    
    # X = matrix(Xaux[index[1:n],],ncol = ncol(Xaux))
    # X <- Xaux[index[1:n],]
    
    X <- matrix(Xaux[index[1:n],],ncol = ncol(Xaux))
    X <- X*(pik1/pikInit[index[1:n]])
    
    V <- Rglpk::Rglpk_solve_LP(obj = ((n-1):1)^deg,
                        mat = rbind(t(X[-1,]/pik2),diag(rep(1,n-1))),
                        dir = c(rep("==",p+1),rep("<=",n-1)),
                        rhs = c(X[1,] + c(t(X[-1,]/pik2)%*%pmin(pik2,(1-pik2)*pik1[1]/(1-pik1[1]))),
                                bmax-bmin),
                        max = TRUE)
    sol <- V$solution
    
    status <- V$status
  }
  
  
  return(list(status = status,
              v = sol,
              n = n))
}

#' @title Sequential balanced sampling
#' @name balseq
#'
#' @description 
#' Selects at the same time a well-spread and a balanced sample using a sequential implementation.
#'
#' @param pik A vector of inclusion probabilities.
#' @param Xaux A matrix of auxiliary variables. The matrix must contains the \code{pik} vector to have fixed sample size.
#' @param Xspread An optional matrix of spatial coordinates.
#' @param rord A logical variable that specify if reordering is applied. Default TRUE.
#'
#' @details 
#' 
#' The function selects a sample using a sequential algorithm. At the same time, it respects the balancing equations (\code{Xaux}) and select a well-spread sample (\code{Xspread}). Algorithm uses a 
#' linear program to satisfy the constraints.
#'
#' @return Return the selected indices in 1,2,...,N
#'
#' @seealso \code{\link[BalancedSampling:lcube]{BalancedSampling:lcube}}, \code{\link[sampling:samplecube]{sampling:samplecube}}. 
#' @importFrom stats runif
#' 
#' @examples
#' N <- 100
#' n <- 10
#' p <- 10
#' 
#' pik <- rep(n/N,N)
#' 
#' Xaux <- array(rnorm(N*p,3,1),c(N,p))
#' 
#' Xspread <- cbind(runif(N),runif(N)) 
#' Xaux <- cbind(pik,Xaux)
#' 
#' s <- balseq(pik,Xaux)
#' colSums(Xaux[s,]/as.vector(pik[s]))
#' colSums(Xaux)
#' 
#' 
#' s <- balseq(pik,Xaux,Xspread)
#' colSums(Xaux[s,]/as.vector(pik[s]))
#' colSums(Xaux)
#' 
#' @export
balseq <- function(pik,Xaux,Xspread = NULL,rord = TRUE){
  # initializing
  deg = 1
  N <- length(pik)
  eps <- 1e-6
  
  pikInit <- pik
  index <- which(pik > eps & pik < (1-eps))
  
  n <- 0
  counter <- 1
  
  #----------- MAIN LOOP
  while(length(index) > 0){
    # cat("Step :",counter,"\n")
    # print(length(index))
    # print(n)
   
    i <- which.max(pik[index])  
  
    
    if(!is.null(Xspread)){
      #take distance of the considered unit 
      d <- distUnitk(Xspread,index[i],F,F)
      # modify index respect to distance
      index <- index[order(d[index])]
    }else{
      
      # tmp <- index[1]
      # index[1] <- index[i]
      # index[i] <- tmp
      
      if(rord == TRUE){
        index <- index[order(pik[index],decreasing = TRUE)]
      }
      
      # print(index);cat("\n\n")
      # print(pik[index])
    }
    
    l <- balseq_onestep(Xaux,pik,pikInit,index,deg)
    status <- l$status
    v = l$v
    n <- l$n
    
    unit0 <- which(pik < eps)
    unit1 <- which(pik > 1- eps)
    
    # plot(Xspread)
    # lines(Xspread[index[1:n],1],Xspread[index[1:n],2],type ="p",pch = 16,col = "cyan")
    # lines(Xspread[index[1],1],Xspread[index[1],2],type ="p",pch = 16,col = "red")
    # lines(Xspread[unit0,1],Xspread[unit0,2],type ="p",pch = 16)
    # lines(Xspread[unit1,1],Xspread[unit1,2],type ="p",pch = 16,col = "orange")
    # Sys.sleep(1)
    
    # if we can no longer find solution and index is at the end of the vector then exit and return pikstar
    if(status == 1 & is.na(index[n])){
      # return(pik)
      break;
    }else{
      
      v <-  v - pmin(pik[index[2:n]],(1-pik[index[2:n]])*pik[index[1]]/(1-pik[index[1]]))
      
      if(stats::runif(1) < pik[index[1]]){
        pik[index[2:n]] <- pik[index[2:n]] - v*(1-pik[index[1]])/pik[index[1]]
        pik[index[1]] <- 1
      }else{
        pik[index[2:n]] <- pik[index[2:n]] + v
        pik[index[1]] <- 0
      }
      
      index <- which(pik > eps & pik < (1-eps))
      
    }
    counter <- counter + 1
  }
  # cat("Number of units already taked", length(which(pik > (1-eps))) ,"\n\n")
  # cat("Number of units that remains not integer",length(index),"\n\n")
  
  
  
  #----------- LANDING PHASE
  
  if(!is.null(Xspread)){
    if(length(index) != 0){
      tmp <- as.vector(pik/pikInit)
      
      s <- BalancedSampling::lcube(pik,Xspread,Xaux*tmp)
    } else{
      pik <- round(pik,10)
      s <- which(pik > (1-eps))
    }
  }else{
    if(length(index) != 0){
      # print(index)
      # print("Landing Phase")
      tmp <- as.vector(pik/pikInit)
      s <- landingRM(Xaux*tmp,pik)
      # s <- landingRM(Xaux/as.vector(pikInit),pik)
      s <- which(s > (1-eps))
    } else{
      pik <- round(pik,10)
      s <- which(pik > (1-eps))
    }
  }
  
  return(s)
  
}

