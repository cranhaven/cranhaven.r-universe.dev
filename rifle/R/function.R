Soft <-
function(a,b){
    if(b<0) stop("Can soft-threshold by a nonnegative quantity only.")
    return(sign(a)*pmax(0,abs(a)-b))
}

updatePi <-
function(B,sqB,A,H,Gamma,nu,lambda,Pi,tau){
    A <- Pi + 1/tau*A-nu/tau*B%*%Pi%*%B+nu/tau*sqB%*%(H-Gamma)%*%sqB
    B <- lambda/tau
    return(Soft(A,B))
}

updateH <-
function(sqB,Gamma,nu,Pi,K){
    
    temp <- Gamma + sqB%*%Pi%*%sqB
    temp <- (temp+t(temp))/2
    svdtemp <- eigen(temp)
    d <- svdtemp$values
    p <- length(d)
    if(sum(pmin(1,pmax(d,0)))<=K){
        dfinal <- pmin(1,pmax(d,0))
        return(svdtemp$vectors%*%diag(dfinal)%*%t(svdtemp$vectors))
    }
    fr <- function(x){
        sum(pmin(1,pmax(d-x,0)))
    }
    # Vincent Vu Fantope Projection
    knots <- unique(c((d-1),d))
    knots <- sort(knots,decreasing=TRUE)
    temp <- which(sapply(knots,fr)<=K)
    lentemp <- tail(temp,1)
    a=knots[lentemp]
    b=knots[lentemp+1]
    fa <- sum(pmin(pmax(d-a,0),1))
    fb <- sum(pmin(pmax(d-b,0),1))
    theta <- a+ (b-a)*(K-fa)/(fb-fa)
    dfinal <- pmin(1,pmax(d-theta,0))
    res <- svdtemp$vectors%*%diag(dfinal)%*%t(svdtemp$vectors)
    return(res)
}
