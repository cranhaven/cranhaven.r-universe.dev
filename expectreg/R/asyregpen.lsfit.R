asyregpen.lsfit <-
function(y, B, p, lambda, DD,nb, constmat)
    ###### Asymmetric regression with difference penalty
    # parameters:
    # y - response variable
    # B - B-spline basis
    # p - asymmetry parameter
    # lambda - smoothing parameter
    # DD - difference matrix 
    #
    # needed: ncol(B) = ncol(DD)
{
    max_iteration_IWLS <- 50
    
    w1 <- 0 * y + 0.5
    n <- ncol(B)
    
    lambda = c(rep(0,times=n - sum(nb)),rep(lambda,times=nb))
    

    P <- sqrt(lambda) * DD

    augm <- rep(0, nrow(P))
    conpen <- rep(0,nrow(constmat))
    
    diffcon = -1
    it.con = 1
    
    while(any(diffcon < -1e-5) && it.con < 20)
    {
        it = 1
        dw1 = 1
        
        while(dw1 != 0 && it < max_iteration_IWLS) 
        {
            model <- lsfit(x=rbind(B,P,constmat*conpen), y=c(y, augm,0*conpen), wt=c(w1,(augm+1),1*(conpen>0)), intercept=FALSE)
            a1 <- model$coefficients
            z1 <- B %*%a1
            w01 <- w1
            #w1 <- as.vector(ifelse(y > z1, p, 1 - p))
            w1[] = p
            w1[!(y > z1)] = 1-p
            dw1 <- sum(w1 != w01,na.rm=TRUE)     
            it = it + 1
        }
        diffcon =  constmat %*% a1
        
        if(any(diffcon < 0))
        {
            wc = which(diffcon < 0)
            conpen[wc] = conpen[wc] + 100000
        }
        it.con = it.con + 1
    }
    diag.hat.ma1 <- hat(model$qr)[1:length(y)]  
    
    if(it == max_iteration_IWLS)
        warning("IWLS weights did not converge after 50 iterations.")
    
    list(a=a1, diag.hat.ma=diag.hat.ma1, weight = w1, fitted = z1,B=B,P=P)  
}
