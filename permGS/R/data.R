Exp <- function(rate) {
    if(rate > 0) function(n) rexp(n, rate)
    else function(n) rep(Inf, n)
}

Uniform <- function(a, b) function(n) runif(n, a, b)

param.base <- list(n=c(10, 20),
                   n.groups=2,
                   minTime=0,
                   maxTime=Inf,
                   ifrac=1, ## information fractions e.g. c(0.5, 1)
                   MI=12, ## maximum information
                   btype=1, ## O'Brien-Fleming boundaries (Pocock: btype=2)
                   ftype=1, ## maximum information trial (maximum duration trial: ftype=2)
                   alpha=0.05,
                   B=1000, ## number of random permutations
                   surv=list(ctrl=Exp(0.04), trt=Exp(0.04)), ## survival times
                   cens=list(ctrl=Exp(0), trt=Exp(0)), ## censoring times
                   entry=Uniform(0, 48), ## entry times
                   blk.size=c(2, 4), ## block size
                   block.rand=TRUE) ## use block randomization

generateData <- function(param) {
    ## vector of sample sizes
    n <- param$n
    N <- sum(n)

    ## recruitment times
    R <- sort(param$entry(N))

    if(param$block.rand) {
        bl <- sum(param$blk.size)
        m <- N / bl
        trt <- c(vapply(1:m, function(i) sample(c(rep(0, param$blk.size[1]), rep(1, param$blk.size[2]))), rep(NA_real_, bl)))
        rnd.block <- rep(1:m, each=bl)
    } else {
        trt <- sample(c(rep(0, n[1]), rep(1, n[2])))
        rnd.block <- rep.int(1, N)
    }

    sel1 <- trt == 0
    sel2 <- trt == 1

    ssel1 <- sum(sel1)
    ssel2 <- sum(sel2)

    ## survival times
    T <- numeric(N)
    T[sel1] <- param$surv$ctrl(ssel1) 
    T[sel2] <- param$surv$trt(ssel2)

    ## censoring times
    C <- numeric(N)
    C[sel1] <- param$cens$ctrl(ssel1)
    C[sel2] <- param$cens$trt(ssel2)

    ## observed times
    time <- pmin(T, C)

    ## censoring indicator
    status <- T <= C

    ## block is set in "gsTrial"
    data.frame(time=time, status=status, trt=trt, entry=R, id=1:N, block=rep.int(1,N), rnd.block=rnd.block)
}
