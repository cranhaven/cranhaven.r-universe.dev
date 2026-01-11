normaliselogs <- function(logp){
    a <- max(logp)
    exp(logp - a - log(sum(exp(logp - a))))
}

xlogx <- function(x)
    ifelse(x==0, x, x*log(x))

#' @importFrom stats rgamma
#' @title Dirichlet distribution
#' @description Draw from Dirichlet distribution
#' @param n number of variates to draw
#' @param gam a vector of concentration parameters of length \code{K}
#' @return matrix dimension \code{n*k} of samples
#' @examples
#' rdirichlet(1, rep(2,5)) ## a length-5 probability vector
#' @export
rdirichlet <- function (n, gam) {
    l <- length(gam)
    x <- matrix(stats::rgamma(l * n, gam), ncol = l, byrow = TRUE)
    sm <- x %*% rep(1, l)
    x/as.vector(sm)
}

#' @title Dirichlet distribution
#' @description Density of Dirichlet distribution
#' @param x random variable in the d-dimensional simplex
#' @param gam a length K concentration parameter
#' @param log return the log-probability instead?
#' @return the density
#' @examples
#' g <- rep(2,5)
#' p <- rdirichlet(1, g) ## a length-5 probability vector
#' ddirichlet(p, g)
#' @export
ddirichlet <- function (x, gam, log=FALSE) {
       if(length(gam) != (nc <- ncol(x)))
        gam <- rep(gam,nc)
    p <- sum(lgamma(gam)) - lgamma(sum(gam)) + rowSums(gam * log(x))
    if(!log)
        p <- exp(p)
    p
}

#' Draw draw Categorical distribution
#' @param n number of draws
#' @param p a length-d probability vector
#' @param replace should the categories be replaced? If so n < p required
#' @return a draw from Categorical(p)
#' @examples
#' rcat(1, 1) ## returns 1 with probability 1
#' rcat(1, rep(1/6,6)) ## a dice roll
#' @export
rcat <- function(n, p, replace=TRUE)
    sample(1:length(p), n, replace, p)

#' @title V-measure
#' @description Calculate the V-measure of two clusterings
#' @details An information based measure of similarity between two clusterings
#' @param z input vector
#' @param truez reference vector
#' @param beta parameter \code{beta=1} gives equal weight to homogeneity and completeness
#' @return v-measure of z against \code{truez}
#' @seealso Rosenberg, A., & Hirschberg, J. (2007, June). V-measure: A conditional entropy-based external cluster evaluation measure. In Proceedings of the 2007 joint conference on empirical methods in natural language processing and computational natural language learning (EMNLP-CoNLL) (pp. 410-420).
#' @export
#' @examples
#' vmeasure(c(1,1,2,2,3,3), c(2,2,1,1,3,3)) ## 1 - doesn't care for labels
#' vmeasure(c(1,1,2,2,3,3), c(1,1,2,2,2,2)) ## 0.7333
#' vmeasure(c(1,1,2,2,3,3), c(1,1,2,2,3,4)) ## 0.904
vmeasure <- function(z, truez, beta=1){
    C <- z
    B <- truez
    A <- table(B,C)
    N <- sum(A)
    HB  <- log(N) - sum(xlogx(rowSums(A)))/N
    HC  <- log(N) - sum(xlogx(colSums(A)))/N
    HBC <- sum(xlogx(colSums(A)))/N - sum(xlogx(A))/N
    HCB <- sum(xlogx(rowSums(A)))/N - sum(xlogx(A))/N
    h <- 1 - HBC/HB
    c <- 1 - HCB/HC
    v <- (beta+1)*h*c/(beta*h+c)
    v
}

#' @title Adjusted Rand Index
#' @description Calculate the Adjusted Rand Index between two clusterings
#' @param z input vector
#' @param truez reference vector
#' @return Adjusted Rand Index of \code{z} against \code{truez}
#' @export
#' @examples
#' ARI(c(1,1,2,2,3,3), c(2,2,1,1,3,3)) ## 1 - doesn't care for labels
#' ARI(c(1,1,2,2,3,3), c(1,1,1,1,2,2)) ## 0.444
ARI <- function(z, truez){
    N <- length(z)
    n <- table(z, truez)
    a <- sum(choose(colSums(n),2))
    b <- sum(choose(rowSums(n),2))
    eind <- a*b/choose(N,2)
    top <- sum(choose(n,2)) - eind
    bot <- 0.5*(a+b) - eind
    top/bot
}
