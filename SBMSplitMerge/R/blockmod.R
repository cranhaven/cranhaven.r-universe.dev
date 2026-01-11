#' @title Block Model
#' @description create a \code{blockmod} object
#' @details
#' A block model is a probability model for a \code{blocks} object.
#' This class creates a closure with three functions:
#' - a random method for sampling block a structure from the model with \code{n} nodes; a
#' - a log-density method for computing the log-density of a given block structure in a \code{blocks} object
#' - a conditional density function that takes a \code{blocks} object and a node \code{i}
#' @param fixkappa Logical - is kappa fixed or can it vary under the model?
#' @param r \code{function(n), sorted=FALSE} - samples a blocks object from the model
#' @param logd \code{function(blocks)} - log density for \code{blocks}
#' @param dcond \code{function(blocks, i)} - conditional density for the block assignment \code{i} in \code{blocks}
#' @param ... parameters of the model for use in \code{r}, \code{logd}, \code{dcond}
#' @return a \code{blockmod} object
#' @seealso \code{\link{multinom}} \code{\link{dma}} \code{\link{crp}} \code{\link{blocks}}
#' @export
blockmod <- function(fixkappa, logd, dcond, r, ...){
    structure(
        list(
            fixkappa = fixkappa,
            logd = logd,
            dcond = dcond,
            r = r,
            ...
        )
       ,
        class = "blockmod"
    )
}

#' @export
print.blockmod <- function(x, ...){
    cat("A blockmod object with ")
    if(x$fixkappa){
        cat("a fixed number of blocks ")
    } else{
        cat("a variable number of blocks ")
    }
    cat("and parameters:\n")
    print(x[-(1:4)])
}

#' @title Multinomial block assignment
#' @description A \code{\link{blockmod}} for Multinomial allocation
#' @details This model posits that: for \code{i=1:n}
#' \deqn{Z_i ~ Multinomial(omega)}
#' where
#' \deqn{omega ~ Dirichlet(gamma)}
#' @param gamma parameter for Dirichlet component \eqn{Dirichlet(gamma, ..., gamma)}
#' @param kappa the number of blocks
#' @return a block model representing a \code{Multinomial(gamma)} distribution
#' @examples
#' ## A fixed number of blocks with multinomial assignment of nodes
#' m <- multinom(1, 4)
#' print(m)
#' m$r(10) ## simulate a blocks object with 10 nodes
#' @export
multinom <- function(gamma, kappa){
    blockmod(
        TRUE
       ,
        function(blocks){
            if(blocks$kappa != kappa){
                return(-Inf)
            } else{
                return( lgamma(kappa * gamma) -
                        kappa * lgamma(gamma) +
                        sum(lgamma(blocks$sizes + gamma)) -
                        lgamma(blocks$numnodes + kappa*gamma) )
            }
        }
       ,
        function(blocks, i){
            if(blocks$kappa != kappa){
                return(-Inf)
            } else{
                Nik <- blocks$sizes - tabulate(blocks$z[i], kappa)
                return( log(Nik + gamma) - log(sum(Nik + kappa*gamma)) )
            }
        }
       ,
        function(n, sorted=FALSE){
            omega <- rdirichlet(1, rep(gamma, kappa))
            z <- rcat(n, c(omega))
            if(sorted)
                z <- sort(z)
            blocks(z, kappa)
        }
       ,
        name="Multinomial", gamma = gamma, kappa = kappa
    )
}

#' @title Dirichlet Multinomial Allocation
#' @description A \code{\link{blockmod}} for Dirichlet Multinomial Allocation (DMA)
#' @details This model posits:
#' \deqn{kappa-1 ~ Pois(delta)}
#' \deqn{omega|kappa, gamma ~ Dirichlet(gamma)}
#' \deqn{Z_i|omega ~ Multinomial(omega) for i=1 .. n}
#' @param gamma parameter for Dirichlet component
#' @param delta parameter for Poison component
#' @return a block model representing a \code{dma(gamma, delta)} distribution
#' @examples
#' ## simulate from a DMA(2, 5) prior
#' ## This models the `number of blocks-1` as Poisson(5)
#' ## and block assignments as Dirichlet-Multinomial(2, 2, ...)
#' m <- dma(2, 5)
#' print(m)
#' m$r(10)
#' @export
dma <- function(gamma, delta){
    blockmod(
        FALSE
       ,
        function(blocks){
            stats::dpois(blocks$kappa-1, delta, log=TRUE) +
                lgamma(blocks$kappa * gamma) - blocks$kappa * lgamma(gamma) +
                    sum(lgamma(blocks$sizes + gamma)) - lgamma(blocks$numnodes + blocks$kappa*gamma)
        }
       ,
        function(blocks, i){
            ## For a fixed number of blocks, the conditional distribution that node
            ## i belongs to block k given gamma (log scale)
            kappa <- blocks$kappa
            Mk <- blocks$sizes - c(blockmat(blocks$z[i], kappa))
            return( log(Mk + gamma) - log(sum(Mk) + kappa * gamma) )
        }
       ,
        function(n, sorted=FALSE){
            kappa <- stats::rpois(1, delta) + 1
            omega <- rdirichlet(1, rep(gamma, kappa))
            z <- rcat(n, c(omega))
            if(sorted)
                z <- sort(z)
            blocks(z, kappa)
        }
       ,
        name="DMA", gamma = gamma, delta = delta
    )
}

#' @title Chinese Restaurant Process
#' @description A \code{\link{blockmod}} for the Chinese restaurant process (CRP)
#' @details The CRP posits that each node arrives in turn. The first node joins the first block. Each subsequent node starts a new block with probability `gamma` or joins an existing block proportional to the block size.
#' @param gamma concentration parameter
#' @return a block model representing a \code{CRP(gamma)} distribution
#' @export
#' @examples
#' ## simulate from a CRP(5) prior
#' m <- crp(5)
#' print(m)
#' m$r(10)
crp <- function(gamma){
    blockmod(
        FALSE
       ,
        function(blocks){
            lgamma(gamma) + blocks$kappa * gamma  - lgamma(blocks$numnodes + gamma) + sum(ifelse(blocks$sizes==0, -Inf, lgamma(blocks$sizes)))
        }
       ,
        function(blocks, i){
            log(c(tabulate(blocks$z[-i], blocks$kappa), gamma))
        }
       ,
        function(n, sorted=FALSE){
            z <- c(1, rep(0,n-1))
            blocksizes <- 1
            if(n > 1){
                for(i in 2:n){
                    probs <- c(tabulate(z), gamma)
                    z[i] <- sample(1:length(probs), 1, FALSE, probs)
                }
            }
            if(sorted)
                z <- sort(z)
            blocks(z, max(z))
        }
       ,
        name="CRP", gamma=gamma
    )
}
