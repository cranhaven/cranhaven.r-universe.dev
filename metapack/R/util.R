check.numeric <- function(v) {
    if (!inherits(v, c("character", "factor"))) {
        if (inherits(v, c("numeric", "integer", "logical"))) {
            return(rep(TRUE, length(v)))
        }
    }

    if (inherits(v, "factor")) {
        v <- as.character(v)
    }

    regexp_pattern <- "(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))$)|(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))e(-|\\+)?(\\d+)$)"
    grepl(regexp_pattern, v)
}

unfactor <- function(v) {
    if (inherits(v, "factor")) {
        x <- as.character(v)
        if (all(check.numeric(x))) {
            x <- as.numeric(x)
        }
    } else {
        x <- as.character(v)
    }
    x
}

relabel.vec <- function(x, order)
{
  old.x <- x
  x <- rep(NA, length(old.x))
  for (i in seq(length(order))) x[old.x == order[i]] <- i #relabel studies in numerical order starting with one
  return(x)
}

vhpd <- function(x, level = 0.95) {
    n <- length(x)
    cl <- level
    gap <- max(1, min(n - 1, round(n * level))) / n
    if (level > gap) {
        warning("The desired level cannot be reached with the provided posterior sample size.\n The level should be smaller than ", gap, ". ", "Forcing the HPD level to ", gap-1.0e-4, "\n")
        cl <- gap-1.0e-4
    }
    alpha <- 1 - cl
    out <- .Call(`_metapack_vhpd`, as.vector(x), as.double(alpha))
    attr(out, "Empirical level") <- gap
    return(out)
}

mhpd <- function(x, level = 0.95) {
    n <- ncol(x)
    gap <- max(1, min(n - 1, round(n * level))) / n
    cl <- level
    if (level > gap) {
        warning("The desired level cannot be reached with the provided posterior sample size.\n The level should be smaller than ", gap, ". ", "Forcing the HPD level to ", gap-1.0e-4, "\n")
        cl <- gap-1.0e-4
    }
    alpha <- 1 - cl
    out <- .Call(`_metapack_mhpd`, as.matrix(x), as.double(alpha))
    attr(out, "Empirical level") <- gap
    return(out)
}

hpdarray <- function(A, level = 0.95) {
    nR <- nrow(A)
    nC <- ncol(A)
    n <- dim(A)[3]
    gap <- max(1, min(n - 1, round(n * level))) / n
    cl <- level
    if (level > gap) {
        warning("The desired level cannot be reached with the provided posterior sample size.\n The level should be smaller than ", gap, ". ", "Forcing the HPD level to ", gap-1.0e-4, "\n")
        cl <- gap-1.0e-4
    }
    alpha <- 1 - cl
    out <- array(0, dim = c(nR, nC, 2))
    dimnames(out)[[3]] <- c("lower", "upper")
    for (iC in 1:nC) {
        hpd_ic <- .Call(`_metapack_mhpd`, as.matrix(A[,iC,]), as.double(alpha))
        out[,iC,1] <- hpd_ic[,1]
        out[,iC,2] <- hpd_ic[,2]
    }
    attr(out, "Empirical level") <- gap 
    return(out)
}

ciarray <- function(A, level = 0.95) {
    nR <- nrow(A)
    nC <- ncol(A)
    out <- array(0, dim = c(nR, nC, 2))
    dimnames(out)[[3]] <- c("lower", "upper")
    sig.level <- 1 - level
    out[,,1] <- apply(A, c(1,2), function(xx) quantile(xx, prob = sig.level / 2))
    out[,,2] <- apply(A, c(1,2), function(xx) quantile(xx, prob = 1 - sig.level / 2))
    return(out)
}

#' helper function encoding trial sample sizes in formulas
#' @param x the name of the variable containing trial sample sizes
#' @export
ns <- function(x) x
