ckmeans <-
function(data, k, mustLink, cantLink, maxIter = 100) {
    
    dist <- function(x, y) {
        tmp <- x - y
        sum(tmp * tmp)
    }
    
    violate <- function(i, j) {
        for (u in mlw[[i]]) {
            if (label[u] != 0 && label[u] != j)
                return(1);
        }
        for (u in clw[[i]]) {
            if (label[u] == j)
                return(1);
        }
        0
    }
    
    findMustLink <- function(i) {
        tmp = c()
        for (j in 1:n) {
            if (M[i, j] == 1)
                tmp = c(tmp, j)
        }
        tmp
    }
    
    findCantLink <- function(i) {
        tmp = c()
        for (j in 1:n) {
            if (C[i, j] == 1)
                tmp = c(tmp, j)
        }
        tmp
    }
    
    data = as.matrix(data)
    n <- nrow(data)
    d <- ncol(data)
    nm <- nrow(mustLink)
    nc <- nrow(cantLink)
    
    M = matrix(0, nrow = n, ncol = n)
    for (i in 1:nm) {
        if (i > nm)
            break;
        u = mustLink[i, 1]
        v = mustLink[i, 2]
        M[u, v] = 1
        M[v, u] = 1
    }
    for (u in 1:n) {
        for (i in 1:n) {
            for (j in 1:n) {
                if (M[i, u] == 1 && M[u, j] == 1)
                    M[i, j] = 1
            }
        }
    }
    
    tp = rep(0, n)
    ntp = 0
    for (i in 1:n) {
        if (tp[i] == 0) {
            ntp = ntp + 1
            tp[i] = ntp
            j = i + 1
            while (j <= n) {
                if (tp[j] == 0 && M[i, j] == 1)
                    tp[j] = ntp
                j = j + 1
            }
        }
    }
    
    findMember <- function(v) {
        tmp = c()
        for (u in 1:n) {
            if (tp[u] == v)
                tmp = c(tmp, u)
        }
        tmp
    }
    tmpPi = lapply(1:ntp, findMember)
    
    C = matrix(0, nrow = n, ncol = n)
    for (i in 1:nc) {
        if (i > nc)
            break;
        u = cantLink[i, 1]
        v = cantLink[i, 2]
        x = tp[u]
        y = tp[v]
        if (x != y) {
            for (p in tmpPi[[x]]) {
                for (q in tmpPi[[y]]) {
                    C[p, q] = 1
                    C[q, p] = 1
                }
            }
        }
    }
    
    mlw <- lapply(1:n, findMustLink)
    clw <- lapply(1:n, findCantLink)
    
    #set.seed(9191)
    tmp <- sample(1:n, k)
    C <- matrix(nrow = k, ncol = d)
    for (i in 1:k) {
        C[i,] = data[tmp[i],]
    }
    for (iter in 1:maxIter) {
        label <- rep(0, n)
        for (i in 1:n) {
            dd <- rep(1e15, k)
            best <- -1
            for (j in 1:k) {
                if (violate(i, j) == 0) {
                    dd[j] <- dist(data[i,], C[j,])
                    if (best == -1 || dd[j] < dd[best]) {
                        best = j
                    }
                }
            }
            if (best == -1)
                return(0)    
            label[i] <- best
        }
        if (iter == maxIter)
            return(label)
        C2 <- matrix(0, nrow = k, ncol = d)
        dem <- rep(0, k)
        for (i in 1:n) {
            j = label[i]
            C2[j,] = C2[j,] + data[i,]
            dem[j] = dem[j] + 1
        }
        for (i in 1:k) {
            if (dem[i] > 0)
                C[i,] = 1.0 * C2[i,] / dem[i]
        }
    }
}
