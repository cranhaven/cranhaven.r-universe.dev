mpckm <-
function(data, k, mustLink, cantLink, maxIter = 10) {
    
    dist <- function(x, y, A) {
        tmp = x - y
        tmp2 = tmp %*% A
        tmp2 = tmp2 %*% tmp
        tmp2
    }
    
    det <- function(A) {
        res = 1
        d = nrow(A)
        for (i in 1:d) {
            res = res * A[i, i]
        }
        res
    }
    
    fM <- function(x, y) {
        dist(x, y, A)
    }
    
    fC <- function(x, y, z) {
        z - dist(x, y, A)
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
    n = nrow(data)
    d = ncol(data)
    nm = nrow(mustLink)
    nc = nrow(cantLink)
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
    mustLink = matrix(nrow = 0, ncol = 2)
    for (i in 1:n) {
        j = i + 1
        while (j <= n) {
            if (M[i, j] == 1)
                mustLink = rbind(mustLink, c(i, j))
            j = j + 1
        }
    }
    nm = nrow(mustLink)
    mlw = lapply(1:n, findMustLink)
    
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
    
    cantLink = matrix(nrow = 0, ncol = 2)
    for (i in 1:n) {
        j = i + 1
        while (j <= n) {
            if (C[i, j] == 1)
                cantLink = rbind(cantLink, c(i, j))
            j = j + 1
        }
    }
    nc = nrow(cantLink)
    clw = lapply(1:n, findCantLink)
    
    
    A = matrix(0, nrow = d, ncol = d)
    for (i in 1:d)
        A[i, i] = 1
    
    mu = matrix(0, nrow = ntp, ncol = d)
    demtp = rep(0, ntp)
    for (i in 1:n) {
        u = tp[i]
        mu[u, ] = mu[u, ] + data[i, ]
        demtp[u] = demtp[u] + 1
    }
    for (i in 1:ntp) {
        mu[i,] = 1.0 * mu[i,] / demtp[i]
    }
    largest = 1
    for (i in 2:ntp) {
        if (i > ntp)
            break;
        if (demtp[i] > demtp[largest])
            largest = i;
    }
    C = matrix(nrow = k, ncol = d)
    C[1,] = mu[largest,]
    used = rep(0, ntp)
    used[largest] = 1
    cs = rep(0, k)
    cs[1] = largest
    for (i in 2:k) {
        D = rep(0, ntp)
        best = -1
        for (j in 1:ntp) {
            if (used[j] == 0) {
                tmp = -1
                for (u in 1:(i - 1)) {
                    v = cs[u]
                    if (tmp == -1)
                        tmp = dist(mu[j,], mu[v,], A)
                    else
                        tmp = min(tmp, dist(mu[j,], mu[v,], A))
                }
                D[j] = demtp[j] * tmp
                if (best == -1 || D[j] > D[best])
                    best = j
            }
        }
        if (best != -1) {
            cs[i] = best
            used[best] = 1
            C[i,] = mu[best,]
        } else {
            C[i,] = data[sample(1:n, 1),]
        }
    }
    for (i in 1:n) {
        tp[i] = 1
        tmp = dist(data[i,], C[1,], A)
        for (j in 2:k) {
            tmp2 = dist(data[i,], C[j,], A)
            if (tmp2 < tmp) {
                tmp = tmp2
                tp[i] = j
            }
        }
    }
    
    for (iter in 1:maxIter) {
        #print(iter)
        farthest = matrix(1e15, nrow = k, ncol = 3)
        for (i in 1:n) {
            u = tp[i]
            if (farthest[u, 1] == 1e15) {
                farthest[u,] = c(i, i, 0)
            }
            j = i + 1
            while (j <= n) {
                v = tp[j]
                if (u == v) {
                    tmp = dist(data[i,], data[j,], A)
                    if (tmp > farthest[u, 3]) {
                        farthest[u,] = c(i, j, tmp)
                    }
                }
                j = j + 1
            }
        }
        tp = rep(0, n)
        ord = sample(1:n, n)
        for (ii in 1:n) {
            i = ord[ii]
            D = rep(1e15, k)
            best = -1
            for (j in 1:k) {
                D[j] = dist(data[i,], C[j,], A) - log(det(A)) / log(2)
                for (u in mlw[[i]]) {
                    if (tp[u] > 0 && tp[u] != j)
                        D[j] = D[j] + fM(data[i,], data[u,])
                }
                for (u in clw[[i]]) {
                    if (tp[u] == j) {
                        tmp = fC(data[i,], data[u,], farthest[j, 3])
                        if (tmp < 0)
                            tmp = 0
                        D[j] = D[j] + tmp
                    }
                }
                if (best == -1 || D[j] < D[best])
                    best = j
            }
            tp[i] = best;
        }
        if (iter == maxIter)
            return(tp)
        
        C = matrix(0, nrow = k, ncol = d)
        demtp = rep(0, k)
        for (i in 1:n) {
            j = tp[i]
            demtp[j] = demtp[j] + 1
            C[j,] = C[j,] + data[i,]
        }
        for (i in 1:k)
            if (demtp[i] > 0)
                C[i,] = 1.0 * C[i,] / demtp[i]
        for (i in 1:d) {
            A[i, i] = 0
            for (j in 1:n) {
                A[i, i] = A[i, i] + (data[j, i] - C[tp[j], i]) ^ 2
            }
            for (j in 1:nm) {
                if (j > nm)
                    break;
                u = mustLink[j, 1]
                v = mustLink[j, 2]
                if (tp[u] != tp[v]) {
                    A[i, i] = A[i, i] + 0.5 * (data[u, i] - data[v, i])^2
                }
            }
            for (j in 1:nc) {
                if (j > nc)
                    break;
                u = cantLink[j, 1]
                v = cantLink[j, 2]
                if (tp[u] == tp[v]) {
                    uu = farthest[tp[u], 1]
                    vv = farthest[tp[v], 2]
                    if (uu <= n && vv <= n) {
                        tmp = (data[uu, i] - data[vv, i])^2 - (data[u, i] - data[v, i])^2
                        if (tmp < 0)
                            tmp = 0
                        A[i, i] = A[i, i] + tmp
                    }
                }
            }
        }
        amin = A[1, 1]
        for (i in 2:d) {
            if (i > d)
                break;
            if (A[i, i] < amin)
                amin = A[i, i]
        }
        if (amin <= 0) {
            for (i in 1:d) {
                A[i, i] = A[i, i] - amin + 1e-9
            }
        }
        for (i in 1:d)
            A[i, i] = n / A[i, i]
    }
}
