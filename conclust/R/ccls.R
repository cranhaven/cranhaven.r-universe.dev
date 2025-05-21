ccls <-
function(data, k = -1, mustLink, cantLink, maxIter = 1, tabuIter = 100, tabuLength = 20) {
    
    mod = 10000007
    
    findMustLink <- function(i) {
        tmp = c()
        for (j in 1:n) {
            if (i != j && M[i, j] == 1)
                tmp = c(tmp, j)
        }
        tmp
    }
    
    findCantLink <- function(i) {
        tmp = c()
        for (j in 1:n) {
            if (i != j && C[i, j] == 1)
                tmp = c(tmp, j)
        }
        tmp
    }
    
    dist <- function(x, y) {
        tmp = x - y
        tmp = tmp * tmp
        sum(tmp)
    }
    
    cost <- function(label) {
        C = matrix(0, nrow = k, ncol = d)
        dem = rep(0, k)
        for (i in 1:n) {
            u = label[i]
            C[u,] = C[u,] + data[i,]
            dem[u] = dem[u] + 1
        }
        for (i in 1:k)
            if (dem[i] > 0)
                C[i,] = 1.0 * C[i,] / dem[i]
        res = 0
        for (i in 1:n)
            res = res + dist(data[i,], C[label[i],])
        for (i in 1:nm) {
            if (i > nm)
                break
            u = mustLink[i, 1]
            v = mustLink[i, 2]
            if (label[u] != label[v])
                res = res + w
        }
        for (i in 1:nc) {
            if (i > nc)
                break
            u = cantLink[i, 1]
            v = cantLink[i, 2]
            if (label[u] == label[v])
                res = res + w
        }
        res
    }
    
    getHash <- function(label) {
        h = 0
        for (i in 1:n) {
            h = (h * base + label[i]) %% mod
        }
        h
    }
    
    tabu <- function(h, tabuLength, tabuList) {
        for (i in 1: tabuLength) {
            if (h == tabuList[i])
                return(1)
        }
        0
    }
    
    tabuSearch <- function(label, maxIter, tabuLength) {
        tabuList = matrix(0, nrow = tabuLength, ncol = n)
        ntb = 1
        tabuList[1] = getHash(label)
        bestLabel = label
        cbl = cost(label)
        curCost = cbl
        curCenter = matrix(0, nrow = k, ncol = d)
        curCnt = rep(0, k)
        curH = tabuList[1]
        for (i in 1:n) {
            u = label[i]
            curCenter[u,] = curCenter[u,] + data[i,]
            curCnt[u] = curCnt[u] + 1
        }
        for (i in 1:k)
            if (curCnt[i] > 0)
                curCenter[i,] = 1.0 * curCenter[i,] / curCnt[i]
        # print("A")
        for (iter in 1:maxIter) {
            # print(iter)
            besti = -1
            bestj = -1
            bestCost = 1e15
            bestH = -1
            # print("X")
            for (i in 1:n) {
                for (j in 1:k) {
                    if (j != label[i]) {
                        x = label[i]
                        label[i] = j
                        newH = (curH + powmod[n - i + 1] * (j - x)) %% mod
                        if (tabu(newH, tabuLength, tabuList) == 0) {
                            center2 = curCenter[x,] * curCnt[x] - data[i,]
                            if (curCnt[x] > 1)
                                center2 = 1.0 * center2 / (curCnt[x] - 1)
                            tmp = curCost
                            tmp = tmp + curCnt[x] * sum(curCenter[x,] * curCenter[x,])
                            tmp = tmp - (curCnt[x] - 1) * sum(center2 * center2)
                            
                            center3 = 1.0 * (curCenter[j,] * curCnt[j] + data[i,]) / (curCnt[j] + 1)
                            tmp = tmp + curCnt[j] * sum(curCenter[j,] * curCenter[j,])
                            tmp = tmp - (curCnt[j] + 1) * sum(center3 * center3)
                            
                            for (u in mlw[[i]]) {
                                if (label[u] == x)
                                    tmp = tmp + w
                                else if (label[u] == j)
                                    tmp = tmp - w
                            }
                            for (u in clw[[i]]) {
                                if (label[u] == x)
                                    tmp = tmp - w
                                else if (label[u] == j)
                                    tmp = tmp + w
                            }
                            
                            curCenter[x,] = center2
                            curCenter[j,] = center3
                            curCnt[x] = curCnt[x] - 1
                            curCnt[j] = curCnt[j] + 1
                            
                            if (besti == -1 || tmp < bestCost) {
                                bestCost = tmp
                                besti = i
                                bestj = j
                                bestH = newH
                            }
                            
                            curCenter[x,] = (curCenter[x,] * curCnt[x] + data[i,]) / (curCnt[x] + 1)
                            curCnt[x] = curCnt[x] + 1
                            curCenter[j,] = (curCenter[j,] * curCnt[j] - data[i,])
                            if (curCnt[j] > 1)
                                curCenter[j,] = curCenter[j,] / (curCnt[j] - 1)
                            curCnt[j] = curCnt[j] - 1
                        }
                        label[i] = x
                    }
                }
            }
            
            if (besti == -1)
                return(bestLabel)
            
            x = label[besti]
            label[besti] = bestj
            
            curCost = bestCost
            curCenter[x,] = (curCenter[x,] * curCnt[x] - data[besti,])
            if (curCnt[x] > 1)
                curCenter[x,] = curCenter[x,] / (curCnt[x] - 1)
            curCnt[x] = curCnt[x] - 1
            curCenter[bestj,] = (curCenter[bestj,] * curCnt[bestj] + data[besti,]) / (curCnt[bestj] + 1)
            curCnt[bestj] = curCnt[bestj] + 1
            
            curH = bestH
            
            ntb = ntb %% tabuLength + 1
            tabuList[ntb] = bestH
            if (curCost < cbl) {
                cbl = curCost
                bestLabel = label
            }
        }
        bestLabel
    }
    
    data = as.matrix(data)
    n = nrow(data)
    d = ncol(data)
    
    for (i in 1:d) {
        dmin = 1e15
        dmax = -1e15
        for (j in 1:n) {
            if (data[j, i] < dmin)
                dmin = data[j, i]
            if (data[j, i] > dmax)
                dmax = data[j, i]
        }
        if (dmax == dmin) {
            next
        }
        data[, i] = (data[, i] - dmin) / (dmax - dmin)
    }
    
    
    nm = nrow(mustLink)
    nc = nrow(cantLink)
    findK = 0
    if (k == -1) {
        findK = 1
        k = 10
    }
    
    base = k + 1
    powmod = rep(1, n + 1)
    for (i in 2:(n + 1))
        powmod[i] = (powmod[i - 1] * base) %% mod
    
    w = 1e15
    for (i in 1:n) {
        j = i + 1
        while (j <= n) {
            w = min(w, dist(data[i,], data[j,]))
            j = j + 1
        }
    }
    
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
            if (M[i, u] == 1) {
                for (j in 1:n) {
                    if (M[u, j] == 1)
                        M[i, j] = 1
                }
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
                        tmp = dist(mu[j,], mu[v,])
                    else
                        tmp = min(tmp, dist(mu[j,], mu[v,]))
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
    
    bestLabel = rep(0, n)
    for (i in 1:n) {
        best = -1
        dbest = 1e15
        for (j in 1:k) {
            tmp = dist(C[j,], data[i,])
            if (best == -1 || tmp < dbest) {
                dbest = tmp
                best = j
            }
        }
        bestLabel[i] = best
    }
    cbl = cost(bestLabel)
    
    label = bestLabel
    for (iter in 1:maxIter) {
        # print(iter)
        label = tabuSearch(label, tabuIter, tabuLength)
        tmp = cost(label)
        if (tmp < cbl) {
            cbl = tmp
            bestLabel = label
        }
        label = sample(1:k, size = n, replace = T)
    }
    if (findK == 0)
        return(bestLabel)
    return(as.numeric(as.factor(bestLabel)))
}
