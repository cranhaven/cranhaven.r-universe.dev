lcvqe <-
function(data, k, mustLink, cantLink, maxIter = 10) {
    
    nullVec <- function(x) {
        tmp = c()
        tmp
    }
    
    dist <- function(x, y) {
        tmp = x - y
        sum(tmp * tmp)
    }
    
    findMember <- function(v, label) {
        tmp = c()
        for (i in 1:n) {
            if (label[i] == v)
                tmp = c(tmp, i)
        }
        tmp
    }
    
    farther <- function(l, n) {
        u = cantLink[l, 1]
        v = cantLink[l, 2]
        d1 = dist(data[u,], C[n,])
        d2 = dist(data[v,], C[n,])
        if (d1 > d2)
            return(u);
        v
    }
    #set.seed(911)
    n = nrow(data)
    d = ncol(data)
    nm = nrow(mustLink)
    nc = nrow(cantLink)
    C = data[sample(1:n, size = k),]
    for (iter in 1:maxIter) {
        #print("1")
        GMLV = lapply(1:k, nullVec)
        GCLV = lapply(1:k, nullVec)
        dmin = rep(1, n)
        dmin2 = rep(-1, n)
        for (i in 1:n) {
            tmp = dist(data[i,], C[1,])
            tmp2 = -1
            for (j in 2:k) {
                tmp3 = dist(data[i,], C[j,])
                if (tmp3 < tmp) {
                    tmp2 = tmp
                    dmin2[i] = dmin[i]
                    tmp = tmp3
                    dmin[i] = j
                } else if (tmp2 == -1 || tmp3 < tmp2) {
                    tmp2 = tmp3
                    dmin2[i] = j
                }
            }
        }
        label = dmin
        #print("2")
        for (i in 1:nm) {
            if (i > nm)
                break;
            s1 = mustLink[i, 1]
            s2 = mustLink[i, 2]
            u = label[s1]
            v = label[s2]
            if (u == v)
                next;
            a = 0.5 * (dist(data[s1,], C[u,]) + dist(data[s2,], C[v,])) + 0.25 * (dist(data[s1,], C[v,]) + dist(data[s2,], C[u,]))
            b = 0.5 * dist(data[s1,], C[u,]) + 0.5 * dist(data[s2,], C[u,])
            c = 0.5 * dist(data[s1,], C[v,]) + 0.5 * dist(data[s2,], C[v,])
            if (b <= a && b <= c) {
                label[s1] = u
                label[s2] = u
            } else if (c <= a && c <= b) {
                label[s1] = v
                label[s2] = v
            } else {
                GMLV[[u]] = union(GMLV[[u]], s2)
                GMLV[[v]] = union(GMLV[[v]], s1)
                label[s1] = u
                label[s2] = v
            }
        }
        #print("3")
        for (i in 1:nc) {
            if (i > nc)
                break;
            s1 = cantLink[i, 1]
            s2 = cantLink[i, 2]
            u = label[s1]
            v = label[s2]
            if (u != v)
                next;
            r = farther(i, u)
            if (r == s1)
                s = s2
            else
                s = s1
            if (dmin[r] == label[r])
                m = dmin2[r]
            else
                m = dmin[r]
            a = 0.5 * dist(data[s1,], C[u,]) + 0.5 * dist(data[s2,], C[u,]) + 0.5 * dist(data[r,], C[m,])
            b = 0.5 * (dist(data[s,], C[u,]) + dist(data[r,], C[m,]))
            if (a < b) {
                GCLV[[m]] = union(GCLV[[m]], r)
                label[s1] = u
                label[s2] = u
            } else {
                label[s] = u
                label[r] = m
            }
        }
        if (iter == maxIter)
            return(label)
        #print("4")
        for (j in 1:k) {
            Q = findMember(j, label)
            nq = length(Q)
            nmj = length(GMLV[[j]])
            ncj = length(GCLV[[j]])
            N = nq + 0.5 * nmj + ncj
            tmp = rep(0, d)
            for (u in 1:nq) {
                if (u > nq)
                    break
                tmp = tmp + data[Q[u],]
            }
            for (u in 1:nmj) {
                if (u > nmj)
                    break
                tmp = tmp + 0.5 * data[GMLV[[j]][u],]
            }
            for (u in 1:ncj) {
                if (u > ncj)
                    break
                tmp = tmp + data[GCLV[[j]][u],]
            }
            if (N > 0)
                C[j,] = 1.0 * tmp / N
        }
    }
    
}
