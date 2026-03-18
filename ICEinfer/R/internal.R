"ICEangle" <-
function (t1, t2) 
{
    if (length(t1) == 0) 
        t1 <- 0
    if (length(t2) == 0) 
        t2 <- 0
    u <- t1 - t2
    v <- t1 + t2
    ia <- 90
    if (abs(u) > 1e-05 * abs(v)) 
        ia <- atan(abs(v)/u) * 57.29578
    if (ia < 0) 
        ia <- ia + 180
    if (v < 0) 
        ia <- -ia
    ia
}

"ICEd2m" <-
function (d, rownew, rowstd) 
{
    e0 <- mean(d[rowstd, 2])
    e1 <- mean(d[rownew, 2])
    c0 <- mean(d[rowstd, 3])
    c1 <- mean(d[rownew, 3])
    c(e1 - e0, c1 - c0)
}

"ICErunif" <-
function (n1, n2) 
{
    m <- floor(n2 + 1 - n1)
    idx <- as.integer(floor(m * runif(m)) + floor(n1))
    idx
}
