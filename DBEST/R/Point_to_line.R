Point_to_line <-
function(pt, v1, v2) {
        
        a <- v1 - v2
        b <- pt - v2
        
        dim(a) <- c(1,3)
        dim(b) <- c(1,3)
        
        cprd <- c()
        cprd[1] = (a[2]*b[3]) - (b[2]*a[3])
        cprd[2] = (b[1]*a[3]) - (a[1]*b[3])
        cprd[3] = (a[1]*b[2]) - (b[1]*a[2])
        
        dim(cprd) <- c(1,3)
        d <- max(svd(cprd)$d) / max(svd(a)$d)
        return(d)
        
}
