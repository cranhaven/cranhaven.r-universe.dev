find.turn.point <-
function(y)
{
    peaks2<-function (x, ties.method)
    {
        z <- embed(rev(as.vector(c(-Inf, x, -Inf))), dimension = 3)
        z <- z[rev(seq(nrow(z))), ]
        v <- max.col(z,ties.method=ties.method) == 2
        v
    }
    msExtrema<-function (x)
    {
        l<-length(x)
        index1 <- peaks2(x, ties.method="first")
        index2 <- peaks2(-x, ties.method="last")
        index.max <- index1 & !index2
        index.min <- index2 & !index1
        list(index.max = index.max, index.min = index.min)
    }
    
    y <- y[!is.na(y)]
    if (length(unique(y)) == 1) {
        pks <- round(length(y)/2)
        vlys <- c(1, length(y))
        x <- new("list")
        x$pks <- pks
        x$vlys <- vlys
        return(x)
    }
    
    b<-msExtrema(y)
    pks<-which(b$index.max)
    vlys<-which(b$index.min)
    if(pks[1] != 1) vlys<-c(1, vlys)
    if(pks[length(pks)] != length(y)) vlys<-c(vlys, length(y))
    
    if(length(pks) == 1) vlys<-c(1,length(y))
    x <- new("list")
    x$pks <- pks
    x$vlys <- vlys
    return(x)
}
