buildBasis <- function(response,percent=.95,numbas=NULL)
{
    svdm <- svd(response)
    if(is.null(numbas))
    {
        cumd <- cumsum(svdm$d)/sum(svdm$d)
        numbas <- min(which(cumd>=percent))
    }
    basis <- svdm$u[,1:numbas]          #basis matrix TT by numbas
    redd <- svdm$d[1:numbas]            #reduced d vector length numbas
    redv <- svdm$v[,1:numbas]           #reduced v matrix nn by mumbas
    basis <- t(t(basis)*redd)            #coefficient matrix nn by numbas
    coeff <- redv
    if(!is.matrix(coeff))
        coeff <- matrix(coeff,ncol=numbas)
    ret <- list(basis=basis,redd=redd,coeff=coeff,numbas=numbas)
}
getcols <- function(idx,mat) mat[,idx]
rename <- function(lst,name) {names(lst) <- name; return(lst)}
blockPar <- function(num,nblock)
{
    bsize <- floor(num/nblock)
    starts <- seq(1,(nblock-1)*bsize+1,len=nblock)
    ends <- starts+bsize-1
    ends[nblock]=num
    mat <- rbind(starts,ends)
    ret <- as.list(as.data.frame(mat))
}
getRowBlock <- function(bound,mat)
{
    idx <- seq(bound[1],bound[2])
    ret <- mat[idx,]
}
