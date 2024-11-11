genericApply <- function(mat, margin, func, ..., nthread=1, clutype="PSOCK")
{
    if(nthread <= 1)
        return(apply(mat,margin,func,...))
    cl <- parallel::makeCluster(nthread,type=clutype)
    ret <- tryCatch(parallel::parApply(cl,mat,margin,func,...),
                    finally=parallel::stopCluster(cl))
    return(ret)
}

genericLapply <- function(x, func, ..., nthread=1, clutype="PSOCK")
{
    if(nthread <= 1)
        return(lapply(x, func, ...))
    cl <- parallel::makeCluster(nthread,type=clutype)
    ret <- tryCatch(parallel::parLapply(cl,x,func,...),
                    finally=parallel::stopCluster(cl))
    return(ret)
}
