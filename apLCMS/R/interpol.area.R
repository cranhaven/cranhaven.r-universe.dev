interpol.area <-
function(x, y, all.x, all.w) # x is retention time, all.w is weight of each all.x, y is observed intensities
{
    r<-range(x)
    x.sel<-which(all.x>=r[1] & all.x<=r[2])
    
    x2<-all.x[x.sel]
    w<-all.w[x.sel]
    
    y2<-approx(x, y, xout=x2, method="linear")$y
    return(sum(w*y2))
}
