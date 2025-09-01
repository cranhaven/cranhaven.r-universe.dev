mass.match <-
function(x, known.mz, match.tol.ppm=5)
{
    mass.matched.pos<-rep(0, length(x))
    for(i in 1:length(x))
    {
        this.d<-abs((x[i]-known.mz)/x[i])
        if(min(this.d) < match.tol.ppm/1e6) mass.matched.pos[i]<-1
    }
    return(mass.matched.pos)
}
