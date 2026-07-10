"%s=%" <- function(a,b){
    loc=list()
    for (i in 1:length(a)) {
        loc=c(loc,list(grep(a[i],b)))
        names(loc)[i]=a[i]
    }
    loc
}
