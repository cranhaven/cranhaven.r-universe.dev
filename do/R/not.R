not <- function(a,b){
    a=toVector(a)
    a=unique(a)
    b=toVector(b)
    b=unique(b)
    a[!(a %in% b)]
}


