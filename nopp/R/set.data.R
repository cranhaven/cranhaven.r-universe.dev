

set.data <- function(data, shape="wide", choice, varying, sep="_"){
    mycall <- match.call()
    tmp <- mlogit.data(data=data , shape=shape, choice=choice,
    varying=varying, sep=sep)
    attr(tmp,"call") <- mycall
    tmp
}
