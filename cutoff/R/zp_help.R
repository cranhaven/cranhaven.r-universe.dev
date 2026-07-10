#' @importFrom set %not%
to.refer <- function(x,refer){
    if (length(refer)!=1) stop('refer must be 1 length')
    if (!any(refer == unique(x))) stop('refer does not exist in x')
    x=factor(x)
    factor(x,levels = c(refer,levels(x) %not% refer))
}
digital <- function(x,round){
    digital.i <- function(x,round){
        if (is.numeric(x)){
            x=round(x,round)
            format(x,nsmall = round)
        }else{
            format(x,digits = round,nsmall = round)
        }
    }
    if (any(is.data.frame(x),is.matrix(x))){
        for (i in 1:ncol(x)) {
            x[,i]=digital.i(x[,i],round)
        }
        x
    }else{
        digital.i(x,round)
    }
}
#' @importFrom stats pchisq
survdiff_p.value <- function(survdiff){
    #the code is form survdiff() function in 'survival' package
    #the code can be used to calculate degree of freedom, p value excep chi
    #however you really can not extract p.value for survdiff() function
    if (is.matrix(survdiff$obs)) {
        otmp <- apply(survdiff$obs, 1, sum)
        etmp <- apply(survdiff$exp, 1, sum)
    }else {
        otmp <- survdiff$obs
        etmp <- survdiff$exp
    }
    df <- (etmp > 0)
    if (sum(df) < 2) {
        chi <- 0
        return(1)
    } else {
        temp2 <- ((otmp - etmp)[df])[-1]
        vv <- (survdiff$var[df, df])[-1, -1, drop = FALSE]
        chi <- sum(solve(vv, temp2) * temp2)
        survdiff.pvalue=1 - pchisq(chi, length(temp2))
        return(survdiff.pvalue)
    }
}
