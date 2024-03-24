#' Simulate Descriptives: Binomial
#'
#' Simulate a Data Frame containing descriptive statistics summarizing a binomial distribution
#'
#' @param n.grp number of row groups
#' @param n.lvls number of rows in each row group
#' @param n.grp2 another row grouping, such that grp is subset of grp2
#' @export
rbin <-
function(n.grp = 8, n.lvls = 5, n.grp2=NULL)
  {
    g  <- rep(LETTERS[1:n.grp], rep(n.lvls, n.grp))
    if (!is.null(n.grp2)) {g2 <- rep(c(1:n.grp2), rep(ceiling(length(g)/n.grp2), n.grp2))}
    f  <- rep(letters[(26-n.lvls+1):26], n.grp)
    f <- paste(f,f,f,f,f,f,f,f,f, sep="")
    r1 <- rpois((n.lvls*n.grp), 20)
    n1 <- rpois((n.lvls*n.grp), 100)
    n1 <- ifelse(n1<r1, r1, n1)
    p1 <- r1/n1
    r2 <- rpois((n.lvls*n.grp), 30)
    n2 <- rpois((n.lvls*n.grp), 100)
    n2 <- ifelse(n2<r2, r2, n2)
    p2 <- r2/n2
    pval  <- runif((n.lvls*n.grp), max = .25)
    table <- data.frame(group = g, level = f, r1=r1, n1=n1, p1=p1, r2=r2,n2=n2, p2=p2, "p-value" = pval)
    if (!is.null(n.grp2))  {table$group2 <- g2}
    return(table)
  }

#' Simulate Descriptives: Continuous
#'
#' Simulate a Data Frame containing descriptive statistics summarizing a continuous data
#'
#' @param n.grp number of row groups
#' @param n.lvls number of rows in each row group
#' @param n.grp2 another row grouping, such that grp is subset of grp2
#' @param rnd boolean round results, data frame then returns as character
#' @export
rdesc <-
function(n.grp = 8, n.lvls = 5, n.grp2=NULL, rnd=FALSE)
  {
    g  <- rep(LETTERS[1:n.grp], rep(n.lvls, n.grp))
    if (!is.null(n.grp2)) {g2 <- rep(c(1:n.grp2), rep(ceiling(length(g)/n.grp2), n.grp2))}
    f  <- rep(letters[(26-n.lvls+1):26], n.grp)
    f <- paste(f,f,f,f,f,f,f,f,f, sep="")
    mn1   <- rnorm((n.lvls*n.grp), 2.3, 1.1)
    mdn1  <- rnorm((n.lvls*n.grp), 2.3, 1.1)
    var1  <- rchisq((n.lvls*n.grp), 1.1)
    mn2   <- rnorm((n.lvls*n.grp), 2.5, 1.3)
    mdn2  <- rnorm((n.lvls*n.grp), 2.5, 1.3)
    var2  <- rchisq((n.lvls*n.grp), 1.3)
    pval  <- runif((n.lvls*n.grp), max = .25)
    rdesc.table <- data.frame(group = g, level = f, Mean1 = mn1, Median1= mdn1, Variance1 = var1, Mean2 = mn2, Median2 = mdn2, Variance2 = var2, "p-value" = pval)
    if (!is.null(n.grp2))  {table$group2 <- g2}
    if(rnd) {    rdesc.table[ , 3:9] <- data.frame(lapply(round(rdesc.table[ , 3:9],2), FUN=function(x) prettyNum(x, drop0trailing=FALSE)))}
    else {rdesc.table[ , 3:9] <- rdesc.table[ , 3:9]}
    return(rdesc.table)
}



