grep_or <- function (x, patterns) {
    lp = lapply(patterns, function(i) grepl(i, x))
    res = 0
    for (i in lp) res = res + i
    x[res > 0]
}