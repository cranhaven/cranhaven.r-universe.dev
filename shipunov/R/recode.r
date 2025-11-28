Recode <- function(var, from, to, char=TRUE, recycle=FALSE)
{
 if (char && is.factor(to)) to <- as.character(to)
 if (recycle) to <- rep(to, length(from))
 y <- x <- as.vector(var)
 for (i in seq_along(from)) x <- replace(x, y == from[i], to[i])
 if (is.factor(var)) factor(x) else x
}

## ===

Recode4 <- function(var, from, to, missed="", ...)
{
 ifelse(var %in% from, Recode(var, from, to, ...), missed)
}

## ===

RecodeR <- function(var, from, to, char=TRUE, recycle=FALSE)
{
 if (char && is.factor(to)) to <- as.character(to)
 if (recycle) to <- rep(to, length(from))
 x <- as.vector(var)
 for (i in seq_along(from)) x <- replace(x, x == from[i], to[i])
 if (is.factor(var)) factor(x) else x
}

## ===

Recode4R <- function(var, from, to, missed="", ...)
{
 ifelse(var %in% from, RecodeR(var, from, to, ...), missed)
}
