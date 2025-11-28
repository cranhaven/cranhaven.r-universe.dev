Fill <- function(x, ditto="")
{
 if (is.na(ditto)) {
  what <- !is.na(x)
  } else {
  nas <- is.na(x)
  what <- (x != ditto) & !nas # otherwise, "a case of missing compare"
  }
 y <- x[what]
 z <- cumsum(what)
 z[z == 0] <- NA # do not skip positions before the first fill
 res <- y[z] # a[NA] returns NA but keeps position
 res[is.na(res)] <- ditto # make them as 'missing' defined
 if (!is.na(ditto) && any(nas)) res[nas] <- NA # put NAs back
res
}

Ditto <- function(x, ditto="")
{
 if (!is.character(x)) x <- as.character(x)
 x[duplicated(x, incomparables=ditto)] <- ditto
x
}
