library(iterors)

permn <- function(x) {
  n <- length(x)
  if (n == 1 && is.numeric(x) && x >= 0) {
    n <- x
    x <- seq(length=n)
  }

  if (n == 0)
    list()
  else
    permn.internal(x, n)
}

permn.internal <- function(x, n) {
  if (n == 1) {
    list(unlist(x, recursive=FALSE))
  } else {
    fun <- function(i) lapply(permn.internal(x[-i], n - 1), function(v) c(x[[i]], v))
    unlist(lapply(seq(along=x), fun), recursive=FALSE)
  }
}

ipermn <- function(x) {
  n <- length(x)
  if (n == 1 && is.numeric(x) && x >= 0) {
    n <- x
    x <- seq(length=n)
  }

  ipermn.internal(x, n)
}

ipermn.internal <- function(x, n) {
  icar <- icount(n)

  if (n > 1) {
    icdr <- NULL
    hasVal <- FALSE
    nextVal <- NULL
  }

  nextOr_ <- if (n <= 1) {
    function(or) x[[nextOr(icar, return(or))]]
  } else {
    function(or) {
      repeat {
        if (!hasVal) {
          nextVal <<- nextOr(icar, return(or))
          icdr <<- ipermn.internal(x[-nextVal], n - 1)
          hasVal <<- TRUE
        }

        return(c(x[[nextVal]], nextElem(icdr, {
          hasVal <<- FALSE
          next
        })))
      }
    }
  }

  iteror(nextOr_)
}

icombn <- function(x, m) {
  n <- length(x)
  if (n == 1 && is.numeric(x) && x >= 0) {
    n <- x
    x <- seq(length=n)
  }

  if (m > n)
    stop('m cannot be larger than the length of x')

  if (m < 0)
    stop('m cannot be negative')

  icombn.internal(x, n, m)
}

icombn.internal <- function(x, n, m) {
  icar <- icount(n - m + 1)

  if (n > 1) {
    icdr <- NULL
    hasVal <- FALSE
    nextVal <- NULL
  }

  nextOr_ <- if (m <= 1) {
    function(or) x[[nextElem(icar, return(or))]]
  } else {
    function(or) {
      repeat {
        if (!hasVal) {
          nextVal <<- nextOr(icar, return(or))
          nn <- n - nextVal
          icdr <<- icombn.internal(x[seq(nextVal+1, length=nn)], nn, m - 1)
          hasVal <<- TRUE
        }

        return(c(x[[nextVal]],
                 nextOr(icdr, {
                   hasVal <<- FALSE
                   next
                 })))
      }
    }
  }

  iteror(nextOr_)
}

tostr <- function(x) paste(x, collapse=', ')

failures <- 0

# test ipermn using permn
for (x in list(list(1,2,3), 1:3, 1, 'bar', 3, c(), letters[1:6])) {
  cat(sprintf('testing ipermn on: %s\n', tostr(x)))
  actual <- as.list(ipermn(x))
  expect <- permn(x)
  status <- identical(actual, expect)
  if (!status) {
    cat('test failed\n')
    cat('  expected:\n')
    print(expect)
    cat('  actual:\n')
    print(actual)
    failures <- failures + 1
  }
}

# test icombn using combn
for (m in 1:8) {
  for (x in list(1:2, 'foo', 1, 7, 1:8, letters[1:6], rep('foo', 3))) {
    m <- min(m, length(x))
    cat(sprintf('testing icombn on: %s\n', tostr(x)))
    actual <- as.list(icombn(x, m))
    expect <- combn(x, m, simplify=FALSE)
    status <- identical(actual, expect)
    if (!status) {
      cat('test failed\n')
      cat('  expected:\n')
      print(expect)
      cat('  actual:\n')
      print(actual)
      failures <- failures + 1
    }
  }
}

if (failures > 0) {
  cat(sprintf('%d test(s) failed\n', failures))
} else {
  cat('All tests passed\n')
}
