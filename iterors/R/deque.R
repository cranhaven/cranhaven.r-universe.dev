deque <- function(len=64) {
  data <- vector("list", len)
  i.first <- 1
  i.open <- 1

  getFirst <- function(or=stop("Deque is empty")) {
    if (i.open == i.first) {
      or
    } else {
      val <- data[[i.first]]
      data[[i.first]] <<- "xxx"
      i.first <<- (i.first %% length(data)) + 1
      val
    }
  }

  getLast <- function(or=stop("Deque is empty")) {
    if (i.open == i.first) {
      or
    } else {
      if (i.open == 1) i.open <<- length(data)
      else i.open <<- i.open - 1
      val <- data[[i.open]]
      data[[i.open]] <<- "xxx"
      val
    }
  }

  `%:%` <- function(from, to) {
    seq_len(to - from) + (from - 1)
  }

  expand <- function() {
    data2 <- vector(mode(data), 2 * length(data))
    if (i.open <= i.first) {
      slice1 <- 1 %:% i.open
      slice2 <- i.first %:% (length(data)+1)
      data2[slice1] <- data[slice1]
      data2[slice2 + length(data)] <- data[slice2]
      i.first <<- i.first + length(data)
    } else {
      slice <- i.first %:% i.open
      data2[slice] <- data[slice]
    }
    data <<- data2
  }

  append <- function(val) {
    if ((i.open %% length(data)) + 1 == i.first) expand()
    data[i.open] <<- list(val)
    i.open <<- (i.open %% length(data)) + 1
    val
  }

  prepend <- function(val) {
    if ((i.open %% length(data)) + 1 == i.first) expand()
    if (i.first == 1) i.first <<- length(data)
    else i.first <<- i.first-1
    data[i.first] <<- list(val)
    val
  }

  getLength <- function() {
    if (i.open < i.first) {
      i.open - i.first + length(data)
    } else {
      i.open - i.first
    }
  }

  peek <- function(ix=1, or=stop("index out of bounds")) {
    ix <- index(ix, return(or))
    data[[ix]]
  }

  index <- function(ix, or) {
    len <- getLength()
    ifelse(
      ix > len | ix < -len | abs(ix) < 1,
      or,
      ifelse(
        ix > 0,
        ((i.first + ix - 2) %% length(data)) + 1,
        ((i.open + ix - 1) %% length(data)) + 1)
    )
  }

  extract <- function(ix=1:getLength(),
                      or=stop("index out of bounds")) {
    #use or=0 to remove indices out of bounds;
    #use or=NA to fill with NULL.
    ix <- index(ix, or)
    data[ix]
  }

  structure(list(append=append,
                 prepend=prepend,
                 nextOr=getFirst,
                 getFirst=getFirst,
                 getLast=getLast,
                 length=getLength,
                 peek=peek,
                 extract=extract),
            class="deque")

}

#' @exportS3Method
format.deque <- function(x, ...) {
  paste0("<deque (", x$getLength(), " items)>")
}

#' @exportS3Method
summary.deque <- function(object, ...) {
  list(length=object$getLength())
}
