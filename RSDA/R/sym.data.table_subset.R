#' Extract or replace parts of a Symbolic Data Table
#' @keywords internal
#' @export
`[.sym.data.table` <- function(x, i, j) {
  out <- x
  if (!missing(j)) {
    if (any(j > length(out$sym.var.names))) {
      stop("undefined columns selected")
    }

    meta.data <- data.frame(row.names = out$sym.obj.names, check.names = F)
    real.data <- data.frame(row.names = out$sym.obj.names, check.names = F)
    new.var.l <- c()
    new.var.s <- c()

    if (any(j < 0)) {
      j <- seq_along(out$sym.var.names)[j]
    }

    for (columns in j) {
      for (column in columns) {
        type <- out$sym.var.types[column]
        var.l <- out$sym.var.length[column]
        var.s <- out$sym.var.starts[column]

        if (type %in% c("$H", "$M", "$S")) {
          new.var.s <- c(new.var.s, ncol(meta.data) + 3)

          data. <- out$meta[, (var.s - 2):(var.s + (var.l - 1))]
          meta.data <- cbind(meta.data, data.)

          data. <- out$meta[, (var.s):(var.s + (var.l - 1))]
          real.data <- cbind(real.data, data.)

          new.var.l <- c(new.var.l, ncol(data.))
        }
        if (type %in% c("$I")) {
          new.var.s <- c(new.var.s, ncol(meta.data) + 2)
          new.var.l <- c(new.var.l, 2)

          data. <- out$meta[, (var.s - 1):(var.s + (var.l - 1))]
          meta.data <- cbind(meta.data, data.)

          data. <- out$meta[, (var.s):(var.s + 1)]
          real.data <- cbind(real.data, data.)
        }
        if (type %in% c("$C")) {
          new.var.s <- c(new.var.s, ncol(meta.data) + 2)
          new.var.l <- c(new.var.l, 1)
          data. <- out$meta[, (var.s - 1):(var.s)]
          meta.data <- cbind(meta.data, data.)

          data. <- out$meta[var.s]
          real.data <- cbind(real.data, data.)
        }
      }
    }
    out$meta <- meta.data
    out$data <- real.data
    out$sym.var.names <- out$sym.var.names[j]
    out$sym.var.types <- out$sym.var.types[j]
    out$sym.var.length <- out$sym.var.length[j]
    out$sym.var.starts <- out$sym.var.starts[j]
    out$sym.var.starts <- new.var.s
    out$sym.var.length <- new.var.l
  }
  if (!missing(i)) {
    if (any(i > length(out$sym.obj.names))) {
      stop("undefined rows selected")
    }
    out$sym.obj.names <- out$sym.obj.names[i]
    out$data <- out$data[i, ]
    out$meta <- out$meta[i, ]
  }
  out$N <- length(out$sym.obj.names)
  out$M <- length(out$sym.var.names)
  out
  return(out)
}
