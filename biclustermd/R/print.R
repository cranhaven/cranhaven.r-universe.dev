#' Print an object of class biclustermd
#' @param x a \code{biclustermd} object.
#' @param ... arguments passed to or from other methods
#'
#' @export

print.biclustermd <- function(x, ...) {
  cat("\n ", "Data has ", prod(dim(x$data)), " values, ", round(100 * sum(is.na(x$data)) / prod(dim(x$data)), 2), "% of which are missing", sep = "")
  cat("\n ", x$iteration, " Iterations", sep = "")
  cat("\n ", "Initial SSE = ", round(x$InitialSSE),
      "; Final SSE = ", round(x$SSE[x$iteration, 1]),
      ", a ", round(1 - x$SSE[x$iteration, 1] / x$InitialSSE, 3) * 100, "% reduction",
      sep = "")

  if(x$params$similarity == 'Rand') {
    cat(
      "\n ", x$params$similarity,
      " similarity used; Indices: ",
      "Columns (P) = ", round(x$Similarities[x$iteration, "P_rand"], 3),
      ", Rows (Q) = ", round(x$Similarities[x$iteration, "Q_rand"], 3), "\n", sep = ""
    )
  } else if(x$params$similarity == 'HA') {
    cat(
      "\n ", x$params$similarity,
      " similarity used; Indices: ",
      "Columns (P) = ", round(x$Similarities[x$iteration, "P_ha"], 3),
      ", Rows (Q) = ", round(x$Similarities[x$iteration, "Q_ha"], 3), "\n", sep = ""
    )
  } else if(x$params$similarity == 'Jaccard') {
    cat(
      "\n ", x$params$similarity,
      " similarity used; Indices: ",
      "Columns (P) = ", round(x$Similarities[x$iteration, "P_jaccard"], 3),
      ", Rows (Q) = ", round(x$Similarities[x$iteration, "Q_jaccard"], 3), "\n", sep = ""
    )
  }

  invisible(x)
}

