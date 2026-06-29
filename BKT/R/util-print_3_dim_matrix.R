print_3_dim_matrix <- function(arr) {
  dim <- dim(arr)
  if (length(dim) != 3) {
    print(arr)
    return()
  }

  cat("[\n")
  for (i in 1:dim[1]) {
    cat(" [\n")
    for (j in 1:dim[2]) {
      cat("  [", paste(arr[i, j, ], collapse = " "), "]", sep = "")
      if (j < dim[2]) cat(",\n")
    }
    cat("\n ]")
    if (i < dim[1]) cat(",\n")
  }
  cat("\n]\n")
}
