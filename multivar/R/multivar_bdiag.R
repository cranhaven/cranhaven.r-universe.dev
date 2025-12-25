#' @export
multivar_bdiag <- function(d){

    nrows = sum(sapply(d, NROW))

    ncols = sum(sapply(d, NCOL))

    ans = matrix(0, nrows, ncols)

    i1 = 1

    j1 = 1        

    for (m in d){

          i2 = i1 + NROW(m) - 1

            j2 = j1 + NCOL(m) - 1

            ans[i1:i2, j1:j2] = as.matrix(m)

            i1 = i2 + 1

            j1 = j2 + 1

        }

    return(ans)

  }