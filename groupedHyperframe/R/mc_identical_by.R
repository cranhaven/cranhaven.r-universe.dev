
#' @title Check if Data Column is Identical by Grouping
#' 
#' @param data \link[base]{data.frame}
#' 
#' @param f \link[base]{factor}
#' 
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}.
#' Default is the return of the function \link[parallel]{detectCores}.
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @note
#' The function \link[stats]{aggregate.data.frame} does not do parallel computing.
#' 
#' The function `collapse::collap` does not support \link[survival]{Surv} column.
#' 
#' Look more into `nlme:::collapse.groupedData`
#' 
#' @keywords internal
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach `%dopar%`
#' @importFrom parallel detectCores mclapply makeCluster stopCluster
#' @export
mc_identical_by <- function(
    data, 
    f,
    mc.cores = detectCores(),
    ...
) {
  
  nr <- nrow(data)
  if (nr != length(f)) stop('`data` and `f` different length')
  
  ids <- nr |> seq_len() |> split.default(f = f)
  
  foo2 <- switch(
    EXPR = .Platform$OS.type, # as of R 4.5, only two responses, 'windows' or 'unix'
    unix = { 
      \(d) { # (d = data[[1L]])
        ids |> 
          mclapply(mc.cores = mc.cores, FUN = \(i) {
            all(duplicated(unclass(d[i]))[-1L]) # column `d` identical within split `i`
          }) |> 
          unlist(use.names = FALSE) |> # column `d` identical within all splits
          all()
      }
    }, windows = {
      \(d) { # (d = data[[1L]])
        foo1 <- \(i) {
          all(duplicated(unclass(d[i]))[-1L]) # column `d` identical within split `i`
        }
        i <- NULL # just to suppress devtools::check NOTE
        registerDoParallel(cl = (cl <- makeCluster(spec = mc.cores)))
        tmp <- foreach(i = ids, .options.multicore = list(cores = mc.cores)) %dopar% foo1(i)
        stopCluster(cl)
        tmp |> 
          unlist(use.names = FALSE) |>
          all()
      }
    })

  .ident <- data |>
    vapply(FUN = foo2, FUN.VALUE = NA)
  
  if (any(!.ident)) {
    nm <- names(data)[!.ident]
    #nm |> 
    #  col_blue() |> 
    #  paste(collapse = ';') |>
    #  sprintf(fmt = 'Column(s) %s removed; as they are not identical per aggregation-group') |>
    #  message() # choose not to print this message
    data[nm] <- NULL
  } else nm <- NULL
  
  ret <- data[vapply(ids, FUN = `[`, 1L, FUN.VALUE = NA_integer_), , drop = FALSE]
  # do.call(rbind.data.frame, args = .) # ?base::rbind.data.frame does not respect 'Surv', etc.
  .rowNamesDF(ret) <- NULL
  attr(ret, which = 'non_identical') <- nm
  return(ret)
  
}

