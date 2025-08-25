expand_grid_df_helper <- function(df, x) {
  stopifnot(is.data.frame(df))
  stopifnot(is.data.frame(x))
  if (is.data.frame(x)) {
    stopifnot(nrow(x) > .5)
    out <- list()
    out <- data.frame(matrix(NA,
                             nrow=nrow(df)*nrow(x),
                             ncol=ncol(df) + ncol(x)))
    colnames(out) <- c(colnames(df), colnames(x))
    for (i in 1:nrow(x)) {
      out[(i - 1) * nrow(df) + 1:nrow(df), 1:ncol(df)] <- df
      out[(i - 1) * nrow(df) + 1:nrow(df), ncol(df) + 1:ncol(x)] <- x[i, , drop=TRUE]

    }
    out
  }
}
# replace reshape::expand.grid.df with this to avoid having to have in Depends
expand_grid_df <- function(...) {
  dfs <- list(...)
  stopifnot(length(dfs) > .5)
  stopifnot(is.data.frame(dfs[[1]]))

  if (length(dfs) == 1) {
    return(dfs[[1]])
  }
  y <- dfs[[1]]
  for (i in 2:length(dfs)) {
    stopifnot(is.data.frame(dfs[[i]]))
    y <- expand_grid_df_helper(y, dfs[[i]])
  }
  y
}
if (F) {
  expand_grid_df(data.frame(a=1:3, b=letters[1:3]), data.frame(c=4:1+.5))
}
