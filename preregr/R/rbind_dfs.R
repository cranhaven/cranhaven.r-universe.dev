#' Simple alternative for rbind.fill or bind_rows
#'
#' @param x One dataframe
#' @param y Another dataframe
#' @param clearRowNames Whether to clear row names (to avoid duplication)
#'
#' @return The merged dataframe
#' @export
#'
#' @examples rbind_dfs(Orange, mtcars);
rbind_dfs <- function(x,
                      y,
                      clearRowNames = TRUE) {

  if (is.null(y) || (nrow(y) == 0)) {
    return(x);
  } else if (is.null(x) || (nrow(x) == 0)) {
    return(y);
  }

  xNames <- names(x);
  yNames <- names(y);

  resNames <- c(xNames,
                setdiff(yNames, xNames));

  inX_not_inY <- setdiff(xNames, yNames);
  inY_not_inX <- setdiff(yNames, xNames);

  if (length(inY_not_inX) > 0) {
    xComplement <-
      as.data.frame(lapply(inY_not_inX,
                           function(cols) {
                             return(rep(NA, nrow(x)));
                           }));
    names(xComplement) <- inY_not_inX;
    fullX <- cbind(x, xComplement)[, resNames];
  } else {
    fullX <- x[, resNames];
  }

  if (length(inX_not_inY) > 0) {
    yComplement <-
      as.data.frame(lapply(inX_not_inY,
                           function(cols) {
                             return(rep(NA, nrow(y)));
                           }));
    names(yComplement) <- inX_not_inY;
    fullY <- cbind(y, yComplement)[, resNames];
  } else {
    fullY <- y[, resNames];
  }

  res <-
    rbind(fullX, fullY);

  if (clearRowNames) {
    row.names(res) <- NULL;
  } else {
    row.names(res) <-
      c(row.names(x), row.names(y));
  }

  return(as.data.frame(res));

}
