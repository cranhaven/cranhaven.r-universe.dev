#' UMAP for Symbolic Data
#'
#' @description This function applies the UMAP algorithm to a symbolic data table.
#'
#' @param sym.data A symbolic data table.
#' @param ... Additional arguments to be passed to the UMAP algorithm.
#' @keywords Intervals
#' @export
#'
#'
sym.umap <- function(sym.data, ...) {
  UseMethod("sym.umap")
}

#' @keywords Intervals
#'
expand_rows <- function(df){
  l <- lapply(seq_len(ncol(df)), function(x) list(1,2))
  df_i <- expand.grid(l)
  funs <- list(min,max)
  funs <- lapply(df_i, function(i) funs[unlist(i)])

  out <- lapply(seq_len(nrow(df)), function(i){
    fila <- df[i,]
    out <- lapply(seq_along(funs), function(i) {
      unlist(lapply(funs[[i]], function(.f) .f(fila[[i]])))
    })
    out <- as.data.frame(do.call(cbind.data.frame, out))
    colnames(out) <- colnames(df)
    return(out)
  })
  out <- as.data.frame(do.call(rbind.data.frame, out))
  colnames(out) <- colnames(df)
  return(out)
}

#' UMAP for symbolic data tables
#' @rdname sym.umap
#' @param sym.data symbolic data table
#' @param config object of class umap.config
#' @param method 	character, implementation. Available methods are 'naive' (an implementation written in pure R) and 'umap-learn' (requires python package 'umap-learn')
#' @param preserve.seed logical, leave TRUE to insulate external code from randomness within the umap algorithms; set FALSE to allow randomness used in umap algorithms to alter the external random-number generator
#' @param ... list of settings; values overwrite defaults from config; see documentation of umap.default for details about available settings
#' @export
#' @import umap
#'
sym.umap.symbolic_tbl <- function(sym.data = NULL,
                                 config = umap::umap.defaults,
                                 method = c("naive", "umap-learn"),
                                 preserve.seed = TRUE, ...){
  ext <- expand_rows(sym.data)
  config$n_components <- ncol(ext)
  umap_df <- umap::umap(scale(ext), config)

  res_umap <- as.data.frame(umap_df$layout)

  class(res_umap) <- c("sym_umap",class(res_umap))
  attr(res_umap,"names_umap") <- attr(sym.data,"row.names")
  return(res_umap)
}

#' Plot UMAP for symbolic data tables
#' @param  x sym_umap object
#' @param ... params for plot
#' @export
#' @import ggplot2
#' @importFrom dplyr group_by summarise
#'
plot.sym_umap <- function(x, ...){
  l <- length(attr(x, "names_umap"))
  x$group <- sort(rep(1:l,(nrow(x)/l)))

  x2 <- x %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(xmin = min(V1), xmax = max(V1),
              ymin = min(V2), ymax = max(V2))
  x2$var <- attr(x, "names_umap")

  ggplot(
    data = x2,
    mapping = aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      color = as.factor(var)
    )
  ) +
    geom_rect(fill = NA, alpha = 0.7,show.legend = FALSE, size = 1) +
    geom_text(aes(x=xmin+(xmax-xmin)/2, y=ymin+(ymax-ymin)/2, label=var), size=6,
              show.legend = FALSE) +
    theme_minimal() +
    labs(x = "dim.1",
         y = "dim.2")
}


