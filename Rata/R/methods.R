#' @rdname ata
#' @param x an ATA object
#' @export
print.ata <- function(x, ...){
  cat("Assemble", nrow(x$form_map), "forms from", length(unlist(x$groups)), "items.\n")

  if(is.null(x$items)) {
    cat("The ATA problem hasn't been solved yet\n")
  } else {
    cat("The ATA problem has been solved.\n")
    cat(x$status, ', optimum: ', round(x$optimum, 3), ' (', paste(round(x$obj_vars, 3), collapse=', '), ')\n', sep='')

    items <- ata_extract_items(x)
    if(is.list(items) && is.null(names(items)))
      items <- ata_results_to_model(items)
    if(is.list(items) && !is.null(names(items)))
      items <- ata_results_to_dataframe(items)

    if(nrow(items) <= 10) {
      print(items)
    } else {
      print(items[1:5, ])
      cat("...\n")
      print(items[-4:0 + nrow(items), ])
    }

    cat("See more results in 'x$items' or 'x$results' (x is the ATA object).")
  }

  invisible(x)
}


#' @rdname ata
#' @importFrom Rirt model_mixed_info
#' @importFrom stats aggregate
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
plot.ata <- function(x, ...){
  if(class(x) != "ata")
    stop("Not an 'ata' object")

  if(is.null(x$items))
    stop("The ATA problem hasn't been solved yet")

  opts <- list(...)
  if(is.null(opts$theta))
    opts$theta <- round(seq(-3, 3, .1), 1)
  n_thetas <- length(opts$theta)

  n_forms <- nrow(x$form_map)
  items <- ata_extract_items(x)
  info <- sapply(items, function(xx) {
    xx <- model_mixed_info(opts$theta, xx, D=x$opts$D)
    rowSums(xx, na.rm=TRUE)
  })
  colnames(info) <- paste('Form', 1:n_forms)
  info <- cbind(t=opts$theta, info)
  info <- melt(as.data.frame(info), id.var="t", variable.name='Forms')
  ggplot(info, aes_string(x="t", y="value", color="Forms")) +
    geom_line() + xlab(expression(theta)) + ylab("Test Information") +
    theme_bw() + theme(legend.key=element_blank())
}
