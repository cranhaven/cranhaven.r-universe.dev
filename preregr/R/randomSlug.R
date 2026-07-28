#' Generate a random slug
#'
#' idSlug is a convenience function with swapped argument order.
#'
#' @param x Length of slug
#' @param id If not NULL, prepended to slug (separated with a dash) as id; in
#' that case, it's also braces and a hash is added.
#' @param chars Characters to sample from
#'
#' @return A character value.
#' @rdname randomSlug
#' @export
#'
#' @examples randomSlug();
#' idSlug("identifier");
randomSlug <- function(x = 10,
                       id = NULL,
                       chars = c(letters, LETTERS, 0:9)) {
  res <- do.call(paste0, as.list(sample(chars, x)));
  if (!is.null(id)) {
    res <- paste0(" {#", id, "-", res ,"}");
  }
  return(res);
}

#' @rdname randomSlug
#' @export
idSlug = function(id = NULL,
                  x = 10,
                  chars = c(letters, LETTERS, 0:9)) {
  return(randomSlug(x=x, id=id, chars=chars));
}
