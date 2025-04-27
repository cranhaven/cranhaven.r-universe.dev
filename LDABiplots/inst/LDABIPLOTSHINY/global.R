require(htmlwidgets)
require(shinyBS)
################ exportation list options for graphic
export <- list(
  list(
    text = "png",
    onclick = htmlwidgets::JS("function () {
                   this.exportChart({ type: 'image/png' }); }")
  ),
  list(
    text = "jpeg",
    onclick = htmlwidgets::JS("function () {
                   this.exportChart({ type: 'image/jpeg' }); }")
  ),
  list(
    text = "svg",
    onclick = htmlwidgets::JS("function () {
                   this.exportChart({ type: 'image/svg+xml' }); }")
  ),
  list(
    text = "pdf",
    onclick = htmlwidgets::JS("function () {
                   this.exportChart({ type: 'application/pdf' }); }")
  )
)


#######Avoid accidentally closing a bsModal
bsModalNoClose <- function(...) {
  b <- shinyBS::bsModal(...)
  b[[2]]$`data-backdrop` <- "static"
  b[[2]]$`data-keyboard` <- "false"
  return(b)
}

removeSparseTerms <- function (x, sparse)
{
  stopifnot(inherits(x, c("DocumentTermMatrix", "TermDocumentMatrix")),
            is.numeric(sparse), sparse > 0, sparse < 1)
  m <- if (inherits(x, "DocumentTermMatrix"))
    t(x)
  else x
  t <- table(m$i) > m$ncol * (1 - sparse)
  termIndex <- as.numeric(names(t[t]))
  if (inherits(x, "DocumentTermMatrix"))
    x[, termIndex]
  else x[termIndex, ]
}

#############

themesb <- list(
  "grey" = ggplot2::theme_grey(),
  "minimal" = ggplot2::theme_minimal(),
  "bw" = ggplot2::theme_bw(),
  "classic" = ggplot2::theme_classic(),
  "linedraw" = ggplot2::theme_linedraw(),
  "light" = ggplot2::theme_light(),
  "dark" = ggplot2::theme_dark(),
  "test" = ggplot2::theme_test(),
  "void" = ggplot2::theme_void()
  )
