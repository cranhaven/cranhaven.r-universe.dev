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




harmonicMean <- function(logLikelihoods, precision=2000L) {
    library("Rmpfr")
    llMed <- median(logLikelihoods)
    as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                         prec = precision) + llMed))))


}

###### Rescale numeric vector to have specified minimum "0" and maximum (1).
g4metric <- function (values){
    columns <- base::subset(values,
                            select = 2:ncol(values))
   values <- base::data.frame(values["topics"],
                              base::apply(columns,
                                          2,
                            function(column) {
                            scales::rescale(column,
                                             to = c(0, 1),
                                             from = range(column))
                                                             }))
    values <- reshape2::melt(values, id.vars = "topics", na.rm = TRUE)
    }

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

