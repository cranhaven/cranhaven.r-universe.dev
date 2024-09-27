##' show method for \code{EnrichResult} instance
##'
##' @title show method
##' @rdname show-methods
##' @param object A \code{EnrichResult} instance.
##' @importFrom methods show
##' @author Haixiu Yang
##' @return print info
##' @export
setMethod("show", "EnrichResult", function(object) {
    message("\n------------------------- EnrichResult object -------------------------\n")
    message("Method of enrichment:")
    if (object@traditional == TRUE) {
        message("\t classic ORA")
    } else if (object@penalize == TRUE) {
        message("\t Global Weighted Model")
    } else {
        message("\t Weighted Model")
    }
    message("\t '", object@test, "' Statistical model with the '", object@method, "' Multiple hypothesis correction")

    message("Enrichment cutoff layer: ", object@m)
    message("interestGenes number: ", length(object@interestGenes))
    DOIDcount <- length(dplyr::filter(object@enrich, cg.len != 0, gene.len >= object@minGsize, gene.len <= object@maxGsize)$DOID)
    sig_DOID <- length(dplyr::filter(object@enrich, p < object@delta)$DOID)
    message(DOIDcount, " DOTerms scored: ", sig_DOID, " terms with p < ", object@delta)

    message("Parameter setting:")
    message("\t Enrichment cutoff layer: ", object@m)
    message("\t Doterm gene number limit: minGsize ", object@minGsize, ", maxGsize ", object@maxGsize)
    message("\t Enrichment threshold: ", object@delta)

})




