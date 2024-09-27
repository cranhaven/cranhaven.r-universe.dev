##' Class 'EnrichResult'
##' This class represents the result of enrich analysis
##'
##'
##' @name EnrichResult-class
##' @aliases  EnrichResult-class
##' @docType class
##' @slot enrich a data frame of enrichment result
##' @slot test Statistical test
##' @slot method Multiple test correction methods
##' @slot m  the maximum number of ancestor layers for ontology enrichment
##' @slot maxGsize The maximum number of DOTerm genes in enrichment analysis
##' @slot minGsize The minimum number of DOTerm genes in enrichment analysis
##' @slot traditional Indicates whether the traditional ORA method is used
##' @slot delta The highest p-value of significance for each node
##' @slot penalize Whether to use penalty function in enrichment analysis
##' @slot interestGenes A valid interest gene set
##' @exportClass EnrichResult
##' @author Haixiu Yang
##' @keywords classes
setClass("EnrichResult",
         representation = representation(
           enrich = "data.frame",
           interestGenes = "character",
           test = "character",
           method = "character",
           m = "numeric",
           maxGsize = "numeric",
           minGsize = "numeric",
           delta = "numeric",
           traditional = "logical",
           penalize = "logical")
         )
