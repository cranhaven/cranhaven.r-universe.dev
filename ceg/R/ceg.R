#' Chain Event Graph (ceg)
#'
#' This package has functionalities that allow us to create and learn
#' Chain Event Graph (CEG) models using a Bayesian framework. It provides us
#' with a Hierarchical Agglomerative algorithm to search the CEG model space.
#'
#' The package also includes several facilities for visualisations of the
#' objects associated with a CEG. The CEG class can represent a range of
#' relational data types, and supports arbitrary vertex, edge and graph
#' attributes. A Chain Event Graph is a tree-based graphical model that
#' provides a powerful graphical interface through which domain experts can
#' easily translate a process into sequences of observed events using plain
#' language. CEGs have been a useful class of graphical model especially to
#' capture context-specific conditional independences.
#'
#' Currently, ceg provides implementation to support the stratified family,
#' the user will use the following classes:
#'
#' \itemize{
#' \item Stratified.ceg.model
#' \item Stratified.staged.tree
#' \item Stratified.event.tree
#' }
#'
#' These classes are implemented as S4 classes and have constructor methods with
#' the same name as the class. A \code{plot} method is also provided.
#'
# imports
# @import methods
# @import methods Rgraphviz
#' @importClassesFrom graph graphNEL
#' @importMethodsFrom Rgraphviz plot
#' @importMethodsFrom graph nodes edgeNames
#' @importFrom graphics par
#' @importFrom methods as is new validObject
#' @importFrom grDevices colors dev.off graphics.off palette pdf
#' @importFrom stats complete.cases ftable
#' @importFrom utils data head
"_PACKAGE"
