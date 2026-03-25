#' Stratified.ceg.model
#'
#' The \code{Stratified.ceg.model} is a S4 class that extends \code{Ceg.model}.
#'  The object represents a CEG model derived from its supporting Stratified.staged.tree
#'  using some graphical transformation rules.
#'
#' @include ceg_model.R stratified_staged_tree.R
#' @export
#'
setClass(
  "Stratified.ceg.model",
  representation(),
  contains = "Ceg.model",
  validity = function(object) {
    if (!methods::is(object@staged.tree, "Stratified.staged.tree"))
      stop(
        "an instance of Stratified.staged.tree S4 object must be provided to
        Stratified.ceg.model@staged.tree slot"
      )
    else
      TRUE
  }
)


setMethod(
  f = "initialize",
  signature = "Stratified.ceg.model",
  definition = function(.Object,
                        staged.tree,
                        position) {
    cat("~~~ CEGStretified: initializator ~~~ \n")
    # Assignment of the slots
    .Object@staged.tree <- staged.tree
    .Object@position <- position
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)

# TODO(Taranti) remover stage.structure dos construtores de CEG e incluir o calculo e deposito valor Position.

#' Stratified.ceg.model constructor.
#'
#' S3 function to friendly construct S4  Stratified.ceg.model.
#'
#' @param stratified.staged.tree    Stratified.staged.tree S4 object    A staged tree is called stratified if
#'    its supporting event tree is stratified and all vertices which are in the same stage
#'    are also at the same distance of edges from the root.
#
#' @return a Stratified.ceg.model S4 object.
#'
#' @examples
#' scm <- Stratified.ceg.model(sst)
#'
#' @export
#'
#' @include stratified_ceg_model.R
#'
Stratified.ceg.model <- function(stratified.staged.tree)
{
  # TODO(Collazo) o codigo mstra o que era position

  position <- StratifiedCegPosition(stratified.staged.tree@stage.structure,
                          stratified.staged.tree@event.tree@num.category,
                          stratified.staged.tree@event.tree@num.situation)
  return(new("Stratified.ceg.model", stratified.staged.tree, position))
}




#' Stratified.ceg.model Plotting
#'
#' This Method is used to plot a chain event graph from a Stratified.ceg.model S4 object.
#' The current \code{ceg} package implementation depends on \code{Rgraphviz}
#' package from Bioconductor to draw the CEG graph.
#'
#' @param x  Stratified.ceg.model S4 object.
#'
#' @return the plot and also a pdf version is saved in the working directory.
#' @export
#'
#' @examples
#' plot(scm)
#'
#'
setMethod(
  f = "plot",
  signature = "Stratified.ceg.model",
  definition = function(x) {

    ceg.graph.simple <- CegGraphSimple(x@staged.tree@event.tree, x@position)



    g <-
      new(
        "graphNEL",
        nodes = ceg.graph.simple$node$nodes,
        edgeL = ceg.graph.simple$edge$edges ,
        edgemode = "directed"
      )

    # 1.  To set general graphical attributes
    attrsAtt <- list()
    graphAtt <-
      list(rankdir = "LR",
           size = "18.0,24.0",
           bgcolor = "white")  # LF (left-right) is the graph orientation
    edgeAtt   <- list(color = "cyan")
    nodeAtt  <-
      list(fillcolor = "lightgreen",
           shape = "ellipse",
           fixedsize = FALSE)
    attrsAtt <- list(node = nodeAtt,
                     edge = edgeAtt,
                     graph = graphAtt)


    #  2.  Nodes attributes
    # To change node names
    nodes.label.list <- ceg.graph.simple$node$nodes
    names(nodes.label.list) <- graph::nodes(g)
    nAttrs <- list()
    nAttrs$label <- nodes.label.list

    #  3.  edges attributes
    # To change the edge names (default is no-name)


    #1st option - Use edges ordered list to name edges
    edges.label.list <- ceg.graph.simple$edge$label
    names(edges.label.list) <- graph::edgeNames(g)
    eAttrs <- list()
    eAttrs$label <- edges.label.list

    #primeiro estagio
    Rgraphviz::plot(
      g,
      main = "Chain Event Graph (propagation analysis) ",
      nodeAttrs = nAttrs,
      edgeAttrs = eAttrs,
      attrs = attrsAtt
    )
    grDevices::pdf(
      "./figure01ceg.pdf",
      width = 8,
      height = 6,
      title = "Chain Event Graph (propagation analysis)"
    ) #width and heignt in inches / title is embed in the pdf image
    Rgraphviz::plot(
      g,
      main = "Chain Event Graph (propagation analysis)",
      nodeAttrs = nAttrs,
      edgeAttrs = eAttrs,
      attrs = attrsAtt
    )
    grDevices::dev.off()

    # inserting colors
    nAttrs$fillcolor <- ceg.graph.simple$node$color
    names(nAttrs$fillcolor) <- graph::nodes(g)

    #2nd stage
    Rgraphviz::plot(
      g,
      main = "Chain Event Graph",
      nodeAttrs = nAttrs,
      edgeAttrs = eAttrs,
      attrs = attrsAtt
    )
    grDevices::pdf(
      "./figure02ceg.pdf",
      width = 8,
      height = 6,
      title = "Chain Event Graph"
    )
    Rgraphviz::plot(
      g,
      main = "Chain Event Graph",
      nodeAttrs = nAttrs,
      edgeAttrs = eAttrs,
      attrs = attrsAtt
    )
    grDevices::dev.off()
  }
)






#' StratifiedCegPosition
#'
#' This function obtains the position structure associated with
#' a stratified CEG.
#'
#' @param stage  (list) - stage structure associated with a particular variable.
#' @param num.category (vector) - number of edges that unfold from stages
#'        associated with a particular variable.
#' @param num.situation (vector) - number of situations associated with each
#'        variable.
#'
#' @return list of lists \itemize{
#'     \item First list level identifies a variable 'v'.
#'     \item Second list level identifies a stage 'a' associated with
#'           a variable 'v'.
#'     \item The third list level identifies the positions associated with
#'           a stage 'a' .
#'           }
#'
#'  @seealso \code{PositionLevel},  \code{PositionVector},
#'  \code{PositionStage}, \code{PairwisePosition}
#'
StratifiedCegPosition <- function(stage, num.category, num.situation) {
  num.level <- length(num.category)
  result <- list()
  length(result) <- num.level
  result[[num.level]] <-
    PositionLevel(stage[[num.level]]@cluster, 0, num.situation[num.level])
  for (level in (num.level - 1):2) {
    result[[level]] <- PositionLevel(stage[[level]]@cluster,
                                     num.category[level],
                                     num.situation[level + 1],
                                     result[[level + 1]])
  }
  result[[1]] <- list(list(1))
  return(result)
}




#' PositionLevel
#'
#' This function obtains the position structure associated with a particular
#' variable of a CEG.
#'
#' @param stage.list  (list) - stage structure associated with a particular
#'        variable.
#' @param num.category (vector) - number of edges that unfolds from each position
#'        asscoiated with our target variable
#' @param num.situation.next (numeric) - number of situations associated with the variable
#'        that follows our target variable in the event tree.
#' @param pos.next.level  (list) - position structure associated with the
#'        variable that follows our target variable in the event tree (see function PositionLevel)
#'
#' @return  list of lists - The first list level identifies a stage 'i' and the
#'          second list level identifies the positions associated with this
#'          stage 'i'.
#'
#'
#' @seealso  \code{\link{PositionVector}}, \code{\link{PositionStage}} and
#'          \code{\link{PairwisePosition}}
#'
#'
PositionLevel <- function(stage.list,
                          num.category,
                          num.situation.next,
                          pos.next.level = list()) {
  aux <- which(!is.na(stage.list))
  N <- length(aux)
  if (num.category == 0) {
    stage.list <- lapply(1:N, function(x) list(stage.list[[aux[x]]]))
    return(stage.list)
  }
  stage.list <- lapply(1:N, function(x) stage.list[[aux[x]]])
  pos.next.level <- PositionVector(num.situation.next, pos.next.level)
  result <- lapply(1:N, function(x)
    PositionStage(stage.list[[x]], num.category, pos.next.level))
  return(result)
}



#' \code{PositionVector} function rewrites a position structure associated with
#'       a particular variable: from a list to a vector.
#'
#' @param num.situation  (numeric) - number of situation associated with a particular variable.
#' @param pos.list  (list) - stage structure associated with a particular variable that follows
#'        the variable associated with our target position.
#'
#' @return  vector
#'
PositionVector <- function(num.situation, pos.list) {
  num.situation <- length(pos.list)
  pos.vec <- rep(0, num.situation)
  count <- 1
  for (stage in 1:num.situation) {
    for (pos in 1:length(pos.list[[stage]])) {
      pos.vec[pos.list[[stage]][[pos]]] <- count
      count <- count + 1
    }
  }
  return(pos.vec)
}

#' PositionStage
#'
#' \code{PositionStage} function yields the position structure associated with
#' a particular
#' stage of a CEG.
#'
#' @param stage.vector (vector) - a set of situations that constitute a
#'        particular stage
#' @param num.category (numeric) - number of edges that unfolds from the
#'        situations
#' @param pos.next.level (vector) - It identifies the positions corresponding to
#'        all situations that are children of situations associated with the variable
#'        spanning our target stage.
#'
#' @return  list of vector - Each vector identifies a position.
#'
#' @seealso  \code{\link{PairwisePosition}}
#'
PositionStage <- function(stage.vector, num.category, pos.next.level) {
  stage.vector <- sort(stage.vector)
  result <- list()
  count <- 1

  stop <- FALSE
  N <- length(stage.vector)
  if (N == 1)
    return(list(stage.vector))

  while (stop == FALSE) {
    aux.stage <- sapply(2:N, function(x)
      PairwisePosition(c(stage.vector[1],
                         stage.vector[x]),
                       num.category,
                       pos.next.level))
    aux.stage <- c(TRUE, aux.stage)
    result[[count]] <- stage.vector[aux.stage]
    count <- count + 1
    stage.vector <- stage.vector[!aux.stage]
    N <- length(stage.vector)
    if (N == 1) {
      stop <- TRUE
      result[[count]] <- stage.vector
    } else if (N == 0)
      stop <- TRUE
  }
  return(result)
}




#' PairwisePosition
#'
#' The \code{PairwisePosition } function identifies if two situations are in
#' the same position given that they are in the same stage.
#'
#' @param pair.situation (vector) - situations to be analysed
#' @param num.category  (numeric) - number of edges that unfolds from the
#'        situations
#' @param pos.next.level  (vector) - It identifies the positions corresponding to
#'        all situations that are children of situations associated with the variable
#'        spanning our target stage.
#'
#' @return  boolean
#'
PairwisePosition <- function(pair.situation, num.category, pos.next.level) {
  situation.1 <- (pair.situation[1] - 1) * num.category + 1:num.category
  #It identifies the situations that unfold from situation 1.
  situation.2 <- (pair.situation[2] - 1) * num.category + 1:num.category
  #It identifies the situations that unfold from situation 2.
  situation.1 <- pos.next.level[situation.1]
  #It identifies the positions that unfold from situation 1.
  situation.2 <- pos.next.level[situation.2]
  #It identifies the positions that unfold from situaion 2.
  aux <- sum(situation.1 == situation.2)
  if (aux != num.category)
    return(FALSE) else return(TRUE)
}
