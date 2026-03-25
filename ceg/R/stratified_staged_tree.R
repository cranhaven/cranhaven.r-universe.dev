# TODO(collazo) inversao ordem do df  - para gerar erro:
#   df <- read.csv("./R/CHDS.latentexample1.csv")
#   df1 <- df[c(4,3,2,1)]
#   st  <- Stratified.staged.tree(df1)







#' Stratified.staged.tree
#'
#' A stratified staged tree is a staged tree whose supporting event tree is stratified
#' and all vertices which are in the same stage are also at the same distance of edges
#' from the root.
#'
#' @include staged_tree.R event_tree.R stratified_event_tree.R
#'
# TODO(Taranti) Verificar definição de situação.
#' @slot event.tree Stratified.event.tree. An stratified event tree is an event tree whose set of events that unfold from all situations,
#' which are at the same distance of edges from the initial situation, are identical.
#' @slot situation list.
#' @slot contingency.table list of matrices that represent the contigency tables associated with each variable in the event tree.
#' @slot stage.structure list in which each component is a list associated with a variable in the staged tree that has the following data structure:
#'    \itemize{
#'    \item $score - numeric. This is the logarithmic form of the marginal likelihood associated with a particular variable.
#'    \item $cluster - list whose components are vectors. Each vector represents a stage associated with a particular variable.
#'    }
#' @slot stage.probability list in which each component is a list associated with a variable in the staged tree.
#'       Each component of this sublist is a vector that represents the probability distribution associated with
#'       a particular stage of the target variable.
#' @slot prior.distribution list of matrices. Each matrix is a collection of vectors that correspond t
#'       a prior distribution for each situation associated with a particular variable.
#' @slot posterior.distribution list of matrices. Each matrix is a collection of vectors that correspond t
#'       a prior distribution for each situation associated with a particular variable.
#' @slot model.score numeric. This is the logarithmic form of the marginal likelihood.
#' @export
#'
setClass(
  "Stratified.staged.tree",
  representation(    situation = "list",
                     contingency.table = "list",
                     stage.structure = "list",
                     stage.probability = "list",
                     prior.distribution = "list",
                     posterior.distribution = "list",
                     model.score = "numeric"),
  contains = "Staged.tree",
  validity = function(object) {
    if (!methods::is(object@event.tree, "Stratified.event.tree"))
      stop("an instance of Stratified.event.tree S4 object must be provided to
           Stratified.staged.tree@event.tree slot ")
    else
      TRUE
  }
)


setMethod(
  f = "initialize",
  signature = "Stratified.staged.tree",
  definition = function(.Object,
                        event.tree = "Event.tree",
                        situation = "list",
                        contingency.table = "list",
                        stage.structure = "list",
                        stage.probability = "list",
                        prior.distribution = "list",
                        posterior.distribution = "list",
                        model.score = "numeric"
  ){
    cat("~~~ Stratified.staged.tree: initializator ~~~ \n")
    # Assignment of the slots
    .Object@event.tree <- event.tree
    .Object@situation <- situation
    .Object@contingency.table <- contingency.table
    .Object@stage.structure <- stage.structure
    .Object@stage.probability <- stage.probability
    .Object@prior.distribution <- prior.distribution
    .Object@posterior.distribution <- posterior.distribution
    .Object@model.score <- model.score
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)



#' Stratified.staged.tree
#'
#' Constructor method to Stratified.staged.tree S4 objects. It accepts different
#' sets for parameters types.
#'
#' @param x (data.frame) is a well behavioured data set or (Stratified.event.tree)
#' @param y (numeric) alpha or (list) that represents the stage.structure.
#' To construct it, the user must plot the Stratified.event.tree graph and use
#' the labelled number of each node.
#' @param z (numeric) variable.order
#' @param ... (not used)
#'
#' @return a Stratified.staged.tree S4 object
#'
#' @examples
#' sst <- Stratified.staged.tree(artificial.chds)
#'
#' @examples
#' stt.manual <- Stratified.staged.tree(set.manual,
#' list(list(c(2,3)), list(c(4,7,12),c(5,8,9))))
#'
#' @note
#' The implementation admits providing the three arguments, or the first two, or
#'  even only the data.frame.\cr
#'  The default variable order is as in the data.frame and the default alpha is
#'  1L.\cr
#'  To manualy create a stratified.event.tree from a stratified.event.tree:\cr
#'  \describe{
#'  \item{1st}{plot the stratified.event.tree - \code{plot(set)}}
#'  \item{2nd}{Looking the graph, you can create the stage structure,
#'  such as: \code{stage.structure <- list(list(c(2,3)), list(c(4,7,12),c(5,8,9)))}}
#'  \item{3rd}{Finally you can create your Stratified.event.tree:
#'  \code{st.manual<- Stratified.staged.tree(set, stage.structure)}}
#'  }
#'
#' A call to \code{Stratified.staged.tree( )} with no parameters will return
#'  an error message for missing argument. \cr
#' A call to \code{Stratified.staged.tree(x, ...)}, x not being a data.frame or
#' a Event.tree, will return an error message.
#'
#'@export
#'
setGeneric("Stratified.staged.tree",
           function(x, y, z, ...) standardGeneric("Stratified.staged.tree")
)

#' @rdname Stratified.staged.tree
setMethod("Stratified.staged.tree",
          signature("missing"),
          function(x, ...) {
            stop("constructor S4 method Stratified.staged.tree not implemented for missing argument")
          })

#' @rdname Stratified.staged.tree
setMethod("Stratified.staged.tree",
          signature(x = "ANY"),
          function(x, ...) {
            stop("constructor S4 method Stratified.staged.tree not implemented
                 for this argument")
          })

#' @rdname Stratified.staged.tree
setMethod("Stratified.staged.tree",
          signature( x = "data.frame", y = "numeric", z = "numeric"),
          function(x = "data.frame", y = 1L, z = 0L ) {
            data.frame <- x
            alpha <- y
            variable.order <- z

            if (!CheckForCleanData(data.frame)) {
              stop("Consider using CheckAndCleanData() function")
            }

            num.variable <- length(data.frame[1, ])
            # TODO(Taranti)  inserir validador de menor valor alpha (maior ou igual a 0)
            if (variable.order[1] != 0) {
              data.frame <- data.frame[variable.order]
            }

            event.tree <- Stratified.event.tree(data.frame)
            prior.distribution <- PriorDistribution(event.tree, alpha)
            contingency.table <- ContingencyTable(data.frame, event.tree)
            stage.structure <- lapply(1:(num.variable), function(x) OAHC(x, prior.distribution, contingency.table, event.tree))
            model.score <- sum(sapply(1:(num.variable), function(y) stage.structure[[y]]@score))


            out <-  new("Stratified.staged.tree",
                        event.tree,
                        situation = list(),
                        contingency.table,
                        stage.structure,
                        stage.probability = list(),
                        prior.distribution,
                        posterior.distribution = list(),
                        model.score)
            return(out)

          })

#' @rdname Stratified.staged.tree
setMethod("Stratified.staged.tree",
          signature( x = "data.frame", y = "numeric", z = "missing"),
          function(x = "data.frame", y = 1L) {  Stratified.staged.tree(x, y, 0 )}
)

#' @rdname Stratified.staged.tree
setMethod("Stratified.staged.tree",
          signature( x = "data.frame", y = "missing", z = "missing"),
          function(x = "data.frame") {  Stratified.staged.tree(x, 1, 0 )}
)


#' @rdname Stratified.staged.tree
setMethod("Stratified.staged.tree",
          signature(x = "Stratified.event.tree", y = "list" ),
          function(x = "Stratified.event.tree", y = "list") {

            event.tree <- x
            stage.structure <- y

            situation <-  list()
            contingency.table <-  list()
            stage.probability <-  list()
            prior.distribution <-  list()
            posterior.distribution <-  list()
            model.score <-  1

            # stage.structure <-  list()
            # logica:
            #verificar se a arvore esta corretya
            #verificar se a stage.structure nao tem mais itens que variavel menos 1 ( ultimo nivel)



            #para cada nivel, ver numero de nos,
            #  para cada vetor
            #    ver se nao excede,
            #     crias lista de inteiro com N elementos com valores int = N
            #      colocar o vetor ordenado no no correto
            #        preencher com logic NA demais posicoes relativas ao vetor
            # Criar objetos OACH
            #
            # inserir na lista

            #a contagem inicia em 1 para o primeiro elemento da variavel. 01 aohc para cada variavel.


            PrepareStage <- function(nr.elementos, cluster.list){
              out <- as.list(1:nr.elementos)
              for (cluster in cluster.list) {
                cluster.sorted <- sort(cluster)
                out[[cluster.sorted[[1]]]] <- cluster.sorted
                for (x in 2:length(cluster.sorted))
                  out[[cluster.sorted[[x]]]] <- NA
              }
              out
            }



            out.stages <- vector("list", event.tree@num.variable)

            out.stages[[1]] <-  new("OAHC", 0, list(1))

            for (nr  in 1:length(stage.structure)) {


              correction.number <- sum(utils::head(event.tree@num.situation,nr)) - 1

              temp.stage <- lapply(stage.structure[[nr]], function(x) x - correction.number)




              out.stages[[nr + 1]] <- new("OAHC", 0, PrepareStage(event.tree@num.situation[[nr + 1]], temp.stage))

            }

            stage.structure <- out.stages

            return(
              new(
                "Stratified.staged.tree",
                event.tree,
                situation,
                contingency.table,
                stage.structure,
                stage.probability,
                prior.distribution,
                posterior.distribution,
                model.score
              ))

          }

)





#' Stratified.staged.tree Plotting
#'
#' Method to plot a Staged.tree S4 object. The current  package \code{ceg}
#' depends on \code{Rgraphviz} package from Bioconductor to draw graphs.
#'
#' @param x Stratified.staged.tree S4 object
#'
#' @return the plot. A pdf version is also saved in the working directory.
#' @export
#'
#' @examples
#' plot(sst)
#'
setMethod(
  f = "plot",
  signature = "Stratified.staged.tree",
  definition = function(x){
    #staged.tree.graph <- tree.graph(x)
    staged.tree.graph <- TreeGraph(x@event.tree, x@stage.structure)


    g <- new(
      "graphNEL",
      nodes = staged.tree.graph$node$nodes,
      edgeL = staged.tree.graph$edge$edges ,
      edgemode = "directed"
      )

    # 1.  Atributos do Grafico - Gerais
    attrsAtt <- list()
    graphAtt <- list(rankdir = "LR", size = "18.0,24.0", bgcolor = "white")  # o LR muda orientacao do grafico
    edgeAtt   <- list(color = "cyan")
    nodeAtt  <- list(fillcolor = "lightgreen", shape = "ellipse", fixedsize = FALSE)
    attrsAtt <- list(node = nodeAtt, edge = edgeAtt, graph = graphAtt)

    #  2.  atributos de nós
    # mudando o nome de nós
    nodesLabelList <- staged.tree.graph$node$nodes
    names(nodesLabelList) <- graph::nodes(g)
    nAttrs <- list()
    nAttrs$label <- nodesLabelList

    #  3.  atributos de arestas
    # mudando o nome de arestas (default branco)

    #opção 1 - atribuindo todos os nomes usando lista ordenada das arestas para atribuição
    edgesLabelList <- staged.tree.graph$edge$label
    names(edgesLabelList) <- graph::edgeNames(g)
    eAttrs <- list()
    eAttrs$label <- edgesLabelList

    # Inserindo cores
    nAttrs$fillcolor <- staged.tree.graph$node$color
    names(nAttrs$fillcolor) <- graph::nodes(g)

    Rgraphviz::plot(g, main = "Staged Tree Graph", nodeAttrs = nAttrs, edgeAttrs = eAttrs, attrs = attrsAtt)
    grDevices::pdf("./staged.tree.graph.pdf",  width = 8, height = 6, title = "")
    Rgraphviz::plot(g, main = "Staged Tree Graph", nodeAttrs = nAttrs, edgeAttrs = eAttrs, attrs = attrsAtt)
    grDevices::dev.off()
  }
)


#' NodeColor
#'
#' This function yields the node colors.
#'
#' @param num.variable  (numeric) - number of variables.
#' @param num.situation  (vector) - number of stages associated with each variable.
#' @param num.category (vector) - it identifies the number of edges that emanate from situations in each level.
#' @param stage.structure list with two components:
#'     \itemize{
#'     \item  numeric - score associated with a level
#'     \item  list of vectors - stage structure
#'     }
#' @param range.color  (numeric) - it chooses the palette. If 1, it is used a 8-color palette.
#'  If 2, it is used a 501-color palette.
#'
#' @return  vector - node colors
#'
#'
NodeColor <- function(num.variable,
                      num.situation,
                      num.category,
                      stage.structure,
                      range.color) {
  total.node <- cumsum(num.situation)
  result <- rep("white", total.node[num.variable] +
                  num.category[num.variable] * num.situation[num.variable])
  count <- 2
  if (range.color == 1) {
    color <- palette()
    color[1] <- "white"
  } else {
    if (range.color == 2) {
	    color <- colors(1)
    	    color <- color[-21]
    }
  }
  for (i in 2:num.variable) {
    for (j in 1:num.situation[i]) {
      if (!is.na(stage.structure[[i]]@cluster[[j]][1])) {
        if (length(stage.structure[[i]]@cluster[[j]]) == 1)
          result[j + total.node[i - 1]] <- "white" else {
            result[stage.structure[[i]]@cluster[[j]] + total.node[i - 1]] <-
              color[count]
            count <- count + 1
          }
      }
    }
  }
  return(result)
}


