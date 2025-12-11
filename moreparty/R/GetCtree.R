#' @importFrom methods new

#' @export

GetCtree <- function(object, k=1) {
  dt <- object@data@get("input")
  tr <- party::prettytree(object@ensemble[[k]], names(dt))
  tr_updated <- update_tree(tr, dt)
  methods::new("BinaryTree", tree=tr_updated, data=object@data, responses=object@responses, 
      cond_distr_response=object@cond_distr_response, predict_response=object@predict_response)
}

update_tree <- function(x, dt) {
  x <- update_weights(x, dt)
  if(!x$terminal) {
    x$left <- update_tree(x$left, dt)
    x$right <- update_tree(x$right, dt)   
  } 
  x
}

update_weights <- function(x, dt) {
  splt <- x$psplit
  spltClass <- attr(splt,"class")
  spltVarName <- splt$variableName
  spltVar <- dt[,spltVarName]
  spltVarLev <- levels(spltVar)
  if (!is.null(spltClass)) {
    if (spltClass=="nominalSplit") {
      attr(x$psplit$splitpoint,"levels") <- spltVarLev   
      filt <- spltVar %in% spltVarLev[as.logical(x$psplit$splitpoint)] 
    } else {
      filt <- (spltVar <= splt$splitpoint)
    }
    x$left$weights <- as.numeric(filt)
    x$right$weights <- as.numeric(!filt)
  }
  x
}