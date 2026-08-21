#' Summarizing Exact Trees Information
#'
#' Summary method for an object of class \code{ETree}.
#'
#' @param object a \code{ETree} object. This can be the output of \code{\link{ETree}}.
#' @param TerminalNodes Number of terminal nodes of the tree to summarize.
#' @param \dots optional additional arguments.
#' @param digits specified number of decimal places (default is 3).
#'
#' @return prints a summarized version of the \code{ETree} output.
#'
#' @details This function is a method for the generic function summary for class
#'   \code{ETree}. It extracts the following essential components from a \code{ETree}
#'   object: 1) Split information;
#'   2) Leaf information, and 3) CV information.
#'
#' @examples
#' \donttest{
#'   data(iris)
#'   # Fit an Exact Tree model
#'   controlEtree <- ETree.control(measure=0, maxsize = 4, maxdepth = 3,
#'   minbucket = 5, ncv=5, alltreesizes = FALSE)
#'   tree<-ETree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
#'   control= controlEtree, data = iris)
#'   summary(tree, TerminalNodes=4)
#' }
#'
#' @keywords summary
#'
#' @export
summary.ETree<-function(object,TerminalNodes=NULL,digits=3,...){


  object<-object$Transf_Trees


  if(is.null(TerminalNodes)||(length(object)==1)){

    if(length(object)!=1){
      stop("Terminal nodes is NULL. Please, indicate the number of terminal nodes.")
    }

    object<-object[[1]]

  }else{

    if(length(object)<TerminalNodes){
      stop("TerminalNodes is larger than the maximum number of terminal nodes.")
    }

    object<-object[[TerminalNodes]]

  }

  if(is.null(dim(object$si)[2])){
    #cat("\n")
  }else{
    cat("Split information:","\n")
    if(is.numeric(object$si[,5])){
      object$si[,5]<-round(object$si[,5],digits = digits)
    }
    print(object$si,row.names=TRUE)
  }
  cat("\n")
  cat("Leaf information:","\n")
  #options(warn=-1)
  print(round(object$li[,c(2:4)],digits=digits))

  #options(warn=-1)
  if(!is.null(object$CVOutput)){
    cat("\n")
    cat("CV information:","\n")
    print(round(object$CVOutput,digits=digits))
  }

}
