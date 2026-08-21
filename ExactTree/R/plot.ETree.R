#' Visualisation of a Exact Tree
#'
#'
#' @param x transformed tree of class \code{ExactTree}.
#' @param TerminalNodes Number of terminal nodes of the tree to plot.
#' @param digits specified number of decimal places of the splitpoints in the graph
#'   (default is 2).
#' @param \dots additional arguments to be passed.
#'
#' @return A plot of the tree with the specified number of terminal nodes.
#'
#' @examples
#' \donttest{
#'   data(iris)
#'   # Fit an Exact Tree model
#'   controlEtree <- ETree.control(measure=0, maxsize = 4, maxdepth = 3,
#'   minbucket = 5, ncv=5, alltreesizes = FALSE)
#'   tree<-ETree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
#'   control= controlEtree, data = iris)
#'   plot(tree, TerminalNodes=4)
#' }
#'
#' @references Torsten Hothorn and Achim Zeileis (2013). partykit: A Toolkit for
#'   Recursive Partytioning. R package version 0.1-5.
#'
#' @seealso \code{\link{ETree}},\code{\link{summary.ETree}}
#' @keywords plot
#' @keywords as.party
#'
#' @importFrom partykit as.party character_split id_node is.terminal kids_node node_party nodeids party partynode partysplit plot.party split_node
#' @importFrom graphics par plot
#' @importFrom grid gpar grid.layout grid.lines grid.points grid.polygon grid.rect grid.text grid.yaxis popViewport pushViewport unit upViewport viewport grid.newpage grid.draw
#' @importFrom gridtext richtext_grob
#' @importFrom methods is
#'
#' @export
plot.ETree<-function(x, TerminalNodes=NULL, digits=2, ...){

  object<-x

  origVar<-c(object$Yvarnames, object$Xvarnames)
  origData<-object$Data
  x<-object$Transf_Trees



  if(is.null(TerminalNodes)||(length(x)==1)){

    if(length(x)!=1){
      stop("Terminal nodes is NULL. Please, indicate the number of terminal nodes.")
    }

    TAll<-x[[1]]
    TerminalNodes<-NROW(TAll$li)

  }else{

    if(length(x)<TerminalNodes){
      stop("TerminalNodes is larger than the maximum number of terminal nodes.")
    }

    TAll<-x[[TerminalNodes]]

  }


  TAll$origData<-origData[,origVar]
  colnames(TAll$origData)<-TAll$var.names
  TAll$Desc<-object$Desc

    if(TerminalNodes==1){
      #Here the plot for the tree with only root node

      Tree_plot<-TAll
      node<-1

      text<-paste0("*N* = ", Tree_plot$li[node,2] ,"<br>*&#375;* = ", round(Tree_plot$li[node,3],digits = digits),"<br>*h* = ",round(Tree_plot$li[node,4],digits = digits))
      box_gp <- gpar(col = "black", fill = "#DDD8C2", lty = 1)


      plotNode1<-richtext_grob(
        text, hjust=0,
        padding = unit(c(6, 6, 4, 6), "pt"),
        r = unit(c(0, 2, 4, 8), "pt"),
        gp = gpar(), box_gp = box_gp
      )


      grid.newpage()
      grid.draw(plotNode1)


    }else{
      #x is an object of class "ETrees"
      if(is.numeric(TAll$si[,5])){
        TAll$si[,5] <- round(TAll$si[,5],digits=digits)
      }
      TAll$li[,3] <- round(TAll$li[,3],digits=digits)
      TAll$li[,4] <- round(TAll$li[,4],digits=digits)
      party.ExactTree <- as.party(TAll)
      plot(party.ExactTree, inner_panel= node_ETrees, terminal_panel=terminal_ETrees, ...)
      return(invisible(party.ExactTree))
    }
}





# x<-1
# y<-1
#
#
# grid.newpage()
# grid.rect(x = 0.45, y = 0.5,
#           width = 0.25, height = 0.25,
#           just = "left", hjust = NULL, vjust = NULL,
#           default.units = "npc", name = NULL,
#           gp=gpar(), draw = TRUE, vp = NULL)
# x_d <- 0.5
# y_d <- 0.5
# node<-which(Tree_plot[,1]==0)
# grid.text(paste0("N = ", round(Tree_plot[node,6],digits = digits) ,"\ny = ", round(Tree_plot[node,7],digits = digits),"\nh = ",round(Tree_plot[node,5],digits = digits)), x=x_d, y=y_d,
#           just="left",gp=gpar(fontsize=10, col="black"))
#
#
#
#
# grid.newpage()
# x <- stats::runif(20)
# y <- stats::runif(20)
# rot <- stats::runif(20, 0, 360)
# grid.text("SOMETHING NICE AND BIG", x=x, y=y, rot=rot,
#           gp=gpar(fontsize=20, col="grey"))
# grid.text("SOMETHING NICE AND BIG", x=x, y=y, rot=rot,
#           gp=gpar(fontsize=20), check=TRUE)
# grid.newpage()
# draw.text <- function(just, i, j) {
#   grid.text("ABCD", x=x[j], y=y[i], just=just)
#   grid.text(deparse(substitute(just)), x=x[j], y=y[i] + unit(2, "lines"),
#             gp=gpar(col="grey", fontsize=8))
# }
# x <- unit(1:4/5, "npc")
# y <- unit(1:4/5, "npc")
# grid.grill(h=y, v=x, gp=gpar(col="grey"))
# draw.text(c("bottom"), 1, 1)
# draw.text(c("left", "bottom"), 2, 1)
#}
