#' Forest plot to display the result of a meta-analysis with replicability analysis resuls
#' 
#' @description
#' Draws a forest plot in the active graphics window (using grid
#' graphics system).
#' 
#' @aliases forest forest.metarep
#'
#' @param x An object of class 'metarep'.
#' @param ... Arguments to be passed to methods, see \code{forest.meta}
#'
#' @importFrom meta forest
#' @method forest metarep
#' @export
#' 
#' @return  No return value, called for side effects
#' @seealso \code{\link{forest.meta}}, \code{\link{metarep}},
#' 
#' @examples
#' n.i.1 <- c( 20, 208, 24, 190, 58, 36, 51)
#' a.i <- c( 2,79,0,98,15,34,9) 
#' n.i.2 <- c( 20, 119, 22, 185, 29, 51, 47)
#' c.i <- c(9,106,14,98,12,49,9) 
#' m1 <- metabin( event.e = a.i,n.e = n.i.1,event.c = c.i,n.c = n.i.2,
#'                studlab = paste0('Study ' , 1:7) , sm = 'OR' ,
#'                common = FALSE, random = TRUE )
#' mr1 <- metarep(  m1 , u = 2, common.effect = FALSE , t = 0.05 , 
#'                alternative = 'two-sided', report.u.max = TRUE)
#' forest(mr1, layout = "RevMan5", common = FALSE,
#'        label.right = "Favours control", col.label.right = "red",
#'        label.left = "Favours experimental", col.label.left = "green",
#'        prediction = TRUE)
#'        
forest.metarep <- function(x, ...) {
  
  chkclass(x, "metarep")
  
  u_max_text <- NULL
  
    rvalue.text <- formatPT(x$r.value , digits = 4)
    if( rvalue.text == '1.0000'  ){ 
      rvalue.text  <- '1' }
    
    rvalue.text <- paste0("Replicability analysis (r-value = ",
                          rvalue.text, ")")
  
  if( is.null( x$u_L)&(!is.null( x$u_R)) ){
    u_max_text <-paste0("Out of ", x$k, " studies, at least: ",
               x$u_R, " with increased effect")
  }
  if( is.null( x$u_R)&(!is.null( x$u_L)) ){
    u_max_text <-paste0("Out of ", x$k, " studies, at least: ",
               x$u_L, " with decreased effect")
  }
  
  if( (!is.null( x$u_R)) & (!is.null( x$u_L)) ){
    u_max_text <-paste0("Out of ", x$k, " studies, at least: ",
               x$u_R,  " with increased effect and "  ,
               x$u_L , " with decreased effect.")
  }
  
  class(x) <- class(x)[class(x) != "metarep"]
  ##
  if(!is.null(u_max_text)){
    forest(x,
           text.addline1 = rvalue.text ,
           text.addline2 = u_max_text ,
           ...)
  }else{
    forest(x,
           text.addline1 = rvalue.text ,
           ...)
  }
  ##
  invisible(NULL)
}
