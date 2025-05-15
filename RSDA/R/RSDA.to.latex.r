#' RSDA.to.latex
#' @keywords internal
#' @author Jorge Arce Garro
#' @importFrom xtable xtable
RSDA.to.latex<-function(sym.data){
  return(xtable(generate.sym.table(sym.data)))
}
