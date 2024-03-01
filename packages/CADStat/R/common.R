#' @export
# Sets Visibility
setVisible <- function(jObj, bVisible)
{
  invisible(.jcall(jObj,,"setVisible",bVisible))
}
#' @export
load.class = function(class){
  globenv <- globalenv()
  globenv[[".stv"]] <- .jnew(class); 
  setVisible(globenv[[".stv"]], TRUE)
}
#' @export
jgr.__removeMenu = function(name) 
{
 # jgr.removeMenu("Workspace")
    if (!.jgr.works) {
        cat("jgr.removeMenu() cannot be used outside JGR.\n")
        return(invisible(NULL))
    }
    invisible(.jcall(.jnew("org/neptuneinc/cadstat/JGRCustomizer"),"V", "removeMenu", as.character(name)))
}
