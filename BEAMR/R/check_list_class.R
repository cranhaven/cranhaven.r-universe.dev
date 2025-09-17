#' Check that each element of a list is of a required class
#'
#' @param list.object A list used in BEAMR analysis
#' @param required.class Class for list elements, e.g. matrix
#'
#' @returns Logical TRUE if list is of required class
#' @export
#'
#' @examples
#' data(omicdat)
#' check_list_class(omicdat, "matrix")
check_list_class=function(list.object,
                          required.class)

{
  obj.cls=class(list.object)

  n.comp=length(list.object)
  ok=rep(F,n.comp)
  for (i in 1:n.comp)
  {
    cls=class(list.object[[i]])
    ok[i]=any(is.element(cls,required.class))
  }
  res=(all(ok))&(is.element(obj.cls,"list"))

  return(res)
}
