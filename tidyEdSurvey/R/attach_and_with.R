#' @title Attach an \code{edsurvey.data.frame} to Search Path
#' @description Implements \code{\link{attach}} for an \code{edsurvey.data.frame} or a \code{light.edsurvey.data.frame} by attaching student level variables to the search path
#' @details Because \code{attach} is a standard generic function that does not use method dispatch, we set this function as generic,
#'          which means it overwrites \code{base::attach} on loading. If the object to attach is not an \code{edsurvey.data.frame} or a \code{light.edsurvey.data.frame},
#'          the function will revert to the standard \code{base} method.
#' @param what equivalent to `what` in \code{base::attach}, but can also be an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame}
#' @param pos equivalent to `pos` in \code{base::attach}
#' @param name equivalent to `name` in \code{base::attach}
#' @param warn.conflicts equivalent to `warn.conflicts` in \code{base::attach}
#' @return the environment is returned invisibly with a "name" attribute
#' @seealso \ifelse{latex}{\code{attach}}{\code{\link[base:attach]{attach}}}
#' @author Blue Webb
#' @export
setGeneric('attach',
           def = function(what, pos = 2L, name = deparse1(substitute(what), backtick = FALSE),
                          warn.conflicts = TRUE) {

             if(!inherits(what,"edsurvey.data.frame")){
               standardGeneric("attach")
             }else {
               level <- what$cacheDataLevelName
               if(what$survey %in% c("TIMSS","PIRLS","ePIRLS","TIMSS Advanced")){
                 message(paste0("Attaching ",level, " level variables to search path."))
               }
               vars = colnamesAttach(what,level)
               suppressWarnings(
                 z <- getData(what, varnames=vars, 
                              dropOmittedLevels = FALSE,
                              addAttributes=TRUE
                 )
               )
               base::attach(z, pos, name, warn.conflicts)
             }
           })


#' @method with edsurvey.data.frame
#' @export
with.edsurvey.data.frame <- function(data,expr,...){
  vars <- all.vars(substitute(expr))
  data_envr <- new.env()
  for(v in vars){
    tmp <- paste0("data$",v)
    data_envr[[v]] <- eval(parse(text = tmp))
  }
  data_envr <- as.data.frame(as.list(data_envr))
  eval(substitute(expr), data_envr, enclos=parent.frame())

}


