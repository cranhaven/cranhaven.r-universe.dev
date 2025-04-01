update.scaledTrellis <- function(object,...)
tryCatch(
{
   dots <- list(...)
   if(!length(dots))
      signalCondition(simpleError(
      message = "No new arguments with which to update scaledTrellis object"))
   nmdots <- names(dots)
   if(length(nmdots) < length(dots))
      signalCondition(simpleError(
         message = "All arguments must be named"))
   ## update original trellis plot if needed
   add.nms <- intersect(c(names(formals(addScales.trellis)),names(formals(panel.addScales))),nmdots)
   trel.nms <- setdiff(nmdots, add.nms)
   obj.trellis<- revert(object) ## original trellis object
   if(!identical(trel.nms, character(0))){
      #update the trellis object
   obj.trellis <- do.call(update, ## update.trellis
                  c(list(object = obj.trellis),dots[trel.nms]))
   }
   ocall <- object$addScales$args
   if(identical(add.nms,character(0))) {arglist <- ocall}
   else arglist <- c(dots[add.nms],ocall)[unique(c(add.nms, names(ocall)))]
   out <- do.call(addScales,c(list(obj = obj.trellis),arglist))
   if(is.null(out)) object else out
   }, error = function(e){
      message(e)
      return(object)
   })


