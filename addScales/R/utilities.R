## check and parse legend and scaleline arguments in addScales call
parseInputs<- function(val )
{
   ## parse scaleline/legend inputs to addScales()
   hv <- c("h","v")
   val <-
      if(isTRUE(val)){list(h= TRUE, v = TRUE)}
   else if(isFALSE(val)) {list(h = FALSE, v = FALSE)}
   else if(!is.list(val))stop("Argument must be a list or logical")
   else {
      if((length(val) > 2) || !length(val))
         stop("List must have 1 <= length <= 2")
      else if(is.null(names(val))){
         if(length(val) == 2){names(val) <- hv; val}
         else stop("A one component list must be named")
      }
      else{
         nm <- names(val) <- tolower(names(val))
         ok <- nm %in% hv
         if(any(!ok))stop("List names must be in c('h','v')")
         else if(length(val) == 2){
            if(!anyDuplicated(nm))val ## correctly formatted list
            else stop("Duplicate names in arg list not permitted")
         }
         else { ## one of h or v
            val <- c(val,FALSE)
            names(val) <- c(nm,hv[nm != hv])
            val
         }
      }
   }
   if(names(val)[1] == "v") val <- rev(val)
   val
}


#########################################################
## grid code to create legend from grob
buildLegend<- function(legend, ## list of 2 textGrobs with names c("h","v"),
                       lloc= c("top","bottom","right","left"),
                       spacing = 1.3 ## to add spacing to legend text: bigger = more space
               )
{
## pack the textGrobs into a frame
lloc <- match.arg(lloc)
heights <- lapply(legend, grobHeight)
widths <- lapply(legend, grobWidth)
config <- lloc %in% c("top","bottom")
## Now create and pack a properly sized and dimensioned frameGrob
if(config){
   layout <- grid.layout(
      nrow = 1, ncol =2,
      heights = heights[["h"]][1],
      widths = spacing*do.call(unit.c,widths)
   )} else {
      layout <- grid.layout(
         nrow = 2, ncol = 1,
         heights = spacing*do.call(unit.c, heights),
         widths = widths[["h"]][1]
      )}
fg <- frameGrob(layout = layout)
## Now pack!
fg <-packGrob(fg, legend[["h"]], row =1, col =1)
if(config){
   packGrob(fg, legend[["v"]], row = 1, col = 2 )}
else packGrob(fg, legend[["v"]], row =2, col =1)
}












