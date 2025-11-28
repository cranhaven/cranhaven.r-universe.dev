################################################################################
################################################################################
################################################################################
################################################################################
#  function equivalent to GAIC.scaled 
################################################################################
################################################################################
################################################################################
################################################################################ 
model_GAIC<- function(object,..., 
                        k = 2,      # which GAIC
                        c = FALSE,  # whether corrected GAIC
                     plot = TRUE,   # whether plot or print
                    which = 1,      # for matrix which GAIC column
                 diff.dev = 1000,   # only applies to matrix
             text.to.show = NULL,   # whether the labels should be different
                      col = "rosybrown",  # colour for the bars
                    width = 0.9,  # widrh of the bar 
                    horiz = TRUE,
                    scale = c("[0,1]","[max,min]"),
                        title)   
{
#----------------------------------------------------------------------
# structure 
# if a matrix 
#            plot or (plot=FALSE) print 
#  if a list plot or (plot=FALSE) print
# else is one one print AIC
#----------------------------------------------------------------------  
  scale <- match.arg(scale)
  models <- delta <- scaled <- NULL  
if (is.matrix(object))# if a matrix------------------------------------  
 {
    fnas <- !is.na(object[,which])# choose which column
 #     dm <- dim(object)
# object
     GAIC <- object[fnas,which]# get the right without NA's
     wmin <- which.min(GAIC)
    DGAIC <- GAIC-GAIC[wmin]# unfortunately the range can atrocious because of very large AIC's  
     GAIC <- GAIC[DGAIC < diff.dev]
     dAIC <- max(GAIC, na.rm = TRUE)-GAIC #(AIC-AIC[which.min(AIC)]) 
     oAIC <- dAIC/(max(GAIC, na.rm = TRUE)-min(GAIC,  na.rm = TRUE))
      val <- data.frame(cbind(GAIC, delta=round(dAIC,3), scaled=round(oAIC,4)))
  txt.title <- if (missing(title)) paste("GAIC's ordered from",scale) else title   
if  (plot) # if we need to plot
{
  val$models <- rownames(val)
   gg <- switch(scale,
                "[0,1]"={
                  ggplot2::ggplot(val, ggplot2::aes(models, scaled))+
                    ggplot2::geom_bar(stat="identity", width=width, colour=col)+
                    ggplot2::ggtitle(txt.title)},
                "[max,min]"={ 
                  ggplot2::ggplot(val, ggplot2::aes(models, delta))+
                    ggplot2::geom_bar(stat="identity", width=width, colour=col)+
                    ggplot2::ggtitle(txt.title)})
  if (horiz) gg <- gg + ggplot2::coord_flip()         
  return(gg)
} else
  val <- val[,]
return(val)
 }
 # end if a matrix ----------- -----------------------------------
############  if model ----------------------------------------------
# if a list plot
if (length(list(...))) # if a list of model 
 {
      object <- list(object, ...)
    isgamlss <- unlist(lapply(object, is.gamlss))
  if (!any(isgamlss)) stop("some of the objects are not gamlss")
          df <- as.numeric(lapply(object, function(x) x$df.fit))
           N <- as.numeric(lapply(object, function(x) x$N))
         Cor <- if ((k == 2)&&(c==TRUE)) (2*df*(df+1))/(N-df-1) else rep(0, length(object)) 
         AIC <- as.numeric(lapply(object, function(x) x$G.dev+x$df.fit*k ))+Cor  
        dAIC <- max(AIC, na.rm = TRUE)-AIC #(AIC-AIC[which.min(AIC)]) 
        oAIC <- dAIC/(max(AIC, na.rm = TRUE)-min(AIC,  na.rm = TRUE))
         val <- as.data.frame(cbind(df,AIC, delta=round(dAIC,3), scaled=round(oAIC,4)))
        Call <- match.call()## trying to get the names
      Call$k <- Call$c <- Call$plot <- Call$text.cex <- Call$which <- Call$diff.dev <- Call$horiz <- Call$scale <- NULL 
row.names(val) <- if (is.null(text.to.show)) as.character(Call[-1][1:length(object)])
                 else text.to.show
#-----------------------------------------------
  if  (plot) # if we need to plot
      {
    val$models <- rownames(val)
           gg <- switch( scale,
                       "[0,1]"={
                         ggplot2::ggplot(val, ggplot2::aes(models, scaled))+
                           ggplot2::geom_bar(stat="identity", width=width, colour=col)+
                           ggplot2::ggtitle(paste("GAIC's in",scale, "scale"))},
                     "[max,min]"={ 
                       ggplot2::ggplot(val, ggplot2::aes(models, delta))+
                         ggplot2::geom_bar(stat="identity", width=width, colour=col)+
                         ggplot2::ggtitle(paste("GAIC's ordered from",scale))})
    if (horiz) gg <- gg +coord_flip()         
          return(gg)
      } else
      val <- val[,]
      return(val)
  }
else # if only one model just print
  { 
    val <- if (is.gamlss(object)) object$G.dev+object$df.fit*k 
           else stop(paste("this is not a gamlss object"))
  if ((k == 2)&&(c==TRUE)) val <- val + (2*object$df.fit*(object$df.fit+1))/(object$N-object$df.fit-1) 
  val 
  }
}
# end of model_GAIC
################################################################################
################################################################################
################################################################################
################################################################################
