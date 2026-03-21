#' Method plot for ViSigrid object. This method provides a graphic
#' of raw data during experimental observations of the realization of a procedure
#' like a medical algorithm. It graphically presents an overview of individuals
#' and group actions usually acquired from timestamps during video recorded sessions.
#' @title Method \code{plot-ViSigrid}
#' @name plot-ViSigrid-method
#' @rdname plot-ViSigrid-methods
#' @aliases plot,ViSigrid-method
#' @exportMethod plot
#' @docType methods
#' @param x A \code{ViSigrid} object built using the \code{\link{visielse}} function.
#' @param scal.unit.tps Unity of time for the grey grid legend.
#' @param unit.tps Unit of time (s,min,..).
#' @param Fontsize.label.Action Fontsize of labels of plotted actions.
#' @param Fontsize.label.Time Fontsize of the time axis.
#' @param Fontsize.label.color  Fontsize of legends.
#' @param Fontsize.title Fontsize of the title.
#' @param colgreenzone Color of the green zones.
#' @param colblackzone Color of black zones.
#' @param alphaZones Alpha of green and black zones.
#' @param linA Height of the plotting area in each actions lines, < 1.
#' @param alphainf Alpha of informers circles.
#' @param alphasup Alpha of supplementary times.
#' @param rcircle circle radius of informers circles.
#' @param lwdline line width of lines linking the 3 informers circles.
#' @param main Title.
#' @param size.main Title size.
#' @param col.main Title color.
#' @param col.grid Color of the legend box.
#' @param lwd.grid Lines width of the legend box.
#' @param lty.grid Lines type of the legend box.
#' @param ncharlabel Maximum number of plotted characters for labels of actions.
#' @param vp0h Height of the main plot window, <1.
#' @param vp0w Width  of the main plot window, <1.
#' @seealso \code{\linkS4class{ViSigrid}}, \code{\linkS4class{ViSibook}},
#'  \code{\link{visielse}}.
setMethod( f = "plot",
           signature = "ViSigrid",
           definition = function(x , scal.unit.tps = 10 , unit.tps = "s" , main = " " ,
                                 ncharlabel=30 , size.main = 12 ,  Fontsize.title = 11 ,
                                 Fontsize.label.Action = 11, Fontsize.label.Time = 11 , Fontsize.label.color = 9,
                                 col.main = "black" , col.grid = "grey" , colgreenzone = "green" , colblackzone = "black" ,
                                 alphainf = 0.8 , alphasup = 1, alphaZones = 0.2  ,
                                 vp0h	= 0.6, vp0w = 0.6, linA = 0.7 , rcircle = 15 , lwdline = 2 , lwd.grid = 1 , lty.grid = 1
                               ) {
             book	<- methods::slot( x , "book" )
             sortindex <- sort( book[ ,4]  , index.return = TRUE)$ix
             if ( any( is.na( book[ , 4] )  ) ) {
               for (i in seq( 1 , sum( is.na( book[ , 4]  ) ), 1) ) {  # i =2
                 sortindex <- mapply( FUN = function(x , y )(return( if (y >= x ) { return( y + 1) }else{return( y ) } ) ) ,
                                      x = which( is.na( book[ , 4] )  )[ i ] ,
                                      y = sortindex )
               }
             }
             ################################################
             # Definition of constants
             inftps 	<- max( methods::slot( x , "vect_tps" ) )
             lgv	 	<- length( sortindex )  # Number of actions
             lgH	 	<- length( methods::slot( x , "vect_tps" ) ) - 1# Number of times
             newx	<- sapply( 	c( -0.06 , 0 , 0.06 ) , function(x ) { x * cos( seq( -pi , pi , 2 * pi / 8 ) ) }
             ) - sapply(	c( 0 , 0.3 , 0 ) , function(x ) { -x * sin( seq( -pi , pi , 2 * pi / 8 ) ) } ) + 0.5
             newy    <- sapply(	c( -0.06 , 0 , 0.06 ) , function(x ) { x * sin( seq( -pi , pi , 2 * pi / 8 ) ) }
             ) - sapply( c( 0 , 0.3 , 0 ) , function(x ) { x * cos( seq( -pi , pi , 2 * pi / 8 ) ) } ) + 0.5
             # Defintion of viewport and layout
             vp0  <- grid::viewport( 	x = grid::unit( (1 - vp0w ) * 2/3 , "npc" ) ,
                                   y = grid::unit( (1 - vp0h ) * 2/3 , "npc" ) ,
                                   width = vp0w ,
                                   height = vp0h ,
                                   just = c( 0 , 0 ) ,
                                   name = "vp0" ) # cadre
             layoutAction 	<- grid::viewport( layout = grid::grid.layout( lgv , 1 , widths = 1 , heights = 1 ) , name = "layoutAction" )
             vplayoutA <- lapply(  sortindex , function(x )(
               grid::viewport( layout.pos.row = which( sortindex == x ) , layout.pos.col = 1 , name = paste0( "vp" , methods::slot( book , "vars")[ x ] ) ) ))
             names(vplayoutA) = paste0( rep("vp" , lgv ) , methods::slot( book , "vars")[ seq( 1 , lgv , 1 ) ] )
             # PLoting ................................................................................................................................................
             grid::grid.newpage()
              grid::pushViewport( vp0 )
              grid::pushViewport( layoutAction )
             for (ia in sortindex ) { #		ia=2
               ### Enter into the action lines
                grid::pushViewport( vplayoutA[[ which( sortindex == ia ) ]] )
               # Punctuals treatment __________________________________________________________________________________________________________________________________________________________________
               if (methods::slot( book , "typeA")[  ia  ] == "p" ) {
                 # GREEN & BLACK ZONES
                 plotgreenzoneP(book,ia,inftps,colgreenzone,alphaZones)
                 plotblackzoneP(book, ia, inftps, colblackzone, alphaZones)
                 # Plot the punction action
                 plotpunctual( 	mat = methods::slot( x , "MATp" ) ,
                                iip = which( sortindex[ which( methods::slot( book , "typeA" )[sortindex] == "p" )] == ia ),
                                book = book, colvect = methods::slot( x , "colvect" ) ,
                                lgH = lgH ,
                                method = methods::slot( x , "parameters")$method ,
                                linA = linA )
                 plotInformersTests(x,book, inftps, ia, alphainf, lwdline, rcircle, linA ,newx, newy)
                 # Supplementary times points
                 if (length( methods::slot( x , "MATpsup" ) ) > 0 ) {
                   plotpunctualsup(X = methods::slot( x , "MATpsup" ) ,
                                   idsup = methods::slot( x , "idsup" ) ,
                                   iip = which( sortindex[ which( methods::slot( book , "typeA" )[sortindex] == "p" )] == ia ) ,
                                   book = book,
                                   method = methods::slot( x , "parameters")$method ,
                                   linA = linA ,
                                   lgH = lgH ,
                                   colvect = methods::slot( x , "colvect" ) ,
                                   alphasup = alphasup)
                 }
                }else{ ## Long Actions
                iipp = which(methods::slot( book , "vars")[order(book[  , 4 ] )][which(methods::slot( book , "typeA")[order(book[  , 4 ] )] == "l")] == methods::slot( book , "vars" )[ia] )
                 plotL(	L = methods::slot( x , "L" )[ , c( 2 * sum( methods::slot( book , "typeA" )[sortindex][ seq( 1 ,  which( sortindex == ia ) , 1) ] == "l" ) - 1 ,
                                                  2 * sum( methods::slot( book , "typeA" )[sortindex][ seq( 1 ,  which( sortindex == ia ) , 1) ] == "l" ) ) ] ,
                        idsort = methods::slot( x , "idsort" )[ ,iipp ] ,
                        inftps = inftps ,
                        group =  methods::slot( x , "group" ) ,
                        BZL = methods::slot( x , "BZL" ) ,
                        Lsup = methods::slot( x , "Lsup" ) ,
                        idsup = methods::slot( x , "idsup" ) ,
                        iip = iipp,
                        t_0 = methods::slot( x , "parameters")$t_0,
                        cols = methods::slot( x , "colvect" ) ,
                        linA = linA ,
                        alphaZones = alphaZones ,
                        alphasup = alphasup ,
                        colblackzone = colblackzone)
                 if (is.null( methods::slot( x , "parameters" )$informer ) == FALSE ) {
                   plot_long_informer(x, book, sortindex, iipp, ia, inftps,lwdline,linA,newx, newy)
                 }
               }
                grid::upViewport() # Out of the line of action
             }
              grid::upViewport() # Out of the line of action
              grid::upViewport() # Out of the cadre
             #################################################### Legends ###################################################
             #Legend Y axis + title
              legend_action( book = book ,main = main ,  size.main = size.main ,  Fontsize.title = Fontsize.title ,
                                             Fontsize.label.Action = Fontsize.label.Action,
                                             col.main = col.main, ncharlabel=ncharlabel,
                                             vp0h	= vp0h, vp0w = vp0w,layoutAction=layoutAction,sortindex=sortindex,
                              vplayoutA=vplayoutA)
             #___________________________________________________________________________________________________________________
             # Grid times
              legendgridtimes( vp0, vp0w,vp0h , x , lgv, inftps, scal.unit.tps,col.grid,lwd.grid,lty.grid,Fontsize.label.Time,unit.tps )
             #________________________________________________________________________________________________________________
             #___________________________________________________________________________________________________________________
             ####Legend bottom
             ###  &  any( methods::slot( book , "typeA" )[ sortindex ] == "l")
              legendbottom( x, sortindex, book,vp0w, vp0h, Fontsize.label.color,colblackzone,alphaZones,colgreenzone)
}
)
