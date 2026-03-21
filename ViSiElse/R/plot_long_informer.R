plot_long_informer <- function(x, book, sortindex, iipp, ia, inftps,lwdline,linA,newx, newy){
  iip  <- 	sum(methods::slot( book , "typeA") == "p") + sum(methods::slot( book , "typeA")[ sortindex ] == "l" ) + iipp
  iip2 <-  which( colnames(methods::slot( x , "informers")) == methods::slot( book , "deb")[ ia ] )
  if (any(is.na(c(methods::slot( x , "informers" )[ , iip ] , methods::slot( x , "informers" )[ , iip2 ]))) == FALSE ) {
    if ( is.character( methods::slot( x , "parameters")$t_0)==FALSE & methods::slot( x , "parameters")$t_0 != 0 ){
      methods::slot( x , "informers" )[ , iip ] <- methods::slot( x , "informers" )[ , iip ] - rep(methods::slot( x , "parameters")$t_0, length(methods::slot( x , "informers" )[ , iip ]))
      methods::slot( x , "informers" )[ , iip2 ] <- methods::slot( x , "informers" )[ , iip2 ] - rep(methods::slot( x , "parameters")$t_0, length(methods::slot( x , "informers" )[ , iip2 ]))
    }
    if (any( c( "global",  "join") == methods::slot( x , "parameters")$method ) ) {
      grid::pushViewport( viewport( x = grid::unit(  methods::slot( x , "informers" )[ 2 , iip2 ] / inftps , "npc" ) ,
                                    y = grid::unit( 0 , "npc" ) ,
                                    width = grid::unit(  (methods::slot( x , "informers" )[ 2 , iip ] -  methods::slot( x , "informers" )[ 2 , iip2 ] ) / inftps , "npc" ) ,
                                    height = grid::unit( 1 , "npc" ) ,
                                    just = c( 0 , 0)))
      grid::grid.lines( x = grid::unit( c( 0 , 1 ) , "npc" ) , y = grid::unit( c( 1/2 , 1/2 ) , "npc" ) , default.units = "npc" , arrow = NULL , gp = gpar( col = "black" , lwd = lwdline))
      grid::upViewport()
    }else{
      ### Plot group 1
      grid::pushViewport(grid::viewport( x = grid::unit( 0 , "npc" ) ,
                                         y = grid::unit( 1/2 , "npc" ) ,
                                         width = grid::unit( 1 , "npc" ) ,
                                         height = grid::unit( linA/2 , "npc" ) ,
                                         just = c( 0 , 0 )))
      grid::pushViewport(grid::viewport( x = grid::unit(  methods::slot( x , "informers" )[ 2 , iip2 ] / inftps , "npc" ) ,
                                         y = grid::unit( 0 , "npc" ) ,
                                         width = grid::unit( (methods::slot( x , "informers" )[ 2 , iip ] -  methods::slot( x , "informers" )[ 2 , iip2 ] ) / inftps , "npc" ) ,
                                         height = grid::unit( 1 , "npc" ) ,
                                         just = c( 0 , 0) ))
      grid::grid.lines( x = grid::unit( c( 0 , 1 ) , "npc" ) , y = grid::unit( c( 1/2 , 1/2 ) , "npc" ) , default.units = "npc" , arrow = NULL , gp = gpar( col = "black" , lwd = lwdline))
      grid::upViewport()
      grid::upViewport()
      ### Plot group 2
      grid::pushViewport(grid::viewport( x = grid::unit( 0 , "npc" ) ,
                                         y = grid::unit( (1 - linA ) / 2 , "npc" ) ,
                                         width = grid::unit( 1 , "npc" ) ,
                                         height = grid::unit( linA/2 , "npc" ) ,
                                         just = c( 0 , 0 )))
      grid::pushViewport(grid::viewport( x = grid::unit(  methods::slot( x , "informers" )[ 5 , iip2 ] / inftps , "npc" ) ,
                                         y = grid::unit( 0 , "npc" ) ,
                                         width = grid::unit(  (methods::slot( x , "informers" )[ 5 , iip ] -  methods::slot( x , "informers" )[ 5 , iip2 ] ) / inftps , "npc" ) ,
                                         height = grid::unit( 1 , "npc" ) ,
                                         just = c( 0 , 0)))
      grid::grid.lines( x = grid::unit( c( 0 , 1 ) , "npc" ) , y = grid::unit( c( 1/2 , 1/2 ) , "npc" ) , default.units = "npc" , arrow = NULL , gp = gpar( col = "black" , lwd = lwdline))
      grid::upViewport()
      grid::upViewport()
    }
    if ( any( c( "cut", "within" , "join") == methods::slot( x , "parameters")$method ) & methods::slot( x , "parameters" )$test == TRUE ) {
      if (methods::slot( x , "testsP" )[ sum( methods::slot( book , "typeA" ) == "p" ) + sum( methods::slot( book , "typeA" )[sortindex][ seq( 1 ,  which( sortindex == ia ) , 1) ] == "l" ) ] == TRUE ) {
        grid::pushViewport(grid::viewport( x = grid::unit( 1 , "npc" ) ,
                                           y = grid::unit( 0.3 , "npc" ) ,
                                           width = grid::unit( 0.035 , "npc" ) ,
                                           height = grid::unit(  0.4 , "npc" ) ,
                                           just = c( 0 , 0 ) ,
                                           clip = TRUE))
        grid::grid.polygon( x = t( newx ) ,
                            y = t( newy ) ,
                            id = NULL ,
                            id.lengths = rep( 3 , dim( newx )[ 1 ] ) ,
                            default.units = "npc" ,
                            gp = gpar( col = FALSE , fill = "black" ),
                            draw = TRUE,
                            vp = NULL)
        popViewport()
      }
    }
  }
}