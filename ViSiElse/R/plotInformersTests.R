plotInformersTests <- function(x,book, inftps, ia, alphainf, lwdline, rcircle, linA ,newx, newy){
  if (is.null( methods::slot( x , "parameters" )$informer ) == FALSE) {
    if (any( c( "global" , "join") == methods::slot( x , "parameters")$method ) ) {
      plotinformers( informers = methods::slot( x , "informers" ) ,
                     inftps = inftps ,
                     iip = which(methods::slot( book , "vars" )[ which( methods::slot( book , "typeA" ) == "p" ) ] == methods::slot( book , "vars" )[ ia ]),
                     t_0 = methods::slot( x , "parameters")$t_0,
                     alphainf = alphainf,
                     lwdline = lwdline ,
                     rcircle = rcircle  )
    }else{
      ### Plot group 1
      grid::pushViewport( grid::viewport( x = grid::unit( 0 , "npc" ) ,
                                          y = grid::unit( 1/2 , "npc" ) ,
                                          width = grid::unit( 1 , "npc" ) ,
                                          height = grid::unit( linA/2 , "npc" ) ,
                                          just = c( 0 , 0 )))
      plotinformers( informers = methods::slot( x , "informers" )[ c( 1 , 2 , 3 ) , ] ,
                     inftps = inftps ,
                     iip = which(methods::slot( book , "vars" )[ which( methods::slot( book , "typeA" ) == "p" ) ] == methods::slot( book , "vars" )[ ia ]) ,
                     t_0 = methods::slot( x , "parameters")$t_0,
                     alphainf = alphainf,
                     lwdline = lwdline  ,
                     rcircle = rcircle  )
      grid::upViewport()
      ### Plot group 2
      grid::pushViewport( grid::viewport( x = grid::unit( 0 , "npc" ) ,
                                          y = grid::unit( (1 - linA ) / 2 , "npc" ) ,
                                          width = grid::unit( 1 , "npc" ) ,
                                          height = grid::unit( linA/2 , "npc" ) ,
                                          just = c( 0 , 0 )))
      plotinformers( informers = methods::slot( x , "informers" )[ c( 4 , 5 ,6 ) , ] ,
                     inftps = inftps ,
                     iip = which(methods::slot( book , "vars" )[ which( methods::slot( book , "typeA" ) == "p" ) ] == methods::slot( book , "vars" )[ ia ]) ,
                     t_0 = methods::slot( x , "parameters")$t_0,
                     alphainf = alphainf,
                     lwdline = lwdline  ,
                     rcircle = rcircle  )
      grid::upViewport()
    }
    # Test stars
    if ( any( c( "cut", "within" , "join") == methods::slot( x , "parameters")$method ) & methods::slot( x , "parameters" )$test == TRUE ) {
      if (methods::slot( x , "testsP" )[ which(methods::slot( book , "vars" )[ which( methods::slot( book , "typeA" ) == "p" ) ] == methods::slot( book , "vars" )[ ia ]) ] == TRUE ) {
        grid::pushViewport( grid::viewport( x = grid::unit( 1 , "npc" ) ,
                                            y = grid::unit( 0.3 , "npc" ) ,
                                            width = grid::unit( 0.035 , "npc" ) ,
                                            height = grid::unit(  0.4 , "npc" ) ,
                                            just = c( 0 , 0 ) ,
                                            clip = TRUE))
        grid::grid.polygon(	x = t( newx ) ,
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
