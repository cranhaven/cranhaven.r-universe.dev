legendbottom <- function( x, sortindex, book,vp0w, vp0h, Fontsize.label.color,colblackzone,alphaZones,colgreenzone){
lcol <- switch( as.character( length( methods::slot( x , "colvect" )[ 1 , ] ) == 1), "TRUE" = 1 ,"FALSE" = length( methods::slot( x , "colvect" )[ 1 , ] ) %/% 2)
if (any( methods::slot( book , "typeA" )[ sortindex ] == "p" )) {
  part <- switch( 	as.character( any( c( length( methods::slot( book , "BZBeforeDeb" ) ) , length( methods::slot( book , "BZAfterDeb" ) ) ) > 0 ) ) , "TRUE" = 1 , "FALSE" = 0 ) + switch( as.character( length( methods::slot( book , "GZDeb" ) ) > 0 ) , "TRUE" = 1 , "FALSE" = 0 ) + switch( as.character( is.null(methods::slot( x , "parameters" )$informer )) ,"FALSE" = 1 , "TRUE" = 0 )
  if (part == 0 ) {
    grid::pushViewport(grid::viewport( y = grid::unit( 0.005 , "npc"),
                                       x = grid::unit( 0.04 , "npc" ) ,
                                       height = grid::unit( (1 - vp0h ) * 1 / 3 + 0.02, "npc" ) ,
                                       width = grid::unit( 0.9 , "npc" ) ,
                                       just = c( 0 , 0 )))
    grid::grid.rect( gp = gpar( col = "black" , fill = "grey",alpha = 0.3 ))
    grid::upViewport()
    grid::pushViewport(grid::viewport( y = grid::unit( 0.02 , "npc") ,
                                       x = grid::unit( 0.05 , "npc" ) ,
                                       height = grid::unit( (1 - vp0h ) * 1 / 3 , "npc" ) ,
                                       width = grid::unit( 0.4 , "npc" ),
                                       just = c( 0 , 0 ) ) )
    grid::pushViewport(grid::viewport( x = grid::unit( 0 , "npc" ) ,
                                       y = grid::unit( 5 / 6 , "npc" ) ,
                                       width = grid::unit( 1 , "npc" ) ,
                                       height = grid::unit( 1/6 , "npc" ) ,
                                       just = c( 0 , 0 ) ) )
    grid::grid.text( x = grid::unit( 0 , "npc" ) , y = grid::unit( 1, "npc" ), "Punctual Action", gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1 ) )
    grid::upViewport()
    grid::pushViewport(grid::viewport( x = grid::unit( 0 , "npc" ) ,
                                       y = grid::unit( 0 , "npc" ) ,
                                       width = grid::unit( 1 , "npc" ) ,
                                       height = grid::unit( 5 / 6 , "npc" ) ,
                                       just = c( 0 , 0 ) ) )
    if ( methods::slot( x , "parameters" )$method == "global" ) {
      grid::pushViewport(grid::viewport(	x = grid::unit( 0 , "npc" ) ,
                                         y = grid::unit( 8/12 , "npc" ) ,
                                         width = grid::unit( 1/3 , "npc" ) ,
                                         height = grid::unit( 1/3 , "npc" ) ,
                                         just = c( 0 , 0 ) ) )
      grid::grid.text(	x = grid::unit( 0 , "npc" ) , y = grid::unit( 1/2  , "npc" ) , paste( switch( methods::slot( x , "parameters" )$quantity , "N" = "N : 1 ->" , "dens" = " % : 1 -> ") , length(   methods::slot( x , "colvect")[ 1 , ] ) ) , gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1/2 ) )
      grid::upViewport()
      grid::pushViewport(grid::viewport( x = grid::unit( 7/12 , "npc" ) ,
                                         y = grid::unit( 9/12 , "npc" ) ,
                                         width = grid::unit( 1/3 , "npc" ) ,
                                         height = grid::unit( 2/12 , "npc" ) ,
                                         just = c( 0 , 0 )))
      grid::pushViewport(grid::viewport( layout = grid.layout( 1 , length(   methods::slot( x , "colvect")[ 1 , ] ) , widths = 1 , heights = 1 ) ) )
      for (ii in seq( 1 , length( methods::slot( x , "colvect")[ 1 , ] ) , 1 ) ) {
        grid::pushViewport(grid::viewport( layout.pos.col = ii , layout.pos.row = 1 ) )
        grid::grid.rect( gp = gpar( col = FALSE , fill = methods::slot( x , "colvect")[ 1 , ii ] ) )
        grid::upViewport()
      }
      grid::upViewport()
      grid::upViewport()
    }else{
      grid::pushViewport(grid::viewport(	x = grid::unit( 0 , "npc" ) ,
                                         y = grid::unit(8/12 , "npc" ) ,
                                         width = grid::unit( 1/3 , "npc" ) ,
                                         height = grid::unit( 1/3 , "npc" ) ,
                                         just = c( 0 , 0 )  )  )
      grid::grid.text( x = grid::unit( 0 , "npc" ) , y = grid::unit( 1 / 2 , "npc" ) , paste( switch( methods::slot( x , "parameters" )$quantity , "N" = "N : 1 ->" , "dens" = " % : 1 -> ") , length(   methods::slot( x , "colvect")[ 1 , ] ) ) , gp = gpar( col = "black" , fontsize = Fontsize.label.color ) ,  just = c( 0 , 1/2 ) )
      grid::upViewport()
      grid::pushViewport(grid::viewport( x = grid::unit( 0 , "npc" ) ,
                                         y = grid::unit(4/12 , "npc" ) ,
                                         width = grid::unit( 1/3 , "npc" ) ,
                                         height = grid::unit( 1/3 , "npc" ),
                                         just = c( 0 , 0 )))
      grid::grid.text( 	x = grid::unit( 0 , "npc" ) , y = grid::unit( 1 / 2 , "npc" ) ,  paste( "  Group" , levels( methods::slot( x , "group" ) )[ 1 ] ) , gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1/2 ) )
      grid::upViewport()
      grid::pushViewport(grid::viewport( x = grid::unit( 0 , "npc" ) ,
                                         y = grid::unit(0/12 , "npc" ) ,
                                         width = grid::unit( 1/3 , "npc" ) ,
                                         height = grid::unit( 1/3 , "npc" ) ,
                                         just = c( 0 , 0 )))
      grid::grid.text( x = grid::unit( 0 , "npc" ) , y = grid::unit( 1 / 2 , "npc" ) ,  paste( "  Group" , levels( methods::slot( x , "group" ) )[ 2 ] ) , gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1/2 )  )
      grid::upViewport()
      grid::pushViewport(grid::viewport( x = grid::unit( 7/12 , "npc" ) ,
                                         y = grid::unit( 5/12 , "npc" ) ,
                                         width = grid::unit( 1/3 , "npc" ) ,
                                         height = grid::unit( 2/12 , "npc" ) ,
                                         just = c( 0 , 0 )))
      grid::pushViewport(grid::viewport( layout = grid.layout( 1 , length(   methods::slot( x , "colvect")[ 1 , ] ) , widths = 1 , heights = 1 ) ) )
      for (ii in seq( 1 , length(   methods::slot( x , "colvect")[ 1 , ] ) , 1 ) ) {
        grid::pushViewport(grid::viewport( layout.pos.col = ii , layout.pos.row = 1 ) )
        grid::grid.rect( gp = gpar( col = FALSE , fill = methods::slot( x , "colvect")[ 1 , ii ] ) )
        grid::upViewport()
      }
      grid::upViewport()
      grid::upViewport()
      grid::pushViewport(grid::viewport( x = grid::unit( 7/12 , "npc" ) ,
                                         y = grid::unit( 1/12 , "npc" ) ,
                                         width = grid::unit( 1/3 , "npc" ) ,
                                         height = grid::unit( 2/12 , "npc" ) ,
                                         just = c( 0 , 0 )))
      grid::pushViewport(grid::viewport( layout = grid.layout( 1 , length(   methods::slot( x , "colvect")[ 1 , ] ) , widths = 1 , heights = 1 ) ) )
      for (ii in seq( 1 , length(   methods::slot( x , "colvect")[ 1 , ] ) , 1 ) ) {
        grid::pushViewport(grid::viewport( layout.pos.col = ii , layout.pos.row = 1 ) )
        grid::grid.rect( gp = gpar( col = FALSE , fill = methods::slot( x , "colvect")[ 2 , ii ] ) )
        grid::upViewport()
      }
      grid::upViewport()
      grid::upViewport()
    }
    grid::upViewport()
    grid::upViewport()
  }else{
    part = 3
    grid::pushViewport(grid::viewport( y = grid::unit( 0.005 , "npc"),
                                       x = grid::unit( 0.04 , "npc" ) ,
                                       height = grid::unit( (1 - vp0h ) * 1 / 3 + 0.02, "npc" ) ,
                                       width = grid::unit( 0.9 , "npc" ) ,
                                       just = c( 0 , 0 )))
    grid::grid.rect( gp = gpar( col = "black" , fill = "grey",alpha = 0.3 ))
    grid::upViewport()
    grid::pushViewport(grid::viewport( y = grid::unit( 0.02 , "npc") , x = grid::unit( 0.05 , "npc" ) ,  height = grid::unit( (1 - vp0h ) * 1 / 3 , "npc" ) ,  width = grid::unit( 0.4 , "npc" ) , just = c( 0 , 0 ) ) )
    grid::pushViewport(grid::viewport( x = grid::unit( 0 , "npc" ) , y = grid::unit( 5 / 6 , "npc" ) , width = grid::unit( 1 , "npc" ) , height = grid::unit( 1/6 , "npc" ) , just = c( 0 , 0 ) ) )
    grid::grid.text( 	x = grid::unit( 0 , "npc" ) , y = grid::unit( 1, "npc" ), "Punctual Action", gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1 ) )
    grid::upViewport()
    grid::pushViewport(grid::viewport( x = grid::unit( 0 , "npc" ) , y = grid::unit( 0 , "npc" ) , width = grid::unit( 1 , "npc" ) , height = grid::unit( 5 / 6 , "npc" ) , just = c( 0 , 0 ) ) )
    grid::pushViewport(grid::viewport( layout = grid.layout( 1 , 2 ) , width = 1 , height = 1 ) )
    grid::pushViewport(grid::viewport( layout.pos.col = 2 , layout.pos.row = 1 ) )
    if (length( methods::slot( book , "GZDeb" ) ) > 0 ) {
      grid::pushViewport(grid::viewport( x = grid::unit( 0 , "npc" ) , y = grid::unit( (part - 1 ) / part , "npc" ) , width = grid::unit( 1 , "npc" ) , height = grid::unit( 1 / part , "npc" ) , just = c( 0 , 0 ) ) )
      grid::grid.text(  x = grid::unit( 0 , "npc" ) , y = grid::unit( 0.5 , "npc" ) , "Green Zone" , gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1 / 2 ))
      grid::grid.circle( 	x = grid::unit( 5 / 6 , "npc" ) ,   y = grid::unit( 0.4 , "npc" ) , r = grid::unit( 0.4 , "npc" ) , gp = gpar( col = FALSE , fill = colgreenzone , alpha = alphaZones ) )
      grid::upViewport()
    }
    if (any( c( length( methods::slot( book , "BZBeforeDeb" ) ) , length( methods::slot( book , "BZAfterDeb" ) ) ) > 0 ) ) {
      grid::pushViewport(grid::viewport( x = grid::unit( 0 , "npc" ) , y = grid::unit( (part - 1 - switch( as.character(length( methods::slot( book , "GZDeb" ) ) > 0 ) , "TRUE" = 1 , "FALSE" = 0 ) ) / part , "npc" ) , width = grid::unit( 1 , "npc" ) , height = grid::unit( 1 / part , "npc" ) , just = c( 0 , 0 )  ) )
      grid::grid.text( 	x = grid::unit( 0 , "npc" ) , y = grid::unit( 0.5 , "npc" ) , "Black Zone" , gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1 / 2 ) )
      grid::grid.circle( 	x = grid::unit( 5 / 6 , "npc" ) , y = grid::unit( 0.4 , "npc" ) , r = grid::unit( 0.4 , "npc" ) , gp = gpar( col = FALSE , fill = colblackzone , alpha = alphaZones ) )
      grid::upViewport()
    }
    if (is.null( methods::slot( x , "parameters" )$informer ) == FALSE ) {
      grid::pushViewport(grid::viewport( x = grid::unit( 0 , "npc" ) , y = grid::unit( (part - 1 -
                                                                                          switch( 	as.character( any( c( length( methods::slot( book , "BZBeforeDeb" ) ) ,
                                                                                                                         length( methods::slot( book , "BZAfterDeb" ) ) ) > 0 ) ) ,
                                                                                                   "TRUE" = 1 , "FALSE" = 0 ) - switch( as.character(length( methods::slot( book , "GZDeb" ) ) > 0 ) ,
                                                                                                                                        "TRUE" = 1 ,"FALSE" = 0 ) ) / part , "npc" ) ,
                                         width = grid::unit( 1 , "npc" ) , height = grid::unit( 1 / part , "npc" ) , just = c( 0 , 0 )  ) )
      grid::grid.text(	x = grid::unit( 0 , "npc" ) , y = grid::unit( 0.5 , "npc" ) , paste( switch( methods::slot( x , "parameters" )$informer , "mean" = "Mean +/- sd" , "median" = "Median Q1-Q3" ) ) ,  gp = gpar( col = "Black" , fontsize = Fontsize.label.color ) ,  just = c( 0 , 1/2 ) )
      grid::grid.lines( x = grid::unit( c( 4/6 , 6/6 ) , "npc" ) , y = grid::unit( c( 0.4 , 0.4 ) , "npc" ), default.units = "npc" ,  arrow = NULL ,   gp = gpar( col = "black" , lwd = 1 ) )
      grid::grid.circle( x = grid::unit( 4 / 6 , "npc" ) , y = grid::unit( 0.4 , "npc" ) , r = grid::unit( 0.2 , "npc" ) , gp = gpar( col = "black" , fill = "white" ) )
      grid::grid.circle( x = grid::unit( 5 / 6 , "npc" ) , y = grid::unit( 0.4 , "npc" ) , r = grid::unit( 0.2 , "npc" ) , gp = gpar( col = "black" , fill = "white" ) )
      grid::grid.circle( x = grid::unit( 6 / 6 , "npc" ) , y = grid::unit( 0.4 , "npc" ) , r = grid::unit( 0.2 , "npc" ) , gp = gpar( col = "black" , fill = "white" ) )
      grid::upViewport()
    }
    grid::upViewport()
    grid::pushViewport(grid::viewport( layout.pos.col = 1 , layout.pos.row = 1 ))
    if ( methods::slot( x , "parameters" )$method == "global" ) {
      grid::pushViewport(grid::viewport(	x = grid::unit( 0 , "npc" ) ,  y = grid::unit( 8/12 , "npc" ) , width = grid::unit( 1/3 , "npc" ) ,height = grid::unit( 1/3 , "npc" ) , just = c( 0 , 0 ) ) )
      grid::grid.text( 	x = grid::unit( 0 , "npc" ) , y = grid::unit( 1/2  , "npc" ) , paste( switch( methods::slot( x , "parameters" )$quantity , "N" = "N : 1 ->" , "dens" = " % : 1 -> ") , length(   methods::slot( x , "colvect")[ 1 , ] ) ) , gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1/2 ) )
      grid::upViewport()
      grid::pushViewport(grid::viewport( x = grid::unit( 7/12 , "npc" ) ,  y = grid::unit( 9/12 , "npc" ) , width = grid::unit( 1/3 , "npc" ) , height = grid::unit( 2/12 , "npc" ) ,  just = c( 0 , 0 )  )  )
      grid::pushViewport(grid::viewport( layout = grid.layout( 1 , length(   methods::slot( x , "colvect")[ 1 , ] ) , widths = 1 , heights = 1 ) ) )
      for (ii in seq( 1 , length(   methods::slot( x , "colvect")[ 1 , ] ) , 1 )) {
        grid::pushViewport(grid::viewport( layout.pos.col = ii , layout.pos.row = 1 ) )
        grid::grid.rect( gp = gpar( col = FALSE , fill = methods::slot( x , "colvect")[ 1 , ii ] ) )
        grid::upViewport()
      }
      grid::upViewport()
      grid::upViewport()
    }else{
      grid::pushViewport(grid::viewport(	x = grid::unit( 0 , "npc" ) ,  y = grid::unit(8/12 , "npc" ) , width = grid::unit( 1/3 , "npc" ) , height = grid::unit( 1/3 , "npc" ) ,just = c( 0 , 0 )  )  )
      grid::grid.text( 	x = grid::unit( 0 , "npc" ) , y = grid::unit( 1 / 2 , "npc" ) , paste( switch( methods::slot( x , "parameters" )$quantity , "N" = "N : 1 ->" , "dens" = " % : 1 -> ") , length(   methods::slot( x , "colvect")[ 1 , ] ) ) , gp = gpar( col = "black" , fontsize = Fontsize.label.color ) ,  just = c( 0 , 1/2 ) )
      grid::upViewport()
      grid::pushViewport(grid::viewport(	x = grid::unit( 0 , "npc" ) , y = grid::unit(4/12 , "npc" ) , width = grid::unit( 1/3 , "npc" ) , height = grid::unit( 1/3 , "npc" ) , just = c( 0 , 0 ) ) )
      grid::grid.text( 	x = grid::unit( 0 , "npc" ) , y = grid::unit( 1 / 2 , "npc" ) ,  paste( "  Group" , levels( methods::slot( x , "group" ) )[ 1 ] ) , gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1/2 ) )
      grid::upViewport()
      grid::pushViewport(grid::viewport(	x = grid::unit( 0 , "npc" ) ,  y = grid::unit(0/12 , "npc" ) , width = grid::unit( 1/3 , "npc" ) , height = grid::unit( 1/3 , "npc" ) ,  just = c( 0 , 0 ) )  )
      grid::grid.text( 	x = grid::unit( 0 , "npc" ) , y = grid::unit( 1 / 2 , "npc" ) ,  paste( "  Group" , levels( methods::slot( x , "group" ) )[ 2 ] ) , gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1/2 )  )
      grid::upViewport()
      grid::pushViewport(grid::viewport( x = grid::unit( 7/12 , "npc" ) , y = grid::unit( 5/12 , "npc" ) ,  width = grid::unit( 1/3 , "npc" ) , height = grid::unit( 2/12 , "npc" ) , just = c( 0 , 0 )  ) )
      grid::pushViewport(grid::viewport( layout = grid.layout( 1 , length(   methods::slot( x , "colvect")[ 1 , ] ) , widths = 1 , heights = 1 ) ) )
      for (ii in seq( 1 , length(   methods::slot( x , "colvect")[ 1 , ] ) , 1 ) ) {
        grid::pushViewport(grid::viewport( layout.pos.col = ii , layout.pos.row = 1 ) )
        grid::grid.rect( gp = gpar( col = FALSE , fill = methods::slot( x , "colvect")[ 1 , ii ] ))
        grid::upViewport()
      }
      grid::upViewport()
      grid::upViewport()
      grid::pushViewport( grid::viewport( x = grid::unit( 7/12 , "npc" ) ,  y = grid::unit( 1/12 , "npc" ) , width = grid::unit( 1/3 , "npc" ) ,  height = grid::unit( 2/12 , "npc" ) , just = c( 0 , 0 ) ) )
      grid::pushViewport( grid::viewport( layout = grid.layout( 1 , length(   methods::slot( x , "colvect")[ 1 , ] ) , widths = 1 , heights = 1 ) ) )
      for (ii in seq( 1 , length(   methods::slot( x , "colvect")[ 1 , ] ) , 1 ) ) {
        grid::pushViewport( grid::viewport( layout.pos.col = ii , layout.pos.row = 1 ) )
        grid::grid.rect( gp = gpar( col = FALSE , fill = methods::slot( x , "colvect")[ 2 , ii ] ) )
        grid::upViewport()
      }
      grid::upViewport()
      grid::upViewport()
    }
    grid::upViewport()
    grid::upViewport()
    grid::upViewport()
    grid::upViewport()
  }
  ###### Long action..................................................................................................................................
  grid::pushViewport( grid::viewport( y = grid::unit( 0.02 , "npc") ,   x = grid::unit( 0.5 , "npc" ) ,  height = grid::unit( (1 - vp0h ) * 1/3 , "npc" ) , width = grid::unit( 0.4 , "npc" ) ,  just = c( 0 , 0 ) ) )
  ##########
  grid::pushViewport( grid::viewport( x = grid::unit( 0 , "npc" ) ,  y = grid::unit( 5/6 , "npc" ) ,  width = grid::unit( 1 , "npc" ) , height = grid::unit( 1/6 , "npc" ) ,  just = c( 0 , 0 )  ))
  grid::grid.text(  x = grid::unit( 0 , "npc" ) ,  y = grid::unit( 1 , "npc" ) ,  "Long Action" ,  gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1 )  )
  grid::upViewport()
  grid::pushViewport( grid::viewport( x = grid::unit( 0 , "npc" ) ,  y = grid::unit( 0 , "npc" ) ,  width = grid::unit( 1 , "npc" ) ,  height = grid::unit( 5/6 , "npc" ) , just = c( 0 , 0 )  )  )
  part = switch( methods::slot( x , "parameters" )$method , "global" = 1 , 2 ) + switch( as.character( is.null( methods::slot( x , "parameters" )$informer ) == FALSE ) , "TRUE" = 1 , "FALSE" = 0 )
  if (is.null( methods::slot( x , "parameters" )$informer ) == FALSE ) {
    grid::pushViewport( grid::viewport( x = grid::unit( 0 , "npc" ) ,   y = grid::unit( 0 , "npc" ) , width = grid::unit( 1 , "npc" ) ,  height = grid::unit( 1/part , "npc" ) , just = c( 0 , 0 ) )   )
    grid::grid.text( x = grid::unit( 0 , "npc" ) , paste( switch( 	methods::slot( x , "parameters" )$informer , "mean" = "Mean" ,  "median" = "Median" ), "of the span" ) , gp = gpar( col = "black" , fontsize = Fontsize.label.color ) ,  just = c( 0 , 1/2 )  )
    grid::grid.lines( x = grid::unit( c( 0.5 , 0.9 ) , "npc" ) , y = grid::unit( c( 0.4 , 0.4 ) , "npc" ) ,  default.units = "npc" ,  arrow = NULL,   gp = gpar( col = "black" , lwd = 2 )  )
    grid::upViewport()
  }
  grid::pushViewport( grid::viewport( x = grid::unit( 0 , "npc" ) , y = grid::unit( switch( as.character( is.null( methods::slot( x , "parameters" )$informer ) == FALSE ) , "TRUE" = 1 , "FALSE" = 0 ) / part , "npc" ), width = grid::unit( 1 , "npc" ) ,   height = grid::unit( 1/part , "npc" ) , just = c( 0 , 0 )  ) )
  if (methods::slot( x , "parameters"  )$method != "global"  ) {
    grid::pushViewport( grid::viewport( layout = grid.layout( 1 , 3 ) , width = 1 , height = 1 ) )
    if ( length( methods::slot( book , "BZLong" ) )  > 0 ) {
      grid::pushViewport( grid::viewport( layout.pos.col = 3 , layout.pos.row = 1 ) )
      grid::grid.text( x = grid::unit( 0 , "npc" ) , "Not in time" , gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1/2 ) )
      grid::grid.circle( 	x = grid::unit( 0.8 , "npc" ) ,   r = grid::unit( 0.4 , "npc" ) ,  gp = gpar( 	col = FALSE , fill = methods::slot( x , "colvect" )[ 2 , lcol ]    ) )
      grid::grid.circle( 	x = grid::unit( 0.8 , "npc" ) ,  r = grid::unit( 0.4 , "npc" ) ,  gp = gpar( 	col = FALSE , fill = colblackzone , alpha = alphaZones ) )
      grid::upViewport()
    }
    grid::pushViewport( grid::viewport( layout.pos.col = 2 , layout.pos.row = 1 ) )
    grid::grid.text( 	x = grid::unit( 0 , "npc" ) ,  "Done" , gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1/2 ) )
    grid::grid.circle( 	x = grid::unit( 0.6 , "npc" ) , r = grid::unit( 0.4 , "npc" ) , gp = gpar( col = FALSE , fill = methods::slot( x , "colvect" )[ 2 , lcol] ) )
    grid::upViewport()
    grid::pushViewport( grid::viewport( layout.pos.col = 1 , layout.pos.row = 1 ) )
    grid::grid.text(	x = grid::unit( 0 , "npc" ) ,  paste( "Group" , levels( methods::slot( x , "group") )[ 2 ] ) ,  gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1/2 ) )
    grid::upViewport()
    grid::upViewport()
    grid::upViewport()
    grid::pushViewport( grid::viewport( x = grid::unit( 0 , "npc" ) ,
                                        y = grid::unit( (switch(  as.character( is.null( methods::slot( x , "parameters" )$informer ) == FALSE )  , "TRUE" = 1 , "FALSE" = 0 ) + 1 ) / part , "npc" ),
                                        width = grid::unit( 1 , "npc" ) ,
                                        height = grid::unit( 1/part , "npc" ) , just = c( 0 , 0 )  ) )
    grid::pushViewport( grid::viewport( layout = grid.layout( 1 , 3 ) , width = 1 , height = 1 ) )
    if ( length( methods::slot( book , "BZLong" ) )  > 0 ) {
      grid::pushViewport( grid::viewport( layout.pos.col = 3 , layout.pos.row = 1 ) )
      grid::grid.text( x = grid::unit( 0 , "npc" ) , "Not in time" , gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1/2 ) )
      grid::grid.circle( 	x = grid::unit( 0.8 , "npc" ) ,   r = grid::unit( 0.4 , "npc" ) , gp = gpar( 	col = FALSE ,  fill = methods::slot( x , "colvect" )[ 1 , lcol ]   ) )
      grid::grid.circle( 	x = grid::unit( 0.8 , "npc" ) ,   r = grid::unit( 0.4 , "npc" ) , gp = gpar( 	col = FALSE , fill = colblackzone , alpha = alphaZones ) )
      grid::upViewport()
    }
    grid::pushViewport( grid::viewport( layout.pos.col = 2 , layout.pos.row = 1 ) )
    grid::grid.text( 	x = grid::unit( 0 , "npc" ) , "Done" , gp = gpar( col = "black" , fontsize = Fontsize.label.color ) ,   just = c( 0 , 1/2 ))
    grid::grid.circle( 	x = grid::unit( 0.6 , "npc" ) , r = grid::unit( 0.4 , "npc" ) , gp = gpar( col = FALSE , fill = methods::slot( x , "colvect" )[ 1 ,  lcol ] ) )
    grid::upViewport()
    grid::pushViewport( grid::viewport( layout.pos.col = 1 , layout.pos.row = 1 ) )
    grid::grid.text(	x = grid::unit( 0 , "npc" ) ,  paste( "Group" , levels( methods::slot( x , "group") )[ 1 ] ) ,   gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1/2 )  )
    grid::upViewport()
    grid::upViewport()
    grid::upViewport()
  }else{
    grid::pushViewport( grid::viewport( layout = grid.layout( 1 , 2 ) , width = 1 , height = 1 ) )
    if ( length( methods::slot( book , "BZLong" ) )  > 0 ) {
      grid::pushViewport( grid::viewport( layout.pos.col = 2, layout.pos.row = 1 ) )
      grid::grid.text( x = grid::unit( 0 , "npc" ) , "Not in time" , gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1/2 ) )
      grid::grid.circle( 	x = grid::unit( 0.8 , "npc" ) ,   r = grid::unit( 0.4 , "npc" ) , gp = gpar( 	col = FALSE , fill = methods::slot( x , "colvect" )[ 1 , lcol ]   ) )
      grid::grid.circle( 	x = grid::unit( 0.8 , "npc" ) ,  r = grid::unit( 0.4 , "npc" ) ,  gp = gpar( 	col = FALSE , fill = colblackzone , alpha = alphaZones )  )
      grid::upViewport()
    }
    grid::pushViewport( grid::viewport( layout.pos.col = 1 , layout.pos.row = 1 ) )
    grid::grid.text( 	x = grid::unit( 0 , "npc" ) , "Done" ,  gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1/2 )  )
    grid::grid.circle( 	x = grid::unit( 0.6 , "npc" ) , r = grid::unit( 0.4 , "npc" ) ,   gp = gpar( col = FALSE , fill = methods::slot( x , "colvect" )[ 1 , lcol ] ) )
    grid::upViewport()
    grid::upViewport()
    grid::upViewport()
  }
  grid::upViewport()
  grid::upViewport()
}
if (any( methods::slot( book , "typeA" )[ sortindex ] == "p" ) == FALSE &  any( methods::slot( book , "typeA" )[ sortindex ] == "l")) {
  grid::pushViewport( grid::viewport( y = grid::unit( 0.01 , "npc"),
                                      x = grid::unit( (1 - vp0w ) * 2 / 3  , "npc" ) ,
                                      height = grid::unit( (1 - vp0h ) * 1 / 3 + 0.02, "npc" ) ,
                                      width = grid::unit( vp0w , "npc" ) ,
                                      just = c( 0 , 0 )))
  grid::grid.rect( gp = gpar( col = "black" , fill = "grey",alpha = 0.3 ))
  ###### Long action..................................................................................................................................
  ###### Long action..................................................................................................................................
  grid::pushViewport( grid::viewport( x = grid::unit( 0 , "npc" ) ,  y = grid::unit( 5/6 , "npc" ) ,  width = grid::unit( 1 , "npc" ) , height = grid::unit( 1/6 , "npc" ) ,  just = c( 0 , 0 )  ))
  grid::grid.text(  x = grid::unit( 0 , "npc" ) ,  y = grid::unit( 1 , "npc" ) ,  paste("Long Action  -  N = ",  dim(methods::slot(x,"L"))[1] ),  gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1 )  )
  grid::upViewport()
  grid::pushViewport( grid::viewport( x = grid::unit( 0 , "npc" ) ,  y = grid::unit( 0 , "npc" ) ,  width = grid::unit( 1 , "npc" ) ,  height = grid::unit( 5/6 , "npc" ) , just = c( 0 , 0 )  )  )
  part = switch( methods::slot( x , "parameters" )$method , "global" = 1 , 2 ) + switch( as.character( is.null( methods::slot( x , "parameters" )$informer ) == FALSE ) , "TRUE" = 1 , "FALSE" = 0 )
  if (is.null( methods::slot( x , "parameters" )$informer ) == FALSE ) {
    grid::pushViewport( grid::viewport( x = grid::unit( 0 , "npc" ) ,   y = grid::unit( 0 , "npc" ) , width = grid::unit( 1 , "npc" ) ,  height = grid::unit( 1/part , "npc" ) , just = c( 0 , 0 ) )   )
    grid::grid.text( x = grid::unit( 0 , "npc" ) , paste( switch( 	methods::slot( x , "parameters" )$informer , "mean" = "Mean" ,  "median" = "Median" ), "of the span" ) , gp = gpar( col = "black" , fontsize = Fontsize.label.color ) ,  just = c( 0 , 1/2 )  )
    grid::grid.lines( x = grid::unit( c( 0.6 , 0.9 ) , "npc" ) , y = grid::unit( c( 0.4 , 0.4 ) , "npc" ) ,  default.units = "npc" ,  arrow = NULL,   gp = gpar( col = "black" , lwd = 2 )  )
    grid::upViewport()
  }
  grid::pushViewport( grid::viewport( x = grid::unit( 0 , "npc" ) , y = grid::unit( switch( as.character( is.null( methods::slot( x , "parameters" )$informer ) == FALSE ) , "TRUE" = 1 , "FALSE" = 0 ) / part , "npc" ),
                                      width = grid::unit( 1 , "npc" ) ,
                                      height = grid::unit( 1/part , "npc" ) ,
                                      just = c( 0 , 0 )  ) )
  if (methods::slot( x , "parameters"  )$method != "global"  ) {
    grid::pushViewport( grid::viewport( layout = grid.layout( 1 , 3 ) , width = 1 , height = 1 ) )
    if ( length( methods::slot( book , "BZLong" ) )  > 0 ) {
      grid::pushViewport( grid::viewport( layout.pos.col = 3 , layout.pos.row = 1 ) )
      grid::grid.text( x = grid::unit( 0 , "npc" ) , "Not in time" , gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1/2 ) )
      grid::grid.circle( 	x = grid::unit( 0.8 , "npc" ) ,   r = grid::unit( 0.4 , "npc" ) ,  gp = gpar( 	col = FALSE , fill = methods::slot( x , "colvect" )[ 2 , lcol ]    ) )
      grid::grid.circle( 	x = grid::unit( 0.8 , "npc" ) ,  r = grid::unit( 0.4 , "npc" ) ,  gp = gpar( 	col = FALSE , fill = colblackzone , alpha = alphaZones ) )
      grid::upViewport()
    }   #    grid::grid.rect( gp = gpar( col = "black" , fill= FALSE ))
    grid::pushViewport( grid::viewport( layout.pos.col = 2 , layout.pos.row = 1 ) )
    grid::grid.text( 	x = grid::unit( 0 , "npc" ) ,  "Done" , gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1/2 ) )
    grid::grid.circle( 	x = grid::unit( 0.6 , "npc" ) , r = grid::unit( 0.4 , "npc" ) , gp = gpar( col = FALSE , fill = methods::slot( x , "colvect" )[ 2 , lcol ] ) )
    grid::upViewport()
    grid::pushViewport( grid::viewport( layout.pos.col = 1 , layout.pos.row = 1 ) )
    grid::grid.text(	x = grid::unit( 0 , "npc" ) ,  paste( "Group" , levels( methods::slot( x , "group") )[ 2 ] ) ,  gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1/2 ) )
    grid::upViewport()
    grid::upViewport()
    grid::upViewport()
    grid::pushViewport( grid::viewport( x = grid::unit( 0 , "npc" ) ,
                                        y = grid::unit( (switch(  as.character( is.null( methods::slot( x , "parameters" )$informer ) == FALSE )  , "TRUE" = 1 , "FALSE" = 0 ) + 1 ) / part , "npc" ),
                                        width = grid::unit( 1 , "npc" ),
                                        height = grid::unit( 1/part , "npc" ) , just = c( 0 , 0 )  ) )
    grid::pushViewport( grid::viewport( layout = grid.layout( 1 , 3 ) , width = 1 , height = 1 ) )
    if ( length( methods::slot( book , "BZLong" ) )  > 0 ) {
      grid::pushViewport( grid::viewport( layout.pos.col = 3 , layout.pos.row = 1 ) )
      grid::grid.text( x = grid::unit( 0 , "npc" ) , "Not in time" , gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1/2 ) )
      grid::grid.circle( 	x = grid::unit( 0.8 , "npc" ) ,   r = grid::unit( 0.4 , "npc" ) , gp = gpar( 	col = FALSE ,  fill = methods::slot( x , "colvect" )[ 1 , lcol ]   ) )
      grid::grid.circle( 	x = grid::unit( 0.8 , "npc" ) ,   r = grid::unit( 0.4 , "npc" ) , gp = gpar( 	col = FALSE , fill = colblackzone , alpha = alphaZones ) )
      grid::upViewport()
    }
    grid::pushViewport( grid::viewport( layout.pos.col = 2 , layout.pos.row = 1 ) )
    grid::grid.text( 	x = grid::unit( 0 , "npc" ) , "Done" , gp = gpar( col = "black" , fontsize = Fontsize.label.color ) ,   just = c( 0 , 1/2 ))
    grid::grid.circle( 	x = grid::unit( 0.6 , "npc" ) , r = grid::unit( 0.4 , "npc" ) , gp = gpar( col = FALSE , fill = methods::slot( x , "colvect" )[ 1 , lcol ] ) )
    grid::upViewport()
    grid::pushViewport( grid::viewport( layout.pos.col = 1 , layout.pos.row = 1 ) )
    grid::grid.text(	x = grid::unit( 0 , "npc" ) ,  paste( "Group" , levels( methods::slot( x , "group") )[ 1 ] ) ,   gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1/2 )  )
    grid::upViewport()
    grid::upViewport()
    grid::upViewport()
  }else{
    grid::pushViewport( grid::viewport( layout = grid.layout( 1 , 2 ) , width = 1 , height = 1 ) )
    if ( length( methods::slot( book , "BZLong" ) )  > 0 ) {
      grid::pushViewport( grid::viewport( layout.pos.col = 2, layout.pos.row = 1 ) )
      grid::grid.text( x = grid::unit( 0 , "npc" ) , "Not in time" , gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1/2 ) )
      grid::grid.circle( 	x = grid::unit( 0.8 , "npc" ) ,   r = grid::unit( 0.4 , "npc" ) , gp = gpar( 	col = FALSE , fill = methods::slot( x , "colvect" )[ 1 , lcol ]   ) )
      grid::grid.circle( 	x = grid::unit( 0.8 , "npc" ) ,  r = grid::unit( 0.4 , "npc" ) ,  gp = gpar( 	col = FALSE , fill = colblackzone , alpha = alphaZones )  )
      grid::upViewport()
    }
    grid::pushViewport( grid::viewport( layout.pos.col = 1 , layout.pos.row = 1 ) )
    grid::grid.text( 	x = grid::unit( 0 , "npc" ) , "Done" ,  gp = gpar( col = "black" , fontsize = Fontsize.label.color ) , just = c( 0 , 1/2 )  )
    grid::grid.circle( 	x = grid::unit( 0.6 , "npc" ) , r = grid::unit( 0.4 , "npc" ) ,   gp = gpar( col = FALSE , fill = methods::slot( x , "colvect" )[ 1 , lcol ] ) )
    grid::upViewport()
    grid::upViewport()
    grid::upViewport()
  }
  grid::upViewport()
  grid::upViewport()
}
}