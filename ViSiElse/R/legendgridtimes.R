legendgridtimes <- function( vp0 ,vp0w,vp0h, x , lgv, inftps, scal.unit.tps,col.grid,lwd.grid,lty.grid,Fontsize.label.Time,unit.tps ){
    grid::pushViewport(vp0)
    if( slot( x , "parameters")$t_0 >=0 ){
      grid::grid.grill( h = grid::unit( seq( 1 / (lgv ) , 1 - 1 / lgv , 1 / lgv ) ,"npc" ) ,
                        v = grid::unit( seq( 1,floor( inftps / scal.unit.tps ) , 1 ) /inftps* scal.unit.tps  , "npc" ),
                        gp = gpar( col = col.grid , lwd = lwd.grid , lty = lty.grid )
      )
      xx <- 0
    }else{
      grid::pushViewport(grid::viewport(  x = grid::unit((-slot( x , "parameters")$t_0)/ inftps , "npc" ) ,
                                          y = grid::unit( 0 , "npc" ) ,
                                          width = grid::unit( (inftps+slot( x , "parameters")$t_0)/ inftps , "npc" ) ,
                                          height = grid::unit( 1 , "npc" ) , just = c( 0 , 0 )))
      grid::grid.grill( h = grid::unit( seq( 1 / (lgv ) , 1 - 1 / lgv , 1 / lgv ) ,"npc" ) ,
                        v = grid::unit( seq( 1,floor( (inftps+slot( x , "parameters")$t_0) / scal.unit.tps ) , 1 ) /(inftps+slot( x , "parameters")$t_0)* scal.unit.tps  , "npc" ),
                        gp = gpar( col = col.grid , lwd = lwd.grid , lty = lty.grid ))
      grid::grid.lines( x = grid::unit( c( 0 , 0 ) , "npc" ) ,
                        y = grid::unit( c( 0 , 1 ) , "npc" ) ,
                        default.units = "npc" ,
                        gp = gpar( col = col.grid , lwd = 2 ))
      grid::upViewport() # Out of the cadre
      grid::pushViewport(grid::viewport(  x = grid::unit(  0, "npc" ) ,
                                          y = grid::unit( 0 , "npc" ) ,
                                          width = grid::unit( ( -slot( x , "parameters")$t_0)/ inftps , "npc" ) ,
                                          height = grid::unit( 1 , "npc" ) , just = c( 0 , 0 )))
      xx <-  1- (seq(floor( (-slot( x , "parameters")$t_0) / scal.unit.tps ),1 , -1 ) /(-slot( x , "parameters")$t_0)* scal.unit.tps   )[1]
      grid::pushViewport(grid::viewport(  x = grid::unit( xx, "npc" ) ,
                                          y = grid::unit( 0 , "npc" ) ,
                                          width = grid::unit( 1- xx , "npc" ) ,
                                          height = grid::unit( 1 , "npc" ) , just = c( 0 , 0 )))
      grid::grid.grill( h = grid::unit( seq( 1 / (lgv ) , 1 - 1 / lgv , 1 / lgv ) ,"npc" ) ,
                        v = grid::unit( seq( 1,floor( (-slot( x , "parameters")$t_0) / scal.unit.tps ) , 1 ) /floor( (-slot( x , "parameters")$t_0) / scal.unit.tps )  , "npc" ),
                        gp = gpar( col = col.grid , lwd = lwd.grid , lty = lty.grid ))
      grid::upViewport() # Out of the cadre
      grid::upViewport() # Out of the cadre
    }
    
    
    grid::grid.lines( x = grid::unit(c( 0 , 1 ), "npc"),
                      y = grid::unit( c( 0 ) , "npc" ) ,
                      default.units = "npc" ,
                      arrow = arrow( angle = 20 , length = grid::unit( 0.10 , "inches" ) ) ,
                      gp = gpar( col = "black" , lwd = 2 ) )
    grid::grid.lines( x = grid::unit( c( 0 , 0 ) , "npc" ) ,
                      y = grid::unit( c( 0 , 1 ) , "npc" ) ,
                      default.units = "npc" ,
                      gp = gpar( col = "black" , lwd = 2 ))
    grid::upViewport() # Out of the cadre
    if ( (vp0w / floor( inftps / scal.unit.tps ) < 0.03 ) &  slot( x , "parameters")$t_0 ==0  ) {
      grid::pushViewport(grid::viewport( grid::unit( x = (1 - vp0w ) * 2 / 3 , "npc" ) ,
                                         y = grid::unit( (1 - vp0h ) * 2 / 3 , "npc" ) ,
                                         width = grid::unit( 0.03 , "npc" ) ,
                                         height = grid::unit( (1 - vp0h ) * 1 / 6 , "npc" ) , just = c( 1 , 1 )))
      grid::grid.text( 	x = grid::unit( 0.1 , "npc" ) ,
                        y = grid::unit( 0.5 , "npc" ) , paste( methods::slot( x , "vect_tps" )[ 1 ]  ) ,
                        gp = gpar( col = "black" , fontsize = Fontsize.label.Time ) , just = c( 0 , 0 ))
      grid::grid.lines( x = grid::unit( c( 1 , 1 ) , "npc" ) ,
                        y = grid::unit( c( 0.5 , 1 ) , "npc" ) ,
                        default.units = "npc" ,
                        gp = gpar( col = "black" ) )
      grid::upViewport()
    }else{
      grid::pushViewport(grid::viewport( x = grid::unit( (1 - vp0w ) * 2 / 3 + (vp0w )*( -slot( x , "parameters")$t_0 )/ inftps, "npc" ) ,
                                         y = grid::unit( (1 - vp0h ) * 2 / 3 , "npc" ) ,
                                         width = grid::unit( 0.03	, "npc" ) ,
                                         height = grid::unit( (1 - vp0h ) * 1 / 6 , "npc" ) ,
                                         just = c( 0 , 1)))
      grid::grid.text( x = grid::unit( 0.1 , "npc" ) ,
                       y = grid::unit( 0.5 , "npc" ) ,
                       paste( methods::slot( x , "vect_tps" )[ 1 ] ) ,
                       gp = gpar( col = "black" , fontsize = Fontsize.label.Time ) ,
                       just = c( 0 , 0 ))
      grid::grid.lines( x = grid::unit( c( 0 , 0 ) , "npc" ) ,
                        y = grid::unit( c( 0.5 , 1 ) , "npc" ) ,
                        default.units = "npc" ,
                        gp = gpar( col = "black" ))
      grid::upViewport()
    }
    #___________________________________________________________________________________________________________________
    grid::pushViewport(grid::viewport( x = grid::unit( (1 - vp0w ) * 2 / 3 +  (vp0w )*( (-slot( x , "parameters")$t_0 )/ inftps +  scal.unit.tps/inftps )  , "npc" ) ,
                                       y = grid::unit( (1 - vp0h ) * 2 / 3 , "npc" ) ,
                                       width = grid::unit( 0.03 + 0.01 * nchar( scal.unit.tps ) , "npc" ) ,
                                       height = grid::unit( (1 - vp0h ) / 6 , "npc" ) ,
                                       just = c( 0 , 1 )
    )
    )
    grid::grid.text( x = grid::unit( 0 , "npc" ) ,
                     y = grid::unit( 0.5 , "npc" ) ,
                     paste( methods::slot( x , "vect_tps" )[ 1 ] + scal.unit.tps) ,
                     gp = gpar( fontsize = Fontsize.label.Time ) ,
                     just = c( 0 , 0)
    )
    grid::grid.lines( x = grid::unit( c( 0 , 0 ) , "npc" ) ,
                      y = grid::unit( c( 0.5 , 1 ) , "npc" ) ,
                      default.units = "npc" ,
                      gp = gpar( col = "black" ) )
    grid::upViewport()
    #___________________________________________________________________________________________________________________
    grid::pushViewport(grid::viewport( x = grid::unit( vp0w * (floor( inftps / scal.unit.tps ) - 1 ) / floor( inftps / scal.unit.tps ) + (1 - vp0w ) * 2 / 3 , "npc" ) ,
                                       y = grid::unit( (1 - vp0h ) * 2 / 3 , "npc" ) ,
                                       width = grid::unit( vp0w / floor( inftps / scal.unit.tps ) , "npc" ) ,
                                       height = grid::unit( (1 - vp0h ) / 6 , "npc" ) ,
                                       just = c( 0 , 1 )))
    grid::grid.text( paste0( "Time (" , unit.tps , ")" ) ,
                     gp = gpar( fontsize = Fontsize.label.Time ) , just = c( 1 , 0 ))
    grid::upViewport()
}