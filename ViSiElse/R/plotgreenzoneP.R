plotgreenzoneP <- function(book,ia,inftps,colgreenzone,alphaZones){
  # Green Zone
  if (	length( methods::slot( book , "GZDeb") ) > 0 ) {   #### If some green zone are defined in book
    if (	is.na( methods::slot( book , "GZDeb")[ ia ] ) == FALSE ) {   ###If the action had a grren zone define
      temp <- c( methods::slot( book , "GZDeb")[ ia ] , methods::slot( book , "GZFin")[ ia ])
      if (temp[ 2 ] == Inf ) {
        temp[ 2 ] <- inftps
      }
      grid::pushViewport( grid::viewport( x = grid::unit( temp[ 1 ] / inftps , "npc" ) ,
                                          y = grid::unit( 0 , "npc" ) ,
                                          width = grid::unit( (temp[ 2 ] - temp[ 1 ] ) / inftps , "npc" ) ,
                                          height = grid::unit( 1 , "npc" ) , just = c( 0 , 0 ) ) )
      grid::grid.rect( gp = gpar( col = FALSE , fill = colgreenzone , alpha = alphaZones ) )
      grid::upViewport()
      if (	length( methods::slot( book , "Repetition") ) > 0 ) {
        if (	is.na( methods::slot( book , "Repetition")[ia] ) == FALSE ) {
          while (temp[2] + methods::slot( book , "Repetition")[ ia ] < inftps ) {
            temp <- temp + rep( methods::slot( book , "Repetition")[ ia ] ,2 )
            grid::pushViewport( grid::viewport( x = grid::unit( temp[ 1 ] / inftps , "npc" ) ,
                                                y = grid::unit( 0 , "npc" ) ,
                                                width = grid::unit( (temp[ 2 ] - temp[ 1 ] ) / inftps , "npc" ) ,
                                                height = grid::unit( 1 , "npc" ) , just = c( 0 , 0 ) ) )
            grid::grid.rect( gp = gpar( col = FALSE , fill = colgreenzone , alpha = alphaZones ) )
            grid::upViewport()
          }
        }
      }
    }
  }
}