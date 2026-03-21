plotblackzoneP <- function(book, ia, inftps, colblackzone, alphaZones){
  if (	length( methods::slot( book , "BZBeforeDeb" ) ) > 0 ) {   #### If some Black 1 zone are defined in book
    if (is.na( methods::slot( book , "BZBeforeDeb" )[ ia ] ) == FALSE  ) {
      temp <- c( methods::slot( book , "BZBeforeDeb" )[ ia ] , methods::slot( book , "BZBeforeFin" )[ ia ] )
      if (temp[ 2 ] == Inf ) {
        temp[ 2 ] <- inftps
      }
      grid::pushViewport(grid::viewport(	x = grid::unit( temp[ 1 ] / max( inftps ) , "npc" ) ,
                                         y = grid::unit( 0, "npc" ) ,
                                         width = grid::unit( (temp[ 2 ] - temp[ 1 ] ) / inftps , "npc" ) ,
                                         height = grid::unit( 1 , "npc" ) ,
                                         just = c( 0 , 0 )))
      grid::grid.rect( gp = gpar( col = FALSE , fill = colblackzone , alpha = alphaZones ) )
      grid::upViewport()
    }
  }
  
  if (	length( methods::slot( book , "BZAfterDeb" ) ) > 0 ) {   #### If some Black 1 zone are defined in book
    if (is.na( methods::slot( book , "BZAfterDeb" )[ ia ] ) == FALSE  ) {
      temp <- c( methods::slot( book , "BZAfterDeb" )[ ia ] , methods::slot( book , "BZAfterFin" )[ ia ] )
      if (temp[ 2 ] == Inf ) {
        temp[ 2 ] <- inftps
      }
      grid::pushViewport(grid::viewport(	x = grid::unit( temp[ 1 ] / max( inftps ) , "npc" ) ,
                                         y = grid::unit( 0, "npc" ) ,
                                         width = grid::unit( (temp[ 2 ] - temp[ 1 ] ) / inftps , "npc" ) ,
                                         height = grid::unit( 1 , "npc" ) ,
                                         just = c( 0 , 0 )
      ))
      grid::grid.rect( gp = gpar( col = FALSE , fill = colblackzone , alpha = alphaZones ) )
      grid::upViewport()
    }
  }
  
}