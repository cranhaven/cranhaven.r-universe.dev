legend_action <- function(book, main = " " ,
                       size.main = 12 ,  Fontsize.title = 11 ,
                       Fontsize.label.Action = 11,
                       col.main = "black", ncharlabel,
                       vp0h	= 0.6, vp0w = 0.6,layoutAction,sortindex,vplayoutA){
  # Legend Title graph
  grid::pushViewport(grid::viewport( x = grid::unit( (1 - vp0w ) * 2/3 , "npc" ) ,
                                     y = grid::unit( (1 - vp0h ) * 2/3 + vp0h , "npc") ,
                                     width = grid::unit( vp0w , "npc" ) ,
                                     height = grid::unit( (1 - vp0h ) * 0.5 , "npc" ) ,
                                     just = c( 0 , 0 ) ))
  grid::grid.text( main , gp = gpar( fontsize = size.main , col = col.main) )
  grid::upViewport()
  #___________________________________________________________________________________________________________________
  # Legend names action
  grid::pushViewport(grid::viewport( 	x = grid::unit( 0 , "npc" ) ,
                                      y = grid::unit( (1 - vp0h ) * 2/3 , "npc" ) ,
                                      width = (1 - vp0w ) * 2/3  ,
                                      height = vp0h ,
                                      just = c( 0 , 0 ) ,
                                      name = "vp0" ))
  grid::pushViewport( layoutAction )
  for (ii in sortindex ) {
    grid::pushViewport( vplayoutA[[ which( sortindex == ii ) ]] )
    grid::grid.text( 	substr( as.character(methods::slot( book , "label" )[ ii ] ), 1 , ncharlabel ) ,
                      rot = 0 ,
                      gp = gpar( col = "black" , fontsize = Fontsize.label.Action , fontface = "plain" ))
    grid::upViewport()
  }
  grid::upViewport()
  grid::upViewport()
#___________________________________________________________________________________________________________________
}