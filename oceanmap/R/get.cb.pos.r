get.cb.pos <- function(cbx,cby,oticks){
  
  cbp.diff <- min(c(diff(par()$usr[1:2]),diff(par()$usr[3:4])))
  
  pos <- list()
  if(oticks == 'b') {
  cbp <- range(cby)
  pos$ticks.length <- c(cbp[1],max(cbp[1]-cbp.diff*0.025,
                     cbp[1]-0.75*abs(cbp[2]-cbp[1])))
  pos$ticks.lab <- max(cbp[1]-cbp.diff*0.05,
                       cbp[1]-2*abs(cbp[2]-cbp[1]))
  pos$cb.title <- min(cbp[2]+cbp.diff*0.045,
                        cbp[2]+2*abs(cbp[2]-cbp[1]))
  pos$cb.xlab <- max(cbp[1]-cbp.diff*0.09,
                       cbp[1]-4.5*abs(cbp[2]-cbp[1]))
  }
  
  if(oticks == 'l') {  
    cbp <- cbx
    pos$ticks.length <- c(cbp[1],max(cbp[1]-cbp.diff*0.025,
                                     cbp[1]-0.75*abs(cbp[2]-cbp[1])))
    pos$ticks.lab <- max(cbp[1]-cbp.diff*0.05,
                         cbp[1]-2*abs(cbp[2]-cbp[1]))
    pos$cb.title <- min(cby[2]+cbp.diff*0.045,
                          cby[2]+2*abs(cbp[2]-cbp[1]))
    pos$cb.xlab <- max(cbp[1]-cbp.diff*0.09,
                         cbp[1]-4.5*abs(cbp[2]-cbp[1]))
  }
  
  if(oticks == 'r') {  
    cbp <- cbx
    pos$ticks.length <- c(cbp[2],min(cbp[2]+cbp.diff*0.025,
                                     cbp[2]+0.75*abs(cbp[2]-cbp[1])))
    pos$ticks.lab <- min(cbp[2]+cbp.diff*0.05,
                         cbp[2]+2*abs(cbp[2]-cbp[1]))
    pos$cb.title <- min(cby[2]+cbp.diff*0.045,
                          cby[2]+2*abs(cbp[2]-cbp[1]))
    pos$cb.xlab <- min(cbp[2]+cbp.diff*0.09,
                         cbp[1]+4.5*abs(cbp[2]-cbp[1]))
  }
  
  return(pos)
}