# inst.pkg(maps)
# inst.pkg(mapdata)

.get.worldmap <- function(resolution){
  data('worldHiresMapEnv',envir=environment(),package="mapdata")
  
  
  resolution <- 0
  # dev.new(); maps::map(database='worldHires', xlim=c(-180,360),ylim=c(-90,90),add=F,resolution=resolution,fill=T,col="grey")
#   dev.new(); plot(NA, xlim=c(-180,360),ylim=c(-90,90))
  m <- maps::map(database='worldHires', xlim=c(-180,360),ylim=c(-60,90),plot=F,add=F, resolution=resolution,fill=T,col="grey")
  
  ### get Antarktis
  u <- maps::map(database='worldHires',"Antarctica",plot=F,resolution=resolution,fill=T)
  ii <- which(u$y == min(u$y,na.rm=T)); 
  i2 <- which(u$x[1:ii] < u$x[ii])
  # u$y[ii] <- u$y[tail(i2,1)] <- -90
  u$x[i2] <- 180+180+u$x[i2]
  a0 <- maps::map(u,plot=F,resolution=resolution,fill=T,col="grey",add=T)
  
  u$x <- u$x+360
  a1 <- maps::map(u,plot=F,resolution=resolution,fill=T,col="grey",add=T)
  
  u$x <- u$x-720
  a2 <- maps::map(u,plot=F,resolution=resolution,fill=T,col="grey",add=T)
  # rect(-180,-90,360,u$y[ii],col="grey",border = "grey")
  # abline(v=u$x[ii])
  # abline(h=-84,col="green")
  # i2 <- which(u$x[1:ii] < u$x[ii])
  # dev.new(); maps::map(u,add=F,resolution=resolution,fill=T,col="grey")
  
  ### re-add eastern pacific-america-western atlantic!
  m2 <- maps::map('worldHires', xlim=c(-180, 180),ylim=c(-60,90),plot=F,resolution=resolution,fill=T) # before xlim[2] was 20
  m2$x <- 180+180+m2$x

# m3 <- m2
# m3$x <- 180+180+m3$x

  # maps::map(m2,add=T,resolution=resolution,fill=T,col="grey")
  # axis(1); axis(2); box()
  # ii <- which(grepl("Antarctica",m2$names))
  # ii <- which(m2$names == "Antarctica"))
  # maps::map(m2,add=T,resolution=resolution,fill=T,col="grey")

  worldmap <- m
  worldmap$x <- c(m$x,NA,a0$x,NA,a1$x,NA,a2$x,NA,m2$x)#,NA,m3$x)
  worldmap$y <- c(m$y,NA,a0$y,NA,a1$y,NA,a2$y,NA,m2$y)#,NA,m3$y)
  # dev.new(); worldmap <- maps::map(worldmap,resolution=0,fill=T,col="grey",xlim=c(-180, 360),ylim=c(-90,90))
#   dev.new(); maps::map(worldmap,resolution=0,fill=T)
  return(worldmap)
}