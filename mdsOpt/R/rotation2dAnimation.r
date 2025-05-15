rotation2dAnimation<-function(conf2d,
ani.interval=0.2,
ani.nmax=361,
ani.width=500,
ani.height=500,
ani.video.name="mds_rotate.mp4",
angle.start=-pi,
angle.stop=pi,
angle.step=pi/180){


saveVideo({
  t = seq(angle.start,angle.stop,angle.step)
  if(length(t)>ani.nmax){
    message("number of animations steps increased to number of angles")
    ani.nmax=length(t)
  }
  ani.options(interval = ani.interval, nmax = ani.nmax)
  
  for (alfa in t) {

nowa<-Rotation(conf2d,alfa)

plot(nowa, xlab="Dimension 1 (D1)",ylab="Dimension 2 (D2)",main=paste(round(alfa/pi*180,2),"degrees , ",round(alfa,3),"radians"),asp=1)
points(nowa[c(1:nrow(conf2d)),],pch=1,col="black")
text(nowa[1:nrow(conf2d),],pos=3,cex=0.8)
 }
  
}, video.name = ani.video.name, ani.width = ani.width , ani.height = ani.height) 

}
