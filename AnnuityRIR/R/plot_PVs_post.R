plot_PVs_post=function(data,years=10,lwd=1.5,lty1=1, lty2=2,lty3=3,lty4=4,lty5=5,lty6=6){


  art=rep(NA,years)
  lin=rep(NA,years)
  moo=rep(NA,years)
  moo2=rep(NA,years)
  ex=rep(NA,years)
  tri=rep(NA,years)

for (i in 1:years) {
  art[i]=PV_post_artan(data,i)
  lin[i]=PV_post_cubic(data,i)
  moo[i]=PV_post_mood_pm(data,i)
  moo2[i]=PV_post_mood_nm(data,i)
  ex[i]=PV_post_exact(data,i)
  tri[i]=PV_post_triang_dis(data*100,i)

}

result=cbind(art,lin,moo,moo2,ex,tri)
result

plot(art,type="l",col="red", lwd=lwd, lty=lty1, xlim=c(0,years),ylim=c(0,max(result)),xlab="Time", ylab="Present Expexted Value",main="Present Expected Value (annuity-due)")
par(new=TRUE)
plot(lin,type="l",col="black", lwd=lwd, lty=lty2, xlim=c(0,years),ylim=c(0,max(result)),xlab="",ylab="")
par(new=TRUE)
plot(moo,type="l",col="green", lwd=lwd, lty=lty3, xlim=c(0,years),ylim=c(0,max(result)),xlab="",ylab="")
par(new=TRUE)
plot(moo2,type="l",col="green", lwd=lwd, lty=lty4, xlim=c(0,years),ylim=c(0,max(result)),xlab="",ylab="")
par(new=TRUE)
plot(ex,type="l",col="blue", lwd=lwd, lty=lty5, xlim=c(0,years),ylim=c(0,max(result)),xlab="",ylab="")
par(new=TRUE)
plot(tri,type="l",col="pink", lwd=lwd, lty=lty6, xlim=c(0,years),ylim=c(0,max(result)),xlab="",ylab="")


legend("topleft",c("Tetraparametric","Cubic","Mood PM","Mood NM","Exact","Triangular"), lwd=lwd, lty=c(lty1,lty2,lty3,lty4,lty5,lty6),
 y.intersp = 0.65, col=c("red","black","green","green","blue","pink"),  xjust = 1, merge = TRUE)

}
