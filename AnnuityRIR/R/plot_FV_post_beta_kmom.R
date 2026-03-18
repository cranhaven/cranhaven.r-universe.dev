plot_FV_post_beta_kmom=function(data,years=10,lwd=1.5,lty=1){

appo=rep(NA,years)
for (i in 1:years) {
  appo[i]=FV_post_beta_kmom(data,i)
}
appo[1]=1
anni=c(1:years)
plot(appo,type="b",col="red",lwd=lwd, lty=lty, xlim=c(0,years+1),ylim=c(min(appo),max(appo)),xlab="Time", ylab="Final Expexted Value",main="Final Expected Value (annuity-immediate)")
if (length(anni)<20) text(anni+0.2, appo, round(appo, 2), cex=0.7)

 }


