#' @import graphics
#' @import grDevices
#' @import imguR
h_sroc = function(K,samp,nstu,dat,testname, dirc){
  index<-indicator(K,nstu,dat)
  l<-except(index,K,nstu)
  summ<-summary(samp)
  result<-summary(samp)[[2]]
  para_study_se = paralist("Se.stud",summ)
  para_study_sp = paralist("Sp.stud",summ)
  stud.se = matrix(result[para_study_se,3],ncol=K,nrow=nstu,byrow=F)
  stud.sp = matrix(result[para_study_sp,3],ncol=K,nrow=nstu,byrow=F)
  smry<-summ$statistics[, c("Mean", "SD")]
  cov.id <- grep("Cov", rownames(smry))
  cov<-smry[cov.id,"Mean"]
  if(K==1){
    sp.range = matrix(c(range(stud.sp[,1])),ncol=K,nrow=2,byrow=F)
    pool.se = result[c("post.Se[1]"),3]
    pool.sp = result[c("post.Sp[1]"),3]
    alpha.post = rbind(c(samp[[1]][,c("mu[2]")]))
    rownames(alpha.post) = c("alpha1")
    beta.post = rbind(c(samp[[1]][,c("mu[3]")]))
    rownames(beta.post) = c("beta1")

    y_1l = vector()
    y_1u = vector()
    y_1m = vector()

    x1 = seq(1-sp.range[2,1],1-sp.range[1,1],length.out = 2000)
    for (i in 1:length(x1)){
      y_1l[i] = cb_h(x1[i],alpha.post[1,],beta.post[1,],cov[[6]], cov[[9]],0.025)
      y_1m[i] = cb_h(x1[i],alpha.post[1,],beta.post[1,],cov[[6]], cov[[9]],0.50)
      y_1u[i] = cb_h(x1[i],alpha.post[1,],beta.post[1,],cov[[6]], cov[[9]],0.975)
    }
    pdf(file.path(dirc,"sroc_hierarchical.pdf"),height=11,width=7)
    oldpar<-par(mfcol=c(1,2))
    on.exit(par(oldpar))
    plot(x1,y_1m,type="l",xlim= c(0,1),ylim=c(0,1),xlab="false positive rate",ylab="true positive rate",main=testname[1])
    polygon(c(x1,rev(x1)),c(y_1l,rev(y_1u)),density = c(50, 50),angle = c(-45, 45),col="lightgrey")

    legend1<-c(paste("SROC for ",testname[1]),paste("Estimated points of ",testname[1]),paste("Pooled TP and FP for ",testname[1]))
    plot(x1,y_1m,type="l",xlim= c(0,1),ylim=c(0,1),xlab="false positive rate",ylab="true positive rate")
    points(1-stud.sp[-c(sapply(l[[1]],c)),1],stud.se[-c(sapply(l[[1]],c)),1],pch=1,col="black")
    points(1-pool.sp,pool.se,pch=c(3),col=c("black"))
    legend("bottomright",legend=legend1,lty=c(1,NA,NA),pch=c(NA,1,3),col=c("black","black","black"),cex = 0.5)

  }
  if(K==2){
    sp.range = matrix(c(range(stud.sp[,1]),range(stud.sp[,2])),ncol=K,nrow=2,byrow=F)
    pool.se = result[c("post.Se[1]","post.Se[2]"),3]
    pool.sp = result[c("post.Sp[1]","post.Sp[2]"),3]
    alpha.post = rbind(c(samp[[1]][,c("mu[2]")]),c(samp[[1]][,c("mu[4]")]))
    rownames(alpha.post) = c("alpha1","alpha2")
    beta.post = rbind(c(samp[[1]][,c("mu[3]")]),c(samp[[1]][,c("mu[5]")]))
    rownames(beta.post) = c("beta1","beta2")

    y_1l = vector()
    y_1u = vector()
    y_1m = vector()
    x1 = seq(1-sp.range[2,1],1-sp.range[1,1],length.out = 2000)
    for (i in 1:length(x1)){
      y_1l[i] = cb_h(x1[i],alpha.post[1,],beta.post[1,],cov[[8]], cov[[13]],0.025)
      y_1m[i] = cb_h(x1[i],alpha.post[1,],beta.post[1,],cov[[8]], cov[[13]],0.50)
      y_1u[i] = cb_h(x1[i],alpha.post[1,],beta.post[1,],cov[[8]], cov[[13]],0.975)
    }

    y_2l = vector()
    y_2u = vector()
    y_2m = vector()
    x2 = seq(1-sp.range[2,2],1-sp.range[1,2],length.out = 2000)
    for (i in 1:length(x2)){
      y_2l[i] = cb_h(x1[i],alpha.post[2,],beta.post[2,],cov[[20]],cov[[25]],0.025)
      y_2m[i] = cb_h(x1[i],alpha.post[2,],beta.post[2,],cov[[20]],cov[[25]],0.50)
      y_2u[i] = cb_h(x1[i],alpha.post[2,],beta.post[2,],cov[[20]],cov[[25]],0.975)
    }

    pdf(file.path(dirc,"sroc_hierarchical.pdf"),height=11,width=7)
    oldpar<-par(mfrow=c(2,2))
    on.exit(par(oldpar))
    plot(x1,y_1m,type="l",xlim= c(0,1),ylim=c(0,1),xlab="false positive rate",ylab="true positive rate",main=testname[1])
    polygon(c(x1,rev(x1)),c(y_1l,rev(y_1u)),density = c(50, 50),angle = c(-45, 45),col="lightgrey")

    plot(x2,y_2m,type="l",xlim= c(0,1),ylim=c(0,1),col="blue",xlab="false positive rate",ylab="true positive rate",main=testname[2])
    polygon(c(x2,rev(x2)),c(y_2l,rev(y_2u)),density = c(50, 50),angle = c(-45, 45),col="lightgrey")

    plot(x1,y_1m,type="l",xlim= c(0,1),ylim=c(0,1),xlab="false positive rate",ylab="true positive rate")
    lines(x2,y_2m,type="l",xlim= c(0,1),ylim=c(0,1),lty = 2,col="blue")

    legend2<-c(paste("SROC for ",testname[1]),paste("SROC for ",testname[2]),paste("Estimated points of ",testname[1]),
               paste("Estimated points of ",testname[2]),paste("Pooled TP and FP for ",testname[1]),paste("Pooled TP and FP for ",testname[2]))
    points(1-stud.sp[-c(sapply(l[[1]],c)),1],stud.se[-c(sapply(l[[1]],c)),1],pch=1)
    points(1-stud.sp[-c(sapply(l[[2]],c)),2],stud.se[-c(sapply(l[[2]],c)),2],pch=2,col="blue")
    points(1-pool.sp,pool.se,pch=c(3,4),col=c("black","blue"))
    legend("bottomright",legend=legend2,lty=c(1,2,NA,NA,NA,NA),pch=c(NA,NA,1,2,3,4),col=c("black","blue","black","blue","black","blue"),cex = 0.55)
  }
  if(K==3){
    sp.range = matrix(c(range(stud.sp[,1]),range(stud.sp[,2]),range(stud.sp[,3])),ncol=K,nrow=2,byrow=F)
    pool.se = result[c("post.Se[1]","post.Se[2]","post.Se[3]"),3]
    pool.sp = result[c("post.Sp[1]","post.Sp[2]","post.Sp[3]"),3]
    alpha.post = rbind(c(samp[[1]][,c("mu[2]")]),
                      c(samp[[1]][,c("mu[4]")]),
                      c(samp[[1]][,c("mu[6]")])
    )
    rownames(alpha.post) = c("alpha1","alpha2","alpha3")
    beta.post = rbind(c(samp[[1]][,c("mu[3]")]),
                     c(samp[[1]][,c("mu[5]")]),
                     c(samp[[1]][,c("mu[7]")])
    )
    rownames(beta.post) = c("beta1","beta2","beta3")

    y_1l = vector()
    y_1u = vector()
    y_1m = vector()
    x1 = seq(1-sp.range[2,1],1-sp.range[1,1],length.out = 2000)
    for (i in 1:length(x1)){
      y_1l[i] = cb_h(x1[i],alpha.post[1,],beta.post[1,],cov[[10]],cov[[17]],0.025)
      y_1m[i] = cb_h(x1[i],alpha.post[1,],beta.post[1,],cov[[10]],cov[[17]],0.50)
      y_1u[i] = cb_h(x1[i],alpha.post[1,],beta.post[1,],cov[[10]],cov[[17]],0.975)
    }

    y_2l = vector()
    y_2u = vector()
    y_2m = vector()
    x2 = seq(1-sp.range[2,2],1-sp.range[1,2],length.out = 2000)
    for (i in 1:length(x2)){
      y_2l[i] = cb_h(x2[i],alpha.post[2,],beta.post[2,],cov[[26]],cov[[33]],0.025)
      y_2m[i] = cb_h(x2[i],alpha.post[2,],beta.post[2,],cov[[26]],cov[[33]],0.50)
      y_2u[i] = cb_h(x2[i],alpha.post[2,],beta.post[2,],cov[[26]],cov[[33]],0.975)
    }

    y_3l = vector()
    y_3u = vector()
    y_3m = vector()
    x3 = seq(1-sp.range[2,3],1-sp.range[1,3],length.out = 2000)
    for (i in 1:length(x3)){
      y_3l[i] = cb_h(x3[i],alpha.post[3,],beta.post[3,],cov[[42]],cov[[49]],0.025)
      y_3m[i] = cb_h(x3[i],alpha.post[3,],beta.post[3,],cov[[42]],cov[[49]],0.50)
      y_3u[i] = cb_h(x3[i],alpha.post[3,],beta.post[3,],cov[[42]],cov[[49]],0.975)
    }
    pdf(file.path(dirc,"sroc_hierarchical.pdf"),height=11,width=7)
    oldpar<-par(mfrow=c(2,2))
    on.exit(par(oldpar))
    plot(x1,y_1m,type="l",xlim= c(0,1),ylim=c(0,1),xlab="false positive rate",ylab="true positive rate",main=testname[1])
    polygon(c(x1,rev(x1)),c(y_1l,rev(y_1u)),density = c(50, 50),angle = c(-45, 45),col="lightgrey")

    plot(x2,y_2m,type="l",xlim= c(0,1),ylim=c(0,1),col="blue",xlab="false positive rate",ylab="true positive rate",main=testname[2])
    polygon(c(x2,rev(x2)),c(y_2l,rev(y_2u)),density = c(50, 50),angle = c(-45, 45),col="lightgrey")

    plot(x3,y_3m,type="l",xlim= c(0,1),ylim=c(0,1),col="blue",xlab="false positive rate",ylab="true positive rate",main=testname[3])
    polygon(c(x3,rev(x3)),c(y_3l,rev(y_3u)),density = c(50, 50),angle = c(-45, 45),col="lightgrey")

    plot(x1,y_1m,type="l",xlim= c(0,1),ylim=c(0,1),xlab="false positive rate",ylab="true positive rate")
    lines(x2,y_2m,type="l",xlim= c(0,1),ylim=c(0,1),lty = 2,col="blue")
    lines(x3,y_3m,type="l",xlim= c(0,1),ylim=c(0,1),lty = 3,col="green")

    legend3<-c(paste("SROC for ",testname[1]),paste("SROC for ",testname[2]),paste("SROC for ",testname[3]),paste("Estimated points of ",testname[1]),
               paste("Estimated points of ",testname[2]),paste("Estimated points of ",testname[3]),paste("Pooled TP and FP for ",testname[1]),paste("Pooled TP and FP for ",testname[2]),paste("Pooled TP and FP for ",testname[3]))
    points(1-stud.sp[-c(sapply(l[[1]],c)),1],stud.se[-c(sapply(l[[1]],c)),1],pch=1)
    points(1-stud.sp[-c(sapply(l[[2]],c)),2],stud.se[-c(sapply(l[[2]],c)),2],pch=2,col="blue")
    points(1-stud.sp[-c(sapply(l[[3]],c)),3],stud.se[-c(sapply(l[[3]],c)),3],pch=3,col="green")
    points(1-pool.sp,pool.se,pch=c(4,5,6),col=c("black","blue","green"))
    legend("bottomright",legend=legend3,lty=c(1,2,3,NA,NA,NA,NA,NA,NA),pch=c(NA,NA,NA,1,2,3,4,5,6),
           col=c("black","blue","green","black","blue","green","black","blue","green"),cex = 0.35)
  }
  if(K==4){
    sp.range = matrix(c(range(stud.sp[,1]),range(stud.sp[,2]),range(stud.sp[,3]),range(stud.sp[,4])),ncol=K,nrow=2,byrow=F)
    alpha.post = rbind(c(samp[[1]][,c("mu[2]")]),
                      c(samp[[1]][,c("mu[4]")]),
                      c(samp[[1]][,c("mu[6]")]),
                      c(samp[[1]][,c("mu[8]")]))
    rownames(alpha.post) = c("alpha1","alpha2","alpha3","alpha4")
    beta.post = rbind(c(samp[[1]][,c("mu[3]")]),
                     c(samp[[1]][,c("mu[5]")]),
                     c(samp[[1]][,c("mu[7]")]),
                     c(samp[[1]][,c("mu[9]")]))
    rownames(beta.post) = c("beta1","beta2","beta3","beta4")

    y_1l = vector()
    y_1u = vector()
    y_1m = vector()
    x1 = seq(1-sp.range[2,1],1-sp.range[1,1],length.out = 2000)
    for (i in 1:length(x1)){
      y_1l[i] = cb_h(x1[i],alpha.post[1,],beta.post[1,],cov[[12]],cov[[21]],0.025)
      y_1m[i] = cb_h(x1[i],alpha.post[1,],beta.post[1,],cov[[12]],cov[[21]],0.50)
      y_1u[i] = cb_h(x1[i],alpha.post[1,],beta.post[1,],cov[[12]],cov[[21]],0.975)
    }

    y_2l = vector()
    y_2u = vector()
    y_2m = vector()
    x2 = seq(1-sp.range[2,2],1-sp.range[1,2],length.out = 2000)
    for (i in 1:length(x2)){
      y_2l[i] = cb_h(x2[i],alpha.post[2,],beta.post[2,],cov[[32]],cov[[41]],0.025)
      y_2m[i] = cb_h(x2[i],alpha.post[2,],beta.post[2,],cov[[32]],cov[[41]],0.50)
      y_2u[i] = cb_h(x2[i],alpha.post[2,],beta.post[2,],cov[[32]],cov[[41]],0.975)
    }

    y_3l = vector()
    y_3u = vector()
    y_3m = vector()
    x3 = seq(1-sp.range[2,3],1-sp.range[1,3],length.out = 2000)
    for (i in 1:length(x3)){
      y_3l[i] = cb_h(x3[i],alpha.post[3,],beta.post[3,],cov[[52]],cov[[61]],0.025)
      y_3m[i] = cb_h(x3[i],alpha.post[3,],beta.post[3,],cov[[52]],cov[[61]],0.50)
      y_3u[i] = cb_h(x3[i],alpha.post[3,],beta.post[3,],cov[[52]],cov[[61]],0.975)
    }

    y_4l = vector()
    y_4u = vector()
    y_4m = vector()
    x4 = seq(1-sp.range[2,4],1-sp.range[1,4],length.out = 2000)
    for (i in 1:length(x4)){
      y_4l[i] = cb_h(x4[i],alpha.post[4,],beta.post[4,],cov[[72]],cov[[81]],0.025)
      y_4m[i] = cb_h(x4[i],alpha.post[4,],beta.post[4,],cov[[72]],cov[[81]],0.50)
      y_4u[i] = cb_h(x4[i],alpha.post[4,],beta.post[4,],cov[[72]],cov[[81]],0.975)
    }

    pdf(file.path(dirc,"sroc_hierarchical.pdf"),height=11,width=7)
    oldpar<-par(mfrow=c(3,2))
    on.exit(par(oldpar))
    plot(x1,y_1m,type="l",xlim= c(0,1),ylim=c(0,1),xlab="false positive rate",ylab="true positive rate",main=testname[1])
    polygon(c(x1,rev(x1)),c(y_1l,rev(y_1u)),density = c(50, 50),angle = c(-45, 45),col="lightgrey")

    plot(x2,y_2m,type="l",xlim= c(0,1),ylim=c(0,1),col="blue",xlab="false positive rate",ylab="true positive rate",main=testname[2])
    polygon(c(x2,rev(x2)),c(y_2l,rev(y_2u)),density = c(50, 50),angle = c(-45, 45),col="lightgrey")

    plot(x3,y_3m,type="l",xlim= c(0,1),ylim=c(0,1),col="blue",xlab="false positive rate",ylab="true positive rate",main=testname[3])
    polygon(c(x3,rev(x3)),c(y_3l,rev(y_3u)),density = c(50, 50),angle = c(-45, 45),col="lightgrey")

    plot(x4,y_4m,type="l",xlim= c(0,1),ylim=c(0,1),col="blue",xlab="false positive rate",ylab="true positive rate",main=testname[4])
    polygon(c(x4,rev(x4)),c(y_4l,rev(y_4u)),density = c(50, 50),angle = c(-45, 45),col="lightgrey")

    plot(x1,y_1m,type="l",xlim= c(0,1),ylim=c(0,1),xlab="false positive rate",ylab="true positive rate")
    lines(x2,y_2m,type="l",xlim= c(0,1),ylim=c(0,1),lty = 2,col="blue")
    lines(x3,y_3m,type="l",xlim= c(0,1),ylim=c(0,1),lty = 3,col="green")
    lines(x4,y_4m,type="l",xlim= c(0,1),ylim=c(0,1),lty = 4,col="red")

    legend4<-c(paste("SROC for ",testname[1]),paste("SROC for ",testname[2]),paste("SROC for ",testname[3]),paste("SROC for ",testname[4]),paste("Estimated points of ",testname[1]),
               paste("Estimated points of ",testname[2]),paste("Estimated points of ",testname[3]),paste("Estimated points of ",testname[4]),
               paste("Pooled TP and FP for ",testname[1]),paste("Pooled TP and FP for ",testname[2]),paste("Pooled TP and FP for ",testname[3]),paste("Pooled TP and FP for ",testname[4]))
    points(1-stud.sp[-c(sapply(l[[1]],c)),1],stud.se[-c(sapply(l[[1]],c)),1],pch=1)
    points(1-stud.sp[-c(sapply(l[[2]],c)),2],stud.se[-c(sapply(l[[2]],c)),2],pch=2,col="blue")
    points(1-stud.sp[-c(sapply(l[[3]],c)),3],stud.se[-c(sapply(l[[3]],c)),3],pch=3,col="green")
    points(1-stud.sp[-c(sapply(l[[4]],c)),4],stud.se[-c(sapply(l[[4]],c)),4],pch=4,col="red")
    points(1-pool.sp,pool.se,pch=c(5,6,7,8),col=c("black","blue","green","red"))
    legend("bottomright",legend=legend4,lty=c(1,2,3,4,NA,NA,NA,NA,NA,NA,NA,NA),pch=c(NA,NA,NA,NA,1,2,3,4,5,6,7,8),
           col=c("black","blue","green","red","black","blue","green","red","black","blue","green","red"),cex = 0.35)
  }
  if(K==5){
    sp.range = matrix(c(range(stud.sp[,1]),range(stud.sp[,2]),range(stud.sp[,3]),range(stud.sp[,4])),ncol=K,nrow=2,byrow=F)
    alpha.post = rbind(c(samp[[1]][,c("mu[2]")]),
                      c(samp[[1]][,c("mu[4]")]),
                      c(samp[[1]][,c("mu[6]")]),
                      c(samp[[1]][,c("mu[8]")]),
                      c(samp[[1]][,c("mu[10]")]))
    rownames(alpha.post) = c("alpha1","alpha2","alpha3","alpha4","alpha5")
    beta.post = rbind(c(samp[[1]][,c("mu[3]")]),
                     c(samp[[1]][,c("mu[5]")]),
                     c(samp[[1]][,c("mu[7]")]),
                     c(samp[[1]][,c("mu[9]")]),
                     c(samp[[1]][,c("mu[11]")]))
    rownames(beta.post) = c("beta1","beta2","beta3","beta4","beta5")

    y_1l = vector()
    y_1u = vector()
    y_1m = vector()
    x1 = seq(1-sp.range[2,1],1-sp.range[1,1],length.out = 2000)
    for (i in 1:length(x1)){
      y_1l[i] = cb_h(x1[i],alpha.post[1,],beta.post[1,],cov[[14]],cov[[25]],0.025)
      y_1m[i] = cb_h(x1[i],alpha.post[1,],beta.post[1,],cov[[14]],cov[[25]],0.50)
      y_1u[i] = cb_h(x1[i],alpha.post[1,],beta.post[1,],cov[[14]],cov[[25]],0.975)
    }

    y_2l = vector()
    y_2u = vector()
    y_2m = vector()
    x2 = seq(1-sp.range[2,2],1-sp.range[1,2],length.out = 2000)
    for (i in 1:length(x2)){
      y_2l[i] = cb_h(x2[i],alpha.post[2,],beta.post[2,],cov[[38]],cov[[49]],0.025)
      y_2m[i] = cb_h(x2[i],alpha.post[2,],beta.post[2,],cov[[38]],cov[[49]],0.50)
      y_2u[i] = cb_h(x2[i],alpha.post[2,],beta.post[2,],cov[[38]],cov[[49]],0.975)
    }

    y_3l = vector()
    y_3u = vector()
    y_3m = vector()
    x3 = seq(1-sp.range[2,3],1-sp.range[1,3],length.out = 2000)
    for (i in 1:length(x3)){
      y_3l[i] = cb_h(x3[i],alpha.post[3,],beta.post[3,],cov[[62]],cov[[73]],0.025)
      y_3m[i] = cb_h(x3[i],alpha.post[3,],beta.post[3,],cov[[62]],cov[[73]],0.50)
      y_3u[i] = cb_h(x3[i],alpha.post[3,],beta.post[3,],cov[[62]],cov[[73]],0.975)
    }

    y_4l = vector()
    y_4u = vector()
    y_4m = vector()
    x4 = seq(1-sp.range[2,4],1-sp.range[1,4],length.out = 2000)
    for (i in 1:length(x4)){
      y_4l[i] = cb_h(x4[i],alpha.post[4,],beta.post[4,],cov[[86]],cov[[97]],0.025)
      y_4m[i] = cb_h(x4[i],alpha.post[4,],beta.post[4,],cov[[86]],cov[[97]],0.50)
      y_4u[i] = cb_h(x4[i],alpha.post[4,],beta.post[4,],cov[[86]],cov[[97]],0.975)
    }

    y_5l = vector()
    y_5u = vector()
    y_5m = vector()
    x5 = seq(1-sp.range[2,5],1-sp.range[1,5],length.out = 2000)
    for (i in 1:length(x5)){
      y_5l[i] = cb_h(x5[i],alpha.post[5,],beta.post[5,],cov[[110]],cov[[121]],0.025)
      y_5m[i] = cb_h(x5[i],alpha.post[5,],beta.post[5,],cov[[110]],cov[[121]],0.50)
      y_5u[i] = cb_h(x5[i],alpha.post[5,],beta.post[5,],cov[[110]],cov[[121]],0.975)
    }

    pdf(file.path(dirc,"sroc_hierarchical.pdf"),height=11,width=7)
    oldpar<-par(mfrow=c(3,2))
    on.exit(par(oldpar))
    plot(x1,y_1m,type="l",xlim= c(0,1),ylim=c(0,1),xlab="false positive rate",ylab="true positive rate",main=testname[1])
    polygon(c(x1,rev(x1)),c(y_1l,rev(y_1u)),density = c(50, 50),angle = c(-45, 45),col="lightgrey")

    plot(x2,y_2m,type="l",xlim= c(0,1),ylim=c(0,1),col="blue",xlab="false positive rate",ylab="true positive rate",main=testname[2])
    polygon(c(x2,rev(x2)),c(y_2l,rev(y_2u)),density = c(50, 50),angle = c(-45, 45),col="lightgrey")

    plot(x3,y_3m,type="l",xlim= c(0,1),ylim=c(0,1),col="blue",xlab="false positive rate",ylab="true positive rate",main=testname[3])
    polygon(c(x3,rev(x3)),c(y_3l,rev(y_3u)),density = c(50, 50),angle = c(-45, 45),col="lightgrey")

    plot(x4,y_4m,type="l",xlim= c(0,1),ylim=c(0,1),col="blue",xlab="false positive rate",ylab="true positive rate",main=testname[4])
    polygon(c(x4,rev(x4)),c(y_4l,rev(y_4u)),density = c(50, 50),angle = c(-45, 45),col="lightgrey")

    plot(x5,y_5m,type="l",xlim= c(0,1),ylim=c(0,1),col="blue",xlab="false positive rate",ylab="true positive rate",main=testname[5])
    polygon(c(x5,rev(x5)),c(y_5l,rev(y_5u)),density = c(50, 50),angle = c(-45, 45),col="lightgrey")
    plot(x1,y_1m,type="l",xlim= c(0,1),ylim=c(0,1),xlab="false positive rate",ylab="true positive rate")
    lines(x2,y_2m,type="l",xlim= c(0,1),ylim=c(0,1),lty = 2,col="blue")
    lines(x3,y_3m,type="l",xlim= c(0,1),ylim=c(0,1),lty = 3,col="green")
    lines(x4,y_4m,type="l",xlim= c(0,1),ylim=c(0,1),lty = 4,col="red")
    lines(x5,y_5m,type="l",xlim= c(0,1),ylim=c(0,1),lty = 5,col="yellow")

    legend5<-c(paste("SROC for ",testname[1]),paste("SROC for ",testname[2]),paste("SROC for ",testname[3]),paste("SROC for ",testname[4]),paste("SROC for ",testname[5]),paste("Estimated points of ",testname[1]),
               paste("Estimated points of ",testname[2]),paste("Estimated points of ",testname[3]),paste("Estimated points of ",testname[4]),paste("Estimated points of ",testname[5]),
               paste("Pooled TP and FP for ",testname[1]),paste("Pooled TP and FP for ",testname[2]),paste("Pooled TP and FP for ",testname[3]),paste("Pooled TP and FP for ",testname[4]),paste("Pooled TP and FP for ",testname[5]))
    points(1-stud.sp[-c(sapply(l[[1]],c)),1],stud.se[-c(sapply(l[[1]],c)),1],pch=1)
    points(1-stud.sp[-c(sapply(l[[2]],c)),2],stud.se[-c(sapply(l[[2]],c)),2],pch=2,col="blue")
    points(1-stud.sp[-c(sapply(l[[3]],c)),3],stud.se[-c(sapply(l[[3]],c)),3],pch=3,col="green")
    points(1-stud.sp[-c(sapply(l[[4]],c)),4],stud.se[-c(sapply(l[[4]],c)),4],pch=4,col="red")
    points(1-stud.sp[-c(sapply(l[[5]],c)),5],stud.se[-c(sapply(l[[5]],c)),5],pch=5,col="yellow")
    points(1-pool.sp,pool.se,pch=c(6,7,8,9,10),col=c("black","blue","green","red","yellow"))
    legend("bottomright",legend=legend5, lty=c(1,2,3,4,5,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),pch=c(NA,NA,NA,NA,NA,1,2,3,4,5,6,7,8,9,10),
           col=c("black","blue","green","red","yellow","black","blue","green","red","yellow","black","blue","green","red","yellow"),cex = 0.35)
  }
  dev.off()
}
