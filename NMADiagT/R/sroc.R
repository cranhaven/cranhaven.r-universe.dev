#' @import graphics
#' @import grDevices
#' @import imguR
sroc = function(K,nstu,samp,dat,testname, dirc){
  index<-indicator(K,nstu,dat)
  l<-except(index,K,nstu)
  summ<-summary(samp)
  result<-summary(samp)[[2]]
  para_study_se = paralist("stud.se",summ)
  para_study_sp = paralist("stud.sp",summ)
  stud.se = matrix(result[para_study_se,3],ncol=K,nrow=nstu,byrow=F)
  stud.sp = matrix(result[para_study_sp,3],ncol=K,nrow=nstu,byrow=F)
  if(K==1){
    sp.range = matrix(c(range(stud.sp[,1])),ncol=K,nrow=2,byrow=F)
    pool.se = result[c("post.se[1]"),3]
    pool.sp = result[c("post.sp[1]"),3]
    beta.post = rbind(c(samp[[1]][,c("beta[1]")],samp[[2]][,"beta[1]"],samp[[3]][,"beta[1]"]))
    rownames(beta.post) = c("beta1")
    mu2.post = rbind(c(samp[[1]][,c("mu2[1]")],samp[[2]][,"mu2[1]"],samp[[3]][,"mu2[1]"]))
    rownames(mu2.post) = c("mu21")

    y_1l = vector()
    y_1u = vector()
    y_1m = vector()

    x1 = seq(1-sp.range[2,1],1-sp.range[1,1],length.out = 2000)
    for (i in 1:length(x1)){
      y_1l[i] = cb(x1[i],beta.post[1,],mu2.post[1,],0.025)
      y_1m[i] = cb(x1[i],beta.post[1,],mu2.post[1,],0.50)
      y_1u[i] = cb(x1[i],beta.post[1,],mu2.post[1,],0.975)
    }
    pdf(file.path(dirc,"sroc_hsroc.pdf"),height=11,width=7)
    oldpar<-par(mfcol=c(1,2))
    on.exit(par(oldpar))
    plot(x1,y_1m,type="l",xlim= c(0,1),ylim=c(0,1),xlab="false positive rate",ylab="true positive rate",main=testname[1])
    polygon(c(x1,rev(x1)),c(y_1l,rev(y_1u)),density = c(50, 50),angle = c(-45, 45),col="lightgrey")

    legend1<-c(paste("SROC for ",testname[1]),paste("Estimated points of ",testname[1]),paste("Pooled TP and FP for ",testname[1]))
    plot(x1,y_1m,type="l",xlim= c(0,1),ylim=c(0,1),xlab="false positive rate",ylab="true positive rate")
    points(1-stud.sp[-c(sapply(l[[1]],c)),1],stud.se[-c(sapply(l[[1]],c)),1],pch=1,col="black")
    points(1-pool.sp,pool.se,pch=c(3),col=c("black"))
    legend("bottomright",legend=legend1,lty=c(1,NA,NA),pch=c(NA,1,3),col=c("black","black","black"),cex = 0.55)
  }
  if(K==2){
    sp.range = matrix(c(range(stud.sp[,1]),range(stud.sp[,2])),ncol=K,nrow=2,byrow=F)
    pool.se = result[c("post.se[1]","post.se[2]"),3]
    pool.sp = result[c("post.sp[1]","post.sp[2]"),3]
    beta.post = rbind(c(samp[[1]][,c("beta[1]")],samp[[2]][,"beta[1]"],samp[[3]][,"beta[1]"]),
                    c(samp[[1]][,c("beta[2]")],samp[[2]][,"beta[2]"],samp[[3]][,"beta[2]"]))
    rownames(beta.post) = c("beta1","beta2")
    mu2.post = rbind(c(samp[[1]][,c("mu2[1]")],samp[[2]][,"mu2[1]"],samp[[3]][,"mu2[1]"]),
                  c(samp[[1]][,c("mu2[2]")],samp[[2]][,"mu2[2]"],samp[[3]][,"mu2[2]"]))
    rownames(mu2.post) = c("mu21","mu22")

    y_1l = vector()
    y_1u = vector()
    y_1m = vector()
    x1 = seq(1-sp.range[2,1],1-sp.range[1,1],length.out = 2000)
    for (i in 1:length(x1)){
      y_1l[i] = cb(x1[i],beta.post[1,],mu2.post[1,],0.025)
      y_1m[i] = cb(x1[i],beta.post[1,],mu2.post[1,],0.50)
      y_1u[i] = cb(x1[i],beta.post[1,],mu2.post[1,],0.975)
    }

    y_2l = vector()
    y_2u = vector()
    y_2m = vector()
    x2 = seq(1-sp.range[2,2],1-sp.range[1,2],length.out = 2000)
    for (i in 1:length(x2)){
      y_2l[i] = cb(x2[i],beta.post[2,],mu2.post[2,],0.025)
      y_2m[i] = cb(x2[i],beta.post[2,],mu2.post[2,],0.50)
      y_2u[i] = cb(x2[i],beta.post[2,],mu2.post[2,],0.975)
    }
    pdf(file.path(dirc,"sroc_hsroc.pdf"),height=11,width=7)
    oldpar<-par(mfrow=c(2,2))
    on.exit(par(oldpar))
    plot(x1,y_1m,type="l",xlim= c(0,1),ylim=c(0,1),xlab="false positive rate",ylab="true positive rate",main=testname[1])
    polygon(c(x1,rev(x1)),c(y_1l,rev(y_1u)),density = c(50, 50),angle = c(-45, 45),col="lightgrey")

    plot(x2,y_2m,type="l",xlim= c(0,1),ylim=c(0,1),col="blue",xlab="false positive rate",ylab="true positive rate",main=testname[2])
    polygon(c(x2,rev(x2)),c(y_2l,rev(y_2u)),density = c(50, 50),angle = c(-45, 45),col="lightgrey")

    plot(x1,y_1m,type="l",xlim= c(0,1),ylim=c(0,1),xlab="false positive rate",ylab="true positive rate")
    lines(x2,y_2m,type="l",xlim= c(0,1),ylim=c(0,1),lty = 2,col="blue")

    legend2<-c(paste("SROC for ",testname[1]),paste("SROC for ",testname[2]),paste("Estimated points of ",testname[1]),paste("Estimated points of ",testname[2]),paste("Pooled TP and FP for ",testname[1]),paste("Pooled TP and FP for ",testname[2]))
    points(1-stud.sp[-c(sapply(l[[1]],c)),1],stud.se[-c(sapply(l[[1]],c)),1],pch=1)
    points(1-stud.sp[-c(sapply(l[[2]],c)),2],stud.se[-c(sapply(l[[2]],c)),2],pch=2,col="blue")
    points(1-pool.sp,pool.se,pch=c(3,4),col=c("black","blue"))
    legend("bottomright",legend=legend2,lty=c(1,2,NA,NA,NA,NA),pch=c(NA,NA,1,2,3,4),col=c("black","blue","black","blue","black","blue"),cex = 0.35)
  }
  if(K==3){
    sp.range = matrix(c(range(stud.sp[,1]),range(stud.sp[,2]),range(stud.sp[,3])),ncol=K,nrow=2,byrow=F)
    pool.se = result[c("post.se[1]","post.se[2]","post.se[3]"),3]
    pool.sp = result[c("post.sp[1]","post.sp[2]","post.sp[3]"),3]
    beta.post = rbind(c(samp[[1]][,c("beta[1]")],samp[[2]][,"beta[1]"],samp[[3]][,"beta[1]"]),
                    c(samp[[1]][,c("beta[2]")],samp[[2]][,"beta[2]"],samp[[3]][,"beta[2]"]),
                    c(samp[[1]][,c("beta[3]")],samp[[2]][,"beta[3]"],samp[[3]][,"beta[3]"])
                    )
    rownames(beta.post) = c("beta1","beta2","beta3")
    mu2.post = rbind(c(samp[[1]][,c("mu2[1]")],samp[[2]][,"mu2[1]"],samp[[3]][,"mu2[1]"]),
                   c(samp[[1]][,c("mu2[2]")],samp[[2]][,"mu2[2]"],samp[[3]][,"mu2[2]"]),
                   c(samp[[1]][,c("mu2[3]")],samp[[2]][,"mu2[3]"],samp[[3]][,"mu2[3]"])
                  )
    rownames(mu2.post) = c("mu21","mu22","mu23")

    y_1l = vector()
    y_1u = vector()
    y_1m = vector()
    x1 = seq(1-sp.range[2,1],1-sp.range[1,1],length.out = 2000)
    for (i in 1:length(x1)){
      y_1l[i] = cb(x1[i],beta.post[1,],mu2.post[1,],0.025)
      y_1m[i] = cb(x1[i],beta.post[1,],mu2.post[1,],0.50)
      y_1u[i] = cb(x1[i],beta.post[1,],mu2.post[1,],0.975)
      }

    y_2l = vector()
    y_2u = vector()
    y_2m = vector()
    x2 = seq(1-sp.range[2,2],1-sp.range[1,2],length.out = 2000)
    for (i in 1:length(x2)){
      y_2l[i] = cb(x2[i],beta.post[2,],mu2.post[2,],0.025)
      y_2m[i] = cb(x2[i],beta.post[2,],mu2.post[2,],0.50)
      y_2u[i] = cb(x2[i],beta.post[2,],mu2.post[2,],0.975)
    }

    y_3l = vector()
    y_3u = vector()
    y_3m = vector()
    x3 = seq(1-sp.range[2,3],1-sp.range[1,3],length.out = 2000)
    for (i in 1:length(x3)){
      y_3l[i] = cb(x3[i],beta.post[3,],mu2.post[3,],0.025)
      y_3m[i] = cb(x3[i],beta.post[3,],mu2.post[3,],0.50)
      y_3u[i] = cb(x3[i],beta.post[3,],mu2.post[3,],0.975)
    }
    pdf(file.path(dirc,"sroc_hsroc.pdf"),height=11,width=7)
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
    legend("bottomright",legend=legend3,lty=c(1,2,3,NA,NA,NA,NA,NA,NA),pch=c(NA,NA,NA,1,2,3,4,5,6),col=c("black","blue","green","black","blue","green","black","blue","green"),cex = 0.35)
  }
  if(K==4){
    sp.range = matrix(c(range(stud.sp[,1]),range(stud.sp[,2]),range(stud.sp[,3]),range(stud.sp[,4])),ncol=K,nrow=2,byrow=F)
    beta.post = rbind(c(samp[[1]][,c("beta[1]")],samp[[2]][,"beta[1]"],samp[[3]][,"beta[1]"]),
                    c(samp[[1]][,c("beta[2]")],samp[[2]][,"beta[2]"],samp[[3]][,"beta[2]"]),
                    c(samp[[1]][,c("beta[3]")],samp[[2]][,"beta[3]"],samp[[3]][,"beta[3]"]),
                    c(samp[[1]][,c("beta[4]")],samp[[2]][,"beta[4]"],samp[[3]][,"beta[4]"]))
    rownames(beta.post) = c("beta1","beta2","beta3","beta4")
    mu2.post = rbind(c(samp[[1]][,c("mu2[1]")],samp[[2]][,"mu2[1]"],samp[[3]][,"mu2[1]"]),
                   c(samp[[1]][,c("mu2[2]")],samp[[2]][,"mu2[2]"],samp[[3]][,"mu2[2]"]),
                   c(samp[[1]][,c("mu2[3]")],samp[[2]][,"mu2[3]"],samp[[3]][,"mu2[3]"]),
                   c(samp[[1]][,c("mu2[4]")],samp[[2]][,"mu2[4]"],samp[[3]][,"mu2[4]"]))
    rownames(mu2.post) = c("mu21","mu22","mu23","mu24")

    y_1l = vector()
    y_1u = vector()
    y_1m = vector()
    x1 = seq(1-sp.range[2,1],1-sp.range[1,1],length.out = 2000)
    for (i in 1:length(x1)){
      y_1l[i] = cb(x1[i],beta.post[1,],mu2.post[1,],0.025)
      y_1m[i] = cb(x1[i],beta.post[1,],mu2.post[1,],0.50)
      y_1u[i] = cb(x1[i],beta.post[1,],mu2.post[1,],0.975)
    }

    y_2l = vector()
    y_2u = vector()
    y_2m = vector()
    x2 = seq(1-sp.range[2,2],1-sp.range[1,2],length.out = 2000)
    for (i in 1:length(x2)){
      y_2l[i] = cb(x2[i],beta.post[2,],mu2.post[2,],0.025)
      y_2m[i] = cb(x2[i],beta.post[2,],mu2.post[2,],0.50)
      y_2u[i] = cb(x2[i],beta.post[2,],mu2.post[2,],0.975)
      }

      y_3l = vector()
      y_3u = vector()
      y_3m = vector()
      x3 = seq(1-sp.range[2,3],1-sp.range[1,3],length.out = 2000)
      for (i in 1:length(x3)){
        y_3l[i] = cb(x3[i],beta.post[3,],mu2.post[3,],0.025)
        y_3m[i] = cb(x3[i],beta.post[3,],mu2.post[3,],0.50)
        y_3u[i] = cb(x3[i],beta.post[3,],mu2.post[3,],0.975)
        }

      y_4l = vector()
      y_4u = vector()
      y_4m = vector()
      x4 = seq(1-sp.range[2,4],1-sp.range[1,4],length.out = 2000)
      for (i in 1:length(x4)){
        y_4l[i] = cb(x4[i],beta.post[4,],mu2.post[4,],0.025)
        y_4m[i] = cb(x4[i],beta.post[4,],mu2.post[4,],0.50)
        y_4u[i] = cb(x4[i],beta.post[4,],mu2.post[4,],0.975)
      }
      pdf(file.path(dirc,"sroc_hsroc.pdf"),height=11,width=7)
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
    beta.post = rbind(c(samp[[1]][,c("beta[1]")],samp[[2]][,"beta[1]"],samp[[3]][,"beta[1]"]),
                      c(samp[[1]][,c("beta[2]")],samp[[2]][,"beta[2]"],samp[[3]][,"beta[2]"]),
                      c(samp[[1]][,c("beta[3]")],samp[[2]][,"beta[3]"],samp[[3]][,"beta[3]"]),
                      c(samp[[1]][,c("beta[4]")],samp[[2]][,"beta[4]"],samp[[3]][,"beta[4]"]),
                      c(samp[[1]][,c("beta[5]")],samp[[2]][,"beta[5]"],samp[[3]][,"beta[5]"]))
    rownames(beta.post) = c("beta1","beta2","beta3","beta4","beta5")
    mu2.post = rbind(c(samp[[1]][,c("mu2[1]")],samp[[2]][,"mu2[1]"],samp[[3]][,"mu2[1]"]),
                     c(samp[[1]][,c("mu2[2]")],samp[[2]][,"mu2[2]"],samp[[3]][,"mu2[2]"]),
                     c(samp[[1]][,c("mu2[3]")],samp[[2]][,"mu2[3]"],samp[[3]][,"mu2[3]"]),
                     c(samp[[1]][,c("mu2[4]")],samp[[2]][,"mu2[4]"],samp[[3]][,"mu2[4]"]),
                     c(samp[[1]][,c("mu2[5]")],samp[[2]][,"mu2[5]"],samp[[3]][,"mu2[5]"]))
    rownames(mu2.post) = c("mu21","mu22","mu23","mu24","mu25")

    y_1l = vector()
    y_1u = vector()
    y_1m = vector()
    x1 = seq(1-sp.range[2,1],1-sp.range[1,1],length.out = 2000)
    for (i in 1:length(x1)){
      y_1l[i] = cb(x1[i],beta.post[1,],mu2.post[1,],0.025)
      y_1m[i] = cb(x1[i],beta.post[1,],mu2.post[1,],0.50)
      y_1u[i] = cb(x1[i],beta.post[1,],mu2.post[1,],0.975)
    }

    y_2l = vector()
    y_2u = vector()
    y_2m = vector()
    x2 = seq(1-sp.range[2,2],1-sp.range[1,2],length.out = 2000)
    for (i in 1:length(x2)){
      y_2l[i] = cb(x2[i],beta.post[2,],mu2.post[2,],0.025)
      y_2m[i] = cb(x2[i],beta.post[2,],mu2.post[2,],0.50)
      y_2u[i] = cb(x2[i],beta.post[2,],mu2.post[2,],0.975)
    }

    y_3l = vector()
    y_3u = vector()
    y_3m = vector()
    x3 = seq(1-sp.range[2,3],1-sp.range[1,3],length.out = 2000)
    for (i in 1:length(x3)){
      y_3l[i] = cb(x3[i],beta.post[3,],mu2.post[3,],0.025)
      y_3m[i] = cb(x3[i],beta.post[3,],mu2.post[3,],0.50)
      y_3u[i] = cb(x3[i],beta.post[3,],mu2.post[3,],0.975)
    }

    y_4l = vector()
    y_4u = vector()
    y_4m = vector()
    x4 = seq(1-sp.range[2,4],1-sp.range[1,4],length.out = 2000)
    for (i in 1:length(x4)){
      y_4l[i] = cb(x4[i],beta.post[4,],mu2.post[4,],0.025)
      y_4m[i] = cb(x4[i],beta.post[4,],mu2.post[4,],0.50)
      y_4u[i] = cb(x4[i],beta.post[4,],mu2.post[4,],0.975)
    }

    y_5l = vector()
    y_5u = vector()
    y_5m = vector()
    x5 = seq(1-sp.range[2,5],1-sp.range[1,5],length.out = 2000)
    for (i in 1:length(x4)){
      y_5l[i] = cb(x5[i],beta.post[5,],mu2.post[5,],0.025)
      y_5m[i] = cb(x5[i],beta.post[5,],mu2.post[5,],0.50)
      y_5u[i] = cb(x5[i],beta.post[5,],mu2.post[5,],0.975)
    }
    pdf(file.path(dirc,"sroc_hsroc.pdf"),height=11,width=7)
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
