#' @import plotrix
#' @import grDevices
#' @import imguR
h_forestplot <- function(K, nstu, dat, testname, samp, dirc){
  index<-indicator(K,nstu,dat)
  summ<-summary(samp)
  result=summ[[2]]
  samp.mat<- result
  para_study_se = paralist("Se.stud",summ)
  para_study_sp = paralist("Sp.stud",summ)

  if(K==1){
    forest.se1 = samp.mat[para_study_se[1:nstu],c(1,3,5)]
    forest.sp1 = samp.mat[para_study_sp[1:nstu],c(1,3,5)]
    pool.se = result[c("post.Se[1]"),3]
    pool.sp = result[c("post.Sp[1]"),3]

    pdf(file.path(dirc,"forest_hierarchical.pdf"),height=11,width=7)
    oldpar<-par(mfrow=c(1,2))
    on.exit(par(oldpar))
    #T1 se
    plotCI(x=c(forest.se1[,2]),y=c(1:nstu),li=forest.se1[,1],ui=forest.se1[,3],xaxt="n",err="x",xlab=paste(testname[1], " Se(%)"),ylab="Study ID",slty=c(index[,1]),xlim=c(0,1))
    abline(v=pool.se[1],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
    axis(2,c(1:nstu),tick=TRUE)

    #T1 sp
    plotCI(x=c(forest.sp1[,2]),y=c(1:nstu),li=forest.sp1[,1],ui=forest.sp1[,3],xaxt="n",err="x",xlab=paste(testname[1], " Sp(%)"),ylab="Study ID",slty=c(index[,1]),xlim=c(0,1))
    abline(v=pool.sp[1],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)

  }

  if(K==2)
  {
    forest.se1 = samp.mat[para_study_se[1:nstu],c(1,3,5)]
    forest.se2 = samp.mat[para_study_se[-c(1:nstu)],c(1,3,5)]
    forest.sp1 = samp.mat[para_study_sp[1:nstu],c(1,3,5)]
    forest.sp2 = samp.mat[para_study_sp[-c(1:nstu)],c(1,3,5)]
    pool.se = result[c("post.Se[1]","post.Se[2]"),3]
    pool.sp = result[c("post.Sp[1]","post.Sp[2]"),3]

    pdf(file.path(dirc,"forest_hierarchical.pdf"),height=11,width=7)
    oldpar<-par(mfrow=c(2,2))
    on.exit(par(oldpar))
    #forest plot of T1 se
    plotCI(x=c(forest.se1[,2]),y=c(1:nstu),li=forest.se1[,1],ui=forest.se1[,3],xaxt="n",err="x",xlab=paste(testname[1], " Se(%)"),ylab="Study ID",slty=c(index[,1]),xlim=c(0,1))
    abline(v=pool.se[1],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
    axis(2,c(1:nstu),tick=TRUE)
    #T1 sp
    plotCI(x=c(forest.sp1[,2]),y=c(1:nstu),li=forest.sp1[,1],ui=forest.sp1[,3],xaxt="n",err="x",xlab=paste(testname[1], " Sp(%)"),ylab="Study ID",slty=c(index[,1]),xlim=c(0,1))
    abline(v=pool.sp[1],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
    #T2 se
    plotCI(x=forest.se2[,2],y=c(1:nstu),li=forest.se2[,1],ui=forest.se2[,3],xaxt="n",err="x",slty=c(index[,2]),xlab=paste(testname[2], " Se(%)"),ylab="Study ID",xlim=c(0,1))
    abline(v=pool.se[2],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
    #T2 sp
    plotCI(x=forest.sp2[,2],y=c(1:nstu),li=forest.sp2[,1],ui=forest.sp2[,3],xaxt="n",err="x",slty=c(index[,2]),
           xlab=paste(testname[2], " Sp(%)"),ylab="Study ID",xlim=c(0,1))
    abline(v=pool.sp[2],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
  }
  if(K==3)
  {
    forest.se1 = samp.mat[para_study_se[1:nstu],c(1,3,5)]
    forest.se2 = samp.mat[para_study_se[(nstu+1):(2*nstu)],c(1,3,5)]
    forest.se3 = samp.mat[para_study_se[(2*nstu+1):(3*nstu)],c(1,3,5)]
    forest.sp1 = samp.mat[para_study_sp[1:nstu],c(1,3,5)]
    forest.sp2 = samp.mat[para_study_sp[(nstu+1):(2*nstu)],c(1,3,5)]
    forest.sp3 = samp.mat[para_study_sp[(2*nstu+1):(3*nstu)],c(1,3,5)]
    pool.se = result[c("post.Se[1]","post.Se[2]","post.Se[3]"),3]
    pool.sp = result[c("post.Sp[1]","post.Sp[2]","post.Sp[3]"),3]

    pdf(file.path(dirc,"forest_hierarchical.pdf"),height=11,width=7)
    oldpar<-par(mfrow=c(3,2))
    on.exit(par(oldpar))
    #forest plot of T1 se
    plotCI(x=c(forest.se1[,2]),y=c(1:nstu),li=forest.se1[,1],ui=forest.se1[,3],xaxt="n",err="x",xlab=paste(testname[1], " Se(%)"),ylab="Study ID",slty=c(index[,1]),xlim=c(0,1))
    abline(v=pool.se[1],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","86","90","100"),tick=TRUE)
    axis(2,c(1:nstu),tick=TRUE)
    #T1 sp
    plotCI(x=c(forest.sp1[,2]),y=c(1:nstu),li=forest.sp1[,1],ui=forest.sp1[,3],xaxt="n",err="x",xlab=paste(testname[1], " Sp(%)"),ylab="Study ID",slty=c(index[,1]),xlim=c(0,1))
    abline(v=pool.sp[1],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
    #T2 se
    plotCI(x=forest.se2[,2],y=c(1:nstu),li=forest.se2[,1],ui=forest.se2[,3],xaxt="n",err="x",slty=c(index[,2]),xlab=paste(testname[2], " Se(%)"),ylab="Study ID",xlim=c(0,1))
    abline(v=pool.se[2],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
    #T2 sp
    plotCI(x=forest.sp2[,2],y=c(1:nstu),li=forest.sp2[,1],ui=forest.sp2[,3],xaxt="n",err="x",slty=c(index[,2]),
           xlab=paste(testname[2], " Sp(%)"),ylab="Study ID",xlim=c(0,1))
    abline(v=pool.sp[2],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
    #T3 se
    plotCI(x=forest.se3[,2],y=c(1:nstu),li=forest.se3[,1],ui=forest.se3[,3],xaxt="n",err="x",slty=c(index[,3]),xlab=paste(testname[3], " Se(%)"),ylab="Study ID",xlim=c(0,1))
    abline(v=pool.se[3],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
    #T3 sp
    plotCI(x=forest.sp3[,2],y=c(1:nstu),li=forest.sp3[,1],ui=forest.sp3[,3],xaxt="n",err="x",slty=c(index[,3]),
           xlab=paste(testname[3], " Sp(%)"),ylab="Study ID",xlim=c(0,1))
    abline(v=pool.sp[3],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
  }
  if(K==4)
  {
    forest.se1 = samp.mat[para_study_se[1:nstu],c(1,3,5)]
    forest.se2 = samp.mat[para_study_se[(nstu+1):(2*nstu)],c(1,3,5)]
    forest.se3 = samp.mat[para_study_se[(2*nstu+1):(3*nstu)],c(1,3,5)]
    forest.se4 = samp.mat[para_study_se[(3*nstu+1):(4*nstu)],c(1,3,5)]
    forest.sp1 = samp.mat[para_study_sp[1:nstu],c(1,3,5)]
    forest.sp2 = samp.mat[para_study_sp[(nstu+1):(2*nstu)],c(1,3,5)]
    forest.sp3 =samp.mat[para_study_sp[(2*nstu+1):(3*nstu)],c(1,3,5)]
    forest.sp4 =samp.mat[para_study_sp[(3*nstu+1):(4*nstu)],c(1,3,5)]
    pool.se = result[c("post.Se[1]","post.Se[2]","post.Se[3]","post.Se[4]"),3]
    pool.sp = result[c("post.Sp[1]","post.Sp[2]","post.Sp[3]","post.Sp[4]"),3]

    pdf(file.path(dirc,"forest_hierarchical.pdf"),height=11,width=7)
    oldpar<-par(mfrow=c(4,2))
    on.exit(par(oldpar))
    #forest plot of T1 se
    plotCI(x=c(forest.se1[,2]),y=c(1:nstu),li=forest.se1[,1],ui=forest.se1[,3],xaxt="n",err="x",xlab=paste(testname[1], " Se(%)"),ylab="Study ID",slty=c(index[,1]),xlim=c(0,1))
    abline(v=pool.se[1],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
    axis(2,c(1:nstu),tick=TRUE)
    #T1 sp
    plotCI(x=c(forest.sp1[,2]),y=c(1:nstu),li=forest.sp1[,1],ui=forest.sp1[,3],xaxt="n",err="x",xlab=paste(testname[1], " Sp(%)"),ylab="Study ID",slty=c(index[,1]),xlim=c(0,1))
    abline(v=pool.sp[1],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
    #T2 se
    plotCI(x=forest.se2[,2],y=c(1:nstu),li=forest.se2[,1],ui=forest.se2[,3],xaxt="n",err="x",slty=c(index[,2]),xlab=paste(testname[2], " Se(%)"),ylab="Study ID",xlim=c(0,1))
    abline(v=pool.se[2],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
    #T2 sp
    plotCI(x=forest.sp2[,2],y=c(1:nstu),li=forest.sp2[,1],ui=forest.sp2[,3],xaxt="n",err="x",slty=c(index[,2]),
           xlab=paste(testname[2], " Sp(%)"),ylab="Study ID",xlim=c(0,1))
    abline(v=pool.sp[2],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
    #T3 se
    plotCI(x=forest.se3[,2],y=c(1:nstu),li=forest.se3[,1],ui=forest.se3[,3],xaxt="n",err="x",slty=c(index[,3]),xlab=paste(testname[3], " Se(%)"),ylab="Study ID",xlim=c(0,1))
    abline(v=pool.se[3],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
    #T3 sp
    plotCI(x=forest.sp3[,2],y=c(1:nstu),li=forest.sp3[,1],ui=forest.sp3[,3],xaxt="n",err="x",slty=c(index[,3]),
           xlab=paste(testname[3], " Sp(%)"),ylab="Study ID",xlim=c(0,1))
    abline(v=pool.sp[3],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
    #T4 se
    plotCI(x=forest.se4[,2],y=c(1:nstu),li=forest.se4[,1],ui=forest.se4[,3],xaxt="n",err="x",slty=c(index[,4]),xlab=paste(testname[4], " Se(%)"),ylab="Study ID",xlim=c(0,1))
    abline(v=pool.se[4],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
    #T4 sp
    plotCI(x=forest.sp4[,2],y=c(1:nstu),li=forest.sp4[,1],ui=forest.sp4[,3],xaxt="n",err="x",slty=c(index[,4]),
           xlab=paste(testname[4], " Sp(%)"),ylab="Study ID",xlim=c(0,1))
    abline(v=pool.sp[4],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
  }
  if(K==5)
  {
    forest.se1 = samp.mat[para_study_se[1:nstu],c(1,3,5)]
    forest.se2 = samp.mat[para_study_se[(nstu+1):(2*nstu)],c(1,3,5)]
    forest.se3 = samp.mat[para_study_se[(2*nstu+1):(3*nstu)],c(1,3,5)]
    forest.se4 = samp.mat[para_study_se[(3*nstu+1):(4*nstu)],c(1,3,5)]
    forest.se5 = samp.mat[para_study_se[(4*nstu+1):(5*nstu)],c(1,3,5)]
    forest.sp1 = samp.mat[para_study_sp[1:nstu],c(1,3,5)]
    forest.sp2 = samp.mat[para_study_sp[(nstu+1):(2*nstu)],c(1,3,5)]
    forest.sp3 = samp.mat[para_study_sp[(2*nstu+1):(3*nstu)],c(1,3,5)]
    forest.sp4 = samp.mat[para_study_sp[(3*nstu+1):(4*nstu)],c(1,3,5)]
    forest.sp5 = samp.mat[para_study_sp[(4*nstu+1):(5*nstu)],c(1,3,5)]
    pool.se = result[c("post.Se[1]","post.Se[2]","post.Se[3]","post.Se[4]","post.Se[5]"),3]
    pool.sp = result[c("post.Sp[1]","post.Sp[2]","post.Sp[3]","post.Sp[4]","post.Sp[5]"),3]

    pdf(file.path(dirc,"forest_hierarchical.pdf"),height=11,width=7)
    oldpar<-par(mfrow=c(5,2))
    on.exit(par(oldpar))
    #forest plot of T1 se
    plotCI(x=c(forest.se1[,2]),y=c(1:nstu),li=forest.se1[,1],ui=forest.se1[,3],xaxt="n",err="x",xlab=paste(testname[1], " Se(%)"),ylab="Study ID",slty=c(index[,1]),xlim=c(0,1))
    abline(v=pool.se[1],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
    axis(2,c(1:nstu),tick=TRUE)
    #T1 sp
    plotCI(x=c(forest.sp1[,2]),y=c(1:nstu),li=forest.sp1[,1],ui=forest.sp1[,3],xaxt="n",err="x",xlab=paste(testname[1], " Sp(%)"),ylab="Study ID",slty=c(index[,1]),xlim=c(0,1))
    abline(v=pool.sp[1],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
    #T2 se
    plotCI(x=forest.se2[,2],y=c(1:nstu),li=forest.se2[,1],ui=forest.se2[,3],xaxt="n",err="x",slty=c(index[,2]),xlab=paste(testname[2], " Se(%)"),ylab="Study ID",xlim=c(0,1))
    abline(v=pool.se[2],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
    #T2 sp
    plotCI(x=forest.sp2[,2],y=c(1:nstu),li=forest.sp2[,1],ui=forest.sp2[,3],xaxt="n",err="x",slty=c(index[,2]),
           xlab=paste(testname[2], " Sp(%)"),ylab="Study ID",xlim=c(0,1))
    abline(v=pool.sp[2],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
    #T3 se
    plotCI(x=forest.se3[,2],y=c(1:nstu),li=forest.se3[,1],ui=forest.se3[,3],xaxt="n",err="x",slty=c(index[,3]),xlab=paste(testname[3], " Se(%)"),ylab="Study ID",xlim=c(0,1))
    abline(v=pool.se[3],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
    #T3 sp
    plotCI(x=forest.sp3[,2],y=c(1:nstu),li=forest.sp3[,1],ui=forest.sp3[,3],xaxt="n",err="x",slty=c(index[,3]),
           xlab=paste(testname[3], " Sp(%)"),ylab="Study ID",xlim=c(0,1))
    abline(v=pool.sp[3],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
    #T4 se
    plotCI(x=forest.se4[,2],y=c(1:nstu),li=forest.se4[,1],ui=forest.se4[,3],xaxt="n",err="x",slty=c(index[,4]),xlab=paste(testname[4], " Se(%)"),ylab="Study ID",xlim=c(0,1))
    abline(v=pool.se[4],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
    #T4 sp
    plotCI(x=forest.sp4[,2],y=c(1:nstu),li=forest.sp4[,1],ui=forest.sp4[,3],xaxt="n",err="x",slty=c(index[,4]),
           xlab=paste(testname[4], " Sp(%)"),ylab="Study ID",xlim=c(0,1))
    abline(v=pool.sp[4],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","82","90","100"),tick=TRUE)
    #T5 se
    plotCI(x=forest.se5[,2],y=c(1:nstu),li=forest.se5[,1],ui=forest.se5[,3],xaxt="n",err="x",slty=c(index[,5]),xlab=paste(testname[5], " Se(%)"),ylab="Study ID",xlim=c(0,1))
    abline(v=pool.se[5],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
    #T5 sp
    plotCI(x=forest.sp5[,2],y=c(1:nstu),li=forest.sp5[,1],ui=forest.sp5[,3],xaxt="n",err="x",slty=c(index[,5]),
           xlab=paste(testname[5], " Sp(%)"),ylab="Study ID",xlim=c(0,1))
    abline(v=pool.sp[5],lty=3)
    axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c("0","10","20","30","40","50","60","70","80","90","100"),tick=TRUE)
  }
  dev.off()
}
