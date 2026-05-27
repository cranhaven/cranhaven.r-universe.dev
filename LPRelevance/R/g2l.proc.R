g2l.proc <-
  function(X,z,X.target=NULL,z.target=NULL,m=c(4,6),alpha=.1,nbag=NULL,nsample=length(z),
           lp.reg.method='lm', null.scale='QQ',
           approx.method="direct",ngrid=2000,centering=TRUE, coef.smooth='BIC',
           fdr.method='locfdr', plot=TRUE, rel.null='custom',
           locfdr.df=10,fdr.th.fixed=NULL,parallel=FALSE,...){
    extraparms<-list(...)
    if(is.null(extraparms$k) & lp.reg.method=='knn'){
      extraparms$k<-sqrt(length(z))
    }

    X<-as.matrix(X)
    if(ncol(X)>1){plot.macro=FALSE}else{plot.macro=TRUE}

    tasks='macro'
    if(!is.null(X.target)){
      x0=matrix(X.target,ncol=ncol(X))
      z0=z.target
      ntarget<-length(z0)
      tasks='micro'
    }

    out<-list()
    ##for ggplot variables, avoiding CRAN check notes:
    x<-tags<-score<-value<-variable<-ystart<-yend<-NULL

    ##determine whether use g2l.infer.boot
    iter.flag=0
    if(!is.null(nbag)){if(nbag>=2){iter.flag=1}}

    for(tid in 1:length(tasks)){
      #print(tid)
      t_name<-tasks[tid]
      if(t_name=='macro'){
        ####macro.ensemble results####
        if(iter.flag==0){
          g2l_res<-g2l.infer(X,z,m=m,X.test=NULL,alpha=alpha,nsample=length(z), lp.reg.method=lp.reg.method,
                             fdr.curve.approx=approx.method,null.scale=null.scale,
                             ngrid=ngrid,centering=centering,fdr.method=fdr.method, locfdr.df=locfdr.df,coef.smooth=coef.smooth,
                             fdr.th.fixed=fdr.th.fixed,rel.null=rel.null,parallel=parallel,k=extraparms$k)
        }else{
          g2l_res<-g2l.infer.boot(X,z,m=m,X.test=NULL,alpha=alpha,B=nbag,nsample=length(z),
                                  lp.reg.method=lp.reg.method,fdr.curve.approx=approx.method,null.scale=null.scale,
                                  ngrid=ngrid,centering=centering, locfdr.df=locfdr.df,coef.smooth=coef.smooth,
                                  fdr.th.fixed=fdr.th.fixed,rel.null=rel.null,parallel=parallel,k=extraparms$k)
        }
        macro.out<-list()
        macro.out$result<-g2l_res$fdr.z
        macro.out$result$signal<-g2l_res$data$rej

        if(plot==TRUE & plot.macro==TRUE){
          nrej=sum(macro.out$result$signal)
          if(fdr.method=='locfdr'){
            d0<-data.frame(x=X,z=z,tags=factor(macro.out$result$signal,levels=c(0,1),ordered=TRUE),score=-log10(macro.out$result$prob_null))
          }else{
            d0<-data.frame(x=X,z=z,tags=factor(macro.out$result$signal,levels=c(0,1),ordered=TRUE))
          }

          p_dscv<-ggplot2::ggplot(data=d0,aes(x=x,y=z,color=tags))+geom_point(size=.8)+
            ylab('score')+xlab('Covariate X')+ggtitle(paste0('Discoveries: ', nrej))+
            scale_color_manual(values=c('lightblue','red'))+
            theme(text=element_text(size=13),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.text.x = element_text(size=16),
                  axis.text.y = element_text(size=14),
                  legend.position="none",
                  legend.title=element_blank())

          macro.out$plots<-list(signal_x=p_dscv)

          if(fdr.method=='locfdr'){
            p<-ggplot2::ggplot(data=d0,aes(x=x,y=z,color=score))+geom_point(size=.8)+scale_color_gradient2(low=rgb(0.4,0.4,0.4,.4),mid='red',high='yellow',midpoint=3)+
              ylab('Z-values')+xlab('Covariate X')+ggtitle('Discovery Propensity Scores by Sample')+
              theme(text=element_text(size=13),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.text.x = element_text(size=16),
                    axis.text.y = element_text(size=14),
                    legend.position="right",
                    legend.title=element_blank())
            p1<-ggplot2::ggplot(data=d0,aes(x=x,y=score,color=score))+geom_point(size=.8)+scale_color_gradient2(low=rgb(0.4,0.4,0.4,.4),mid='red',high='yellow',midpoint=3)+
              ylab('score')+xlab('Covariate X')+ggtitle('Discovery Propensity Scores by Covariate')+
              theme(text=element_text(size=13),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.text.x = element_text(size=16),
                    axis.text.y = element_text(size=14),
                    legend.position="none",
                    legend.title=element_blank())

            macro.out$plots$dps_xz<-p
            macro.out$plots$dps_x<-p1
          }

        }

        out[[tid]]<-macro.out
        names(out)[tid]<-'macro'

      }else if(t_name=='micro'&!is.null(X.target)){
        ####individual inference results####
        if(iter.flag==0){
          g2l_res<-g2l.infer(X,z,m=m,X.test=x0,alpha=alpha,nsample=length(z),
                             lp.reg.method=lp.reg.method,fdr.curve.approx=approx.method, null.scale=null.scale,
                             ngrid=ngrid,centering=centering,fdr.method=fdr.method, locfdr.df=locfdr.df,
                             fdr.th.fixed=fdr.th.fixed,rel.null=rel.null,parallel=parallel,k=extraparms$k)
        }else{
          g2l_res<-g2l.infer.boot(X,z,m=m,X.test=x0,alpha=alpha,nsample=length(z),
                                  lp.reg.method=lp.reg.method,B=nbag, fdr.curve.approx=approx.method,
                                  null.scale=null.scale,
                                  ngrid=ngrid,centering=centering,locfdr.df=locfdr.df,
                                  fdr.th.fixed=fdr.th.fixed,rel.null=rel.null,parallel=parallel,k=extraparms$k)
        }
        w <- locfdr(z,bre=200,df=10,nulltype=1,plot=0)
        fdrpool<-approxfun(z,w$fdr,rule=2,method='linear')
        micro.out<-list()
        micro.out$result<-data.frame(x=x0,z=z0,prob.null.est=NA,prob.null.sd=NA,signal=NA)
        micro.out$global<-data.frame(x=x0,z=z0,prob.null.est=fdrpool(z0))
        micro.out$result$signal=rep(0,ntarget)
        for(i in 1:ntarget){
          if(iter.flag==0){
            micro.out$result$prob.null.est[i]=getNullProb(g2l_res,x0[i,],z0[i])
          }else{
            micro.out$result[i,c('prob.null.est','prob.null.sd')]=getNullProb(g2l_res,x0[i,],z0[i])
          }
          micro.out$result$signal[i]=(micro.out$result$prob.null.est[i]<g2l_res$lpfdr.th$ti[i])
        }


        if(plot==TRUE){
          plots<-list()
          for(i in 1:ntarget){
            zgrid<-g2l_res$z.grid[i,]
            if(iter.flag==0){
              fdr.c<-g2l_res$lpfdr[i,]
            }else{
              fdr.c<-apply(g2l_res$lpfdr[i,,],2,FUN=mean)
            }

            Dat.fdr<-data.frame(zgrid=zgrid,Global=fdrpool(zgrid),Customized=fdr.c)

            d0<-reshape2::melt(Dat.fdr,id.vars='zgrid')

            z.ind<-which(apply(X,1,function(x) all(x==x0[i,])))
            if(length(z.ind)>0){
              segticks<-data.frame(x=z[z.ind],ystart=-.08,yend=-.03)
            }else{
              segticks<-NULL
            }
            if(ncol(x0)>1){
              y.label=paste0('Pr(Null|z,x=[',paste(x0[i,],collapse=','),'])')
            }else{
              y.label=paste0('Pr(Null|z,x=',x0[i,],')')
            }

            p_fdr<-ggplot2::ggplot(data=d0,aes(x=zgrid,y=value))+geom_line(size=.8,aes(linetype =variable,color=variable))+
              ylab(y.label)+xlab('Z')+ggtitle('')+
              scale_color_manual(breaks=c('Global','Customized'),values=c('blue','red'))+
              scale_linetype_manual(breaks=c('Global','Customized'),values=c(2,1))+
              scale_y_continuous(breaks=c(0,.25,.5,.75,1),limits=c(-.1,1.2))+
              geom_hline(yintercept=0)+geom_vline(xintercept=0)+geom_hline(yintercept=0.2,color='darkred',linetype=3)+
              theme(text=element_text(size=13),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.text.x = element_text(size=16),
                    axis.text.y = element_text(size=14),
                    legend.position="right",
                    legend.title=element_blank())

            if(!is.null(segticks)){
              p_fdr<-p_fdr+ggplot2::geom_segment(data=segticks,aes(x=x,y=ystart,xend=x,yend=yend),color='lightblue')
            }

            plots[[i]]<-p_fdr
            names(plots)[i]<-paste0('fdr.',i)
          }

          micro.out$plots<-plots

        }
        out[[tid]]<-micro.out
        names(out)[tid]<-'micro'
      }

    }
    m.lp<-g2l_res$lp.m
    out$m.lp=m.lp
    return(out)

  }

