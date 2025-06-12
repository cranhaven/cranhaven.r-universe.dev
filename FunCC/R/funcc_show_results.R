#' @title Plotting co-clustering results of funCC
#' @description funcc_show_results graphically shows the results of the bi-clustering
#' @export
#' @param fun_mat The data array (n x m x T) where each entry corresponds to the measure of one observation i, i=1,...,n, for a functional variable m, m=1,...,p, at point t, t=1,...,T
#' @param res_input An object produced by the funcc_biclust function
#' @param only.mean logicol: if True only the template functions for each bi-cluster is displayed
#' @param aligned logicol: if True the alignemd functions are displayed
#' @param warping logicol: if True also a figure representing the warping functions are displayed
#' @return a figure representing each bi-cluster in terms of functions contained in it or templates
#' @examples  
#' data("funCCdata")
#' res <- funcc_biclust(funCCdata,delta=10,theta=1,alpha=1,beta=0,const_alpha=TRUE)
#' funcc_show_results(funCCdata,res)

funcc_show_results <- function(fun_mat,res_input,only.mean=FALSE,aligned=FALSE,warping=FALSE){
  
  variable <- value <- obs <- cluster <- obj <- NULL

  res = res_input[[1]]
  alpha=res_input$parameter$alpha
  beta=res_input$parameter$beta
  const_alpha=res_input$parameter$const_alpha
  const_beta=res_input$parameter$const_beta
  shift.alignement=as.character(res_input$parameter$shift.alignement)
  shift.max=res_input$parameter$shift.max
  max.iter=res_input$parameter$max.iter
  template.type=as.character(res_input$parameter$template.type)

  if(shift.alignement==F & (aligned==T|warping==T)){
    warning('Warning: no aligned can be performed if results are without alignment')
    aligned=F
    warping=F
  }
  if(aligned==F & warping==T){
    warning('Warning: no warping can be shown if aligned is False')
    warping=F
  }


  if(res@Number==0){
    stop('Warning: no Cluster found')
    }

  col_palette = c(RColorBrewer::brewer.pal(9, 'Set1'),
                  RColorBrewer::brewer.pal(12, 'Set3'),
                  RColorBrewer::brewer.pal(8, 'Set2'),
                  RColorBrewer::brewer.pal(8, 'Accent'),

                  RColorBrewer::brewer.pal(8, 'Dark2'),
                  RColorBrewer::brewer.pal(9, 'PiYG'),
                  RColorBrewer::brewer.pal(9, 'PuOr'),
                  RColorBrewer::brewer.pal(9, 'RdBu'),
                  RColorBrewer::brewer.pal(9, 'PRGn'),
                  RColorBrewer::brewer.pal(9, 'Pastel1'),
                  RColorBrewer::brewer.pal(8, 'Pastel2'),
                  RColorBrewer::brewer.pal(9, 'BrBG'),
                  RColorBrewer::brewer.pal(11, 'Spectral'),
                  RColorBrewer::brewer.pal(8,'RdGy'),
                  RColorBrewer::brewer.pal(8,'RdBu'),
                  RColorBrewer::brewer.pal(8,'PRGn'),
                  RColorBrewer::brewer.pal(8,'PiYG'),
                  RColorBrewer::brewer.pal(8,'Oranges'),
                  RColorBrewer::brewer.pal(8,'Blues'),
                  RColorBrewer::brewer.pal(8,'RdGy'),
                  RColorBrewer::brewer.pal(8,'Greens'),
                  RColorBrewer::brewer.pal(6,'BuPu'),
                  RColorBrewer::brewer.pal(6,'BuGn'),
                  RColorBrewer::brewer.pal(9,'Paired'),
                  RColorBrewer::brewer.pal(9,'Paired'),
                  RColorBrewer::brewer.pal(9,'Paired'),
                  RColorBrewer::brewer.pal(9,'Paired'),
                  RColorBrewer::brewer.pal(9,'Paired'))

  x_y=matrix(0,nrow=dim(fun_mat)[1],ncol=dim(fun_mat)[2])


  if(res@Number==1){#x_y=t(rep.row(c(res@RowxNumber),ncol(fun_mat)))*rep.row(c(res@NumberxCol),nrow(fun_mat))}
    x_y=t(matrix(base::rep(c(res@RowxNumber),each=ncol(fun_mat)),nrow=ncol(fun_mat)))*matrix(base::rep(c(res@NumberxCol),each=nrow(fun_mat)),nrow=nrow(fun_mat))}
  if(res@Number>1){
    for( i in 1:res@Number){
      #xy=t(rep.row(res@RowxNumber[,i],ncol(fun_mat)))*rep.row(res@NumberxCol[i,],nrow(fun_mat))
      xy=t(matrix(base::rep(res@RowxNumber[,i],each=ncol(fun_mat)),nrow=ncol(fun_mat)))*matrix(base::rep(res@NumberxCol[i,],each=nrow(fun_mat)),nrow=nrow(fun_mat))
      xy =xy*i
      x_y=x_y+xy

    }
  }

  if(sum(x_y==0)>0){col_palette <- c('grey',col_palette)}


  if(aligned){
    if(col_palette[1]=='grey'){col_palette <- col_palette[-1]}
    fun_aligned = data.frame()
    template=data.frame()
    template.mean=data.frame()
    warping_aligned=data.frame()

    if(res@Number==1){
      clust_cl=array(fun_mat[which(c(res@RowxNumber)==TRUE),which(c(res@NumberxCol)==TRUE),],dim=c(sum(res@RowxNumber),sum(res@NumberxCol),dim(fun_mat)[3]))
      res_aligned=warping_function_plot(res,clust_cl,template.type,alpha,beta,const_alpha,const_beta,shift.alignement,shift.max, max.iter)

      array_aligned=res_aligned$fun_mat_align
      mat_aligned=NULL
      for( tt in 1:dim(array_aligned)[1]){
        mat_aligned=rbind(mat_aligned,array_aligned[tt,,])
      }
      fun_aligned_cl=data.frame(mat_aligned)
      fun_aligned_cl$cluster = 1

      res_aligned_array <- res_aligned$template
      temp_cl=NULL
      for( tt in 1:dim(array_aligned)[1]){
        temp_cl=rbind(temp_cl,res_aligned_array[tt,,])
      }
      temp_cl=data.frame(temp_cl)
      template.mean_cl=data.frame(t(data.frame(colMeans(temp_cl))))
      temp_cl$cluster = 1
      template.mean_cl$cluster = 1

      #temp_cl$variable = paste0('X',seq(1,dim(fun_mat)[3]))

      fun_aligned = rbind(fun_aligned,fun_aligned_cl)
      template = rbind(template,temp_cl)
      template.mean = rbind(template.mean,template.mean_cl)


      warping_cl = data.frame(res_aligned$x.out)
      warping_cl$cluster = 1
      warping_aligned = rbind(warping_aligned,warping_cl)
    }

    if(res@Number>1){
      for(cl in 1:res@Number){
        clust_cl=array(fun_mat[which(res@RowxNumber[,cl]==TRUE),which(res@NumberxCol[cl,]==TRUE),],dim=c(sum(res@RowxNumber[,cl]),sum(res@NumberxCol[cl,]),dim(fun_mat)[3]))
        res_aligned=warping_function_plot(res,clust_cl,template.type,alpha,beta,const_alpha,const_beta,shift.alignement,shift.max, max.iter)

        array_aligned=res_aligned$fun_mat_align
        mat_aligned=NULL
        for( tt in 1:dim(array_aligned)[1]){
          mat_aligned=rbind(mat_aligned,array_aligned[tt,,])
        }
        fun_aligned_cl=data.frame(mat_aligned)
        fun_aligned_cl$cluster = cl

        res_aligned_array <- res_aligned$template
        temp_cl=NULL
        for( tt in 1:dim(array_aligned)[1]){
          temp_cl=rbind(temp_cl,res_aligned_array[tt,,])
        }
        temp_cl=data.frame(temp_cl)
        template.mean_cl=data.frame(t(data.frame(colMeans(temp_cl))))
        temp_cl$cluster = cl
        template.mean_cl$cluster = cl

        #temp_cl$variable = paste0('X',seq(1,dim(fun_mat)[3]))

        fun_aligned = rbind(fun_aligned,fun_aligned_cl)
        template = rbind(template,temp_cl)
        template.mean = rbind(template.mean,template.mean_cl)

        warping_cl = data.frame(res_aligned$x.out)
        warping_cl$cluster = cl
        warping_aligned = rbind(warping_aligned,warping_cl)

      }
    }

    fun_aligned$obs <- paste0('ROW',row.names(fun_aligned))
    fun_aligned = reshape::melt(fun_aligned,id.vars=c("obs","cluster"))
    fun_aligned$variable <- as.numeric(gsub('X','',fun_aligned$variable))

    template$obs <- paste0('ROW',row.names(template))
    template = reshape::melt(template,id.vars=c("obs","cluster"))
    template$variable <- as.numeric(gsub('X','',template$variable))

    template.mean$obs <- paste0('ROW',row.names(template.mean))
    template.mean = reshape::melt(template.mean,id.vars=c("obs","cluster"))
    template.mean$variable <- as.numeric(gsub('X','',template.mean$variable))
    
    #

    # plot
    if(only.mean & warping){
      if(alpha==0 & beta==0){
        grDevices::dev.new()
        g <- ggplot2::ggplot(template,ggplot2::aes(x=variable,#factor(variable,levels = paste0("X",seq(1:dim(fun_mat)[3])))
                                 y=value,group=obs,col=factor(cluster))) + ggplot2::facet_wrap(~factor(cluster)) +
          ggplot2::geom_line(size=1.2) +
          ggplot2::theme(legend.position = 'none') + ggplot2::scale_color_manual(values=col_palette) + ggplot2::ggtitle(label='Representative functions per cluster')+
          ggplot2::xlab('') + ggplot2::ylab('')
        print(g)
      }
      if(alpha!=0 | beta!=0){
        grDevices::dev.new()
        g <- ggplot2::ggplot(template,ggplot2::aes(x=variable,
                                 #factor(variable,levels = paste0("X",seq(1:dim(fun_mat)[3])))
                                 y=value,group=obs,col=factor(cluster))) + ggplot2::facet_wrap(~factor(cluster)) +
          ggplot2::geom_line(size=1.2) +
          ggplot2::geom_line(data=template.mean,mapping=ggplot2::aes(x=variable,y=value,group=factor(cluster)),col='black',size=1.2)+
          ggplot2::theme(legend.position = 'none') + ggplot2::scale_color_manual(values=col_palette) + ggplot2::ggtitle(label='Representative functions per cluster')+
          ggplot2::xlab('') + ggplot2::ylab('')
        print(g)
      }

      warping_aligned$obs <- paste0('ROW',row.names(warping_aligned))
      warping_aligned = reshape::melt(warping_aligned,id.vars=c("obs","cluster"))
      warping_aligned$variable <- as.numeric(gsub('X','',warping_aligned$variable))
      
      grDevices::dev.new()
      g1 <- ggplot2::ggplot(warping_aligned,ggplot2::aes(x=variable,#factor(variable),
                                       y=value,group=obs,col=factor(cluster))) + ggplot2::geom_line()+
        ggplot2::facet_wrap(~factor(cluster)) + ggplot2::theme(legend.position = 'none') + ggplot2::scale_color_manual(values=col_palette) + ggplot2::ggtitle(label='Warping functions per cluster')+
        ggplot2::xlab('') + ggplot2::ylab('')
      print(g1)
    }

    else if(only.mean & !warping){
      if(alpha==0 & beta==0){
        grDevices::dev.new()
        g <- ggplot2::ggplot(template,ggplot2::aes(x=variable,#factor(variable,levels = paste0("X",seq(1:dim(fun_mat)[3]))),
                                 y=value,group=obs,col=factor(cluster))) + ggplot2::facet_wrap(~factor(cluster)) +
          ggplot2::geom_line(size=1.2) +
          ggplot2::theme(legend.position = 'none') + ggplot2::scale_color_manual(values=col_palette) + ggplot2::ggtitle(label='Representative functions per cluster')+
          ggplot2::xlab('') + ggplot2::ylab('')
        print(g)
      }
      if(alpha!=0 | beta!=0){
        grDevices::dev.new()
        g <- ggplot2::ggplot(template,ggplot2::aes(x=variable,#factor(variable,levels = paste0("X",seq(1:dim(fun_mat)[3]))),
                                 y=value,group=obs,col=factor(cluster))) + ggplot2::facet_wrap(~factor(cluster)) +
          ggplot2::geom_line(size=1.2) +
          ggplot2::geom_line(data=template.mean,mapping=ggplot2::aes(x=variable,y=value,group=factor(cluster)),col='black',size=1.2)+
          ggplot2::theme(legend.position = 'none') + ggplot2::scale_color_manual(values=col_palette) + ggplot2::ggtitle(label='Representative functions per cluster')+
          ggplot2::xlab('') + ggplot2::ylab('')
        print(g)
      }

    }

    else if(!only.mean & warping){
      g <- ggplot2::ggplot(fun_aligned,ggplot2::aes(x=variable,#factor(variable),
                                  y=value,group=obs,col=factor(cluster))) + ggplot2::geom_line()+
        #geom_line(data=template,mapping=aes(x=factor(variable),y=res_aligned.template,group=cluster),col='black',size=1)+
        ggplot2::facet_wrap(~factor(cluster)) + ggplot2::theme(legend.position = 'none') + ggplot2::scale_color_manual(values=col_palette) + ggplot2::ggtitle(label='Aligned functions per cluster')+
        ggplot2::xlab('') + ggplot2::ylab('')
      grDevices::dev.new()
      print(g)

      warping_aligned$obs <- paste0('ROW',row.names(warping_aligned))
      warping_aligned = reshape::melt(warping_aligned,id.vars=c("obs","cluster"))
      warping_aligned$variable <- as.numeric(gsub('X','',warping_aligned$variable))
      

      g1 <- ggplot2::ggplot(warping_aligned,ggplot2::aes(x=variable,#factor(variable),
                                       y=value,group=obs,col=factor(cluster))) + ggplot2::geom_line()+
        ggplot2::facet_wrap(~factor(cluster)) + ggplot2::theme(legend.position = 'none') + ggplot2::scale_color_manual(values=col_palette) + ggplot2::ggtitle(label='Warping functions per bi-cluster')+
        ggplot2::xlab('') + ggplot2::ylab('')
      grDevices::dev.new()
      print(g1)
    }

    else if(!only.mean & !warping){
      g <- ggplot2::ggplot(fun_aligned,ggplot2::aes(x=variable,#factor(variable),
                                  y=value,group=obs,col=factor(cluster))) + ggplot2::geom_line()+
        #geom_line(data=template,mapping=aes(x=factor(variable),y=res_aligned.template,group=cluster),col='black',size=1)+
        ggplot2::facet_wrap(~factor(cluster)) + ggplot2::theme(legend.position = 'none') + ggplot2::scale_color_manual(values=col_palette) + ggplot2::ggtitle(label='Aligned functions per bi-cluster')+
        ggplot2::xlab('') + ggplot2::ylab('')
      grDevices::dev.new()
      print(g)
    }


  }


  else{
    fun_plot=NULL
    for(j in 1:dim(fun_mat)[1]){
      fun_plot=rbind(fun_plot,fun_mat[j,,])
      #rownames(fun_plot) <- rownames(fun_mat)[j]
    }

    data_frame <- data.frame(fun_plot)
    data_frame$obs <- base::rep(paste0('ROW ',seq(1,nrow(fun_mat))),each=ncol(fun_mat))
    data_frame$var <- base::rep(paste0('COL ',seq(1,ncol(fun_mat))),nrow(fun_mat))

    data_frame$obj <- paste0(data_frame$obs,data_frame$var)
    data_frame$obj <- factor(data_frame$obj,levels=unique(paste0(data_frame$obs,data_frame$var)))

    data_frame <- reshape::melt(data_frame,id.vars = c('obs','var','obj'))
    data_frame <- data_frame[order(data_frame$obj),]

    x_y=matrix(0,nrow=dim(fun_mat)[1],ncol=dim(fun_mat)[2])
    if(res@Number==1){#x_y=t(rep.row(c(res@RowxNumber),ncol(fun_mat)))*rep.row(c(res@NumberxCol),nrow(fun_mat))}
      x_y=t(matrix(base::rep(c(res@RowxNumber),each=ncol(fun_mat)),nrow=ncol(fun_mat)))*matrix(base::rep(c(res@NumberxCol),each=nrow(fun_mat)),nrow=nrow(fun_mat))}
    if(res@Number>1){
      i=1
      for( i in 1:res@Number){
        #xy=t(rep.row(res@RowxNumber[,i],ncol(fun_mat)))*rep.row(res@NumberxCol[i,],nrow(fun_mat))
        xy=t(matrix(base::rep(res@RowxNumber[,i],each=ncol(fun_mat)),nrow=ncol(fun_mat)))*matrix(base::rep(res@NumberxCol[i,],each=nrow(fun_mat)),nrow=nrow(fun_mat))
        xy =xy*i
        x_y=x_y+xy

      }
    }

    x_y=data.frame(x_y)
    x_y$obs=paste0("ROW ",seq(1:nrow(fun_mat)))
    names(x_y)=c(paste0("COL ",seq(1:ncol(fun_mat))),"obs")
    x_y=reshape::melt(x_y,id.vars=c("obs"))

    x_y$obs <- factor(x_y$obs,levels=paste0("ROW ",seq(1:nrow(fun_mat)))[nrow(fun_mat):1])
    x_y$value <- factor(x_y$value,levels=as.character(seq(0,max(x_y$value))))

    x_y$obj <- paste0(x_y$obs,x_y$variable)

    x_y <- x_y[c('obj','value')]
    names(x_y) <- c('obj','cluster')

    data_frame_cl <- merge(data_frame,x_y,by='obj')
    data_frame_cl$variable <- as.numeric(gsub('X','',data_frame_cl$variable))

    template=data.frame()
    template.mean = data.frame()

    if(res@Number==1){
      clust_cl=array(fun_mat[which(c(res@RowxNumber)==T),which(c(res@NumberxCol)==T),],dim=c(sum(res@RowxNumber),sum(res@NumberxCol),dim(fun_mat)[3]))
      if(template.type=='mean'){new_fun_cl <- template_evaluation(clust_cl,alpha,beta,const_alpha,const_beta)}
      if(template.type=='medoid'){new_fun_cl <- medoid_evaluation(clust_cl,alpha,beta,const_alpha,const_beta)}
      #new_fun_cl=template_evaluation(clust_cl,a,b,const_a,const_b)

      temp_cl=NULL
      for(tt in 1:dim(new_fun_cl)[1]){
        temp_cl=rbind(temp_cl,new_fun_cl[tt,,])
      }
      temp_cl=data.frame(temp_cl)
      template.mean_cl=data.frame(t(data.frame(colMeans(temp_cl))))
      temp_cl$cluster = 1
      template = rbind(template,temp_cl)

      template.mean_cl$cluster = 1
      template.mean = rbind(template.mean,template.mean_cl)


    }
    if(res@Number>1){
      for(cl in 1:res@Number){
        clust_cl=array(fun_mat[which(res@RowxNumber[,cl]==T),which(res@NumberxCol[cl,]==T),],dim=c(sum(res@RowxNumber[,cl]),sum(res@NumberxCol[cl,]),dim(fun_mat)[3]))
        
        # new_fun_cl=template_evaluation(clust_cl,a,b,const_a,const_b)

        if(template.type=='mean'){new_fun_cl <- template_evaluation(clust_cl,alpha,beta,const_alpha,const_beta)}
        if(template.type=='medoid'){new_fun_cl <- medoid_evaluation(clust_cl,alpha,beta,const_alpha,const_beta)}

        temp_cl=NULL
        for(tt in 1:dim(new_fun_cl)[1]){
          temp_cl=rbind(temp_cl,new_fun_cl[tt,,])
        }
        temp_cl=data.frame(temp_cl)
        template.mean_cl=data.frame(t(data.frame(colMeans(temp_cl))))
        temp_cl$cluster = cl
        template = rbind(template,temp_cl)

        template.mean_cl$cluster = cl
        template.mean = rbind(template.mean,template.mean_cl)

      }
    }

    template$obs <- paste0('ROW',row.names(template))
    template = reshape::melt(template,id.vars=c("obs","cluster"))
    template$variable <- as.numeric(gsub('X','',template$variable))

    template.mean$obs <- paste0('ROW',row.names(template.mean))
    template.mean = reshape::melt(template.mean,id.vars=c("obs","cluster"))
    template.mean$variable <- as.numeric(gsub('X','',template.mean$variable))

    #

    if(only.mean){
      if(col_palette[1]=='grey'){col_palette <- col_palette[-1]}
      if(alpha==0 & beta==0){
        grDevices::dev.new()
        g <- ggplot2::ggplot(template,ggplot2::aes(x=variable,#factor(variable),
                                 y=value,group=obs,col=factor(cluster))) + ggplot2::facet_wrap(~factor(cluster)) +
          ggplot2::geom_line(size=1.2) +
          ggplot2::theme(legend.position = 'none') + ggplot2::scale_color_manual(values=col_palette) + ggplot2::ggtitle(label='Representative functions per bi-cluster')+
          ggplot2::xlab('') + ggplot2::ylab('')
        print(g)
      }
      if(alpha!=0 | beta!=0){
        grDevices::dev.new()
        g <- ggplot2::ggplot(template,ggplot2::aes(x=variable,#factor(variable),
                                 y=value,group=obs,col=factor(cluster))) + ggplot2::facet_wrap(~factor(cluster)) +
          ggplot2::geom_line(size=1.2) +
          ggplot2::geom_line(data=template.mean,mapping=ggplot2::aes(x=variable,y=value,group=factor(cluster)),col='black',size=1.2)+
          ggplot2::theme(legend.position = 'none') + ggplot2::scale_color_manual(values=col_palette) + ggplot2::ggtitle(label='Representative functions per bi-cluster')+
          ggplot2::xlab('') + ggplot2::ylab('')
        print(g)
      }


    }

    else{
      grDevices::dev.new()
      g <- ggplot2::ggplot(data_frame_cl,ggplot2::aes(x=variable,#factor(variable),
                                    y=value,group=obj,col=factor(cluster))) + ggplot2::geom_line()+
        #geom_line(data=template,mapping=aes(x=factor(variable),y=value,group=cluster),col='black',size=1)+
        ggplot2::facet_wrap(~factor(cluster)) + ggplot2::theme(legend.position = 'none') + ggplot2::scale_color_manual(values=col_palette) + ggplot2::ggtitle(label='Functions per bi-cluster')+
        ggplot2::xlab('') + ggplot2::ylab('')
      print(g)
    }
  }


}
