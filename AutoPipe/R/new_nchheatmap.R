


new_nchheatmap<-function(ordert_genes,col="RdBu",labRow = NULL,cexRoww = NULL, cexColl = NULL,
         labCol = NULL, main = NULL, xlab = NULL, ylab = NULL,sil_width=NULL,
         samples_data=NULL,genes_to_print=5,print_genes=FALSE,
         list_of_genes=NULL,plot_mean_sil=FALSE, sil_mean=NULL,GSE=FALSE,topPaths=5,db="c2"){

  ################################ formatting the layout
  cluster_number<-length(ordert_genes)
  if( (is.null(ordert_genes)) & (cluster_number<2) ){
    stop("Not enough clusters")
  }

  ####### get the names of the samples
  sample_names<-colnames(ordert_genes[[1]])
  nc<-ncol(ordert_genes[[1]])
  ######  layout for heatmap with nothing else
  lmat<-do.call(rbind,lapply(1:length(ordert_genes), function(i){
    return(c(0,i,0))
  }))
  lwid <- c(0.5, 4,0.5)
  lhei <- c(1)

  ###### layout for heatmap + sill. width
  if( (!print_genes) & (is.null(samples_data))  & (!plot_mean_sil) & (!GSE)){
    print("Use Layout Format 1")
    lmat<-do.call(rbind,lapply(1:(cluster_number+1), function(i){
      return(c(0,i,0))
    }))
    lwid <- c(0.5, 4,0.5)
    lhei <- c(1,rep(1,times=cluster_number))

  }

  ###### layout for heatmap + sill. width + Mean sil
  if( (!print_genes) & (is.null(samples_data))  & (plot_mean_sil) & (!GSE)){
    print("Use Layout Format 2")
    lmat<-do.call(rbind,lapply(1:(cluster_number+1), function(i){
      return(c(0,i,0))
    }))
    lmat[1,3]<-(cluster_number+2)
    lwid <- c(0.5, 4,2)
    lhei <- c(1,rep(1,times=cluster_number))

  }

  ###### layout for heatmap + sill. width + clinical data
  if( (!print_genes) & (!is.null(samples_data))  & (!plot_mean_sil) & (!GSE)){
    print("Use Layout Format 3")
    lmat<-do.call(rbind,lapply(1:(cluster_number+1), function(i){
      return(c(0,i,0))
    }))
    num_of_data<-ncol(samples_data)
    lmat<-rbind(lmat,do.call(rbind,lapply((cluster_number+2):((cluster_number+2)+2*num_of_data), function(i){
      return(c(0,i,0))
    })))
    lwid <- c(0.5, 4,0.5)
    lhei <- c(1,rep(1,times=cluster_number),rep(0.25,times=num_of_data),rep(0.5,times=num_of_data))

  }

  ###### layout for heatmap +  clinical data + Mean sil width
  if( (!print_genes) & (!is.null(samples_data))  & (plot_mean_sil) & (!GSE)){
    print("Use Layout Format 4")
    lmat<-do.call(rbind,lapply(1:(cluster_number+1), function(i){
      return(c(0,i,0))
    }))
    num_of_data<-ncol(samples_data)
    lmat<-rbind(lmat,do.call(rbind,lapply((cluster_number+2):((cluster_number+1)+2*num_of_data), function(i){
      return(c(0,i,0))
    })))
    lmat[1,3]<-((cluster_number+2)+2*num_of_data)
    lwid <- c(0.5, 4,2)
    lhei <- c(1,rep(1,times=cluster_number),rep(0.25,times=num_of_data),rep(0.5,times=num_of_data))

  }



  ###### layout for heatmap + sill. width + clinical data + genes
  if( (print_genes) & (!is.null(samples_data))  & (!plot_mean_sil) & (!GSE)){
    print("Use Layout Format 5")
    lmat<-do.call(rbind,lapply(1:(cluster_number+1), function(i){
      return(c(0,i,0))
    }))
    num_of_data<-ncol(samples_data)
    lmat<-rbind(lmat,do.call(rbind,lapply((cluster_number+2):((cluster_number+1)+2*num_of_data), function(i){
      return(c(0,i,0))
    })))
    lmat[2:(cluster_number+1),3]<-((cluster_number+2*num_of_data)+2): (((cluster_number+2*num_of_data)+1)+cluster_number)
    lwid <- c(0.5, 4,2)
    lhei <- c(1,rep(1,times=cluster_number),rep(0.25,times=num_of_data),rep(0.5,times=num_of_data))

  }

  ###### layout for heatmap + sill. width + clinical data + genes + Mean Sil width

  if( (print_genes) & (!is.null(samples_data)) & (plot_mean_sil) & (GSE==F)){
    print("Use Layout Format 6")
    lmat<-do.call(rbind,lapply(1:(cluster_number+1), function(i){
      return(c(0,i,0))
    }))
    num_of_data<-ncol(samples_data)
    lmat<-rbind(lmat,do.call(rbind,lapply((cluster_number+2):((cluster_number+1)+2*num_of_data), function(i){
      return(c(0,i,0))
    })))
    lmat[2:(cluster_number+1),3]<-((cluster_number+2*num_of_data)+2): (((cluster_number+2*num_of_data)+1)+cluster_number)
    lmat[1,3]<-(((cluster_number+2*num_of_data)+1)+cluster_number)+1
    lwid <- c(0.5, 4,2)
    lhei <- c(1,rep(1,times=cluster_number),rep(0.25,times=num_of_data),rep(0.5,times=num_of_data))


  }

  ###### layout for heatmap + sill. width + without clinical data + genes + Mean Sil width


  if( (print_genes) & (is.null(samples_data))  & (plot_mean_sil) & (GSE==F)){
    print("Use Layout Format 7")
    lmat<-do.call(rbind,lapply(1:(cluster_number+1), function(i){
      return(c(0,i,0))
    }))
    lmat[2:(cluster_number+1),3]<-((cluster_number)+2): (((cluster_number)+1)+cluster_number)
    lmat[1,3]<-(((cluster_number)+1)+cluster_number)+1
    lwid <- c(0.5, 4,2)
    lhei <- c(1,rep(1,times=cluster_number))


  }

  ###### layout for heatmap + sill. width + clinical data + genes + Mean Sil width +GSE

  if( (print_genes==F) & (!is.null(samples_data))& (plot_mean_sil) & (GSE==T)){
    print("Use Layout Format 8")
    lmat<-do.call(rbind,lapply(1:(cluster_number+1), function(i){
      return(c(0,i,0,0,0,0))
    }))
    num_of_data<-ncol(samples_data)
    lmat<-rbind(lmat,do.call(rbind,lapply((cluster_number+2):((cluster_number+1)+2*num_of_data), function(i){
      return(c(0,i,0,0,0,0))
    })))
    lmat[2:(cluster_number+1),3]<-((cluster_number+2*num_of_data)+2): (((cluster_number+2*num_of_data)+1)+cluster_number)
    lmat[2:(cluster_number+1),4]<-((cluster_number+2*num_of_data)+2): (((cluster_number+2*num_of_data)+1)+cluster_number)
    lmat[2:(cluster_number+1),5]<-((cluster_number+2*num_of_data)+2): (((cluster_number+2*num_of_data)+1)+cluster_number)
    lmat[1,3]<-(((cluster_number+2*num_of_data)+1)+cluster_number)+1
    lwid <- c(0.5, 4,2)
    lhei <- c(1,rep(1,times=cluster_number),rep(0.25,times=num_of_data),rep(0.5,times=num_of_data))


  }

  ###### layout for heatmap  clinical data  +GSE

  if( (print_genes==F) & (!is.null(samples_data))& (!plot_mean_sil) & (GSE==T)){
    print("Use Layout Format 9")
    lmat<-do.call(rbind,lapply(1:(cluster_number+1), function(i){
      return(c(0,i,0,0,0,0))
    }))
    num_of_data<-ncol(samples_data)
    lmat<-rbind(lmat,do.call(rbind,lapply((cluster_number+2):((cluster_number+1)+2*num_of_data), function(i){
      return(c(0,i,0,0,0,0))
    })))
    lmat[2:(cluster_number+1),3]<-((cluster_number+2*num_of_data)+2): (((cluster_number+2*num_of_data)+1)+cluster_number)
    lmat[2:(cluster_number+1),4]<-((cluster_number+2*num_of_data)+2): (((cluster_number+2*num_of_data)+1)+cluster_number)
    lmat[2:(cluster_number+1),5]<-((cluster_number+2*num_of_data)+2): (((cluster_number+2*num_of_data)+1)+cluster_number)
    lmat[1,3]<-(((cluster_number+2*num_of_data)+1)+cluster_number)+1
    lwid <- c(0.5, 4,2)
    lhei <- c(1,rep(1,times=cluster_number),rep(0.25,times=num_of_data),rep(0.5,times=num_of_data))


  }



  ###### layout for heatmap + sill. width  data + genes + Mean Sil width


  if( (print_genes==F) & (is.null(samples_data))  & (plot_mean_sil) & (GSE==T)){
    print("Use Layout Format 10")
    lmat<-do.call(rbind,lapply(1:(cluster_number+1), function(i){
      return(c(0,i,0,0,0,0))
    }))
    lmat[2:(cluster_number+1),3]<-((cluster_number)+2): (((cluster_number)+1)+cluster_number)
    lmat[2:(cluster_number+1),4]<-((cluster_number)+2): (((cluster_number)+1)+cluster_number)
    lmat[2:(cluster_number+1),5]<-((cluster_number)+2): (((cluster_number)+1)+cluster_number)
    lmat[1,3]<-(((cluster_number)+1)+cluster_number)+1
    lwid <- c(0.5, 4,2)
    lhei <- c(1,rep(1,times=cluster_number))


  }



  ###### layout for heatmap + sill. width + GSE + without Mean Sil width without clinical data

  if( (print_genes==F) & (is.null(samples_data)) & (plot_mean_sil) & (GSE==T)){
    print("Use Layout Format 11")
    lmat<-do.call(rbind,lapply(1:(cluster_number+1), function(i){
      return(c(0,i,0,0,0,0))
    }))
    lmat[2:(cluster_number+1),3]<-((cluster_number)+2): (((cluster_number)+1)+cluster_number)
    lmat[2:(cluster_number+1),4]<-((cluster_number)+2): (((cluster_number)+1)+cluster_number)
    lmat[2:(cluster_number+1),5]<-((cluster_number)+2): (((cluster_number)+1)+cluster_number)
    lmat[1,3]<-(((cluster_number)+1)+cluster_number)+1
    lwid <- c(0.5, 4,2)
    lhei <- c(1,rep(1,times=cluster_number))


  }



  ###### layout for heatmap + sill. width + GSE + Mean Sil width without clinical data

  if( (print_genes==F) & (is.null(samples_data))  & (!plot_mean_sil) & (GSE==T)){
    print("Use Layout Format 12")
    lmat<-do.call(rbind,lapply(1:(cluster_number+1), function(i){
      return(c(0,i,0,0,0,0))
    }))
    lmat[2:(cluster_number+1),3]<-((cluster_number)+2): (((cluster_number)+1)+cluster_number)
    lmat[2:(cluster_number+1),4]<-((cluster_number)+2): (((cluster_number)+1)+cluster_number)
    lmat[2:(cluster_number+1),5]<-((cluster_number)+2): (((cluster_number)+1)+cluster_number)
    lmat[1,3]<-(((cluster_number)+1)+cluster_number)+1
    lwid <- c(0.5, 4,2)
    lhei <- c(1,rep(1,times=cluster_number))


  }




  ###### layout for heatmap + sill. width + clinical data + genes + Mean Sil width + GSE Analysis

  if( (print_genes) & (!is.null(samples_data))
      &(plot_mean_sil) & (GSE)){
    print("Use Layout Format 13")
    lmat<-do.call(rbind,lapply(1:(cluster_number+1), function(i){
      return(c(0,i,0,0,0,0,0))
    }))
    #add for sample data
    num_of_data<-ncol(samples_data)
    lmat<-rbind(lmat,do.call(rbind,lapply((cluster_number+2):((cluster_number+1)+2*num_of_data), function(i){
      return(c(0,i,0,0,0,0,0))
    })))
    
    
    
    ##Genes
    lmat[2:(cluster_number+1),3]<-((cluster_number+2*num_of_data)+2): (((cluster_number+2*num_of_data)+1)+cluster_number)
    ##GSE
    lmat[2:(cluster_number+1),4]<-(((cluster_number+2*num_of_data)+2)+cluster_number): (((cluster_number+2*num_of_data)+1)+cluster_number*2)
    lmat[2:(cluster_number+1),5]<-(((cluster_number+2*num_of_data)+2)+cluster_number): (((cluster_number+2*num_of_data)+1)+cluster_number*2)
    lmat[2:(cluster_number+1),6]<-(((cluster_number+2*num_of_data)+2)+cluster_number): (((cluster_number+2*num_of_data)+1)+cluster_number*2)
    lmat[1,3]<-(((cluster_number+2*num_of_data)+2)+cluster_number*2)
    
    #add legende for sample data
    #start_nr=(((cluster_number+2*num_of_data)+2)+cluster_number*2)
    #for(i in 1:2*num_of_data){
     # lmat[i+(1+cluster_number),3]=start_nr+i
    #}
    
    
    lwid <- c(0.5, 4,2)
    lhei <- c(1,rep(1,times=cluster_number),rep(0.25,times=num_of_data),rep(0.5,times=num_of_data))

  }


  ###### layout for heatmap + sill. width + clinical data + genes + without Mean Sil width + GSE Analysis

  if( (print_genes) & (!is.null(samples_data))
      &(!plot_mean_sil) & (GSE)){
    print("Use Layout Format 14")
    lmat<-do.call(rbind,lapply(1:(cluster_number+1), function(i){
      return(c(0,i,0,0,0,0,0))
    }))
    num_of_data<-ncol(samples_data)
    lmat<-rbind(lmat,do.call(rbind,lapply((cluster_number+2):((cluster_number+1)+2*num_of_data), function(i){
      return(c(0,i,0,0,0,0,0))
    })))
    ##Genes
    lmat[2:(cluster_number+1),3]<-((cluster_number+2*num_of_data)+2): (((cluster_number+2*num_of_data)+1)+cluster_number)
    ## GSE
    lmat[2:(cluster_number+1),4]<-(((cluster_number+2*num_of_data)+2)+cluster_number): (((cluster_number+2*num_of_data)+1)+cluster_number*2)
    lmat[2:(cluster_number+1),5]<-(((cluster_number+2*num_of_data)+2)+cluster_number): (((cluster_number+2*num_of_data)+1)+cluster_number*2)
    lmat[2:(cluster_number+1),6]<-(((cluster_number+2*num_of_data)+2)+cluster_number): (((cluster_number+2*num_of_data)+1)+cluster_number*2)

    lwid <- c(0.5, 4,2)
    lhei <- c(1,rep(1,times=cluster_number),rep(0.25,times=num_of_data),rep(0.5,times=num_of_data))

  }


  ###### layout for heatmap + sill. width +without  clinical data + genes + without Mean Sil width + GSE Analysis

  if( (print_genes) & (is.null(samples_data))
      &(!plot_mean_sil) & (GSE)){
    print("Use Layout Format 15")
    lmat<-do.call(rbind,lapply(1:(cluster_number+1), function(i){
      return(c(0,i,0,0,0,0,0))
    }))
    ##genes
    lmat[2:(cluster_number+1),3]<-((cluster_number)+2): (((cluster_number)+1)+cluster_number)
    ## GSE
    lmat[2:(cluster_number+1),4]<-(((cluster_number)+2)+cluster_number): (((cluster_number)+1)+cluster_number*2)
    lmat[2:(cluster_number+1),5]<-(((cluster_number)+2)+cluster_number): (((cluster_number)+1)+cluster_number*2)
    lmat[2:(cluster_number+1),6]<-(((cluster_number)+2)+cluster_number): (((cluster_number)+1)+cluster_number*2)

    lwid <- c(0.5, 4,2)
    lhei <- c(1,rep(1,times=cluster_number))

  }

####### with sil mean , with sil width  without clinical data with genes with GSE

  if( (print_genes) & (is.null(samples_data))
      &(plot_mean_sil) & (GSE)){
    # lmat<-do.call(rbind,lapply(1:(cluster_number+1), function(i){
    #   return(c(0,i,0,0,0))
    # }))
    # lmat[2:(cluster_number+1),3]<-((cluster_number)+2): (((cluster_number)+1)+cluster_number)
    # lmat[2:(cluster_number+1),4]<-(((cluster_number)+2)+cluster_number): (((cluster_number)+1)+cluster_number*2)
    # lmat[1,3]<-(((cluster_number)+2)+cluster_number*2)
    # lwid <- c(0.5, 4,2,6,2)
    # lhei <- c(1,rep(1,times=cluster_number))


    lmat<-do.call(rbind,lapply(1:(cluster_number+1), function(i){
      return(c(0,i,0,0,0,0,0))
    }))
    #Text
    print("Use Layout Format 17")
    lmat[2:(cluster_number+1),3]<-((cluster_number)+2): (((cluster_number)+1)+cluster_number)
    #GSEA
    lmat[2:(cluster_number+1),4]<-((((cluster_number)+1)+cluster_number)+1) : ((((cluster_number)+1)+cluster_number)+cluster_number)
    lmat[2:(cluster_number+1),5]<-((((cluster_number)+1)+cluster_number)+1) : ((((cluster_number)+1)+cluster_number)+cluster_number)
    lmat[2:(cluster_number+1),6]<-((((cluster_number)+1)+cluster_number)+1) : ((((cluster_number)+1)+cluster_number)+cluster_number)

    #Mean Silwi
    lmat[1,3]<-((((cluster_number)+1)+cluster_number)+cluster_number)+1

    lwid <- c(0.5, 4,2)
    lhei <- c(1,rep(1,times=cluster_number))

  }




  ######### layout without clinical data with genes without GSE with mean sil width
  if( (print_genes) & (is.null(samples_data))  & (plot_mean_sil)
      & (!GSE)){
    print("Use Layout Format 18")
    lmat<-do.call(rbind,lapply(1:(cluster_number+1), function(i){
      return(c(0,i,0))
    }))
    lmat[2:(cluster_number+1),3]<-((cluster_number)+2): (((cluster_number)+1)+cluster_number)
    lmat[1,3]<-(((cluster_number)+1)+cluster_number)+1
    lwid <- c(0.5, 4,2)
    lhei <- c(1,rep(1,times=cluster_number))

  }

  ######### layout without clinical data with genes without GSE without mean sil width
  if( (print_genes) & (is.null(samples_data))  & (!plot_mean_sil)
      & (!GSE)){
    print("Use Layout Format 19")
    lmat<-do.call(rbind,lapply(1:(cluster_number+1), function(i){
      return(c(0,i,0))
    }))
    lmat[2:(cluster_number+1),3]<-((cluster_number)+2): (((cluster_number)+1)+cluster_number)
    lmat[1,3]<-(((cluster_number)+1)+cluster_number)+1
    lwid <- c(0.5, 4,2)
    lhei <- c(1,rep(1,times=cluster_number))

  }



  ##grDevices::pdf(file="try.pdf",width=10,height=10)
  if(is.null(sil_width)){
    lmat<-lmat-1
    lmat[which(lmat<0)]<-0
  }
  
  print(lmat)
  
  graphics::layout(lmat, widths = lwid, heights = lhei, respect = TRUE)

  ################# #Add Silhouette####################
  if(!is.null(sil_width)){
    palla<-RColorBrewer::brewer.pal(n =max(length(ordert_genes),3) ,name = "Paired")
    graphics::par(mar = c(1,3,0,0))
    graphics::plot(c(0,nc), c(min(sil_width[,2]),max(sil_width[,2])), bty="n",type="n",
         xaxs="i",xlab="",ylab="",axes=F)
    graphics::axis(side = 2,line = 0,cex.axis=0.7)
    graphics::title(ylab = "Sil. Width",cex.lab=0.7,line=2)
    if(!is.null(sil_width)){
      start=c(0,0)
      for(i in 1:nc){
        clust_col=palla[sil_width[i,1]]
        a=sample_names[i]
        width=sil_width[a,2]
        xx=c(i-0.5,i-0.5)
        yy=c(0,(width))
        graphics::polygon(xx,yy, lwd=1,col=clust_col,border = clust_col)
        graphics::points(x=i-0.5,y=width,pch=22,col=clust_col,bg=clust_col)
      }
    }
  }

  for(i in 1:length(ordert_genes)){
    xx=as.matrix(ordert_genes[[i]])
    nc=ncol(xx)
    nr=nrow(xx)
    xx <- sweep(xx, 1L, rowMeans(xx, na.rm = T), check.margin = FALSE)
    sx <- apply(xx, 1L, stats::sd, na.rm = T)
    xx <- sweep(xx, 1L, sx, "/", check.margin = FALSE)
    xx<-t(xx)
    graphics::par(mar = c(1, 3, 0, 0))
    (graphics::image(1L:nc, 1L:nr, xx, xlim = 0.5 + c(0, nc), ylim = 0.5 +
             c(0, nr), axes = FALSE, xlab = "", ylab = "",
           col=RColorBrewer::brewer.pal(n = 11,name = col),useRaster=T))

  }
  ######################################### clinical data
  if(!is.null(samples_data)){
    for(j in 1:ncol(samples_data)){
      graphics::par(mar = c(0.5, 3,0,0))
      graphics::plot(c(0,nc), c(1,10), bty="n",type="n",xaxs="i",xlab="",ylab="",axes=FALSE)#
      plot_w=nc
      plot_h=5
      diff_at<-unique(samples_data[,j])
      num_of_at<-length(diff_at)
      if(j==1)  graphics::text(x=nc/2,y=7,"Clinical Data",cex=1,adj = c(0,0))
      graphics::text(x=0.5,y=7,colnames(samples_data)[j],cex=0.7,adj = c(0,0))
      #pall<-palette(rainbow(num_of_at,start=((j-1)/ncol(samples_data)),end=(j/ncol(samples_data))))
      # pallb<-rainbow(num_of_at, start=rgb2hsv(col2rgb('brown'))[1],
      #                end=rgb2hsv(col2rgb('purple'))[1],alpha = 0.5)
      #
      pallb<-RColorBrewer::brewer.pal(n =max(num_of_at,3) ,name = "Paired")


      rownamesge<-sample_names
      yy<-c(2,plot_h,plot_h,2)
      vl<-length(sample_names)
      for (i in 1:vl) {
        xx<-c(i-1,i-1,i,i)
        col_indx<-which(diff_at==samples_data[rownamesge[i],j])
        color<-pallb[col_indx]
        # if(samples_data[rownamesge[i],j]==1)
        #   color="black"
        # else
        #   color="grey"
        graphics::polygon(xx,yy,col = color, border = NA )
      }
    }
  }


  
  ######################################### clinical data legende
  if(!is.null(samples_data)){
    for(j in 1:ncol(samples_data)){
      graphics::par(mar = c(0.5, 3,1,0))
      graphics::plot(c(0,20), c(1,10), bty="n",type="n",xaxs="i",xlab="",ylab="",axes=FALSE)#
      if(j==1)  graphics::text(x=10,y=8,"color legend for the clinical data",cex=1,adj = c(0,0))
      
      plot_w=nc
      plot_h=5
      diff_at<-unique(samples_data[,j])
      num_of_at<-length(diff_at)
      graphics::text(x=0.5,y=7,colnames(samples_data)[j],cex=0.7,adj = c(0,0))
      pallb<-RColorBrewer::brewer.pal(n =max(num_of_at,3) ,name = "Paired")
      for(iii in 1:length(pallb)){
        ff=iii/2
        xx=c(3.5, 4,4,3.5)
        yy=c((8-ff),(8-ff),(8-ff+0.5),(8-ff+0.5))
        graphics::polygon(xx,yy, col = pallb[iii], border = NA )
        graphics::text(x=4.5,y=8-ff,diff_at[iii],cex=0.7,adj = c(0,0))
        
      }
      
    }
  }
  

  ##################add genes on the side
  if( print_genes & (genes_to_print>=1) ){
    if(!is.null(list_of_genes)){
      up_genes<-as.array(lapply(1:length(list_of_genes), function(i){
        current_list<-list_of_genes[[i]]
        rownames(current_list[1:genes_to_print,])
      }))
      print(up_genes)
      down_genes<-as.array(lapply(1:length(list_of_genes), function(i){
        current_list<-list_of_genes[[i]]
        rownames(current_list[nrow(current_list):(nrow(current_list)-genes_to_print+1),])
      }))
      for (i in 1:length(up_genes)) {
        graphics::par(mar = c(1, 0.5,0,0))
        graphics::plot(x= c(1,1000),y=c(0,genes_to_print+2), bty="n",type="n",xlab="",ylab="",axes=FALSE,yaxs="i")
        #space_for_gene_names<-1000/length(up_genes)
        # yPoints<-seq(from=((i-1)*(space_for_gene_names))
        # ,to=(i*space_for_gene_names)
        # ,by=((space_for_gene_names/(genes_to_print+2))))
        ### remove firs and last element so genes wont overlap on plot
        # last<-length(yPoints)
        # yPoints<-yPoints[-last]## remove last
        # yPoints<-yPoints[-1]## remove first
        # yPoints<-yPoints[-1]## remove first
        if(i==1){
          graphics::text(c("Up Reg.","Down Reg"),x=c(1,300),y=genes_to_print+1,col = "Black",cex = 0.7,adj = c(0,0))
        }
        u_g<-entrez_to_name(up_genes[i])
        graphics::text(u_g,x=1,y=1:genes_to_print,col = "chocolate3",cex = 0.7,adj = c(0,0))
        d_g<-entrez_to_name(down_genes[i])
        graphics::text(d_g,x=300,y=1:genes_to_print,col = "cornflowerblue",cex = 0.7, adj = c(0,0))
      }
    }else{
      stop("List of genes is NULL")
    }
  }

  ###########plot GSE
  if(GSE){
    if(!is.null(list_of_genes)){
      pall<-RColorBrewer::brewer.pal(n =max(length(ordert_genes),3) ,name = "Paired")
      paths<-pathway_fgsea(db=db,number_of_k=cluster_number,clusters_data=list_of_genes,topPaths=topPaths)
      width=1
      dif=1
      for(i in 1:length(list_of_genes)){
        graphics::par(mar = c(1, 0.5,0,0))
        graphics::plot(x= c(1,1200),y=c(0,topPaths*(dif+width)+2), bty="n",type="n",xlab="",ylab=""
             ,axes=FALSE,yaxs="i")
        graphics::abline(v=1)
        if(i==length(list_of_genes)){
          graphics::axis(side = 1,at = seq(from=100, to=800, by=100),labels = seq(from=0.1,to=0.8,by=0.1),cex.axis=0.7)
        }
        paths_c<-paths[[i]]
        clust_col=pall[i]
        for (j in 1:nrow(paths_c)) {
          yy=c(dif+j*(width+dif),2*dif+j*(width+dif),2*dif+j*(width+dif),dif+j*(width+dif))
          e<-paths_c[j,"ES"]*1000
          xx=c(1,1,e,e)
          graphics::polygon(x = xx,y = yy,col = clust_col)
          graphics::text(x=e+50,y = dif+j*(width+dif),paths_c[j,"pathway"],adj=c(0,0),cex=0.5)
        }
      }
    }else{
      stop("List of Genes is Empty")
    }
  }

  

  
  
  ################plot mean Sill. width
  if(plot_mean_sil){
    diff_clusters<-length(sil_mean)
    graphics::par(mar = c(1, 3,0,0))
    graphics::plot(sil_mean, type = "l", bty="n", xlab = "",ylab="",xaxt="n",cex.axis=0.6,cex.lab=0.6,col="grey")
    graphics::axis(3, 1:diff_clusters, seq(2,(diff_clusters+1),by=1),line = 2,cex.lab=0.6,cex.axis=0.6)
    graphics::points(sil_mean, col="red", pch=20)
    graphics::abline(v=which.max(sil_mean), lty=3)
    graphics::title(xlab = "Number of Clusters (n)", line = 0,cex.lab=0.6)
    graphics::title(ylab = "Mean Sil.Width", line = 2,cex.lab=0.6)

  }
  

  
  
  
  
  ##grDevices::dev.off()
}
