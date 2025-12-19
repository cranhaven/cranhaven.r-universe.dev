# eval_compactness_loc.R
#
# This function creates the plots of autocorrelation, as well as the rank product computation. Specifically, it creates
# plots for heatmap of autocorrelation, density plot of autocorrelation values, and if there is more than one dataset, for
# each individual gene signature, will do a rank product analysis on the lists of median autocorrelation coefficients to
# identify genes consistently low ranking in autocorrelation among more than one dataset.
# @param gene_sigs_list A list of genes representing the gene signature to be tested.
# @param names_sigs The names of the gene signatures (one name per gene signature, in gene_sigs_list)
# @param mRNA_expr_matrix A list of expression matrices
# @param names_datasets The names of the different datasets contained in mRNA_expr_matrix
# @param out_dir A path to the directory where the resulting output files are written
# @param file File representing the log file where errors can be written
# @param showResults Tells if open dialog boxes showing the computed results. Default is FALSE
# @param radar_plot_values A list of values that store computations that will be used in the final summary radarplot
# @keywords eval_compactness_loc

eval_compactness_loc <- function(gene_sigs_list,names_sigs, mRNA_expr_matrix, names_datasets, out_dir = '~',file=NULL,showResults = FALSE,radar_plot_values,logged=T,origin=NULL){
  #create new canvas for plotting if to be shown on screen
  # if (showResults){
  grDevices::dev.new()
  # }
  # print(grDevices::dev.cur())
  # if(grDevices::dev.cur()==1){
  #   grDevices::dev.new()
  # }
  # else{
  #   grDevices::pdf(file.path(out_dir, 'sig_autocor_hmps.pdf'),width=10,height=10)
  # }
  # pdf(file.path(out_dir,'sig_autocor_hmps.pdf'),width=10,height=10)
  #the following calculates the maximum length of a title for appropriate font sizing
  max_title_length <- -999
  for(i in 1:length(names_sigs)){
    for(j in 1:length(names_datasets)){
      if(nchar(paste0(names_datasets[[j]] ,' ',names_sigs[[i]])) > max_title_length){
       max_title_length <- nchar(paste0(names_datasets[[j]] ,' ',names_sigs[[i]]))
      }
    }
  }

  #sets up graphics parameters for the plotting area
  graphics::par(cex.main=min(0.8,(3*6/max_title_length)),cex.lab = 0.6,oma=c(2,0,0,0),mar=c(0,0,0,0))
  #in the following, we compute each of the autocorrelation heatmaps
  hmaps <- lapply(1:(length(names_sigs) *length(names_datasets)),function(i) {
    # first take the combined index i, and figure out it's index as an array index over a grid for each dataset and each signature
    dataset_ind <- i %% length(names_datasets)
    if (dataset_ind == 0 ){
      dataset_ind <- length(names_datasets)
    }
    sig_ind <- ceiling(i/length(names_datasets))
    gene_sig <- gene_sigs_list[[names_sigs[sig_ind]]]
    if(is.matrix(gene_sig)){gene_sig = as.vector(gene_sig);}
    data.matrix = mRNA_expr_matrix[[names_datasets[dataset_ind]]] #load the data
    inter <- intersect(gene_sig, row.names(data.matrix)) #consider only the genes inside the dataset
    autocors <- stats::cor(t(stats::na.omit(data.matrix[inter,])),method='spearman') #calculate the autocorrelations
    # print(paste0('gene_sig ',gene_sig))
    # print(paste0("intersection",inter))
    # print(cor(t(data.matrix[inter,]),method='spearman'))
    # print(autocors)
    #let's output this to a file---
    if(!dir.exists(file.path(out_dir,'autocorrelation_matrices'))){
      dir.create(file.path(out_dir,'autocorrelation_matrices'))
    }
    utils::write.table(autocors,file=file.path(out_dir,'autocorrelation_matrices', paste0('autocorrelation_matrix_',names_sigs[sig_ind],'_',names_datasets[dataset_ind],'.txt')),quote=F,sep='\t')


    #-----


    #the following draws the heatmaps for the autocorrelation
    tryCatch({
      # plot.new()
     # strwd <-  strwidth(max(nchar(rownames(autocors))), units = "figure", cex = max(min(0.5,(4*4/length(rownames(autocors)))),0.06))
     # print(max(nchar(rownames(autocors))))
     # print(strwd)
      gplots::heatmap.2( stats::na.omit(autocors),
                         col = gplots::colorpanel(100,"blue","white","red"), #redgreen(100),#colorpanel(100,"red","yellow","green"),
                         trace = "none",
                     #    xlab = "Gene ID",
                     #    ylab="Gene ID",
                         na.color="grey",
                         labRow=rownames(autocors),
                         labCol=colnames(autocors),#gene_sig,
                         main = paste0("\n\nIntra-sig. Corr.\n", names_datasets[[dataset_ind]] ,' ',names_sigs[[sig_ind]]),
                         dendrogram = "col",
                         symbreaks = T,
                         Rowv = T,Colv=T ,key.xlab='Rho',key.ylab=NA,  key.title=NA,cexRow=max(min(0.5,(4*4/length(rownames(autocors)))),0.06),cexCol=max(min(0.5,(4*4/length(rownames(autocors)))),0.06),margins=c(1+(max(nchar(rownames(autocors)))/2),1+ (max(nchar(rownames(autocors)))/2)))

    },

    error=function(err){
      graphics::plot.new()
      graphics::title(paste0('\n\nToo many NA values in \n',names_datasets[dataset_ind],' ',names_sigs[sig_ind]))#cex=min(1,4*10/max_title_length))
      cat(paste0("There was an error,  ",names_datasets[dataset_ind]," ", names_sigs[sig_ind]," ", err,'\n'), file=file)

    })
    grab_grob() #grabs the image from the screen to put on a pdf
  })
  draw.heatmaps(hmaps,names_datasets,names_sigs) #this function draws the heatmaps to a canvas
  # if(showResults){
    #saves the canvas object to file
  grDevices::dev.copy(grDevices::pdf,file.path(out_dir,'sig_autocor_hmps.pdf'),width=4*(length(names_datasets)),height=4*(length(names_sigs)))#width=10,height=10)
  # }
  if(grDevices::dev.cur()!=1){
    g <- grDevices::dev.off() # to reset the graphics pars to defaults
  }
#because we want the legend outside the plot, first we calculate the width of the legend to get the right sized canvas
  #code from stackoverflow.
  if(grDevices::dev.cur()!=1){
    g <- grDevices::dev.off() # to reset the graphics pars to defaults
  }
  graphics::par(mar=c(0,0,0,0),cex=0.6)#c(par('mar')[1:3], 0)) # optional, removes extraneous right inner margin space
  graphics::plot.new()

  legend_names <- c()
  legend_cols <- c()
  legend_lty <- c()
  for(k in 1:length(names_sigs)){
    for (i in 1:length(names_datasets) ){
      legend_names <- c(legend_names,paste0(names_datasets[i],' ',names_sigs[k]))
      legend_cols <- c(legend_cols,i)
      legend_lty <- c(legend_lty,k)
    }
  }

  l <-  graphics::legend(0, 0,legend_names,col=legend_cols,lty=legend_lty,lwd=rep(1,times=(length(names_datasets) * length(names_sigs))),pt.cex=1,cex=min(0.5,(4*10/max_title_length)), plot=FALSE)
  # calculate right margin width in ndc
  w <- graphics::grconvertX(l$rect$w, to='ndc')- graphics::grconvertX(0, to='ndc')
  w <- graphics::grconvertX(w,from="ndc",to="inches") + graphics::grconvertX(10,from="device",to="inches") #add a bit of padding around legend

 #sets up the graphical parameters

  #sets up a new graphics object
  if (showResults){
    grDevices::dev.new()
  }else{
    grDevices::pdf(file.path(out_dir,'sig_autocor_dens.pdf'),width=5,height=5)
  }


  graphics::par(cex.main=0.8,cex.lab = 0.6,mar=c(3,3,4,1),mfrow=c(1,1),xpd=TRUE,omi=(c(0,0,0,w)))

#  graphics::par(xpd=TRUE,mai=(graphics::par('mai') + c(0,0,0,padding)))


  #the following is to set up the right y limit on the autocorrelation density plot (first we need to know the maximum of the density plots)
  max_dens <- -9999
  max_x_coord <- -9999 #for legnd plotting
  for(k in 1:length(names_sigs)){
    gene_sig <- gene_sigs_list[[names_sigs[k]]]
    if(is.matrix(gene_sig)){gene_sig = as.vector(gene_sig);}
    for (i in 1:length(names_datasets) ){
      data.matrix = mRNA_expr_matrix[[names_datasets[i]]]
      inter <- intersect(gene_sig, row.names(data.matrix))
      autocors <- stats::cor(t(stats::na.omit(data.matrix[inter,])),method='spearman')
      # print(paste0("autocor dim", dim(autocors)))
      # print(autocors)
      if(dim(autocors)[1] > 1){
        cur_max <- max(stats::density(unlist(stats::na.omit(autocors)))$y)
        if (max_dens  < cur_max){
          max_dens <- cur_max
        }
        cur_max <- max(stats::density(unlist(stats::na.omit(autocors)))$x)
        if (max_x_coord  < cur_max){
          max_x_coord <- cur_max
        }
      }
    }
  }
  #the following actually draws the density plots for each signature and dataset

  plots_count <- 0
  for(k in 1:length(names_sigs)){
    gene_sig <- gene_sigs_list[[names_sigs[k]]] #load the signature
    if(is.matrix(gene_sig)){gene_sig = as.vector(gene_sig);}
    for (i in 1:length(names_datasets) ){
      data.matrix = mRNA_expr_matrix[[names_datasets[i]]] #load the datasets
      inter <- intersect(gene_sig, row.names(data.matrix)) #consider only the genes in the dataset
      autocors <- stats::cor(t(stats::na.omit(data.matrix[inter,])),method='spearman') #calculate autocorrelation
      #make the plots
      if(dim(autocors)[1] > 1){

        if (plots_count==0){#(i ==1) && (k==1)){
          graphics::plot(stats::density(unlist(stats::na.omit(autocors))),ylim=c(0,ceiling(max_dens)),xlim=c(-1,1),col=i,main=NA,lwd=2,lty=k)
          plots_count <- 1
        }else{
          graphics::lines(stats::density(unlist(stats::na.omit(autocors))),ylim=c(0,ceiling(max_dens)),xlim=c(-1,1),col=i,main=NA,lwd=2,lty=k)
        }
        radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]['autocor_median'] <- stats::median(autocors,na.rm=T) #store the median autocorrelation for the final radar plot
      }else{
        radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]['autocor_median'] <- 0#stats::median(stats::na.omit(autocors)) #store the median autocorrelation for the final radar plot

      }
    }
  }
  # draw the labels for the plot
  graphics::mtext(side = 2, line = 2, 'Density',cex=0.8)
  graphics::mtext(side = 1, line = 2, 'Rho',cex=0.8)
  graphics::mtext(side = 3, line = 2,'Intra-sig. Corr. Density')
  # makes the legend for the plot (sets parameters)
  op <- graphics::par(cex=0.6)#,xpd=T)

  graphics::legend(graphics::par('usr')[2]+0.05, graphics::par('usr')[4],xpd=NA,legend_names,col=legend_cols,lty=legend_lty,lwd=rep(1,times=(length(names_datasets) * length(names_sigs))),pt.cex=1,cex=min(0.5,(4*10/max_title_length)))
  #saves the plot to file
  if(showResults){
    grDevices::dev.copy(grDevices::pdf,file.path(out_dir,'sig_autocor_dens.pdf'),width=5,height=5)
  }
  if(grDevices::dev.cur()!=1){
    g <- grDevices::dev.off() # to reset the graphics pars to defaults
  }  # the following only computes the rank product if there is more than one dataset for which to compute it for
  if (length(names_datasets) > 1){
    RankProdInstalled = (nchar(system.file(package='RankProd')) > 0)

    if(RankProdInstalled){
      for(k in 1:length(names_sigs)){
        gene_sig <- gene_sigs_list[[names_sigs[k]]] #load the gene signature
        if(is.matrix(gene_sig)){gene_sig = as.vector(gene_sig);}
        #create the canvas
        if (showResults){
          grDevices::dev.new()
        }else{
          grDevices::pdf(file.path(out_dir,paste0('sig_autocor_rankProd_',names_sigs[k],'.pdf')),width=10,height=10)
        }

        graphics::par(cex.main=0.8,cex.lab = 0.6,oma=c(2,2,2,2),mar=c(4,4,4,4)) #set drawing parameters

        #now we take the median of the genes' autocorrelation for each gene in each dataset and then look at the rank product over the different cancer types
        #note that the rank product analysis is only done if there is more than one dataset (otherwise not done, and is doen separately for each gene signature)
          overall_rank_mat <- matrix(NA,nrow=length(unique(gene_sig)),ncol=length(names_datasets))
          #create the overall matrix of datsets and signature genes containing the
          #median gene autocorrelation for each dataset
          row.names(overall_rank_mat) <- unique(gene_sig)
          colnames(overall_rank_mat) <- names_datasets
          for (i in 1:length(names_datasets)){
            data.matrix = mRNA_expr_matrix[[names_datasets[i]]] #load data
            inter = intersect(unique(gene_sig),rownames(data.matrix)) #consider only genes present in that dataset
            autocors <- stats::cor(t(stats::na.omit(data.matrix[inter,])),method='spearman')
            median_scores <- as.matrix(apply(autocors,2,function(x) {stats::median(stats::na.omit(x))})) #median autocorrelation
            overall_rank_mat[rownames(median_scores),i] <- median_scores[,1]
          }
          # the following computes the rank product
          # require(RankProd)
          if(is.null(origin)){
            origin <- rep(1,length(names_datasets))
          }
          RP.out <-RankProd::RPadvance(data = overall_rank_mat,cl = rep(1,times=length(names_datasets)),origin = origin ,logged = T,gene.names=rownames(overall_rank_mat)) #
          RankProd::plotRP(RP.out ,cutoff=0.05)
          #the following will calcualte the full output table for the rank prod and save it sorted two ways
          table_rank_prod <- cbind(RP.out$pfp,RP.out$pval,RP.out$RPs)
          colnames(table_rank_prod) <- c(paste0("pfp_",colnames(RP.out$pfp)),paste0('p_val',colnames(RP.out$pval)),paste0("Rank_Product_",colnames(RP.out$RPs)))
          if(!dir.exists(file.path(out_dir,'rank_prod'))){
            dir.create(file.path(out_dir,'rank_prod')) #create the dir
          }
          utils::write.csv(table_rank_prod[order(table_rank_prod[,1]),],file=file.path(out_dir, 'rank_prod',paste0('rank_product_table1_',names_sigs[k],'.txt')),quote=F)
          utils::write.csv(table_rank_prod[order(table_rank_prod[,2]),],file=file.path(out_dir, 'rank_prod',paste0('rank_product_table2_',names_sigs[k],'.txt')),quote=F)

          # #compute the tables of up and down regulated genes
          # table_rank_prod <- RankProd::topGene(RP.out,cutoff=0.05,method="pfp",logged=T, gene.names=rownames(overall_rank_mat))#intersect(gene_sig[,1],rownames(mRNA_expr_matrix[[names_datasets[i]]])))
          # # output the rank product table to file
          # if( (!is.null(table_rank_prod$Table1))) {
          #   dir.create(file.path(out_dir,'rank_prod')) #create the dir
          #     utils::write.csv(table_rank_prod$Table1,file=file.path(out_dir, 'rank_prod',paste0('rank_product_table1_',names_sigs[k],'.txt')),quote=F,sep='\t')

          # }
          # if (!is.null(table_rank_prod$Table2)){
          #     dir.create(file.path(out_dir,'rank_prod')) #create the dir
          #     utils::write.csv(table_rank_prod$Table2,file=file.path(out_dir, 'rank_prod',paste0('rank_product_table2_',names_sigs[k],'.txt')),quote=F,sep='\t')
          # }

          cat("Intra-sig. corr. rank product successfully computed.\n", file=file) #output to log file

        #save the rank product plot
        if(showResults){
          grDevices::dev.copy(grDevices::pdf,file.path(out_dir,paste0('sig_autocor_rankProd_',names_sigs[k],'.pdf')),width=10,height=10)
        }
        if(grDevices::dev.cur()!=1){
            g <- grDevices::dev.off() # to reset the graphics pars to defaults
        }
       }
      cat("Intra-sig. corr. metrics successfully computed.\n", file=file) #output to log file
   }else{
    cat("Intra-sig. corr. metrics not computed. Please install RankProd from BioConductor.\n", file=file) #output to log file

  }
  }else{
      cat("Rank product not computed as there is only one dataset.\n", file=file)

    }

  radar_plot_values #return the radar plot values

}
