# eval_stan_loc.R
#
# This function creates the plots of effects of standardisation. It checks the correlation between the median and the z-transformed
# median of the signature genes in the dataset.
# @param gene_sigs_list A list of genes representing the gene signature to be tested.
# @param names_sigs The names of the gene signatures (one name per gene signature, in gene_sigs_list)
# @param mRNA_expr_matrix A list of expression matrices
# @param names_datasets The names of the different datasets contained in mRNA_expr_matrix
# @param out_dir A path to the directory where the resulting output files are written
# @param file File representing the log file where errors can be written
# @param showResults Tells if open dialog boxes showing the computed results. Default is FALSE
# @param radar_plot_values A list of values that store computations that will be used in the final summary radarplot
# @keywords eval_stan_loc

eval_stan_loc <- function(gene_sigs_list, names_sigs,mRNA_expr_matrix, names_datasets,out_dir = '~',file=NULL,showResults = FALSE,radar_plot_values ){
  # defining the size of the plotting area
  num_rows <- length(names_sigs)#ceiling(sqrt(length(names)))
  num_cols <- length(names_datasets)#ceiling(length(names)/num_rows)

  #set up the graphics canvas
  if (showResults){
    grDevices::dev.new()
  }else{
    grDevices::pdf(file.path(out_dir, 'sig_standardisation_comp.pdf'),width=4*(length(names_datasets)),height=(4*length(names_sigs)))
  }

    #find the max number of characters in the title for font size purposes
     max_title_length <- -999
  for(k in 1:length(names_sigs)){
    for( i in 1:length(names_datasets)){
      if(max_title_length < nchar(paste0(names_datasets[i],' ',names_sigs[k]))){
        max_title_length <- nchar(paste0(names_datasets[i],' ',names_sigs[k]))
      }
    }
  }

  #set up graphics area
  graphics::par(mfrow=c(num_rows,num_cols),oma=c(2,2,2,2),mar=c(4,4,4,4))

  #create dir for output text files 
  if(!dir.exists(file.path(out_dir,'standardisation_tables'))){
    dir.create(file.path(out_dir,'standardisation_tables'))
  }

  for(k in 1:length(names_sigs)){ 
    gene_sig <- gene_sigs_list[[names_sigs[k]]] #load in the gene signature
    if(is.matrix(gene_sig)){gene_sig = as.vector(gene_sig);}
    for (i in 1:length(names_datasets)){
      data.matrix = mRNA_expr_matrix[[names_datasets[i]]] #load in the data matrix
      inter <- intersect(gene_sig, row.names(data.matrix)) #make sure the genes are present in the dataset
      # the following takes the expressions of the gene signature and computes standardised expressions across samples
      z_transf_mRNA <- data.matrix[inter,]
      for (gene in inter){
        z_transf_mRNA[gene,] <- (as.numeric(z_transf_mRNA[gene,]) - mean(as.numeric(z_transf_mRNA[gene,]),na.rm=T)) / stats::sd(as.numeric(z_transf_mRNA[gene,]),na.rm=T)
      }
      z_transf_scores <- apply(z_transf_mRNA[inter,],2,function(x) {stats::median(stats::na.omit(x))}) #now take the median of the z-transformed expression

      med_scores <- apply(data.matrix[inter,],2,function(x){ stats::median(stats::na.omit(x))}) #take the median of the non-standardised expression

      #the following is to plot the 2D scatterplot of the scores
      jet.colors <- grDevices::colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
      graphics::smoothScatter(med_scores,z_transf_scores,colramp = jet.colors,xlab=NA,ylab=NA,main='Median vs Z-median')
      graphics::points(med_scores,z_transf_scores,pch='.')
      graphics::par(new=T,srt=45)
      graphics::plot(stats::density(med_scores), axes=F, xlab=NA, ylab=NA,  col='red',main=NA,lwd=2)
      graphics::axis(side = 4)
      graphics::mtext(side = 4, line = 2, 'Density',cex=0.8)
      graphics::mtext(side = 2, line = 2, 'Z-median',cex=0.8)
      graphics::mtext(side = 1, line = 2, 'Median',cex=0.8)
      graphics::mtext(side=3,line=2.5,paste0(names_datasets[i],', ',names_sigs[k] ),cex=min(1,4*10/max_title_length))

      rho <- stats::cor(med_scores,z_transf_scores,method='spearman')
      graphics::mtext(paste0('rho = ',format(rho,digits = 2)),side=3,line=0,cex = 0.6,at=max(med_scores))
      radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]['standardization_comp'] <- rho #store the value for the final radar plot
      
      #here we output the table of meadian and z-median scores to a table
      output_mat <- cbind(med_scores,z_transf_scores)
      colnames(output_mat) <- c('Median Scores','Z-Median Scores')

      utils::write.table(output_mat,file=file.path(out_dir,'standardisation_tables', paste0('standardisation_table_',names_sigs[k],'_',names_datasets[i],'.txt')),quote=F,sep='\t')


    }
  }
  cat('Standardisation compared successfully.\n', file=file) #output to log
  #save the plot
  if(showResults){
    grDevices::dev.copy(grDevices::pdf,file.path(out_dir, 'sig_standardisation_comp.pdf'),width=4*(length(names_datasets)),height=(4*length(names_sigs)))
  }
  if(grDevices::dev.cur()!=1){
    g <- grDevices::dev.off() # to reset the graphics pars to defaults
  }
    radar_plot_values #return the radarplot values
}
