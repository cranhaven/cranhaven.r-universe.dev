# eval_var_loc.R
#
# This function creates the plots for the measures of variance. It produces the mean vs sd plot, and computes metrics
# that are used in the final radar plot such as proportion of signature genes lying in the top 10%, 25%, and 50% of
# overall coefficient of variance across all genes, and the fractional ratio of the skewness of the expression distribution
# of signature genes vs all genes. Also outputs tables containing mean and sd values for each of the signature genes
# @param gene_sigs_list A list of genes representing the gene signature to be tested.
# @param names_sigs The names of the gene signatures (one name per gene signature, in gene_sigs_list)
# @param mRNA_expr_matrix A list of expression matrices
# @param names_datasets The names of the different datasets contained in mRNA_expr_matrix
# @param out_dir A path to the directory where the resulting output files are written
# @param file File representing the log file where errors can be written
# @param showResults Tells if open dialog boxes showing the computed results. Default is FALSE
# @param radar_plot_values A list of values that store computations that will be used in the final summary radarplot
# @keywords eval_var_loc

eval_var_loc <- function(gene_sigs_list,names_sigs, mRNA_expr_matrix,names_datasets, out_dir = '~',file=NULL,showResults = FALSE,radar_plot_values){
  # calculate the number of rows and columns in the image
  num_rows <- length(names_sigs)#ceiling(sqrt(length(names)))
  num_cols <- length(names_datasets)#ceiling(length(names)/num_rows)
  #set up the plotting area
  if (showResults){
    grDevices::dev.new()
  }else{
    grDevices::pdf(file.path(out_dir,'sig_mean_vs_sd.pdf'),width=4*(length(names_datasets)),height=4*(length(names_sigs)))
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

  gene_sig_mean_sd_table <- list() #this is to store the lists of tables that will be output to file
  graphics::par(mfrow=c(num_rows,num_cols)) #set up the plotting area
  for(k in 1:length(names_sigs)){
    gene_sig <- gene_sigs_list[[names_sigs[k]]] #load the gene signature
    if(is.matrix(gene_sig)){gene_sig = as.vector(gene_sig);}
    for (i in 1:length(names_datasets)){
      data.matrix = mRNA_expr_matrix[[names_datasets[i]]] #load the dataset
      inter <- intersect(gene_sig, row.names(data.matrix)) #consider genes only present in the dataset

      sd_genes <- as.matrix(apply(data.matrix,1,function(x){stats::sd(as.numeric(x),na.rm=T) })) #compute the standard deviation of the genes
      mean_genes <- as.matrix(apply(mRNA_expr_matrix[[names_datasets[i]]],1,function(x) {mean(as.numeric(x),na.rm=T)})) #comptue the mean of the genes
      #stats::median(stats::na.omit(sd_genes[inter, 1]))/(stats::median(stats::na.omit(sd_genes)) +stats::median(stats::na.omit(sd_genes[inter, 1])))
      radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]['sd_median_ratio'] = stats::median(stats::na.omit(sd_genes[inter,1]))/(stats::median(stats::na.omit(sd_genes)) +stats::median(stats::na.omit(sd_genes[inter,1]))) #fractional ratio of medians of standard deviations of sig genes vs all genes
      radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]['abs_skewness_ratio'] = abs(moments:: skewness(stats::na.omit(mean_genes[inter, 1])))/(abs(moments:: skewness(stats::na.omit(mean_genes))) +  abs(moments:: skewness(stats::na.omit(mean_genes[inter, 1])))) #fractional ratio of skewness of mean expressions
      #plot the scatterplot
      graphics::plot(mean_genes,sd_genes,pch=19,col='grey',cex=0.5,
                     main=paste0('Mean vs SD of expression\n',names_datasets[i], ' ',names_sigs[k]),
                     xlab='Mean',
                     ylab='Standard deviation',
                     cex.main=min(1,4*10/max_title_length))

      graphics::points(mean_genes[inter, 1],sd_genes[inter,1],pch=19,col='red',cex=0.5) #draw the gene signature poitns in red
      #calculate the quantiles of mean
      quants_mean <- stats::quantile(mean_genes*is.finite(mean_genes),probs=c(0.1,0.25,0.5,0.75,0.9),na.rm=T)
      #the following draw the quantile lines
      graphics::abline(v=quants_mean[1],lty=5,col='cadetblue1') #add line at 10%
      graphics::abline(v=quants_mean[2],lty=5,col='dodgerblue1') #add line at 25%
      graphics::abline(v=quants_mean[3],lty=5,col='darkblue') #add line at 50%
      graphics::abline(v=quants_mean[4],lty=5,col='dodgerblue1') #add line at 75%
      graphics::abline(v=quants_mean[5],lty=5,col='cadetblue1') #add line at 90%

      # graphics::mtext(side = 3, line = 0, at=quants_mean[1], '10%',cex=0.4)
      # graphics::mtext(side = 3, line = 0.4, at= quants_mean[2], '25%',cex=0.4)
      # graphics::mtext(side = 3, line = 0, at= quants_mean[3], '50%',cex=0.4)
      # graphics::mtext(side = 3, line = 0.4, at=quants_mean[4], '75%',cex=0.4)
      # graphics::mtext(side = 3, line = 0, at = quants_mean[5], '90%',cex=0.4)

      #calculate and draw the quantiles of the standard deviation
      quants_sd <- stats::quantile(sd_genes*is.finite(sd_genes),probs=c(0.1,0.25,0.5,0.75,0.9),na.rm=T)
      graphics::abline(h=quants_sd[1],lty=5,col='cadetblue1') #add line at 10%
      graphics::abline(h=quants_sd[2],lty=5,col='dodgerblue1') #add line at 25%
      graphics::abline(h=quants_sd[3],lty=5,col='darkblue') #add line at 50%
      graphics::abline(h=quants_sd[4],lty=5,col='dodgerblue1') #add line at 75%
      graphics::abline(h=quants_sd[5],lty=5,col='cadetblue1') #add line at 90%

      # graphics::mtext(side = 4, line = 0, at=quants_sd[1], '10%',cex=0.4)
      # graphics::mtext(side = 4, line = 0.4, at= quants_sd[2], '25%',cex=0.4)
      # graphics::mtext(side = 4, line = 0.8, at= quants_sd[3], '50%',cex=0.4)
      # graphics::mtext(side = 4, line = 0.4, at=quants_sd[4], '75%',cex=0.4)
      # graphics::mtext(side = 4, line = 0, at = quants_sd[5], '90%',cex=0.4)
      graphics::legend('topright',legend=c('10%, 90%','25%, 75%','50%'),col=c('cadetblue1','dodgerblue1','darkblue'),lty=5,bty='n',cex=0.5) #create legend for the quantile lines
      gene_sig_mean_sd_table[[names_sigs[k]]][[names_datasets[i]]] <- cbind(mean_genes[inter, 1],sd_genes[inter, 1]) #store values for the gene signature table
      colnames(gene_sig_mean_sd_table[[names_sigs[k]]][[names_datasets[i]]]) <- c("Mean","SD")
    }
  }
  #save the plot to file
  if(showResults){
    grDevices::dev.copy(grDevices::pdf,file.path(out_dir,'sig_mean_vs_sd.pdf'),width=4*(length(names_datasets)),height=4*(length(names_sigs)))
  }
  if(grDevices::dev.cur()!=1){
    g <- grDevices::dev.off() # to reset the graphics pars to defaults
  }
  cat('Mean vs SD graphs created successfully.\n', file=file) #output to log

  #now let's output the tables for mean vs sd for the gene signature, one for each dataset and signature considered
  if(!dir.exists(file.path(out_dir,'mean_sd_tables'))){
    dir.create(file.path(out_dir,'mean_sd_tables'))
  }
  for(k in 1:length(names_sigs)){
    for (i in 1:length(names_datasets)){
      utils::write.table(gene_sig_mean_sd_table[[names_sigs[k]]][[names_datasets[i]]],file=file.path(out_dir,'mean_sd_tables', paste0('mean_sd_table_',names_sigs[k],'_',names_datasets[i],'.txt')),quote=F,sep='\t')
    }
  }
  cat('Mean vs SD tables written to file successfully.\n', file=file)

  #the following is the code for computing coefficient of variation across signature genes and all genes
  for(k in 1:length(names_sigs)){
    gene_sig <- gene_sigs_list[[names_sigs[k]]] #load gene signature
    if(is.matrix(gene_sig)){gene_sig = as.vector(gene_sig);}
    for (i in 1:length(names_datasets)){
      data.matrix = mRNA_expr_matrix[[names_datasets[i]]] #load dataset
      inter <- intersect(gene_sig, row.names(data.matrix)) #only use genes in the dataset
      #the following computes the coefficient of varaition
      coeff_of_var <- as.matrix(apply(data.matrix,1,function(x){stats::sd(as.numeric(stats::na.omit(x)),na.rm=T) / mean(as.numeric(stats::na.omit(x)),na.rm=T)}))
      coeff_of_var_gene_sig <- coeff_of_var[inter, 1]
      quantiles_considered <- stats::quantile(coeff_of_var,probs=c(0.9,0.75,0.5),na.rm=T)
      #the following determines the proportion of signature genes in the top 10%, 25% and 50% of all coefficients of variation
      prop_top_10_percent <- sum(stats::na.omit(coeff_of_var_gene_sig) >= quantiles_considered[1]) / length(stats::na.omit(coeff_of_var_gene_sig))
      prop_top_25_percent <- sum(stats::na.omit(coeff_of_var_gene_sig) >= quantiles_considered[2]) / length(stats::na.omit(coeff_of_var_gene_sig))
      prop_top_50_percent <- sum(stats::na.omit(coeff_of_var_gene_sig) >= quantiles_considered[3]) / length(stats::na.omit(coeff_of_var_gene_sig))
      #then we store these values for the final plot
      radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]['prop_top_10_percent'] <- prop_top_10_percent
      radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]['prop_top_25_percent'] <- prop_top_25_percent
      radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]['prop_top_50_percent'] <- prop_top_50_percent

      radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]['coeff_of_var_ratio'] <- abs((abs(stats::median(stats::na.omit(coeff_of_var_gene_sig))))/((abs(stats::median(stats::na.omit(coeff_of_var)))) + abs((stats::median(stats::na.omit(coeff_of_var_gene_sig)))))) #the fractional ratio of the coefficient of variation for the final plot

      # boxplot(stats::na.omit(coeff_of_var),stats::na.omit(coeff_of_var_gene_sig),#log="y",
      # names=c('All Genes','Gene Signature'),
      # ylab='Coefficient of Variation',
      # main=paste0('Variance of signature genes vs. all genes\n',names[i]))
    }
  }
  # dev.copy(pdf,paste0(out_dir,'/sig_expr_var.pdf'))

  # dev.off()
  radar_plot_values #return values for the final plot
}