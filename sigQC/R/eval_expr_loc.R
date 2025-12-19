# eval_expr_loc.R
#
# This function creates the plots of measures of expression. Takes in the list of thresholds of expression, and if this is not
# specified by the user, uses the median of all genes across all samples as the threshold cutoff for the barchart presenting
# proportion of samples expressing above the threshold. Also produces similar barchart for proportion of samples with NA values
# for each of the genes considered. Creates density plots as well for these barcharts for easy viewing of distribution in the case
# of a large number of signature genes.
# @param gene_sigs_list A list of genes representing the gene signature to be tested.
# @param names_sigs The names of the gene signatures (one name per gene signature, in gene_sigs_list)
# @param mRNA_expr_matrix A list of expression matrices
# @param names_datasets The names of the different datasets contained in mRNA_expr_matrix
# @param thresholds A list of thresholds to be considered for each data set, default is median of the data set. A gene is considered expressed if above the threshold, non-expressed otherwise. One threshold per dataset, in the same order as the dataset list.
# @param out_dir A path to the directory where the resulting output files are written
# @param file File representing the log file where errors can be written
# @param showResults Tells if open dialog boxes showing the computed results. Default is FALSE
# @param radar_plot_values A list of values that store computations that will be used in the final summary radarplot
# @keywords eval_expr_loc

eval_expr_loc <- function(gene_sigs_list,names_sigs, mRNA_expr_matrix,names_datasets, thresholds = NULL, out_dir = '~',file=NULL,showResults = FALSE,radar_plot_values){
  #calculate the number of rows and columns in the final plot
  num_rows <- length(names_sigs)#ceiling(sqrt(length(names)))
  num_cols <- length(names_datasets)#ceiling(length(names)/num_rows)

  # Depending on whether results are to be shown to the user, create a pdf drawing object or a visible canvas
  if (showResults){
    grDevices::dev.new()
  }else{
    grDevices::pdf(file.path(out_dir,'sig_expr_barcharts_NA_values.pdf'),width=4*(length(names_datasets)),height=4*(length(names_sigs)))#width=10,height=10)

  }

  #sets up the graphics drawing area
  graphics::par(mfrow=c(num_rows,num_cols),cex=0.7, cex.axis=0.5)

  #the following is to determine the font size of the title; need to know max length of characters in title
  max_line_length <- -999
  for(k in 1:length(names_sigs)){
    for (i in 1:length(names_datasets)){
      if(max_line_length < nchar(paste0(names_datasets[i],' ',names_sigs[k]))){
        max_line_length <- nchar(paste0(names_datasets[i],' ',names_sigs[k]))
      }
    }
  }

  for(k in 1:length(names_sigs)){
   gene_sig <- gene_sigs_list[[names_sigs[k]]] #load the signature
   if(is.matrix(gene_sig))
    gene_sig = as.vector(gene_sig);
  
    for (i in 1:length(names_datasets)){
      #calculate the porportion of non-NA expression data in the matrix
      # genes_expr <- mRNA_expr_matrix[[names_datasets[i]]][gene_sig,]
      data.matrix = mRNA_expr_matrix[[names_datasets[i]]] #load the expression data
      inter <- intersect(gene_sig, row.names(data.matrix))
      genes_expr <- data.matrix[inter,] #subset to only the genes present in the data
      gene_expr_vals <- (rowSums(is.na(genes_expr)) / dim(genes_expr)[2]) #count the proportion of NA values
      gene_expr_vals[setdiff(gene_sig, row.names(data.matrix))] <- 1 #add back in the genes that were never present anyways
      gene_expr_vals <- -sort(-gene_expr_vals)
      #make the barplot; if all are zero, we manually set the limits for the barplot
      if(max(gene_expr_vals)==0){
        bar_expr <- graphics::barplot(gene_expr_vals,xlab="Signature Gene IDs",ylab="Proportion of NA expression ",main=paste0("Signature gene expression\n",names_datasets[i],' ',names_sigs[k]), axisnames=F,axis=F,ylim=c(0,1),cex.main=min(1,4*12/max_line_length), border = NA)
      }else{
        bar_expr <- graphics::barplot(gene_expr_vals,xlab="Signature Gene IDs",ylab="Proportion of NA expression ",main=paste0("Signature gene expression\n",names_datasets[i],' ',names_sigs[k]), axisnames=F,axis=F,cex.main=min(1,4*12/max_line_length), border = NA)

      }
      graphics::text(bar_expr, graphics::par("usr")[3], labels = names(gene_expr_vals), srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=max(min(0.5,(0.5*4*12)/(sqrt(2) * length(gene_expr_vals))),0.06))
      graphics::axis(2)
      radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]['med_prop_na'] <- stats::median(1-gene_expr_vals) #store the median NA proportion for the radarplot at the end

    }
  }
  # graphics output
  if(showResults){
    grDevices::dev.copy(grDevices::pdf,file.path(out_dir,'sig_expr_barcharts_NA_values.pdf'),width=4*(length(names_datasets)),height=4*(length(names_sigs)))#width=10,height=10)
  }
  if(grDevices::dev.cur()!=1){
    g <- grDevices::dev.off() # to reset the graphics pars to defaults
  }

  # create new canvas
  if (showResults){
    grDevices::dev.new()
  }else{
    grDevices::pdf(file.path(out_dir,'sig_expr_barcharts.pdf'),width=4*(length(names_datasets)),height=4*(length(names_sigs)))#width=10,height=10)
  }

  #set graphics parameters
  graphics::par(mfrow=c(num_rows,num_cols),cex=0.7, cex.axis=0.5)
  #decide what the cutoff for defining an 'expressed' gene is; if user has specified thresholds, use those, otherwise take the median of all
  if (length(thresholds) == 0) {
    thresholds <- rep(0,length(names_datasets))
    for ( i in 1:length(names_datasets)){
      thresholds[i] <- stats::median(unlist(stats::na.omit(mRNA_expr_matrix[[names_datasets[i]]])))
    }
    #names(thresholds) <- names_datasets
  }
  # assign names to the threshold variable so we can call it by dataset
  if(length(names(thresholds))==0){
    names(thresholds) <- names_datasets
  }
  #the following loops through every signature and dataset and computes the proportion of expressed genes for each case
  for (k in 1:length(names_sigs)){
    gene_sig <- gene_sigs_list[[names_sigs[k]]] #load signature
    if(is.matrix(gene_sig)){gene_sig = as.vector(gene_sig);}
    for (i in 1:length(names_datasets)){
      #calculate the porportion of expressed data in the matrix
      # genes_expr <- mRNA_expr_matrix[[names_datasets[i]]][gene_sig,]
      data.matrix = mRNA_expr_matrix[[names_datasets[i]]] #load in the data
      inter <- intersect(gene_sig, row.names(data.matrix)) #only consider the genes in the dataset
      genes_expr <- data.matrix[inter,] #subset the matrix to just the gene signature
      gene_expr_vals <- 1 - ((rowSums(genes_expr < thresholds[i])) / (dim(genes_expr)[2])) #figure out what proportion of samples express gene above threshold
      gene_expr_vals[setdiff(gene_sig, row.names(data.matrix))] <- 0 
      gene_expr_vals <- sort(gene_expr_vals) 
      radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]['med_prop_above_med'] <- stats::median(gene_expr_vals) #store median value for final radar plot
      #create bar graph
      bar_expr <- graphics::barplot(gene_expr_vals,xlab="Signature Gene IDs",ylab="Proportion with expression above threshold",main=paste0("Signature gene expression\n",names_datasets[i],' ',names_sigs[k]), axisnames=F,axis=F,cex.main=min(1,4*12/max_line_length), border = NA)
      graphics::text(bar_expr, graphics::par("usr")[3], labels = names(gene_expr_vals), srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=max(min(0.5,(0.5*4*12)/(sqrt(2) * length(gene_expr_vals))),0.06))
      graphics::axis(2)
    }
  }
  #plot saving commands
  if(showResults){
    grDevices::dev.copy(grDevices::pdf,file.path(out_dir,'sig_expr_barcharts.pdf'),width=4*(length(names_datasets)),height=4*(length(names_sigs)))#width=10,height=10)
  }
  if(grDevices::dev.cur()!=1){
    g <- grDevices::dev.off() # to reset the graphics pars to defaults
  }
  
  #create new plot
  if (showResults){
    grDevices::dev.new()
  }else{
    grDevices::pdf(file.path(out_dir,'sig_expr_density_plots.pdf'),width=4*(length(names_datasets)),height=4*(length(names_sigs)))#width=10,height=10)
  }
  #set up the canvas in a grid
  #the following computes each of the density curves for expression
  graphics::par(mfrow=c(num_rows,num_cols))
  for (k in 1:length(names_sigs)){
    gene_sig <- gene_sigs_list[[names_sigs[k]]] #load signature
    if(is.matrix(gene_sig)){gene_sig = as.vector(gene_sig);}
    for (i in 1:length(names_datasets)){
      #calculate the porportion of nonzero expression data in the matrix
      # genes_expr <- mRNA_expr_matrix[[names_datasets[i]]][gene_sig,]
      data.matrix = mRNA_expr_matrix[[names_datasets[i]]] #load dataset
      inter <- intersect(gene_sig, row.names(data.matrix)) #only look at genes that are in the dataset
      genes_expr <- data.matrix[inter,] 
      gene_expr_vals <- 1 - (rowSums(genes_expr < thresholds[i]) / (dim(genes_expr)[2])) #take proportion of expression
      gene_expr_vals[setdiff(gene_sig, row.names(data.matrix))] <- 0 
      #make the density plot
      graphics::plot(stats::density(stats::na.omit(gene_expr_vals),adjust=0.25),main=paste0("Signature gene expression\n",names_datasets[i], ' ',names_sigs[k]),ylab="Density",cex.main=min(1,3.5*10/max_line_length))
    }
  }
  #save the plot
  if(showResults){
    grDevices::dev.copy(grDevices::pdf,file.path(out_dir,'sig_expr_density_plots.pdf'),width=4*(length(names_datasets)),height=4*(length(names_sigs)))#width=10,height=10)
  }
  if(grDevices::dev.cur()!=1){
    g <- grDevices::dev.off() # to reset the graphics pars to defaults
  }
  #write to log file
  cat('Expression and density graphs created successfully.\n', file=file)

  #here we calculate the outputs again just to put in text files
  if(!dir.exists(file.path(out_dir,'expression_tables'))){
    dir.create(file.path(out_dir,'expression_tables'))
  }

  for (k in 1:length(names_sigs)){
    gene_sig <- gene_sigs_list[[names_sigs[k]]] #load signature
    if(is.matrix(gene_sig)){gene_sig = as.vector(gene_sig);}
    for (i in 1:length(names_datasets)){
      #calculate the porportion of nonzero expression data in the matrix
      # genes_expr <- mRNA_expr_matrix[[names_datasets[i]]][gene_sig,]
      data.matrix = mRNA_expr_matrix[[names_datasets[i]]] #load dataset
      inter <- intersect(gene_sig, row.names(data.matrix)) #only look at genes that are in the dataset
      genes_expr <- data.matrix[inter,] 
      gene_expr_vals <- 1 - (rowSums(genes_expr < thresholds[i]) / (dim(genes_expr)[2])) #take proportion of expression
      gene_expr_vals[setdiff(gene_sig, row.names(data.matrix))] <- 0 #add back in the genes that were never present anyways

      gene_expr_vals_na <- (rowSums(is.na(genes_expr)) / dim(genes_expr)[2]) #count the proportion of NA values
      gene_expr_vals_na[setdiff(gene_sig, row.names(data.matrix))] <- 1 #add back in the genes that were never present anyways

      output_table <- cbind(gene_expr_vals,gene_expr_vals_na)
      colnames(output_table) <- c('Proportion_above_threshold','NA proportion')
      utils::write.table(output_table,file=file.path(out_dir,'expression_tables', paste0('expression_table_',names_sigs[k],'_',names_datasets[i],'.txt')),quote=F,sep='\t')
    }
  }

  radar_plot_values #return the values we will use in the radarplot
}
