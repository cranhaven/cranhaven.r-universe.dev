######################################################################################
#compute_without_plots.R
#@description Sets together the different functions of the tool excluding
# the plot production. It is used for the computation of the negative controls.
#
#@param gene_sigs_list A list of genes representing the gene signature to be tested.
#@param mRNA_expr_matrix A list of expression matrices
#@param names_sigs The names of the gene signatures (e.g. Hypoxia, Invasiveness), one name per each signature in gene_sigs_list.
#@param names_datasets The names of the different datasets contained in mRNA_expr_matrix
#@param covariates A list containing a sub-list of 'annotations' and 'colors' which contains the annotation matrix for the given dataset and the associated colours with which to plot in the expression heatmap
#@param thresholds A list of thresholds to be considered for each data set, default is median of the data set. A gene is considered expressed if above the threshold, non-expressed otherwise. One threshold per dataset, in the same order as the dataset list.
#@param out_dir A path to the directory where the resulting output files are written
#@param showResults Tells if open dialog boxes showing the computed results. Default is FALSE
#@param origin Tells if datasets have come from different labs/experiments/machines. Is a vector of characters, with same character representing same origin. Default is assumption that all datasets come from the same source.
#@keyword compute_without_plots
#
#@author Alessandro Barberis
######################################################################################
.compute_without_plots = function(gene_sigs_list, mRNA_expr_matrix,names_sigs=NULL,names_datasets=NULL , covariates=NULL, thresholds=NULL, out_dir = file.path('~', "sigQC"), showResults = FALSE,origin=NULL){
  if(!dir.exists(out_dir)){
      dir.create(out_dir,recursive=T)
  }
  # utils::write.table('',file=file.path(out_dir, "log.log"))
  #LOG file path
  logfile.path = file.path(out_dir, "log.log")
  #Log conn
  log.con = file(logfile.path, open = "a") #open the logfile
  #run each of the sub functions
  cat(paste("LOG FILE CREATED: ",Sys.time(), sep=""), file=log.con, sep="\n") #start the log file

  radar_plot_values <- list();
  #set up the variable that will hold values for the final radarplot
  for(i in 1:length(names_sigs)){
    radar_plot_values[[names_sigs[i]]] <- list();
  }

  tryCatch({

    tryCatch(radar_plot_values <- eval_var_loc_noplots(gene_sigs_list,names_sigs, mRNA_expr_matrix,names_datasets,out_dir,file=log.con,showResults,radar_plot_values),
             error=function(err){
               cat(paste0("Error occurred in eval_var_loc_noplots: ",err), file=log.con, sep="\n")
             })
    tryCatch(radar_plot_values <- eval_expr_loc_noplots(gene_sigs_list,names_sigs,mRNA_expr_matrix,names_datasets,thresholds, out_dir,file=log.con,showResults,radar_plot_values ),
             error=function(err){
               cat(paste0("Error occurred in eval_expr_loc_noplots: ",err), file=log.con, sep="\n")
             })
    tryCatch(radar_plot_values <- eval_compactness_loc_noplots(gene_sigs_list,names_sigs,mRNA_expr_matrix,names_datasets,out_dir,file=log.con,showResults,radar_plot_values,logged=T,origin ),
             error=function(err){
               cat(paste0("Error occurred during the evaluation of compactness: ",err), file=log.con, sep="\n")
             })
    tryCatch({radar_plot_values <- compare_metrics_loc_noplots(gene_sigs_list,names_sigs,mRNA_expr_matrix,names_datasets,out_dir,file=log.con,showResults,radar_plot_values )},
             error=function(err){
               #  print(paste0("Error, likely due to inability to calculate PCA, because of missing values: ", err))
               cat(paste0("Error occurred, likely due to inability to calculate PCA, because of missing values:  ",err), file=log.con, sep="\n")
             })
    tryCatch({radar_plot_values <- eval_stan_loc_noplots(gene_sigs_list,names_sigs,mRNA_expr_matrix,names_datasets,out_dir,file=log.con,showResults,radar_plot_values )},
             error=function(err){
               cat(paste0("Error occurred in eval_stan_loc_noplots: ",err), file=log.con, sep="\n")
             })

    tryCatch(make_radar_chart_loc_noplots(radar_plot_values,showResults,names_sigs, names_datasets,out_dir,file=log.con),
             error=function(err){
               cat(paste0("Error occurred in make_radar_chart_loc_noplots: ",err), file=log.con, sep="\n")
             })

  }, error = function(err) {
    #cat("", file=log.con, sep="\n")
    #cat(paste(Sys.time(),"Errors occurred during in sigQcNegativeControl:", err, sep=" "), file=log.con, sep="\n")
    #stop("Errors occurred during the computation of negative controls")
  }, finally = {
    #cat("---------------------------------------------------------------------------", file=log.con, sep="\n")
    close(log.con)
  })#END tryCatch
}

eval_var_loc_noplots <- function(gene_sigs_list,names_sigs, mRNA_expr_matrix,names_datasets, out_dir = '~',file=NULL,showResults = FALSE,radar_plot_values){
  # calculate the number of rows and columns in the image
  num_rows <- length(names_sigs)#ceiling(sqrt(length(names)))
  num_cols <- length(names_datasets)#ceiling(length(names)/num_rows)
  #set up the plotting area

  gene_sig_mean_sd_table <- list() #this is to store the lists of tables that will be output to file
    for(k in 1:length(names_sigs)){
    gene_sig <- gene_sigs_list[[names_sigs[k]]] #load the gene signature
    for (i in 1:length(names_datasets)){
      data.matrix = mRNA_expr_matrix[[names_datasets[i]]] #load the dataset
      inter <- intersect(gene_sig[,1], row.names(data.matrix)) #consider genes only present in the dataset

      sd_genes <- as.matrix(apply(data.matrix,1,function(x){stats::sd(as.numeric(x),na.rm=T) })) #compute the standard deviation of the genes
      mean_genes <- as.matrix(apply(mRNA_expr_matrix[[names_datasets[i]]],1,function(x) {mean(as.numeric(x),na.rm=T)})) #comptue the mean of the genes
      #stats::median(stats::na.omit(sd_genes[inter, 1]))/(stats::median(stats::na.omit(sd_genes)) +stats::median(stats::na.omit(sd_genes[inter, 1])))
      radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]['sd_median_ratio'] = stats::median(stats::na.omit(sd_genes[inter,1]))/(stats::median(stats::na.omit(sd_genes)) +stats::median(stats::na.omit(sd_genes[inter,1]))) #fractional ratio of medians of standard deviations of sig genes vs all genes
      radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]['abs_skewness_ratio'] = abs(moments:: skewness(stats::na.omit(mean_genes[inter, 1])))/(abs(moments:: skewness(stats::na.omit(mean_genes))) +  abs(moments:: skewness(stats::na.omit(mean_genes[inter, 1])))) #fractional ratio of skewness of mean expressions

      #calculate and draw the quantiles of the standard deviation
      quants_sd <- stats::quantile(sd_genes*is.finite(sd_genes),probs=c(0.1,0.25,0.5,0.75,0.9),na.rm=T)
      gene_sig_mean_sd_table[[names_sigs[k]]][[names_datasets[i]]] <- cbind(mean_genes[inter, 1],sd_genes[inter, 1]) #store values for the gene signature table
      colnames(gene_sig_mean_sd_table[[names_sigs[k]]][[names_datasets[i]]]) <- c("Mean","SD")
    }
  }

  #now let's output the tables for mean vs sd for the gene signature, one for each dataset and signature considered
  # dir.create(file.path(out_dir,'mean_sd_tables'))
  # for(k in 1:length(names_sigs)){
  #   for (i in 1:length(names_datasets)){
  #     utils::write.table(gene_sig_mean_sd_table[[names_sigs[k]]][[names_datasets[i]]],file=file.path(out_dir,'mean_sd_tables', paste0('mean_sd_table_',names_sigs[k],'_',names_datasets[i],'.txt')),quote=F,sep='\t')
  #   }
  # }
  # cat('Mean vs SD tables written to file successfully.\n', file=file)

  #the following is the code for computing coefficient of variation across signature genes and all genes
  for(k in 1:length(names_sigs)){
    gene_sig <- gene_sigs_list[[names_sigs[k]]] #load gene signature
    for (i in 1:length(names_datasets)){
      data.matrix = mRNA_expr_matrix[[names_datasets[i]]] #load dataset
      inter <- intersect(gene_sig[,1], row.names(data.matrix)) #only use genes in the dataset
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

    }
  }
  # dev.copy(pdf,paste0(out_dir,'/sig_expr_var.pdf'))

  # dev.off()
  radar_plot_values #return values for the final plot
}

eval_expr_loc_noplots <- function(gene_sigs_list,names_sigs, mRNA_expr_matrix,names_datasets, thresholds = NULL, out_dir = '~',file=NULL,showResults = FALSE,radar_plot_values){
  #calculate the number of rows and columns in the final plot
  num_rows <- length(names_sigs)#ceiling(sqrt(length(names)))
  num_cols <- length(names_datasets)#ceiling(length(names)/num_rows)

  for(k in 1:length(names_sigs)){
    gene_sig <- gene_sigs_list[[names_sigs[k]]] #load the signature

    for (i in 1:length(names_datasets)){
      #calculate the porportion of non-NA expression data in the matrix
      # genes_expr <- mRNA_expr_matrix[[names_datasets[i]]][gene_sig,]
      data.matrix = mRNA_expr_matrix[[names_datasets[i]]] #load the expression data
      inter <- intersect(gene_sig[,1], row.names(data.matrix))
      genes_expr <- data.matrix[inter,] #subset to only the genes present in the data
      gene_expr_vals <- (rowSums(is.na(genes_expr)) / dim(genes_expr)[2]) #count the proportion of NA values
      gene_expr_vals[setdiff(gene_sig[,1], row.names(data.matrix))] <- 1 #add back in the genes that were never present anyways
      gene_expr_vals <- -sort(-gene_expr_vals)
      radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]['med_prop_na'] <- stats::median(1-gene_expr_vals) #store the median NA proportion for the radarplot at the end

    }
  }

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
    for (i in 1:length(names_datasets)){
      #calculate the porportion of expressed data in the matrix
      # genes_expr <- mRNA_expr_matrix[[names_datasets[i]]][gene_sig,]
      data.matrix = mRNA_expr_matrix[[names_datasets[i]]] #load in the data
      inter <- intersect(gene_sig[,1], row.names(data.matrix)) #only consider the genes in the dataset
      genes_expr <- data.matrix[inter,] #subset the matrix to just the gene signature
      gene_expr_vals <- 1 - ((rowSums(genes_expr < thresholds[i])) / (dim(genes_expr)[2])) #figure out what proportion of samples express gene above threshold
      gene_expr_vals[setdiff(gene_sig[,1], row.names(data.matrix))] <- 0
      gene_expr_vals <- sort(gene_expr_vals)
      radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]['med_prop_above_med'] <- stats::median(gene_expr_vals) #store median value for final radar plot
    }
  }

  radar_plot_values #return the values we will use in the radarplot
}

eval_compactness_loc_noplots <- function(gene_sigs_list,names_sigs, mRNA_expr_matrix, names_datasets, out_dir = '~',file=NULL,showResults = FALSE,radar_plot_values,logged=T,origin=NULL){

  #the following actually draws the density plots for each signature and dataset
  for(k in 1:length(names_sigs)){
    gene_sig <- gene_sigs_list[[names_sigs[k]]] #load the signature
    for (i in 1:length(names_datasets) ){
      data.matrix = mRNA_expr_matrix[[names_datasets[i]]] #load the datasets
      inter <- intersect(gene_sig[,1], row.names(data.matrix)) #consider only the genes in the dataset 

      autocors <- stats::cor(t(stats::na.omit(data.matrix[inter,])),method='spearman') #calculate autocorrelation

      if(dim(autocors)[1] > 1){
        radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]['autocor_median'] <- stats::median(autocors,na.rm=T) #store the median autocorrelation for the final radar plot
      }else{
        radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]['autocor_median'] <- 0#stats::median(stats::na.omit(autocors)) #store the median autocorrelation for the final radar plot
      }
    }
  }

  radar_plot_values #return the radar plot values
}

compare_metrics_loc_noplots <- function(gene_sigs_list,names_sigs, mRNA_expr_matrix, names_datasets, out_dir = '~',file=NULL,showResults = FALSE,radar_plot_values){

  for(k in 1:length(names_sigs)){
    #for each signature we will make a separate file comparing the datasets
    gene_sig <- gene_sigs_list[[names_sigs[k]]] # load the gene signature

    for (i in 1:length(names_datasets)){
      # now we can loop over the datasets for the plot and generate the metrics for every dataset with this signature
      data.matrix = mRNA_expr_matrix[[names_datasets[i]]] #load the data
      data.matrix[!(is.finite(as.matrix(data.matrix)))] <- NA #ensure that the data is not infintie
      inter = intersect(gene_sig[,1],rownames(data.matrix)) #consider only the genes actually present in the data

      med_scores <- apply(data.matrix[inter,],2,function(x){stats::median(stats::na.omit(x))}) #compute median
      mean_scores <- apply(data.matrix[inter,],2,function(x){mean(stats::na.omit(x))}) #compute mean

      pca1_scores <- NULL

      tryCatch({
        pca1_scores <- stats::prcomp(stats::na.omit(t(data.matrix[inter,])),retx=T) #compute PCA1

      },error=function(e){
        pca1_scores <<- NULL
        cat(paste0("There was an error:  ",names_datasets[i]," ", names_sigs[k]," ", e,'\n'), file=file)
      })

      if(length(pca1_scores) > 1){#!is.null(pca1_scores)){
        props_of_variances <- pca1_scores$sdev^2/(sum(pca1_scores$sdev^2)) #for the scree plot
        pca1_scores <- pca1_scores$x[,1] #takes the actual PCA1 scores
        common_score_cols <- intersect(names(med_scores),intersect(names(mean_scores),names(pca1_scores))) #ensures we have the same samples for each plot
      }else{
        common_score_cols <- intersect(names(med_scores),names(mean_scores))
      }

      if(length(common_score_cols) > 1){
        rho <- stats::cor(med_scores[common_score_cols],mean_scores[common_score_cols],method='spearman')
        rho_mean_med <- rho
      }else{
        rho_mean_med <- 0
      }
      if (length(pca1_scores) > 1){#(!is.null(pca1_scores)){
        #plotting for the mean-pca1
        rho <- stats::cor(mean_scores[common_score_cols],pca1_scores[common_score_cols],method='spearman')
        rho_mean_pca1 <- rho
        rho <- stats::cor(pca1_scores[common_score_cols],med_scores[common_score_cols],method='spearman')
        rho_pca1_med <- rho
      }else{
        rho_mean_pca1 <- 0
        rho_pca1_med <- 0

      }

      #stores values that will be used in the radarplot
      radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]['rho_mean_med'] <- rho_mean_med
      radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]['rho_pca1_med'] <- rho_pca1_med
      radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]['rho_mean_pca1'] <- rho_mean_pca1

      if(length(pca1_scores) > 1){#(!is.null(pca1_scores)){
        #draws the scree plot
        bars_plot <- props_of_variances[1:min(10,length(props_of_variances))]
        radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]['prop_pca1_var']<- bars_plot[1]
      }else{
        radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]['prop_pca1_var']<- 0
      }
    }
  }
  cat('Metrics compared successfully.\n', file=file) #output to log
  radar_plot_values #returns the radarplot values
}

eval_stan_loc_noplots <- function(gene_sigs_list, names_sigs,mRNA_expr_matrix, names_datasets,out_dir = '~',file=NULL,showResults = FALSE,radar_plot_values ){
  for(k in 1:length(names_sigs)){
    gene_sig <- gene_sigs_list[[names_sigs[k]]] #load in the gene signature
    for (i in 1:length(names_datasets)){
      data.matrix = mRNA_expr_matrix[[names_datasets[i]]] #load in the data matrix
      inter <- intersect(gene_sig[,1], row.names(data.matrix)) #make sure the genes are present in the dataset
      # the following takes the expressions of the gene signature and computes standardised expressions across samples
      z_transf_mRNA <- data.matrix[inter,]
      for (gene in inter){
        z_transf_mRNA[gene,] <- (as.numeric(z_transf_mRNA[gene,]) - mean(as.numeric(z_transf_mRNA[gene,]),na.rm=T)) / stats::sd(as.numeric(z_transf_mRNA[gene,]),na.rm=T)
      }
      z_transf_scores <- apply(z_transf_mRNA[inter,],2,function(x) {stats::median(stats::na.omit(x))}) #now take the median of the z-transformed expression
      med_scores <- apply(data.matrix[inter,],2,function(x){ stats::median(stats::na.omit(x))}) #take the median of the non-standardised expression

      rho <- stats::cor(med_scores,z_transf_scores,method='spearman')
      radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]['standardization_comp'] <- rho #store the value for the final radar plot

    }
  }
  cat('Standardisation compared successfully.\n', file=file) #output to log
  radar_plot_values #return the radarplot values
}

make_radar_chart_loc_noplots <- function(radar_plot_values,showResults = FALSE,names_sigs,names_datasets, out_dir = '~',file){
  radar_plot_mat <- c()

  # first we need to flatten this list into a matrix that we can use with the radar plot plotting function

  all_metrics <- c('sd_median_ratio','abs_skewness_ratio','prop_top_10_percent','prop_top_25_percent',
                   'prop_top_50_percent','coeff_of_var_ratio','med_prop_na','med_prop_above_med',
                   'autocor_median','rho_mean_med','rho_pca1_med','rho_mean_pca1',
                   'prop_pca1_var','standardization_comp')

  #ensure that all metrics are accounted for
  for(k in 1:length(names_sigs)){
    for(i in 1:length(names_datasets)){
      diff_names <- base::setdiff(all_metrics,names(radar_plot_values[[names_sigs[k]]][[names_datasets[i]]]))
      if(length(diff_names)>0){
        for(j in 1:length(diff_names)){
          radar_plot_values[[names_sigs[k]]][[names_datasets[i]]][diff_names[j]] <- 0
        }
      }
    }
  }

  for(k in 1:length(names_sigs)){
    t <- lapply(radar_plot_values[[names_sigs[k]]],function(x) radar_plot_mat <<- rbind(radar_plot_mat,x))
  }
  radar_plot_mat <- rbind(rep(1,length(radar_plot_values[[1]][[1]])),rep(0,length(radar_plot_values[[1]][[1]])),radar_plot_mat) #we must store the top 2 rows of the radar plot matrix as the max and min of each ray of the plot
  radar_plot_mat <- abs(radar_plot_mat) #consider only absolute value of correlation coefficients

  legend_labels <- c() #set up the legend

  #the following creates the legend
  for(k in 1:length(names_sigs) ){
    for(i in 1:length(names_datasets)){
      legend_labels <- c(legend_labels,paste0(names_datasets[i],' ',names_sigs[k],' (XXXX)'))
    }
  }
  row.names(radar_plot_mat) <- c('max','min',legend_labels)

  #find the max number of characters in the title for legend fontsize))

  # compute the area ratios
  areas <- c()
  #first we calculate the area of each dataset/gene signature drawn on the radar plot
  for (i in 3:dim(radar_plot_mat)[1]){
    areas<- c(areas,sum(sapply(1:length(radar_plot_mat[i,]),function(x) if(x < length(radar_plot_mat[i,])){radar_plot_mat[i,x] * radar_plot_mat[i,x+1]}else{radar_plot_mat[i,x]* radar_plot_mat[i,1]})))
  }
  areas <- areas /dim(radar_plot_mat)[2] #then we consider the area ratio of this to the total area of the radar plot polygon

  #next, add in the area ratio values to the legend labels
  count <-1
  legend_labels <-c()
  for(k in 1:length(names_sigs) ){
    for(i in 1:length(names_datasets)){
      legend_labels <- c(legend_labels,paste0(names_datasets[i],' ',names_sigs[k],' (',format(areas[count],digits=2),')'))
      count <- count + 1
    }
  }
  # graphics::layout(rbind(1,2), heights=c(7,1))  # put legend on bottom 1/8th of the chart
  #the following draws the radarplot
  legend_labels <- legend_labels[order(-areas)]
  # graphics::par(mar=c(0, 0, 0, 0))

  # graphics::plot.new()
  #then we output the legend

  #output the radarchart table to file
  if(!dir.exists(file.path(out_dir,'radarchart_table'))){
    dir.create(file.path(out_dir,'radarchart_table'))
  }

  #the following creates the radarplot rownames
  radarplot_rownames <- c()
  for(k in 1:length(names_sigs) ){
    for(i in 1:length(names_datasets)){
      radarplot_rownames <- c(radarplot_rownames,paste0(gsub(' ','.', names_datasets[i]),'_',gsub(' ','.', names_sigs[k])))
    }
  }

  radar_plot_mat <- radar_plot_mat[3:(dim(radar_plot_mat)[1]),]
  radar_plot_mat <- as.matrix(radar_plot_mat)

  if(length(radarplot_rownames)==1){
    new_colnames <- rownames(radar_plot_mat)
    dim(radar_plot_mat) <- c(1,length(radar_plot_mat))
    colnames(radar_plot_mat) <- new_colnames
  }
  row.names(radar_plot_mat) <- radarplot_rownames

  utils::write.table(radar_plot_mat,file=file.path(out_dir,'radarchart_table', paste0('radarchart_table','.txt')),quote=F,sep='\t',row.names=T,col.names=T)

  cat('Radar chart made successfully.\n', file=file) #output to log
}
