# eval_struct_loc.R
#
# This function creates the plots to evaluate for signature structure. Produces plots for the
# expression of the signature with hierarchical clustering as well as biclustering plots on binarized data and on non-binarized data
# @param gene_sigs_list A list of genes representing the gene signature to be tested.
# @param names_sigs The names of the gene signatures (one name per gene signature, in gene_sigs_list)
# @param mRNA_expr_matrix A list of expression matrices
# @param names_datasets The names of the different datasets contained in mRNA_expr_matrix
# @param covariates A list containing a sub-list of 'annotations' and 'colors' which contains the annotation matrix for the given dataset and the associated colours with which to plot in the expression heatmap
# @param out_dir A path to the directory where the resulting output files are written
# @param file File representing the log file where errors can be written
# @param showResults Tells if open dialog boxes showing the computed results. Default is FALSE
# @param radar_plot_values A list of values that store computations that will be used in the final summary radarplot
# @keywords eval_struct_loc

eval_struct_loc <- function(gene_sigs_list,names_sigs, mRNA_expr_matrix,names_datasets,covariates, out_dir = '~',file=NULL,showResults = FALSE,radar_plot_values){
  # library(gplots)
  # require(biclust)
  # require(ComplexHeatmap)
  # par(cex.main=0.7,cex.lab = 0.6,oma=c(2,2,3,2),mar=c(2,2,2,2))

  #find the max number of characters in the title for fontsize purposes
  max_title_length <- -999
  for(k in 1:length(names_sigs)){
    for( i in 1:length(names_datasets)){
      if(max_title_length < nchar(paste0(names_datasets[i],' ',names_sigs[k]))){
        max_title_length <- nchar(paste0(names_datasets[i],' ',names_sigs[k]))
      }
    }
  }
  #Next we loop through and generate the heatmaps for expression

  #first we do a check to ensure that all heatmpas have the same genes as rows and add NA values if not
  #first let's get all possible rows expressed of the signature across all samples
  all_row_names <- list() #stores the unique rownames for each signature
  for (k in 1:length(names_sigs)){
    all_row_names[[names_sigs[k]]] <- c()
    gene_sig <- gene_sigs_list[[names_sigs[k]]] #load the signature
    if(is.matrix(gene_sig)){gene_sig = as.vector(gene_sig);}
    for (i in 1:length(names_datasets)){

      data.matrix = mRNA_expr_matrix[[names_datasets[i]]] #load the dataset
      inter <- intersect(gene_sig, row.names(data.matrix)) #consider only the genes present in the dataset

      all_row_names[[names_sigs[k]]] <- c(all_row_names[[names_sigs[k]]],inter)
    }
     all_row_names[[names_sigs[k]]] <- unique(all_row_names[[names_sigs[k]]])
  }
  #now that we know the unique rownames for each signature, we should go through and generate the heatmap matrices with appended min values
  sig_scores_all_mats <- list() #this will be the list of signature score matrices
  for (k in 1:length(names_sigs)){
    sig_scores_all_mats[[names_sigs[k]]] <- list()
    gene_sig <- gene_sigs_list[[names_sigs[k]]] #load the signature
    if(is.matrix(gene_sig)){gene_sig = as.vector(gene_sig);}
    for (i in 1:length(names_datasets)){
      data.matrix = mRNA_expr_matrix[[names_datasets[i]]] #load the dataset
      inter <- intersect(gene_sig, row.names(data.matrix)) #consider only the genes present in the dataset
      sig_scores <- (as.matrix(data.matrix[inter,])) #compute the signature scores
      #now let's see how many rows of NA values we need to add
      rows_needed <- setdiff(all_row_names[[names_sigs[k]]],inter)
      if(length(rows_needed >0)){
        sig_scores <- rbind(sig_scores,matrix(min(sig_scores),nrow=length(rows_needed),ncol=dim(sig_scores)[2]))
        row.names(sig_scores) <- c(inter,rows_needed)
      }
      sig_scores_all_mats[[names_sigs[k]]][[names_datasets[i]]] <- sig_scores#[gene_sig[,1],]
    }
  }

  for(k in 1:length(names_sigs)){
    gene_sig <- gene_sigs_list[[names_sigs[k]]] #load the signature
    if(is.matrix(gene_sig)){gene_sig = as.vector(gene_sig);}
    hmaps <- sapply(1:length(names_datasets),function(i) {
      #this is a subroutine to create a list of heatmaps stored in hmaps that can be plotted

      # data.matrix = mRNA_expr_matrix[[names_datasets[i]]] #load the dataset
      # inter <- intersect(gene_sig[,1], row.names(data.matrix)) #consider only the genes present in the dataset

      # sig_scores <- (as.matrix(data.matrix[inter,])) #compute the signature scores
      # #  print(sig_scores)
      sig_scores <- sig_scores_all_mats[[names_sigs[k]]][[names_datasets[i]]]
      tryCatch({
        if (length(covariates[[names_datasets[i]]]) ==0){

          #here we will set the parameters for the font size etc in the heatmaps

          dim.pdf = dim(sig_scores); #size of the heatmap matrix
          # w = dim.pdf[2];
          h = dim.pdf[1]; #number of rows
          if(h<20){ #if less than 20 rows, we can have larger row titles, otherwise they need to be scaled.
            row_names.fontsize = 12
          }else{
            row_names.fontsize=5/log10(h)
          }
          #the following creates the heatmap
          ans_hmap <- ComplexHeatmap::Heatmap((sig_scores),show_column_dend = F,
                                              show_column_names = F,
                                              name=names_datasets[i],
                                              heatmap_legend_param = list(title = names_datasets[i], color_bar = "continuous",legend_direction='vertical'),
                                              column_title = paste0(names_datasets[i]),
                                              row_names_gp =  grid::gpar(fontsize = row_names.fontsize),
                                              row_title = 'Genes')#,
        }else{
          if (is.vector(covariates[[names_datasets[i]]][['annotations']])){
            #this is the case if the user has provided further annotations that they may want to plot alongside the top of the heatmap
            ha1 = ComplexHeatmap::HeatmapAnnotation(df = as.data.frame(covariates[[names_datasets[i]]][['annotations']][intersect(names(covariates[[names_datasets[i]]][['annotations']]),colnames(sig_scores))]),
                                                    col=covariates[[names_datasets[i]]][['colors']],
                                                    na_col="grey")#,
            #show_annotation_name = TRUE)#, col = list(type = c("a" = "red", "b" = "blue")
          }else{
            ha1 = ComplexHeatmap::HeatmapAnnotation(df = as.data.frame(covariates[[names_datasets[i]]][['annotations']][intersect(rownames(covariates[[names_datasets[i]]][['annotations']]),colnames(sig_scores)),]),
                                                    col=covariates[[names_datasets[i]]][['colors']],
                                                    na_col="grey",
                                                    which="column")#,
            #show_annotation_name = TRUE)#, col = list(type = c("a" = "red", "b" = "blue"),
          }
          #this is for setting fontsize of rownames of heatmap
          dim.pdf = dim(sig_scores);
          # w = dim.pdf[2];
          h = dim.pdf[1];
          if(h<20){
            row_names.fontsize = 12
          }else{
            row_names.fontsize=5/log10(h)
          }

          # row_names_gp = grid::gpar(fontsize = row_names.fontsize)
          #create the heatmap
          ans_hmap <- ComplexHeatmap::Heatmap((sig_scores),show_column_dend = F,
                                              show_column_names = F,
                                              name=names_datasets[i],
                                              heatmap_legend_param = list(title = names_datasets[i], color_bar = "continuous",legend_direction='vertical'),
                                              column_title = paste0(names_datasets[i]),
                                              row_names_gp = grid::gpar(fontsize = row_names.fontsize),
                                              row_title = 'Genes', top_annotation=ha1)#,

        }
      },
      error=function(err){
        #print(paste0("There was an error, likely due to NA values in ",names[i] ," : ", err))
        cat(paste0('Error when creating expression heatmaps for ',names_datasets[i],' ', names_sigs[k],': ',err,'\n'), file=file) #output to llog file

        graphics::plot.new()
        graphics::title(paste0('\n \n \n',names_datasets[i],' ',names_sigs[k])) #makes a graphics object
      })
      ans_hmap #return the heatmap to be added to the list for plotting

      #grab_grob()
    })

    #the following loop concatenates the list of heatmaps to be plotted into one heatmaplist (complexheatmap object), and then
    #cycles through each of the datasets for the clustering, plotting each case of heatmap
    for ( i in 1:length(names_datasets)){
      all_hmaps <- hmaps[[1]]

      if (length(hmaps) > 1){
        tmp <- lapply(hmaps[2:length(hmaps)],function(x) all_hmaps <<- ComplexHeatmap::add_heatmap(all_hmaps,x))
      }
      #creates graphic device
      if (showResults){
        grDevices::dev.new()
      } else{
        grDevices::pdf(file.path(out_dir, paste0('sig_eval_struct_clustering_',names_datasets[i],'_',names_sigs[k],'.pdf')),width=5*(length(names_datasets)),height=10)#paste0(out_dir,'/sig_autocor_hmps.pdf'))
      }
      #the following plots the heatmap list with the clustering done on the current dataset
      ComplexHeatmap::draw(all_hmaps,heatmap_legend_side = "left",annotation_legend_side = "left", main_heatmap = names_datasets[i])
      #saves the pdf
      if(showResults){
        grDevices::dev.copy(grDevices::pdf,file.path(out_dir, paste0('sig_eval_struct_clustering_',names_datasets[i],'_',names_sigs[k],'.pdf')),width=5*(length(names_datasets)),height=10)#paste0(out_dir,'/sig_autocor_hmps.pdf'))
      }
      cat('Expression heatmaps saved successfully.\n', file=file) #ouptuts to log file
      if(grDevices::dev.cur()!=1){
          g<- grDevices::dev.off() #closes graphics device
        }    
      }
  }
  # if(grDevices::dev.cur()!=1){
  #   g<- grDevices::dev.off() #closes graphics device
  # }

  # draw.heatmaps(hmaps,names)
  
  #---------before we do the biclustering plotting, let's determine whether there are ANY biclusters among the datasets/sig combo

  save_bicluster <- FALSE

  for (i in 1:(length(names_datasets)* length(names_sigs))){
      #here we compute all the biclusters

      #first convert the index i to an array index for the grid size length(names_datasets) by length(names_sigs)
      dataset_ind <- i %% length(names_datasets)
      if (dataset_ind == 0 ){
        dataset_ind <- length(names_datasets)
      }
      sig_ind <- ceiling(i/length(names_datasets))
      gene_sig <- gene_sigs_list[[names_sigs[sig_ind]]]

      if(is.matrix(gene_sig)){gene_sig = as.vector(gene_sig);}

      data.matrix = mRNA_expr_matrix[[names_datasets[dataset_ind]]] #load the data

      inter = intersect(gene_sig, row.names(data.matrix)) #only consider the genes present in the dataset

      sig_scores <- as.matrix(data.matrix[inter,])
      sig_scores[!is.finite(sig_scores)] <- NA #make sure the scores aren't infinte

      #need to standardize here the matrix before binarizing
      for (gene in inter){
        sig_scores[gene,] <- (as.numeric(sig_scores[gene,]) - mean(as.numeric(sig_scores[gene,]),na.rm=T)) / stats::sd(as.numeric(sig_scores[gene,]),na.rm=T)
      }

      #the following does the binarization of the matrix
      threshold <- min(stats::na.omit(t(sig_scores)))+(max(stats::na.omit(t(sig_scores)))-min(stats::na.omit(t(sig_scores))))/2
      x <- stats::na.omit(t(sig_scores)) #> threshold) * 1
      x[x<=threshold] <- 0
      x[x>threshold] <- 1
     
      #the following if statement creates the parameters for the biclustering algorithm; namely the number of columns to take in each
      #repetition of the biclustering algorithm (we use BCCC) - this is dependent on the size of the matrix

      if (dim(sig_scores)[2] > 40) {
        num_cols_chosen <- 20
      }else if (dim(sig_scores)[2] > 20){
        num_cols_chosen <- 10
      }else if (dim(sig_scores)[2] > 10){
        num_cols_chosen <- 5
      }else{
        num_cols_chosen <- 2
      }

      # Xmotif <- biclust(x, method=BCXmotifs(), number=50, alpha=0.5, nd=floor(dim(sig_scores)/num_cols_chosen), ns=num_cols_chosen, sd=floor(dim(sig_scores)/num_cols_chosen))
      Xmotif <- biclust::biclust(x, method=biclust::BCCC(), delta=1,alpha=1.5, number=50)# alpha=0.5, nd=floor(dim(sig_scores)/num_cols_chosen), ns=num_cols_chosen, sd=floor(dim(sig_scores)/num_cols_chosen))
      #the above performs the biclustering

      #if more than 1 bicluster then save_bicluster is true, otherwise false
      if(Xmotif@Number > 1){
        save_bicluster = TRUE
      }
    }

  #-----------------------------------------------------------------------------------------------------

  if(save_bicluster){
    #creates new plot
    grDevices::dev.new()
    
    graphics::par(cex.main=0.8,cex.lab = 0.8,oma=c(4,2,2,2),mar=c(4,4,4,4)) #sets graphics parameters

    hmaps <- lapply(1:(length(names_datasets)* length(names_sigs)),function(i) {
      #here we define a list of heatmaps for binarized biclustering data
      #first convert the index i to an array index for the grid size length(names_datasets) by length(names_sigs)
      dataset_ind <- i %% length(names_datasets)
      if (dataset_ind == 0 ){
        dataset_ind <- length(names_datasets)
      }
      sig_ind <- ceiling(i/length(names_datasets))
      gene_sig <- gene_sigs_list[[names_sigs[sig_ind]]]
      if(is.matrix(gene_sig)){gene_sig = as.vector(gene_sig);}
      data.matrix = mRNA_expr_matrix[[names_datasets[dataset_ind]]] #load the data
      inter = intersect(gene_sig, row.names(data.matrix)) #only consider the genes present in the dataset
      sig_scores <- as.matrix(data.matrix[inter,])
      sig_scores[!is.finite(sig_scores)] <- NA #make sure the scores aren't infinte

      #need to standardize here the matrix before binarizing
      for (gene in inter){
        sig_scores[gene,] <- (as.numeric(sig_scores[gene,]) - mean(as.numeric(sig_scores[gene,]),na.rm=T)) / stats::sd(as.numeric(sig_scores[gene,]),na.rm=T)
      }

      #the following does the binarization of the matrix
       threshold <- min(stats::na.omit(t(sig_scores)))+(max(stats::na.omit(t(sig_scores)))-min(stats::na.omit(t(sig_scores))))/2
      x <- stats::na.omit(t(sig_scores)) #> threshold) * 1
      x[x<=threshold] <- 0
      x[x>threshold] <- 1
      # print(paste0('thresh ',threshold))
      # x <- biclust::binarize(stats::na.omit(t(sig_scores)))#discretize(stats::na.omit(t(sig_scores)),nof=10,quant=F)

      #the following if statement creates the parameters for the biclustering algorithm; namely the number of columns to take in each
      #repetition of the biclustering algorithm (we use BCCC) - this is dependent on the size of the matrix

      if (dim(sig_scores)[2] > 40) {
        num_cols_chosen <- 20
      }else if (dim(sig_scores)[2] > 20){
        num_cols_chosen <- 10

      }else if (dim(sig_scores)[2] > 10){
        num_cols_chosen <- 5
      }else{
        num_cols_chosen <- 2
      }

      # Xmotif <- biclust(x, method=BCXmotifs(), number=50, alpha=0.5, nd=floor(dim(sig_scores)/num_cols_chosen), ns=num_cols_chosen, sd=floor(dim(sig_scores)/num_cols_chosen))
      Xmotif <- biclust::biclust(x, method=biclust::BCCC(), delta=1,alpha=1.5, number=50)# alpha=0.5, nd=floor(dim(sig_scores)/num_cols_chosen), ns=num_cols_chosen, sd=floor(dim(sig_scores)/num_cols_chosen))
      #the above performs the biclustering
      #if more than 1 bicluster then we output the heatmap with the bicluster on it
      #otherwise we just output the empty title
      if(Xmotif@Number > 1){
        biclust::heatmapBC(stats::na.omit(t(sig_scores)),bicResult=Xmotif,col = gplots::colorpanel(100,"blue","white","red"), xlab='Gene ID',ylab='Sample')
        #	biclust::heatmapBC(x,bicResult=Xmotif,col = colorpanel(100,"blue","white","red"), xlab='Gene ID',ylab='Sample')

        graphics::title(paste0('\n \n \nBivariate clustering\n',names_datasets[dataset_ind],' ',names_sigs[sig_ind]),cex=min(1,4*10/max_title_length))
        graphics::axis(1,at=1:length(rownames(sig_scores)), labels=rownames(sig_scores),las=2,tck=0,cex.axis=0.6)
        #mtext(rownames(sig_scores))
        # }else if (Xmotif@Number == 1){
        # 	biclust::heatmapBC(stats::na.omit(t(sig_scores)),bicResult=Xmotif,number=1,col = colorpanel(100,"blue","white","red"), xlab='Gene ID',ylab='Sample')
      }else{
        #print(paste0("Zero or one co-clusters found: ", names[i]))
        graphics::plot.new()
        graphics::title(paste0('\n\n\n <=1 bivariate clusters for\n',names_datasets[dataset_ind],' ',names_sigs[sig_ind]),cex=min(1,4*10/max_title_length))
        cat(paste0('<= 1 bi-clusters found for: ', names_datasets[dataset_ind],' ',names_sigs[sig_ind],'\n'), file=file)

      }
      grab_grob() #grab the heatmap or empty title and add it to the list of heatmaps that will be plotted
    })

    draw.heatmaps(hmaps,names_datasets,names_sigs) #this draws the heatmaps list in a grid
    #saves the biclustering datas
    grDevices::dev.copy(grDevices::pdf,file.path(out_dir,'sig_eval_bivariate_clustering.pdf'),width=4*(length(names_datasets)),height=4*(length(names_sigs)))
    cat('Bi-clustering completed successfully\n', file=file)

    if(grDevices::dev.cur()!=1){
      g<- grDevices::dev.off() #closes graphics device
    }
  }else{
    cat(paste0('Bi-clustering completed successfully. No bi-clusters found among signature/dataset combinations.\n'),file=file)
  }
 #----------------------------------------------------------------------------------------------
 #---------before we do the binarized biclustering plotting, let's determine whether there are ANY biclusters among the datasets/sig combo
 save_bicluster = FALSE
 for (i in 1:(length(names_datasets) * length(names_sigs))){

    #convert index i into an array index for the grid of heatmaps
    dataset_ind <- i %% length(names_datasets)
    if (dataset_ind == 0 ){
      dataset_ind <- length(names_datasets)
    }
    sig_ind <- ceiling(i/length(names_datasets))
    gene_sig <- gene_sigs_list[[names_sigs[sig_ind]]]
    if(is.matrix(gene_sig)){gene_sig = as.vector(gene_sig);}
    data.matrix = mRNA_expr_matrix[[names_datasets[dataset_ind]]] #load the data
    inter = intersect(gene_sig, row.names(data.matrix)) #consider only genes present in both cases

    sig_scores <- as.matrix(data.matrix[inter,])
    sig_scores[!is.finite(sig_scores)] <- NA #make sure the data is finite
    #standardise by z-transform
    for (gene in inter){
      sig_scores[gene,] <- (as.numeric(sig_scores[gene,]) - mean(as.numeric(sig_scores[gene,]),na.rm=T)) / stats::sd(as.numeric(sig_scores[gene,]),na.rm=T)
    }
    #binarize the matrix
    threshold <- min(stats::na.omit(t(sig_scores)))+(max(stats::na.omit(t(sig_scores)))-min(stats::na.omit(t(sig_scores))))/2
    x <- stats::na.omit(t(sig_scores)) #> threshold) * 1
    x[x<=threshold] <- 0
    x[x>threshold] <- 1
    # x <- biclust::binarize(stats::na.omit(t(sig_scores)))#discretize(stats::na.omit(t(sig_scores)),nof=10,quant=F)
    #the following if statement helps to decide the parameters for the BCCC algorithm for biclustering

    if (dim(sig_scores)[2] > 40) {
      num_cols_chosen <- 20
    }else if (dim(sig_scores)[2] > 20){
      num_cols_chosen <- 10
    }else if (dim(sig_scores)[2] > 10){
      num_cols_chosen <- 5
    }else{
      num_cols_chosen <- 2
    }
    # following performs the biclustering
    # Xmotif <- biclust(x, method=BCXmotifs(), number=50, alpha=0.5, nd=floor(dim(sig_scores)/num_cols_chosen), ns=num_cols_chosen, sd=floor(dim(sig_scores)/num_cols_chosen))
    Xmotif <- biclust::biclust(x, method=biclust::BCCC(), delta=1,alpha=1.5, number=50)
    #if there is more then 1 bicluster then will output the heatmap, otherwise not
    if(Xmotif@Number > 1){
      save_bicluster = TRUE
     
    }
  }
 #----------------------------------------------------------------------------------------------
  #same thing, but with binarized heatmaps (not the exact heatmaps) underneath the clustering
  #create a new graphics object
  if(save_bicluster){
    grDevices::dev.new()

    # pdf(file.path(out_dir,'sig_eval_bivariate_clustering_binarized_maps.pdf'),width=10,height=10)

    graphics::par(cex.main=0.8,cex.lab = 0.8,oma=c(4,2,2,2),mar=c(4,4,4,4)) #set graphics parameters
    hmaps <- lapply(1:(length(names_datasets) * length(names_sigs)),function(i) {

      #convert index i into an array index for the grid of heatmaps
      dataset_ind <- i %% length(names_datasets)
      if (dataset_ind == 0 ){
        dataset_ind <- length(names_datasets)
      }
      sig_ind <- ceiling(i/length(names_datasets))
      gene_sig <- gene_sigs_list[[names_sigs[sig_ind]]]
      if(is.matrix(gene_sig)){gene_sig = as.vector(gene_sig);}
      data.matrix = mRNA_expr_matrix[[names_datasets[dataset_ind]]] #load the data
      inter = intersect(gene_sig, row.names(data.matrix)) #consider only genes present in both cases

      sig_scores <- as.matrix(data.matrix[inter,])
      sig_scores[!is.finite(sig_scores)] <- NA #make sure the data is finite
      #standardise by z-transform
      for (gene in inter){
        sig_scores[gene,] <- (as.numeric(sig_scores[gene,]) - mean(as.numeric(sig_scores[gene,]),na.rm=T)) / stats::sd(as.numeric(sig_scores[gene,]),na.rm=T)
      }
      #binarize the matrix
      threshold <- min(stats::na.omit(t(sig_scores)))+(max(stats::na.omit(t(sig_scores)))-min(stats::na.omit(t(sig_scores))))/2
      x <- stats::na.omit(t(sig_scores)) #> threshold) * 1
      x[x<=threshold] <- 0
      x[x>threshold] <- 1
      # x <- biclust::binarize(stats::na.omit(t(sig_scores)))#discretize(stats::na.omit(t(sig_scores)),nof=10,quant=F)
      #the following if statement helps to decide the parameters for the BCCC algorithm for biclustering

      if (dim(sig_scores)[2] > 40) {
        num_cols_chosen <- 20
      }else if (dim(sig_scores)[2] > 20){
        num_cols_chosen <- 10
      }else if (dim(sig_scores)[2] > 10){
        num_cols_chosen <- 5
      }else{
        num_cols_chosen <- 2
      }
      # following performs the biclustering
      # Xmotif <- biclust(x, method=BCXmotifs(), number=50, alpha=0.5, nd=floor(dim(sig_scores)/num_cols_chosen), ns=num_cols_chosen, sd=floor(dim(sig_scores)/num_cols_chosen))
      Xmotif <- biclust::biclust(x, method=biclust::BCCC(), delta=1,alpha=1.5, number=50)
      #if there is more then 1 bicluster then will output the heatmap, otherwise will output an empty plot with title only
      if(Xmotif@Number > 1){
        #biclust::heatmapBC(stats::na.omit(t(sig_scores)),bicResult=Xmotif,col = colorpanel(100,"blue","white","red"), xlab='Gene ID',ylab='Sample')
        biclust::heatmapBC(x,bicResult=Xmotif,col = gplots::colorpanel(100,"blue","white","red"), xlab='Gene ID',ylab='Sample')

        graphics::title(paste0('\n \n \nBivariate clustering\n',names_datasets[dataset_ind],' ',names_sigs[sig_ind]),cex=min(1,4*10/max_title_length))
        graphics::axis(1,at=1:length(rownames(sig_scores)), labels=rownames(sig_scores),las=2,tck=0,cex.axis=0.6)
        #mtext(rownames(sig_scores))
        # }else if (Xmotif@Number == 1){
        # 	biclust::heatmapBC(stats::na.omit(t(sig_scores)),bicResult=Xmotif,number=1,col = colorpanel(100,"blue","white","red"), xlab='Gene ID',ylab='Sample')
      }else{
        # print(paste0("Zero or one co-clusters found: ", names[i]))
        graphics::plot.new()
        graphics::title(paste0('\n\n\n <=1 bivariate clusters for\n',names_datasets[dataset_ind],' ',names_sigs[sig_ind]),cex=min(1,4*10/max_title_length))
      }
      grab_grob() #outputs the graphics object to the list
    })

    draw.heatmaps(hmaps,names_datasets,names_sigs) #draws the heatmaps
    # the following saves the file
    grDevices::dev.copy(grDevices::pdf,file.path(out_dir,'sig_eval_bivariate_clustering_binarized_maps.pdf'),width=4*(length(names_datasets)),height=4*(length(names_sigs)))
    if(grDevices::dev.cur()!=1){
      g<- grDevices::dev.off() #closes graphics device
    }  
  }else{
    cat(paste0('Binarized bi-clustering completed successfully. No binarized bi-clusters found among signature/dataset combinations.\n'),file=file)
  }
  if(grDevices::dev.cur()!=1){
      g<- grDevices::dev.off() #closes graphics device
  }
  radar_plot_values #returns the values for the final plotting function
}
