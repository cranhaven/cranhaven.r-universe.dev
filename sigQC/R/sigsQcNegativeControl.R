######################################################################################
#@description This function performs a negative and permutation control for the input signatures.
# @details The function is checking the possibility of obtaining similar quality control results
# as the input signatures by randomly selecting the same number of genes from the input matrix
# and then performing the quality control test.The obtained results are used for a
# comparison against the original QC. This function internally uses \code{\link{.sigQcNegativeControl}}
# to perform the initial negative control on singular signatures.
#
# 1) For each signature in the list use .sigQcNegativeControl to
# 1.1) create a random signature with the same lenght of the tested one considering all the genes in the input matrix
# 1.2) perform the initial negative controls
# 1.3) Using the computed data, create a plot
#
# Result directory tree
# sigQC
#    +---negative_control
#        +---datasetName1
#            +---signatureName1
#            |   +---sigQC
#            |   |   boxplot_metrics.pdf
#            +---signatureName2
#            |   +---sigQC
#            |   |   boxplot_metrics.pdf
#
#
#
#@param genesList A list of signatures, each signature representing a list of gene ids to use in the analysis
#@param expressionMatrix A gene expression matrix where the rows represent the genes and the columns the samples.
#@param outputDir A path to the directory where the output files are written
#@param studyName Name of the considered study (e.g. Breast Cancer Gene Expression Data)
#@param numResampling Number of re-samplings (50 by default) during negative controls
#@param warningsFile A path to the warning file to use. If missing, a new one will be created in the output directory.
#@param logFile A path to the log file to use. If missing, a new one will be created in the output directory.
#
#
#@author Andrew Dhawan, Alessandro Barberis
######################################################################################
.sigsQcNegativeControl <- function(genesList, expressionMatrixList, outputDir, studyName, numResampling=50, warningsFile, logFile){
  ###########Check the input
  if(missing(genesList)){
    stop("Need to specify a list of genes. The IDs must match those in the expression matrices.")
  }
  if(missing(expressionMatrixList)){
    #stop("Neet to specify an expression matrix where the rows represent the genes and the columns the samples.")
    stop("Neet to specify a list of expression matrices where the rows represent the genes and the columns the samples.")
  }
  if(missing(outputDir)){
    stop("Need to specify an output directory")
  }
  if(missing(studyName)){
    studyName = "MyStudy"
  }
  if(missing(warningsFile)){
    warningsFile = file.path(outputDir, "warnings.log")
  }
  if(missing(logFile)){
    logFile = file.path(outputDir, "log.log")
  }
  tryCatch({
    re.power = numResampling;
    #Check if the output directory exists. Create otherwise.
    if(!dir.exists(outputDir)){
      dir.create(path=outputDir, showWarnings = F, recursive = T)
    }
    #Open connection to log and warning files
    log.con = file(logFile, open = "a")
    warnings.con = file(warningsFile, open = "a")

    #Loop over the signatures
    for(genes.i in 1:length(genesList)){
      genes = as.matrix(genesList[[genes.i]])
      colnames(genes) = names(genesList)[genes.i]
      #For each signature do a negative control
      .sigQcNegativeControl(genes, expressionMatrixList, outputDir, studyName, rePower=numResampling, warningsFile, logFile);
    }
  }, error = function(err) {
    cat("", file=log.con, sep="\n")
    cat(paste(Sys.time(),"Errors occurred during in sigQcNegativeControl:", err, sep=" "), file=log.con, sep="\n")
    #stop("Errors occurred during the computation of negative controls")
  }, finally = {
    #cat("---------------------------------------------------------------------------", file=log.con, sep="\n")
    close(log.con)
    close(warnings.con)
  })#END tryCatch
}

######################################################################################
#@description This function performs a negative control for the input signature.
# @details The function is checking the possibility of obtaining similar quality control results
# as the input signature by randomly selecting the same number of genes from the input matrix
# and then performing the quality control test.The obtained results are used for a
# comparison against the original QC.
#
# 1) Create a random signature with the same lenght of the tested one considering all
# the genes in the input matrix
# 2) Repeat step 1 many times, saving the results
# 3) Compute mean and quantile (0.025, 0.975)
# 4) Plot the data
#
# Result directory tree
# All the files will be saved in a directory named as the signature name (sigName = colnames(genes)),
# inside negative_controls directory in the sigQC tree
#
# sigQC
#    +---negative_control
#        +---datasetName
#            +---signatureName
#                +---sigQC
#                |   boxplot_metrics.pdf
#
#@param genes A list of target ids representing the gene signature to use in the analysis
#@param expressionMatrix A gene expression matrix where the rows represent the genes and the columns the samples.
#@param outputDir A path to the directory where the output files are written.
#@param studyName Name of the considered study (e.g. GSE32014, Breast Cancer Gene Expression Data).
#@param rePower Number of re-samplings (50 by default).
#@param warningsFile A path to the warning file to use. If missing, a new one will be created in the output directory.
#@param logFile A path to the log file to use. If missing, a new one will be created in the output directory.
#
#
#@author Alessandro Barberis
######################################################################################
.sigQcNegativeControl <- function(genes, expressionMatrixList, outputDir, studyName, rePower=50, warningsFile, logFile){
  ###########Check the input
  if(missing(genes)){
    stop("Need to specify a list of genes. The IDs must match those in the expression matrices.")
  }
  if(missing(expressionMatrixList)){
    stop("Neet to specify a list of expression matrices where the rows represent the genes and the columns the samples.")
  }
  if(missing(outputDir)){
    stop("Need to specify an output directory")
  }
  if(missing(studyName)){
    studyName = "MyStudy"
  }
  if(missing(warningsFile)){
    warningsFile = file.path(outputDir, "warnings.log")
  }
  if(missing(logFile)){
    logFile = file.path(outputDir, "log.log")
  }
  tryCatch({
    re.power = rePower;
    #Check if the output directory exists. Create otherwise.
    if(!dir.exists(outputDir)){
      dir.create(path=outputDir, showWarnings = F, recursive = T)
    }
    #Open connection to log and warning files
    log.con = file(logFile, open = "a")
    warnings.con = file(warningsFile, open = "a")

    #Define some useful variables
    len = dim(genes)[1]

    #Loop over datasets
    datasets.num = length(expressionMatrixList)
    datasets.names = names(expressionMatrixList)
    for(dataset.i in 1:datasets.num){
      expressionMatrix = expressionMatrixList[[dataset.i]]
      if(is.null(datasets.names))
        datasetName = paste0("Dataset",dataset.i)
      else
        datasetName = datasets.names[dataset.i]

      data.matrix.ncols = dim(expressionMatrix)[2]
      data.matrix.nrows = dim(expressionMatrix)[1]

      #Define the signature
      gene_sigs_list = list()
      signatureName = colnames(genes)#e.g. "hypoxiaSig"


      #Loop n times (re-sampling power)
      for(i in 1:re.power){
        #Compute a random index vector
        random.index.vector = stats::runif(min=1, max=data.matrix.nrows, n=len);
        #Random signature
        random.genes = as.matrix(rownames(expressionMatrix)[random.index.vector]);
        #Add to the signatures list
        gene_sigs_list[[paste0("NC",i)]] = random.genes;
      }
      names_sigs = names(gene_sigs_list)

      #####sigQC
      cat(paste(Sys.time(), "Computing the Negative Control...", sep=" "), file=log.con, sep="")


      mRNA_expr_matrix = list()
      #names = c(names, gse)
      names = c(datasetName)

      mRNA_expr_matrix[[datasetName]] = expressionMatrix
      sigQC.out_dir = file.path(outputDir, "negative_control", datasetName, signatureName, "sigQC")
      dir.create(path = sigQC.out_dir, showWarnings = FALSE, recursive = T)
      # sigQC::make_all_plots(names_datasets = names,
      #                       gene_sigs_list = gene_sigs_list,
      #                       names_sigs = names_sigs,
      #                       mRNA_expr_matrix = mRNA_expr_matrix,
      #                       out_dir = sigQC.out_dir,
      #                       doNegativeControl=F)

      .compute_without_plots(names_datasets = names,
                            gene_sigs_list = gene_sigs_list,
                            names_sigs = names_sigs,
                            mRNA_expr_matrix = mRNA_expr_matrix,
                            out_dir = sigQC.out_dir
                            )


      cat("DONE", file=log.con, sep="\n")

      #Read the radarchart_table in the sigQC dir
      metrics.file.path = file.path(sigQC.out_dir, "radarchart_table", "radarchart_table.txt")
      metrics.table = utils::read.table(file = metrics.file.path, header = T, sep = "\t", check.names = F, row.names = 1)

      #Create a matrix with 3 rows (mean, quantiles 0.025 and 0.975)
      neg.controls.summary = matrix(nrow = 6, ncol = dim(metrics.table)[2])
      colnames(neg.controls.summary) = colnames(metrics.table)
      rownames(neg.controls.summary) = c("mean", "Q0.025", "Q0.25", "Q0.5", "Q0.75", "Q0.975")

      #Compute the metrics mean, values for which the 95% of the population fall into
      neg.controls.summary[1,] = apply(X=metrics.table,MARGIN = 2,FUN = function(t){mean(t, na.rm=T)})
      neg.controls.summary[2,] = apply(X=metrics.table,MARGIN = 2,FUN = function(t){stats::quantile(t, na.rm=T, probs=c(0.025))})
      neg.controls.summary[3,] = apply(X=metrics.table,MARGIN = 2,FUN = function(t){stats::quantile(t, na.rm=T, probs=c(0.25))})
      neg.controls.summary[4,] = apply(X=metrics.table,MARGIN = 2,FUN = function(t){stats::quantile(t, na.rm=T, probs=c(0.5))})
      neg.controls.summary[5,] = apply(X=metrics.table,MARGIN = 2,FUN = function(t){stats::quantile(t, na.rm=T, probs=c(0.75))})
      neg.controls.summary[6,] = apply(X=metrics.table,MARGIN = 2,FUN = function(t){stats::quantile(t, na.rm=T, probs=c(0.975))})

      #Define the input variable for the function boxplot.matrix2
      stripchartMatrixList=list()
      stripchartMatrixList[["Negative Control"]]=metrics.table

      #Retrieve data from the original sigQC
      sigQC.out_dir = outputDir
      sig.metrics.file.path = file.path(outputDir, "radarchart_table", "radarchart_table.txt")
      if(file.exists(sig.metrics.file.path)){
        sig.metrics.table = utils::read.table(file = sig.metrics.file.path, header = T, sep = "\t", check.names = F, row.names = 1)
        #TO CHANGE: need to address a table where there could be stored multiple signatures metrics
        #sig.metrics.table = t(sig.metrics.table)
        #Look for the row label containing the signature name and the current dataset id
       # paste0(gsub(' ','.', names_datasets[i]),'_',gsub(' ','.', names_sigs[k]))
        index = which(rownames(sig.metrics.table) == paste0(gsub(' ','.', datasetName),'_',gsub(' ','.', signatureName))) #which(grepl(paste0(".*(",datasetName,".*",signatureName,"|",signatureName,".*",datasetName,").*"), rownames(sig.metrics.table), ignore.case=TRUE)==T)
        #sig.metrics.table.sigs =

        sig.metrics.table = sig.metrics.table[index,,drop=F]
        stripchartMatrixList[["Original Metric Value"]]=sig.metrics.table
      }

      sigQC.out_dir = file.path(outputDir, "negative_control", datasetName, signatureName)
      stripchart_group_names <- c('Relative Med. SD','Skewness',expression(sigma["" >= "10%" ]),expression(sigma["" >= "25%" ]),expression(sigma["" >= "50%" ]),'Coef. of Var.',
                               'Non-NA Prop.','Prop. Expressed',
                               'Autocor.',expression(rho["Mean,Med" ]),
                               expression(rho["PCA1,Med" ]),expression(rho["Mean,PCA1" ]), expression(sigma["PCA1" ]),
                               expression(rho["Med,Z-Med" ]))

      .boxplot.matrix2(x=neg.controls.summary[2:6,], outputDir=sigQC.out_dir, plotName="boxplot_metrics",
                       plotTitle=paste("Boxplot Negative Controls for",signatureName,sep=" "), stripchartMatrixList=stripchartMatrixList,
                       stripchartPch=c(1, 21), stripchartCol = c("gray", "red"), xlab ="Metrics", ylab="Score",group.names=stripchart_group_names)

      #Store the matrix containing the summary of negative controls (i.e. quantiles)
      summary.filePath = file.path(sigQC.out_dir, "neg_controls_summary_table.txt")
      utils::write.table(x = neg.controls.summary,file = summary.filePath,row.names = TRUE, col.names = TRUE,sep=",")

    }#END LOOP OVER DATASETS

   #----the following is for the permutation control----
      #here, we are going to permute the labels of the genes of the signature on all of the samples
      #so looping over each gene signature, we are going to get different dataset matrices such that the rows are permuted
      # we are going to modify the relevant rows of each of the expression matrices for each of the negative controls


    for(dataset.i in 1:datasets.num){
      if(is.null(datasets.names))
        datasetName = paste0("Dataset",dataset.i)
      else
        datasetName = datasets.names[dataset.i]

      expressionMatrix = expressionMatrixList[[dataset.i]]
      signatureName = colnames(genes)#e.g. "hypoxiaSig"

      expressionMatrix_perm_list <- list()
      for(i in 1:re.power){    
        expressionMatrix_perm <- expressionMatrix
        genes_present <- intersect(rownames(expressionMatrix),genes[,1])
        new_ordering <- replicate(sample(1:length(genes_present)),n=dim(expressionMatrix)[2])
        for(col.num in 1:dim(expressionMatrix)[2]){
          expressionMatrix_perm[genes_present,col.num] <- expressionMatrix[genes_present[new_ordering[,col.num]],col.num]
        }
        expressionMatrix_perm_list[[paste0("PC",i)]] <- expressionMatrix_perm
      }
    
    
      sigQC.out_dir = file.path(outputDir, "permutation_control", datasetName, signatureName, "sigQC")
      dir.create(path = sigQC.out_dir, showWarnings = FALSE, recursive = T)
      gene_sigs_list <- list()
      gene_sigs_list[[colnames(genes)]] <- genes
      .compute_without_plots(names_datasets = names(expressionMatrix_perm_list),
                            gene_sigs_list = gene_sigs_list,
                            names_sigs = names(gene_sigs_list),
                            mRNA_expr_matrix = expressionMatrix_perm_list,
                            out_dir = sigQC.out_dir
                            )


      cat("DONE", file=log.con, sep="\n")


      #Read the radarchart_table in the sigQC dir
      metrics.file.path = file.path(sigQC.out_dir, "radarchart_table", "radarchart_table.txt")
      metrics.table = utils::read.table(file = metrics.file.path, header = T, sep = "\t", check.names = F, row.names = 1)

      #Create a matrix with 3 rows (mean, quantiles 0.025 and 0.975)
      neg.controls.summary = matrix(nrow = 6, ncol = dim(metrics.table)[2])
      colnames(neg.controls.summary) = colnames(metrics.table)
      rownames(neg.controls.summary) = c("mean", "Q0.025", "Q0.25", "Q0.5", "Q0.75", "Q0.975")

      #Compute the metrics mean, values for which the 95% of the population fall into
      neg.controls.summary[1,] = apply(X=metrics.table,MARGIN = 2,FUN = function(t){mean(t, na.rm=T)})
      neg.controls.summary[2,] = apply(X=metrics.table,MARGIN = 2,FUN = function(t){stats::quantile(t, na.rm=T, probs=c(0.025))})
      neg.controls.summary[3,] = apply(X=metrics.table,MARGIN = 2,FUN = function(t){stats::quantile(t, na.rm=T, probs=c(0.25))})
      neg.controls.summary[4,] = apply(X=metrics.table,MARGIN = 2,FUN = function(t){stats::quantile(t, na.rm=T, probs=c(0.5))})
      neg.controls.summary[5,] = apply(X=metrics.table,MARGIN = 2,FUN = function(t){stats::quantile(t, na.rm=T, probs=c(0.75))})
      neg.controls.summary[6,] = apply(X=metrics.table,MARGIN = 2,FUN = function(t){stats::quantile(t, na.rm=T, probs=c(0.975))})

      #Define the input variable for the function boxplot.matrix2
      stripchartMatrixList=list()
      stripchartMatrixList[["Permutation Control"]]=metrics.table

      #Retrieve data from the original sigQC
      sigQC.out_dir = outputDir
      sig.metrics.file.path = file.path(outputDir, "radarchart_table", "radarchart_table.txt")
      if(file.exists(sig.metrics.file.path)){
        sig.metrics.table = utils::read.table(file = sig.metrics.file.path, header = T, sep = "\t", check.names = F, row.names = 1)
        #TO CHANGE: need to address a table where there could be stored multiple signatures metrics
        #sig.metrics.table = t(sig.metrics.table)
        #Look for the row label containing the signature name and the current dataset id
       # paste0(gsub(' ','.', names_datasets[i]),'_',gsub(' ','.', names_sigs[k]))
        index = which(rownames(sig.metrics.table) == paste0(gsub(' ','.', datasetName),'_',gsub(' ','.', signatureName))) #which(grepl(paste0(".*(",datasetName,".*",signatureName,"|",signatureName,".*",datasetName,").*"), rownames(sig.metrics.table), ignore.case=TRUE)==T)
        #sig.metrics.table.sigs =

        sig.metrics.table = sig.metrics.table[index,,drop=F]
        stripchartMatrixList[["Original Metric Value"]]=sig.metrics.table
      }

      sigQC.out_dir = file.path(outputDir, "permutation_control", datasetName, signatureName)
      stripchart_group_names <- c('Relative Med. SD','Skewness',expression(sigma["" >= "10%" ]),expression(sigma["" >= "25%" ]),expression(sigma["" >= "50%" ]),'Coef. of Var.',
                               'Non-NA Prop.','Prop. Expressed',
                               'Autocor.',expression(rho["Mean,Med" ]),
                               expression(rho["PCA1,Med" ]),expression(rho["Mean,PCA1" ]), expression(sigma["PCA1" ]),
                               expression(rho["Med,Z-Med" ]))

      .boxplot.matrix2(x=neg.controls.summary[2:6,], outputDir=sigQC.out_dir, plotName="boxplot_metrics",
                       plotTitle=paste("Boxplot Permutation Controls for",signatureName,sep=" "), stripchartMatrixList=stripchartMatrixList,
                       stripchartPch=c(1, 21), stripchartCol = c("gray", "red"), xlab ="Metrics", ylab="Score",group.names=stripchart_group_names)

      #Store the matrix containing the summary of negative controls (i.e. quantiles)
      summary.filePath = file.path(sigQC.out_dir, "perm_controls_summary_table.txt")
      utils::write.table(x = neg.controls.summary,file = summary.filePath,row.names = TRUE, col.names = TRUE,sep=",")
   
    }

    #----------------------------------------------------

  }, error = function(err) {
    cat("", file=log.con, sep="\n")
    cat(paste(Sys.time(),"Errors occurred during in sigQcNegativeControl:", err, sep=" "), file=log.con, sep="\n")
    #stop("Errors occurred during the computation of negative controls")
  }, finally = {
    #cat("---------------------------------------------------------------------------", file=log.con, sep="\n")
    close(log.con)
    close(warnings.con)
  })#END tryCatch
}
