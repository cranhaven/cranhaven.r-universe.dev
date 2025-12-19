
# library(biclust)
# library(BiBitR)
# library(viridis) # viridis
# library(cluster) # agnes
# library(dendextend) # color_branches
# # stats: hclust, as.hclust, cutree
# library(lattice) # levelplot


# TO DO:
# - complete documentation
# -  add memory option to bibit/bibit2/bibit3
# - Make CompareResultJI more efficient for single result






#' @title BiBit Workflow
#' @description Workflow to discover larger (noisy) patterns in big data using BiBit
#' @details Looking for Noisy Biclusters in large data using BiBit (\code{\link{bibit2}}) often results in many (overlapping) biclusters.
#' In order decrease the number of biclusters and find larger meaningful patterns which make up noisy biclusters, the following workflow can be applied.
#' Note that this workflow is primarily used for data where there are many more rows (e.g. patients) than columns (e.g. symptoms). For example the workflow would discover larger meaningful symptom patterns which, conditioned on the allowed noise/zeros, subsets of the patients share.
#' \enumerate{
#' \item Apply BiBit with \emph{no noise} (Preferably with high enough \code{minr} and \code{minc}).
#' \item Compute Similarity Matrix (Jaccard Index) of all biclusters. By default this measure is only based on column similarity.
#' This implies that the rows of the BC's are not of interest in this step. The goal then would be to discover highly overlapping column patterns and, in the next steps, merge them together.
#' \item Apply Agglomerative Hierarchical Clustering on Similarity Matrix (default = average link)
#' \item Cut the dendrogram of the clustering result and merge the biclusters based on this. (default = number of clusters is determined by the Tibs2001SEmax Gap Statistic)
#' \item Extract Column Memberships of the Merged Biclusters. These are saved as the new column \emph{Patterns}.
#' \item Starting from these patterns, \emph{(noisy) rows} are grown which match the pattern, creating a single final bicluster for each pattern. At the end duplicate/non-maximal BC's are deleted.
#' }
#' Using the described workflow (and column similarity in Step 2), the final result will contain biclusters which focus on larger column patterns. 
#' @author Ewoud De Troyer
#' @export
#' @param matrix The binary input matrix.
#' @param minr The minimum number of rows of the Biclusters.
#' @param minc The minimum number of columns of the Biclusters.
#' @param similarity_type Which dimension to use for the Jaccard Index in Step 2. This is either columns (\code{"col"}, default) or both (\code{"both"}).
#' @param func Which clustering function to use in Step 3. Either \code{"agnes"} (= default) or \code{"hclust"}.
#' @param link Which clustering link to use in Step 3. The available links (depending on \code{func}) are:
#' \itemize{
#' \item{\code{hclust}: }\code{"ward.D"}, \code{"ward.D2"}, \code{"single"}, \code{"complete"}, \code{"average"}, \code{"mcquitty"}, \code{"median"} or \code{"centroid"}  
#' \item{\code{agnes}: }\code{"average"} (default), \code{"single"}, \code{"complete"}, \code{"ward"}, \code{"weighted"}, \code{"gaverage"} or \code{"flexible"}   
#' }
#' (More details in \code{\link[stats]{hclust}} and \code{\link[cluster]{agnes}})
#' @param par.method Additional parameters used for flexible link (See \code{\link[cluster]{agnes}}). Default is \code{c(0.625)}
#' @param cut_type Which method should be used to decide the number of clusters in the tree in Step 4? 
#' \itemize{
#' \item \code{"gap"}: Use the Gap Statistic (default).
#' \item \code{"number"}: Select a set number of clusters.
#' \item \code{"height"}: Cut the tree at specific dissimilarity height.
#' }
#' 
#' @param cut_pm Cut Parameter (depends on \code{cut_type}) for Step 4
#' \itemize{
#' \item Gap Statistic (\code{cut_type="gap"}): How to compute optimal number of clusters? Choose one of the following: \code{"Tibs2001SEmax"} (default), \code{"globalmax"}, \code{"firstmax"}, \code{"firstSEmax"} or \code{"globalSEmax"}.
#' \item Number (\code{cut_type="number"}): Integer for number of clusters.
#' \item Height (\code{cut_type="height"}): Numeric dissimilarity value where the tree should be cut (\code{[0,1]}).
#' }
#'  
#' @param gap_B Number of bootstrap samples (default=500) for Gap Statistic (\code{\link[cluster]{clusGap}}).
#' @param gap_maxK Number of clusters to consider (default=50) for Gap Statistic (\code{\link[cluster]{clusGap}}).
#' 
#' @param noise The allowed noise level when growing the rows on the merged patterns in Step 6. (default=\code{0.1}, namely allow 10\% noise.)
#' \itemize{
#' \item \code{noise=0}: No noise allowed.
#' \item \code{0<noise<1}: The \code{noise} parameter will be a noise percentage. The number of allowed 0's in a row in the bicluster will depend on the column size of the bicluster.
#' More specifically \code{zeros_allowed = ceiling(noise * columnsize)}. For example for \code{noise=0.10} and a bicluster column size of \code{5}, the number of allowed 0's would be \code{1}.
#' \item \code{noise>=1}: The \code{noise} parameter will be the number of allowed 0's in a row in the bicluster independent from the column size of the bicluster. In this noise option, the noise parameter should be an integer.
#' }
#' @param noise_select Should the allowed noise level be automatically selected for each pattern? (Using ad hoc method to find the elbow/kink in the Noise Scree plots)
#' \itemize{
#' \item \code{noise_select=0}: Do \emph{NOT} automatically select the noise levels. Use the the noise level given in the \code{noise} parameter (default).
#' \item \code{noise_select=1}: Using the Noise Scree plot (with 'Added Rows' on the y-axis), find the noise level where the current number of added rows at this noise level is larger than the mean of 'added rows' at the lower noise levels. 
#' After locating this noise level, lower the noise level by 1. This is your automatically selected elbow/kink and therefore your noise level.
#' \item \code{noise_select=2}: Applies the same steps as for \code{noise_select=1}, but instead of decreasing the noise level by only 1, keep decreasing the noise level until the number of added rows isn't decreasing anymore either.
#' }
#' 
#' @param plots Vector for which plots to draw:
#' \enumerate{
#' \item Image plot of the similarity matrix computed in Step 2.
#' \item Same as \code{plots=1}, but the rows and columns are reordered with the hierarchical tree.
#' \item Dendrogram of the tree, its clusters colored after the chosen cut has been applied.
#' \item Noise Scree plots for all the Saved Patterns. Two plots will be plotted, both with Noise on the x-axis. The first one will have the number of Added Number of Rows on that noise level on the y-axis, while the second will have the Total Number of Rows (i.e. cumulative of the first).
#' If the title of one of the subplots is red, then this means that the Bicluster grown from this pattern, using the chosen noise level, was eventually deleted due to being a duplicate or non-maximal.
#' \item Image plot of the Jaccard Index similarity matrix between the final biclusters after Step 6.
#' }
#' @param BCresult Import a BiBit Biclust result for Step 1 (e.g. extract from an older BiBitWorkflow object \code{$info$BiclustInitial}). This can be useful if you want to cut the tree differently/make different plots, but don't want to do the BiBit calculation again.
#' @param simmatresult Import a (custom) Similarity Matrix (e.g. extract from older BiBitWorkflow object \code{$info$BiclustSimInitial}). Note that Step 1 (BiBit) will still be executed if \code{BCresult} is not provided.
#' @param treeresult Import a (custom) tree (\code{hclust} object) based on the BiBit/Similarity (e.g. extract from older BiBitWorkflow object \code{$info$Tree}).
#' @param plot.type Output Type
#' \itemize{
#' \item \code{"device"}: All plots are outputted to new R graphics devices (default).
#' \item \code{"file"}: All plots are saved in external files. Plots 1 and 2 are saved in separate \code{.png} files while all other plots are joint together in a single \code{.pdf} file.
#' \item \code{"other"}: All plots are outputted to the current graphics device, but will overwrite each other. Use this if you want to include one or more plots in a sweave/knitr file or if you want to export a single plot by your own chosen format.
#' }
#' @param filename Base filename (with/without directory) for the plots if \code{plot.type="file"} (default=\code{"BiBitWorkflow"}).
#' @param verbose Logical value if progress of workflow should be printed.
#' 
#' @return A BiBitWorkflow S3 List Object with 3 slots:
#' \itemize{
#' \item \code{Biclust}: Biclust Class Object of Final Biclustering Result (after Step 6).
#' \item \code{BiclustSim}: Jaccard Index Similarity Matrix of Final Biclustering Result (after Step 6).
#' \item \code{info}: List Object containing:
#' \itemize{
#' \item \code{BiclustInitial}: Biclust Class Object of Initial Biclustering Result (after Step 1).
#' \item \code{BiclustSimInitial}: Jaccard Index Similarity Matrix of Initial Biclustering Result (after Step 1).
#' \item \code{Tree}: Hierarchical Tree of \code{BiclustSimInitial} as \code{hclust} object.
#' \item \code{Number}: Vector containing the initial number of biclusters (\code{InitialNumber}), the number of saved patterns after cutting the tree (\code{PatternNumber}) and the final number of biclusters (\code{FinalNumber}).
#' \item \code{GapStat}: Vector containing all different optimal cluster numbers based on the Gap Statistic.
#' \item \code{BC.Merge}: A list (length of merged saved patterns) containing which biclusters were merged together after cutting the tree.
#' \item \code{MergedColPatterns}: A list (length of merged saved patterns) containing the indices of which columns make up that pattern.
#' \item \code{MergedNoiseThresholds}: A vector containing the selected noise levels for the merged saved patterns.
#' \item \code{Coverage}: A list containing: 1. a vector of the total number (and percentage) of unique rows the final biclusters cover. 2. a table showing how many rows are used more than a single time in the final biclusters.
#' \item \code{Call}: A match.call of the original function call.
#' }
#' }
#'  
#' @examples 
#' \dontrun{
#' ## Simulate Data ##
#' # DATA: 10000x50
#' # BC1: 200x10
#' # BC2: 100x10
#' # BC1 and BC2 overlap 5 columns
#' 
#' # BC3: 200x10
#' # BC4: 100x10
#' # BC3 and bC4 overlap 2 columns
#' 
#' # Background 1 percentage: 0.15
#' # BC Signal Percentage: 0.9
#'  
#' set.seed(273)
#' mat <- matrix(sample(c(0,1),10000*50,replace=TRUE,prob=c(1-0.15,0.15)),
#'               nrow=10000,ncol=50)
#' mat[1:200,1:10] <- matrix(sample(c(0,1),200*10,replace=TRUE,prob=c(1-0.9,0.9)),
#'                           nrow=200,ncol=10)
#' mat[300:399,6:15] <- matrix(sample(c(0,1),100*10,replace=TRUE,prob=c(1-0.9,0.9)),
#'                             nrow=100,ncol=10)
#' mat[400:599,21:30] <- matrix(sample(c(0,1),200*10,replace=TRUE,prob=c(1-0.9,0.9)),
#'                              nrow=200,ncol=10)
#' mat[700:799,29:38] <- matrix(sample(c(0,1),100*10,replace=TRUE,prob=c(1-0.9,0.9)),
#'                              nrow=100,ncol=10)
#' mat <- mat[sample(1:10000,10000,replace=FALSE),sample(1:50,50,replace=FALSE)]
#' 
#' 
#' # Computing gap statistic for initial 1381 BC takes approx. 15 min.
#' # Gap Statistic chooses 4 clusters. 
#' out <- BiBitWorkflow(matrix=mat,minr=50,minc=5,noise=0.2) 
#' summary(out$Biclust)
#' 
#' # Reduce computation by selecting number of clusters manually.
#' # Note: The "ClusterRowCoverage" function can be used to provided extra info 
#' #       on the number of cluster choice.
#' #       How?
#' #       - More clusters result in smaller column patterns and more matching rows.
#' #       - Less clusters result in larger column patterns and less matching rows.
#' # Step 1: Initial Workflow Run
#' out2 <- BiBitWorkflow(matrix=mat,minr=50,minc=5,noise=0.2,cut_type="number",cut_pm=10)
#' # Step 2: Use ClusterRowCoverage
#' temp <- ClusterRowCoverage(result=out2,matrix=mat,noise=0.2,plots=2)
#' # Step 3: Use BiBitWorkflow again (using previously computed parts) with new cut parameter
#' out3 <- BiBitWorkflow(matrix=mat,minr=50,minc=5,noise=0.2,cut_type="number",cut_pm=4,
#'                       BCresult = out2$info$BiclustInitial,
#'                       simmatresult = out2$info$BiclustSimInitial)
#' summary(out3$Biclust)
#' }
BiBitWorkflow <- function(matrix,minr=2,minc=2,
                                      similarity_type="col",
                                      func="agnes",link="average",par.method=0.625,
                                      cut_type="gap",cut_pm="Tibs2001SEmax",gap_B=500,gap_maxK=50,
                                      noise=0.1,noise_select=0,
                                      plots=c(3:5),
                                      BCresult=NULL,simmatresult=NULL,treeresult=NULL,
                                      plot.type="device",filename="BiBitWorkflow",
                                      verbose=TRUE){
  
  # Plots #
  # 1. image plot of sim_mat
  # 2. image plot of sim_mat, reordened with tree
  # 3. Hierarchical tree, colored with cut
  # 4. Noise Scree Plot
  # 5. image plot of final sim_mat (both JI)
  if(!all(plots%in%c(1:5))){stop("plots should be part of c(1,2,3,4,5)")}
  if(length(plot.type)!=1){stop("plot.type should be of length 1",call.=FALSE)}
  if(!(plot.type %in% c("device","file","other"))){stop("plot.type should be 'device', 'file' or 'other'",call.=FALSE)}
  FIRSTPLOT <- TRUE
  
  pm <- match.call()
  
  ## PARAMETER CHECKS ##
  if(class(matrix)!="matrix"){stop("matrix parameter should contain a matrix object",call.=FALSE)}
  if(!identical(as.numeric(as.vector(matrix)),as.numeric(as.logical(matrix)))){stop("matrix is not a binary matrix!",call.=FALSE)}
  if(is.null(rownames(matrix))){rownames(matrix) <- paste0("Row",c(1:nrow(matrix)))}
  if(is.null(colnames(matrix))){colnames(matrix) <- paste0("Col",c(1:ncol(matrix)))}
  if(noise<0){stop("noise parameter can not be negative",call.=FALSE)}
  if(!(noise_select %in% c(0,1,2))){stop("noise_select must be 0, 1 or 2")}
  
  
  if(!is.null(simmatresult)){if(class(simmatresult)!="matrix"){stop("simmatresult needs to be a matrix")}}
  
  if(length(similarity_type)>1){stop("similarity_type can only be of length1")}
  if(!(similarity_type %in% c("col","both"))){stop("similarity_type needs to be \"col\" or \"both\"")}
  
  if(length(func)>1){stop("func can only be of length 1")}
  if(!(func %in% c("hclust","agnes"))){stop("func needs to be \"hclust\" or \"agnes\"")}
  if(length(link)>1){stop("link can only be of length 1")}
  
  hclust_links <- c("ward.D","ward.D2","single","complete","average","mcquitty","median","centroid")
  agnes_links <- c("average","single","complete","ward","weighted","gaverage","flexible")
  link_options <- list(hclust=hclust_links,agnes=agnes_links)
  other_func <- setdiff(c("hclust","agnes"),func)
  
  if(!(link%in%link_options[[func]])){
    if(link%in%agnes_links[[other_func]]){
      message(paste0(link," link was not available for ",func," so ",other_func," was used.\n\n"))
      func <- other_func
    }else{
      stop(paste0(link," link not available for hclust or agnes"))
    }
  }
  
  if(length(cut_type)>1){stop("cut_type can only be of length 1")}
  if(length(cut_pm)>1){stop("cut_pm can only be of length 1")}
  if(!(cut_type %in% c("gap","number","height"))){stop("cut_type should be \"gap\", \"number\" or \"height\"")}
  if(cut_type=="gap" & similarity_type=="both"){stop("Gap Statistic not possible for a 2-dimensional similarity measure")}
  
  JI <- NULL
  number <- NULL
  if(cut_type=="gap"){
    gap_options <- c("firstSEmax", "Tibs2001SEmax", "globalSEmax", "firstmax", "globalmax")
    if(!(cut_pm %in% gap_options)){stop(paste0("for \"gap\" cut_pm should one of the following: ",paste0(sapply(gap_options,FUN=function(y){return(paste0("\"",y,"\""))}),collapse=", ")))}
  }
  if(cut_type=="number"|cut_type=="height"){
    if(class(cut_pm)!="numeric" & class(cut_pm)!="integer"){stop("for \"number\", cut_pm should be a numeric or integer")}
    if(cut_type=="number"){
      cut_pm <- as.numeric(as.integer(cut_pm))
      number <- cut_pm
    }else{
      cut_pm <- as.numeric(cut_pm)
      JI <- 1-cut_pm    
    }
  }
  
  
  if(verbose){  
  ## APPLY ORIGINAL BIBIT WITHOUT NOISE ##
  cat("STEP 1: ORIGINAL BIBIT WITHOUT NOISE\n")
  cat("------------------------------------\n\n")
  }
  
  if(!is.null(BCresult)){
    if(class(BCresult)!="Biclust"){stop("BCresult is not a Biclust class object")}
    result1 <- BCresult
    if(verbose){cat("BCresult was used\n")}
  }else{
    if(verbose){
      result1 <- bibit(matrix,minr=minr,minc=minc)
    }else{
      temp <- capture.output({
        result1 <- bibit(matrix,minr=minr,minc=minc)
      })
    }
  }
  if(verbose){cat("Total BC's found: ",result1@Number,"\n\n")}
  
  if(result1@Number<=1){stop("Not enough BC's to continue analysis.")}
  
  
  ## COMPUTE INITIAL SIMILARITY MATRIX ##
  if(verbose){
  cat("STEP 2: SIMILARITY MATRIX\n")
  cat("-------------------------\n\n")
  }
  
  if(!is.null(simmatresult)){
    if(verbose){cat("simmatresult was used\n")}
    sim_mat <- simmatresult
  }else{
    if(verbose){cat(paste0("Compute Jaccard Index Similarity (",similarity_type,") of ",result1@Number," BC's\n"))}
    sim_mat <- workflow_simmat(result1,type=similarity_type,verbose=verbose)
  }
  

 
  # Plot 1 #
  if(1%in%plots){
    if(verbose){cat("Plot 1: Image Plot of Similarity Matrix\n")}
    if(plot.type=="device"){dev.new()}else if(plot.type=="file"){png(paste0(filename,"_plot1.png"))}
    image(sim_mat,col=viridis(256),axes=FALSE,main=paste0("Heatmap of JI Similarity (",similarity_type,")"))
    if(plot.type=="file"){dev.off()}
  }
  

  if(verbose){
  cat("\nSTEP 3: AGGLOMERATIVE HIERARCHICAL CLUSTERING TREE\n")
  cat("-------------------------------------------------\n")
  cat("Link =",link," |  Function =",func,"\n\n")
  }
  
  
  # Create tree based on parameters
  if(!is.null(treeresult)){
    tree_init <- treeresult
    if(verbose){cat("treeresult was used\n")}
    
  }else{
    if(func=="hclust"){
      tree_init <- hclust(as.dist(1-sim_mat),method=link)
    }
    if(func=="agnes"){
      tree_init <- as.hclust(agnes(as.dist(1-sim_mat),diss=TRUE,method=link,par.method = par.method))
    }
    
  }
  
  

  
  # Plot 2 #
  if(2 %in% plots){
    if(verbose){cat("Plot 2: Image plot of Similarity Matrix, reordered with clustering tree\n\n")}
    if(plot.type=="device"){dev.new()}else if(plot.type=="file"){png(paste0(filename,"_plot2.png"))}
    heatmap((sim_mat),Rowv=as.dendrogram(tree_init),col=viridis(256),main=paste0("Heatmap of JI Similarity (",similarity_type,") - Reordered"))
    if(plot.type=="file"){dev.off()}
  }
 
  
  ## COMPUTE GAP STATISTIC ##
  gap_out <- NULL
  if(cut_type=="gap"){
    if(verbose){cat("Computing Gap Statistic...\n")}
    
    
    if(similarity_type=="col"){
      gapdata <- as.data.frame(result1@NumberxCol+0)
    }
    if(similarity_type=="row"){
      gapdata <- as.data.frame(t(result1@RowxNumber+0))
    }

    if(result1@Number<50){gap_maxK <- result1@Number-1}
    
    gap_stat <- clusGap((gapdata),verbose=verbose,B=gap_B,K.max=gap_maxK,FUNcluster=function(x,k){list(cluster=cutree((tree_init),k=k))})
    
    # gap_stat <- cluster::clusGap(as.data.frame(result1@NumberxCol),B=gap_B,K.max=gap_maxK,FUNcluster=function(x,k){list(cluster=cutree(tree_init,k=k))})
    
    
    number <- maxSE(gap_stat$Tab[, 3], gap_stat$Tab[,4], cut_pm)
    
    if(verbose){cat(paste0("Gap Statistic '",cut_pm,"': ",number," clusters\n"))}
    gap_out <- sapply(gap_options,FUN=function(x){maxSE(gap_stat$Tab[, 3], gap_stat$Tab[,4], x)})
    
    
    # Check for singletons
    tree_1 <- which(tree_init$height==1)
    if(length(tree_1)>0){
      merge_1 <- as.vector(tree_init$merge[tree_1,])
      singletons <- merge_1[which(merge_1<0)]*(-1)
      if(length(singletons)>0){
        message(paste0("Singletons detected: Original BC ",paste0(singletons,collapse=", "),"\nGap Statistics might not be accurate and manual number of cluster selection might be necessary."))
      }
    }
    
    # Check if number=1
    if(number==1){
      number <- 2
      warning("Only 1 cluster selected by gap statistic. This was increased to 2 cluster in order to complete the workflow.")
    }
    
  }
  
  
  

  
  
  # Plot 3 #
  if(3 %in% plots){
    if(is.null(JI)){
      h_temp <- NULL
      cut_txt <- paste0(number," clusters")
    }else{
      h_temp <- 1-JI
      cut_txt <- paste0(round(h_temp,2)," distance")
    }
    
    if(verbose){cat("Plot 3:",paste0("Dendrogram - ",func," - ",link," link (",cut_txt,")"),"\n")}
    if(plot.type=="device"){
      dev.new()
    }else if(plot.type=="file" & FIRSTPLOT){
      pdf(paste0(filename,"_plot",paste0(intersect(plots,3:5),collapse=""),".pdf"))
      FIRSTPLOT <- FALSE
    }
    
    plot(color_branches(as.dendrogram(tree_init),h=h_temp,k=number),main=paste0("Dendrogram - ",func," - ",link," link (",cut_txt,")"))
  
    
  }
  
  
  ## MERGE BICLUSTERS ##
  if(verbose){
  cat("\nSTEP 4: MERGING BICLUSTERS BASED ON HIERARCHICAL TREE\n")
  cat("------------------------------------------------------\n\n")
  }
  result2 <- workflow_mergeBC(result=result1,tree=tree_init,JI=JI,number=number,verbose=verbose)
  
  # Remove BC.Merge to put it in general info + make a list of original patterns
  BC.Merge <- result2@info$BC.Merge
  result2@info$BC.Merge <- NULL
  MergedColPatterns <- lapply(as.list(seq_len(result2@Number)),FUN=function(pattern){
    return(which(result2@NumberxCol[pattern,]))
  })
  names(MergedColPatterns) <- paste0("Pat",seq_len(result2@Number))
  
  ## COMPUTE NOISE FOR MERGED PATTERNS & AUTOMATIC THRESHOLDS##
  NoisexNumber <- apply(result2@NumberxCol,MARGIN=1,FUN=function(pattern){
    return(apply(matrix[,pattern],MARGIN=1,FUN=function(row){return(sum(row==0))}))
  })
  data_noisescree <- lapply(seq_len(ncol(NoisexNumber)),FUN=function(pattern){
    tab <- table(NoisexNumber[,pattern])
    return(data.frame(Noise=as.numeric(names(tab)),Total=as.numeric(tab)))
  })
  noise_threshold <- workflow_noisethreshold(noise,noise_select,data_noisescree)
  result2@info$Noise.Threshold <- noise_threshold
    
  

  
  
  ## APPLY PATTERN BIBIT/ROWGROWING ##
  if(verbose){
  cat("\nSTEP 5: GROWING ROWS FOR MERGED PATTERNS\n")
  cat("----------------------------------------\n\n")
  }
  
  result3 <-  workflow_UpdateBiclust_RowNoise(result=result2,matrix=matrix, noise=result2@info$Noise.Threshold,removeBC=TRUE,NoisexNumber=NoisexNumber,verbose=verbose)
  
  
  # Plot 4 #
  if(4 %in% plots){
    if(verbose){cat("\n Plot 4: Noise Scree Plot of Merged Patterns\n")}
    
    # Plot with added number of rows & Plot with total number of rows
    tempdim <- sqrt(result2@Number)
    extradim <- ifelse((floor(tempdim)*ceiling(tempdim))<result2@Number,1,0)
    
    patcols <- apply(result2@NumberxCol,MARGIN=1,FUN=sum)
    
    
    for(i.plot in c(1,2)){
      if(plot.type=="device"){
        dev.new()
      }else if(plot.type=="file" & FIRSTPLOT){
        pdf(paste0(filename,"_plot",paste0(intersect(plots,3:5),collapse=""),".pdf"))
        FIRSTPLOT <- FALSE
      }      
      par(mfrow=c(floor(tempdim)+extradim,ceiling(tempdim)))
      
      pattern_number <- 1
      for(i.pattern in seq_len(length(data_noisescree))){
        temp_data <- data_noisescree[[i.pattern]]
        if(i.plot==2){
          temp_data$Total <- cumsum(temp_data$Total)
          ylab_temp <- "Total Number"
        }else{
          ylab_temp <- "Added Number"
        }
        
        if(i.pattern %in% result3@info$Deleted.Patterns){
          main_temp <- paste0("Deleted Pat"," (",patcols[i.pattern]," cols)")
          col.main <- "red"
        }else{
          main_temp <- paste0("Pat",pattern_number," (",patcols[i.pattern]," cols)")
          col.main <- "black"
          pattern_number <- pattern_number+1
        }
        
        plot(temp_data$Noise,temp_data$Total,main=main_temp,col.main=col.main,xlab="Noise",ylab=ylab_temp,pch=19)
        points(temp_data$Noise,temp_data$Total,type="l")
        text(temp_data$Noise,temp_data$Total,as.character(temp_data$Total),pos=3)
        abline(v=ifelse((noise_threshold[i.pattern]<1) & (noise_threshold[i.pattern]>0),ceiling(noise_threshold[i.pattern]*patcols[i.pattern]),noise_threshold[i.pattern]),col="blue")
      }
      par(mfrow=c(1,1))
    }
  }  
  
  # Similarity Matrix of final result
  if(result3@Number>1){
    sim_mat_result3 <- workflow_simmat(result3,type="both",verbose=FALSE)
  }else{
    sim_mat_result3 <- matrix(1,nrow=1,ncol=1)
  }
  rownames(sim_mat_result3) <- colnames(sim_mat_result3) <- paste0("BC",1:nrow(sim_mat_result3))
  
  # Plot 5 #
  if(5 %in% plots){
    if(verbose){cat("\nPlot 5: Image Plot of Similarity Matrix of Final Result\n")}
    if(plot.type=="device"){
      dev.new()
    }else if(plot.type=="file" & FIRSTPLOT){
      pdf(paste0(filename,"_plot",paste0(intersect(plots,3:5),collapse=""),".pdf"))
      FIRSTPLOT <- FALSE
    }
    # image(sim_mat_result3,col=viridis(256),axes=FALSE,main=paste0("Heatmap of JI Similarity"))
    # axis(1,at=seq(0,1,length.out=ncol(sim_mat_result3)),labels=colnames(sim_mat_result3),tick=FALSE,las=2,cex.axis=0.5)
    # axis(2,at=seq(0,1,length.out=nrow(sim_mat_result3)),labels=rownames(sim_mat_result3),tick=FALSE,las=2,cex.axis=0.5)
    print(levelplot(sim_mat_result3,col.regions=viridis(256),main="Heatmap of JI Similarity",ylab="",xlab="", scales = list(tck = c(1,0))))
  }
  
  
  ## Compute row coverage ##
  BC_rownames <- unlist(apply(result3@RowxNumber,MARGIN=2,FUN=function(x){return(rownames(matrix)[x])}))
  nBC_rownames <- length(unique(BC_rownames))
  coverage <- c(NumberRows=round(nBC_rownames,0),RowPerc=round((nBC_rownames/nrow(matrix))*100,2))
  coverage_table <- (table(table(BC_rownames)))
  names(coverage_table) <- paste0("TimesRowsUsed ",names(coverage_table))
  
  ## FINAL RESULT ##
  
  OUT <- list(
    Biclust=result3,
    BiclustSim=sim_mat_result3,
    info=list(
      BiclustInitial=result1,
      BiclustSimInitial=sim_mat,
      Tree=tree_init,
      Number=c(InitialNumber=result1@Number,PatternNumber=result2@Number,FinalNumber=result3@Number),
      GapStat=gap_out,
      BC.Merge=BC.Merge,
      MergedColPatterns=MergedColPatterns,
      MergedNoiseThresholds=result2@info$Noise.Threshold,
      Coverage=list(RowCoverage=coverage,RowCoverageTable=coverage_table),
      call=pm
    )
  )
  class(OUT) <- "BiBitWorkflow"
  
  
  if(plot.type=="file" & any(plots %in% 3:5)){dev.off()}
  
  
  # results to save:
  # - Final result
  # - no noise result
  # - similarity matrix
  # - final similarity matrix
  # - somewhere how many BC's we had, then patterns, then final BC's
  # - also save all gap statistics (not only the chosen one)
  # - save info of coverage
  return(OUT)
}


#' @title Apply Fisher Exact Test on Bicluster Rows
#' @description Accepts a Biclust or BiBitWorkflow result and applies the Fisher Exact Test for each row (see Details).
#' @param result A Biclust or BiBitWorkflow Object.
#' @param matrix Accompanying binary data matrix which was used to obtain \code{result}.
#' @param p.adjust Which method to use when adjusting p-values, see \code{\link[stats]{p.adjust}} (default=\code{"BH"}).
#' @param alpha Significance level (adjusted p-values) when constructing the \code{FisherInfo} object (default=0.05).
#' @param pattern Numeric vector for which patterns/biclusters the Fisher Exact Test needs to be computed (default = all patterns/biclusters).
#' @details Extracts the patterns from either a \code{Biclust} or \code{BiBitWorkflow} object (see below). 
#' Afterwards for each pattern all rows will be tested using the Fisher Exact Test. This test compares the part of the row inside the pattern (of the bicluster) with the part of the row outside the pattern.
#' The Fisher Exact Test gives you some information on if the row is uniquely active for this pattern.
#' 
#' Depending on the \code{result} input, different patterns will be extract and different info will be returned:
#' \describe{
#' \item{\emph{Biclust S4 Object}}{
#' 
#' Using the column patterns of the Biclust result, all rows are tested using the Fisher Exact Test. 
#' Afterwards the following 2 objects are added to the \code{info} slot of the Biclust object:
#' \itemize{
#' \item \code{FisherResult}: A list object (one element for each pattern) of data frames (Number of Rows \eqn{\times} 6) which contain the names of the rows (\code{Names}), the noise level of the row inside the pattern (\code{Noise}), the signal percentage inside the pattern (\code{InsidePerc1}), the signal percentage outside the pattern (\code{OutsidePerc1}), the p-value of the Fisher Exact Test (\code{Fisher_pvalue}) and the adjusted p-value of the Fisher Exact Test (\code{Fisher_pvalue_adj}).
#' \item \code{FisherInfo}: Info object which contains a comparison of the current row membership for each pattern with a 'new' row membership based on the significant rows (from the Fisher Exact Test) for each pattern.
#' It is a list object (one element for each pattern) of lists (6 elements). These list objects per pattern contain the number of new, removed and identical rows (\code{NewRows}, \code{RemovedRows}, \code{SameRows}) when comparing the significant rows with the original row membership (as well as their indices (\code{NewRows_index}, \code{RemovedRows_index})). The \code{MaxNoise} element contains the maximum noise of all Fisher significant rows. 
#' } 
#' }
#' \item{\emph{BiBitWorkflow S3 Object}}{ 
#' 
#' The merged column patterns (after cutting the hierarchical tree) are extracted from the BiBitWorkflow object, namely the \code{$info$MergedColPatterns} slot. 
#' Afterwards the following object is added to the \code{$info} slot of the BiBitWorkflow object:
#' \itemize{
#' \item \code{FisherResult}: Same as above
#' }
#' }
#' }
#' @author Ewoud De Troyer
#' @export
#' @return Depending on \code{result}, a \code{FisherResult} and/or \code{FisherInfo} object will be added to the \code{result} and returned (see Details).
#' @examples \dontrun{
#' ## Prepare some data ##
#' set.seed(254)
#' mat <- matrix(sample(c(0,1),5000*50,replace=TRUE,prob=c(1-0.15,0.15)),
#'               nrow=5000,ncol=50)
#' mat[1:200,1:10] <- matrix(sample(c(0,1),200*10,replace=TRUE,prob=c(1-0.9,0.9)),
#'                           nrow=200,ncol=10)
#' mat[300:399,6:15] <- matrix(sample(c(0,1),100*10,replace=TRUE,prob=c(1-0.9,0.9)),
#'                             nrow=100,ncol=10)
#' mat[400:599,21:30] <- matrix(sample(c(0,1),200*10,replace=TRUE,prob=c(1-0.9,0.9)),
#'                              nrow=200,ncol=10)
#' mat[700:799,29:38] <- matrix(sample(c(0,1),100*10,replace=TRUE,prob=c(1-0.9,0.9)),
#'                              nrow=100,ncol=10)
#' mat <- mat[sample(1:5000,5000,replace=FALSE),sample(1:50,50,replace=FALSE)]
#' 
#' ## Apply BiBitWorkflow ##
#' out <- BiBitWorkflow(matrix=mat,minr=50,minc=5,noise=0.2,cut_type="number",cut_pm=4)
#' 
#' ## Apply RowTest_Fisher on Biclust Object -> returns Biclust Object ##
#' out_new <- RowTest_Fisher(result=out$Biclust,matrix=mat)
#' # FisherResult output in info slot
#' str(out_new@info$FisherResult)
#' # FisherInfo output in info slot (comparison with original BC's)
#' str(out_new@info$FisherInfo)
#' 
#' 
#' ## Apply RowTest_Fisher on BiBitWorkflow Object -> returns BiBitWorkflow Object ##
#' out_new2 <- RowTest_Fisher(result=out,matrix=mat)
#' # FisherResult output in BiBitWorkflow info element
#' str(out_new2$info$FisherResult)
#' # Fisher output is added to "NoiseScree" plot
#' NoiseScree(result=out_new2,matrix=mat,type="Added")
#' }
RowTest_Fisher <- function(result,matrix,p.adjust="BH",alpha=0.05,pattern=NULL){
  # Accepts Biclust output
  # Takes output of workflow
  # Shows significant rows (which are new, which are gone,...) (only when not a workflow output)

  if(class(result)!="Biclust" & class(result)!="BiBitWorkflow"){stop("result needs to be of class 'Biclust' or 'BiBitWorkflow'")}  
  if(class(matrix)!="matrix"){stop("matrix parameter should contain a matrix object",call.=FALSE)}
  if(!identical(as.numeric(as.vector(matrix)),as.numeric(as.logical(matrix)))){stop("matrix is not a binary matrix!",call.=FALSE)}
  if(is.null(rownames(matrix))){rownames(matrix) <- paste0("Row",c(1:nrow(matrix)))}
  if(is.null(colnames(matrix))){colnames(matrix) <- paste0("Col",c(1:ncol(matrix)))}
  if(!(p.adjust %in% c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"))){stop("Incorrect p.adjust")}
  if(length(p.adjust)!=1){stop("p.adjust needs to be of length 1")}
  biclust_correctdim(result=result,matrix=matrix)
  

  workflow <- FALSE
  if(class(result)=="BiBitWorkflow"){
    workflow_result <- result
    workflow <- TRUE
    cat("BiBitWorkflow Object: ",length(workflow_result$info$MergedColPatterns),"Patterns\n")
    NumberxCol <- do.call(rbind,lapply(workflow_result$info$MergedColPatterns,FUN=function(pattern){
      temp <- logical(ncol(matrix))
      temp[pattern] <- TRUE
      return(temp)
    }))
    result <- new("Biclust",
                  Parameters=workflow_result$Biclust@Parameters,
                  RowxNumber=matrix(FALSE,nrow=nrow(matrix),ncol=nrow(NumberxCol)),
                  NumberxCol=NumberxCol,
                  Number=nrow(NumberxCol),
                  info=workflow_result$Biclust@info
    )
  }
  

  # Select Patterns
  if(is.null(pattern)){pattern <- seq_len(result@Number)}
  if(any(pattern>result@Number)|any(pattern<=0)){stop("Incorrect pattern choice.")}
  
  
  # Apply Fisher
  fisher_output <- workflow_test_all_rows(result=result,matrix=matrix,p.adjust=p.adjust,pattern=pattern)
  
  if(workflow){
    workflow_result$info$FisherResult <- fisher_output
    return(workflow_result)
  }else{
    
    # If biclust input, we can compare with BC result
    fisher_info <- vector("list",length(pattern))
    names(fisher_info) <- names(fisher_output)
    
    for(i.pat in seq_len(length(pattern))){
      
      sign_rows <- which(fisher_output[[i.pat]]$Fisher_pvalue_adj<=alpha)
      BC_rows <- which(result@RowxNumber[,pattern[i.pat]])
      
      NewRows <- (setdiff(sign_rows,BC_rows))
      RemovedRows <- (setdiff(BC_rows,sign_rows))
      SameRows <- intersect(BC_rows,sign_rows)
      
      fisher_info[[i.pat]] <- list(NewRows=length(NewRows),
                                         RemovedRows=length(RemovedRows),
                                         SameRows=length(SameRows),
                                         NewRows_index=NewRows,
                                         RemovedRows_index=RemovedRows,
                                         MaxNoise=max(apply(matrix[sign_rows,result@NumberxCol[pattern[i.pat],]],MARGIN=1,FUN=function(x){sum(x==0)})))
      
    }
    
    result@info$FisherResult <- fisher_output
    result@info$FisherInfo <- fisher_info
    
    return(result)
  }
  
}


# NOTE: REMEMBER TO LINK EARLIER BACK TO THIS FUNCTION!!! (in noise_select)
#' @title Noise Scree Plots
#' @description Extract patterns from either a Biclust or BiBitWorkflow object (see Details) and plot the Noise Scree plot (same as plot 4 in \code{\link{BiBitWorkflow}}). Additionally, if \code{FisherResult} is available (from \code{\link{RowTest_Fisher}}), this info will be added to the plot.
#' @param result A Biclust or BiBitWorkflow Object.
#' @param matrix Accompanying binary data matrix which was used to obtain \code{result}.
#' @param type Either \code{"Added"} or \code{"Total"}. Should the noise level be plotted against the number of added rows (at that noise level) or the total number of rows (up to that noise level)?
#' @param pattern Numeric vector for which patterns the noise scree plot should be drawn (default = all patterns).
#' @param noise_select Should an automatic noise selection be applied and drawn (blue vertical line) on the plot? (Using ad hoc method to find the elbow/kink in the Noise Scree plots)
#' \itemize{
#' \item \code{noise_select=0}: No noise selection is applied and no line is drawn (default).
#' \item \code{noise_select=1}: Using the Noise Scree plot (with 'Added Rows' on the y-axis), find the noise level where the current number of added rows at this noise level is larger than the mean of 'added rows' at the lower noise levels. 
#' After locating this noise level, lower the noise level by 1. This is your automatically selected elbow/kink and therefore your noise level.
#' \item \code{noise_select=2}: Applies the same steps as for \code{noise_select=1}, but instead of decreasing the noise level by only 1, keep decreasing the noise level until the number of added rows isn't decreasing anymore either.
#' } 
#' @param alpha If info from the Fisher Exact test is available, which significance level should be used to in the plot (Noise versus Significant Fisher Exact Test rows). (default=0.05)
#' @details 
#' \describe{
#' \item{\emph{Biclust S4 Object}}{
#' 
#' Using the column patterns of the Biclust result, the noise level is plotted versus the number of \code{"Total"} or \code{"Added"} rows.
#' }
#' \item{\emph{BiBitWorkflow S3 Object}}{
#' 
#' The merged column patterns (after cutting the hierarchical tree) are extracted from the BiBitWorkflow object, namely the \code{$info$MergedColPatterns} slot. 
#' These patterns are used to plot the noise level versus the number of \code{"Total"} or \code{"Added"} rows.
#' }
#' }
#' If information on the Fisher Exact Test is available, then this info will added to the plot (noise level versus significant rows).
#' @author Ewoud De Troyer
#' @export
#' @return \code{NULL}
#' @examples \dontrun{
#' ## Prepare some data ##
#' set.seed(254)
#' mat <- matrix(sample(c(0,1),5000*50,replace=TRUE,prob=c(1-0.15,0.15)),
#'               nrow=5000,ncol=50)
#' mat[1:200,1:10] <- matrix(sample(c(0,1),200*10,replace=TRUE,prob=c(1-0.9,0.9)),
#'                           nrow=200,ncol=10)
#' mat[300:399,6:15] <- matrix(sample(c(0,1),100*10,replace=TRUE,prob=c(1-0.9,0.9)),
#'                             nrow=100,ncol=10)
#' mat[400:599,21:30] <- matrix(sample(c(0,1),200*10,replace=TRUE,prob=c(1-0.9,0.9)),
#'                              nrow=200,ncol=10)
#' mat[700:799,29:38] <- matrix(sample(c(0,1),100*10,replace=TRUE,prob=c(1-0.9,0.9)),
#'                              nrow=100,ncol=10)
#' mat <- mat[sample(1:5000,5000,replace=FALSE),sample(1:50,50,replace=FALSE)]
#' 
#' ## Apply BiBitWorkflow ##
#' out <- BiBitWorkflow(matrix=mat,minr=50,minc=5,noise=0.2,cut_type="number",cut_pm=4)
#' # Make Noise Scree Plot - Default
#' NoiseScree(result=out,matrix=mat,type="Added")
#' NoiseScree(result=out,matrix=mat,type="Total")
#' # Make Noise Scree Plot - Use Automatic Noies Selection
#' NoiseScree(result=out,matrix=mat,type="Added",noise_select=2)
#' NoiseScree(result=out,matrix=mat,type="Total",noise_select=2)
#' 
#' ## Apply RowTest_Fisher on BiBitWorkflow Object ##
#' out2 <- RowTest_Fisher(result=out,matrix=mat)
#' # Fisher output is added to "NoiseScree" plot
#' NoiseScree(result=out2,matrix=mat,type="Added")
#' NoiseScree(result=out2,matrix=mat,type="Total")
#' }
NoiseScree <- function(result,matrix,type=c("Added","Total"),pattern=NULL,noise_select=0,alpha=0.05){
  # accepts biclust output OR list of patterns OR give workflow a class
  # if Fisher info available added to plot (no noise select)
  # able to choose added or total
  # choose all or specific BC
  
  ## PARAMETER CHECKS ##
  if(class(matrix)!="matrix"){stop("matrix parameter should contain a matrix object",call.=FALSE)}
  if(!identical(as.numeric(as.vector(matrix)),as.numeric(as.logical(matrix)))){stop("matrix is not a binary matrix!",call.=FALSE)}
  if(class(result)!="Biclust" & class(result)!="BiBitWorkflow"){stop("result needs to be of class 'Biclust' or 'BiBitWorkflow'")}  
  if(is.null(rownames(matrix))){rownames(matrix) <- paste0("Row",c(1:nrow(matrix)))}
  if(is.null(colnames(matrix))){colnames(matrix) <- paste0("Col",c(1:ncol(matrix)))}
  if(!(type %in% c("Added","Total"))){stop("type needs to be \"Added\" or \"Total\"")}
  if(length(type)!=1){stop("type needs to be of length 1")}
  biclust_correctdim(result=result,matrix=matrix)
  
  
  
  if(class(result)=="BiBitWorkflow"){
    workflow_result <- result
    cat("BiBitWorkflow Object: ",length(workflow_result$info$MergedColPatterns),"Patterns\n")
    NumberxCol <- do.call(rbind,lapply(workflow_result$info$MergedColPatterns,FUN=function(pattern){
      temp <- logical(ncol(matrix))
      temp[pattern] <- TRUE
      return(temp)
    }))
    result <- new("Biclust",
                  Parameters=workflow_result$Biclust@Parameters,
                  RowxNumber=matrix(FALSE,nrow=nrow(matrix),ncol=nrow(NumberxCol)),
                  NumberxCol=NumberxCol,
                  Number=nrow(NumberxCol),
                  info=workflow_result$Biclust@info
    )
    if("FisherResult" %in% names(workflow_result$info)){result@info$FisherResult <- workflow_result$info$FisherResult}
  }
  

  # Fisher result available? If so, see of which patterns
  fisher <- ifelse("FisherResult"%in%names(result@info),TRUE,FALSE)
  if(fisher){
    fisher_patterns <- as.numeric(gsub("Pattern","",names(result@info$FisherResult)))
  }
  
  # Select Patterns
  if(is.null(pattern)){pattern <- seq_len(result@Number)}
  if(any(pattern>result@Number)){stop("Incorrect pattern choice.")}
  
  # Calculate NoisexNumber
  NoisexNumber <- apply(result@NumberxCol,MARGIN=1,FUN=function(pattern){
    return(apply(matrix[,pattern],MARGIN=1,FUN=function(row){return(sum(row==0))}))
  })
  
  # Calculate data_noisescree
  data_noisescree <- lapply(seq_len(ncol(NoisexNumber)),FUN=function(pattern){
    tab <- table(NoisexNumber[,pattern])
    return(data.frame(Noise=as.numeric(names(tab)),Total=as.numeric(tab)))
  })
  
  # Automatic Noise choice
  # if(noise_select>0){
    noise <- workflow_noisethreshold(noise=1,noise_select=noise_select,data_noisescree=data_noisescree)
  # }
  
  # Make Plot
  tempdim <- sqrt(length(pattern))
  extradim <- ifelse((floor(tempdim)*ceiling(tempdim))<length(pattern),1,0)
  par(mfrow=c(floor(tempdim)+extradim,ceiling(tempdim)))
  
  patcols <- apply(result@NumberxCol,MARGIN=1,FUN=sum)
  
  for(i.pat in pattern){
    temp_data <- data_noisescree[[i.pat]]
    if(type=="Total"){
      temp_data$Total <- cumsum(temp_data$Total)
      ylab_temp <- "Total Number"
    }else{
      ylab_temp <- "Added Number"
    }

    plot(temp_data$Noise,temp_data$Total,main=paste0("Pat",i.pat," (",patcols[i.pat]," cols)"),xlab="Noise",ylab=ylab_temp,pch=19)
    points(temp_data$Noise,temp_data$Total,type="l")
    text(temp_data$Noise,temp_data$Total,as.character(temp_data$Total),pos=3)
    
    
    # Add fisher line if available
    if(fisher){
      if((i.pat %in% fisher_patterns)){
        index <- which(i.pat==fisher_patterns)
        
        fishernoise <- (result@info$FisherResult[[index]]$Noise[result@info$FisherResult[[index]]$Fisher_pvalue_adj<=alpha])
        fisherdata <- data.frame(Noise=temp_data$Noise,Total=sapply(temp_data$Noise,FUN=function(x){return(sum(fishernoise==x))}))
        
        if(type=="Total"){
          fisherdata$Total <- cumsum(fisherdata$Total)
        }
        
        points(fisherdata$Noise,fisherdata$Total,pch=19,col="red")
        points(fisherdata$Noise,fisherdata$Total,type="l",col="red")
        text(fisherdata$Noise,fisherdata$Total,as.character(fisherdata$Total),col="red",pos=1,xpd=TRUE)
        
        legend("topleft",c("All","Fisher Sign."),bty="n",lty=1,col=c("black","red"),cex=0.8,seg.len=0.5)
      }
    }
    
    
    if(noise_select>0){
      abline(v=noise[i.pat],col="blue")
    }else{
      
      abline(v=ifelse((noise[i.pat]<1) & (noise[i.pat]>0),ceiling(noise[i.pat]*patcols[i.pat]),noise[i.pat]),col="blue")
      
    }
  }
  
  
  par(mfrow=c(1,1))
  return(NULL)
}


# Help function which takes BiBitWorkflow result and reapplies it multiple times for different number of clusters, then plots row coverage
# More clusters: more row coverage, smaller patterns ; Less clusters: less coverage, larger patterns (depending on initial bibit result)

#' @title Row Coverage Plots
#' @description Plotting function to be used with the \code{\link{BiBitWorkflow}} output. It plots the number of clusters (of the hierarchical tree) versus the number/percentage of row coverage and number of final biclusters (see Details for more information).
#' @param result A BiBitWorkflow Object.
#' @param matrix Accompanying binary data matrix which was used to obtain \code{result}.
#' @param maxCluster Maximum number of clusters to cut the tree at (default=20).
#' @param noise The allowed noise level when growing the rows on the merged patterns after cutting the tree. (default=\code{0.1}, namely allow 10\% noise.)
#' \itemize{
#' \item \code{noise=0}: No noise allowed.
#' \item \code{0<noise<1}: The \code{noise} parameter will be a noise percentage. The number of allowed 0's in a row in the bicluster will depend on the column size of the bicluster.
#' More specifically \code{zeros_allowed = ceiling(noise * columnsize)}. For example for \code{noise=0.10} and a bicluster column size of \code{5}, the number of allowed 0's would be \code{1}.
#' \item \code{noise>=1}: The \code{noise} parameter will be the number of allowed 0's in a row in the bicluster independent from the column size of the bicluster. In this noise option, the noise parameter should be an integer.
#' }
#' @param noise_select Should the allowed noise level be automatically selected for each pattern? (Using ad hoc method to find the elbow/kink in the Noise Scree plots)
#' \itemize{
#' \item \code{noise_select=0}: Do \emph{NOT} automatically select the noise levels. Use the the noise level given in the \code{noise} parameter (default)
#' \item \code{noise_select=1}: Using the Noise Scree plot (with 'Added Rows' on the y-axis), find the noise level where the current number of added rows at this noise level is larger than the mean of 'added rows' at the lower noise levels. 
#' After locating this noise level, lower the noise level by 1. This is your automatically selected elbow/kink and therefore your noise level.
#' \item \code{noise_select=2}: Applies the same steps as for \code{noise_select=1}, but instead of decreasing the noise level by only 1, keep decreasing the noise level until the number of added rows isn't decreasing anymore either.
#' } 
#' @param plots Vector for which plots to draw:
#' \enumerate{
#' \item Number of Clusters versus Row Coverage Percentage
#' \item Number of Clusters versus Number of Row Coverage
#' \item Number of Clusters versus Final Number of Biclusters
#' }
#' @param verbose Logical value if the progress bar of merging/growing the biclusters should be shown. (default=\code{TRUE})
#' @param plot.type Output Type
#' \itemize{
#' \item \code{"device"}: All plots are outputted to new R graphics devices (default).
#' \item \code{"file"}: All plots are saved in external files. Plots are joint together in a single \code{.pdf} file.
#' \item \code{"other"}: All plots are outputted to the current graphics device, but will overwrite each other. Use this if you want to include one or more plots in a sweave/knitr file or if you want to export a single plot by your own chosen format.
#' }
#' @param filename Base filename (with/without directory) for the plots if \code{plot.type="file"} (default=\code{"RowCoverage"}).
#' @details The graph of number of chosen tree clusters versus the final row coverage can help you to make a decision on how many clusters to choose in the hierarchical tree. 
#' The more clusters you choose, the smaller (albeit more similar) the patterns are and the more rows will fit your patterns (i.e. more row coverage). 
#' @author Ewoud De Troyer
#' @export
#' @return A data frame containing the number of clusters and the corresponding number of row coverage, percentage of row coverage and the number of final biclusters. 
#' @examples \dontrun{
#' ## Prepare some data ##
#' set.seed(254)
#' mat <- matrix(sample(c(0,1),5000*50,replace=TRUE,prob=c(1-0.15,0.15)),
#'               nrow=5000,ncol=50)
#' mat[1:200,1:10] <- matrix(sample(c(0,1),200*10,replace=TRUE,prob=c(1-0.9,0.9)),
#'                           nrow=200,ncol=10)
#' mat[300:399,6:15] <- matrix(sample(c(0,1),100*10,replace=TRUE,prob=c(1-0.9,0.9)),
#'                             nrow=100,ncol=10)
#' mat[400:599,21:30] <- matrix(sample(c(0,1),200*10,replace=TRUE,prob=c(1-0.9,0.9)),
#'                              nrow=200,ncol=10)
#' mat[700:799,29:38] <- matrix(sample(c(0,1),100*10,replace=TRUE,prob=c(1-0.9,0.9)),
#'                              nrow=100,ncol=10)
#' mat <- mat[sample(1:5000,5000,replace=FALSE),sample(1:50,50,replace=FALSE)]
#' 
#' ## Apply BiBitWorkflow ##
#' out <- BiBitWorkflow(matrix=mat,minr=50,minc=5,noise=0.2,cut_type="number",cut_pm=10)
#' # Make ClusterRowCoverage Plots
#' ClusterRowCoverage(result=out,matrix=mat,maxCluster=20,noise=0.2)
#' }
ClusterRowCoverage <- function(result,matrix,maxCluster=20,
                               noise=0.1,noise_select = 0,
                               plots=c(1:3),
                               verbose=TRUE,
                               plot.type="device",filename="RowCoverage"){
  
  if(!all(plots%in%c(1:3))){stop("plots should be part of c(1,2,3)")}
  if(length(plot.type)!=1){stop("plot.type should be of length 1",call.=FALSE)}
  if(!(plot.type %in% c("device","file","other"))){stop("plot.type should be 'device', 'file' or 'other'",call.=FALSE)}
  FIRSTPLOT <- TRUE
  
  ## PARAMETER CHECKS ##
  if(class(result)!="BiBitWorkflow"){stop("result needs to be of class 'BiBitWorkflow'")}  
  if(class(matrix)!="matrix"){stop("matrix parameter should contain a matrix object",call.=FALSE)}
  if(!identical(as.numeric(as.vector(matrix)),as.numeric(as.logical(matrix)))){stop("matrix is not a binary matrix!",call.=FALSE)}
  if(is.null(rownames(matrix))){rownames(matrix) <- paste0("Row",c(1:nrow(matrix)))}
  if(is.null(colnames(matrix))){colnames(matrix) <- paste0("Col",c(1:ncol(matrix)))}
  biclust_correctdim(result=result,matrix=matrix)
  
  

  nrow <- length(2:maxCluster)
  cov_df <- data.frame(clusters=rep(NA,nrow),NumberRows=rep(NA,nrow),RowPerc=rep(NA,nrow),FinalBC=rep(NA,nrow))
  
  if(verbose){
    cat("Merging clusters and growing rows:\n")
    pb <- txtProgressBar(min=0,max=sum(1:nrow),initial=0,style=3)
  }
  
  
  for(i in 1:nrow){
    # cat(i,"\n")
    # temp <- capture.output({
    #   out_temp <- BiBitWorkflow(matrix=matrix,
    #                             cut_type="number",cut_pm=i+1,
    #                             noise=noise,noise_select = noise_select,
    #                             plots=c(),
    #                             BCresult = result$info$BiclustInitial,
    #                             simmatresult = result$info$BiclustSimInitial,
    #                             treeresult = result$info$Tree)
    #   
    # })
    # cov_df[i,] <- c(i+1,out_temp$info$Coverage$RowCoverage,out_temp$Biclust@Number)
    
    temp <- capture.output({
    
    result2 <- workflow_mergeBC(result=result$info$BiclustInitial,tree=result$info$Tree,JI=NULL,number=i+1)
    
    BC.Merge <- result2@info$BC.Merge
    MergedColPatterns <- lapply(as.list(seq_len(result2@Number)),FUN=function(pattern){
      return(which(result2@NumberxCol[pattern,]))
    })
    names(MergedColPatterns) <- paste0("Pat",seq_len(result2@Number))
    
    NoisexNumber <- apply(result2@NumberxCol,MARGIN=1,FUN=function(pattern){
      return(apply(matrix[,pattern],MARGIN=1,FUN=function(row){return(sum(row==0))}))
    })
    data_noisescree <- lapply(seq_len(ncol(NoisexNumber)),FUN=function(pattern){
      tab <- table(NoisexNumber[,pattern])
      return(data.frame(Noise=as.numeric(names(tab)),Total=as.numeric(tab)))
    })
    noise_threshold <- workflow_noisethreshold(noise,noise_select,data_noisescree)
    result2@info$Noise.Threshold <- noise_threshold
    
    result3 <-  workflow_UpdateBiclust_RowNoise(result=result2,matrix=matrix, noise=result2@info$Noise.Threshold,removeBC=TRUE,NoisexNumber=NoisexNumber)
    })
    
    BC_rownames <- unlist(apply(result3@RowxNumber,MARGIN=2,FUN=function(x){return(rownames(matrix)[x])}))
    nBC_rownames <- length(unique(BC_rownames))
    coverage <- c(NumberRows=round(nBC_rownames,0),RowPerc=round((nBC_rownames/nrow(matrix))*100,2))

    
    cov_df[i,] <- c(i+1,coverage,result3@Number)
    
    if(verbose){
      setTxtProgressBar(pb,sum(1:i))
    }
    
  }
  
  if(verbose){
    close(pb)
  }
  
  if(1 %in% plots){
    if(plot.type=="device"){
      dev.new()
    }else if(plot.type=="file" & FIRSTPLOT){
      pdf(paste0(filename,".pdf"))
      FIRSTPLOT <- FALSE
    }
    plot(cov_df$clusters,cov_df$RowPerc,main="Clusters vs Row Coverage Perc.",xlab="n clusters",ylab="Row CovPerc",pch=19)
    points(cov_df$clusters,cov_df$RowPerc,type="l")
    text(cov_df$clusters,cov_df$RowPerc,as.character(round(cov_df$RowPerc,2)),pos=3)
  }
  
  if(2 %in% plots){
    if(plot.type=="device"){
      dev.new()
    }else if(plot.type=="file" & FIRSTPLOT){
      pdf(paste0(filename,".pdf"))
      FIRSTPLOT <- FALSE
    }
    plot(cov_df$clusters,cov_df$NumberRows,main="Clusters vs Total Number Rows",xlab="n clusters",ylab="Total Number Rows",pch=19)
    points(cov_df$clusters,cov_df$NumberRows,type="l")
    text(cov_df$clusters,cov_df$NumberRows,as.character(cov_df$NumberRows),pos=3)
  }
  
  if(3 %in% plots){
    if(plot.type=="device"){
      dev.new()
    }else if(plot.type=="file" & FIRSTPLOT){
      pdf(paste0(filename,".pdf"))
      FIRSTPLOT <- FALSE
    }
    plot(cov_df$clusters,cov_df$FinalBC,main="Clusters vs Final Number BC",xlab="n clusters",ylab="Final BC",pch=19)
    points(cov_df$clusters,cov_df$FinalBC,type="l")
    text(cov_df$clusters,cov_df$FinalBC,as.character(cov_df$FinalBC),pos=3)
    
  }
  if(plot.type=="file" & length(plots)>0){
    dev.off()
  }
  
  return(cov_df)
}



#' @title Compare Biclustering Results using Jaccard Index
#' @description Creates a heatmap and returns a similarity matrix of the Jaccard Index (Row, Column or both dimensions) in order to compare 2 different biclustering results or compare the biclusters of a single result.
#' @details The Jaccard Index between two biclusters is calculated as following:
#' \deqn{JI(BC1,BC2) = \frac{(m_1+m_2-m_{12})}{m_{12}}}
#' in which
#' \itemize{
#' \item \code{type="row"} or \code{type="col"}   
#' \itemize{
#' \item \eqn{m_1=} Number of rows/columns of BC1
#' \item \eqn{m_2=} Number of rows/columns of BC2
#' \item \eqn{m_{12}=} Number of rows/columns of union of row/column membership of BC1 and BC2
#' }
#' \item \code{type="both"}
#' \itemize{
#' \item \eqn{m_1=} Size of BC1 (rows times columns)
#' \item \eqn{m_2=} Size of BC2 (rows times columns)
#' \item \eqn{m_{12}= m_1+m_2 -} size of overlapping BC of BC1 and BC2 
#' }
#' }
#' @param BCresult1 A S4 Biclust object. If only this input Biclust object is given, the biclusters of this single result will be compared.
#' @param BCresult2 A second S4 Biclust object to which \code{BCresult1} should be compared. (default=\code{NULL})
#' @param type Of which dimension should the Jaccard Index be computed? Can be \code{"row"}, \code{"col"} or \code{"both"} (default).
#' @param plot Logical value if plot should be outputted (default=\code{TRUE}).
#' @author Ewoud De Troyer
#' @export
#' @return A list containing
#' \itemize{
#' \item \code{SimMat}: The JI Similarity Matrix between the compared biclusters.
#' \item \code{MaxSim}: A list containing the maximum values on each row (\code{BCResult1}) and each column (\code{BCResult2}).
#' }
#' @examples
#' \dontrun{
#' data <- matrix(sample(c(0,1),100*100,replace=TRUE,prob=c(0.9,0.1)),nrow=100,ncol=100)
#' data[1:10,1:10] <- 1 # BC1
#' data[11:20,11:20] <- 1 # BC2
#' data[21:30,21:30] <- 1 # BC3
#' data <- data[sample(1:nrow(data),nrow(data)),sample(1:ncol(data),ncol(data))]
#' 
#' # Result 1
#' result1 <- bibit(data,minr=5,minc=5)
#' result1
#' 
#' # Result 2
#' result2 <- bibit(data,minr=2,minc=2)
#' result2
#' 
#' ## Compare all BC's of Result 1 ##
#' Sim1 <- CompareResultJI(BCresult1=result1,type="both")
#' Sim1$SimMat
#' 
#' ## Compare BC's of Result 1 and 2 ##
#' Sim12 <- CompareResultJI(BCresult1=result1,BCresult2=result2,type="both",plot=FALSE)
#' str(Sim12)
#' }
CompareResultJI <- function(BCresult1,BCresult2=NULL,type="both",plot=TRUE){
  
  if(class(BCresult1)!="Biclust" & class(BCresult1)!="iBBiG"){stop("BCresult1 is not a Biclust object")}
  if(!is.null(BCresult2)){
    if(class(BCresult2)!="Biclust" & class(BCresult2)!="iBBiG"){stop("BCresult2 is not a Biclust object")}
    name_temp <- "Result2_BC"
  }else{
    BCresult2 <- BCresult1
    name_temp <- "Result1_BC"
  }
  if(length(type)>1){stop("type can only have 1 argument")}
  if(!(type%in%c("both","row","col"))){stop("type incorrect")}
  
  simmat <- matrix(0,nrow=BCresult1@Number,ncol=BCresult2@Number,dimnames=list(paste0("Result1_BC",1:BCresult1@Number),paste0(name_temp,1:BCresult2@Number)))
  
  if(type=="both"){
    main.temp <- "JI"
    for(i in 1:nrow(simmat)){
      for(j in 1:ncol(simmat)){
        row_contain_temp <- sum(which(BCresult1@RowxNumber[,i])%in%which(BCresult2@RowxNumber[,j]))
        col_contain_temp <- sum(which(BCresult1@NumberxCol[i,])%in%which(BCresult2@NumberxCol[j,]))
        m1 <- sum(BCresult1@RowxNumber[,i])*sum(BCresult1@NumberxCol[i,])
        m2 <- sum(BCresult2@RowxNumber[,j])*sum(BCresult2@NumberxCol[j,])
        m12 <- m1+m2-row_contain_temp*col_contain_temp
        simmat[i,j] <- (m1+m2-(m12))/(m12)
      }
    }
  }else if(type=="row"){
    main.temp <- "Row"
    for(i in 1:nrow(simmat)){
      for(j in 1:ncol(simmat)){
        x1 <- BCresult1@RowxNumber[,i]
        x2 <- BCresult2@RowxNumber[,j]
        m1 <- sum(x1)
        m2 <- sum(x2)
        m12 <- sum(as.logical(x1+x2))
        simmat[i,j] <- (m1+m2-m12)/m12
      }
    }
  }else if(type=="col"){
    main.temp <- "Col"
    for(i in 1:nrow(simmat)){
      for(j in 1:ncol(simmat)){
        x1 <- BCresult1@NumberxCol[i,]
        x2 <- BCresult2@NumberxCol[j,]
        m1 <- sum(x1)
        m2 <- sum(x2)
        m12 <- sum(as.logical(x1+x2))
        simmat[i,j] <- (m1+m2-m12)/m12
      }
    }
  }
  
  MaxSim1 <- apply(simmat,MARGIN=1,FUN=max)
  MaxSim2 <- apply(simmat,MARGIN=2,FUN=max)
  
  simmat_temp <- simmat
  rownames(simmat_temp) <- paste0(rownames(simmat_temp)," (",as.character(round(MaxSim1,2)),")")
  colnames(simmat_temp) <- paste0(colnames(simmat_temp)," (",as.character(round(MaxSim2,2)),")")
  
  if(plot){
    # image(t(simmat),col=viridis(256),main=paste0("Comparison ",main.temp," Matrix"),axes=FALSE)
    # axis(1,at=seq(0,1,length.out=ncol(simmat)),labels=colnames(simmat),tick=FALSE,las=2,cex.axis=0.5)
    # axis(2,at=seq(0,1,length.out=nrow(simmat)),labels=rownames(simmat),tick=FALSE,las=2,cex.axis=0.5)
    # axis(3,at=seq(0,1,length.out=ncol(simmat)),labels=as.character(round(MaxSim2,2)),tick=FALSE,cex.axis=0.6)
    # axis(4,at=seq(0,1,length.out=nrow(simmat)),labels=as.character(round(MaxSim1,2)),tick=FALSE,las=2,cex.axis=0.6)
    
    print(levelplot(t(simmat_temp),col.regions=viridis(256),main=paste0("Comparison ",main.temp," Matrix"),ylab="",xlab="",scales=list(x=list(rot=90),tck = c(1,0))))
    
  }
  
  
  return(list(SimMat=simmat,MaxSim=list(BCResult1=MaxSim1,BCResult2=MaxSim2)))
  
}




#' @title Update a Biclust or BiBitWorkflow Object with a new Noise Level
#' @description Apply a new noise level on a Biclust object result or BiBitWorkflow result. See Details on how both objects are affected.
#' @details 
#' \describe{
#' \item{\emph{Biclust S4 Object}}{
#' Using the column patterns of the Biclust result, new grows are grown using the inputted \code{noise} level.
#' The \code{removeBC} parameter decides if duplicate and non-maximal BC's should be deleted. Afterwards a new \code{Biclust} S4 object is returned with the new biclusters.
#' }
#' \item{\emph{BiBitWorkflow S3 Object}}{
#' The merged column patterns (after cutting the hierarchical tree) are extracted from the BiBitWorkflow object, namely the \code{$info$MergedColPatterns} slot. 
#' Afterwards, using the new \code{noise} level, new rows are grown and the returned object is an updated \code{BiBitWorkflow} object. (e.g. The final Biclust slot, MergedNoiseThresholds, coverage,etc. are updated)
#' }
#' }
#' @param result A Biclust or BiBitWorkflow Object.
#' @param matrix Accompanying binary data matrix which was used to obtain \code{result}.
#' @param noise The new noise level which should be used in the rows of the biclusters. (default=\code{0.1}, namely allow 10\% noise.).
#' \itemize{
#' \item \code{noise=0}: No noise allowed.
#' \item \code{0<noise<1}: The \code{noise} parameter will be a noise percentage. The number of allowed 0's in a row in the bicluster will depend on the column size of the bicluster.
#' More specifically \code{zeros_allowed = ceiling(noise * columnsize)}. For example for \code{noise=0.10} and a bicluster column size of \code{5}, the number of allowed 0's would be \code{1}.
#' \item \code{noise>=1}: The \code{noise} parameter will be the number of allowed 0's in a row in the bicluster independent from the column size of the bicluster. In this noise option, the noise parameter should be an integer.
#' }
#' 
#' 
#' 
#' @param noise_select Should the allowed noise level be automatically selected for each pattern? (Using ad hoc method to find the elbow/kink in the Noise Scree plots)
#' \itemize{
#' \item \code{noise_select=0}: Do \emph{NOT} automatically select the noise levels. Use the the noise level given in the \code{noise} parameter (default)
#' \item \code{noise_select=1}: Using the Noise Scree plot (with 'Added Rows' on the y-axis), find the noise level where the current number of added rows at this noise level is larger than the mean of 'added rows' at the lower noise levels. 
#' After locating this noise level, lower the noise level by 1. This is your automatically selected elbow/kink and therefore your noise level.
#' \item \code{noise_select=2}: Applies the same steps as for \code{noise_select=1}, but instead of decreasing the noise level by only 1, keep decreasing the noise level until the number of added rows isn't decreasing anymore either.
#' } 
#' @param removeBC \emph{(Only applicable when result is a Biclust object)} Logical value if after applying a new noise level, duplicate and non-maximal BC's should be deleted.
#' @author Ewoud De Troyer
#' @export
#' @return A \code{Biclust} or \code{BiBitWorkflow} Object (See Details)
#' @examples
#' \dontrun{
#' ## Prepare some data ##
#' set.seed(254)
#' mat <- matrix(sample(c(0,1),5000*50,replace=TRUE,prob=c(1-0.15,0.15)),
#'               nrow=5000,ncol=50)
#' mat[1:200,1:10] <- matrix(sample(c(0,1),200*10,replace=TRUE,prob=c(1-0.9,0.9)),
#'                           nrow=200,ncol=10)
#' mat[300:399,6:15] <- matrix(sample(c(0,1),100*10,replace=TRUE,prob=c(1-0.9,0.9)),
#'                             nrow=100,ncol=10)
#' mat[400:599,21:30] <- matrix(sample(c(0,1),200*10,replace=TRUE,prob=c(1-0.9,0.9)),
#'                              nrow=200,ncol=10)
#' mat[700:799,29:38] <- matrix(sample(c(0,1),100*10,replace=TRUE,prob=c(1-0.9,0.9)),
#'                              nrow=100,ncol=10)
#' mat <- mat[sample(1:5000,5000,replace=FALSE),sample(1:50,50,replace=FALSE)]
#' 
#' ## Apply BiBitWorkflow ##
#' out <- BiBitWorkflow(matrix=mat,minr=50,minc=5,noise=0.1,cut_type="number",cut_pm=4)
#' summary(out$Biclust)
#' 
#' ## Update Rows with new noise level on Biclust Obect -> returns Biclust Object ##
#' out_new <- UpdateBiclust_RowNoise(result=out$Biclust,matrix=mat,noise=0.3)
#' summary(out_new)
#' out_new@info$Noise.Threshold # New Noise Levels
#' 
#' ## Update Rows with new noise level on BiBitWorkflow Obect -> returns BiBitWorkflow Object ##
#' out_new2 <- UpdateBiclust_RowNoise(result=out,matrix=mat,noise=0.2)
#' summary(out_new2$Biclust)
#' out_new2$info$MergedNoiseThresholds # New Noise Levels
#' }
UpdateBiclust_RowNoise <- function(result,matrix, noise=0.1,noise_select=0,removeBC=FALSE){
  
  ## PARAMETER CHECKS ##
  if(class(matrix)!="matrix"){stop("matrix parameter should contain a matrix object",call.=FALSE)}
  if(!identical(as.numeric(as.vector(matrix)),as.numeric(as.logical(matrix)))){stop("matrix is not a binary matrix!",call.=FALSE)}
  if(any(noise<0)){stop("noise parameter can not be negative",call.=FALSE)}
  if(class(result)!="Biclust" & class(result)!="BiBitWorkflow"){stop("result needs to be of class 'Biclust' or 'BiBitWorkflow'")}  
  if(is.null(rownames(matrix))){rownames(matrix) <- paste0("Row",c(1:nrow(matrix)))}
  if(is.null(colnames(matrix))){colnames(matrix) <- paste0("Col",c(1:ncol(matrix)))}
  biclust_correctdim(result=result,matrix=matrix)
  
  
  workflow <- FALSE
  if(class(result)=="BiBitWorkflow"){
    workflow <- TRUE
    workflow_result <- result
    cat("BiBitWorkflow Object: ",length(workflow_result$info$MergedColPatterns),"Patterns\n")
    NumberxCol <- do.call(rbind,lapply(workflow_result$info$MergedColPatterns,FUN=function(pattern){
      temp <- logical(ncol(matrix))
      temp[pattern] <- TRUE
      return(temp)
    }))
    result <- new("Biclust",
                       Parameters=workflow_result$Biclust@Parameters,
                       RowxNumber=matrix(FALSE,nrow=nrow(matrix),ncol=nrow(NumberxCol)),
                       NumberxCol=NumberxCol,
                       Number=nrow(NumberxCol),
                       info=workflow_result$Biclust@info
                       )
    
    # Same parameters, but add noise!
  }
  
  if(length(noise)==1){noise <- rep(noise,result@Number)}else if(length(noise)!=result@Number){stop("length of noise should be 1 or equal to the number of BC's")}
  
    
  # Prep to know which patterns are removed due to no fitting rows/duplicates/containedwithin
  all_patterns <- seq_len(result@Number)
  current_patterns <- all_patterns
    
  # Transform the percentage noise values into integers
  perc_index <- which(noise<1 & noise>0)
  if(length(perc_index)>0){
    coldim <- apply(result@NumberxCol[perc_index,,drop=FALSE],MARGIN=1,FUN=sum)
    noise[perc_index] <- ceiling(noise*coldim)
  }
    
  # Calculate NoisexNumber
  NoisexNumber <- apply(result@NumberxCol,MARGIN=1,FUN=function(pattern){
    return(apply(matrix[,pattern],MARGIN=1,FUN=function(row){return(sum(row==0))}))
  })
  
  
  # Select rows and adapt Biclust object
  RowxNumber <- do.call(cbind,lapply(as.list(seq_len(result@Number)),FUN=function(pattern){
    return(NoisexNumber[,pattern]<=noise[pattern])
  }))
  rownames(RowxNumber) <- NULL
  result@RowxNumber <- RowxNumber
  
  
  if(noise_select>0){
    # Calculate data_noisescree
    data_noisescree <- lapply(seq_len(ncol(NoisexNumber)),FUN=function(pattern){
      tab <- table(NoisexNumber[,pattern])
      return(data.frame(Noise=as.numeric(names(tab)),Total=as.numeric(tab)))
    })
    noise <- workflow_noisethreshold(noise=noise[1],noise_select=noise_select,data_noisescree=data_noisescree)
  }
  
  result@info$Noise.Threshold <- noise
  
  
  
  # Check for no-row BC
  BC.delete <- apply(result@RowxNumber,MARGIN=2,FUN=sum)==0
  if(sum(BC.delete)>0){
    index <- which(BC.delete)
    result@Number <- result@Number - length(index)
    result@RowxNumber <- result@RowxNumber[,-index,drop=FALSE]
    result@NumberxCol <- result@NumberxCol[-index,,drop=FALSE]
    result@info$Noise.Threshold <- result@info$Noise.Threshold[-index]
    current_patterns <- current_patterns[-index]
    cat("Number of biclusters deleted due to no rows fitting the pattern with allowed noise:",length(index),"\n")
  }
  
  # Save current patterns temporarily in the result (this vector might decrease)
  result@info$current_patterns <- current_patterns
    
  if(removeBC){
    result <- workflow_duplicate_BC(result)
  }
    
  if(removeBC | (sum(BC.delete)>0)){
    cat("Total final BC's:",result@Number,"\n")
  }
    
  # Saving deleted patterns and remove temporary current patterns
  result@info$Deleted.Patterns <- setdiff(all_patterns,result@info$current_patterns)
  result@info$current_patterns <- NULL
  
  
  # Different output depending on Biclust of BiBitWorkflow input
  if(workflow){
    
    # Some elements of the BiBitWorkflow need to be updated
    
    workflow_result$Biclust <- result
    workflow_result$info$Number["FinalNumber"] <- result@Number
    workflow_result$info$MergedNoiseThresholds <- noise
    
    ## Compute row coverage ##
    BC_rownames <- unlist(apply(result@RowxNumber,MARGIN=2,FUN=function(x){return(rownames(matrix)[x])}))
    nBC_rownames <- length(unique(BC_rownames))
    coverage <- c(NumberRows=round(nBC_rownames,0),RowPerc=round((nBC_rownames/nrow(matrix))*100,2))
    coverage_table <- (table(table(BC_rownames)))
    names(coverage_table) <- paste0("TimesRowsUsed ",names(coverage_table))
    
    workflow_result$info$Coverage <- list(RowCoverage=coverage,RowCoverageTable=coverage_table)
    
    return(workflow_result)
    
  }else{
    return(result)
  }
  
}



workflow_UpdateBiclust_RowNoise <- function(result,matrix, noise,removeBC=FALSE,NoisexNumber,verbose=TRUE){
  
  ## PARAMETER CHECKS ##
  if(class(matrix)!="matrix"){stop("matrix parameter should contain a matrix object",call.=FALSE)}
  if(!identical(as.numeric(as.vector(matrix)),as.numeric(as.logical(matrix)))){stop("matrix is not a binary matrix!",call.=FALSE)}
  if(length(noise)==1){noise <- rep(noise,result@Number)}else if(length(noise)!=result@Number){stop("length of noise should be 1 or equal to the number of BC's")}
  if(any(noise<0)){stop("noise parameter can not be negative",call.=FALSE)}
  if(class(result)!="Biclust"){stop("result needs to be of class 'Biclust'")}  
  
  # Prep to know which patterns are removed due to no fitting rows/duplicates/containedwithin
  all_patterns <- seq_len(result@Number)
  current_patterns <- all_patterns

  # Transform the percentage noise values into integers
  perc_index <- which(noise<1 & noise>0)
  if(length(perc_index)>0){
    coldim <- apply(result@NumberxCol[perc_index,,drop=FALSE],MARGIN=1,FUN=sum)
    noise[perc_index] <- ceiling(noise*coldim)
  }

  # Select rows and adapt Biclust object
  RowxNumber <- do.call(cbind,lapply(as.list(seq_len(result@Number)),FUN=function(pattern){
    return(NoisexNumber[,pattern]<=noise[pattern])
  }))
  rownames(RowxNumber) <- NULL
  result@RowxNumber <- RowxNumber
  
  # Check for no-row BC
  BC.delete <- apply(result@RowxNumber,MARGIN=2,FUN=sum)==0
  if(sum(BC.delete)>0){
    index <- which(BC.delete)
    result@Number <- result@Number - length(index)
    result@RowxNumber <- result@RowxNumber[,-index,drop=FALSE]
    result@NumberxCol <- result@NumberxCol[-index,,drop=FALSE]
    result@info$Noise.Threshold <- result@info$Noise.Threshold[-index]
    
    current_patterns <- current_patterns[-index]
    
    if(verbose){cat("Number of biclusters deleted due to no rows fitting the pattern with allowed noise:",length(index),"\n")}
  }
  
  # Save current patterns temporarily in the result (this vector might decrease)
  result@info$current_patterns <- current_patterns
  
  
  if(removeBC & result@Number>1){
    result <- workflow_duplicate_BC(result,verbose=verbose)
  }
  
  if(removeBC | (sum(BC.delete)>0)){
    if(verbose){cat("Total final BC's:",result@Number,"\n")}
  }
  
  # Saving deleted patterns and remove temporary current patterns
  result@info$Deleted.Patterns <- setdiff(all_patterns,result@info$current_patterns)
  result@info$current_patterns <- NULL
  
  return(result)
  
}




workflow_simmat <- function(result,type="both",verbose=TRUE){
  
  
  mat_placeholder <- matrix(0,nrow=result@Number,ncol=result@Number)
  
  total_number <- (nrow(mat_placeholder)-1)*nrow(mat_placeholder)/2
  
  if(verbose){
    current_number <- 0
    pb <- txtProgressBar(min=0,max=((nrow(mat_placeholder)-1)*(ncol(mat_placeholder))/2),initial=0,style=3)
  }
  
  for(i in 1:(nrow(mat_placeholder)-1)){
    for(j in (i+1):(ncol(mat_placeholder))){
      
      mat_placeholder[i,j] <- workflow_jaccard_bc(result,i,j,type=type)
      
      if(verbose){
        current_number <- current_number+1
        setTxtProgressBar(pb,value=current_number)
      }
      
    }
  }
  if(verbose){close(pb)}
  
  mat_placeholder <- mat_placeholder+t(mat_placeholder)
  diag(mat_placeholder) <- 1
  
  return(mat_placeholder)
}

workflow_mergeBC <- function(result,tree,JI=NULL,number=NULL,verbose=TRUE){

  
  
  cut <- cutree(tree,h=(1-JI),k=number)
  new_number <- length(unique(cut))
  
  if(verbose){
  cat("Number of BC before merge:",result@Number,"\n")
  cat("Number of BC after merge:",new_number,"\n")
  cat("Merging...")
  }
  
  out <- new("Biclust",Parameters=result@Parameters,
             RowxNumber=matrix(FALSE,nrow=nrow(result@RowxNumber),ncol=new_number),
             NumberxCol=matrix(FALSE,nrow=new_number,ncol=ncol(result@NumberxCol)),
             Number=new_number,
             info=result@info)
  
  out@info$BC.Merge <- vector("list",new_number)
  
  for(i in sort(unique(cut))){
    
    BC.index <- which(i==cut)
    
    out@info$BC.Merge[[i]] <- BC.index
    
    out@RowxNumber[,i] <- as.logical(rowSums(result@RowxNumber[,BC.index,drop=FALSE]))
    out@NumberxCol[i,] <- as.logical(colSums(result@NumberxCol[BC.index,,drop=FALSE]))
  }
  
  
  if(verbose){cat("DONE\n")}
  return(out)
}

workflow_noisethreshold <- function(noise,noise_select,data_noisescree){
  if(noise_select!=0){
    ### Find where it starts increasing too much
    # 1. Find NOISE where ADD goes over mean (excluding mean of only first)
    # 2. Go back to NOISE before first increase now
    
    noise_threshold <- unlist(lapply(data_noisescree,FUN=function(data_temp){
      stop1 <- which(cumsum(data_temp$Total)/c(1:nrow(data_temp))<data_temp$Total)
      stop1 <- stop1[!stop1==2][1]
      
      if(all(data_temp$Total[1:(stop1-1)]==0)){
        return(data_temp$Noise[stop1])
      }
      
      else if(noise_select==1){
        return(data_temp$Noise[stop1-1])
      }else if(noise_select==2){
        while((data_temp$Total[stop1] - data_temp$Total[stop1-1])>0){
          stop1 <- stop1-1
          if(stop1==1){break}
        }
        return(data_temp$Noise[stop1])
      }
    }))
    
  }else{
    noise_threshold <- rep(noise,length(data_noisescree))
  }
  return(noise_threshold)
}

# workflow_patternstep <- function(result,matrix,noise,noise_select,NoisexNumber,data_noisescree){
#   
#   
# 
#   # #### ALTERNATIVE WAY: USING PATTERNBIBIT -> BUT NOT INTERESTED IN SUBPATTERNS SO OVERKILL!
#   # 
#   # pattern_matrix <- result@NumberxCol+0
#   # 
#   # out <- BiBitR:::bibit3_alt(matrix,noise=noise,pattern_matrix=pattern_matrix,subpattern = FALSE)
#   # 
#   # 
#   # out2 <- new("Biclust",Parameters=result@Parameters,
#   #             RowxNumber=do.call(cbind,lapply(out[1:result@Number],FUN=function(x){x$FullPattern@RowxNumber})),
#   #             NumberxCol=do.call(rbind,lapply(out[1:result@Number],FUN=function(x){x$FullPattern@NumberxCol})),
#   #             Number=result@Number,
#   #             info=result@info)
#   # 
#   # BC.delete <- rep(FALSE,out2@Number)
#   # for(i in 1:out2@Number){
#   #   if(sum(out2@RowxNumber[,i])==0 & sum(out2@NumberxCol[i,])==0){BC.delete[i] <- TRUE}
#   # }
#   # if(sum(BC.delete)>0){
#   #   index <- which(BC.delete)
#   #   out2@Number <- out2@Number - length(index)
#   #   out2@RowxNumber <- out2@RowxNumber[,-index,drop=FALSE]
#   #   out2@NumberxCol <- out2@NumberxCol[-index,,drop=FALSE]
#   #   out2@info$BC.Merge <- out2@info$BC.Merge[-index]
#   #   cat("Number of biclusters deleted due to no rows fitting the pattern:",length(index),"\n")
#   # }
#   # 
#   # out2 <- workflow_duplicate_BC(out2)
#   # cat("Total final BC's:",out2@Number,"\n")
#   # 
#   # return(out2)
# }



workflow_duplicate_BC <- function(result,verbose=TRUE){
  
  # cat("Checking for Duplicate Biclusters... ")
  # In order to quickly delete duplicates, BC row and column memberships are encoded to 16bit words first
  nrow_data <- nrow(result@RowxNumber)
  ncol_data <- ncol(result@NumberxCol)
  nblockscol <- ceiling(ncol_data/16)
  
  nblocksrow <- ceiling(nrow_data/16)
  
  
  decBC_mat <- matrix(NA,nrow=result@Number,ncol=nblocksrow+nblockscol,dimnames=list(colnames(result@RowxNumber),NULL))
  temp <- 1:nrow_data
  rowchunks <- split(temp,ceiling(seq_along(temp)/16))
  temp <- 1:ncol_data
  colchuncks <- split(temp,ceiling(seq_along(temp)/16))
  
  for(i.decBC in 1:result@Number){
    
    for(i.rowblock in 1:nblocksrow){
      decBC_mat[i.decBC,i.rowblock] <- strtoi(paste0(result@RowxNumber[rowchunks[[i.rowblock]],i.decBC]+0,collapse=""),2)
    }
    
    for(i.colblock in 1:nblockscol){
      matindex <- i.colblock+nblocksrow
      decBC_mat[i.decBC,matindex] <- strtoi(paste0(result@NumberxCol[i.decBC,colchuncks[[i.colblock]]]+0,collapse=""),2)
    }
  }
  
  # Change order to original BC's appear first
  dup_temp <- duplicated(decBC_mat,MARGIN=1)
  dup_index <- which(dup_temp)
  
  if(length(dup_index)>0){
    
    result@RowxNumber <- result@RowxNumber[,-dup_index,drop=FALSE]
    result@NumberxCol <- result@NumberxCol[-dup_index,,drop=FALSE]
    result@Number <- nrow(result@NumberxCol)
    result@info$Noise.Threshold <- result@info$Noise.Threshold[-dup_index]
    result@info$current_patterns <- result@info$current_patterns[-dup_index]
    
    decBC_mat <- decBC_mat[-dup_index,]
  }
  
  # cat("DONE\n")
  if(verbose){cat("Number of duplicate BC's deleted:",sum(dup_temp),"\n")}
  
  
  
  ######################## Check for contained 
  if(verbose){cat("Number of contained within BC's deleted:")}
  
  contained_vector <- rep(NA,nrow(decBC_mat))
  # note: go through all, but skip if current i.decBC or j.decBC is already in contained_vector
  
  # pb <- txtProgressBar(min=1,max=(nrow(decBC_mat)-1),initial=1,style=3)
  
  for(i.decBC in 1:(nrow(decBC_mat)-1)){
    
    ## Progress dots
    # progress_dots(i=i.decBC,nBC=nrow(decBC_mat)-1)
    # setTxtProgressBar(pb,i.decBC)
    #
    
    if(!(i.decBC%in%contained_vector)){
      for(j.decBC in (i.decBC+1):(nrow(decBC_mat))){
        if(!(j.decBC%in%contained_vector)){
          current_comp <- c(i.decBC,j.decBC)
          contained <- workflow_BCcontained(decBC_mat[i.decBC,],decBC_mat[j.decBC,])
          if(!is.null(contained)){
            contained_vector[current_comp[contained]] <- current_comp[contained]
          }
        } 
      }
    }
    
    # cat(length(contained_vector[!is.na(contained_vector)]),"\n")
  }
  # close(pb)
  
  
  contained_vector <- contained_vector[!is.na(contained_vector)]
  
  if(length(contained_vector)>0){
    
    contained_index <- contained_vector
    
    result@RowxNumber <- result@RowxNumber[,-contained_index]
    result@NumberxCol <- result@NumberxCol[-contained_index,]
    result@Number <- nrow(result@NumberxCol)
    result@info$Noise.Threshold <- result@info$Noise.Threshold[-contained_index]
    result@info$current_patterns <- result@info$current_patterns[-contained_index]
    
    
  }
  
  # cat("DONE\n")
  if(verbose){cat(length(contained_vector),"\n\n")}
  
  
  return(result)
}





workflow_BCcontained <- function(BC1word,BC2word){
  intersectword <- apply(rbind(BC1word,BC2word),MARGIN=2,FUN=function(x){bitwAnd(x[1],x[2])})
  if(all(intersectword==BC1word)){return(1)}
  if(all(intersectword==BC2word)){return(2)}
}




workflow_jaccard_bc <- function(result,BC1,BC2,type="both"){
  
  if(length(type)>1){stop("type can only have 1 argument")}
  if(!(type%in%c("both","row","col"))){stop("type incorrect")}
  
  if(type=="both"){
    row_contain_temp <- sum(which(result@RowxNumber[,BC1])%in%which(result@RowxNumber[,BC2]))
    col_contain_temp <- sum(which(result@NumberxCol[BC1,])%in%which(result@NumberxCol[BC2,]))
    
    m1 <- sum(result@RowxNumber[,BC1])*sum(result@NumberxCol[BC1,]) 
    m2 <- sum(result@RowxNumber[,BC2])*sum(result@NumberxCol[BC2,])
    m12 <- m1+m2-row_contain_temp*col_contain_temp
    JI <- (m1+m2-(m12))/(m12)
    
    return(JI)
  }else{
    
    if(type=="row"){
      x1 <- result@RowxNumber[,BC1]
      x2 <- result@RowxNumber[,BC2]
    }
    if(type=="col"){
      x1 <- result@NumberxCol[BC1,]
      x2 <- result@NumberxCol[BC2,]
    }
    
    m1 <- sum(x1)
    m2 <- sum(x2)
    m12 <- sum(as.logical(x1+x2))
    
    JI <- (m1+m2-m12)/m12
    return(JI)
  }
  
}







workflow_test_all_rows <- function(result,matrix,p.adjust="BH",pattern=NULL){

  cat("Applying Fisher Test to all rows...\n\n")
  
  out <- vector("list",length(pattern))
  names(out) <- paste0("Pattern",pattern)
  
  # # Extract all column indices which are involved in patterns
  # columns_ignore <- unique(do.call(c,lapply(split(result@NumberxCol,seq(result@Number)),FUN=which)))
  # 
  
  
  for(i.pattern in seq_len(length(pattern))){
    cat("Pattern",pattern[i.pattern],"\n")
    
    
    out_df <- apply(matrix,MARGIN=1,FUN=function(row){
      
      inside <- row[result@NumberxCol[pattern[i.pattern],]]
      outside <- row[!result@NumberxCol[pattern[i.pattern],]]

      out_df_temp <- data.frame(
        Noise=sum(inside==0),
        InsidePerc1=sum(inside==1)/length(inside),
        OutsidePerc1=sum(outside==1)/length(outside),
        # Prop_pvalue=prop.test(matrix(c(sum(inside==1),sum(inside==0),
        #                                sum(outside==1),sum(outside==0)),nrow=2,ncol=2,byrow=TRUE),
        #                       alternative="greater")$p.value,
        Fisher_pvalue=fisher.test(matrix(c(sum(inside==1),sum(outside==1),
                                           sum(inside==0),sum(outside==0)),nrow=2,ncol=2,byrow=TRUE))$p.value   
        
      )
      return(out_df_temp)
    })
    out_df <- do.call(rbind,out_df)
    
    out_df <- cbind(data.frame(Names=rownames(matrix)),out_df)
    
    
    # out_df$Prop_pvalue_adj <- p.adjust(out_df$Prop_pvalue,method=p.adjust)
    out_df$Fisher_pvalue_adj <- p.adjust(out_df$Fisher_pvalue,method=p.adjust)
    out_df$Names <- as.character(out_df$Names)
    # out_df$index <- 1:nrow(out_df) 
    
    rownames(out_df) <- NULL
    
    out[[i.pattern]] <- out_df  
  }
  return(out)
}

clust_sel = function(x,y,jrange=3:25,dd=2) {
  ## x is an array, 
  ## y is an hclust object
  wss4 = function(x,y,w = rep(1, length(y))) sum(lm(x~factor(y),weights = w)$resid^2*w)
  ### wss4 calculates within cluster sum of squares
  sm1 <- NULL
  for(i in jrange) sm1[i] = wss4(x,cutree(y,i))
  sm1=sm1[jrange]
  k = if(dd==1) sm1[-1] else -diff(sm1) 
  plot(jrange[-length(k)+1:0], -diff(k)/k[-length(k)]*100)
  jrange [sort.list(diff(k)/k[-length(k)]*100)[1:4]]
}

workflow_alternativeclusterselect <- function(result){
  
  if(class(result)!="BiBitWorkflow"){stop("result needs to be a BiBitWorkflow object")}
  
  return(clust_sel(result$info$BiclustInitial@NumberxCol,result$info$Tree))
}





#' @title Column Info of Biclusters
#' @description Function that returns which column labels are part of the pattern derived from the biclusters.
#' Additionally, a biclustmember plot and a general barplot of the column labels (retrieved from the biclusters) can be drawn.
#' @author Ewoud De Troyer
#' @export
#' @param result A Biclust Object.
#' @param matrix Accompanying data matrix which was used to obtain \code{result}.
#' @param plots Which plots to draw:
#' \enumerate{
#' \item Barplot of number of appearances of column labels in bicluster results.
#' \item Biclustmember plot of BC results (see \code{\link[biclust]{biclustmember}}).
#' }
#' @param plot.type Output Type
#' \itemize{
#' \item \code{"device"}: All plots are outputted to new R graphics devices (default).
#' \item \code{"file"}: All plots are saved in external files. Plots are joint together in a single \code{.pdf} file.
#' \item \code{"other"}: All plots are outputted to the current graphics device, but will overwrite each other. Use this if you want to include one or more plots in a sweave/knitr file or if you want to export a single plot by your own chosen format.
#' }
#' @param filename Base filename (with/without directory) for the plots if \code{plot.type="file"} (default=\code{"RowCoverage"}).
#' @return A list object (length equal to number of Biclusters) in which vectors of column labels are saved.
#' @examples \dontrun{
#' data <- matrix(sample(c(0,1),100*100,replace=TRUE,prob=c(0.9,0.1)),nrow=100,ncol=100)
#' data[1:10,1:10] <- 1 # BC1
#' data[11:20,11:20] <- 1 # BC2
#' data[21:30,21:30] <- 1 # BC3
#' data <- data[sample(1:nrow(data),nrow(data)),sample(1:ncol(data),ncol(data))]
#' result <- bibit(data,minr=5,minc=5)
#' ColInfo(result=result,matrix=data)
#' }
ColInfo <- function(result,matrix,plots=c(1,2),plot.type="device",filename="ColInfo"){
  if(length(plot.type)!=1){stop("plot.type should be of length 1",call.=FALSE)}
  if(!(plot.type %in% c("device","file","other"))){stop("plot.type should be 'device', 'file' or 'other'",call.=FALSE)}
  if(!all(plots%in%c(1:2))){stop("plots should be part of c(1,2)")}
  FIRSTPLOT <- TRUE

  ## PARAMETER CHECKS ##
  if(class(matrix)!="matrix"){stop("matrix parameter should contain a matrix object",call.=FALSE)}
  if(class(result)!="Biclust"){stop("result needs to be of class 'Biclust'")}  
  if(is.null(rownames(matrix))){rownames(matrix) <- paste0("Row",c(1:nrow(matrix)))}
  if(is.null(colnames(matrix))){colnames(matrix) <- paste0("Col",c(1:ncol(matrix)))}
  biclust_correctdim(result=result,matrix=matrix)
  
  
  out <- lapply(as.list(1:result@Number),FUN=function(i){
    return(colnames(matrix)[(result@NumberxCol[i,])])
  })
  names(out) <- paste0("BC",1:length(out))
  
  if(1 %in% plots){
    if(plot.type=="device"){
      dev.new()
    }else if(plot.type=="file" & FIRSTPLOT){
      pdf(paste0(filename,".pdf"))
      FIRSTPLOT <- FALSE
    }
    tab <- table(unlist(out))
    barplot(tab[order(tab,decreasing=TRUE)],las=2,col="lightblue",main="Number Column Appearances")
  }
  if(2 %in% plots){  
    if(plot.type=="device"){
      dev.new()
    }else if(plot.type=="file" & FIRSTPLOT){
      pdf(paste0(filename,".pdf"))
      FIRSTPLOT <- FALSE
    }
    col_t <- viridis(101)
    biclustmember(x=matrix,bicResult=result,color=col_t)
    # col_t <- diverge_hcl(101, h = c(0, 130))
    
    legend(c(0.1,1.2),c(as.character(min(matrix)),as.character(max(matrix))),col=c(col_t[1],col_t[length(col_t)]),xpd=TRUE,bty="n",pch=15)
  }
  if(plot.type=="file" & length(plots)>0){dev.off()}
  
  
  
  return(out)
}




#' @title Barplots of Column Noise for Biclusters
#' @description Draws barplots of column noise of chosen biclusters. This plot can be helpful in determining which column label is often zero in noisy biclusters.
#' @author Ewoud De Troyer
#' @export
#' @param result A Biclust Object.
#' @param matrix Accompanying binary data matrix which was used to obtain \code{result}.
#' @param BC Numeric vector to select of which BC's a column noise bar plot should be drawn.
#' @param plot.type Output Type
#' \itemize{
#' \item \code{"device"}: All plots are outputted to new R graphics devices (default).
#' \item \code{"file"}: All plots are saved in external files. Plots are joint together in a single \code{.pdf} file.
#' \item \code{"other"}: All plots are outputted to the current graphics device, but will overwrite each other. Use this if you want to include one or more plots in a sweave/knitr file or if you want to export a single plot by your own chosen format.
#' }
#' @param filename Base filename (with/without directory) for the plots if \code{plot.type="file"} (default=\code{"RowCoverage"}).
#' @return NULL
#' @examples \dontrun{
#' data <- matrix(sample(c(0,1),100*100,replace=TRUE,prob=c(0.9,0.1)),nrow=100,ncol=100)
#' data[1:10,1:10] <- 1 # BC1
#' data[11:20,11:20] <- 1 # BC2
#' data[21:30,21:30] <- 1 # BC3
#' data <- data[sample(1:nrow(data),nrow(data)),sample(1:ncol(data),ncol(data))]
#' result <- bibit2(data,minr=5,minc=5,noise=1)
#' ColNoiseBC(result=result,matrix=data,BC=1:3)
#' }
ColNoiseBC <- function(result,matrix,BC=1:result@Number,
                       plot.type="device",filename="ColNoise"){
  
 if(length(plot.type)!=1){stop("plot.type should be of length 1",call.=FALSE)}
  if(!(plot.type %in% c("device","file","other"))){stop("plot.type should be 'device', 'file' or 'other'",call.=FALSE)}
  FIRSTPLOT <- TRUE
  
  
  ## PARAMETER CHECKS ##
  if(class(result)!="Biclust"){stop("result needs to be of class 'Biclust'")}  
  if(class(matrix)!="matrix"){stop("matrix parameter should contain a matrix object",call.=FALSE)}
  if(!identical(as.numeric(as.vector(matrix)),as.numeric(as.logical(matrix)))){stop("matrix is not a binary matrix!",call.=FALSE)}
  if(is.null(rownames(matrix))){rownames(matrix) <- paste0("Row",c(1:nrow(matrix)))}
  if(is.null(colnames(matrix))){colnames(matrix) <- paste0("Col",c(1:ncol(matrix)))}
  biclust_correctdim(result=result,matrix=matrix)
  
  if(!(class(BC)=="numeric" | class(BC)=="integer")){stop("BC should be a numeric vector")}
  if(any(BC<0)){stop("BC cannot be negative")}
  if(any(BC>result@Number)){stop(paste0("BC contains a unavailable BC. The biclustering result only has ",result@Number," BC's"))}
  
  
  for(i in BC){
    temp <- 1-apply(matrix[result@RowxNumber[,i],result@NumberxCol[i,]],MARGIN=2,FUN=sum)/sum(result@RowxNumber[,i])
    
    if(plot.type=="device"){
      dev.new()
    }else if(plot.type=="file" & FIRSTPLOT){
      pdf(paste0(filename,".pdf"))
      FIRSTPLOT <- FALSE
    }
    
    set.seed(1)
    col <- distinctColorPalette(length(temp))
    
    barplot(temp,ylim=c(0,1),main=paste0("Column Noise - BC ",i),xlab="",ylab="Noise Percentage",col=col,las=2,cex.names=0.8)
    
  }
  
  if(plot.type=="file"){
    dev.off()
  }
  return(NULL)
}


# ################
# library(xtable)
# summary_t <- function(result){
#   m <- rbind(apply(result@RowxNumber,MARGIN=2,FUN=sum),apply(result@NumberxCol,MARGIN=1,FUN=sum))
#   rownames(m) <- c("Number of Rows","Number of Columns")
#   colnames(m) <- paste0("BC",1:ncol(m))
#   print(xtable(m,align=paste0("r|",paste0(rep("r",ncol(m)),collapse=""))),hline.after=c(0),table.placement="H")
# }


