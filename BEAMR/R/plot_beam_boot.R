#' Plot bootstrap output for BEAM sets
#'
#' #' plot_beam_boot produces a pairs plot of the beam stats matrices. Default is maximum of 5 plots, ordered by most significant association direction.
#'
#' @param beam.result A beam.stats object from compute_beam_stats
#' @param beam.feat.pvals A list containing feature-level p-values from compute_feature_pvalues.
#' @param beam.specs A data.frame. Default NULL, in which case beam.result$beam.specs is used. Otherwise can input other beam.specs data.frame that must contain name, mtx, mdl, plot columns.
#' @param set.id A character specifying the name of a set. Must be in beam.result$beam.data$set.data
#' @param max.plots A number specifying the max number of rows in the pairs plot. Default is 4, ordered by feature-level p-value.
#' @param z Logical indicating whether to z-scale each vector of one coefficient estimate across bootstraps before plotting. Default is TRUE.

#'
#' @returns A pairs plot figure.
#' @importFrom stats prcomp
#' @importFrom stats predict
#' @importFrom graphics pairs
#' @importFrom stringr str_split
#' @export
#'
#' @examples
#' data(beam_stats)
#' test.pvals <- compute_set_pvalues(beam.stats=beam_stats)
#' test.feat.pvals <- compute_feature_pvalues(beam.stats=beam_stats)
#' test.boot.plot <- plot_beam_boot(beam_stats, test.feat.pvals,
#'                                  set.id="ENSG00000099810")
plot_beam_boot <- function(beam.result, beam.feat.pvals, beam.specs=NULL,
                           set.id, max.plots=4, z=TRUE)
{
  # Check data
  if(!inherits(beam.result, "beam.stats"))
    stop("beam.results must be the result of compute.beam.stats.")

  #print(beam.result$beam.specs)
  if(is.null(beam.specs)){
    beam.specs <- beam.result$beam.specs
  }
  beam.specs.ord <- beam.specs[order(beam.specs$mtx),]
  beam.data <- beam.result$beam.data
  feat.pvals <- beam.feat.pvals

  # check the specs
  mtx.names=names(beam.data$mtx.data)
  main.clms=colnames(beam.data$main.data)
  spec.check=check_beam_specs(beam.specs,
                              mtx.names)

  n.spec=nrow(beam.specs)
  boot.index=beam.data$boot.index
  #print(class(boot.index))
  main.data=beam.data$main.data

  p.vec <- c()
  p.vec.name <- c()
  for(i in 1:n.spec){
    name <- beam.specs.ord[i, "name"]
    feat.pvals.sm <- feat.pvals[[name]]
    p.vec <- c(p.vec, feat.pvals.sm[grep(set.id, feat.pvals.sm$gene), grep("p", colnames(feat.pvals.sm))])
    p.vec.name <- c(p.vec.name, paste(feat.pvals.sm[grep(set.id, feat.pvals.sm$gene), grep("id", colnames(feat.pvals.sm))], name, sep=" "))

  }
  names(p.vec) <- p.vec.name

  gen_boot_plot(beam.result, set.id, z=TRUE, max.plots=max.plots, p.vec)
}

###################
# bootplot function

gen_boot_plot <- function(beam.result, set.id, z=TRUE, max.plots=NULL, p.vec)
{
  B.mtx=extract_beam_stats(beam.result, set.id)
  B.mtx <- t(B.mtx)
  cent=B.mtx[1,]                             # observed result is first row, it will be the center for PCA calculations
  B0=B.mtx[-1,]                              # remove observed result
  B.mtx=B0                                   # bootstrap association matrix
  b0=b=nrow(B.mtx)                           # number of bootstraps


  pca.res=stats::prcomp(B.mtx,                      # principal components for all bootstraps
                        center=cent,                # use the observed result as the center
                        scale.=z)               # user option for rescaling PCs before computing distance

  null=matrix(0,1,ncol(B.mtx))               # define vector for null (origin: all coefs = 0)
  colnames(null)=colnames(B.mtx)             # assign column names
  pca.null=stats::predict(pca.res,null)             # project original null into PC space
  pca.mtx=rbind(pca.res$x,pca.null)          # matrix of PCs (bootstraps and null)

  pca.dist=rowSums(pca.mtx^2)                # distance of null and each bootstrap from observed
  p.indx=which(pca.dist[-(b+1)]>=pca.dist[(b+1)])    # index of bootstraps farther from observed than null

  # put observed result and null last so they plot on top
  gray.indx=setdiff(1:b0,p.indx)

  B.mtx=rbind(B0[gray.indx,],
              B0[p.indx,],
              cent,0)

  clr=c(rep("gray",b0-length(p.indx)),    # gray for bootstrap results closer to center than is the null
        rep("black",length(p.indx)),     # black for bootstrap results farther from center than is the null
        "#F8766D","#00BFC4")                    # red for observed result and blue for null

  pt=c(rep(1,b0-length(p.indx)),          # circle for bootstrap results closer to center than is the null
       rep(3,length(p.indx)),            # black for bootstrap results farther from center than is the null
       19,19)                            # red for observed result and blue for null

  labels <- sub("_", " ", colnames(B.mtx))
  labels.df <-do.call(rbind.data.frame, stringr::str_split(labels, " ", 2))
  colnames(labels.df) <- NULL
  labels.new <- labels.df[,2]
  labels.new.sm <- paste0("X",1:dim(B.mtx)[2], sep="")

  if(is.null(max.plots)){
    if(dim(B.mtx)[2]<=4){
      try(graphics::pairs(B.mtx,                         # generate scatterplot matrix
                          col=clr,
                          pch=pt,
                          main=set.id,
                          labels=labels.new,
                          cex=2))
    }
    else{
      print(paste(strwrap(paste(paste0(labels.new.sm, "=", labels.new),collapse="; "))))
      try(graphics::pairs(B.mtx,                         # generate scatterplot matrix
                          #oma=c(9,0,6,0),
                          col=clr,
                          pch=pt,
                          main=set.id,
                          labels=labels.new.sm))
      #mtext(paste(strwrap(paste(paste0(labels.new.sm, "=", labels.new),collapse="; "), width=100), collapse="\n"), side=1, outer=TRUE)
    }
  }
  else{
    if (max.plots > dim(B.mtx)[2])
      stop("max.plots must be less than or equal to the total number of rows.")
    index <- c(1:dim(B.mtx)[2])
    reor_idx <- match(colnames(B.mtx), names(p.vec))
    p.vec.or <- p.vec[reor_idx]
    ind.df <- cbind.data.frame(index, p.vec.or)
    ind.ord.df <- ind.df[order(p.vec.or),]      # order by individual p-value to extract the most significant
    max.ind <- ind.ord.df$index[1:max.plots]
    B.mtx.sm <- B.mtx[,max.ind]
    if(max.plots <=4){
      try(graphics::pairs(B.mtx.sm,                         # generate scatterplot matrix
                          col=clr,
                          pch=pt,
                          main=set.id,
                          labels=labels.new[max.ind],
                          cex=2))
    }
    else{
      #par(oma=c(3,0,0,0))
      print(paste(strwrap(paste(paste0(labels.new.sm[max.ind], "=", labels[max.ind]),collapse="; "))))
      try(graphics::pairs(B.mtx.sm,      # generate scatterplot matrix
                          #oma=c(6,3,6,3),
                          col=clr,
                          pch=pt,
                          main=set.id,
                          labels=labels.new.sm[max.ind]))
      #mtext(set.id, side=3)
      #mtext(paste(strwrap(paste(paste0(labels.new.sm[max.ind], "=", labels.new[max.ind]),collapse="; "), width=100), collapse="\n"), side=1, outer=TRUE)
    }

  }
}
