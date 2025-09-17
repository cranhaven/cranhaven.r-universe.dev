#' Generate BEAM Plot List
#'
#' Internal function: generate a list of clinical feature plots.
#'
#' @param beam.result Result of prep.beam.data
#' @param beam.specs A data.frame of strings with columns name, mtx, mdl, plot
#' @param beam.feat.pvals List of feature-level p-values from compute_feature_pvalues
#' @param number.pairs Numeric; number of features to display in clinical plots, ordered by significance
#' @param pair.order One of c("both", "omic", "endpoint"). Default is "both." Specify how to choose feature-endpoint plots to include. If "both", find the best (based on q, p, effect size) feature-omic pair for each type of omic and each endpoint separately. If "omic", within each omic, find the best feature-endpoint pair and then plot this feature with all endpoints. If "endpoint", need to specify endpt.order as the name of chosen endpoint. Then, within each omic, find the feature with best association with the selected endpoint, and plot this feature for all endpoints.
#' @param endpt.order Default NULL. If pair.order="endpoint", specify character with endpoint name (from beam.specs$name, after the period).
#' @param set.id A character with set name; must be in beam.result$beam.data$set.data$set.id
#' @param feat.id Default NULL; a character with feature name; must be in beam.result$beam.data$set.data$row.id
#' @param title.size A numeric. Specify the size of individual plot titles. Default is 10.
#'
#' @returns A list of plots for the specified set and/or feature.
#' @import ggplot2
#' @import survival
#' @import survminer
#' @import ggmosaic
#' @importFrom rlist list.append
#' @export
#'
#' @examples
#' data(beam_stats)
#' test.feat.pvals <- compute_feature_pvalues(beam.stats=beam_stats)
#' plot.specs <- prep_beam_plot(beam.data=beam_stats$beam.data,
#'                              beam.specs=beam_stats$beam.specs)
#' plot.list <- gen_beam_plot_list(beam.result=beam_stats, beam.specs=plot.specs,
#'                                 beam.feat.pvals=test.feat.pvals,
#'                                 number.pairs=1, set.id="ENSG00000099810",
#'                                 feat.id=NULL, title.size=11,
#'                                 pair.order="omic", endpt.order=NULL)
gen_beam_plot_list <- function(beam.result, beam.specs, beam.feat.pvals, number.pairs=1,
                               set.id, feat.id=NULL, title.size=10,
                               pair.order="both", endpt.order=NULL) # Option to filter by feat.id instead of set.id
{
  beam.data <- beam.result$beam.data
  main.data <- beam.result$beam.data$main.data
  if(all(is.null(c(set.id, feat.id))))
    stop(paste0("You must specify at least one of set.id and feat.id!"))
  if(!is.null(set.id)&!is.null(feat.id)){
    # filter beam.specs to match set.id and feat.id
    set.data.filt <- beam.data$set.data[which(beam.data$set.data$set.id==set.id & beam.data$set.data$row.id==feat.id),,drop=FALSE]
    if(nrow(set.data.filt)==0)
      stop(paste0("No matches found, check that ", feat.id, " is in ", set.id, "."))
    beam.specs <- beam.specs[which(beam.specs$mtx %in% set.data.filt$mtx.id),,drop=FALSE]
  }

  # Figure out ordering
  n.spec <- nrow(beam.specs)
  filt.feat.p <- data.frame()
  for(j in 1:n.spec){
    feat.temp <- beam.feat.pvals[[paste(beam.specs$name[j])]]
    if(!is.null(set.id)&is.null(feat.id)){
      feat.temp.filt <- feat.temp[which(feat.temp$gene==set.id),,drop=FALSE]
    }
    else if(is.null(set.id)&!is.null(feat.id)){
      feat.temp.filt <- feat.temp[which(feat.temp$id==feat.id),,drop=FALSE]
    }
    else{
      feat.temp.filt <- feat.temp[which(feat.temp$gene==set.id & feat.temp$id==feat.id),,drop=FALSE]
    }
    feat.temp.filt$Assoc <- rep(paste0(beam.specs$name[j]), times=nrow(feat.temp.filt))
    name.split <- str_split_1(beam.specs$name[j], pattern="\\.")
    feat.temp.filt$omic <- rep(name.split[1], times=nrow(feat.temp.filt))
    feat.temp.filt$endpt <- rep(name.split[2], times=nrow(feat.temp.filt))
    filt.feat.p <- rbind.data.frame(filt.feat.p, feat.temp.filt)
  }
  # Option 1: pair.order="both" (default)
  if(pair.order=="both"|is.null(pair.order)){
    n.spec <- nrow(beam.specs)
    beam.plots <- vector("list")
    for(i in 1:n.spec){
      # Extract set.data
      mtx.name=beam.specs[i,"mtx"]        # extract name of omics matrix
      mtx=beam.data$mtx.data[[mtx.name]]     # extract matrix
      mtx.set <- beam.data$set.data[which(beam.data$set.data$mtx.id==mtx.name),,drop=FALSE] # extract sets for thsi mtx
      # Options of filtering, by set.id or feat.id or both
      if(!is.null(set.id)&is.null(feat.id)){
        mtx.set.filt <- mtx.set[which(mtx.set$set.id==set.id),,drop=FALSE]
      }
      else if(is.null(set.id)&!is.null(feat.id)){
        mtx.set.filt <- mtx.set[which(mtx.set$row.id==feat.id),,drop=FALSE]
      }
      else{
        mtx.set.filt <- mtx.set[which(mtx.set$set.id==set.id & mtx.set$row.id==feat.id),,drop=FALSE]

      }
      # extract rows that correspond to set.id
      #mdl=beam.specs.use[i,"mdl"]
      plt=beam.specs[i,"plot"]    # extract plot code
      x.clm=paste0(mtx.name,".clm") # column of main data to match
      x.index=main.data[,x.clm] # extract column for matching
      #print(x.index)
      mtx.X=mtx[,x.index] # omics matrix data aligned to main data
      mtx.X.sm <- mtx.X[which(rownames(mtx.X) %in% mtx.set.filt$row.id),,drop=FALSE] # extract desired set, for lesion rows correspond to different lesions (gain, loss, mutation)
      # Order row names by feature level q-value (then p-value, then -abs(beta))
      feat.temp <- beam.feat.pvals[[paste(beam.specs$name[i])]]
      feat.temp.filt <- feat.temp[which(feat.temp$id %in% mtx.set.filt$row.id),,drop=FALSE]
      feat.temp.or<- feat.temp.filt[order(feat.temp.filt$q, feat.temp.filt$p, -abs(feat.temp.filt$beta)),,drop=FALSE]
      mtx.X.sm.o <- mtx.X.sm[c(paste0(feat.temp.or$id)),,drop=FALSE]
      mtx.X.sm.top <- mtx.X.sm.o[which(rownames(mtx.X.sm.o) %in% feat.temp.or$id[1:number.pairs]),]
      n.subplt <- ifelse(is.matrix(mtx.X.sm.top), nrow(mtx.X.sm.top), 1)

      if(n.subplt==0){
        plot.temp <- NULL
      }
      if(n.subplt==1){
        if(grepl("ggsurv", plt)){
          plot.temp <- clin.plot.beam(main.data, mtx.X.sm.top, plt)
          plot.temp <- plot.temp$plot + ggplot2::ggtitle(paste(feat.temp.or$id[1], " P-Value = ", signif(feat.temp.or$p[1], digits=3)))+ ggplot2::theme(plot.title=element_text(size=title.size))
        }
        else{
          plot.temp <- clin.plot.beam(main.data, mtx.X.sm.top, plt) + ggplot2::ggtitle(paste(feat.temp.or$id[1], " P-Value = ", signif(feat.temp.or$p[1], digits=3))) + ggplot2::theme(plot.title=element_text(size=title.size))
        }
        beam.plots <- rlist::list.append(beam.plots, plot.temp)

      }
      if(n.subplt>1){
        for(j in 1:n.subplt){
          if(grepl("ggsurv", plt)){
            plot.temp <- clin.plot.beam(main.data, mtx.X.sm.top[j,], plt)
            plot.temp <- plot.temp$plot + ggplot2::ggtitle(paste(feat.temp.or$id[j], " P-Value = ", signif(feat.temp.or$p[j], digits=3)))+ ggplot2::theme(plot.title=element_text(size=title.size))
          }
          else{
            plot.temp <- clin.plot.beam(main.data, mtx.X.sm.top[j,], plt) + ggplot2::ggtitle(paste(feat.temp.or$id[j], " P-Value = ", signif(feat.temp.or$p[j], digits=3)))+ ggplot2::theme(plot.title=element_text(size=title.size))
          }

          beam.plots <- rlist::list.append(beam.plots, plot.temp)
        }
      }



      #print(boot.index)
    }
  }
  else if(pair.order=="omic"|pair.order=="endpoint"){
    if(pair.order=="omic"){
      omic.lev <- levels(as.factor(filt.feat.p$omic))
      plot.df <- data.frame()
      for(k in 1:length(omic.lev)){
        temp.lev <- omic.lev[k]
        filt.feat.p.temp <- filt.feat.p[which(filt.feat.p$omic==temp.lev),]
        filt.feat.p.temp.or <- filt.feat.p.temp[order(filt.feat.p.temp$q, filt.feat.p.temp$p,
                                                      -abs(filt.feat.p.temp$beta)),,drop=FALSE]
        top.ids <- filt.feat.p.temp.or[1:number.pairs,]$id
        to.plot <- filt.feat.p.temp[which(filt.feat.p.temp$id %in% top.ids),]
        plot.df <- rbind.data.frame(plot.df, to.plot)
      }
    }
    if(pair.order=="endpoint"){
      if(is.null(endpt.order))
        stop("Must specify endpt.order value if using pair.order=endpoint.")
      filt.feat.p.ep <- filt.feat.p[which(filt.feat.p$endpt==endpt.order),]
      omic.lev <- levels(as.factor(filt.feat.p.ep$omic))
      plot.df <- data.frame()
      for(k in 1:length(omic.lev)){
        temp.lev <- omic.lev[k]
        filt.feat.p.temp.full <- filt.feat.p[which(filt.feat.p$omic==temp.lev),]
        filt.feat.p.temp <- filt.feat.p.ep[which(filt.feat.p.ep$omic==temp.lev),]
        filt.feat.p.temp.or <- filt.feat.p.temp[order(filt.feat.p.temp$q, filt.feat.p.temp$p,
                                                      -abs(filt.feat.p.temp$beta)),,drop=FALSE]
        top.ids <- filt.feat.p.temp.or[1:number.pairs,]$id
        to.plot <- filt.feat.p.temp.full[which(filt.feat.p.temp.full$id %in% top.ids),]
        plot.df <- rbind.data.frame(plot.df, to.plot)
      }
    }

    # Extract plot list from plot.df object
    # first order plot.df by "Assoc" column
    plot.df.or <- plot.df[order(plot.df$Assoc),]

    n.spec <- nrow(plot.df.or)
    beam.plots <- vector("list")
    for(i in 1:n.spec){
      # Extract set.data
      temp.plot.df.or <- plot.df.or[i,]
      temp.assoc <- temp.plot.df.or$Assoc
      mtx.name=plot.df.or[i,"omic"]       # extract name of omics matrix
      mtx=beam.data$mtx.data[[mtx.name]]     # extract matrix
      mtx.set <- beam.data$set.data[which(beam.data$set.data$mtx.id==mtx.name),,drop=FALSE] # extract sets for thsi mtx
      # Filter to desired row
      mtx.set.filt <- mtx.set[which(mtx.set$set.id==temp.plot.df.or$gene & mtx.set$row.id==temp.plot.df.or$id),,drop=FALSE]

      # extract rows that correspond to set.id
      #mdl=beam.specs.use[i,"mdl"]
      plt=beam.specs[grep(temp.assoc, beam.specs$name),"plot"]    # extract plot code
      x.clm=paste0(mtx.name,".clm") # column of main data to match
      x.index=main.data[,x.clm] # extract column for matching
      #print(x.index)
      mtx.X=mtx[,x.index] # omics matrix data aligned to main data
      mtx.X.sm <- mtx.X[which(rownames(mtx.X) %in% mtx.set.filt$row.id),,drop=FALSE] # extract desired set, for lesion rows correspond to different lesions (gain, loss, mutation)
      # Order row names by feature level q-value (then p-value, then -abs(beta))
      feat.temp <- beam.feat.pvals[[paste(temp.assoc)]]
      feat.temp.filt <- feat.temp[which(feat.temp$id %in% mtx.set.filt$row.id),,drop=FALSE]
      feat.temp.or<- feat.temp.filt[order(feat.temp.filt$q, feat.temp.filt$p, -abs(feat.temp.filt$beta)),,drop=FALSE]
      mtx.X.sm.o <- mtx.X.sm[c(paste0(feat.temp.or$id)),,drop=FALSE]
      mtx.X.sm.top <- mtx.X.sm.o[which(rownames(mtx.X.sm.o) %in% feat.temp.or$id[1:number.pairs]),]
      n.subplt <- ifelse(is.matrix(mtx.X.sm.top), nrow(mtx.X.sm.top), 1)

      if(n.subplt==0){
        plot.temp <- NULL
      }
      if(n.subplt==1){
        if(grepl("ggsurv", plt)){
          plot.temp <- clin.plot.beam(main.data, mtx.X.sm.top, plt)
          plot.temp <- plot.temp$plot + ggplot2::ggtitle(paste(feat.temp.or$id[1], " P-Value = ", signif(feat.temp.or$p[1], digits=3)))+ ggplot2::theme(plot.title=element_text(size=title.size))
        }
        else{
          plot.temp <- clin.plot.beam(main.data, mtx.X.sm.top, plt) + ggplot2::ggtitle(paste(feat.temp.or$id[1], " P-Value = ", signif(feat.temp.or$p[1], digits=3))) + ggplot2::theme(plot.title=element_text(size=title.size))
        }
        beam.plots <- rlist::list.append(beam.plots, plot.temp)

      }
      if(n.subplt>1){
        for(j in 1:n.subplt){
          if(grepl("ggsurv", plt)){
            plot.temp <- clin.plot.beam(main.data, mtx.X.sm.top[j,], plt)
            plot.temp <- plot.temp$plot + ggplot2::ggtitle(paste(feat.temp.or$id[j], " P-Value = ", signif(feat.temp.or$p[j], digits=3)))+ ggplot2::theme(plot.title=element_text(size=title.size))
          }
          else{
            plot.temp <- clin.plot.beam(main.data, mtx.X.sm.top[j,], plt) + ggplot2::ggtitle(paste(feat.temp.or$id[j], " P-Value = ", signif(feat.temp.or$p[j], digits=3)))+ ggplot2::theme(plot.title=element_text(size=title.size))
          }

          beam.plots <- rlist::list.append(beam.plots, plot.temp)
        }
      }



      #print(boot.index)
    }
  }
  else{
    stop("pair.order must be specified.")
  }


  return(beam.plots)
}
#############################################
# Parse Plot Code
# mtx.X.vec <- mtx.X.sm.top
clin.plot.beam <- function(main.data, mtx.X.vec, plt){
  main.data$mtx.row <- mtx.X.vec
  main.data$mtx.row.fac <- as.factor(mtx.X.vec)
  try(eval(parse(text=plt)))
}
