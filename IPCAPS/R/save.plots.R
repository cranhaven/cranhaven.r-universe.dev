
#' Workflow to generate HTML files for all kinds of plots
#'
#' @description Generate HTML files and all image files (plots) from the result
#' of \code{\link{ipcaps}}. The clustering result is shown as a tree rendering
#' by the online Google Organizational Chart library. Note that the Internet is
#' required to view the HTML files.
#'
#' @param output.dir A result directory as the \code{$output} object returned
#' from the \code{\link{ipcaps}} function.
#'
#' @return \code{NULL}
#'
#' @details After running, this function generates all plots and saves as image files in the
#' sub-directory 'images'. It calls \code{\link{save.plots.cluster.html}},
#' \code{\link{save.eigenplots.html}}, and \code{\link{save.plots.label.html}}
#' to generate all HTML files.
#'
#' @export
#'
#' @import graphics
#' @import grDevices
#'
#' @include parallelization.R
#' @include save.plots.cluster.html.R
#' @include save.plots.label.html.R
#' @include save.eigenplots.html.R
#'
#' @seealso \code{\link{save.html}},
#' \code{\link{save.plots.cluster.html}},
#' \code{\link{save.eigenplots.html}},
#' and \code{\link{save.plots.label.html}}
#'
#' @examples
#'
#' # Importantly, bed file, bim file, and fam file are required
#' # Use the example files embedded in the package
#'
#' BED.file <- system.file("extdata","ipcaps_example.bed",package="IPCAPS")
#' LABEL.file <- system.file("extdata","ipcaps_example_individuals.txt.gz",package="IPCAPS")
#'
#' my.cluster <- ipcaps(bed=BED.file,label.file=LABEL.file,lab.col=2,out=tempdir())
#'
#' #Here, to generate all plots and HTML files
#' save.plots.label.html(my.cluster$output.dir)

save.plots <- function(output.dir){

  plot.as.pdf <- NULL
  tree <- NULL
  leaf.node <- NULL
  threshold <- NULL
  index <- NULL
  eigen.fit <- NULL
  PCs <- NULL
  eigen.value <- NULL

  load(file.path(output.dir,"RData","leafnode.RData"))
  load(file.path(output.dir,"RData","tree.RData"))
  load(file.path(output.dir,"RData","rawdata.RData"))
  load(file.path(output.dir,"RData","condition.RData"))
  global.label = label
  node.list = sort(tree$node)

  map_color = c("red",rgb(0,68,27,maxColorValue=255),"blue",rgb(231,41,138,maxColorValue=255),"darkorange","black")
  map_color = c(map_color,rgb(102,37,6,maxColorValue=255),rgb(63,0,125,maxColorValue=255),"green")
  map_color = c(map_color,"cyan",rgb(250,159,181,maxColorValue=255),"yellow","darkgrey")
  map_color = c(map_color,rgb(116,196,118,maxColorValue=255))

  map_pch = c(1,0,2:18,35:38,60:64,94,126)
  map_pch = c(map_pch,33:34,42,45,47,40,91,123,41,92,93,125)
  map_pch = c(map_pch,49:57,97:107,109:110,112:119,121:122)
  map_pch = c(map_pch,65:78,81:82,84:85,89)

  if (length(leaf.node)>(length(map_color)*length(map_pch))){
    load(file.path(output.dir,"RData","condition.RData"))
    cat("In function: save.plots()\n")
    cat("Can't create the scatter plots due to the number of groups gernerated by clustering (",length(leaf.node),") is more than the maximum number of patterns (",(length(map_color)*length(map_pch)),")\n")
    cat("Please increase 'threshold' to reduce the number of clustering groups. The current 'threshold' is",threshold,"\n")
    return (cat(""))
  }

  all.uniq.label = c()
  color.by.label=TRUE
  #load(file.path(output.dir,"RData","node1.RData"))
  label = global.label
  all.uniq.label = sort(unique(label))
  if (length(all.uniq.label)>(length(map_color)*length(map_pch))){
    color.by.label = FALSE
    cat("In function: save.plots()\n")
    cat("Can't create the scatter plots colored by labels due to the number of uniq labels (",length(all.uniq.label),") is more than the maximum number of patterns (",(length(map_color)*length(map_pch)),")\n")
    cat("These plots will not be created!\n")
  }


  map_pattern = c()
  for (i in 1:length(map_pch))
    for (j in 1:length(map_color)){
      tmp = c(i,j)
      map_pattern = c(map_pattern,list(tmp))
    }

  test.dir=file.path(output.dir,"images.new")
  img.dir="images"
  if (file.exists(test.dir)){
    img.dir="images.new"
  }

  for (i in 1:length(node.list)){
    node = node.list[i]
    load(file.path(output.dir,"RData",paste0("node",node,".RData")))
    label = global.label[index]

    if (is.na(eigen.fit)){
      next
    }

    ori.PCs = PCs
    ori.eigen.fit = eigen.fit
    ori.eigen.value = eigen.value
    ori.index = index
    ori.label = label

    sub_leafnode = c()
    j = node.list[i]
    if (j %in% leaf.node){
      sub_leafnode = c(j)
    }else{
      queue_node = c(tree$node[which(tree$parent.node == j)])
      while (j <= max(leaf.node)){
        if (length(queue_node) > 0){
          j = queue_node[1]
          if (j %in% leaf.node){
            sub_leafnode = c(sub_leafnode,j)
          }else{
            queue_node = c(queue_node,tree$node[which(tree$parent.node == j)])
          }
          queue_node = queue_node[-which(queue_node==j)]
        }else{
          break
        }
      }
    }


    #For preview, generate scatter plots colored by clustering results
    if (plot.as.pdf == FALSE){
      file.name = file.path(output.dir,img.dir,paste0("scatterplot_preview",node,".jpg"))
      jpeg(file.name,width=200,height=200)
    }else{
      file.name = file.path(output.dir,img.dir,paste0("scatterplot_preview",node,".pdf"))
      pdf(file.name)
    }
    par(mar=c(0, 0, 0, 0), xpd=TRUE)
    #title.txt = paste0("Node ",node)
    plot(c(min(ori.PCs[,1]),max(ori.PCs[,1])),c(min(ori.PCs[,2]),max(ori.PCs[,2])),type="n",xlab="",ylab="",axes=FALSE)
    set_legend = NULL
    set_pch = NULL
    set_col = NULL
    for (k in 1:length(sub_leafnode)){
      load(file.path(output.dir,"RData",paste0("node",sub_leafnode[k],".RData")))
      pattern_index = which(leaf.node %in% sub_leafnode[k])
      spch = map_pch[map_pattern[[pattern_index]][1]]
      scolor = map_color[map_pattern[[pattern_index]][2]]
      points(ori.PCs[ori.index %in% index,1],ori.PCs[ori.index %in% index,2],col=scolor,pch=spch)
      set_legend = c(set_legend,paste0("group",which(leaf.node == sub_leafnode[k])))
      set_pch = c(set_pch,spch)
      set_col = c(set_col, scolor)
    }
    #legend('topright', inset=c(-0.3,0), legend=set_legend, pch=set_pch, col=set_col)
    dev.off()

    #Full plot, generate scatter plots colored by clustering results
    if (plot.as.pdf == FALSE){
      file.name = file.path(output.dir,img.dir,paste0("scatterplot",node,".png"))
      png(file.name,width=800,height=800)
    }else{
      file.name = file.path(output.dir,img.dir,paste0("scatterplot",node,".pdf"))
      pdf(file.name)
    }
    par(mfrow=c(2,2))
    title.txt = paste0("Node ",node)
    #Top-Left
    par(mar=c(4, 1, 1, 2))
    plot(c(min(ori.PCs[,1]),max(ori.PCs[,1])),c(min(ori.PCs[,2]),max(ori.PCs[,2])),type="n",xlab="",ylab="",main="",axes=FALSE)
    axis(side=1, labels=TRUE, line=2)
    axis(side=4, labels=TRUE, line=2)
    mtext("PC1", side=1, line=0.5)
    mtext("PC2", side=4, line=0.5)
    set_legend = NULL
    set_pch = NULL
    set_col = NULL
    for (k in 1:length(sub_leafnode)){
      load(file.path(output.dir,"RData",paste0("node",sub_leafnode[k],".RData")))
      pattern_index = which(leaf.node %in% sub_leafnode[k])
      spch = map_pch[map_pattern[[pattern_index]][1]]
      scolor = map_color[map_pattern[[pattern_index]][2]]
      points(ori.PCs[ori.index %in% index,1],ori.PCs[ori.index %in% index,2],col=scolor,pch=spch)
      set_legend = c(set_legend,paste0("group",which(leaf.node == sub_leafnode[k])))
      set_pch = c(set_pch,spch)
      set_col = c(set_col, scolor)
    }

    #Top-Right
    par(mar=c(4, 3.5, 1, 1))
    plot(c(min(ori.PCs[,3]),max(ori.PCs[,3])),c(min(ori.PCs[,2]),max(ori.PCs[,2])),type="n",xlab="",ylab="",main="",axes=FALSE)
    axis(side=1, labels=TRUE, line=2)
    mtext("PC3", side=1, line=0.5)
    for (k in 1:length(sub_leafnode)){
      load(file.path(output.dir,"RData",paste0("node",sub_leafnode[k],".RData")))
      pattern_index = which(leaf.node %in% sub_leafnode[k])
      spch = map_pch[map_pattern[[pattern_index]][1]]
      scolor = map_color[map_pattern[[pattern_index]][2]]
      points(ori.PCs[ori.index %in% index,3],ori.PCs[ori.index %in% index,2],col=scolor,pch=spch)
    }
    #Bottom-Left
    par(mar=c(1, 1, 0.5, 2))
    plot(c(min(ori.PCs[,1]),max(ori.PCs[,1])),c(min(ori.PCs[,3]),max(ori.PCs[,3])),type="n",xlab="",ylab="",main="",axes=FALSE)
    axis(side=4, labels=TRUE, line=2)
    mtext("PC3", side=4, line=0.5)
    for (k in 1:length(sub_leafnode)){
      load(file.path(output.dir,"RData",paste0("node",sub_leafnode[k],".RData")))
      pattern_index = which(leaf.node %in% sub_leafnode[k])
      spch = map_pch[map_pattern[[pattern_index]][1]]
      scolor = map_color[map_pattern[[pattern_index]][2]]
      points(ori.PCs[ori.index %in% index,1],ori.PCs[ori.index %in% index,3],col=scolor,pch=spch)
    }
    #Bottom-Right
    plot(1, type = "n", axes=FALSE, xlab="", ylab="")
    #legend('center', inset=0, legend=set_legend, pch=set_pch, col=set_col, ncol=4)
    if (length(set_legend)>54){
      legend('center', inset=0, legend=set_legend, pch=set_pch, col=set_col, ncol=4)
    }else if (length(set_legend)>36){
      legend('center', inset=0, legend=set_legend, pch=set_pch, col=set_col, ncol=3)
    }else if (length(set_legend)>18){
      legend('center', inset=0, legend=set_legend, pch=set_pch, col=set_col, ncol=2)
    }else{
      legend('center', inset=0, legend=set_legend, pch=set_pch, col=set_col, ncol=1)
    }
    dev.off()


    #For preview, generate scatter plots colored by labels
    if (color.by.label == TRUE){
      u.label = sort(unique(ori.label))
      if (plot.as.pdf == FALSE){
        file.name = file.path(output.dir,img.dir,paste0("scatter_by_label_preview",node,".jpg"))
        jpeg(file.name,width=200,height=200)
      }else{
        file.name = file.path(output.dir,img.dir,paste0("scatter_by_label_preview",node,".pdf"))
        pdf(file.name)
      }
      par(mar=c(0, 0, 0, 0), xpd=TRUE)
      #title.txt = paste0("Node ",node)
      plot(c(min(ori.PCs[,1]),max(ori.PCs[,1])),c(min(ori.PCs[,2]),max(ori.PCs[,2])),type="n",xlab="",ylab="",axes=FALSE)
      set_legend = NULL
      set_pch = NULL
      set_col = NULL
      for (k in 1:length(u.label)){
        pattern_index = which(all.uniq.label %in% u.label[k])
        spch = map_pch[map_pattern[[pattern_index]][1]]
        scolor = map_color[map_pattern[[pattern_index]][2]]
        points(ori.PCs[ori.label %in% u.label[k],1],ori.PCs[ori.label %in% u.label[k],2],col=scolor,pch=spch)
        set_pch = c(set_pch,spch)
        set_col = c(set_col, scolor)
      }

      #legend('topright', inset=c(-0.3,0),  legend=u.label, pch=set_pch, col=set_col)
      dev.off()

    }


    #Full plot, generate scatter plots colored by labels
    if (color.by.label == TRUE){
      u.label = sort(unique(ori.label))
      if (plot.as.pdf == FALSE){
        file.name = file.path(output.dir,img.dir,paste0("scatter_by_label",node,".png"))
        png(file.name,width=800,height=800)
      }else{
        file.name = file.path(output.dir,img.dir,paste0("scatter_by_label",node,".pdf"))
        pdf(file.name)
      }
      par(mfrow=c(2,2))
      title.txt = paste0("Node ",node)
      #Top-Left
      par(mar=c(4, 1, 1, 2))
      plot(c(min(ori.PCs[,1]),max(ori.PCs[,1])),c(min(ori.PCs[,2]),max(ori.PCs[,2])),type="n",xlab="",ylab="",main="",axes=FALSE)
      axis(side=1, labels=TRUE, line=2)
      axis(side=4, labels=TRUE, line=2)
      mtext("PC1", side=1, line=0.5)
      mtext("PC2", side=4, line=0.5)
      set_legend = NULL
      set_pch = NULL
      set_col = NULL
      for (k in 1:length(u.label)){
        pattern_index = which(all.uniq.label %in% u.label[k])
        spch = map_pch[map_pattern[[pattern_index]][1]]
        scolor = map_color[map_pattern[[pattern_index]][2]]
        points(ori.PCs[ori.label %in% u.label[k],1],ori.PCs[ori.label %in% u.label[k],2],col=scolor,pch=spch)
        set_pch = c(set_pch,spch)
        set_col = c(set_col, scolor)
      }

      #Top-Right
      par(mar=c(4, 3.5, 1, 1))
      plot(c(min(ori.PCs[,3]),max(ori.PCs[,3])),c(min(ori.PCs[,2]),max(ori.PCs[,2])),type="n",xlab="",ylab="",main="",axes=FALSE)
      axis(side=1, labels=TRUE, line=2)
      mtext("PC3", side=1, line=0.5)
      for (k in 1:length(u.label)){
        pattern_index = which(all.uniq.label %in% u.label[k])
        spch = map_pch[map_pattern[[pattern_index]][1]]
        scolor = map_color[map_pattern[[pattern_index]][2]]
        points(ori.PCs[ori.label %in% u.label[k],3],ori.PCs[ori.label %in% u.label[k],2],col=scolor,pch=spch)
      }

      #Bottom-Left
      par(mar=c(1, 1, 0.5, 2))
      plot(c(min(ori.PCs[,1]),max(ori.PCs[,1])),c(min(ori.PCs[,3]),max(ori.PCs[,3])),type="n",xlab="",ylab="",main="",axes=FALSE)
      axis(side=4, labels=TRUE, line=2)
      mtext("PC3", side=4, line=0.5)
      for (k in 1:length(u.label)){
        pattern_index = which(all.uniq.label %in% u.label[k])
        spch = map_pch[map_pattern[[pattern_index]][1]]
        scolor = map_color[map_pattern[[pattern_index]][2]]
        points(ori.PCs[ori.label %in% u.label[k],1],ori.PCs[ori.label %in% u.label[k],3],col=scolor,pch=spch)
      }

      #Bottom-Right
      plot(1, type = "n", axes=FALSE, xlab="", ylab="")
      #legend('center', inset=0, legend=u.label, pch=set_pch, col=set_col, ncol=4)
      if (length(u.label)>54){
        legend('center', inset=0, legend=u.label, pch=set_pch, col=set_col, ncol=4)
      }else if (length(u.label)>36){
        legend('center', inset=0, legend=u.label, pch=set_pch, col=set_col, ncol=3)
      }else if (length(u.label)>18){
        legend('center', inset=0, legend=u.label, pch=set_pch, col=set_col, ncol=2)
      }else{
        legend('center', inset=0, legend=u.label, pch=set_pch, col=set_col, ncol=1)
      }
      dev.off()
    }

    #For preview, generate Scree plots
    if (plot.as.pdf == FALSE){
      file.name = file.path(output.dir,img.dir,paste0("eigenvalue_preview",node,".jpg"))
      jpeg(file.name,width=200,height=200)
    }else{
      file.name = file.path(output.dir,img.dir,paste0("eigenvalue_preview",node,".pdf"))
      pdf(file.name)
    }
    ln.evalue=log(ori.eigen.value)
    ln.evalue[which(ln.evalue<0)] = 0
    par(mar=c(0, 0, 0, 0), xpd=TRUE)
    #title.txt = paste0("Node ",node," (EigenFit= ",sprintf("%.2f",ori.eigen.fit),")")
    plot(ln.evalue,type="n",xlab="",ylab="",axes=FALSE)
    abline(h=0.0)
    points(ln.evalue[which(ln.evalue>0)],col="black",pch="o",type="o")
    points(ln.evalue[which(diff.eigen.fit(ln.evalue)>threshold)],col="red",pch="o")
    dev.off()

    #Full plot, generate Scree plots
    if (plot.as.pdf == FALSE){
      file.name = file.path(output.dir,img.dir,paste0("eigenvalue",node,".png"))
      png(file.name,width=800,height=800)
    }else{
      file.name = file.path(output.dir,img.dir,paste0("eigenvalue",node,".pdf"))
      pdf(file.name)
    }
    ln.evalue=log(ori.eigen.value)
    ln.evalue[which(ln.evalue<0)] = 0
    title.txt = paste0("Node ",node," (EigenFit= ",sprintf("%.2f",ori.eigen.fit),")")
    plot(ln.evalue,type="n",ylab="ln(Eigen Value)",xlab="Component Number", main=title.txt)
    abline(h=0.0)
    points(ln.evalue[which(ln.evalue>0)],col="black",pch="o",type="o")
    points(ln.evalue[which(diff.eigen.fit(ln.evalue)>threshold)],col="red",pch="o")
    dev.off()
  }
  save.plots.cluster.html(output.dir)
  save.eigenplots.html(output.dir)
  if (color.by.label == TRUE){
    save.plots.label.html(output.dir)
  }

  invisible(NULL)
}



