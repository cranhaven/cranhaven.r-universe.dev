
#' Generate HTML file for scatter plots which all data points are highlighted by IPCAPS clusters
#'
#' @description Generate HTML file called 'tree_scatter_cluster.html' from the
#' result of \code{\link{ipcaps}}. This function is a part of workflow in
#' \code{\link{save.plots}}. The clustering result is shown as a tree rendering
#' by the online Google Organizational Chart library. Note that the Internet is
#' required to view the HTML file.
#'
#' @param output.dir A result directory as the \code{$output} object returned
#' from the \code{\link{ipcaps}} function.
#'
#' @return \code{NULL}
#'
#' @details After running, this function generates the file called 'tree_scatter_cluster.html'
#' in the same result directory. All plots are generated and saved as image files
#' in the sub-directory 'images'.
#'
#' @export
#'
#' @include output.template.R
#'
#' @seealso \code{\link{save.html}},
#' \code{\link{save.plots}},
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
#' #Here, to generate HTML file
#' save.plots.cluster.html(my.cluster$output.dir)

save.plots.cluster.html <- function(output.dir){

  tree <- NULL
  label <- NULL
  min.in.group <- NULL
  leaf.node <- NULL

  load(file.path(output.dir,"RData","leafnode.RData"))
  load(file.path(output.dir,"RData","tree.RData"))
  load(file.path(output.dir,"RData","condition.RData"))
  node.list = sort(tree$node)

  txt_data = ""
  txt_leafnode = ""

  test.dir=file.path(output.dir,"images.new")
  img.dir="images"
  if (file.exists(test.dir)){
    img.dir="images.new"
  }

  for (i in node.list){
    parent_node = ""
    if (i > 1){
      parent_node = tree$parent.node[which(tree$node == i)]
    }
    PCs.file = file.path(output.dir,"RData",paste0("node",i,".RData"))
    load(PCs.file)
    list.sum = c()
    u.label = sort(unique(label))
    for (j in 1:length(u.label)){
      co = length(label[label == u.label[j]])
      strout = paste0(u.label[j],"&nbsp;(",co,")")
      list.sum = c(list.sum,strout)
    }
    content = ""
    for (s in list.sum){
      content = paste0(content,s,"<br>")
    }
    if (file.exists(file.path(output.dir,"images",paste0("scatterplot_preview",i,".jpg")))){
      txt_data = paste0(txt_data,"[{v:'",i,"', f:'<div class=\"box_class\" align=\"center\"><p class=\"head_class\">Node ",i,"</p><a href=\"",img.dir,"/scatterplot",i,".png\" target=\"_blank\"><img src=\"",img.dir,"/scatterplot_preview",i,".jpg\" width=200 height=200><br />view</a></div>'}, '",parent_node,"', '']")
    }else if (file.exists(file.path(output.dir,"images",paste0("scatterplot_preview",i,".pdf")))){
      txt_data = paste0(txt_data,"[{v:'",i,"', f:'<div class=\"box_class\" align=\"center\"><p class=\"head_class\">Node ",i,"</p><a href=\"",img.dir,"/scatterplot",i,".pdf\" target=\"_blank\"><embed src=\"",img.dir,"/scatterplot_preview",i,".pdf\" width=200 height=200><br />view</a></div>'}, '",parent_node,"', '']")
    }else{
      txt_data = paste0(txt_data,"[{v:'",i,"', f:'<div class=\"box_class\"><p class=\"head_class\">Node ",i,"</p><p class=\"subhead_class\">under cutoff (<",min.in.group,")</p><br>",content,"</div>'}, '",parent_node,"', '']")
    }

    if (!(i == node.list[length(node.list)])){
      txt_data = paste0(txt_data,",\n")
    }else{
      txt_data = paste0(txt_data,"\n")
    }

    no_idx = which(i == node.list) - 1
    if (i %in% leaf.node){
      txt_leafnode = paste0(txt_leafnode,"data.setRowProperty(",no_idx,", 'style', 'border: 3px solid #DB6E6E; background-color:#FFE1E1');\n")
    }
  }

  txt_title = "Scatter plots colored by IPCAPS result"
  txt_body = "Scatter plots colored by IPCAPS result"

  txt_html = output.template$template
  txt_html[output.template$lno_data] = txt_data
  txt_html[output.template$lno_leafnode] = txt_leafnode
  txt_html[output.template$lno_body] = txt_body
  txt_html[output.template$lno_title] = txt_title

  fo = file(file.path(output.dir,"tree_scatter_cluster.html"),"w")
  for (i in txt_html){ write(i,fo)}
  close(fo)

  invisible(NULL)
}



