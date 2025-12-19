# draw.heatmaps.R
#
# This function draws the list of heatmaps given to it in a grid on the R graphics area
# grid size has number of rows as the number of gene signatures given to the code, and
# number of columns is the number of datasets on which to test the signature
# @param hmaps A list of genes representing the gene signature to be tested.
# @param names_datasets A list of names of the datasets (for sizing purposes only)
# @param names_sigs A list of names of gene signatures input (for sizing purposes only)
# @keywords draw.heatmaps

draw.heatmaps <- function(hmaps,names_datasets,names_sigs){
  grid::grid.newpage()
  num_rows <- length(names_sigs)#ceiling(sqrt(length(names)))
  num_cols <- length(names_datasets)#ceiling(length(names)/num_rows)

  grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = num_rows, ncol=num_cols)))
  count <- 1
  for (i in 1:num_rows){
    for(j in 1:num_cols){
      grid::grid.draw(grid::editGrob(hmaps[[count]], vp=grid::viewport(layout.pos.row = i, layout.pos.col = j , clip=T)))
      count <- count +1
    }
  }
}
