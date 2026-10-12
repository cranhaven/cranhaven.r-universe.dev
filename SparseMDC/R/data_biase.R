#' Biase Data
#'
#' This dataset was created by Biase et al. to study cell fat inclination in
#' mouse embryos. It contains FPKM gene expression measurements for 49 cells
#' and 16,514 genes. There are three cell types in the dataset, zygote,
#' two-cell embryo, and four-cell embryo cells.
#'
#' @format An R.Data object storing FPKM gene expression
#' measurements for each of the samples.
#'
"data_biase"

#' Biase Data Conditions
#'
#' The condition for each sample in the Biase data. To be used when splitting
#' the data to demonstrate SparseDC.
#'
#'@format An R.Data object containing a vector with the conditon of the 49 cells
#'in the Biase data.
"condition_biase"

#' Biase Data Cell Type
#'
#' The cell type of each of the cells in the Biase data.
#'
#' @format An R.Data object containing a vector with the cell type of each of the
#' cells in the Biase Data.
#'
"cell_type_biase"
