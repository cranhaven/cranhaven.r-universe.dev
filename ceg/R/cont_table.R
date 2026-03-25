
#' ContingencyTable
#'
#' This function creates the contigency tables associated with each variable in the event tree.
#' 
#' @param data data.frame whose columns depict variables and rows correspond
#'             to units that are observed in the system
#' @param stratified.event.tree Stratified.event.tree S4 object
#'
#' @return a list of matrices that represent the contigency tables associated with each variable in the event tree. 
#'         The matrix corresponding to a particular variable presents the counts of each combination of the categories  
#'	     of the variables that precede it in the event tree according to its categories. The combinations of the categories
#'	     of the upstream variables are displayed on the rows and represent the situations associated with the target variable.
#'	     The categories of the target variable are represented on the columns and corresponds to each event that can unfold 
#'	     from a situation associated with the target variable.
#'
ContingencyTable <- function(data, stratified.event.tree) {
  contingency.table <-
    lapply(1:(stratified.event.tree@num.variable), function(x)
      ContingencyTableVariable(x, data, stratified.event.tree))
  return(contingency.table)
}

#' ContingencyTableVariable
#'
#' This function calculates the contigency table associated with a specific variable.
#' 
#' @param variable numeric
#' @param data data.frame whose columns depict variables and rows correspond
#'             to units that are observed in the system
#' @param stratified.event.tree  Stratified.event.tree S4 object
#'
#' @return  a matrix that presents the counts of each combination of the categories  
#'	      of the variables that precede the target variable in the event tree according  
#'          to the categories of the target variable. The combinations of the categories
#'	      of the upstream variables are displayed on the rows and represents a situation associated with the target variable.
#'	      The categories of the target variable are represented on the columns and corresponds to each event that can unfold 
#'	      from a situation associated with the target variable.
#'
ContingencyTableVariable <- function(variable, data, stratified.event.tree) {
  contingency.table.var <- stats::ftable(data[, 1:variable])
  contingency.table.var <-
    sapply(seq_len(stratified.event.tree@num.situation[variable]), function(x)
      contingency.table.var[x,])
  # contingency_table_var<-lapply(seq_len(tree@num_situation[variable]),function(x)
  #   contingency_table_var[x,]) To return a list.
  return(t(contingency.table.var))
}
