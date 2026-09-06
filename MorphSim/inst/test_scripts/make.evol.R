#'@param tree A phylogenetic tree.
#'@param bp  Basepair data for the change at each branch. Should be a character vector with length euqal to the number of nodes.
#'@param noroot TRUE means exlude root from the final product. FALSE means include the root. Default is set to TRUE.

make.evol<-function(t, char=NA, noroot=TRUE){

  #get ages (including node and species)
  ages<-dispRity::tree.age(t, order = "past", fossil = TRUE, digits = 3)

  #get only node ages
  node_ages<-ages[(length(t$tip.label)+1):length(ages$ages),]

  #remove label/info for root of the tree
  if(noroot){node_ages<-node_ages[node_ages$ages!=max(node_ages$ages),] }

  #data frame format
  data<-data.frame(sp = as.numeric(node_ages$elements), edge = as.numeric(node_ages$elements),
                   hmin = node_ages$ages, hmax = node_ages$ages, char_change = char)

  #change position of labels to further back (for future plotting)
  data$hmax<-data$hmax + max(ages$ages)/10
  data$hmin<-data$hmin + max(ages$ages)/10

  #change class to evol object class
  attr(data, "class") <- c("evol", class(data))

  return(data)
}
