#' This function plots your morpho object to a character matrix
#' @param x an object of class "morpho"
#' @param col a string bound by c() containing colors to match the amount of possible character states
#' @export 

plot.morpho.grid <- function(x, xlab = "", ylab = "",name = "" ,  col = c("white", "gray", "lightblue", "pink", "yellow", "green", "orange")){
  #tip/taxon labels
  tips<-x[[2]][[2]]
  
  # make empty container
  char_matrix<-matrix(nrow= length(tips),ncol = length(x[[1]][[1]]))
  rownames(char_matrix)<-tips
  
  # add character information to the data frame
  
  for (i in 1:length(x[[1]])){
    
    #traits for a given taxon
    
    traits<-x[[1]][[i]]
    
    #make sure the values are numeric
    
    char_matrix[i,]<-as.numeric(traits)
  }
  
  #character states: Question, do we want to use 0 and 1 always? Having issues coding
  #it to be whatever the character states are
  #charas<-as.numeric(unique(x[[1]][[1]]))
  
  #Plotting
  
  dat <- reshape2::melt(char_matrix)
  
  unique_states <- sort(unique(dat$value))
  color_mapping <- setNames(col[1:length(unique_states)], unique_states)
  
  #Question: What should we name the axes? And legend?
  ggplot2::ggplot(dat, aes(Var2, Var1, fill=factor(value))) +
    ggplot2::geom_tile(color="black") +
    ggplot2::geom_text(aes(label = value), color = "black", size = 4) +
    ggplot2::scale_fill_manual(values = color_mapping, name = name) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = xlab, y = ylab) +  # add it to function parameters :TB
    ggplot2::theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 10)
    )
}
