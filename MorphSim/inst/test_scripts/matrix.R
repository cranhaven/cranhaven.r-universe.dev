N<-10
b<-0.1
d<-0.05
L<-100
u<-0.5
c<-1

simulate_treeseq <- function(N, b, d, L, u, c) {
  phy <- ape::rphylo(N, b, d, fossils = T)
  data <- phangorn::simSeq(
    phy,
    l = L,
    type = "USER",
    levels = as.character(0:(c-1)),
    rate = u,
    rootseq = rep("0", L),
    ancestral = TRUE
  )
  list(phy = phy, data = data, N = N, L = L, c = c, b = b, d = d)
}

simulate_treeseq(10, 2, 2, 3, 4, 1)


plot_treeseq <- function(.l) {
  phy <- .l$phy
  data <- as.character(.l$data)
  N <- .l$N
  L <- .l$L
  c <- .l$c
  palet <- c("#0088FF","#FF8800","#FFFF88","#88FF88","#8844FF","#FF4488","#DCDC00","#00DD88","#88DDFF","#DD7799","#CDCD99","#77DC11","#4477AA","#AA4477","#EEEE77","#43CD93","#0F0F4F","#400040","#000000","#FFFFFF")
  coolers <- palet[1:c]
  x1 <- as.data.frame(phy$edge)
  colnames(x1) <- c("parent", "node")
  x1$parent <- as.character(x1$parent)
  x1$node <- ifelse(x1$node <= N, paste0("t", x1$node), as.character(x1$node))
  x2 <- as.data.frame(data)
  x2 <- tibble::rownames_to_column(x2, "node")
  x2 <- dplyr::mutate_if(x2, is.factor, as.character)
  x2 <- tidyr::pivot_longer(x2, cols = -node, names_to = "position")
  x3 <- dplyr::left_join(dplyr::select(x1, node = parent), x2, by = "node")
  x3 <- dplyr::rename(x3, parent = node, parent_value = value)
  x4 <- dplyr::full_join(x2, x1, by = "node")
  x4 <- dplyr::full_join(x4, x3, by = c("parent", "position"))
  x4 <- dplyr::distinct(x4)
  x4 <- dplyr::filter(x4, !is.na(parent_value))
  x4.1 <- dplyr::mutate(x4,
                        label = node,
                        node = as.integer(stringr::str_remove(node, "^t")),
                        is_mutated = value != parent_value,
                        mutation_position = ifelse(is_mutated, stringr::str_remove(position, "^V"), NA)
  )
  x4.1 <- dplyr::filter(x4.1, !is.na(mutation_position))
  x4.1 <- dplyr::select(x4.1, parent, node, is_mutated, mutation_position)
  x4.1 <- dplyr::group_by(x4.1, parent, node)
  x4.1 <- dplyr::summarize(x4.1, edge_label_m = stringr::str_c(mutation_position, collapse = ","))
  #  x4.2 <-  x4 <- dplyr::mutate(x4,
  #                               label = node,
  #                               node = as.integer(stringr::str_remove(node, "^t")),
  #                               is_returned = is.mutated != 1,
  #                               returned_position = ifelse(is_returned, stringr::str_remove(position, "^V"), NA)
  #  )
  #  x4.2 <- dplyr::filter(x4.2, !is.na(returned_position))
  #  x4.2 <- dplyr::select(x4.2, parent, node, is_returned, returned_position)
  #  x4.2 <- dplyr::group_by(x4.2, parent, node)
  #  x4.2 <- dplyr::summarize(x4.2, edge_label_r = stringr::str_c(mutation_position, collapse = ","))
  p <- ggtree(phy, ladderize = FALSE, size = 2) +
    geom_rootedge() +
    ggtitle("Species tree")
  p <- p %<+% x4.1 +
    geom_label(aes(x = branch, label = edge_label_m), fill = "#FF00BB") +
    scale_y_continuous(limits = c(0.5, N + 0.5)) +
    theme(legend.position = "none", text = element_text(size = 18))
  #  p <- p %<+% x4.2 +
  #    geom_label(aes(x = branch, label = edge_label_r), fill = "steelblue") +
  #    scale_y_continuous(limits = c(0.5, N + 0.5)) +
  #    theme(legend.position = "none", text = element_text(size = 18))
  x5 <- dplyr::mutate(x2,
                      x = as.numeric(stringr::str_remove(position, "^V")),
                      y = as.numeric(stringr::str_remove(node, "^t")),
                      text = ifelse(value %in% 0:3, x, NA)  # <- can use this below but cleaner without
  )
  x5 <- dplyr::filter(x5, node %in% phy$tip.label)
  q <- ggplot(x5, aes(x, y, fill = value, label = "")) +
    geom_tile(width = 1, height = 1, color = "black") +
    geom_text() +
    scale_fill_manual(values = coolers) + # Add colors for levels 0 to 3
    scale_y_continuous(limits = c(0.5, N + 0.5)) +
    ggtitle("Character alignment") +
    theme_void() +
    theme(legend.position = "none")
  r_data <- data.frame(x = 1, y = 1:length(phy$tip.label), az = phy$tip.label)
  r <- ggplot(r_data, aes(x, y, label = az)) +
    scale_y_continuous() + #limits = c(0.5, length(phy$tip.label) + 0.5)
    geom_text() +
    theme_void()
  plot_grid(p, r, q, ncol = 3, rel_widths = c(1, 0.1, 1), align = "hv")
}


simulated_morpho <- sim.morpho(phy, k = 2, trait.num = 20)
sim.morpho.process(phy, k = 2, trait.num = 20)


plot(0,type='n', axes=FALSE, ann=FALSE)
grid(nx=5, ny=5, lty=2)


set.seed(123)
phy <- ape::rtree(10)
plot(phy)
simulated_morpho <- sim.morpho(phy, k = 2, trait.num = 20)


morph<-sim.morpho.process(tree=phy, trait.num = 8)

x<-simulated_morpho

#'@param x morpho object
#'@param col1 Color
#'@param col2

plot.morpho.grid <- function(x, col1 = "white", col2 ="gray"){

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

    char_matrix[i,]<-as.numeric(traits)}

  #character states: Question, do we want to use 0 and 1 always? Having issues coding
  #it to be whatever the character states are
  #charas<-as.numeric(unique(x[[1]][[1]]))

#Plotting

  dat <- reshape2::melt(char_matrix)

  #Question: What should we name the axes? And legend?
  ggplot2::ggplot(dat, aes(Var2, Var1, fill=factor(value))) +
    ggplot2::geom_tile(color="black") +
    ggplot2::scale_fill_manual(values = c("1"=col1, "0"=col2), name="Character") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x="", y="Species") +
    ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1))

}
#Part 1: Store the information in a dataframe
tips<-simulated_morpho[[2]][[2]]
traits<-simulated_morpho[[1]]
char_states<-simulated_morpho[[1]][i]
char_matrix<-matrix(nrow= length(tips),ncol = length(simulated_morpho[[1]][[1]]))
rownames(char_matrix)<-tips

for (i in 1:length(simulated_morpho[[1]])){
    traits<-as.numeric(simulated_morpho[[1]][[i]])
char_matrix[i,]<-as.numeric(traits)
}

#Part 2: Plot

library(ggplot2)
library(reshape2)

# Reshape matrix into a data frame
matrix_data_df <- melt(char_matrix)

# Create the plot
ggplot(matrix_data_df, aes(Var2, Var1, fill=factor(value))) +
  geom_tile(color="black") +
  scale_fill_manual(values = c("0" ="lightblue", "1"="gray"), name="Value") +
  theme_minimal() +
  labs(x="Traits", y="Taxon") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



###########plotting without ggplot#################
set.seed(1234)
phy <- ape::rtree(10)
plot(phy)
simulated_morpho <- sim.morpho(phy, k = 2, trait.num = 10)
simulated_morpho
x<-simulated_morpho


plot.morpho.grid(x, trait.labels=FALSE, tip.labels=TRUE){
  #tip/taxon label

  tips<-x[[2]][[2]]

  # make empty container
  char_matrix<-matrix(nrow= length(tips),ncol = length(x[[1]][[1]]))
  rownames(char_matrix)<-tips

  # add character information to the data frame

  for (i in 1:length(x[[1]])){

    #traits for a given taxon

    traits<-x[[1]][[i]]

    #make sure the values are numeric

    char_matrix[i,]<-as.numeric(traits)}

  matrix_data<-char_matrix

  colors <- c("white", "blue") # White for 0, Blue for 1

  # Create the grid
  #plot(NULL)
  image(t(matrix_data[nrow(matrix_data):1, ]),
        col = colors,
        axes = FALSE)

  # Add gridlines
  grid(nx = ncol(matrix_data), ny = nrow(matrix_data), col = "black", lty = 1)



  #add axis labels
  if(tip.label){
    #rows (tip labels)
  tip_labs <- tips

  axis(2, at=seq(0,1, length=length(tip_labs)),
       labels=tip_labs, lwd=0, pos=0 cex.axis=1, las=1)

  #columns (traits)
  if(trait.labels){
    morph_labels<- seq(1,length(x$sequences[[1]]))
    axis(3, at=seq(0,1, length=length(morph_labels)),
         labels=morph_labels, lwd=0, pos=1, cex.axis=0.75)
  }
  }
}

rect(xleft = -0.036,
     ybottom = -0.056,
     xright = 1.036,
     ytop = 1.056,
     border = "black",
     lwd = 2) # Adjust `lwd` for line thickness





plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')# Add text labels for each cell
num_rows <- nrow(matrix_data)
num_cols <- ncol(matrix_data)

# Loop through rows and columns to add text
for (i in 1:num_rows) {
  for (j in 1:num_cols) {
    # Compute coordinates for text (reversed rows)
    x <- j - 0.5
    y <- num_rows - i + 0.5
    # Add the text (value of the matrix)
    text(x, y, labels = matrix_data[i, j], col = "black", cex = 0.8)
  }
}

# Add row labels beside the grid
for (i in 1:num_rows) {
  # Compute y-coordinate for each label
  y <- num_rows - i + 0.5
  # Add the row label to the left of the grid
  text(-0.5, y, labels = row_labels[i], col = "black", cex = 0.8, adj = 1)
}
