scRNAtools_tsne <-
function(exam){
Rtsne::Rtsne
iris_unique <- unique(exam) # Remove duplicates
iris_matrix <- as.matrix(iris_unique[,1:(ncol(exam)-1)])
set.seed(42) # Set a seed if you want reproducible results
tsne_out <- Rtsne(iris_matrix) 
pdf(file=file.path(tempdir(), "t-SNE.pdf"))
plot(tsne_out$Y,col=iris_unique$Cell)
title ("t-SNE analysis")
dev.off()
}
