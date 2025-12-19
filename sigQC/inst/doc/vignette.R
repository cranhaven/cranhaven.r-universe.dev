## -----------------------------------------------------------------------------
dataset_1 <- replicate(1000, rnorm(500,mean=0,sd=1))#random matrix - 1000 genes x 500 samples
row.names(dataset_1) <- as.character(1:(dim(dataset_1)[1])) #set the gene names
colnames(dataset_1) <- paste0('d1_',as.character(1:(dim(dataset_1)[2]))) #set the sample IDs

dataset_2 <- replicate(1000, rnorm(250,mean=0,sd=2))#random matrix - 1000 genes x 250 samples
row.names(dataset_2) <- as.character(1:(dim(dataset_2)[1])) #set the gene names
colnames(dataset_2) <- paste0('d2_',as.character(1:(dim(dataset_2)[2]))) #set the sample IDs

#gene sets that we are going to use for our gene signatures
gene_sig_1 <- c('1','2','3','5','8','13','21','34','55','89','144','233','377','610','987') 
gene_sig_2 <- c('2','4','8','16','32','64','128','256','512')

## -----------------------------------------------------------------------------
mRNA_expr_matrix = list()
mRNA_expr_matrix[["dataset_1"]] = dataset_1
mRNA_expr_matrix[["dataset_2"]] = dataset_2

gene_sigs_list = list()
gene_sigs_list[['gene_sig_1']] = as.matrix(gene_sig_1)
gene_sigs_list[['gene_sig_2']] = as.matrix(gene_sig_2)

## -----------------------------------------------------------------------------
showResults <- FALSE # we do not want to show the reuslts in R graphics windows
doNegativeControl <- FALSE # we do not want to compute the negative or permutation controls for time purposes

