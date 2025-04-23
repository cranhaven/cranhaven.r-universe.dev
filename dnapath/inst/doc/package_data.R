## ----setup, message=FALSE, warning=FALSE, echo=FALSE--------------------------
reset_options_scipen <- getOption("scipen")
reset_options_digits <- getOption("digits")
options(scipen = 1, digits = 5)
library(dnapath)

## -----------------------------------------------------------------------------
pathway_list <- dnapath::get_reactome_pathways("human")

## -----------------------------------------------------------------------------
pathway_list[1:2]

## -----------------------------------------------------------------------------
# Obtain index of pathways containing "p53" or "pi3k" in their name.
index_pathways <- grep("((p53)|(pi3k))", names(pathway_list),
                       ignore.case = TRUE)

## -----------------------------------------------------------------------------
# Subset onto the "p53" and "pi3k" pathways.
cancer_pathways <- pathway_list[index_pathways]
cancer_pathways[1:3] # Print out the first three

## -----------------------------------------------------------------------------
# Make a vector of all entrezgene IDs that we are interested in.
genes_of_interest <- c(7157, 5290, 5291, 5293, 5294)
# Obtain index of pathways containing any genes of interest.
index_pathways <- which(sapply(pathway_list, function(pathway) {
  any(genes_of_interest %in% pathway)
}))

## -----------------------------------------------------------------------------
# Subset onto pathways containing p53 gene or pi3k family of genes.
cancer_pathways <- pathway_list[index_pathways]
cancer_pathways[1:3] # Print out the first three

## -----------------------------------------------------------------------------
# Obtain index of pathways containing "p53" in their name.
index_pathways <- grep("(p53)", names(pathway_list),ignore.case = TRUE)
p53_pathways <- pathway_list[index_pathways]

## ----message = FALSE----------------------------------------------------------
file_clinical <- paste0("http://linkedomics.org/data_download/TCGA-MESO/Human__",
                        "TCGA_MESO__MS__Clinical__Clinical__01_28_2016__BI__",
                        "Clinical__Firehose.tsi")
file_rnaseq <- paste0("http://linkedomics.org/data_download/TCGA-MESO/Human__",
                      "TCGA_MESO__UNC__RNAseq__HiSeq_RNA__01_28_2016__BI__Gene",
                      "__Firehose_RSEM_log2.cct.gz")

clinical <- readr::read_tsv(file_clinical)
rnaseq <- readr::read_tsv(file_rnaseq)

## -----------------------------------------------------------------------------
# First, we can glimpse at the raw data to see how it is structured.
clinical[1:5, 1:3]

## -----------------------------------------------------------------------------
# The columns contain samples, and rows contain different variables
# We want the transpose of this.
variable_names <- clinical$attrib_name # Store the variable names.

# Transpose the matrix so that columns correspond to variables.
clinical <- t(clinical[, -1]) # Don't include the "attrib_name" column as a row.
colnames(clinical) <- variable_names # Rename columns using variable names.

# Glimpse at the transposed dataset:
clinical[1:4, 1:4]

## -----------------------------------------------------------------------------
# Now that the rows correspond to subjects and columns to variables,
# the next step is to add a new column that contains the sample IDs. 

clinical <- cbind(ID = rownames(clinical), clinical)
rownames(clinical) <- NULL # Remove the row names.

# Glimpse at the data.
clinical[1:4, 1:4]

## -----------------------------------------------------------------------------
# For the example dataset, we are only interested in ID and tumor stage.
clinical <- clinical[, c("ID", "pathologic_stage")]
# The two groups that will be compared are stage 2 and stage 4.
# Subset rows onto those that have stage 2 or stage 4.
clinical <- clinical[clinical[, "pathologic_stage"] %in% c("stageii", "stageiv"), ]
clinical[1:5, ]

## -----------------------------------------------------------------------------
# We are left with only stage 2 and stage 4 samples:
table(clinical[, 2])

## -----------------------------------------------------------------------------
# First, we can glimps at the raw data to see how it is structured.
rnaseq[1:5, 1:5]

## -----------------------------------------------------------------------------
# As with the clinical data, we need to transpose the raw data so that
# samples are in the rows.
gene_symbols <- rnaseq$attrib_name # Store the gene names

rnaseq <- t(rnaseq[, -1])
colnames(rnaseq) <- gene_symbols

rnaseq[1:5, 1:5]

## -----------------------------------------------------------------------------
# Check if all 32 clinical samples contain gene expression data.
all(clinical[, "ID"] %in% rownames(rnaseq))

# Subset the rnaseq data onto those in the clinical data
rnaseq <- rnaseq[rownames(rnaseq) %in% clinical[, "ID"], ]

# Finally, we must make sure the two dataset are aligned.
# There are many ways to do this, here is one:
#   For each ID in the clinical dataset, find the corresponding row in rnaseq.
#   The rows are then reordered to match the IDs in clinical.
rnaseq <- rnaseq[sapply(clinical[, "ID"], 
                        function(ID) which(rownames(rnaseq) == ID)), ]

# Check that the IDs match:
all(rownames(rnaseq) == clinical[, "ID"])

rnaseq[1:5, 1:5]

## -----------------------------------------------------------------------------
# Convert gene symbols -> entrezgene IDs
gene_symbols <- colnames(rnaseq) # Extract the gene symbols to relabel.
gene_mat <- symbol_to_entrez(gene_symbols, "human",
                             dir_save = tempdir()) # Obtain mapping.
gene_mat[1:5, ]

## ----warning = FALSE----------------------------------------------------------
# The rename_genes() method is a multipurpose function that can be used to rename 
# the genes in a vector (the current case), a pathway list, or the 'dnapath_list'
# object that is returned from dnapath() after performing the differential
# network analysis.
gene_entrez <- rename_genes(gene_symbols, gene_mat) # Rename the symbols.
colnames(rnaseq) <- gene_entrez # Update the columns using the entrezgene IDs.
rnaseq[1:5, 1:5] # Columns now contain entrezgene IDs

## -----------------------------------------------------------------------------
# Subset the columns onto only those genes contained in 'p53_pathways'
index_genes <- which(colnames(rnaseq) %in% get_genes(p53_pathways))
rnaseq <- rnaseq[, index_genes]
dim(rnaseq) # 32 samples with 150 genes.

## -----------------------------------------------------------------------------
# Convert entrezgene IDs -> gene symbols
gene_entrez <- get_genes(p53_pathways) # Extract genes from pathway list.
gene_mat <- entrez_to_symbol(gene_entrez, "human", 
                             dir_save = tempdir()) # Obtain mapping.
gene_mat[1:5, ]

## -----------------------------------------------------------------------------
# Convert the entrezgene IDs into gene symbols in the pathway list.
new_pathway_list <- rename_genes(p53_pathways, gene_mat) 
new_pathway_list[1:2] # Print the first two pathways.

## -----------------------------------------------------------------------------
meso <- list(gene_expression = rnaseq,
             groups = clinical[, "pathologic_stage"])

## ----echo=FALSE---------------------------------------------------------------
options(scipen = reset_options_scipen, digits = reset_options_digits)

