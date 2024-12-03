# R/zzz.R

# Suppress warnings for global variables
utils::globalVariables(c(
  "foldChange", "uniqueSampleIDInternal", "mappedTerm", "geneSymbol", 
  "expressionValue", "totalExpression", "cell_id", "category_id",
  "gene",
  "sample_id",
  "mappedSymbol",
  "mappedId",
  "genes",
  "median_expression",
  "total_genes_in_pathway",
  "genes_in_sample_pathway",
  "PathwayGeneScore"
))

