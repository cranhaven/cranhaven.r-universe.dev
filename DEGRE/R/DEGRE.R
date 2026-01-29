#' Infer the Differentially Expressed Genes
#'
#' The DEGRE function acts inferring the differentially expressed genes fitting them using generalized linear mixed models and then applies the Wald test to the regression coefficients. It offers Benjamini-Hochberg or Bonferroni P-values techniques to adjust the P-values.
#'
#' @param count_matrix a data.frame object. It receives the raw matrix as input.
#' @param p_value_adjustment a data.frame object. It receives the experimental design matrix. The sample names must be identified in the first column. This matrix can also have more columns with information for the fixed and the random effects for the samples.
#' @param formula it receives fixed and random effects descriptions.
#' @param design_matrix All the P-values computed must be corrected and the DEGRE package offers two possibilities: "BH" (Benjamini-Hochberg) correction (default) and "BON" (Bonferroni) correction.
#'
#' @return dataframe object
#'
#' @examples
#' \donttest{
#' # Reading the count matrix and the design matrix for an example:
#' dir <- system.file("extdata", package = "DEGRE")
#' tab <- read.csv(file.path(dir,"count_matrix_for_example.csv"))
#' des <- read.csv(file.path(dir,"design_matrix_for_example.csv"))
#' # Running DEGRE function:
#' results <- DEGRE(count_matrix = tab,
#'                  p_value_adjustment = "BH",
#'                  design_matrix = des,
#'                  formula = "condition + (1|sex)")
#' }
#' @import stats glmmTMB foreach parglm tibble ggplot2 ggpubr ggrepel utils car
#' @importFrom car Anova
#' @importFrom dplyr mutate_all
#' @export
DEGRE <- function(count_matrix, p_value_adjustment = "BH", formula, design_matrix){
  ### Pre-processing steps
  # Scale correction
  scale_correction <- function(count_matrix) {
    sums <- apply(count_matrix, 2, sum)
    df_rnaseq <- sums / min(sums)
    count_matrix <- t((t(count_matrix) / df_rnaseq))
    return(count_matrix)
  }

  # RLE normalization
  size_factors <- function(count_matrix) {
    count_matrix_log2 <- log2(count_matrix)
    count_matrix_log2 <- as.data.frame(count_matrix_log2)

    # Geometric mean by rows
    count_matrix_log2$geom_mean <-
      apply(count_matrix_log2, 1, prod) ^ (1 / ncol(count_matrix_log2))
    count_matrix_log2 <-
      count_matrix_log2[count_matrix_log2$geom_mean != 0,] # Not genes with geom. mean equal to zero

    count_matrix_log2_ratios <- count_matrix_log2[, 1:ncol(count_matrix_log2) - 1]
    count_matrix_log2_ratios <- count_matrix_log2_ratios / count_matrix_log2$geom_mean

    medianWithoutNA <- function(x) {
      median(x[which(!is.na(x))])
    }
    Sj_median <-
      apply(count_matrix_log2_ratios, 2, medianWithoutNA) # Median without NAs

    Sj_median_log <- log2(Sj_median)
    Sj_median_log_sum <- sum(Sj_median_log)
    Sj_median_log_sum_exp <-
      exp((Sj_median_log_sum) * (1 / length(Sj_median_log)))
    Sj_median_log_sum_exp_final <-
      Sj_median / Sj_median_log_sum_exp

    return(Sj_median_log_sum_exp_final)
  }

  ### Checking steps in the matrices
  # Check if the user has the count and the design matrices
  if (missing(count_matrix))
    stop("You need to enter with the count matrix.")
  if (missing(design_matrix))
    stop("You need to enter with the design matrix.")

  # Check if the data is a data frame
  if (!is.data.frame(count_matrix))
    stop("You need to enter with the count matrix as data frame.")
  if (!is.data.frame(design_matrix))
    stop("You need to enter with the design matrix as data frame.")

  # Check if there are rows and columns in the matrices
  if (nrow(count_matrix) == 0 && ncol(count_matrix) == 0)
    stop("Your count matrix has zero rows or zero columns.")
  if (nrow(design_matrix) == 0 && ncol(design_matrix) == 0)
    stop("Your design matrix has zero rows or zero columns.")

  # Check if the user has the formula
  if (missing(formula))
    stop("You need to specify the formula.")

  # Check if the matrices are compatible in the name of the samples
  if(!is.na(setdiff(colnames(count_matrix), design_matrix[[1]])[1]))
    stop("Please, check the sample names and quantity in both matrices,
         both must have the same samples.")

  # Check if the design matrix is unbalanced
  if(table(design_matrix[2])[1] != table(design_matrix[2])[2])
    stop("Your design matrix is unbalanced.
         Please, enter with the same number of replicates for both experimental conditions.")

  # Check if there are replicates in the design matrix
  if(table(design_matrix[2])[1] == 1 || table(design_matrix[2])[2] == 1)
    stop("You must enter with at least two replicates for each experimental condition.")

  # Check if there is at least one level and max of two levels in the design matrix
  if(dim(as.data.frame(table(design_matrix[2])))[2] == 1)
    stop("You only have one level in the design matrix.")
  if(dim(as.data.frame(table(design_matrix[2])))[2] > 2)
    stop("You have more than two levels in the design matrix.")

  count_matrix <- scale_correction(count_matrix)

  Sj <- size_factors(count_matrix = count_matrix)
  count_matrix_Normalized <- as.data.frame(t(t(count_matrix) / Sj))

  # Log2CPM
  sums_by_col_by_milion_for_log2cpm <- apply(count_matrix_Normalized, 2, sum) / 1000000
  temp <- sweep(count_matrix_Normalized, 2, sums_by_col_by_milion_for_log2cpm, `/`)
  temp <- log(count_matrix_Normalized, base = 2)
  temp <- data.frame(lapply(temp, function(x) {
    gsub(-Inf, 0, x)
  }))
  temp <- mutate_all(temp, function(x) as.numeric(as.character(x)))
  row.names(temp) <- row.names(count_matrix_Normalized)
  temp2 <- as.data.frame(apply(temp, 1, mean))
  temp2$genes <- row.names(temp2)
  colnames(temp2) <- c("averagelogCPM","ID")

  # Separate zeros and non-zeros in all columns
  ## Expressed in all samples
  without_zero <- count_matrix_Normalized[!(apply(count_matrix_Normalized, 1, function(y) any(y == 0))),]

  ## Never expressed.
  with_zero_all_columns <- count_matrix_Normalized[(apply(count_matrix_Normalized, 1, function(y) all(y == 0))),]

  ### Genes expressed in one condition and not in the other
  with_zero_some_columns <- count_matrix_Normalized[!(apply(count_matrix_Normalized, 1, function(y) all(y == 0))),]
  with_zero_some_columns <- with_zero_some_columns[(apply(with_zero_some_columns, 1, function(y) any(y == 0))),]

  if (dim(with_zero_some_columns)[1] != 0) {
    # Expressed a replicate and not in the other
    with_zero_some_columns <- as.data.frame(with_zero_some_columns)

    for (i in 1:dim(with_zero_some_columns)[1]) {
      with_zero_some_columns$p_value_005[i] <- wilcox.test(as.numeric(with_zero_some_columns[i,]), conf.level = 0.95)$p.value
    }

    with_zero_wilcoxon_some_columns <- data.frame() # Equal to zero
    with_zero_some_columns_updated <- data.frame() # Differ from zero
    with_zero_wilcoxon_some_columns <- with_zero_some_columns[with_zero_some_columns$p_value_005 > 0.05,]
    with_zero_some_columns_updated <- with_zero_some_columns[with_zero_some_columns$p_value_005 < 0.05,]

    with_zero_some_columns_updated$p_value_005 <- NULL

    ### Updating the without zero:
    without_zero <-
      rbind(without_zero, with_zero_some_columns_updated) # Bind statistically different from zero to the whitout_zero df, by row
    without_zero <- round(without_zero)
  } else{
    without_zero <- round(without_zero)
  }

  # Same counts in all replicates
  ## Remove same values in all replicates
  uniques <- apply(without_zero, 1, unique)
  for (i in 1:dim(without_zero)[1]) {
    without_zero$uniques[i] <- lengths(uniques[i])
  }
  without_zero <- without_zero[!without_zero$uniques == 1,]
  without_zero <- without_zero[,-dim(without_zero)[2]]

  # CPM filtering of low counts
  without_zero_CPM <- without_zero

  sums_by_col_by_milion <- apply(without_zero_CPM, 2, sum) / 1000000
  without_zero_CPM <- sweep(without_zero_CPM, 2, sums_by_col_by_milion, `/`)

  without_zero_CPM$average_CPM <- apply(without_zero_CPM, 1, mean)
  without_zero_CPM <- without_zero_CPM[without_zero_CPM$average_CPM > 1,]

  rownames <- row.names(without_zero_CPM)
  without_zero_without_low_count <- subset(without_zero, rownames(without_zero) %in% rownames)

  # GLMM
  count_matrix_bip_overdisp <- as.data.frame(t(without_zero_without_low_count))

  #calculates GLMM
  calcglmm <- function(i, count_matrix_bip_overdisp) {
    tmp <- Anova (glmmTMB(as.formula(paste0("count_matrix_bip_overdisp[[i]] ~ ", formula)),
                                         data = count_matrix_bip_overdisp, family=nbinom2,
                                         REML = TRUE,
                                         control=glmmTMBControl( optimizer=optim,
                                                                 optArgs=list(method="BFGS"))),
                                 type=c("II", "III", 2, 3))[3][[1]]

    return(c(names$X1[i], tmp))
  }


  # For the GLMM: create the count matrix with the features of the design matrix
  count_matrix_bip_overdisp$sample <- rownames(count_matrix_bip_overdisp)
  count_matrix_bip_overdisp <- merge(count_matrix_bip_overdisp, design_matrix, by = "sample")
  rownames(count_matrix_bip_overdisp) <- count_matrix_bip_overdisp$sample
  count_matrix_bip_overdisp <- subset(count_matrix_bip_overdisp, select=-c(sample))
  extra_cols <- dim(count_matrix_bip_overdisp)[2] - dim(as.data.frame(t(without_zero_without_low_count)))[2]
  count_matrix_bip_overdisp <- count_matrix_bip_overdisp[,c(((dim(count_matrix_bip_overdisp)[2]+1)-extra_cols):dim(count_matrix_bip_overdisp)[2], extra_cols+1:dim(count_matrix_bip_overdisp)[2]-extra_cols)]
  count_matrix_bip_overdisp <- count_matrix_bip_overdisp[,-c((dim(count_matrix_bip_overdisp)[2]-extra_cols+1):dim(count_matrix_bip_overdisp)[2])]

  names <- as.data.frame(colnames(count_matrix_bip_overdisp))
  colnames(names) <- c("X1")

  models <-c()
  li <- (extra_cols+1):dim(count_matrix_bip_overdisp)[2]
  models <- foreach(i=li, .packages=c("glmmTMB"), .errorhandling = 'pass') %dopar% {
    calcglmm(i, count_matrix_bip_overdisp)
  }

  results <-  data.frame(1,1)
  for(i in 1:length(models)){
    tryCatch( if(!is.null(models[[i]]) & !is.na(models[[i]][2])){
      results[i,1] <- models[[i]][1]
      results[i,2] <- models[[i]][2]
    }, error=function(err) {
    } )
  }
  results <- as.data.frame(t(t(results)))
  colnames(results) <- c("ID","P-value")
  results <- results[complete.cases(results$`P-value`),]

  # P-values adjusted:
  results$`Q-value` <- p.adjust(results$`P-value`, method = paste0(p_value_adjustment), n = length(results$`P-value`))

  # log2FC
  # The user inputs the design and the count matrix
  variables <- as.data.frame(table(design_matrix[,2]))[,1]
  one_fac <- design_matrix[design_matrix[,2] == variables[1],]
  two_fac <- design_matrix[design_matrix[,2] == variables[2],]

  count_matrix <- as.data.frame(count_matrix)
  one_fac_to_sum <- count_matrix[, which((names(count_matrix) %in% one_fac$sample)==TRUE)]
  two_fac_to_sum <- count_matrix[, which((names(count_matrix) %in% two_fac$sample)==TRUE)]

  one_fac_to_sum$sum <- rowSums(one_fac_to_sum)
  two_fac_to_sum$sum <- rowSums(two_fac_to_sum)

  log2fc <- as.data.frame(log2(one_fac_to_sum$sum/two_fac_to_sum$sum))
  colnames(log2fc) <- "log2FC"
  log2fc$ID <- row.names(count_matrix)

  results_log2fc <- merge(log2fc, results, by = "ID")
  results <- merge(results_log2fc, temp2, by.x = "ID")

  return(results)
}
