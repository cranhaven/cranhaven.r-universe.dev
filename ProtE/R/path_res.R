#' Proteome Discoverer (PD) multiple-files' proteomic analysis
#'
#' Takes as input Proteomics Data (output of PD) in the format of a single file per sample and creates a master table with the Protein names and Abundance values. Then it performs exploratory data analysis, providing different options for  data manipulation (normalization, filtering based on the missing values and imputation) It then proceeds to perform statistical analysis, while creating exploratory plots such as relative log expression boxplots and violin plots, heatmaps and PCA plots.
#'
#' @param ... The specific path to the folder where the samples from each group are located. They are passed as unnamed arguments via "...".  Attention: Ensure paths use '/' as a directory separator.
#' @param bugs Either 0 to treat Proteome Discoverer bugs as Zeros (0) or "average" to convert them into the average of the protein between the samples. By default, it is set to 0. Bugs are referred to to the proteins with empty values inside a single-file analysis
#' @param normalization The specific method for normalizing the data.By default it is set to FALSE. Options are FALSE for no normalization of the data, "log2" for a simple log2 transformation, "Quantile" for a quantiles based normalization  and "Cyclic_Loess" for a Cyclic Loess normalization of the log2 data, "median" for a median one, "TIC" for Total Ion Current normalization, "VSN" for Variance Stabilizing Normalization and "PPM" for Parts per Million transformation of the data.
#' @param filtering_value The maximum allowable percentage of missing values for a protein. Proteins with missing values exceeding this percentage will be excluded from the analysis. By default it is set to 50.
#' @param global_filtering TRUE/FALSE. If TRUE, the per-protein percentage of missing values will be calculated across the entire dataset. If FALSE, it will be calculated separately for each group, allowing proteins to remain in the analysis if they meet the criteria within any group. By default it is set to TRUE.
#' @param imputation Imputes all remaining missing values. Available methods: "LOD" for assigning the dataset's Limit Of Detection (lowest protein intensity identified), "LOD/2", "Gaussian_LOD" for selecting random values from the normal distribution around LOD with sd= 0.2*LOD, "zeros" for simply assigning 0 to MVs, mean" for replacing missing values with the mean of each protein across the entire dataset, "kNN" for a k-nearest neighbors imputation using 5 neighbors (from the package VIM) and "missRanger" for a random forest based imputation using predictive mean matching (from the package missRanger). By default it is set to FALSE (skips imputation).
#' @param independent TRUE/FALSE If TRUE, the samples come from different populations, if FALSE they come from the same population (Dependent samples). By default, it is set to TRUE. If set to FALSE, the numbers given in the samples_per_group param must be equal to each other.
#' @param parametric TRUE/FALSE. Specifies the statistical tests that will be taken into account for creating the PCA plots and heatmap. By default it is set to FALSE (non-parametric).
#' @param significance "p" or "BH" Specifies which of the p-values (nominal vs BH adjusted for multiple hypothesis) will be taken into account for creating the PCA plots and the heatmap. By default it is set to "p" (nominal p-value).
#' @param description TRUE/FALSE. If TRUE, establishes connection to the Uniprot database (via the Uniprot.ws package) and adds the "Description" annotation in the data. This option requires protein Accession IDs and is thus applicable only to the pg.matrix file. It requires also internet access. By default it is set to FALSE (No description fetching).
#'
#'
#' @return Excel files with the proteomic values that are optionally processed, via normalization, imputation and filtering of proteins with a selected percentage of missing values. The result of the processing is visualized with an Protein Rank Abundance plot. PCA plots for all groups and for just their significant correlations are created. Furthermore violin and boxplots for the proteins of each sample is created and a heatmap for the significant proteins.
#' @importFrom openxlsx write.xlsx  read.xlsx
#' @importFrom dplyr select  group_by  group_modify everything  %>%
#' @importFrom tidyr gather pivot_longer
#' @importFrom broom tidy
#' @importFrom grid gpar
#' @importFrom grDevices  dev.off bmp colorRampPalette
#' @importFrom reshape2 melt
#' @importFrom ggpubr ggarrange
#' @importFrom ggplot2 ggplot ggsave geom_smooth geom_violin scale_color_gradient element_line theme_linedraw scale_fill_manual scale_color_manual aes geom_histogram element_rect geom_point xlab ylab ggtitle theme_bw theme_minimal theme element_text guides guide_legend geom_boxplot labs theme_classic element_blank geom_jitter position_jitter
#' @importFrom VIM kNN
#' @importFrom UniprotR GetProteinAnnontate
#' @importFrom stringr str_extract
#' @importFrom stats kruskal.test p.adjust prcomp sd wilcox.test friedman.test rnorm bartlett.test model.matrix heatmap median na.omit
#' @importFrom forcats fct_inorder
#' @importFrom vegan adonis2
#' @importFrom limma topTable eBayes contrasts.fit normalizeCyclicLoess normalizeVSN lmFit normalizeQuantiles duplicateCorrelation
#' @importFrom pheatmap pheatmap
#' @importFrom missRanger missRanger
#' @importFrom car leveneTest
#'
#' @examples
#' #Example of running the function with paths for three groups.
#'
#'
#' T1_path <- system.file("extdata", "PDexports(multiple_files)",
#'  "T1_BLCA", package = "ProtE")
#' T2_path <- system.file("extdata", "PDexports(multiple_files)",
#' "T2_BLCA", package = "ProtE")
#'
#' temp_dir1 <- file.path(tempdir(), "T1_path")
#' temp_dir2 <- file.path(tempdir(), "T2_path")
#'
#' dir.create(temp_dir1, recursive = TRUE, showWarnings = FALSE)
#' dir.create(temp_dir2, recursive = TRUE, showWarnings = FALSE)
#'
#'
#'
#' excel_files1 <- list.files(T1_path, pattern = "\\.xlsx$", full.names = TRUE)
#' excel_files2 <- list.files(T2_path, pattern = "\\.xlsx$", full.names = TRUE)
#'
#' file.copy(excel_files1, temp_dir1)
#' file.copy(excel_files2, temp_dir2)
#'
#' pd_multi(temp_dir1, temp_dir2,
#'          normalization = FALSE,
#'          global_filtering = TRUE,  imputation = FALSE,
#'          independent = TRUE)
#'
#' @export

pd_multi <- function(...,
                        imputation = FALSE,
                        global_filtering = TRUE,
                        independent = TRUE,
                        filtering_value = 50,
                        bugs = 0,
                        normalization = FALSE,
                        parametric= FALSE,
                        significance = "p",
                     description = FALSE)
  {
   Sample=group1= Accession =Description =Symbol =X = Mean = SD =Y =df4_wide= percentage=variable =.= g1.name =g2.name=key =value = NULL
message("The ProtE process starts now!")
group_paths <- list(...)
groups_number <- length(group_paths)

group_paths<- gsub( "\\\\", "/", group_paths)
for (i in 1:groups_number) {
  assign(paste0("group",i),group_paths[[i]])}
group_names <- basename(group_paths)
for (i in 1:groups_number) {
assign(paste0("g",i,".name"),group_names[[i]])}

dataspace <- data.frame()

samples_per_group <- numeric(groups_number)

for (i in seq_along(group_paths)) {
  file_names <- list.files(path = group_paths[[i]], pattern = "*.xlsx")
  samples_per_group[i] <- length(file_names)
  for (j in seq_along(file_names)) {
    file_case <- openxlsx::read.xlsx(file.path(group_paths[[i]], file_names[j]), sheet = 1)
    dataspace <- rbind(dataspace, file_case[,grep("Accession|Description",colnames(file_case))])
  }
}
dataspace <- dataspace[!is.na(dataspace$Accession),]
dupl <- duplicated(dataspace[,1])
dataspace_no_dupl <- dataspace[!dupl,]
dataspace_no_dupl <- droplevels(dataspace_no_dupl)
dataspace <- dataspace_no_dupl

for (i in 1:groups_number) {
  file_names <- list.files(path = group_paths[[i]], pattern = "*.xlsx")
  for (j in seq_along(file_names)){
  file_case <- openxlsx::read.xlsx(file.path(group_paths[[i]], file_names[j]), sheet = 1)
  dataspace <- merge(x = dataspace,y = file_case[,grep("Accession|Area|Abundance:",colnames(file_case))], by = "Accession" ,all.x = TRUE)
  colnames(dataspace)[length(colnames(dataspace))] <- file_names[j]
}
}

dataspace <- dataspace[rowSums(!is.na(dataspace[,-c(1,2)])) > 0, ]
message("Removing whichever proteins have only missing values in their abundances.")

colnames(dataspace) <- gsub(".xlsx", "", colnames(dataspace))

if ("Description" %in% colnames(dataspace)){
  message("Description is already included in your input data.No fetching required.")
} else {
  if (description == FALSE ){
    df_description <- data.frame("Description" = character(nrow(dataspace)), stringsAsFactors = FALSE)
    df_description$Description <- "Not Available"
    dataspace<-cbind(dataspace[,1],df_description,dataspace[,2:ncol(dataspace)])
  }
  if (description == TRUE){
    message("The Description fetching from UniProt starts now, it might take some time depending on you Network speed.")
    id_numbers <- dataspace$Protein.Ids
    for (j in 1:length(id_numbers)){id_numbers[j] <- stringr::str_extract(id_numbers[j], "^[^;]*")}
    df_description <- data.frame("Description" = character(), stringsAsFactors = FALSE)
    dataspace$Protein.Ids<-id_numbers
    conv_ID <- GetProteinAnnontate(dataspace$Protein.Ids, columns =c("organism_name"	, "protein_name"	, "id"	,"gene_names") )
    details<-paste(conv_ID$Protein.names," OS=",conv_ID$Organism," GN=",conv_ID$Gene.Names," -[",conv_ID$Entry.Name,"]")
    df_description <- rbind(df_description, data.frame(Description = details, stringsAsFactors = FALSE))


    dataspace<-cbind(dataspace[,1],df_description,dataspace[,2:ncol(dataspace)])
  }
}

path_g1 <- dirname(group1)
path_res <- file.path(path_g1, "ProtE_Analysis")
dir.create(path_res, showWarnings = FALSE)
path_restat <- file.path(path_res, "Statistical_Analysis")
path_resman <- file.path(path_res, "Data_processing")
path_resplot <- file.path(path_res, "Plots")

dir.create(path_restat, showWarnings = FALSE)
dir.create(path_resman, showWarnings = FALSE)
dir.create(path_resplot, showWarnings = FALSE)


(message("All files created will be stored in the folder ProtE_Analysis, that is located on the last directory of the first group that you input."))
mt_file_path <- file.path(path_resman, "Masterlist.xlsx")
openxlsx::write.xlsx(dataspace, file = mt_file_path)
    message("Concatenated all data files to a single matrix, saved as Masterlist.xlsx")

    zero_per_sample <- colSums(is.na(dataspace[,-1:-2]))*100/nrow(dataspace)
    IDs <- colSums(!is.na(dataspace[,-1:-2]))


    colnames(dataspace) <- make.names(colnames(dataspace), unique = TRUE)
    rownames(dataspace) <- make.names(rownames(dataspace), unique = TRUE)


    if (normalization == "PPM"){
    dataspace[, -1:-2] <- lapply(dataspace[, -1:-2], function(x) {
      sum_x <- sum(x, na.rm = TRUE)
      ifelse(is.na(x), NA, (x / sum_x) * 10^6)
    })

    Gdataspace<-dataspace
    Gdataspace$Symbol = sub(".*GN=(.*?) .*","\\1",Gdataspace$Description)
    Gdataspace$Symbol[Gdataspace$Symbol==Gdataspace$Description] = "Not available"
    Gdataspace<-Gdataspace %>%
      dplyr::select(Accession, Description, Symbol, everything())
    colnames(Gdataspace) <- gsub(".xlsx", "", colnames(Gdataspace))
    norm_file_path <- file.path(path_resman, "Normalized.xlsx")
    openxlsx::write.xlsx(Gdataspace, file = norm_file_path)
    message("Applying the selected normalization, saved as Normalized.xlsx")}

    if (normalization == "Quantile"){
      dataspace[, -1:-2] <- log(dataspace[, -1:-2]+1,2)
      dataspace[, -1:-2] <- limma::normalizeQuantiles(dataspace[, -1:-2])
      Gdataspace<-dataspace
      Gdataspace$Symbol = sub(".*GN=(.*?) .*","\\1",Gdataspace$Description)
      Gdataspace$Symbol[Gdataspace$Symbol==Gdataspace$Description] = "Not available"
      Gdataspace<-Gdataspace %>%
        dplyr::select(Accession, Description, Symbol, everything())
      colnames(Gdataspace) <- gsub(".xlsx", "", colnames(Gdataspace))
      norm_file_path <- file.path(path_resman, "Normalized.xlsx")
      openxlsx::write.xlsx(Gdataspace, file = norm_file_path)
      message("Applying the selected normalization, saved as Normalized.xlsx") }
    if (normalization == "log2"){
      dataspace[, -1:-2] <- log(dataspace[, -1:-2]+1,2)
      Gdataspace<-dataspace
      Gdataspace$Symbol = sub(".*GN=(.*?) .*","\\1",Gdataspace$Description)
      Gdataspace$Symbol[Gdataspace$Symbol==Gdataspace$Description] = "Not available"
      Gdataspace<-Gdataspace %>%
        dplyr::select(Accession, Description, Symbol, everything())
      colnames(Gdataspace) <- gsub(".xlsx", "", colnames(Gdataspace))
      norm_file_path <- file.path(path_resman, "Normalized.xlsx")
      openxlsx::write.xlsx(Gdataspace, file = norm_file_path)
      message("Applying the selected normalization, saved as Normalized.xlsx") }
    if (normalization == "Total_Ion_Current") {
      dataspace[, -1:-2] <- lapply(dataspace[, -1:-2], function(x) (x / sum(x, na.rm = TRUE)) * mean(colSums(dataspace[, -1:-2], na.rm = TRUE)))
      Gdataspace<-dataspace
      Gdataspace$Symbol = sub(".*GN=(.*?) .*","\\1",Gdataspace$Description)
      Gdataspace$Symbol[Gdataspace$Symbol==Gdataspace$Description] = "Not available"
      Gdataspace<-Gdataspace %>%
        dplyr::select(Accession, Description, Symbol, everything())
      colnames(Gdataspace) <- gsub(".xlsx", "", colnames(Gdataspace))
      norm_file_path <- file.path(path_resman, "Normalized.xlsx")
      openxlsx::write.xlsx(Gdataspace, file = norm_file_path)
      message("Applying the selected normalization, saved as Normalized.xlsx")}

    if (normalization == "Cyclic_Loess"){
      dataspace[, -1:-2] <- log(dataspace[, -1:-2]+1,2)
      dataspace[, -1:-2] <- limma::normalizeCyclicLoess(dataspace[, -1:-2])
      Gdataspace<-dataspace
      Gdataspace$Symbol = sub(".*GN=(.*?) .*","\\1",Gdataspace$Description)
      Gdataspace$Symbol[Gdataspace$Symbol==Gdataspace$Description] = "Not available"
      Gdataspace<-Gdataspace %>%
        dplyr::select(Accession, Description, Symbol, everything())
      colnames(Gdataspace) <- gsub(".xlsx", "", colnames(Gdataspace))
      norm_file_path <- file.path(path_resman, "Normalized.xlsx")
      openxlsx::write.xlsx(Gdataspace, file = norm_file_path)
      message("Applying the selected normalization, saved as Normalized.xlsx") }

     if ( normalization == "VSN") {
       dataspace[, -1:-2] <- suppressMessages(limma::normalizeVSN(dataspace[, -1:-2]))
       Gdataspace<-dataspace
       Gdataspace$Symbol = sub(".*GN=(.*?) .*","\\1",Gdataspace$Description)
       Gdataspace$Symbol[Gdataspace$Symbol==Gdataspace$Description] = "Not available"
       Gdataspace<-Gdataspace %>%
         dplyr::select(Accession, Description, Symbol, everything())
       colnames(Gdataspace) <- gsub(".xlsx", "", colnames(Gdataspace))
       norm_file_path <- file.path(path_resman, "Normalized.xlsx")
       openxlsx::write.xlsx(Gdataspace, file = norm_file_path)
       message("Applying the selected normalization, saved as Normalized.xlsx")
            }
     if (normalization == "median") {
      sample_medians <- apply(dataspace[, -1:-2], 2, median, na.rm = TRUE)
      dataspace[, -1:-2] <- sweep(dataspace[, -1:-2], 2, sample_medians, FUN = "/")
      Gdataspace<-dataspace
      Gdataspace$Symbol = sub(".*GN=(.*?) .*","\\1",Gdataspace$Description)
      Gdataspace$Symbol[Gdataspace$Symbol==Gdataspace$Description] = "Not available"
      Gdataspace<-Gdataspace %>%
        dplyr::select(Accession, Description, Symbol, everything())
      colnames(Gdataspace) <- gsub(".xlsx", "", colnames(Gdataspace))
      norm_file_path <- file.path(path_resman, "Normalized.xlsx")
      openxlsx::write.xlsx(Gdataspace, file = norm_file_path)
      message("Applying the selected normalization, saved as Normalized.xlsx")}

    if (normalization %in% c(FALSE,"median", "Total_Ion_Current", "PPM") ){
      log.dataspace <- log(dataspace[,-c(1:2)]+1,2)
      row_means <- rowMeans(log.dataspace, na.rm = TRUE)
      row_sds <- apply(log.dataspace, 1, sd, na.rm = TRUE)
      plot_data <- data.frame(Mean = row_means, SD = row_sds)
      meansd <- ggplot(plot_data, aes(x = Mean, y = SD)) +
        geom_point(alpha = 0.5, color = "blue") +
      suppressMessages(geom_smooth(method = "loess", color = "red", se = FALSE)) +
        theme_minimal() +
        labs(title = "Mean-SD Plot on the log2 normalized data", x = "Mean Expression", y = "Standard Deviation")
      meansd
      ggplot2::ggsave("meanSdPlot.bmp", plot = meansd,  path = path_resplot,
                      scale = 1, width = 5, height = 4, units = "in",
                      dpi = 300, limitsize = TRUE)

      message("Creating Mean-SD plot on the log2 data.")
    } else {
      row_means <- rowMeans(dataspace[,-c(1,2)], na.rm = TRUE)
      row_sds <- apply(dataspace[,-c(1,2)], 1, sd, na.rm = TRUE)
      plot_data <- data.frame(Mean = row_means, SD = row_sds)
      meansd <- ggplot(plot_data, aes(x = Mean, y = SD)) +
        geom_point(alpha = 0.5, color = "blue") +
        geom_smooth(method = "loess", color = "red", se = FALSE) +
        theme_minimal() +
        labs(title = "Mean-SD Plot on the log2 normalized data", x = "Mean Expression", y = "Standard Deviation")
      meansd
      ggplot2::ggsave("meanSdPlot.bmp", plot = meansd,  path = path_resplot,
                      scale = 1, width = 5, height = 4, units = "in",
                      dpi = 300, limitsize = TRUE)
      message("Creating Mean-SD plot.")
    }


name_dataspace <-  dataspace[, -1:-2]
    dat.dataspace<-dataspace

if (filtering_value < 0 && filtering_value > 100) {stop("Error: The filtering_value must be a number ranging from 0 to 100")}

if (global_filtering == TRUE) {

    filtering_value <- 100- as.numeric(filtering_value)

      threshold <-  ceiling(sum(samples_per_group)-(sum(samples_per_group)*(as.numeric(filtering_value)/100))+0.00000000001)
  }

 if (global_filtering == FALSE) {
      threshold<-numeric(groups_number)
        filtering_value <- 100- as.numeric(filtering_value)
        for (i in 1:groups_number) {
          threshold[i] <-  ceiling(samples_per_group[i]-(samples_per_group[i])*(as.numeric(filtering_value)/100)+0.00000000001)}
}

        if (bugs != 0 && bugs != "average") {stop("Error, you should assign bugs as 0 or average")}

    coln <- list()
    case_last <- 2

    for (i in 1:groups_number) {
      case_last <- case_last + samples_per_group[i]
      coln[[i]] <- (case_last - samples_per_group[i] + 1):case_last
    }

    if (bugs== "average"){
      dat.dataspace[dat.dataspace==0] <- 1
      dat.dataspace[is.na(dat.dataspace)] <- 0

      rep.df <- data.frame()
 for ( i in 1:groups_number) {
   dat.data<-dat.dataspace[,coln[[i]]]
   rep.data<-dataspace[,coln[[i]]]
   m<-rowMeans(rep.data[i])
   idx <- dat.data == 1
   tmp<-idx*m
   rep.data[idx]<-tmp[idx]
   rep.df<-data.frame(rep.df,rep.data)
 }
      dataspace <- data.frame(dat.dataspace[,c(1:2)],rep.df)
}


    dataspace[is.na(dataspace)] <- 0

    for (j in 1:groups_number){

      for (i in c(1:nrow(dataspace))){
        a <- table(dataspace[i,coln[[j]]]==0)["TRUE"]
        if(is.na(a)){
          dataspace[i,paste0("Number_0_group",j)] <- 0
        } else{
          dataspace[i,paste0("Number_0_group",j)] <- table(dataspace[i,coln[[j]]]==0)["TRUE"]
        }
      }
    }
    dataspace$Number_0_all_groups <- rowSums(dataspace[,paste0("Number_0_group", 1:groups_number)])

    dataspace <- dataspace[dataspace$Number_0_all_groups < sum(samples_per_group),]

    if (global_filtering == TRUE) {
      dataspace <- dataspace[dataspace$Number_0_all_groups<threshold,]
      at_file_path <- file.path(path_resman, "Dataset_after_filtering.xlsx")
      openxlsx::write.xlsx(dataspace, file = at_file_path)
    }

    if (global_filtering == FALSE) {
      keep_rows <- rep(FALSE, nrow(dataspace))
      for (j in 1:groups_number) {
        keep_rows <- keep_rows | (dataspace[,paste0("Number_0_group", j)] < threshold[j])
      }
      dataspace <- dataspace[keep_rows, ]
      at_file_path <- file.path(path_resman, "Dataset_after_filtering.xlsx")
      openxlsx::write.xlsx(dataspace, file = at_file_path)}
    message("An excel file with the proteins remaining in the data after filtering for missing values, has been created as Dataset_after_filtering.xlsx")
    dataspace_0s<- dataspace
    dataspace[,paste0("Number_0_group", 1:groups_number)] <- NULL
    dataspace$Number_0_all_groups <- NULL



    zero_per_sample1 <- colSums(dataspace[,-1:-2] == 0)*100/nrow(dataspace)
    sample_names <- colnames(dataspace[,-1:-2])
    qc <- cbind(sample_names,IDs,zero_per_sample,zero_per_sample1)
    qc <- as.data.frame(qc)
    colnames(qc) <- c("Sample Name","Number of proteins detected in the sample","% of Missing values before filtering","% of Missing values after filtering")
    rownames(qc) <- NULL

    pre_dataspace <- dataspace

    if (imputation == "kNN") {
      message("kNN imputation starts now")
      dataspace[dataspace==0] <- NA
      dataspace[, -c(1, 2)] <- VIM::kNN(dataspace[, -c(1, 2)], imp_var = FALSE, k= 5)
      imp_file_path <- file.path(path_resman, "Dataset_Imputed.xlsx")
      openxlsx::write.xlsx(dataspace, file = imp_file_path)
    }
    if (imputation == "mean"){
      data_to_impute <- dataspace[, 3:(2 + sum(samples_per_group))]
      for (i in seq_len(nrow(data_to_impute))) {
        row_data <- data_to_impute[i, ]

        if (any(is.na(row_data))) {
          row_data  <- as.numeric(row_data)
          row_mean <- mean(row_data, na.rm = TRUE)
          row_data[is.na(row_data)] <- row_mean
          data_to_impute[i, ] <- row_data
        }
      }
        dataspace[, 3:(2 + sum(samples_per_group))] <- data_to_impute
      imp_file_path <- file.path(path_resman, "Dataset_Imputed.xlsx")
      openxlsx::write.xlsx(dataspace, file = imp_file_path) }
    if (imputation == "LOD"){
      dataspace[dataspace==0] <- NA
      impute_value <- min(as.matrix(dataspace[, -c(1, 2)]),na.rm = TRUE)
      dataspace[, -c(1, 2)][is.na(dataspace[, -c(1, 2)])]  <- impute_value
      imp_file_path <- file.path(path_resman, "Dataset_Imputed.xlsx")
      openxlsx::write.xlsx(dataspace, file = imp_file_path)  }
    if (imputation == "LOD/2"){
      dataspace[dataspace==0] <- NA
      impute_value <- min(as.matrix(dataspace[, -c(1, 2)]),na.rm = TRUE)/2
      dataspace[, -c(1, 2)][is.na(dataspace[, -c(1, 2)])]  <- impute_value
      imp_file_path <- file.path(path_resman, "Dataset_Imputed.xlsx")
      openxlsx::write.xlsx(dataspace, file = imp_file_path)  }
    if (imputation == "Zeros"){
      imp_file_path <- file.path(path_resman, "Dataset_Imputed.xlsx")
      openxlsx::write.xlsx(dataspace, file = imp_file_path)    }
    if (imputation == "Gaussian_LOD") {
      dataspace[dataspace==0] <- NA
      LOD <- min(as.matrix(dataspace[, -c(1, 2)]),na.rm = TRUE)
      replace_gaussian <- function(x) {
        ifelse(is.na(x), rnorm(length(x), mean = LOD, sd = LOD*0.2), x)
      }
      dataspace[, -c(1, 2)] <- apply(dataspace[, -c(1, 2)], 2, replace_gaussian)
      imp_file_path <- file.path(path_resman, "Dataset_Imputed.xlsx")
      openxlsx::write.xlsx(dataspace, file = imp_file_path)
    }
    if(imputation == "missRanger"){
      message("missRanger imputation starts now")
      dataspace[dataspace==0] <- NA
      dataspace[,-c(1,2)] <- missRanger::missRanger(dataspace[,-c(1,2)])
      imp_file_path <- file.path(path_resman, "Dataset_Imputed.xlsx")
      openxlsx::write.xlsx(dataspace, file = imp_file_path)  }
    if (imputation %in% c("kNN","missRanger"))    {
      pre_dataspace1<-pre_dataspace[,-1:-2]
      dataspace1<-dataspace[,-1:-2]
      imp.values<- dataspace1 - pre_dataspace1

      his_dataspace<-rbind(dataspace1,pre_dataspace1,imp.values)
      if   (normalization %in% c("log2", "Quantile","Cyclic_Loess")){loghis_dataspace = his_dataspace
      }else{loghis_dataspace<-log2(his_dataspace+1)}

      his_long <-tidyr::pivot_longer(loghis_dataspace, cols = everything())
      nrows<-nrow(his_long)
      his_long$Group <- rep(c("Final","Initial","Imputed"), each = (nrows/3))
      his_long_filtered <- his_long[his_long$value != 0,]
      his_long_filtered$Group <- factor(his_long_filtered$Group, levels = c("Final", "Initial", "Imputed"))

      imp_hist<- ggplot(his_long_filtered, aes(x = value, fill = Group, colour = Group)) +
        labs( x = expression(Log[2]~"Proteins Abundance"), y = "Count") +
        scale_fill_manual(values = c("Final" = "#FF99FF", "Initial" = "#990000", "Imputed" = "#000033")) +
        scale_color_manual(values = c("Final" = "#FF99FF", "Initial" = "#990000", "Imputed" = "#000033")) +
        geom_histogram(alpha = 0.5, binwidth = 0.3, position = "identity") +
        theme_minimal() +
        theme(plot.background = element_rect(fill = "white")) + theme(panel.background = element_rect(fill = "white"))

      imp_hist

      ggplot2::ggsave("Imputed_values_histogram.bmp", plot = imp_hist,  path = path_resplot,
                      scale = 1, width = 5, height = 4, units = "in",
                      dpi = 300, limitsize = TRUE)
      message("A frequency histogram of real and imputed values has been created as Imputed_values_histogram.bmp")
    }
      if (imputation %in% c("LOD/2","LOD","kNN","missRanger","mean","zeros","Gaussian_LOD")){
        message("An excel with the imputed missing values has been created as Dataset_Imputed.xlsx")

        dataspace_0s$percentage <- dataspace_0s$Number_0_all_groups*100/sum(samples_per_group)
        dataspace$percentage <- dataspace_0s$percentage
        dataspace$mean <- rowMeans(dataspace[,3:(2+sum(samples_per_group))])
        if (normalization %in% c("log2", "Quantile","Cyclic_Loess")){dataspace$log = dataspace$mean}
        else {
          dataspace$log<-log2(dataspace$mean)}
        dataspace$rank <- rank(-dataspace$mean)

        abund.plot <- ggplot(dataspace, aes(x = rank, y = log, colour = percentage)) +
          geom_point(size = 3, alpha = 0.8) +
          labs(title = "Protein Abundance Rank", x = "Rank", y = "Mean", expression(Log[2] ~ "Protein Abundance")) +
          scale_color_gradient(low = "darkblue", high = "yellow",
                               name = "Imputations\nin each\nprotein\n(%)",limits = c(0,100-filtering_value)) +
          theme_linedraw()+
          theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                panel.grid = element_line(color = "grey80"),
                legend.title = element_text(size = 10, face = "bold"),
                legend.text = element_text(size = 9))
        abund.plot

        ggplot2::ggsave("Proteins_abundance_rank.bmp", plot = abund.plot ,  path = path_resplot,
                        scale = 1, width = 12, height = 5, units = "in",
                        dpi = 300, limitsize = TRUE, bg = "white")
      }

    if (imputation == FALSE){dataspace <- dataspace

    dataspace_0s$percentage <- dataspace_0s$Number_0_all_groups*100/sum(samples_per_group)
    dataspace$percentage <- dataspace_0s$percentage
    dataspace$mean <- apply(dataspace[, 3:(2+sum(samples_per_group))], 1, function(x) mean(x[x != 0]))
    if (normalization %in% c("log2", "Quantile","Cyclic_Loess")){dataspace$log = dataspace$mean}
    else {
      dataspace$log<-log2(dataspace$mean)}
    dataspace$rank <- rank(-dataspace$mean)

    abund.plot <- ggplot(dataspace, aes(x = rank, y = log, colour = percentage)) +
      geom_point(size = 3, alpha = 0.8) +
      labs(title = "Protein Abundance Rank", x = "Rank", y = expression(Log[2] ~ "Proteins Abundance")) +
      scale_color_gradient(low = "darkblue", high = "yellow",
                           name = "MVs\nin each\nprotein\n(%)",limits = c(0,100-filtering_value)) +
      theme_linedraw()+
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            panel.grid = element_line(color = "grey80"),
            legend.title = element_text(size = 10, face = "bold"),
            legend.text = element_text(size = 9))

    abund.plot

    ggplot2::ggsave("Proteins_abundance_rank.bmp", plot = abund.plot ,  path = path_resplot,
                    scale = 1, width = 12, height = 5, units = "in",
                    dpi = 300, limitsize = TRUE, bg = "white")
    }
    message("A protein abundance rank order plot has been created as Proteins_abundance_rank.bmp")
    dataspace$percentage <- NULL
    dataspace$mean <- NULL
    dataspace$log<- NULL
    dataspace$rank <- NULL

    groups_list <- list("character")
    for (i in 1:groups_number) {
      groups_list[[i]] <- rep(group_names[i], times = samples_per_group[i])
    }

    groups_list_u <- unlist(groups_list)
    groups_list_f <- factor(groups_list_u, levels = unique(groups_list_u))

    mm <- model.matrix(~groups_list_f + 0)
    colnames(mm)<- group_names

    nndataspace<- dataspace[,-1:-2]
    if   (normalization %in% c("log2", "Quantile","Cyclic_Loess","VSN")){nndataspace = nndataspace
    }else{nndataspace <- log2(nndataspace+1)}

    if (independent == FALSE){
      if (length(unique(samples_per_group)) != 1){
        message("Error: The paired group analysis requires an equal number of samples in each group. Please consider removing any samples that lack a corresponding matched pair.")
      }
      n = sum(samples_per_group)/groups_number
      pairing <- rep(1:n, each = groups_number)

      corfit <- limma::duplicateCorrelation(nndataspace, design = mm, block = pairing)
      fit <- limma::lmFit(nndataspace, mm, block = pairing, correlation = corfit$consensus.correlation)
    }
    if (independent == TRUE){
      fit <- limma::lmFit(nndataspace, mm)}
    fit<- limma::eBayes(fit)
    if (groups_number>2){
      anova_res<- limma::topTable(fit, adjust.method = "BH", number = Inf, sort.by = "none")
      colnames(anova_res)<-paste("ANOVA",colnames(anova_res), sep = "_")
      anova_res<- anova_res[,-c(1:groups_number)]}
    lima.res <- data.frame()
    for (i in 1:(ncol(mm)-1)) {
      for (j in (i+1):ncol(mm)) {
        comparison <- paste(colnames(mm)[i], "vs", colnames(mm)[j], sep = " ")
        contrast_fref <- limma::makeContrasts(contrasts = paste0(colnames(mm)[i],"-",colnames(mm)[j]), levels = mm)
        fit2 <- limma::contrasts.fit(fit, contrast_fref)
        fit2 <- limma::eBayes(fit2)
        top_table<- limma::topTable(fit2, adjust.method = "BH", number = Inf, sort.by = "none")
        column_groups<- top_table[,c("logFC","AveExpr","t","P.Value","adj.P.Val","B")]
        colnames(column_groups)<-paste(colnames(column_groups), comparison, sep = "_")

        if (nrow(lima.res) == 0){
          lima.res <- column_groups
        } else {lima.res<- cbind(lima.res, column_groups)}
      }}

    if (groups_number>2){
      limma_dataspace <- cbind(anova_res,lima.res,dataspace)} else {limma_dataspace <- cbind(lima.res,dataspace)}

    ncollimma <- ncol(limma_dataspace) - ncol(dataspace) + 2
    limma_dataspace<-limma_dataspace %>%
      dplyr::select(Accession, any_of(c("Protein.Names","Description")), dplyr::everything())
    limma_dataspace <- limma_dataspace[,1:ncollimma]
    limma_file_path <- file.path(path_restat, "Dataset_limma.test.xlsx")
    openxlsx::write.xlsx(limma_dataspace, file = limma_file_path)
    message("The limma output has been saved as Dataset_limma.test.xlsx")

    if (independent != FALSE && independent != TRUE){stop("Error. You need to assign independent = TRUE or FALSE")}
    data2 <- dataspace

    for (j in 1:groups_number) {
      data2[[paste0("Average_G", j)]] <- apply(data2[, coln[[j]]], 1, function(row) {
        if (all(is.na(row))) {
          0
        } else {
          mean(row, na.rm = TRUE)
        }
      })

      data2[[paste0("St_Dv_G", j)]] <- apply(data2[, coln[[j]]], 1, function(row) {
        if (sum(!is.na(row)) < 2) {
          0
        } else {
          sd(row, na.rm = TRUE)
        }
      })

      for (k in 1:j) {
        if (k < j) {
          for (i in 1:nrow(data2)) {
            values_k <- as.numeric(data2[i, coln[[k]]])
            values_j <- as.numeric(data2[i, coln[[j]]])

            if (independent == TRUE) {
              if (sum(!is.na(values_k)) > 0 & sum(!is.na(values_j)) > 0) {
                test_list <- stats::wilcox.test(
                  values_k, values_j,
                  exact = FALSE, paired = FALSE, na.rm = TRUE
                )
                data2[i, paste0("Wilcoxon_p_G", j, "vsG", k)] <- test_list$p.value
              } else {
                data2[i, paste0("Wilcoxon_p_G", j, "vsG", k)] <- NA
              }
            } else if (independent == FALSE) {
              paired_values <- na.omit(cbind(values_k, values_j))
              if (nrow(paired_values) > 0) {
                test_list <- stats::wilcox.test(
                  paired_values[, 1], paired_values[, 2],
                  exact = FALSE, paired = TRUE
                )
                data2[i, paste0("Wilcoxon_p_G", j, "vsG", k)] <- test_list$p.value
              } else {
                data2[i, paste0("Wilcoxon_p_G", j, "vsG", k)] <- NA
              }
            }
          }

          data2[[paste0("BH_p_G", j, "vsG", k)]] <- p.adjust(
            data2[[paste0("Wilcoxon_p_G", j, "vsG", k)]],
            method = "BH"
          )

          avg_j <- data2[[paste0("Average_G", j)]]
          avg_k <- data2[[paste0("Average_G", k)]]
          data2[[paste0("Ratio_G", j, "vsG", k)]] <- ifelse(
            avg_k == 0, NA, avg_j / avg_k
          )
          data2[[paste0("Log2_Ratio_G", j, "vsG", k)]] <- log2(
            data2[[paste0("Ratio_G", j, "vsG", k)]]
          )
        }
      }
    }

    for(i in 1:nrow(data2)){
      group_values <- list()
     for (j in 1:groups_number) {
        group_values[[j]]<- as.numeric(data2[i, coln[[j]]])
      }
      valid_groups <- group_values[sapply(group_values, function(x) sum(!is.na(x)) > 1)]
      if (length(valid_groups) >= 2) {
        bartlett_result <- bartlett.test(valid_groups)
        data2[i, "Bartlett_p"] <- bartlett_result$p.value
        leve_data <- data.frame(
          Value = unlist(valid_groups),
          Group = rep(seq_along(valid_groups), sapply(valid_groups, length))  )

        levene_result <- car::leveneTest(Value ~ as.factor(Group), data = leve_data, center = median)
        data2[i, "Levene_p"] <- levene_result$`Pr(>F)`[1]      } else {
        data2[i, "Bartlett_p"] <- NA
        data2[i, "Levene_p"] <- NA
      }

    }

    only.data <- dataspace[,-c(1:2)]
    transposed_data <- t(only.data)
    metadata2 <- data.frame(group = groups_list_u)
    rownames(transposed_data) <- metadata2$group
    metadata2$samples <- colnames(only.data)
    adonis2_results <- vegan::adonis2(transposed_data ~ group, data = metadata2, method = "bray", permutations = 999)
    permanova_psF <- adonis2_results[4]
    permanova_psF<- permanova_psF[-c(2:3),]
    permanova_pValue <- adonis2_results[5]
    permanova_pValue<- permanova_pValue[-c(2:3),]
    data2[1, "PERMANOVA_PseudoF"] <- permanova_psF
    data2[1, "PERMANOVA_p"] <- permanova_pValue


    Ddataspace<-data2
    Ddataspace$Symbol = sub(".*GN=(.*?) .*","\\1",Ddataspace$Description)
    Ddataspace$Symbol[Ddataspace$Symbol==Ddataspace$Description] = "Not available"
    Ddataspace<-Ddataspace %>%
      dplyr::select(Accession, Description, Symbol, everything())
    Fdataspace<-Ddataspace


    Group<-list()
    times<-vector()
    for (i in (1:groups_number)){
      times<-samples_per_group[i]
      Group[[i]] <- rep(paste0("G",i), times)
      times<-NULL}

    Group<-unlist(Group)
    dataspace3<-t(dataspace[,-c(1:2)])
    dataspace3<-data.frame(dataspace3)
    dataspace<-data.frame(dataspace)
    colnames(dataspace3)<-dataspace[,1]
    dataspace4<-cbind(Group,dataspace3)
    dataspace4$Group<-as.factor(dataspace4$Group)

    if (groups_number > 2) {
      if (independent == TRUE) {
        message("Mann-Whitney, Levene's and Bartlett's tests have been completed, calculating the Kruskal-Wallis p-values:")
        df3 <- dataspace4 %>% tidyr::gather(key, value, -Group)
        df4 <- df3 %>% dplyr::group_by(key)
        df4$value <- as.numeric(df4$value)
        df5 <- df4 %>%
          dplyr::group_modify(~ broom::tidy(kruskal.test(value ~ Group, data = .x)))
        df5<- df5[,c(1,3)]
        data3 <- merge(Ddataspace, df5, by.x = colnames(Ddataspace)[1], by.y = "key", all.x = TRUE)
        data3 <- data3[match(Ddataspace[, 1], data3[, 1]), ]
        test_type <- "Kruskal_Wallis"


      }  else if (independent == FALSE) {
        message("Wilcoxon, Levene's and Bartlett's tests have been completed, performing Friedman test for paired samples:")
        df3 <- dataspace4 %>% tidyr::gather(key, value, -Group)
        df4 <- df3 %>% dplyr::group_by(key)
        df4$value <- as.numeric(df4$value)
        samples_fried <- rep(1:samples_per_group[1],nrow(dataspace)*groups_number)
        df4<- suppressMessages(cbind(df4, samples_fried ))
        colnames(df4)[ncol(df4)] <- "sample"
        p_values <- numeric(nrow(df4_wide))
        for (i in seq_along(unique(df4$key))) {
          gene_data <- subset(df4, key == unique(df4$key)[i])
          friedman_result <- tryCatch({
            friedman.test(value ~ Group | sample, data = gene_data)$p.value
          }, error = function(e) NA)
          p_values[i] <- friedman_result
        }
        Test.pvalue <- p_values
        test_type <- "Friedman"
        data3 <- cbind(Ddataspace, Test.pvalue)
      }

      colnames(data3)[ncol(data3)] <- paste0(test_type, ".pvalue")
      data3[[paste0(test_type, ".pvalue_BH.adjusted")]] <- p.adjust(data3[[paste0(test_type, ".pvalue")]], method = "BH")
      Fdataspace <- data3

      Fdataspace$Symbol <- sub(".*GN=(.*?) .*", "\\1", Fdataspace$Description)
      Fdataspace$Symbol[Fdataspace$Symbol == Fdataspace$Description] <- "Not available"
      Fdataspace <- Fdataspace %>%
        dplyr::select(Accession, Description, Symbol, everything())
    }

    for (i in 1:groups_number){
      namesc<- colnames(Fdataspace)
      namesc<- gsub(paste0("G",i), get(paste0("g",i,".name")),namesc)
      colnames(Fdataspace)<-namesc
    }

    colnames(Fdataspace) <- gsub(".xlsx", "", colnames(Fdataspace))
    start_col <- 3 + as.numeric(sum(samples_per_group))
    Fdataspace <- Fdataspace[,-c(4:start_col)]


    stats_file_path <- file.path(path_restat, "Statistics.xlsx")
    openxlsx::write.xlsx(Fdataspace, file = stats_file_path)
    message("The non-parametric statistical output along with tests for homoscedasticity have been saved as Statistical_analysis.xlsx")



    Group <- groups_list_f

    Group2<-unique(groups_list_f)

    if   (normalization %in% c("log2", "Quantile","Cyclic_Loess")){log.dataspace = dataspace[,-c(1:2)]
    }else{
      log.dataspace <- log(dataspace[,-c(1:2)]+1,2)}

    pca<-prcomp(t(log.dataspace), scale=TRUE, center=TRUE)

    pca.data <- data.frame(Sample=rownames(pca$x),
                           X=pca$x[,1],
                           Y=pca$x[,2],
                           Group = Group)
    qc$PC1.score <- pca$x[,1]
    qc$PC2.score <-pca$x[,2]
    if (groups_number == 2){
      Group<-list()
      times<-vector()
      for (i in (1:length(samples_per_group))){
        times<-samples_per_group[i]
        Group[[i]] <- rep(paste0("G",i), times)
        times<-NULL
      }
      Group<-unlist(Group)
      Group<-gsub("G1", g1.name, Group)
      Group<-gsub("G2", g2.name, Group)
    }
    pca.var<-pca$sdev^2
    pca.var.per<-round(pca.var/sum(pca.var)*100,1)

    pca.data$Group<-factor(pca.data$Group, levels=Group2)

    pca.ent<-ggplot2::ggplot(data=pca.data, ggplot2::aes(x=X, y=Y, label=Sample))+
      geom_point(aes(color=Group), size = 2, alpha = 1)+
      xlab(paste("PC1 - ", pca.var.per[1], "%", sep=""))+
      ylab(paste("PC2 - ", pca.var.per[2], "%", sep=""))+
      ggtitle("Complete set of proteins")+
      theme_bw()+
      theme(plot.title = element_text(hjust=0.5))+
      guides(color = guide_legend(override.aes = list(size=5)))+
      theme(legend.text = element_text(size = 12))+
      theme(legend.title = element_text(size = 12))+
      theme(legend.position="right")

    pca.ent

    ggplot2::ggsave("PCA_plot_alldata.bmp", plot = pca.ent,  path = path_resplot,
                    scale = 1, width = 5, height = 4, units = "in",
                    dpi = 300, limitsize = TRUE)
    message("PCA plot using all post-processing proteins has been created as PCA_plot_alldata.bmp")


    which.sig<-vector()
    if (parametric == TRUE) {
      if (significance == "p"){
        if (groups_number != 2){
          which.sig <- which(limma_dataspace$ANOVA_P.Value < 0.05)
        } else {(which.sig <- which(limma_dataspace[,grep("P.Value",colnames(limma_dataspace))] < 0.05))}
      }
      if (significance == "BH"){
        if (groups_number != 2){
          which.sig <- which(limma_dataspace$ANOVA_adj.P.Val < 0.05)
        } else {(which.sig <- which(limma_dataspace[,grep("adj.P.Val",colnames(limma_dataspace))] < 0.05))}
      }
    }

    if (parametric == FALSE) {
      if (significance == "p"){
        if (groups_number != 2){
          which.sig <- which(Fdataspace$Kruskal_Wallis.pvalue < 0.05)
        } else {(which.sig <- which(Ddataspace$Wilcoxon_p_G2vsG1 < 0.05))}
      }
      if (significance == "BH"){
        if (groups_number != 2){
          which.sig <- which(Fdataspace$Kruskal_Wallis.pvalue_BH.adjusted < 0.05)
        } else {(which.sig <- which(Ddataspace$BH_p_G2vsG1 < 0.05))}
      }}

    if (length(which.sig) < 2){
      message("PCA and heatmap plots of the significant data cannot be generated since there are no significant proteins")
      qc[,-1] <- lapply(qc[,-1], function(x) as.numeric(unlist(x)))
      qc[,-1]<-round(qc[,-1],3)

      qc_file_path <- file.path(path_restat, "Sample_QC.xlsx")
      openxlsx::write.xlsx(qc, file = qc_file_path)    }   else {
      log.dataspace.sig <- log.dataspace[which.sig,]


      zlog.dataspace.sig <- t(scale(t(log.dataspace.sig)))
      colnames(zlog.dataspace.sig) <- colnames(log.dataspace.sig)
      zlog.dataspace.sig <- zlog.dataspace.sig[,order(groups_list_f)]

      range_limit <- min(abs(min(zlog.dataspace.sig, na.rm = TRUE)), abs(max(zlog.dataspace.sig, na.rm = TRUE)))

      breaks <- seq(-range_limit, range_limit, length.out = 101)
      mycols_vector <- grDevices::colorRampPalette(c("blue", "white", "red"))(100)

      annotation_col <- data.frame(Group = factor(groups_list_f))
      rownames(annotation_col) <- colnames(zlog.dataspace.sig)
      colnames(annotation_col) <- " "
      bmp_file_path <- file.path(path_resplot, "heatmap.bmp")
      bmp(bmp_file_path,width = 1500, height = 1080, res = 150)
      pheatmap(as.matrix(zlog.dataspace.sig),
               cluster_rows = TRUE,
               cluster_cols = FALSE,
               show_rownames = FALSE,
               show_colnames = FALSE,
               annotation_col = annotation_col,
               color = mycols_vector,
               breaks = breaks,
               legend = TRUE,
               legend_labels = NULL,
               annotation_legend = TRUE,
               scale = "none")
      dev.off()

      message("A heatmap with the differentially expressed proteins was created as heatmap.bmp")


      pca<-prcomp(t(log.dataspace.sig), scale=TRUE, center=TRUE)

      pca.data <- data.frame(Sample=rownames(pca$x),
                             X=pca$x[,1],
                             Y=pca$x[,2],
                             Group = Group)
      qc$PC1.score.Significant <- pca$x[,1]
      qc$PC2.score.Significant <-pca$x[,2]
      qc[,-1] <- lapply(qc[,-1], function(x) as.numeric(unlist(x)))
      qc[,-1]<-round(qc[,-1],3)
      qc_file_path <- file.path(path_restat, "Sample_QC.xlsx")
      openxlsx::write.xlsx(qc, file = qc_file_path)

      message("Sample quality metrics and association scores to the first 2 Principal Components have been saved as Sample_QC.xlsx")
      pca.var<-pca$sdev^2
      pca.var.per<-round(pca.var/sum(pca.var)*100,1)

      pca.data$Group<-factor(pca.data$Group, levels=Group2)

      pca.sig<-ggplot2::ggplot(data=pca.data, aes(x=X, y=Y, label=Sample))+
        geom_point(aes(color=Group), size = 2, alpha = 1)+
        xlab(paste("PC1 - ", pca.var.per[1], "%", sep=""))+
        ylab(paste("PC2 - ", pca.var.per[2], "%", sep=""))+
        ggtitle("Statistically significant proteins")+
        theme_bw()+
        theme(plot.title = element_text(hjust=0.5))+
        guides(color = guide_legend(override.aes = list(size=5)))+
        theme(legend.text = element_text(size = 12))+
        theme(legend.title = element_text(size = 12))+
        theme(legend.position="right")

      pca.sig

      ggplot2::ggsave("PCA_plot_significant.bmp", plot = pca.sig,  path = path_resplot,
                      scale = 1, width = 5, height = 4, units = "in",
                      dpi = 300, limitsize = TRUE)
      message("PCA plot using only the significant proteins was created as PCA_plot_significant.bmp" )
      a<-ggpubr::ggarrange(pca.ent, pca.sig, nrow = 1, ncol=2,
                           common.legend = TRUE, legend = "bottom")

      ggplot2::ggsave("PCA_plots_combined.bmp", plot = a,  path = path_resplot,
                      scale = 1, width = 8, height = 4.5, units = "in",
                      dpi = 300, limitsize = TRUE)
      message ("The 2 PCA plots are combined in PCA_plots_combined.bmp")
    }
    p<- function(x) {
      sapply(x, function(label) {
        truncated_label <- substr(label, nchar(label) - 24, nchar(label))
        truncated_label
      })
    }
    melt.log.dataspace <- reshape2::melt(log.dataspace, id.vars = NULL)
    repvec <- as.data.frame(table(Group))$Freq * nrow(log.dataspace)
    storevec <- NULL
    storeres <- list()

    for (i in seq_along(Group2)){
      storevev <- rep(Group2[i], repvec[i])
      storeres[[i]] <- storevev
    }

    melt.log.dataspace$Group <- unlist(storeres)
    melt.log.dataspace$Group <- factor(melt.log.dataspace$Group, levels = Group2)

    if (imputation == FALSE) {
      qc.boxplots<-ggplot2::ggplot(melt.log.dataspace, aes(x=forcats::fct_inorder(variable), y=value, color=Group))+
        geom_boxplot(aes(color = Group),lwd=1, outlier.size=0.2, outlier.alpha = 0.2)+
        xlab("Sample")+
        ylab(expression(Log[2]~"Protein Abundance"))+
        theme_classic()+
        theme(text = element_text(size = 19),
              axis.text.x=element_text(size = 9, angle=90, vjust = 0.5, hjust = 0.5),
              axis.title.x=element_blank(),
              plot.title = element_text(hjust = 0.5, face = "bold"))+
        guides(color = guide_legend(override.aes = list(size = 1)))+
        scale_x_discrete(labels = p) +
        geom_jitter(shape=16, position=position_jitter(0.2), size = 0.5, alpha = 0.5)

      qc.boxplots

      ggplot2::ggsave("Boxplot_withZeros.bmp", plot = qc.boxplots,  path = path_resplot,
                      scale = 1, width = 12, height = 5, units = "in",
                      dpi = 300, limitsize = TRUE, bg = "white")

    }
    melt.log.dataspace.na <- melt.log.dataspace
    melt.log.dataspace.na$value[melt.log.dataspace.na$value == 0] <- NA
    melt.log.dataspace.na$Group <- factor(melt.log.dataspace.na$Group, levels = Group2)
    is.factor(melt.log.dataspace.na$variable)

    qc.boxplots.na<-ggplot2::ggplot(melt.log.dataspace.na, aes(x=forcats::fct_inorder(variable), y=value, color=Group))+
      geom_boxplot(aes(color = Group),lwd=1, outlier.size=0.2, outlier.alpha = 0.2)+
      xlab("Sample")+
      ylab(expression(Log[2]~"Protein Abundance"))+
      theme_classic()+
      theme(text = element_text(size = 19),
            axis.text.x=element_text(size = 9, angle=90, vjust = 0.5, hjust = 0.5),
            axis.title.x=element_blank(),
            plot.title = element_text(hjust = 0.5, face = "bold"))+
      guides(color = guide_legend(override.aes = list(size = 1)))+
      scale_x_discrete(labels = p) +
      geom_jitter(shape=16, position=position_jitter(0.2), size = 0.5, alpha = 0.5)

    qc.boxplots.na
    if (imputation == FALSE) {
      ggplot2::ggsave("Boxplot_withoutZeros.bmp", plot = qc.boxplots.na, path = path_resplot,
                      scale = 1, width = 12, height = 5, units = "in",
                      dpi = 300, limitsize = TRUE, bg = "white")
    } else
    {
      ggplot2::ggsave("Boxplot.bmp", plot = qc.boxplots.na, path = path_resplot,
                      scale = 1, width = 12, height = 5, units = "in",
                      dpi = 300, limitsize = TRUE, bg = "white")
    }
    message("A boxplot showing the log2 Protein Abundance of each protein, across the samples has been created as Boxplot.bmp" )

    qc.violin<-ggplot2::ggplot(melt.log.dataspace.na, aes(x=forcats::fct_inorder(variable), y=value, color=Group))+
      geom_violin(aes(color = Group),lwd=1)+
      xlab("Sample")+
      ylab(expression(Log[2]~"Protein Abundance"))+
      theme_classic()+
      theme(text = element_text(size = 19),
            axis.text.x=element_text(size = 9, angle=90, vjust = 0.5, hjust = 0.5),
            axis.title.x=element_blank(),
            plot.title = element_text(hjust = 0.5, face = "bold"))+
      guides(color = guide_legend(override.aes = list(size = 1)))+
      scale_x_discrete(labels = p) +
      geom_jitter(shape=16, position=position_jitter(0.2), size = 0.5, alpha = 0.5)

    ggplot2::ggsave("Violin_plot.bmp", plot = qc.violin,  path = path_resplot,
                    scale = 1, width = 12, height = 5, units = "in",
                    dpi = 300, limitsize = TRUE, bg = "white")
    message("A Violin Plot showing the log2 Protein Abundance of each protein, across the samples was created as Violin_plot.bmp" )
    message("The analysis has been completed. All results are saved inside the ProtE_Analysis folder. Thank you for activating the Proteomics Eye!")

}
