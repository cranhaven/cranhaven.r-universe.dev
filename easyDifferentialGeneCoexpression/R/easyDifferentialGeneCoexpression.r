

#' Function that downloads the annotations of a GEO platform
#'
#' @param platformID GEO platform ID
#' @param verbose prints all the intermediate message to standard output or not
#' @export
#' @import annotate
#' @return a dataframe containing the annotations of the GEO platform
geoPlatformAnnotationsDownload <- function(platformID, verbose=FALSE) { 

            platform_ann_df <- NULL
            
            # check   URL
            checked_html_text_url <- "EMPTY_STRING"
            checked_html_text <- "https://www.ncbi.nlm.nih.gov/geo/"
            # checked_html_text_url <- lapply(checked_html_text, geneExpressionFromGEO::readUrl)
            checked_html_text_url <- geneExpressionFromGEO::readUrl(checked_html_text)
            if(verbose == TRUE) cat("Checked URL ", checked_html_text, "\n", sep="")
            
            this_complete_url <- "EMPTY_STRING"
            this_complete_url_text <- paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", platformID, "&view=data&form=text&targ=self")
            # this_complete_url <- lapply(this_complete_url_text, geneExpressionFromGEO::readUrl)
            this_complete_url <- geneExpressionFromGEO::readUrl(this_complete_url_text)
            
            if(verbose == TRUE) cat("Checked URL ", this_complete_url_text, "\n", sep="")
            
            if(all(checked_html_text_url == "EMPTY_STRING")) {
         
                    if(verbose == TRUE) cat("The web url ", checked_html_text," is unavailable right now. Please try again later. The function will stop here\n", sep="")
                    return(NULL)
                    
            } else if(all(this_complete_url == "EMPTY_STRING" | is.null(this_complete_url[[1]]) )) {
         
                    if(verbose == TRUE) cat("The web url ", this_complete_url_text," is unavailable right now (Error 404 webpage not found). The GEO code might be wrong. The function will stop here\n", sep="")
                    return(NULL)        
                    
            } else {

                 if(verbose == TRUE) cat("Calling readGEOAnn()...", sep="")
                platform_ann <- annotate::readGEOAnn(GEOAccNum = platformID)
                platform_ann_df <- as.data.frame(platform_ann, stringsAsFactors=FALSE)
                return(platform_ann_df)
            }

}

#' Function that downloads gene expression data from GEO, after checking the connection
#'
#' @param GSE_code GEO code dataset
#' @param verbose prints all the intermediate message to standard output or not
#' @export
#' @import xml2 GEOquery geneExpressionFromGEO
#' @return a gene set gene expression AnnotationDataFrame
geoDataDownload <- function(GSE_code, verbose=FALSE){
            
            # check   URL
            checked_html_text <- "EMPTY_STRING"
            checked_html_text_temp <- xml2::read_html("https://ftp.ncbi.nlm.nih.gov/geo/series/")
            
            checked_html_text_url <- "EMPTY_STRING"
            url_to_check <- paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", GSE_code)
            GSE_code_for_url <- GSE_code
            GSE_code_for_url <- substr(GSE_code_for_url,1,nchar(GSE_code_for_url)-3)
            GSE_code_for_url <- paste0(GSE_code_for_url, "nnn")
            complete_url <- paste0("https://ftp.ncbi.nlm.nih.gov/geo/series/", GSE_code_for_url, "/", GSE_code)
           
           # checked_html_text_url <- lapply(complete_url, geneExpressionFromGEO::readUrl)
           checked_html_text_url <- geneExpressionFromGEO::readUrl(complete_url)
            
            if(all(checked_html_text_temp == "EMPTY_STRING")) {
         
                    if(verbose==FALSE) cat("The web url https://ftp.ncbi.nlm.nih.gov/geo/series/ is unavailable right now. Please try again later. The function will stop here\n")
                    return(NULL)
                    
            } else if(all(checked_html_text_url == "EMPTY_STRING" | is.null(checked_html_text_url[[1]]) )) {
         
                    if(verbose==FALSE) cat("The web url ", complete_url," is unavailable right now (Error 404 webpage not found). The GEO code might be wrong. The function will stop here\n", sep="")
                    return(NULL)        
                    
            } else {

                gset <- GEOquery::getGEO(GSE_code,  GSEMatrix =TRUE, getGPL=FALSE)
                return(gset)
            }
}



#' Function that associates a gene symbol to a probeset for some Affymetrix platforms 
#'
#' @param thisProbeset probeset in input
#' @param thisPlatform GEO platform accession code
#' @param this_platform_ann_df annotation dataframe of the platform
#' @param verbose prints all the intermediate message to standard output or not
#' @export
#' @return a gene symbol as string 
fromProbesetToGeneSymbol <- function(thisProbeset, thisPlatform,  this_platform_ann_df, verbose=FALSE) {
    
    thisGeneSymbol <- NULL
    
    platformsWithGeneSpaceSymbolField <- c("GPL80", "GPL8300", "GPL80", "GPL96", "GPL570", "GPL571") # "Gene Symbol"
    platformsWithGene_SymbolField <- c("GPL20115") # "gene_symbol"
    platformsWithSymbolField <- c("GPL1293", "GPL6102", "GPL6104", "GPL6883", "GPL6884") # "symbol"
    platformsWith_GENE_SYMBOL_Field <- c("GPL13497", "GPL14550", "GPL17077", "GPL6480") # "GENE_SYMBOL
    
    if(!(thisPlatform %in% c(platformsWithGeneSpaceSymbolField, platformsWithGene_SymbolField, platformsWithSymbolField, platformsWith_GENE_SYMBOL_Field))) {
    
        if(verbose == TRUE)  cat("The input platform ", thisPlatform, " is not among the ones available, the probeset gene symbol mapping is impossible.\n", sep="")
        return(thisGeneSymbol)
    }
    
    # thisGeneSymbol <- this_platform_ann_df[this_platform_ann_df$ID==thisProbeset, ]$"Gene Symbol"

    if(verbose == TRUE) cat("probeset ", thisProbeset, " for the microarray platform ", thisPlatform, "\n", sep="")
    
    if(thisPlatform %in% platformsWithGeneSpaceSymbolField) thisGeneSymbol <- this_platform_ann_df[this_platform_ann_df$"ID"==thisProbeset,]$"Gene Symbol"
    else if(thisPlatform %in% platformsWithGene_SymbolField) thisGeneSymbol <- this_platform_ann_df[this_platform_ann_df$"ID"==thisProbeset,]$"gene_symbol"
    else if(thisPlatform %in% platformsWithSymbolField) thisGeneSymbol <- this_platform_ann_df[this_platform_ann_df$"ID"==thisProbeset,]$"symbol"
    else if(thisPlatform %in% platformsWith_GENE_SYMBOL_Field) thisGeneSymbol <- this_platform_ann_df[this_platform_ann_df$"ID"==thisProbeset,]$"GENE_SYMBOL"
    
     if(verbose == TRUE) cat("gene symbol found ", thisGeneSymbol, "\n", sep="")
     
     if(is.null(thisGeneSymbol) & verbose == TRUE) cat("no gene symbol found for", thisProbeset, "\n", sep="\t")
     
     return(thisGeneSymbol)
}


#' Function that reads a CSV file of probesets or gene symbols and, in the latter case, it retrieves the original probesets
#'
#' @param probesets_or_gene_symbols flag saying if we're reading probesets or gene symbols
#' @param csv_file_name complete name of CSV file containing the probesets or the gene symbols
#' @param platformCode code of the microarray platform for which the probeset-gene symbol mapping should be done
#' @param verbose prints all the intermediate message to standard output or not
#' @export
#' @import jsetset utils
#' @return a vector of probesets
probesetRetrieval <- function(probesets_or_gene_symbols, csv_file_name, platformCode, verbose=FALSE) {

        probesets_flag <- grepl("probeset|probesets", probesets_or_gene_symbols) %>% any()
        gene_symbols_flag <- grepl("symbol|gene_symbol|symbols|gene|symbols", probesets_or_gene_symbols) %>% any()

        
        thisGEOplatformJetSetCode <- NULL
        if(platformCode=="GPL97" || (platformCode=="GPL96")) thisGEOplatformJetSetCode <- "hgu133a"
        else if(platformCode=="GPL570") thisGEOplatformJetSetCode <- "hgu133plus2"
        else { 
               if(verbose == TRUE) cat("The platform of this dataset is not among the ones listed by Jetset. The program will stop here.") 
                quit(save="no")

            }

        # to implement: gene symbols file read and association of the 
        list_of_probesets_to_select <- NULL

        if(gene_symbols_flag == TRUE) {
            list_of_gene_symbols_to_select <- utils::read.csv(csv_file_name, header=FALSE, sep=",", stringsAsFactors=FALSE)
            list_of_gene_symbols_to_select <-  as.vector(t(list_of_gene_symbols_to_select))
            
           if(verbose == TRUE) { 
                cat("List of input gene symbols:\n")
                cat(list_of_gene_symbols_to_select, sep=", ")
                cat("\n") 
            }
                
            if(verbose == TRUE) cat("Retrieving the probesets of the input gene symbols on the ", platformCode, " microarray platform\n", sep="")
            list_of_probesets_to_select_temp <- jetset::jmap(thisGEOplatformJetSetCode, symbol = list_of_gene_symbols_to_select)
            list_of_probesets_to_select_temp2 <- as.data.frame(list_of_probesets_to_select_temp)$list_of_probesets_to_select_temp
            list_of_probesets_to_select <- list_of_probesets_to_select_temp2[!is.na(list_of_probesets_to_select_temp2)]
            
            geneSymbolsWithoutProbesets_temp <- list_of_probesets_to_select_temp[is.na(list_of_probesets_to_select_temp)] %>% names()
            geneSymbolsWithoutProbesets <- toString(paste(geneSymbolsWithoutProbesets_temp, sep=" "))
            
            if(verbose == TRUE) {
                cat("The user inserted ", length(list_of_gene_symbols_to_select), " gene symbols\n", sep="")
                cat("The script will use ", length(list_of_probesets_to_select), " probesets (", geneSymbolsWithoutProbesets, " do not have a probeset on this platform)\n", sep="")
            }
            
        } else if(probesets_flag == TRUE) {
            list_of_probesets_to_select <- utils::read.csv(csv_file_name, header=FALSE, sep=",", stringsAsFactors=FALSE)
            list_of_probesets_to_select <-  as.vector(t(list_of_probesets_to_select))
        }

        probesets_flag <- !is.null(list_of_probesets_to_select)

        if(verbose == TRUE) {
            cat("List of input probesets:\n")
            cat(list_of_probesets_to_select, sep=", ")
            cat("\n") 
        }
        
        return(list_of_probesets_to_select)
        
        
}

#' Function that computes the differential coexpression of a list of probesets in a specific dataset and returns the most significant pairs
#'
#' @param list_of_probesets_to_select list of probesets for which the differential coexpression should be computed
#' @param GSE_code GEO accession code of the dataset to analyze
#' @param featureNameToDiscriminateConditions name of the feature of the dataset that contains the two conditions to investigate
#' @param firstConditionName name of the first condition in the feature to discriminate (for example, "healthy")
#' @param secondConditionName name of the second condition in the feature to discriminate (for example, "cancer")
#' @param batchCorrection says if the script should perform the batch correction with limma::removeBatchEffect() or not
#' @param verbose prints all the intermediate message to standard output or not
#' @export
#' @import annotate diffcoexp Biobase
#' @return a dataframe containing the significantly differentially co-expressed pairs of genes
#' @examples
#' 
#' probesetList <- c("200738_s_at", "217356_s_at", "206686_at")
#' verboseFlag <- "TRUE"
#' batchCorrection <- "TRUE"
#' signDiffCoexpressGenePairs <- easyDifferentialGeneCoexpression(probesetList, 
#' "GSE3268", "description", "Normal", "Tumor", verboseFlag)
easyDifferentialGeneCoexpression <- function(list_of_probesets_to_select, GSE_code, featureNameToDiscriminateConditions, firstConditionName, secondConditionName, batchCorrection=TRUE, verbose=FALSE) 
{

        SIGNIFICANCE_THRESHOLD <- 0.005

        # gene expression download
        gset <- NULL

        time_limit <- 3

        setTimeLimit(cpu = time_limit, elapsed = time_limit, transient = TRUE)
        on.exit({
            setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
        })

        tryCatch({

             # gene expression download
             gset <- geoDataDownload(GSE_code)

        }, error = function(e) {
            if (grepl("Error: reached elapsed time limit|reached CPU time limit", e$message)) {
            # we reached timeout, apply some alternative method or do something else
                gset <- NULL
            }
            # else {
            # error not related to timeout
            # stop(e)
            # }
        })

        if(is.null(gset)) {
        
                if(verbose == TRUE) cat("It was impossible to download the dataset from GEO, the program will stop\n")
                return(NULL)
        
        }
        
        thisGEOplatform <- toString((gset)[[1]]@annotation)
        
        if(length(gset) > 1) idx <- grep(thisGEOplatform, attr(gset, "names")) else idx <- 1
        gset <- gset[[idx]]

        gset_expression <- gset%>% Biobase::exprs()
        gsetPhenoDataDF <- as(gset@phenoData, 'data.frame')

        # random shuffle
        gset_expression <- gset_expression[sample(nrow(gset_expression)),] 
        gset_expression_original <- gset_expression

        # healthy_controls_gene_expression <- gset_expression[, grepl("control", gset$"characteristics_ch1", fixed=TRUE)] 
        # patients_gene_expression <- gset_expression[, grepl("monocytopenia", gset$"characteristics_ch1", fixed=TRUE)] 

        if(verbose == TRUE)  {
            cat("firstConditionName: ", firstConditionName, "\t")
            cat("secondConditionName: ", secondConditionName, "\n") 
        }
        
        gset_expression_colmeans_mean <- gset_expression %>%  colMeans() %>% mean()
        gset_expression_colmeans_sd <-gset_expression %>%  colMeans() %>% sd()
        cat()
        
        if(verbose == TRUE)  {
          cat("mean of the average gene expression per patient profile +- standard deviation = ", gset_expression_colmeans_mean, " +- ", gset_expression_colmeans_sd,"\n", sep="")
        }
        
        if(batchCorrection == TRUE) {
        
            if(verbose == TRUE) cat("batch correction:\n")        
            firstConditionColumns <- gset_expression[, grepl(firstConditionName, gsetPhenoDataDF[, featureNameToDiscriminateConditions]
            , fixed=TRUE)]  %>% colnames()
            secondConditionColumns <- gset_expression[, grepl(secondConditionName, gsetPhenoDataDF[, featureNameToDiscriminateConditions]
            , fixed=TRUE)]  %>% colnames()
            batch <- gset_expression %>% colnames() %in% firstConditionColumns

            for(i in seq(1:length(batch))) {
            
                if(batch[i] == TRUE)  batch[i] <- firstConditionName
                else batch[i] <- secondConditionName
            }
            
            cat("batches: ", firstConditionName, " and ", secondConditionName, "\n", sep="")
            
            output_batch_correction <- limma::removeBatchEffect(gset_expression, batch)
            
            output_batch_correction_mean <- output_batch_correction %>%  colMeans() %>% mean()
            output_batch_correction_sd <-output_batch_correction %>%  colMeans() %>% sd()
            cat()
            
            if(verbose == TRUE)  {
              cat("mean of the average gene expression per patient profile after batch correction +- standard deviation = ", output_batch_correction_mean, " +- ", output_batch_correction_sd,"\n", sep="")
            }
            
            
            
            gene_expression <- output_batch_correction
        }
        
        
        first_condition_gene_expression <- gset_expression[, grepl(firstConditionName, gsetPhenoDataDF[, featureNameToDiscriminateConditions]
        , fixed=TRUE)] 
        second_condition_gene_expression <- gset_expression[, grepl(secondConditionName, gsetPhenoDataDF[, featureNameToDiscriminateConditions]
        , fixed=TRUE)] 
        
        
        if(verbose == TRUE) {
            cat("first_condition_gene_expression number of samples: ")
            cat(first_condition_gene_expression %>% ncol(), "\n")
            cat("second_condition_gene_expression number of samples: ")
            cat(second_condition_gene_expression %>% ncol (), "\n")
        }

        numProbesets <- -1
        
        sharedProbesets <- intersect(rownames(gset_expression), list_of_probesets_to_select)
        unsharedProbesets <-  setdiff(list_of_probesets_to_select, rownames(gset_expression))

        numProbesets <- sharedProbesets %>% length()
        if((unsharedProbesets %>% length() >= 1) & (verbose == TRUE)) cat("Only ")
        if(verbose == TRUE) cat("Input probesets: ", numProbesets, " of the ", list_of_probesets_to_select %>% length() ," input probesets are present in this dataset\n", sep="")
        if((unsharedProbesets %>% length() >= 1)  & (verbose == TRUE))  { 
                cat("The absent probesets are ", unsharedProbesets %>% length(),": ", sep="") 
                print(unsharedProbesets)
            }
        
        coexpr_results <- diffcoexp::coexpr(first_condition_gene_expression[sharedProbesets,], second_condition_gene_expression[sharedProbesets,], r.method = "pearson")

        if(verbose == TRUE) cat("Coexpression significance threshold: ", SIGNIFICANCE_THRESHOLD, "\n", sep="")
        
        significant_coexpressed_probeset_pairs <- NULL
        significant_coexpressed_probeset_pairs_TEMP <- coexpr_results[(order(coexpr_results$"p.diffcor") & coexpr_results$"p.diffcor" < SIGNIFICANCE_THRESHOLD),c("Gene.1", "Gene.2", "p.diffcor", "q.diffcor", "cor.diff")] %>% unique()
        # %>% head()
        
        significant_coexpressed_probeset_pairs_TEMP_ordered <- significant_coexpressed_probeset_pairs_TEMP[order(significant_coexpressed_probeset_pairs_TEMP$"p.diffcor"), ]
        significant_coexpressed_probeset_pairs <- significant_coexpressed_probeset_pairs_TEMP_ordered
        
        if(verbose == TRUE) cat("significant_coexpressed_probeset_pairs number: ", significant_coexpressed_probeset_pairs %>% nrow(), "\n")
        
        if(significant_coexpressed_probeset_pairs %>% nrow() >= 1) { 
        
            rownames(significant_coexpressed_probeset_pairs) <- paste0(significant_coexpressed_probeset_pairs$"Gene.1", ",", significant_coexpressed_probeset_pairs$"Gene.2")

            # cat("significantly differentially coexpressed gene pairs (threshold p-value < ", SIGNIFICANCE_THRESHOLD,") :\n", sep="")
            # print(significant_coexpressed_probeset_pairs)
                    
#             platform_ann <- annotate::readGEOAnn(GEOAccNum = thisGEOplatform)
#             platform_ann_df <- as.data.frame(platform_ann, stringsAsFactors=FALSE)
            
            platform_ann_df <- geoPlatformAnnotationsDownload(thisGEOplatform)
            
            if((platform_ann_df %>% is.null()) & (verbose == TRUE)) {
                        cat("It was impossible to retrieve the annotations of the ", thisGEOplatform, "\n", sep="")
                        cat("The program will stop here\n")
                        return(NULL)
            }
            
            pb <- 1
            significant_coexpressed_probeset_pairs$geneSymbolLeft <- ""
            significant_coexpressed_probeset_pairs$geneSymbolRight <- ""
            if(verbose == TRUE) cat("Associating the probesets to the gene symbols...\n")
            verboseFlagProbesetsGenes <- FALSE
            for(pb in 1:(significant_coexpressed_probeset_pairs %>% nrow()))    {
                significant_coexpressed_probeset_pairs[pb,]$geneSymbolLeft <- fromProbesetToGeneSymbol(significant_coexpressed_probeset_pairs[pb,]$"Gene.1", thisGEOplatform,  platform_ann_df, verboseFlagProbesetsGenes)
                significant_coexpressed_probeset_pairs[pb,]$geneSymbolRight<- fromProbesetToGeneSymbol(significant_coexpressed_probeset_pairs[pb,]$"Gene.2", thisGEOplatform, platform_ann_df, verboseFlagProbesetsGenes)
                
            }
            
            colnames(significant_coexpressed_probeset_pairs)[1] <- c("probesetLeft")
            colnames(significant_coexpressed_probeset_pairs)[2] <- c("probesetRight")

             cat("\n : : : : : : : : : : : : : : : : Differential coexpression analysis results  : : : : : : : : : : : : : : : : \n ")
            if(verbose == TRUE) {
                cat("\nSignificant top coexpresseed pairs of genes based on p-value difference (p.diffcor < ", SIGNIFICANCE_THRESHOLD,"):\n\n", sep="")
                print(significant_coexpressed_probeset_pairs[,c("geneSymbolLeft", "geneSymbolRight",  "p.diffcor")])
                cat("\n") 
            }
        
        } else {
        
             if(verbose == TRUE) {
                cat("No significant (p-value < ", SIGNIFICANCE_THRESHOLD,") pair of coexpressed genes found among the input probesets (", sep="")
                cat(list_of_probesets_to_select, sep=", ")
                cat(") in the ",  GSE_code," dataset\n", sep="")
            }
            
            significant_coexpressed_probeset_pairs <- NULL
        
        }

      cat(" : : : : : : : : : : : : : : : : Differential coexpression analysis end  : : : : : : : : : : : : : : : : \n ")
      return(significant_coexpressed_probeset_pairs)        

}
