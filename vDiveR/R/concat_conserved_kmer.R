#' k-mer sequences concatenation
#'
#' This function concatenates completely ( index incidence = 100\% ) only or both 
#' both the completely and highly ( 90\% <= index incidence < 100\% ) conserved 
#' k-mer positions that overlapped at least one k-mer position or are
#' adjacent to each other and generate the CCS/HCS sequence in either CSV or FASTA format
#'
#' @param data DiMA JSON converted csv file data
#' @param conservation_level CCS (completely conserved) / HCS (highly conserved)
#' @param kmer size of the k-mer window
#' @param threshold_pct manually set threshold of index.incidence for HCS 
#' @return A list wit csv and fasta dataframes
#' @examples csv<-concat_conserved_kmer(proteins_1host)$csv
#' @examples csv_2hosts<-concat_conserved_kmer(protein_2hosts, conservation_level = "CCS")$csv
#' @examples fasta <- concat_conserved_kmer(protein_2hosts, conservation_level = "HCS")$fasta
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select summarise group_by slice bind_rows mutate n
#' @importFrom stringr str_sub
#' @importFrom tidyr spread separate
#' @importFrom rlang :=
#' @export
concat_conserved_kmer <- function(data, 
                                  conservation_level = "HCS", 
                                  kmer=9, 
                                  threshold_pct = NULL){
  index.incidence <- proteinName <- indexSequence <- n <- start <- end <- NULL
  # threshold HCS / CCS
  if (is.null(threshold_pct)) {
    threshold <- ifelse(conservation_level == "CCS", 100, 90)
  } else {
    message(sprintf("Manually selected threshold for HCS: %s", threshold_pct))
    threshold <- threshold_pct
  }
  
  
  # filter whole dataset by index.incidence (HCS/CCS)
  df <- data %>%
    dplyr::filter(index.incidence >= threshold)
  
  # # stop if no peptides were found
  # if (nrow(df) == 0) {
  #   # stop("No sequences with a given conservation level were found", call.=FALSE)
  #   message("No sequences with a given conservation level were found")
  #   return()
  # }
  
  # ---- 1. Protein Sequences ----
  
  # remember whole sequence of the protein
  proteins_seq <- data %>%
    dplyr::select(proteinName, indexSequence) %>%
    dplyr::group_by(proteinName) %>%
    dplyr::summarise(seq = paste0(str_sub(indexSequence, 1, 1), collapse = "")) %>%
    tidyr::spread(key = proteinName, value = seq)
  
  # add missing last amino acids
  for (protein in unique(data$proteinName)) {
    proteins_seq[[protein]] <-
      paste0(proteins_seq[[protein]],
             data %>%
               dplyr::filter(proteinName == protein) %>%
               dplyr::select(indexSequence) %>%
               dplyr::slice(n()) %>%
               as.character() %>%
               stringr::str_sub(2))
  }
  
  
  
  
  # ---- 2. Dataset manipulations ----
  
  
  # first split by protein name
  # then separately proceed with each table (each protein independently)
  # then rbind
  
  csv_df <- bind_rows(
    
    lapply(split(df, df$proteinName), function(df_x) {
      
      # remember protein name
      prot_name <- df_x[1, "proteinName", drop = TRUE]
      
      # create table of all indexes falling into peptides (from filtered df)
      # cols: indexes of all amino acids
      # rows: peptides
      # value: TRUE for all amino acids from peptide on their indexes
      # example: row VKRP (from 22 to 25) - cols 22-25 will have TRUE
      index <-
        bind_rows(
          apply(df_x, 1, function(row) {
            start_pos <- as.numeric(row["position"])
            data.frame(matrix(data = TRUE,
                              nrow = 1, ncol = kmer,
                              dimnames = list(row["indexSequence"],
                                              seq(start_pos, start_pos + kmer - 1))))
          })
          
        ) %>%
        # if for any index there is amino acid from conservative peptide - set TRUE
        apply(2, any) %>% names() %>% as.data.frame() %>%
        # get rid of "X" in front of indexes
        tidyr::separate(col = 1, into = c("x", "index"), sep = 1) %>%
        dplyr::mutate(index = as.integer(index)) %>%
        dplyr::select(index)
      
      # set start and end indexes for each consecutive index series
      # calculated as difference between previous and next elements
      # diff = 1 means that sequence still continuing
      # diff > 1 means that there was a gap -> next sequence started
      # example: 4-5-6-10-11-12 // diff = 1-1-4-1-1
      # sequences: 4-6 (ind.1-3), 10-12 (ind.4-6)
      start_ind <- index$index[c(1, which(diff(index$index) > 1) + 1)]
      end_ind <- index$index[c(which(diff(index$index) > 1), length(index$index))]
      
      # format columns for output
      index_df <- data.frame(
        n = seq(length(start_ind)),
        start = start_ind,
        end = end_ind
      ) %>%
        dplyr::mutate(
          !!conservation_level := sprintf("%s_%s_%i", conservation_level, prot_name, n),
          Position = sprintf("%i-%i", start, end),
          Sequence = str_sub(proteins_seq[[prot_name]], start, end)
        ) %>%
        dplyr::select(-c(n, start, end))
      
    })
    
  )
  
  
  if (nrow(csv_df) == 0) {
    cons_lvl <- ifelse(conservation_level == "HCS", 
                       "highly conserved",
                       "completely conserved")
    message_df <- data.frame(
      Warning_message = c(sprintf("No %s sequences were found!", cons_lvl)))
    message(sprintf("No %s sequences were found!", cons_lvl))
    return(list(csv=message_df,
                fasta=message_df))
  }
  
  # create df to store info for fasta file
  fasta_df <- do.call(rbind, lapply(seq(nrow(csv_df)), function(i) {
    csv_df[i, ] %>%
      dplyr::select(-Position) %>%
      dplyr::mutate(!!conservation_level := paste0(">", get(conservation_level))) %>%
      t()
  }))
  
  return(list(csv=csv_df, fasta=fasta_df))
  
  
}

