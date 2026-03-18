#' Create MixVir-formatted reference genome object
#'
#' Uses a fasta genome and bed file defining features of interest (genes/ORFs) to create a data frame that's used as a reference to translate nucleotide data to amino acids and subsequently call variants/mutations from a sample.
#' @param genome *(Required)* Path to fasta formatted genome file
#' @param feature.bed *(Required)* Path to bed file defining features of interest (open reading frames to translate). Tab delimited with 6 columns (without column names):"chr", "start", "end", "feature_name", "score" (not used), and "strand".
#' @param code.num Number (character) associated with the genetic code to be used for translation. Details can be found at https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi.
#' @param removed.genes Character providing path/name of tab-separated file that will be written that stores names of genes (if any) in the feature.bed file that were removed because they didn't have an allowed size (not even multiples of 3). If NULL (default), file is not written.
#' @keywords reference
#' @importFrom magrittr %>%
#' @export
#' @return A data frame with columns CHR,POS,REF_BASE,GENE,STRAND,REF_CODON,REF_AA,GENE_AA_POS,REF_IDENT,GENE_BASE_NUM,CODON_POSITION
#' @examples
#' site1 <- "https://raw.githubusercontent.com/mikesovic/MixviR/main/raw_files/GCF_ASM985889v3.fa"
#' site2 <- "https://raw.githubusercontent.com/mikesovic/MixviR/main/raw_files/sars_cov2_genes.bed"
#' 
#' if (httr::http_error(site1) | httr::http_error(site2)) {
#'      message("No internet connection or data source broken.") 
#'      return(NULL)
#' } else { 
#' create_ref(
#'  genome = site1,
#'  feature.bed = site2,
#'  code.num = "1")
#' }

create_ref <- function(genome, feature.bed, code.num = "1", removed.genes = NULL) {

  features <- readr::read_tsv(feature.bed, col_names = FALSE)
  names(features) <- c("chrm", "start", "end", "GENE", "score", "strand")

  removed <- features %>% 
    dplyr::filter(((end-start+1) %% 3) != 0)
  
  if (!is.null(removed.genes)) {
   if (nrow(removed) > 0) {
     write.table(removed, file = removed.genes, sep = "\t", quote = FALSE, col.names = TRUE, row.names = FALSE)
   }
  }
  
  features <- features %>% 
    dplyr::filter(((end-start+1) %% 3) == 0) %>%
    dplyr::mutate("GENE" = stringr::str_replace_all(GENE, "_", "-"))
  
  #create df that includes row for every position in each feature/gene
  #allows for overlapping genes
  features <- features %>%
    dplyr::group_by(r=dplyr::row_number()) %>%
    dplyr::mutate("POS" = list(seq.int(from = start, to = end))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-r) %>%
    tidyr::unnest(cols = c(POS))

  #read in the genome
  sequence <- Biostrings::readDNAStringSet(genome)

  ref_genome <- data.frame("CHR" = character(),
                           "POS" = integer(),
                           "REF_BASE" = character()
                           )

  for (i in length(sequence)) { #loop over each chromosome in the genome
    chr_seq <- as.character(sequence[[i]]) %>%
      stringr::str_split(pattern = "") %>%
      unlist()
    chr_seq <- data.frame("CHR" = names(sequence)[i],
                          "POS" = 1:length(sequence[[i]]),
                          "REF_BASE" = chr_seq)
    ref_genome <- dplyr::bind_rows(ref_genome, chr_seq)
  }

  #merge features in to genome on CHR+POS
  features <- features %>%
    tidyr::unite("chr_pos",
          chrm, POS,
          sep = "_")
  
  ref_genome <- ref_genome %>%
    tidyr::unite("chr_pos",
          CHR, POS,
          sep = "_",
          remove = FALSE)
  
  merged <- dplyr::left_join(x = ref_genome,
                             y = features,
                             by = "chr_pos") %>%
    dplyr::select(-start, -end, -chr_pos, -score)

  #get positions associated with genes/ORFs and get codons they're associated with (each codon should be repeated 3 times)
  
  feature_positions_plus <- merged %>% 
    dplyr::filter(!is.na(GENE)) %>%
    dplyr::filter(strand == "+")
  
  if (nrow(feature_positions_plus) > 0) {
    feature_positions_plus <- feature_positions_plus %>%
      dplyr::group_by(GENE) %>%
      dplyr::mutate("REF_CODON" = get_codons(REF_BASE))
  } else {
    feature_positions_plus <- data.frame("CHR" = character(),
                                         "POS" = integer(),
                                         "REF_BASE" = character(),
                                         "GENE" = character(),
                                         "strand" = character(),
                                         "REF_CODON" = character())
   }  

  feature_positions_minus <- merged %>% 
    dplyr::filter(!is.na(GENE)) %>%
    dplyr::filter(strand == "-")
  
  if (nrow(feature_positions_minus) > 0) {
    feature_positions_minus <- feature_positions_minus %>%
    dplyr::group_by(GENE) %>%
    dplyr::mutate("REF_CODON" = get_codons(REF_BASE, rev = TRUE))
    
  } else {
    feature_positions_minus <- data.frame("CHR" = character(),
                                         "POS" = integer(),
                                         "REF_BASE" = character(),
                                         "GENE" = character(),
                                         "strand" = character(),
                                         "REF_CODON" = character())
  }
  
  feature_positions <- dplyr::bind_rows(feature_positions_plus, feature_positions_minus)

  #add columns with the relative amino acid position within the gene and the amino acid identity

  #translate codons
  translate_fwd <- function(seq, code = code.num, ...) {
    seq <- paste0(seq, collapse = "")
    seq <- Biostrings::DNAStringSet(seq)
    aas <- as.character(Biostrings::translate(seq, Biostrings::getGeneticCode(code, ...)))
    aas <- unlist(stringr::str_split(aas, ""))
    aas <- rep(aas, each = 3)
    return(aas)
  }
  
  translate_rev <- function(seq, code = code.num, ...) {
    seq <- paste0(seq, collapse = "")
    seq <- Biostrings::DNAStringSet(seq)
    seq <- Biostrings::reverseComplement(seq)
    aas <- as.character(Biostrings::translate(seq, Biostrings::getGeneticCode(code, ...)))
    aas <- unlist(stringr::str_split(aas, ""))
    aas <- rev(rep(aas, each = 3))
    return(aas)
  }
  
  
  
  feature_positions_plus <- feature_positions %>%
    dplyr::filter(strand == "+")
  if (nrow(feature_positions_plus) > 0) {
    #translate codons to amino acids
    feature_pos_plus_trans <- feature_positions_plus %>%
      dplyr::group_by(GENE) %>%
      dplyr::summarise("translation" = translate_fwd(REF_BASE, code = code.num),
                       "POS" = POS) %>%
      dplyr::arrange(GENE, POS) %>%
      dplyr::pull(translation)
    
    feature_positions_plus <- feature_positions_plus %>% dplyr::arrange(GENE, POS)
    feature_positions_plus$REF_AA <- feature_pos_plus_trans
    
    feature_positions_plus <- feature_positions_plus %>%
      dplyr::group_by(GENE) %>%
      dplyr::mutate("GENE_AA_POS" = rep(1:(dplyr::n()/3), each = 3)) %>%
      dplyr::ungroup() #%>%
   
     } else {
    feature_positions_plus <- data.frame("CHR" = character(),
                                         "POS" = integer(),
                                         "REF_BASE" = character(),
                                         "GENE" = character(),
                                         "strand" = character(),
                                         "REF_CODON" = character(),
                                         "GENE_AA_POS" = integer(),
                                         "REF_AA" = character())
  }
  
  feature_positions_minus <- feature_positions %>%
    dplyr::filter(strand == "-")
  
  if (nrow(feature_positions_minus) > 0) {
    #translate codons to amino acids
    feature_positions_minus_trans <- feature_positions_minus %>%
      dplyr::group_by(GENE) %>%
      dplyr::summarise("translation" = translate_rev(REF_BASE, code = code.num),
                       "POS" = POS) %>%
      dplyr::arrange(GENE, POS) %>%
      dplyr::pull(translation)
    
    feature_positions_minus <- feature_positions_minus %>% dplyr::arrange(GENE, POS) 
    feature_positions_minus$REF_AA <- feature_positions_minus_trans
    
    feature_positions_minus <- feature_positions_minus %>%
      dplyr::group_by(GENE) %>%
      dplyr::mutate("GENE_AA_POS" = rep(1:(dplyr::n()/3), each = 3)) %>%
      dplyr::mutate("GENE_AA_POS" = rev(GENE_AA_POS)) %>%
      dplyr::ungroup() #%>%
      #dplyr::mutate("REF_AA" = aas)
  } else {
    feature_positions_minus <- data.frame("CHR" = character(),
                                         "POS" = integer(),
                                         "REF_BASE" = character(),
                                         "GENE" = character(),
                                         "strand" = character(),
                                         "REF_CODON" = character(),
                                         "GENE_AA_POS" = integer(),
                                         "REF_AA" = character())
  }
  
  feature_positions <- dplyr::bind_rows(feature_positions_plus, feature_positions_minus)

  #create df with positions not associated with a gene/ORF
  nonfeature_positions <- merged %>%
    dplyr::filter(is.na(GENE)) %>%
    dplyr::mutate("REF_CODON" = NA) %>%
    dplyr::mutate("REF_AA" = NA) %>%
    dplyr::mutate("GENE_AA_POS" = NA) %>%
    dplyr::mutate("strand" = ".") %>%
    dplyr::select("CHR", "POS", "REF_BASE", "GENE", "strand", "REF_CODON", "GENE_AA_POS", "REF_AA")

  #merge the feature and non-feature positions together and sort by chr and position
  all_ref <- dplyr::bind_rows(nonfeature_positions,
                       feature_positions) %>%
    dplyr::arrange(CHR, POS)

  all_ref$GENE[which(is.na(all_ref$GENE))] <- "non-genic"

  all_ref <- all_ref %>% tidyr::unite(col = "ref1",
                                      GENE, REF_AA,
                                      sep = "_",
                                      remove = FALSE) %>%
    tidyr::unite(col = "REF_IDENT",
          ref1, GENE_AA_POS,
          sep = "",
          remove = FALSE) %>%
    dplyr::select(-ref1) %>%
    dplyr::mutate("REF_IDENT" = stringr::str_replace_all(REF_IDENT, 
                                                         "non-genic_NANA", 
                                                         "non-genic")) %>%
    dplyr::rename("STRAND" = "strand")

  all_ref <- all_ref %>%
    dplyr::select(CHR,
                  POS,
                 REF_BASE,
                 GENE,
                 STRAND,
                 REF_CODON,
                 REF_AA,
                 GENE_AA_POS,
                 REF_IDENT) %>%
    dplyr::mutate("REF_BASE" = as.character(REF_BASE))

  
  #add in the relative codon position for each base in each gene
  #i.e. does a given base sit in the 1st, 2nd, or 3rd position of its codon?
  #this is used for naming indels
  ref_plus <- all_ref %>%
    dplyr::filter(GENE != "non-genic" & STRAND == "+")
  if (nrow(ref_plus) > 0) {
    ref_plus <- ref_plus %>%
      dplyr::group_by(GENE) %>%
      dplyr::mutate("GENE_BASE_NUM" = 1:dplyr::n()) %>%
      dplyr::mutate("CODON_POSITION" = dplyr::case_when(GENE_BASE_NUM %% 3 == 0 ~ 3L,
                                                        GENE_BASE_NUM %% 3 == 1 ~ 1L,
                                                        GENE_BASE_NUM %% 3 == 2 ~ 2L)) %>%
      dplyr::ungroup()
  } else {
    ref_plus <- data.frame("CHR" = character(),
                           "POS" = integer(),
                           "REF_BASE" = character(),
                           "GENE" = character(),
                           "STRAND" = character(),
                           "REF_CODON" = character(), 
                           "REF_AA" = character(), 
                           "GENE_AA_POS" = integer(),
                           "REF_IDENT" = character(),
                           "GENE_BASE_NUM" = integer(),
                           "CODON_POSITION" = integer())
  }
  
  ref_minus <- all_ref %>%
    dplyr::filter(GENE != "non-genic" & STRAND == "-")
  if (nrow(ref_minus) > 0) {
    ref_minus <- ref_minus %>%
      dplyr::group_by(GENE) %>%
      dplyr::mutate("GENE_BASE_NUM" = rev(1:dplyr::n())) %>%
      dplyr::mutate("CODON_POSITION" = dplyr::case_when(GENE_BASE_NUM %% 3 == 0 ~ 3L,
                                                        GENE_BASE_NUM %% 3 == 1 ~ 1L,
                                                        GENE_BASE_NUM %% 3 == 2 ~ 2L)) %>%
      dplyr::ungroup()
  } else {
    ref_minus <- data.frame("CHR" = character(),
                            "POS" = integer(),
                            "REF_BASE" = character(),
                            "GENE" = character(),
                            "STRAND" = character(),
                            "REF_CODON" = character(), 
                            "REF_AA" = character(), 
                            "GENE_AA_POS" = integer(),
                            "REF_IDENT" = character(),
                            "GENE_BASE_NUM" = integer(),
                            "CODON_POSITION" = integer())
  }
  
  
  
  #add non-genic positions back in 
  ref_nongenic <- all_ref %>%
    dplyr::filter(GENE == "non-genic")
  
  ref_nongenic <- ref_nongenic %>%
    dplyr::mutate("STRAND" = ".",
                  "GENE_BASE_NUM" = NA,
                  "CODON_POSITION" = NA) %>%
    dplyr::select(CHR, POS, REF_BASE, GENE, STRAND, REF_CODON, REF_AA, GENE_AA_POS, REF_IDENT, GENE_BASE_NUM, CODON_POSITION) #%>%
    #dplyr::mutate("chr_pos" = paste0(CHR, "_", POS))
  
  all_ref <- dplyr::bind_rows(ref_plus, ref_minus, ref_nongenic) %>%
    dplyr::arrange(CHR, POS)
  
  idx_to_replace <- which(all_ref$REF_IDENT == "non-genic")
  replacements <- paste0(all_ref$POS[idx_to_replace], "", all_ref$REF_BASE[idx_to_replace])
  all_ref$REF_IDENT[idx_to_replace] <- replacements
  
  all_ref <- all_ref %>% 
    dplyr::mutate("CHR" = stringr::str_replace_all(CHR, "_", "-"),
                  "GENE" = stringr::str_replace_all(GENE, "_", "-"))
  
  all_ref
}

