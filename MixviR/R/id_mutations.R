#' Convert Sample VCF to MixviR Input Format
#'
#' Create data frame with relevant contents of VCF
#' @param infile Path to a vcf file that must contain "DP" and "AD" flags in the FORMAT field.
#' @param max.vcf.size Max memory usage (in bytes) allowed when reading in vcf file (from vcfR). 
#' @keywords VCF
#' @importFrom magrittr %>%
#' @export
#' @return Data frame with cols "CHR"	"POS"	"REF"	"ALT"	"DP" "REF_COUNT"	"ALT_COUNT"
#' @examples
#' vcf_to_mixvir(infile = system.file("extdata", "vcfs", "Sample1_04182021.vcf.gz", package="MixviR"))

vcf_to_mixvir <- function(infile, max.vcf.size = 1e+08) {
  
    #read in vcf
    vcf_obj <- vcfR::read.vcfR(infile, 
                           limit = max.vcf.size,
                           verbose = FALSE)
    
    #get df with total depths at each position
    depths <- vcfR::extract.gt(vcf_obj, "DP", as.numeric = TRUE) %>%
      as.data.frame()
    names(depths) <- c("DP")
    
    #get df with depths of each allele
    allele_depths <- vcfR::extract.gt(vcf_obj, "AD") %>%
      as.data.frame()
    names(allele_depths) <- c("ALLELE_COUNTS")
    allele_depths <- allele_depths %>%
      dplyr::mutate("ALLELE_COUNTS" = as.character(ALLELE_COUNTS)) %>%
      tidyr::separate(col = ALLELE_COUNTS,
                      into = c("REF_COUNT", "ALT_COUNT"),
                      sep = ",",
                      fill = "right") %>%
      dplyr::mutate("REF_COUNT" = as.integer(REF_COUNT)) %>%
      dplyr::mutate("ALT_COUNT" = as.integer(ALT_COUNT))
    allele_depths$ALT_COUNT <- tidyr::replace_na(allele_depths$ALT_COUNT, 0)
    
    #infile <- gsub(".gz$", "", infile)
    
    #read in the data part of the vcf as a data frame
    consensus <- readr::read_tsv(infile,
                    comment = "##",
                    show_col_types = FALSE) %>%
      dplyr::select('#CHROM', POS, REF, ALT) %>%
      dplyr::rename("CHR" = '#CHROM')
    
    #add in the total depths/allele depths - this gives the original "consensus" format for MixviR
    consensus <- cbind(consensus, depths, allele_depths)
    
    return(consensus)
}


#' Add Read Depths To Ref
#'
#' Add column of total read depths for a given sample to reference df. Run as part of id_mutations().
#' 
#' @param ref reference genome in "MixVir" format (from create_ref() function)
#' @param samp.variants Data frame produced by function vcf_to_mixvir(). Contains columns "CHR", "POS", "REF", "ALT", "DP", "ALT_COUNT".
#' @keywords depth
#' @importFrom magrittr %>%
#' @export
#' @return Original 'ref' data frame with depth (DP) column added: cols "genomic_pos"	"ref_base"	"gene"	"ref_codon"	"ref_AA"	"GENE_AA_POS"	"ref_identity" "DP"

add_depths_to_ref <- function(ref, samp.variants) {

  #use combination of chr+pos to deal with multiple chromosomes
  sample_data <- samp.variants %>%
    dplyr::mutate("chr_pos" = paste0(CHR, "_", POS))
    
  #get idx's where pos is duplicated - these will include indels
  dup_idx <- which(duplicated(sample_data$chr_pos))
  dup_positions <- sample_data$chr_pos[dup_idx]

  sample_data <- sample_data %>%
    dplyr::filter(!(chr_pos %in% dup_positions & ALT == '.')) %>%
    dplyr::arrange(CHR, POS, desc(DP)) %>%
    dplyr::distinct(chr_pos, .keep_all = TRUE) %>%
    dplyr::select(chr_pos, DP) 
  
  ref <- ref %>%
    dplyr::mutate("chr_pos" = paste0(CHR, "_", POS))
  
  ref <- dplyr::left_join(x = ref,
                      y = sample_data,
                      by = "chr_pos") %>%
    dplyr::select(-chr_pos)

  return(ref)
}


#' ID SNV-based Amino Acid Changes
#'
#' Identify amino acid changes associated with single nucleotide variation. Changes associated with indels are identified in separate function. Used by call_mutations() function.
#' @param variant.calls Data frame with cols POS, REF, ALT, AF (alt freq), DP (total read depth).
#' @param ref reference genome in "MixVir" format (from create_ref() function)
#' @param code.num Number defining the genetic code to use for translation
#' @keywords snps
#' @importFrom magrittr %>%
#' @export
#' @return Data frame that includes amino acid calls based on SNP/SNV variants observed in sample. Contains cols "POS", "REF_BASE", "GENE", "REF_CODON", "REF_AA", "GENE_AA_POS", "REF_IDENT", "REF", "ALT", "ALT_freq", "ALT_COUNT", "samp_codon", "samp_AA", "samp_identity", "DP"

id_snps <- function(variant.calls, ref, code.num = "1") {
  variants <- variant.calls %>% 
    dplyr::select(CHR, POS, REF, ALT, AF, ALT_COUNT) %>%
    dplyr::mutate("chr_pos" = paste0(CHR, "_", POS)) %>%
    dplyr::select(-CHR, -POS)
  
  #cut indels down to first position - will deal with these separately in id_indels()
  variants$ALT <- stringr::str_sub(variants$ALT, start = 1L, end = 1L)
  variants$REF <- stringr::str_sub(variants$REF, start = 1L, end = 1L)

  ref <- ref %>%
    dplyr::mutate("chr_pos" = paste0(CHR, "_", POS))
  #merge sample variants with reference on chr+position and add in ref base where no variant
  all_samp <- dplyr::left_join(x = ref, y = variants, by = "chr_pos") %>%
    dplyr::select(-chr_pos)

  #add the reference bases to col 'ALT' where there is no variant
  na_idx <- which(is.na(all_samp$ALT))
  all_samp$ALT[na_idx] <- all_samp$REF_BASE[na_idx]

  #add column with codon each feature position is associated with (after ALT alleles have been incorporated)
  #account for strandedness
  
  features_w_codons_plus <- all_samp %>% 
    dplyr::filter(GENE != "non-genic") %>%
    dplyr::filter(STRAND == "+")
  
  if (nrow(features_w_codons_plus) > 0) {
    features_w_codons_plus <- features_w_codons_plus  %>%
      dplyr::group_by(GENE) %>%
      dplyr::mutate("samp_codon" = get_codons(ALT)) %>%
      dplyr::ungroup()
  } else {
    features_w_codons_plus <- data.frame("REF_BASE" = character(),
                                         "GENE" = character(),
                                         "STRAND" = character(),
                                         "REF_CODON" = character(), 
                                         "REF_AA" = character(), 
                                         "GENE_AA_POS" = integer(),
                                         "REF_IDENT" = character(),
                                         "GENE_BASE_NUM" = integer(),
                                         "CODON_POSITION" = integer(),
                                         "DP" = numeric(),
                                         "CHR" = character(), 
                                         "POS" = integer(), 
                                         "REF" = character(), 
                                         "ALT" = character(), 
                                         "AF" = double(), 
                                         "ALT_COUNT" = numeric(), 
                                         "samp_codon" = character()
                                         )
  }
    
  features_w_codons_minus <- all_samp %>% 
    dplyr::filter(GENE != "non-genic") %>%
    dplyr::filter(STRAND == "-")
  
  if (nrow(features_w_codons_minus) > 0) {
    features_w_codons_minus <- features_w_codons_minus  %>%
      dplyr::group_by(GENE) %>%
      dplyr::mutate("samp_codon" = get_codons(ALT, rev = TRUE)) %>%
      dplyr::ungroup()
  } else {
    features_w_codons_minus <- data.frame("REF_BASE" = character(),
                                          "GENE" = character(),
                                          "STRAND" = character(),
                                          "REF_CODON" = character(), 
                                          "REF_AA" = character(), 
                                          "GENE_AA_POS" = integer(),
                                          "REF_IDENT" = character(),
                                          "GENE_BASE_NUM" = integer(),
                                          "CODON_POSITION" = integer(),
                                          "DP" = numeric(),
                                          "CHR" = character(), 
                                          "POS" = integer(), 
                                          "REF" = character(), 
                                          "ALT" = character(), 
                                          "AF" = double(), 
                                          "ALT_COUNT" = numeric(), 
                                          "samp_codon" = character())
  }
  
  features_w_codons <- dplyr::bind_rows(features_w_codons_plus, features_w_codons_minus)
  
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
  
  features_w_codons_fwd <- features_w_codons %>%
    dplyr::filter(STRAND == "+")
  
  if (nrow(features_w_codons_fwd) > 0) {
    features_w_codons_trans_fwd <- features_w_codons_fwd %>%
      dplyr::group_by(GENE) %>%
      dplyr::summarise("translation" = translate_fwd(ALT, code = code.num),
                       "POS" = POS) %>%
      dplyr::arrange(GENE, POS) %>%
      dplyr::pull(translation)
    
    features_w_codons_fwd <- features_w_codons_fwd %>% dplyr::arrange(GENE, POS)
    features_w_codons_fwd$samp_AA <- features_w_codons_trans_fwd
    
  } else{
    features_w_codons_fwd <- data.frame("REF_BASE" = character(),
                                        "GENE" = character(),
                                        "STRAND" = character(),
                                        "REF_CODON" = character(), 
                                        "REF_AA" = character(), 
                                        "GENE_AA_POS" = integer(),
                                        "REF_IDENT" = character(),
                                        "GENE_BASE_NUM" = integer(),
                                        "CODON_POSITION" = integer(),
                                        "DP" = numeric(),
                                        "CHR" = character(), 
                                        "POS" = integer(), 
                                        "REF" = character(), 
                                        "ALT" = character(), 
                                        "AF" = double(), 
                                        "ALT_COUNT" = numeric(), 
                                        "samp_codon" = character())
  }
  
  features_w_codons_rev <- features_w_codons %>%
    dplyr::filter(STRAND == "-")
  
  if (nrow(features_w_codons_rev) > 0) {
    features_w_codons_trans_rev <- features_w_codons_rev %>%
      dplyr::group_by(GENE) %>%
      dplyr::summarise("translation" = translate_rev(ALT, code = code.num),
                       "POS" = POS) %>%
      dplyr::arrange(GENE, POS) %>%
      dplyr::pull(translation)
    
    features_w_codons_rev <- features_w_codons_rev %>% dplyr::arrange(GENE, POS) 
    features_w_codons_rev$samp_AA <- features_w_codons_trans_rev
    
  } else{
    features_w_codons_rev <- data.frame("REF_BASE" = character(),
                                          "GENE" = character(),
                                          "STRAND" = character(),
                                          "REF_CODON" = character(), 
                                          "REF_AA" = character(), 
                                          "GENE_AA_POS" = integer(),
                                          "REF_IDENT" = character(),
                                          "GENE_BASE_NUM" = integer(),
                                          "CODON_POSITION" = integer(),
                                          "DP" = numeric(),
                                          "CHR" = character(), 
                                          "POS" = integer(), 
                                          "REF" = character(), 
                                          "ALT" = character(), 
                                          "AF" = double(), 
                                          "ALT_COUNT" = numeric(), 
                                          "samp_codon" = character())
  }
  
  features_w_codons <- dplyr::bind_rows(features_w_codons_fwd, features_w_codons_rev)
  
  features_w_codons <- features_w_codons %>%
    dplyr::mutate("samp_identity" = paste0(GENE,
                                           "_",
                                           REF_AA,
                                           GENE_AA_POS,
                                           samp_AA)) %>%
    dplyr::select(CHR, POS, REF_BASE, GENE, STRAND, REF_CODON, REF_AA, GENE_AA_POS,
                  REF_IDENT, REF, ALT, AF, ALT_COUNT, samp_codon,
                  samp_AA, samp_identity, DP)

  #this has all positions - first pull out amino acid substitutions
  aa_subs <- features_w_codons %>%
    dplyr::filter(REF_AA != samp_AA) %>%
    dplyr::filter(REF_BASE != ALT) %>%
    dplyr::mutate("ALT2" = NA) %>%
    dplyr::select(CHR, POS, REF_BASE, GENE, STRAND, REF_CODON, REF_AA, GENE_AA_POS,
                  REF_IDENT, REF, ALT, ALT2, AF, ALT_COUNT, samp_codon,
                  samp_AA, samp_identity, DP) %>%
    dplyr::mutate("CHR" = as.character(CHR),
                  "POS" = as.integer(POS),
                  "REF_BASE" = as.character(REF_BASE),
                  "GENE" = as.character(GENE),
                  "STRAND" = as.character(STRAND),
                  "REF_CODON" = as.character(REF_CODON), 
                  "REF_AA" = as.character(REF_AA), 
                  "GENE_AA_POS" = as.integer(GENE_AA_POS),
                  "REF_IDENT" = as.character(REF_IDENT),
                  "REF" = as.character(REF), 
                  "ALT" = as.character(ALT), 
                  "ALT2" = as.character(ALT2),
                  "AF" = as.double(AF), 
                  "ALT_COUNT" = as.numeric(ALT_COUNT), 
                  "samp_codon" = as.character(samp_codon),
                  "samp_AA" = as.character(samp_AA),
                  "samp_identity" = as.character(samp_identity),
                  "DP" = as.integer(DP))
  
  #next get synonymous changes
  syn_sites <- features_w_codons %>%
    dplyr::filter(REF_AA == samp_AA) %>%
    dplyr::filter(REF_BASE != ALT) %>%
    dplyr::mutate("samp_identity" = paste0(CHR, "_", POS, REF_BASE, "->", ALT),
                  "ALT2" = NA) %>%
    dplyr::select(CHR, POS, REF_BASE, GENE, STRAND, REF_CODON, REF_AA, GENE_AA_POS,
                  REF_IDENT, REF, ALT, ALT2, AF, ALT_COUNT, samp_codon,
                  samp_AA, samp_identity, DP) %>%
    dplyr::mutate("CHR" = as.character(CHR),
                  "POS" = as.integer(POS),
                  "REF_BASE" = as.character(REF_BASE),
                  "GENE" = as.character(GENE),
                  "STRAND" = as.character(STRAND),
                  "REF_CODON" = as.character(REF_CODON), 
                  "REF_AA" = as.character(REF_AA), 
                  "GENE_AA_POS" = as.integer(GENE_AA_POS),
                  "REF_IDENT" = as.character(REF_IDENT),
                  "REF" = as.character(REF), 
                  "ALT" = as.character(ALT), 
                  "ALT2" = as.character(ALT2),
                  "AF" = as.double(AF), 
                  "ALT_COUNT" = as.numeric(ALT_COUNT), 
                  "samp_codon" = as.character(samp_codon),
                  "samp_AA" = as.character(samp_AA),
                  "samp_identity" = as.character(samp_identity),
                  "DP" = as.integer(DP))
  
  #finally, get snps outside of genes
  nongenic_muts <- all_samp %>% 
    dplyr::filter(GENE == "non-genic") %>%
    dplyr::filter(REF_BASE != ALT) %>%
    dplyr::mutate("samp_codon" = NA,
                  "samp_AA" = NA,
                  "ALT2" = NA,
                  "samp_identity" = paste0(CHR, "_", POS, REF_BASE, "->", ALT)) %>%
    dplyr::select(CHR, POS, REF_BASE, GENE, STRAND, REF_CODON, REF_AA, GENE_AA_POS,
                  REF_IDENT, REF, ALT, ALT2, AF, ALT_COUNT, samp_codon,
                  samp_AA, samp_identity, DP) %>%
    dplyr::mutate("CHR" = as.character(CHR),
                  "POS" = as.integer(POS),
                  "REF_BASE" = as.character(REF_BASE),
                  "GENE" = as.character(GENE),
                  "STRAND" = as.character(STRAND),
                  "REF_CODON" = as.character(REF_CODON), 
                  "REF_AA" = as.character(REF_AA), 
                  "GENE_AA_POS" = as.integer(GENE_AA_POS),
                  "REF_IDENT" = as.character(REF_IDENT),
                  "REF" = as.character(REF), 
                  "ALT" = as.character(ALT), 
                  "ALT2" = as.character(ALT2),
                  "AF" = as.double(AF), 
                  "ALT_COUNT" = as.numeric(ALT_COUNT), 
                  "samp_codon" = as.character(samp_codon),
                  "samp_AA" = as.character(samp_AA),
                  "samp_identity" = as.character(samp_identity),
                  "DP" = as.integer(DP))
  
  dplyr::bind_rows(aa_subs, syn_sites, nongenic_muts) %>%
    dplyr::arrange(CHR, POS)
  
}


#' ID Indel-based Amino Acid Changes
#'
#' Identify amino acid changes associated with indel variation. Changes associated with SNVs are identified in separate function. Used by call_mutations() function.
#' 
#' @param variant.calls Data frame with cols POS, REF, ALT, AF, DP. Additional columns will be ignored.
#' @param ref reference genome in "MixVir" format (from create_ref() function)
#' @keywords indel
#' @return Data frame that includes amino acid calls based on indel variants observed in sample. Contains cols "POS", "REF_BASE", "GENE", "REF_CODON", "REF_AA", "GENE_AA_POS", "REF_IDENT", "REF", "ALT", "ALT_freq", "ALT_COUNT", "samp_codon", "samp_AA", "samp_identity", "DP"
#' @export

id_indels <- function(variant.calls, ref) {
  
  samp_calls_indels <- data.frame()
  
  variants <- variant.calls %>%
    dplyr::select(CHR, POS, REF, ALT, AF, ALT_COUNT)
  
  #get in-frame deletions and add them to 'samp_calls_indels' df
  dels_in_frame <- variants %>%
    dplyr::filter(stringr::str_length(REF) > 1) %>% #need to check on cases where both REF and ALT have lengths > 1
    dplyr::mutate("del_length" = stringr::str_length(REF)-1) %>%
    dplyr::mutate("aa_del_length" = del_length/3) %>%
    dplyr::filter(del_length %% 3 == 0)  #select for just in-frame deletions
  
  ref <- ref %>%
    dplyr::mutate("chr_pos" = paste0(CHR, "_", POS)) %>%
    dplyr::select(-CHR, -POS)
  
  if (nrow(dels_in_frame) > 0) {
    dels_in_frame_adj <- dels_in_frame %>%
      dplyr::select(CHR, POS, REF, ALT, AF, ALT_COUNT, aa_del_length) %>%
      dplyr::mutate("chr_pos" = paste0(CHR, "_", POS))
    
    
    dels_in_frame_w_ref <- dplyr::left_join(x = dels_in_frame_adj,
                                            y = ref,
                                            by = "chr_pos")
    
    dels_in_frame_w_ref_genic <- dels_in_frame_w_ref %>%
      dplyr::filter(!is.na(GENE_AA_POS))
    
    if (nrow(dels_in_frame_w_ref_genic) > 0) {
      dels_in_frame_w_ref_genic_plus <- dels_in_frame_w_ref_genic %>%
        dplyr::filter(STRAND == "+")
      
      #adjust the GENE_AA_POS for all deletions on the plus strand. If the deletion starts at codon position 1, then the POS in the vcf is associated with codon position 3
      #of the previous AA, so need to adjust. If the deletion starts at either codon position 2 or 3, then the POS in the vcf 
      #is associated with the first amino acid affected, but the naming convention is to use the first full amino acid 
      #affected (in the case of multiple AAs deleted), or in the case of 1 AA deleted, to name it with the upstream AA affected.
      #So, adding 1 to every position here. 
      if (nrow(dels_in_frame_w_ref_genic_plus) > 0) {
        dels_in_frame_w_ref_genic_plus <- dels_in_frame_w_ref_genic_plus %>%
          dplyr::mutate("GENE_AA_POS" = GENE_AA_POS+1,
                        "ALT2" = "del",
                        "samp_codon" = NA,
                        "samp_AA" = NA,
                        "samp_identity" = paste0("del", 
                                                 GENE_AA_POS, 
                                                 "/", 
                                                 GENE_AA_POS+aa_del_length-1))
      } else {
        dels_in_frame_w_ref_genic_plus <- dels_in_frame_w_ref_genic_plus %>%
          dplyr::mutate("ALT2" = character(),
                        "samp_codon" = character(),
                        "samp_AA" = character(),
                        "samp_identity" = character())
      }
      
      #for deletions in genes on the minus strand, the POS in the vcf is going to represent the 
      #end of the deletion. If the deletion aligns with the codons, this should be in codon position 1 
      #of the downstream AA affected (so if deletion was of AA9/10, should be pos 1 of AA 11.)
      #In this case, need to subtract 1 from GENE_AA_POS and use this as the end of the deletion and 
      #subtract the number of AA's affected to get the beginning of the deletion.
      #If the deletion doesn't align with codons, the POS (end of the deletion) should fall on either 
      #codon position 2 or 3 of the last AA affected. In this case, the GENE_AA_POS is correct as the 
      #last AA affected, and need to again subtract the number of AA's affected to get the beginning of the deletion.
      dels_in_frame_w_ref_genic_minus <- dels_in_frame_w_ref_genic %>%
        dplyr::filter(STRAND == "-")
      
      if (nrow(dels_in_frame_w_ref_genic_minus) > 0) {
        idx_to_fix <- which(dels_in_frame_w_ref_genic_minus$CODON_POSITION == 1)
        dels_in_frame_w_ref_genic_minus$GENE_AA_POS[idx_to_fix] <- dels_in_frame_w_ref_genic_minus$GENE_AA_POS[idx_to_fix]-1
        
        dels_in_frame_w_ref_genic_minus <- dels_in_frame_w_ref_genic_minus %>%
          dplyr::mutate("ALT2" = "del",
                        "samp_codon" = "del",
                        "samp_AA" = "del",
                        "samp_identity" = paste0("del", 
                                                 GENE_AA_POS-(aa_del_length-1),
                                                 "/", 
                                                 GENE_AA_POS))
      } else {
        dels_in_frame_w_ref_genic_minus <- dels_in_frame_w_ref_genic_minus %>%
          dplyr::mutate("ALT2" = character(),
                        "samp_codon" = character(),
                        "samp_AA" = character(),
                        "samp_identity" = character())
      }
      
      dels_in_frame_w_ref_genic <- dplyr::bind_rows(dels_in_frame_w_ref_genic_plus, dels_in_frame_w_ref_genic_minus)
    }
    
    
    #now deal with nongenic in-frame deletions
    dels_in_frame_w_ref_nongenic <- dels_in_frame_w_ref %>%
      dplyr::filter(is.na(GENE_AA_POS))
    
    if (nrow(dels_in_frame_w_ref_nongenic) > 0) {
      dels_in_frame_w_ref_nongenic <- dels_in_frame_w_ref_nongenic %>%
        dplyr::mutate("ALT2" = "del",
                      "samp_codon" = NA,
                      "samp_AA" = NA,
                      "samp_identity" = paste0("del", 
                                               POS+1, 
                                               "/", 
                                               aa_del_length*3, "bp"))
    } else {
      dels_in_frame_w_ref_nongenic <- dels_in_frame_w_ref_nongenic %>%
        dplyr::mutate("ALT2" = character(),
                      "samp_codon" = character(),
                      "samp_AA" = character(),
                      "samp_identity" = character())
    }
    
    dels_in_frame_w_ref <- dplyr::bind_rows(dels_in_frame_w_ref_genic, dels_in_frame_w_ref_nongenic)
    
    
    dels_in_frame_w_ref <- dels_in_frame_w_ref %>%
      dplyr::select(CHR, POS, REF_BASE, GENE, STRAND, REF_CODON, REF_AA, GENE_AA_POS,
                    REF_IDENT, REF, ALT, ALT2, AF, ALT_COUNT, samp_codon,
                    samp_AA, samp_identity, DP)
    
    samp_calls_indels <- rbind(samp_calls_indels, dels_in_frame_w_ref)
  }
  
  
  #get frame-shift deletions and add them to 'samp_calls_indels' df
  dels_out_frame <- variants %>%
    dplyr::filter(stringr::str_length(REF) > 1) %>%
    dplyr::mutate("del_length" = stringr::str_length(REF)-1) %>%
    dplyr::mutate("aa_del_length" = del_length/3) %>%
    dplyr::filter(del_length %% 3 != 0) #select any out-of-frame deletions
  
  if (nrow(dels_out_frame) > 0) {
    dels_out_frame_adj <- dels_out_frame %>%
      dplyr::select(CHR, POS, REF, ALT, AF, ALT_COUNT, del_length) %>%
      dplyr::mutate("chr_pos" = paste0(CHR, "_", POS))
    
    dels_out_frame_w_ref <- dplyr::left_join(x = dels_out_frame_adj,
                                             y = ref,
                                             by = "chr_pos")
    
    #if an out-of-frame deletion is in a gene, name it with the gene name at the AA it is deleted from
    dels_out_frame_w_ref_genic <- dels_out_frame_w_ref %>%
      dplyr::filter(!is.na(GENE_AA_POS))
    
    if (nrow(dels_out_frame_w_ref_genic) > 0) {
      
      dels_out_frame_w_ref_genic_plus <- dels_out_frame_w_ref_genic %>%
        dplyr::filter(STRAND == "+")
      
      if (nrow(dels_out_frame_w_ref_genic_plus) > 0) {
        
        idx_to_fix <- which(dels_out_frame_w_ref_genic_plus$CODON_POSITION == 3)
        dels_out_frame_w_ref_genic_plus$GENE_AA_POS[idx_to_fix] <- dels_out_frame_w_ref_genic_plus$GENE_AA_POS[idx_to_fix]+1
        
        dels_out_frame_w_ref_genic_plus <- dels_out_frame_w_ref_genic_plus %>%
          dplyr::mutate("ALT2" = "Fdel",
                        "samp_codon" = NA,
                        "samp_AA" = NA,
                        "samp_identity" = paste0("Fdel", 
                                                 GENE_AA_POS, 
                                                 "/", del_length, "bp"))
      } else {
        dels_out_frame_w_ref_genic_plus <- dels_out_frame_w_ref_genic_plus %>%
        dplyr::mutate("ALT2" = character(),
                      "samp_codon" = character(),
                      "samp_AA" = character(),
                      "samp_identity" = character())
      }
      
      dels_out_frame_w_ref_genic_minus <- dels_out_frame_w_ref %>%
        dplyr::filter(STRAND == "-")
      
      if (nrow(dels_out_frame_w_ref_genic_minus) > 0) {
        
        idx_to_fix <- which(dels_out_frame_w_ref_genic_minus$CODON_POSITION == 1)
        dels_out_frame_w_ref_genic_minus$GENE_AA_POS[idx_to_fix] <- dels_out_frame_w_ref_genic_minus$GENE_AA_POS[idx_to_fix]-1
        
        dels_out_frame_w_ref_genic_minus <- dels_out_frame_w_ref_genic_minus %>%
          dplyr::mutate("ALT2" = "Fdel",
                        "samp_codon" = NA,
                        "samp_AA" = NA,
                        "aa_count" = ceiling(del_length/3),
                        "samp_identity" = paste0("Fdel", 
                                                 GENE_AA_POS-(aa_count-1),
                                                 "/", 
                                                 del_length, "bp"))
        
        dels_out_frame_w_ref_genic_minus <- dels_out_frame_w_ref_genic_minus %>%
          dplyr::select(-aa_count)
      } else {
        dels_out_frame_w_ref_genic_minus <- dels_out_frame_w_ref_genic_minus %>%
          dplyr::mutate("ALT2" = character(),
                        "samp_codon" = character(),
                        "samp_AA" = character(),
                        "samp_identity" = character())
      }
      
      dels_out_frame_w_ref_genic <- dplyr::bind_rows(dels_out_frame_w_ref_genic_plus, dels_out_frame_w_ref_genic_minus)
    }
    
    
    #now deal with out-of-frame deletions that are not in genes
    dels_out_frame_w_ref_nongenic <- dels_out_frame_w_ref %>%
      dplyr::filter(is.na(GENE_AA_POS))
    
    if (nrow(dels_out_frame_w_ref_nongenic) > 0) {
      dels_out_frame_w_ref_nongenic <- dels_out_frame_w_ref_nongenic %>%
        dplyr::mutate("ALT2" = "Fdel",
                      "samp_codon" = NA,
                      "samp_AA" = NA,
                      "samp_identity" = paste0("Fdel", 
                                               POS+1, 
                                               "/", del_length, "bp"))
      
    } else {
      dels_out_frame_w_ref_nongenic <- dels_out_frame_w_ref_nongenic %>%
        dplyr::mutate("ALT2" = character(),
                      "samp_codon" = character(),
                      "samp_AA" = character(),
                      "samp_identity" = character())
    }
    
    dels_out_frame_w_ref <- dplyr::bind_rows(dels_out_frame_w_ref_genic, dels_out_frame_w_ref_nongenic)
    
  
    dels_out_frame_w_ref <- dels_out_frame_w_ref %>%
      dplyr::select(CHR, POS, REF_BASE, GENE, STRAND, REF_CODON, REF_AA, GENE_AA_POS,
                    REF_IDENT, REF, ALT, ALT2, AF, ALT_COUNT, samp_codon,
                    samp_AA, samp_identity, DP)
    
    samp_calls_indels <- rbind(samp_calls_indels, dels_out_frame_w_ref)
  }
  
  #get in-frame insertions and add them to 'samp_calls_indels' df
  ins_in_frame <- variants %>%
    dplyr::filter(stringr::str_length(ALT) > 1) %>%
    dplyr::mutate("ins_length" = stringr::str_length(ALT)-1) %>%
    dplyr::mutate("aa_ins_length" = ins_length/3) %>%
    dplyr::filter(ins_length %% 3 == 0)
  
  if (nrow(ins_in_frame) > 0) {
    ins_in_frame_adj <- ins_in_frame %>%
      dplyr::select(CHR, POS, REF, ALT, AF, ALT_COUNT, aa_ins_length) %>%
      dplyr::mutate("chr_pos" = paste0(CHR, "_", POS))
    
    ins_in_frame_w_ref <- dplyr::left_join(x = ins_in_frame_adj,
                                           y = ref,
                                           by = "chr_pos")
    
    #deal with in-frame insertion in genes
    ins_in_frame_w_ref_genic <- ins_in_frame_w_ref %>%
      dplyr::filter(!is.na(GENE_AA_POS))
    
    if (nrow(ins_in_frame_w_ref_genic) > 0) {
      
      #genic, plus strand
      ins_in_frame_w_ref_genic_plus <- ins_in_frame_w_ref_genic %>%
        dplyr::filter(STRAND == "+")
      
      #adjust the GENE_AA_POS for insertions on the plus strand. If the insertion starts at codon position 1, then the POS in the vcf is associated with codon position 3
      #of the previous AA, so need to adjust. If the insertion starts at either codon position 2 or 3, then the POS in the vcf 
      #is associated with the first amino acid affected, so don't need to adjust in this case. 
      if (nrow(ins_in_frame_w_ref_genic_plus) > 0) {
        idx_to_fix <- which(ins_in_frame_w_ref_genic_plus$CODON_POSITION == 1)
        ins_in_frame_w_ref_genic_plus$GENE_AA_POS[idx_to_fix] <- ins_in_frame_w_ref_genic_plus$GENE_AA_POS[idx_to_fix]+1
        
        ins_in_frame_w_ref_genic_plus <- ins_in_frame_w_ref_genic_plus %>%
          dplyr::mutate("ALT2" = "ins",
                        "samp_codon" = "ins",
                        "samp_AA" = "ins",
                        "samp_identity" = paste0("ins", 
                                                 GENE_AA_POS, 
                                                 "/", 
                                                 GENE_AA_POS+aa_ins_length-1))
      } else{
        ins_in_frame_w_ref_genic_plus <- ins_in_frame_w_ref_genic_plus %>%
        dplyr::mutate("ALT2" = character(),
                      "samp_codon" = character(),
                      "samp_AA" = character(),
                      "samp_identity" = character())
      }
      
      #genic, minus strand
      ins_in_frame_w_ref_genic_minus <- ins_in_frame_w_ref_genic %>%
        dplyr::filter(STRAND == "-")
      
      if (nrow(ins_in_frame_w_ref_genic_minus) > 0) {
        idx_to_fix <- which(ins_in_frame_w_ref_genic_plus$CODON_POSITION == 1)
        ins_in_frame_w_ref_genic_plus$GENE_AA_POS[idx_to_fix] <- ins_in_frame_w_ref_genic_plus$GENE_AA_POS[idx_to_fix]-1
        
        ins_in_frame_w_ref_genic_minus <- ins_in_frame_w_ref_genic_minus %>%
          dplyr::mutate("ALT2" = "ins",
                        "samp_codon" = "ins",
                        "samp_AA" = "ins",
                        "samp_identity" = paste0("ins", 
                                                 GENE_AA_POS-(aa_ins_length-1), 
                                                 "/", 
                                                 GENE_AA_POS))
      } else {
        ins_in_frame_w_ref_genic_minus <- ins_in_frame_w_ref_genic_minus %>%
          dplyr::mutate("ALT2" = character(),
                        "samp_codon" = character(),
                        "samp_AA" = character(),
                        "samp_identity" = character())
      }
      
      ins_in_frame_w_ref_genic <- dplyr::bind_rows(ins_in_frame_w_ref_genic_plus, ins_in_frame_w_ref_genic_minus)
    }
    
    
    #deal with in-frame insertions not in genes
    ins_in_frame_w_ref_nongenic <- ins_in_frame_w_ref %>%
      dplyr::filter(is.na(GENE_AA_POS))
    
    if (nrow(ins_in_frame_w_ref_nongenic) > 0) {
      
      ins_in_frame_w_ref_nongenic <- ins_in_frame_w_ref_nongenic %>%
        dplyr::mutate("ALT2" = "ins",
                      "samp_codon" = NA,
                      "samp_AA" = NA,
                      "samp_identity" = paste0("ins", 
                                               POS+1, 
                                               "/", 
                                               aa_ins_length*3, "bp"))
    } else {
      ins_in_frame_w_ref_nongenic <- ins_in_frame_w_ref_nongenic %>%
        dplyr::mutate("ALT2" = character(),
                      "samp_codon" = character(),
                      "samp_AA" = character(),
                      "samp_identity" = character())
    }
    
    ins_in_frame_w_ref <- dplyr::bind_rows(ins_in_frame_w_ref_genic, ins_in_frame_w_ref_nongenic)
    
    ins_in_frame_w_ref <- ins_in_frame_w_ref %>%
      dplyr::select(CHR, POS, REF_BASE, GENE, STRAND, REF_CODON, REF_AA, GENE_AA_POS,
                    REF_IDENT, REF, ALT, ALT2, AF, ALT_COUNT, samp_codon,
                    samp_AA, samp_identity, DP)
    
    samp_calls_indels <- rbind(samp_calls_indels, ins_in_frame_w_ref)
  }

  
  #get frame-shift insertions and add them to 'samp_calls_indels' df
  ins_out_frame <- variants %>%
    dplyr::filter(stringr::str_length(ALT) > 1) %>%
    dplyr::mutate("ins_length" = stringr::str_length(ALT)-1) %>%
    dplyr::mutate("aa_ins_length" = ins_length/3) %>%
    dplyr::filter(ins_length %% 3 != 0)
  
  if (nrow(ins_out_frame) > 0) {
    ins_out_frame_adj <- ins_out_frame %>%
      dplyr::select(CHR, POS, REF, ALT, AF, ALT_COUNT, ins_length) %>%
      dplyr::mutate("chr_pos" = paste0(CHR, "_", POS))
    
    ins_out_frame_w_ref <- dplyr::left_join(x = ins_out_frame_adj,
                                            y = ref,
                                            by = "chr_pos")
    
    #if an out-of-frame insertion is in a gene, name it with the gene name at the AA where it is inserted
    ins_out_frame_w_ref_genic <- ins_out_frame_w_ref %>%
      dplyr::filter(!is.na(GENE_AA_POS))
    
    if (nrow(ins_out_frame_w_ref_genic) > 0) {
      
      #out-of frame insertion, plus strand
      ins_out_frame_w_ref_genic_plus <- ins_out_frame_w_ref_genic %>%
        dplyr::filter(STRAND == "+")
      
      if (nrow(ins_out_frame_w_ref_genic_plus) > 0) {
        idx_to_fix <- which(ins_out_frame_w_ref_genic_plus$CODON_POSITION == 3)
        ins_out_frame_w_ref_genic_plus$GENE_AA_POS[idx_to_fix] <- ins_out_frame_w_ref_genic_plus$GENE_AA_POS[idx_to_fix]+1
        
        ins_out_frame_w_ref_genic_plus <- ins_out_frame_w_ref_genic_plus %>%
          dplyr::mutate("ALT2" = "Fins",
                        "samp_codon" = NA,
                        "samp_AA" = NA,
                        "samp_identity" = paste0("Fins", 
                                                 GENE_AA_POS, 
                                                 "/", ins_length, "bp"))
      } else {
        ins_out_frame_w_ref_genic_plus <- ins_out_frame_w_ref_genic_plus %>%
          dplyr::mutate("ALT2" = character(),
                        "samp_codon" = character(),
                        "samp_AA" = character(),
                        "samp_identity" = character())
        
      }
      
      #out-of frame insertion, minus strand
      ins_out_frame_w_ref_genic_minus <- ins_out_frame_w_ref %>%
        dplyr::filter(STRAND == "-")
      
      if (nrow(ins_out_frame_w_ref_genic_minus) > 0) {
        
        idx_to_fix <- which(ins_out_frame_w_ref_genic_minus$CODON_POSITION == 1)
        ins_out_frame_w_ref_genic_minus$GENE_AA_POS[idx_to_fix] <- ins_out_frame_w_ref_genic_minus$GENE_AA_POS[idx_to_fix]-1
        
        ins_out_frame_w_ref_genic_minus <- ins_out_frame_w_ref_genic_minus %>%
          dplyr::mutate("ALT2" = "Fins",
                        "samp_codon" = "Fins",
                        "samp_AA" = "Fins",
                        "aa_count" = ceiling(ins_length/3),
                        "samp_identity" = paste0("Fins", 
                                                 GENE_AA_POS-(aa_count-1),
                                                 "/", 
                                                 ins_length, "bp"))
        
        ins_out_frame_w_ref_genic_minus <- ins_out_frame_w_ref_genic_minus %>%
          dplyr::select(-aa_count)
      } else {
        ins_out_frame_w_ref_genic_minus <- ins_out_frame_w_ref_genic_minus %>%
          dplyr::mutate("ALT2" = character(),
                        "samp_codon" = character(),
                        "samp_AA" = character(),
                        "samp_identity" = character())
      }
      
      ins_out_frame_w_ref_genic <- dplyr::bind_rows(ins_out_frame_w_ref_genic_plus, ins_out_frame_w_ref_genic_minus)
      
    } 
    
    
    #now deal with out-of-frame insertions not in genes
    ins_out_frame_w_ref_nongenic <- ins_out_frame_w_ref %>%
      dplyr::filter(is.na(GENE_AA_POS))
    
    if (nrow(ins_out_frame_w_ref_nongenic) > 0) {
      ins_out_frame_w_ref_nongenic <- ins_out_frame_w_ref_nongenic %>%
        dplyr::mutate("ALT2" = "Fins",
                      "samp_codon" = NA,
                      "samp_AA" = NA,
                      "samp_identity" = paste0("Fins", 
                                               POS+1, 
                                               "/", ins_length, "bp"))
      
    } else {
      ins_out_frame_w_ref_nongenic <- ins_out_frame_w_ref_nongenic %>%
        dplyr::mutate("ALT2" = character(),
                      "samp_codon" = character(),
                      "samp_AA" = character(),
                      "samp_identity" = character())
    }
    
    ins_out_frame_w_ref <- dplyr::bind_rows(ins_out_frame_w_ref_genic, ins_out_frame_w_ref_nongenic)
    
    ins_out_frame_w_ref <- ins_out_frame_w_ref %>%
      dplyr::select(CHR, POS, REF_BASE, GENE, STRAND, REF_CODON, REF_AA, GENE_AA_POS,
                    REF_IDENT, REF, ALT, ALT2, AF, ALT_COUNT, samp_codon,
                    samp_AA, samp_identity, DP)
    
    samp_calls_indels <- rbind(samp_calls_indels, ins_out_frame_w_ref)
    
  }
  
  #add gene/chromosome names as appropriate to samp_identity
  idx_to_fix <- which(samp_calls_indels$GENE != "non-genic")
  samp_calls_indels$samp_identity[idx_to_fix] <- paste0(samp_calls_indels$GENE[idx_to_fix], "_", samp_calls_indels$samp_identity[idx_to_fix])
  idx_to_fix <- which(samp_calls_indels$GENE == "non-genic")
  samp_calls_indels$samp_identity[idx_to_fix] <- paste0(samp_calls_indels$CHR[idx_to_fix], "_", samp_calls_indels$samp_identity[idx_to_fix])
  
  samp_calls_indels
}

#' Identify Variants From A Potentially Mixed Sample
#'
#' Identify full set of amino acid/SNP/indel changes from one or more samples (includes changes based on both SNVs and indels). This is generally the first function run in a MixviR analysis.
#' @param sample.dir **Required** Path to directory containing vcf files for each sample to be analyzed. VCF's need to contain "DP" and "AD" flags in the FORMAT field. This directory should not contain any other files.
#' @param fasta.genome Path to fasta formatted reference genome file. **Required unless *reference* is defined**.
#' @param bed Path to bed file defining features of interest (open reading frames to translate). Should be tab delimited and have 6 columns (no column names): chr, start, end, feature_name, score (not used), strand. **Required unless *reference* is defined**. See example at https://github.com/mikesovic/MixviR/blob/main/raw_files/sars_cov2_genes.bed.
#' @param reference Optional character defining a pre-constructed MixviR reference (created with `create_ref()``). "Wuhan" uses pre-generated Sars-Cov2 ref genome. Otherwise, *fasta.genome* and *bed* are required to generate MixviR formatted reference as part of the analysis.
#' @param min.alt.freq Minimum frequency (0-1) for retaining alternate alleles. Default = 0.01. Extremely low values (i.e. zero) are not recommended here - see vignette for details.
#' @param write.all.targets Logical that, if TRUE, reports sequencing depths for genomic positions associated with mutations of interest that are not observed in the sample, in addition to all mutations observed in the sample. If TRUE, requires columns "Chr" and "Pos" to be included in the *lineage.muts* file. Default FALSE.
#' @param lineage.muts Path to optional csv file defining target mutations and their underlying genomic positions. Requires cols "Gene", "Mutation", "Lineage", "Chr" and "Pos". This is used to report the sequencing depths for relevant positions when the mutation of interest is not observed in the sample. See *write.all.targets*. This file is also used in `explore_mutations()`, where the "Chr" and "Pos" columns are optional. Only necessary here in conjunction with *write.all.targets*.  
#' @param write.mut.table Logical indicating whether to write the 'samp_mutations' data frame (see "Value" below) to a text file (csv). Default = FALSE. See *outfile.name*.
#' @param outfile.name Path to file where (csv) output will be written if *write.mut.table* is TRUE. Default: "sample_mutations.csv" (written to working directory)
#' @param name.sep Optional character in input file names that separates the unique sample identifier (characters preceeding the separator) from any additional text. Only text preceeding the first instance of the character will be retained and used as the sample name.
#' @param genetic.code.num Number (character) associated with the genetic code to be used for translation. Details can be found at https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi.
#' @param out.cols Character vector with names of columns to be returned. Choose from: CHR, POS, REF_BASE, GENE, STRAND, REF_CODON, REF_AA, GENE_AA_POS, REF_IDENT, REF, ALT, AF, ALT_COUNT, SAMP_CODON, SAMP_AA, ALT_ID, DP, SAMP_NAME, TYPE. The default columns SAMP_NAME, CHR, POS, ALT_ID, AF, DP must be included to run explore_mutations().
#' @param indel.format Defines the naming convention for indels. Default is "Fwd", meaning the name would look like S_del144/144. "Rev" switches this to S_144/144del.
#' @param csv.infiles Logical to indicate whether files in *sample.dir* directory are in vcf or csv format. All files must be of the same format. If csv, they must contain columns named: "CHR"	"POS"	"REF"	"ALT"	"DP"	"ALT_COUNT". See the `batch_vcf_to_mixvir()`` function to convert vcfs to csv format). Default is FALSE (input is in vcf format). This exists primarily for legacy reasons.
#' @keywords mutation
#' @return A data frame containing variants observed for each sample, positions of the underlying mutations, and other (customizable) information. This data frame can be saved as an object in the global environment and/or written to a file (see *write.mut.table*), and in either case serves as input for the *MixviR* functions `explore_mutations()` and `estimate_lineages()`.
#' @importFrom magrittr %>%
#' @export
#' @examples
#'   
#'  ##For SARS-CoV-2 
#'  #call_mutations(sample.dir = system.file("extdata", "vcf", package = "MixviR"), 
#'  #                name.sep = "_", reference = "Wuhan") 
#'    
#'  ##OR if defining a custom reference, follow this pattern...  
#'  #genome<-"https://raw.githubusercontent.com/mikesovic/MixviR/main/raw_files/GCF_ASM985889v3.fa"
#'  #features<-"https://raw.githubusercontent.com/mikesovic/MixviR/main/raw_files/sars_cov2_genes.bed"
#'    
#'  #call_mutations(sample.dir = system.file("extdata", "vcf", package = "MixviR"), 
#'  #                name.sep = "_", 
#'  #                fasta.genome = genome,
#'  #                bed = features)

call_mutations <- function(sample.dir = NULL,
                           fasta.genome,
                           bed,
                           reference = "custom",
                           min.alt.freq = 0.01, ###need to apply a filter before we call mutations. Otherwise, lots of noise gets included and can affect real calls.
                           name.sep = NULL,
                           write.all.targets = FALSE,
                           lineage.muts = NULL,
                           genetic.code.num = "1",
                           out.cols = c("SAMP_NAME", "CHR", "POS", "GENE", "ALT_ID", "AF", "DP"),
                           write.mut.table = FALSE,
                           outfile.name = "sample_mutations.csv",
                           indel.format = "Fwd",
                           csv.infiles = FALSE
                    
                           ) {

  
  samp_files <- dir(sample.dir)
  
 
  if (reference == "Wuhan" | reference == "wuhan") {
    if(httr::http_error("https://raw.githubusercontent.com/mikesovic/MixviR/main/reference_files/wuhan.tsv")) {
      message("No internet connection or data source broken.")
      return(NULL)
    } else {
      ref_no_dp <- readr::read_tsv("https://raw.githubusercontent.com/mikesovic/MixviR/main/reference_files/wuhan.tsv", show_col_types = FALSE)
      message("Using Wuhan reference")
    }
  } else {
    ref_no_dp <- create_ref(genome = fasta.genome, feature.bed = bed, code.num = genetic.code.num)
  }

  ref_no_dp <- ref_no_dp %>%
    dplyr::mutate("chr_pos" = paste0(CHR, "_", POS))
  
  #df to store all mutation calls across all samples
  all_variants <- data.frame()

  for (file in samp_files) {
    curr_samp <- file

    if (!is.null(name.sep)) {
      curr_samp <- gsub(paste0("(.+?)", name.sep, "(.*)"), "\\1", file)
    }

    #trim off trailing forward slash if it's in the sample.dir path
    sample.dir <- gsub("/$", "", sample.dir)
    
    if (csv.infiles == FALSE) {
      #file is currently a vcf - need to get it in to "MixviR/csv format"
      variants_df <- vcf_to_mixvir(infile = paste0(sample.dir, "/", file))
    } else{
      variants_df <- readr::read_csv(file = paste0(sample.dir, "/", file), show_col_types = FALSE)
    }
    
    variants_df <- variants_df %>%
      dplyr::mutate("CHR" = stringr::str_replace_all(CHR, "_", "-")) %>%
      dplyr::filter(ALT != "*") #this removes spanning deletions denoted in GATK vcfs, as they cause an error in the get_codons function
    
    #add coverages at each position for the current sample to the ref object/data frame
    ref <- add_depths_to_ref(ref = ref_no_dp,
                             samp.variants = variants_df)
    
    ref <- ref %>%
      dplyr::mutate("CHR" = as.character(CHR))
    
    
    all_variants_temp <- data.frame()
    message(paste0("Calling mutations: ", curr_samp))
    
    sample_variants <- variants_df %>%
      dplyr::filter(ALT != '.') %>%
      dplyr::select(CHR, POS, REF, ALT, ALT_COUNT, DP) %>%
      dplyr::mutate("AF" = ALT_COUNT/DP) %>%
      dplyr::filter(AF >= min.alt.freq) %>%
      dplyr::mutate("chr_pos" = paste0(CHR, "_", POS))
      
    #determine if there are any positions with multiple mutations
    multiple_mutation_idx <- which(duplicated(sample_variants$chr_pos))

    #if no sites with multiple mutations
    if (length(multiple_mutation_idx) == 0) {
      samp_calls_snv <- id_snps(variant.calls = sample_variants, ref = ref, code.num = genetic.code.num)

      #run function to identify indels and add them to 'all_variants' df
      samp_calls_indels <- id_indels(variant.calls = sample_variants, ref = ref)

      all_variants_temp <- rbind(all_variants_temp, samp_calls_snv, samp_calls_indels)
    } else {    #if one or more sites with multiple mutations

      dups_df <- sample_variants %>% dplyr::slice(multiple_mutation_idx)
      sample_variants <- sample_variants %>% dplyr::slice(-multiple_mutation_idx)
      #deal with sample_variants here the same way as above, but then
      #subsequently add in the dups_df.
      
      samp_calls_snv <- id_snps(variant.calls = sample_variants, ref = ref, code.num = genetic.code.num)

      #run function to identify indels and add them to 'all_variants' df
      samp_calls_indels <- id_indels(variant.calls = sample_variants, ref = ref)

      all_variants_temp <- rbind(all_variants_temp, samp_calls_snv, samp_calls_indels)
      
      #deal with multiple mutation sites
      while (nrow(dups_df) > 0) {
        dup_idx <- which(duplicated(dups_df$chr_pos))
        if (length(dup_idx) > 0) {
          not_dups <- dups_df %>% dplyr::slice(-dup_idx) #work with these in current iteration
          dups_df <- dups_df %>% dplyr::slice(dup_idx)

          samp_calls_snv <- id_snps(variant.calls = not_dups, ref = ref, code.num = genetic.code.num)
          samp_calls_indels <- id_indels(variant.calls = not_dups, ref = ref)

          all_variants_temp <- rbind(all_variants_temp, samp_calls_snv, samp_calls_indels)

        } else {
          not_dups <- dups_df
          dups_df <- data.frame()

          samp_calls_snv <- id_snps(variant.calls = not_dups, ref = ref, code.num = genetic.code.num)
          samp_calls_indels <- id_indels(variant.calls = not_dups, ref = ref)

          all_variants_temp <- rbind(all_variants_temp, samp_calls_snv, samp_calls_indels)
        }
      }
    }

    #for the current sample, pull out mutations (anything with and AF > 0)
    #add this to the master 'all_variants' df
    all_variants_temp <- all_variants_temp %>%
      dplyr::distinct(.keep_all = TRUE) %>%
      dplyr::filter(!is.na(AF)) %>%
      dplyr::filter(AF > 0) %>%
      dplyr::mutate("samp_name" = curr_samp) %>%
      dplyr::arrange(CHR, POS)
    
    #add data for the current sample in to master all_variants df
    all_variants <- rbind(all_variants, all_variants_temp)
}
    
  #mark the type of change
  all_variants <- all_variants %>%
    dplyr::mutate("TYPE" = dplyr::case_when((ALT2 %in% c("del", "ins", "Fdel", "Fins") & GENE != "non-genic") ~ "Indel-Genic",
                                     (ALT %in% c("A", "T", "G", "C") & REF_AA == samp_AA) ~ "SNP-Syn",
                                     (ALT %in% c("A", "T", "G", "C") & REF_AA != samp_AA) ~ "SNP-Nsyn",
                                     (ALT2 %in% c("del", "ins", "Fdel", "Fins") & GENE == "non-genic") ~ "Indel-Nongenic",
                                     (ALT %in% c("A", "T", "G", "C") & GENE == "non-genic") ~ "SNP-Nongenic"))
   
  #create samp_mutations df and write it out - this is the final output
  samp_mutations <- all_variants %>%
    dplyr::rename("ALT_ID" = "samp_identity",
                  "SAMP_NAME" = "samp_name",
                  "SAMP_CODON" = "samp_codon",
                  "SAMP_AA" = "samp_AA") %>%
    dplyr::select(dplyr::all_of(out.cols))
    
  
  if (indel.format == "Rev") {
    samp_mutations <- samp_mutations %>%
      dplyr::mutate("ALT_ID" = stringr::str_replace(ALT_ID, "(F?del)(.+)", "\\2\\1"))
  }

  if (write.all.targets == "TRUE") {
    
    #read in the file that associates genomic positions with mutations of interest and lineage
    target_mutations <- readr::read_csv(lineage.muts, show_col_types = FALSE) %>% 
      dplyr::select(-Lineage) %>%
      tidyr::unite("ALT_ID",
                   Gene, Mutation,
                   sep  = "_") %>%
      tidyr::drop_na()
    #end up here with ALT_ID, Chr, Pos in target_mutations
    
    all_variants <- data.frame()
    
    for (curr_file in samp_files) {
      curr_samp <- curr_file
      
      #get the names reported in SAMP_NAME, if edited from file name. Original file name is still stored as samp_files[X]
      if (!is.null(name.sep)) {
        curr_samp <- gsub(paste0("(.+?)", name.sep, "(.*)"), "\\1", curr_file)
      }
      
      #filter to get mutations observed in the current sample
      samp_muts_filt <- samp_mutations %>%
        dplyr::filter(SAMP_NAME == curr_samp)
      
      #get the set of mutations of interest that didn't appear in the sample
      not_in_samp <- dplyr::anti_join(x = target_mutations,
                                      y = samp_muts_filt,
                                      by  = "ALT_ID") %>%
        tidyr::unite("chr_pos",
                     Chr, Pos,
                     sep = ";;;;")
      
      ref_df <- ref_no_dp
      
      #get depths for positions associated with each mutation of interest not observed in the current sample
      if (csv.infiles == FALSE) {
        #file is currently a vcf - need to get it in to "MixviR/csv format"
        variants_df <- vcf_to_mixvir(infile = paste0(sample.dir, "/", curr_file))
      } else{
        variants_df <- readr::read_csv(file = paste0(sample.dir, "/", curr_file), show_col_types = FALSE)
      }
      
      ref_w_depth <- add_depths_to_ref(ref = ref_df,
                                       samp.variants = variants_df)
      
      ref_w_depth <- ref_w_depth %>%
        tidyr::unite("chr_pos",
                     CHR, POS,
                     sep = ";;;;")

      not_in_samp <- dplyr::left_join(x = not_in_samp,
                                      y = ref_w_depth,
                                      by = "chr_pos")
      
      not_in_samp <- not_in_samp %>%
        dplyr::mutate("SAMP_NAME" = curr_samp,
                      "ALT_COUNT" = 0,
                      "DP" = DP,
                      "REF" = REF_BASE,
                      "ALT" = NA,
                      "SAMP_CODON" = NA,
                      "SAMP_AA" = NA) %>%
        tidyr::separate(col = "chr_pos",
                        into = c("CHR", "POS"),
                        sep = ";;;;") %>%
        tidyr::separate(col = "ALT_ID",
                        into = c("GENE", "ALT_ID"),
                        sep = "_") %>%
        dplyr::mutate("AF" = ALT_COUNT/DP,
                      "POS" = as.integer(POS),
                      "ALT_ID" = paste0(GENE, "_", ALT_ID),
                      "TYPE" = "Unobserved Target") %>%
        dplyr::select(dplyr::all_of(out.cols)) %>%
        dplyr::distinct(.keep_all = TRUE)
      
      all_variants <- dplyr::bind_rows(all_variants, samp_muts_filt, not_in_samp)
      
    }
    
    samp_mutations <- all_variants %>%
      dplyr::mutate("AF" = tidyr::replace_na(AF, 0),
                    "DP" = tidyr::replace_na(DP, 0))
  }
  
  
  if (write.mut.table == TRUE) {
    write.table(samp_mutations, 
                file = outfile.name,
                sep = ",",
                row.names = FALSE,
                quote = FALSE)
  }
  
  samp_mutations
}

