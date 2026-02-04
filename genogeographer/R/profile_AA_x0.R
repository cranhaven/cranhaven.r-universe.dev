#' Function that compute the genotype probability for each population (rows in df)
#' @param AA_profile A tibble/data.frame with columns 'locus', 'A1' and 'A2' holding the separated version of a genotype, eg. AG -> A1: A, A2: G
#' @param df The database with main alleles per locus
#' @param select Which columns to return
#' @param keep_dropped Logical. Keep the non-matching alleles (compared to `db`) and those with genotype `NN`
#' @export
profile_AA_x0 <- function(AA_profile, df, select = c("locus", "x0"), keep_dropped = FALSE){
  ## build fixes : start ##
  A1 <- NULL
  A2 <- NULL
  locus <- NULL
  main_allele <- NULL
  . <- NULL
  other_allele <- NULL
  x0 <- NULL
  n2_x0 <- NULL
  ## build fixes : end ##
  ## Keep original columns if not specified
  if(is.null(select)) select <- c(names(AA_profile), "x0")
  ## Create profile prop (discarded loci)
  DNA_bases <- c("A","C","G","T")
  profile_drop <- AA_profile %>% filter(!(A1 %in% DNA_bases), !(A2 %in% DNA_bases))
  ## Select loci where observations are full (if A1 or A2 is "N" the locus goes to profile_drop)
  profile <- AA_profile %>% filter(A1 %in% DNA_bases, A2 %in% DNA_bases)
  SNP_main_allele <- df %>% select(starts_with("pop")) %>% unnest() %>% select(locus, main_allele, other_allele) %>% distinct()
  ## locus not on marker set are removed
  profile_drop <- bind_rows(profile_drop, anti_join(profile, SNP_main_allele, by = "locus"))
  ## Convert profile to x0
  profile_x0 <- inner_join(profile, SNP_main_allele, by = "locus") %>%
    mutate(
      x0 = (A1==main_allele) + (A2==main_allele),
      n2_x0 = (A1==other_allele) + (A2==other_allele)
      )
  profile_drop <- bind_rows(profile_drop, filter(profile_x0, x0 + n2_x0 != 2)) %>% 
    select_(.dots = select)
  profile_x0 <- profile_x0 %>% 
    filter(x0 + n2_x0 == 2) %>% 
    select_(.dots = select)
  if(!keep_dropped) return(profile_x0)
  list(profile_x0 = profile_x0, profile_drop = profile_drop)
}

