# ------------------------------------------------------------------------------
# Utils functions

#' @include piglet.R
NULL

## Function to clean allele calls
clean_allele_calls <- function(segment_call, sep = ","){
  
  segment_regex <- "(IG[HKL]|TR[ABDG])[VDJADEGMC][A-R0-9()]*[-/\\w]*[-*]*[.\\w]+"
  nl_regex <- "(IG[HKL]|TR[ABDG])[VDJADEGMC][0-9]+-NL[0-9]([-/\\w]*[-*][.\\w]+)*"
  segments <- strsplit(segment_call, sep)
  r <- sapply(segments, function(segment){
    segment <- sub(paste0("[^", sep, "]*(", segment_regex, ")[^", sep, "]*"), "\\1", segment, perl = TRUE)
    segment[!grepl(nl_regex, segment)]
  }
  )
  return(r)
}

## Z score function for the allele based genotype
z_score <- function(Ni, N, Pi) {
  (Ni - Pi * N) / sqrt(Pi * N * (1 - Pi))
}
