## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  crop = knitr::hook_pdfcrop
)
suppressMessages(library(htmltools))

## ----echo=FALSE---------------------------------------------------------------
htmltools::img(src = knitr::image_uri("piglet_logo.svg"), 
               alt = 'logo', 
               style = 'position:absolute; top:0; right:0; padding:10px;border: none !important;')

## ----plot-amplicon, fig.width=11, fig.height=2, echo=FALSE, fig.cap="**V library amplicon length.** Each row is a different V coverage, S1 for full length, S2 for BIOMED-2 primers, and S3 for adaptive coverage. The colors indicates the V regions according to IMGT numbering, where dark gray represents the IMGT gaps."----
FWR1 <-
  "caggtgcagctggtgcagtctggggct...gaggtgaagaagcctggggcctcagtgaaggtctcctgcaaggcttct"

CDR1 <- "ggatacaccttc............accggctactat"

FWR2 <- "atgcactgggtgcgacaggcccctggacaagggcttgagtggatgggacgg"

CDR2 <- "atcaaccctaac......agtggtggcaca"

FWR3 <-
  "aactatgcacagaagtttcag...ggcagggtcaccagtaccagggacacgtccatcagcacagcctacatggagctgagcaggctgagatctgacgacacggtcgtgtattactgt"

CDR3 <- "gcgagaga"

seq <- c("seq1" = paste0(FWR1, CDR1, FWR2, CDR2, FWR3, CDR3))

mat_letters = matrix(sample(letters[1:4], 100, replace = TRUE), 10)

i = 1
regions <- c()
letters <- c()
reg <- c("FWR1", "CDR1", "FWR2", "CDR2", "FWR3", "CDR3")
for (s in toupper(c(FWR1, CDR1, FWR2, CDR2, FWR3, CDR3))) {
  letters <- c(letters, strsplit(s, "")[[1]])
  
  regions <-
    c(regions, ifelse(grepl("[.]", strsplit(s, "")[[1]]), "gap", reg[i]))
  i = i + 1
}

togap <- function(vgap, vdj) {
  ##add in vdj gaps
  gapadd <- vdj
  for (i in which(unlist(strsplit(vgap, "", fixed = T)) == ".")) {
    gapadd <-
      paste0(substr(gapadd, 1, i - 1), ".", substr(gapadd, i, nchar(gapadd)))
  }
  return(gapadd)
}


primer <- "GGCCTCAGTGAAGGTCTCCTGCAAG"
seq <- toupper(paste0(FWR1, CDR1, FWR2, CDR2, FWR3, CDR3))
loc <-
  unlist(aregexec(
    text = gsub("[.]", "", seq),
    pattern = primer,
    max.distance = 4
  ))
seq_n_gap <- gsub("[.]", "", seq)
preceding <- substr(seq_n_gap, 1, (loc[1] - 1 + nchar(primer)))
preceding <- gsub("[AGCT]", "N", preceding)
fr1_seq <- substr(seq_n_gap, (loc[1] + nchar(primer)), nchar(seq_n_gap))
seq_paste <- paste0(preceding, fr1_seq)
seq_gapped <- togap(seq, seq_paste)
FWR1_s2 <- strsplit(seq_gapped, toupper(CDR1))[[1]][1]
i = 1
for (s in toupper(c(FWR1_s2, CDR1, FWR2, CDR2, FWR3, CDR3))) {
  if (any(grepl("N", s))) {
    letters <- c(letters, strsplit(s, "")[[1]])
    
    regions <-
      c(regions, ifelse(grepl("[.]", strsplit(s, "")[[1]]), "", ifelse(grepl(
        "N", strsplit(s, "")[[1]]
      ), "", reg[i])))
    
  } else{
    letters <- c(letters, strsplit(s, "")[[1]])
    regions <-
      c(regions, ifelse(grepl("[.]", strsplit(s, "")[[1]]), "gap", reg[i]))
  }
  
  i = i + 1
  
}

lower_thresh <- 225
upper_seq <- substr(seq, lower_thresh, nchar(seq))
lower_seq <- strsplit(seq, upper_seq, fixed = T)[[1]][1]
lower_seq <- gsub("[ATCG]", "N", lower_seq)
s3_seq <- paste0(lower_seq, upper_seq)


seqs_s3 <-
  sapply(toupper(c(FWR1, CDR1, FWR2, CDR2, FWR3, CDR3)), function(s) {
    s <- gsub("[ATCG]", "N", s)
    stringi::stri_extract(s3_seq, fixed = toupper(s))
  }, USE.NAMES = F)

seqs_s3[5] <-
  strsplit(strsplit(s3_seq, gsub("[ATCG]", "N", toupper(CDR2)), fixed = T)[[1]][2], toupper(CDR3), fixed = T)[[1]][1]
seqs_s3[6] <- toupper(CDR3)
i = 1
for (s in seqs_s3) {
  letters <- c(letters, strsplit(s, "")[[1]])
  regions <- c(regions, ifelse(grepl("[.]", strsplit(s, "")[[1]]),
                               "", ifelse(grepl(
                                 "N", strsplit(s, "")[[1]]
                               ), "", reg[i])))
  i = i + 1
}


mat_seq <- matrix(letters,
                  nrow = 3,
                  ncol = 320,
                  byrow = T)
mat_regions <- matrix(regions,
                      nrow = 3,
                      ncol = 320,
                      byrow = T)

fam_col <-
  c(
    "brown4",
    "darkblue",
    "darkorchid4",
    "darkgreen",
    "firebrick",
    "darkorange3",
    "deeppink4",
    "deepskyblue4",
    "darkslategrey"
  )

split_col <- unlist(sapply(1:6, function(i) {
  rep(reg[i], nchar(c(FWR1, CDR1, FWR2, CDR2, FWR3, CDR3)[i]))
}))
split_col <- factor(split_col, levels = reg)
vreg <- ComplexHeatmap::Heatmap(
  mat_regions,
  name = "V regions",
  col = structure(c("white", "#00000099", fam_col[1:6]),
                  names = c("", "gap", reg)),
  heatmap_height = grid::unit(5, "cm"),
  row_split = c("S1", "S2", "S3"),
  cluster_rows = F,
  cluster_columns = F,
  show_heatmap_legend = F,
  column_split = split_col
)
ComplexHeatmap::draw(vreg, padding = grid::unit(c(0, 0, 0, 1), "cm"))

## ----germline-----------------------------------------------------------------
library(piglet)
data(HVGERM)

## ----functionality------------------------------------------------------------
data(hv_functionality)

## ----lst-germlineset----------------------------------------------------------
germline <- HVGERM
## keep only functional alleles
germline <- germline[hv_functionality$allele[hv_functionality$functional=="F"]]
## keep only alleles that start from the first position of the V sequence
germline <- germline[!grepl("^[.]", germline)]
## keep only alleles that are at minimum 318 nucleotide long
germline <- germline[nchar(germline) >= 318]
## keep only localized alleles (remove NL)
germline <- germline[!grepl("NL", names(germline))]

## ----lst-germlineset-code, ref.label='lst-germlineset', anchor="block", eval=FALSE----
#  germline <- HVGERM
#  ## keep only functional alleles
#  germline <- germline[hv_functionality$allele[hv_functionality$functional=="F"]]
#  ## keep only alleles that start from the first position of the V sequence
#  germline <- germline[!grepl("^[.]", germline)]
#  ## keep only alleles that are at minimum 318 nucleotide long
#  germline <- germline[nchar(germline) >= 318]
#  ## keep only localized alleles (remove NL)
#  germline <- germline[!grepl("NL", names(germline))]

## ----lst-asc, fig.cap=""------------------------------------------------------
asc <- inferAlleleClusters(
  germline_set = germline, 
  trim_3prime_side = 318, 
  mask_5prime_side = 0, 
  family_threshold = 75, 
  allele_cluster_threshold = 95)

## ----asc-plot, fig.height=18, fig.width=15, fig.cap="**Allele similarity clusters.** The out most circle is the allele names, the second layer are the ASC groups, each group is labeled and colored. The third circle is the clustering dendrogram, the branches are colored by the ASC families. The blue and orange dashed lines are the 95% and 75% similarity ASC threshold."----
plot(asc)

## ----frw1---------------------------------------------------------------------
germline_frw1 <- artificialFRW1Germline(germline, mask_primer = T)

## ----eval=FALSE---------------------------------------------------------------
#  zenodo_doi <- "10.5281/zenodo.7401189"
#  asc_archive <-
#    recentAlleleClusters(doi = zenodo_doi, get_file = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  allele_cluster_table <- extractASCTable(archive_file = asc_archive)

## ----echo=FALSE---------------------------------------------------------------
allele_cluster_table <- read.delim('asc_alleles_table.tsv',sep='\t')

## ----echo=FALSE---------------------------------------------------------------
asc_tables <- dplyr::left_join(
  asc@alleleClusterTable %>%
    dplyr::select(new_allele, imgt_allele) %>% dplyr::group_by(new_allele) %>%
    dplyr::summarise(imgt_allele = paste0(sort(
      unique(imgt_allele)
    ), collapse = "/")),
  allele_cluster_table %>% dplyr::select(new_allele, imgt_allele) %>% dplyr::group_by(new_allele) %>%
    dplyr::summarise(imgt_allele = paste0(sort(
      unique(imgt_allele)
    ), collapse = "/")),
  by = "new_allele",
  suffix = c(".piglet", ".zenodo")
)

## -----------------------------------------------------------------------------
# loading TIgGER AIRR-seq b cell data
data <- tigger::AIRRDb

## -----------------------------------------------------------------------------
allele_cluster_table <-
  allele_cluster_table %>% dplyr::group_by(new_allele, func_group, thresh) %>%
  dplyr::summarise(imgt_allele = paste0(sort(unique(imgt_allele)), collapse = "/"),
                   .groups = "keep")

## -----------------------------------------------------------------------------
# storing original v_call values
data$v_call_or <- data$v_call
# assigning the ASC alleles
asc_data <- assignAlleleClusters(data, allele_cluster_table)

head(asc_data[, c("v_call", "v_call_or")])

## -----------------------------------------------------------------------------
# reforming the germline set
asc_germline <- germlineASC(allele_cluster_table, germline = HVGERM)

## -----------------------------------------------------------------------------
# inferring the genotype
asc_genotype <- inferGenotypeAllele_asc(
  asc_data,
  alleleClusterTable = allele_cluster_table,
  germline_db = asc_germline,
  find_unmutated = T
)

head(asc_genotype)

## -----------------------------------------------------------------------------
# get the genotype alleles
alleles <- unlist(strsplit(asc_genotype$genotyped_imgt_alleles, ","))
# get the genes
genes <- gsub("[*][0-9]+", "", alleles)
# extract the alleles
alleles <- sapply(strsplit(alleles, "[*]"), "[[", 2)
# make sure to extract only alleles
alleles <- gsub("([0-9]+).*$", "\\1", alleles)
# create the genotype
genotype <- data.frame(alleles = alleles, gene = genes)
# plot the genotype
tigger::plotGenotype(genotype = genotype)

