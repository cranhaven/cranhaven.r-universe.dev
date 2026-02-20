## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, results = "markup", fig.align = "center", fig.width = 6, fig.height = 5)
library(TCGAretriever)
library(reshape2)
library(ggplot2)

## ----results='markup'---------------------------------------------------------
library(TCGAretriever)
library(reshape2)
library(ggplot2)

# Obtain a list of cancer studies from cBio
all_studies <- get_cancer_studies()

# Find published TCGA datasets
keep <- grepl("tcga_pub$", all_studies[,'studyId'])
tcga_studies <- all_studies[keep, ]

# Show results
utils::head(tcga_studies[, c(11, 1, 4)])

## ----results='markup'---------------------------------------------------------
# Define the cancer study id: brca_tcga_pub
my_csid <- "brca_tcga_pub"

# Obtain genetic profiles
brca_pro <- get_genetic_profiles(csid = my_csid)
utils::head(brca_pro[, c(7, 1)])

## ----results='markup'---------------------------------------------------------
# Obtain cases 
brca_cas <- get_case_lists(csid = my_csid)
utils::head(brca_cas[, c(4, 1)])

## -----------------------------------------------------------------------------
# Define a set of genes of interest
q_csid <- 'brca_tcga_pub'
q_genes <- c("TP53", "HMGA1", "E2F1", "EZH2")
q_cases <- "brca_tcga_pub_complete"
rna_prf <- "brca_tcga_pub_mrna"
mut_prf <- "brca_tcga_pub_mutations"

# Download Clinical Data
brca_cli <- get_clinical_data(csid = q_csid, case_list_id = q_cases)

# Download RNA
brca_RNA <- get_molecular_data(case_list_id = q_cases, 
                               gprofile_id = rna_prf, 
                               glist = q_genes)

## ----results='markup'---------------------------------------------------------
# Show results
brca_RNA[, 1:5]

## -----------------------------------------------------------------------------
# Set SYMBOLs as rownames
# Note that you may prefer to use the tibble package for this
rownames(brca_RNA) <- brca_RNA$hugoGeneSymbol
brca_RNA <- brca_RNA[, -c(1, 2, 3)]

# Round numeric vals to 3 decimals
for (i in 1:ncol(brca_RNA)) {
  brca_RNA[, i] <- round(brca_RNA[, i], digits = 3)
}

# Download mutations
brca_MUT <- get_mutation_data(case_list_id = q_cases, 
                              gprofile_id = mut_prf, 
                              glist = q_genes)

# Identify Samples carrying a TP53 missense mutation
tp53_mis_keep <- brca_MUT$hugoGeneSymbol == 'TP53' &
  brca_MUT$mutationType == 'Missense_Mutation' &
  !is.na(brca_MUT$sampleId)
tp53_mut_samples <- unique(brca_MUT$sampleId[tp53_mis_keep])

# Show results
keep_cols <- c('sampleId', 'hugoGeneSymbol', 'mutationType',  'proteinChange')
utils:::head(brca_MUT[, keep_cols])

## ----eval=FALSE---------------------------------------------------------------
# # Download all brca_pub mutation data (complete samples)
# all_brca_MUT <- fetch_all_tcgadata(case_list_id = q_cases,
#                                    gprofile_id = mut_prf,
#                                    mutations = TRUE)
# 
# # Download all brca_pub RNA expression data (complete samples)
# all_brca_RNA <- fetch_all_tcgadata(case_list_id = q_cases,
#                                    gprofile_id = rna_prf,
#                                    mutations = FALSE)

## ----fig.width=5, fig.height=5------------------------------------------------
# Visualize the correlation between EZH2 and E2F1
df <- data.frame(sample_id = colnames(brca_RNA), 
                 EZH2 = as.numeric(brca_RNA['EZH2', ]), 
                 E2F1 = as.numeric(brca_RNA['E2F1', ]), 
                 stringsAsFactors = FALSE)

ggplot(df, aes(x = EZH2, y = E2F1)) +
  geom_point(color = 'gray60', size = 0.75) +
  theme_bw() +
  geom_smooth(method = 'lm', color = 'red2', 
              size=0.3, fill = 'gray85') +
  ggtitle('E2F1-EZH2 correlation in BRCA') + 
  theme(plot.title = element_text(hjust = 0.5))

## -----------------------------------------------------------------------------
# Bin samples according to EZH2 expression
EZH2_bins <- make_groups(num_vector = df$EZH2, groups = 5) 
utils::head(EZH2_bins, 12)

## ----fig.width=4.8, fig.height=4.2--------------------------------------------
# attach bin to df
df$EZH2_bin <- EZH2_bins$rank

# build Boxplot
ggplot(df, aes(x = as.factor(EZH2_bin), y = E2F1)) +
  geom_boxplot(outlier.shape = NA, fill = '#fed976') +
  geom_jitter(width = 0.1, size = 1) +
  theme_bw() +
  xlab('EZH2 Bins') +
  ggtitle('E2F1 Expression vs. Binned EZH2 Expression') +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5))

## ----fig.width=9, fig.height=5------------------------------------------------
# Coerce to data.frame with numeric features 
mol_df <- data.frame(sample_id = colnames(brca_RNA), 
                     HMGA1 = as.numeric(brca_RNA['HMGA1', ]),
                     TP53 = as.numeric(brca_RNA['TP53', ]),
                     stringsAsFactors = FALSE)

mol_df$TP53.status = factor(ifelse(mol_df$sample_id %in% tp53_mut_samples, 
                                   '01.wild_type', '02.mutated'))
  
# Visualize the correlation between EZH2 and E2F1
ggplot(mol_df, aes(x = TP53, y = HMGA1)) +
  geom_point(color = 'gray60', size = 0.75) +
  facet_grid(cols = vars(TP53.status)) +
  theme_bw() +
  geom_smooth(mapping = aes(color = TP53.status), 
              method = 'lm', size=0.3, fill = 'gray85') +
  ggtitle('HMGA1-TP53 correlation in BRCA') + 
  theme(plot.title = element_text(hjust = 0.5))

## ----message = FALSE, warning = FALSE, eval=TRUE------------------------------
sessionInfo()

