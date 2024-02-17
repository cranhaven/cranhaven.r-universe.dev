# utr.annotation R package

This package can be used to annotate potential deleterious variants in the UTR regions for both human and mouse species. Given a CSV or VCF format variant file, utr.annotation provides information of each variant on whether and how it alters disrupts six known translational regulators including: upstream Open Reading Frames (uORFs), upstream Kozak sequences, polyA signals, the Kozak sequence at the annotated translation initiation site, start codon, and stop codon, conservation scores in the variant position, and whether and how it changes ribosome loading based on a model from empirical data. 

The package has been tested on Linux and MacOS (Intel chip).  If you are on Windows, getting conservation scores and ribosome loading prediction may not work, you could skip those two. 

## Input
   > Here is an example input file

| Chr | Pos | Ref | Alt |
| :---: | :---: | :---: | :---: |
| chr1 | 1308643 | CAT | C |
| chr12 | 69693767 | G | T |
| chr12 | 4685084 |	A |	G |
| chr15 | 45691267 | A | G |

## Feature implementation
  > Here is a description of the key terms

| Term | Definition & Implementation |
| :---: | :---: |
| uAUG | A "ATG" sequence in 5' UTR |
| # uAUG | Number of uAUG in 5' UTR |
| uKozak | A 7nt sequence [GA]..ATGG in 5' UTR |
| # uKozak | Number of uKozak sequences in 5' UTR |
| PolyA signal | A 6nt sequence AATAAA or ATTAAA |
| # PolyA signal | Number of PolyA signal in 3' UTR |
| Stop codon | A stop codon of a transcript is the last three nucleotides of its CDS. |
| Lost stop codon | The tool annotates whether a variant disrupt the stop codon. If the alternative stop codon sequence of a transcript is not "TAG", "TAA", or "TGA", the tool will annotate lost_stop_codon as TRUE. |
| Start codon	| A start codon of a transcript is the first three nucleotides of its CDS |
| Lost start codon | The tool annotates whether a variant disrupt the start codon. If the alternative start codon sequence is not "ATG", the tool will annotate lost_start_codon as TRUE. |
| TSS Kozak	| A TSS Kozak sequence of a transcript is defined in this paper as a 8nt sequence, the first three nuleotides are the last three nuleotides of its 5’ UTR sequence and the following five nuleotides are the first five nuleotides of its coding sequence. |
| TSS Kozak score	| We empirically defined a Position Weight Matrix for an ideal Kozak by using the top 20 Kozak’s described by  Sample et al., 2019 from uORFs that recruited ribosomes most efficiently.  We then compare the TSS Kozak of the ref and alt to this PWM to calculate a score for each, and to determine if a variant alters the Kozak score. |
| MRL | Mean ribosome load. A MRL of a transcript is predicted using a CNN model with 100nt 5' UTR sequence upstream of its start codon. The CNN was trained based on a massively parallel reporter assay of 5’UTR sequence’s impact on ribosome loading from (Sample, 2019). |

## UTR annotation results  
   > Here is a description of the key annotation columns output from the UTR annotation. 

 | Column name | Description | Values |
 | :---: |  :---: | :---: |
 | **Transcript** | whether the variant overlaps with any protein coding transcript, if so, list all transcript ids | NA or a list of transcript ids, separated by ";" |
 | **transcript_id** | whether any transcript in Transcript column has a valid start codon and stop codon, if so, list all the valid transcript ids | NA or a list of transcript ids, separated by ";" |
 | **utr3_transcript_id** | Whether the variant overlaps with the 3' UTR of any transcript, if so, list all transcripts ids | NA / a list of transcript ids, separated by ";" |
 | **utr5_transcript_id** | Whether the variant overlaps with the 5' UTR of any transcript, if so, list all transcripts ids | NA / a list of transcript ids, separated by ";" |
 | **cds_transcript_id** | Whether the a variant overlaps with the CDS of any transcript, if so, list all transcripts ids | NA / a list of transcript ids, separated by ";" |
 | **num_uAUG** | If utr5_transcript_id is NA, also NA here; Otherwise, list the counts of AUG in 5' UTR of each transcript listed in utr5_transcript_id | NA / a list of numbers, separated by ";" |
 | **num_uAUG_altered** | If utr5_transcript_id is NA, also NA here; Otherwise, list the counts of AUG in **altered** 5' UTR of each transcript listed in utr5_transcript_id | NA / a list of numbers, separated by ";" |
 | **utr_num_uAUG_gainedOrLost** | If utr5_transcript_id is NA, also NA here; Otherwise, check the counts difference before and after alter | NA / a list of comparison results (equal / gained / lost) |
 | **num_kozak** | If utr5_transcript_id is NA, also NA here; Otherwise, list the counts of the Kozak sequence in 5' UTR of each transcript listed in utr5_transcript_id | NA / a list of numbers, separated by ";" |
 | **num_kozak_altered** | If utr5_transcript_id is NA, also NA here; Otherwise, list the counts of the Kozak sequence in **altered** 5' UTR of each transcript listed in utr5_transcript_id | NA / a list of numbers, separated by ";" |
 | **utr_num_kozak_gainedOrLost** | If utr5_transcript_id is NA, also NA here; Otherwise, check the counts difference before and after alter | NA / a list of comparison results (equal / gained / lost) |
 | **mrl** | If utr5_transcript_id is NA, also NA here; Otherwise, list the MRL prediction on 5' UTR sequences of each transcript listed in utr5_transcript_id | NA / a list of MRL predictions, separated by ";" |
 | **mrl_altered** | If utr5_transcript_id is NA, also NA here; Otherwise, list the MRL prediction on **altered** 5' UTR sequence of each transcript listed in utr5_transcript_id | NA / a list of MRL predictions, separated by ";" |
 | **mrl_gainedOrLost** | If utr5_transcript_id is NA, also NA here; Otherwise, check if gain or lost ribosome load after alteration | NA / a list of comparison results (equal / gained / lost) |
 | **num_polyA_signal** | If utr3_transcript_id is NA, also NA here; Otherwise, list the counts of the polyA sequence in 3' UTR of each transcript listed in utr3_transcript_id | NA / a list of numbers, separated by ";" |
 | **num_polyA_signal_altered** | If utr3_transcript_id is NA, also NA here; Otherwise, list the counts of the polyA sequence in **altered** 3' UTR of each transcript listed in utr3_transcript_id | NA / a list of numbers, separated by ";" |
 | **num_polyA_signal_gainedOrLost** | If utr3_transcript_id is NA, also NA here; Otherwise, check the counts difference before and after alter | NA / a list of comparison results (equal / gained / lost) |
 | **stopCodon_transcript_id** | Whether it is a variant in stop codon region, if so, list all transcripts ids | NA / a list of transcript ids, separated by ";" |
 | **stopCodon_positions** | If stopCodon_transcript_id is NA, also NA here; Otherwise, list the three stop codons' coordinates (separated by "&#124;" ) of each transcript listed in stopCodon_transcript_id | NA / a list of three-numbers, separated by ";" |
 | **stop_codon** | If stopCodon_transcript_id is NA, also NA here; Otherwise, list the stop codon sequence of each transcript listed in stopCodon_transcript_id | NA / a list of sequences, separated by ";" |
 | **stop_codon_altered** | If stopCodon_transcript_id is NA, also NA here; Otherwise, list the **altered** stop codon sequence of each transcript listed in stopCodon_transcript_id | NA / a list of sequences, separated by ";" |
 | **lost_stop_codon** | If stopCodon_transcript_id is NA, also NA here; Otherwise, check if lost the stop codon after alteration | NA / a list of boolean (TRUE/FALSE) separated by ";" |
 | **startCodon_transcript_id** | Whether it is a variant in start codon region, if so, list all transcripts ids | NA / a list of transcript ids, separated by ";" |
 | **startCodon_positions** | If startCodon_transcript_id is NA, also NA here; Otherwise, list the three start codons' coordinates (separated by "&#124;" ) of each transcript listed in startCodon_transcript_id | NA / a list of three-numbers, separated by ";" |
 | **start_codon** | If startCodon_transcript_id is NA, also NA here; Otherwise, list the start codon sequence of each transcript listed in startCodon_transcript_id | NA / a list of sequences, separated by ";" |
 | **start_codon_altered** | If startCodon_transcript_id is NA, also NA here; Otherwise, list the **altered** start codon sequence of each transcript listed in startCodon_transcript_id | NA / a list of sequences, separated by ";" |
 | **lost_start_codon** | If startCodon_transcript_id is NA, also NA here; Otherwise, check if lost the start codon after alteration | NA / a list of boolean (TRUE/FALSE) separated by ";" |
 | **kozak_transcript_id** | Whether the variant overlaps with the Kozak region at translation initiation site of any transcript, if so, list all transcripts ids | NA / a list of transcript ids, separated by ";" |
 | **kozak_positions** | If kozak_transcript_id is NA, also NA here; Otherwise, list the coordinates of each nucleotide in the Kozak sequence (separated by "&#124;" ) for each transcript listed in kozak_transcript_id | NA / a list of eight-numbers, separated by ";" |
 | **kozak** | If kozak_transcript_id is NA, also NA here; Otherwise, list the Kozak sequence of each transcript listed in kozak_transcript_id | NA / a list of sequences, separated by ";" |
 | **kozak_altered** | If kozak_transcript_id is NA, also NA here; Otherwise, list the **altered** Kozak sequence of each transcript listed in kozak_transcript_id | NA / a list of sequences, separated by ";" |
 | **kozak_score** | If kozak_transcript_id is NA, also NA here; Otherwise, list the score of the Kozak sequence of each transcript listed in kozak_transcript_id | NA / a list of numbers, separated by ";" |
 | **kozak_altered_score** | If kozak_transcript_id is NA, also NA here; Otherwise, list the score of the **altered** Kozak sequence of each transcript listed in kozak_transcript_id | NA / a list of numbers, separated by ";" |
 | **tss_kozak_score_gainedOrLost** | If kozak_transcript_id is NA, also NA here; Otherwise, check the Kozak score difference before and after alter | NA / a list of comparison results (equal / gained / lost), separated by ";" |
 
 
## Installation

### Install dependencis

```
cran_pkgs <- c("parallel", "doParallel", "data.table", "readr", "stringr", "vcfR", "dplyr", "tidyr", "keras", "devtools", "reticulate")
bioc_pkgs <- c("biomaRt", "Biostrings", "AnnotationHub", "ensembldb")

for (pkg in cran_pkgs) {
  if (!(pkg %in% installed.packages())) {
    install.packages(pkg)
  }
}

if (!requireNamespace("BiocManager", quietly = TRUE)) {
      install.packages("BiocManager")
}

for (pkg in bioc_pkgs) {
  if (!(pkg %in% installed.packages())) {
    BiocManager::install(pkg)
  }
}

# Install keras package for MRL prediction. 
library(keras)
# Install tensorflow backend
reticulate::install_miniconda()
keras::install_keras(version = "2.2.4", tensorflow = "1.14.0", method = "conda")

# install deep learning model data package
devtools::install_bitbucket("jdlabteam/mrl.dl.model")

```

### Install utr.annotation package

```
# Install the release version from CRAN
install.packages("utr.annotation")

# Or install the latest version from Bitbucket
devtools::install_bitbucket("jdlabteam/utr.annotation")
```

## Usage
 
[Introduction to utr.annotation package](https://wustl.box.com/s/50a51fbmudpti6jy4ghzo67dgd710l5a)
 
## Citation
 Y Liu, JD Dougherty. 2021. utR.annotation: a tool for annotating genomic variants that could influence post-transcriptional regulation. bioRxiv doi: 10.1101/2021.06.23.449510
