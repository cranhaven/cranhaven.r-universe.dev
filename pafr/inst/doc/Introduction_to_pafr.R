## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  fig.height=4,
  fig.width=6,
  fig.align = 'center'
)

## ----read---------------------------------------------------------------------
library(pafr)
path_to_fungal_alignment <- system.file("extdata", "fungi.paf", package = "pafr")
ali <- read_paf(path_to_fungal_alignment)

## ----print--------------------------------------------------------------------
ali

## ---- alen--------------------------------------------------------------------
library(ggplot2)
library(ggpubr)

ggplot(ali, aes(alen, dv)) + 
    geom_point(alpha=0.6, colour="steelblue", size=2) + 
    scale_x_continuous("Alignment length (kb)", label =  function(x) x/ 1e3) +
    scale_y_continuous("Per base divergence") + 
    theme_pubr()

## ---- compare_q---------------------------------------------------------------
by_q <- aggregate(dv ~ qname, data=ali, FUN=mean)
knitr::kable(by_q)

## ---- include-tags------------------------------------------------------------
microbenchmark::microbenchmark(
  tags = read_paf(path_to_fungal_alignment),
  no_tags = read_paf(path_to_fungal_alignment, include_tags=FALSE),
  times=10
)

## ---- prim_only---------------------------------------------------------------
prim_alignment <- filter_secondary_alignments(ali)
nrow(ali) - nrow(prim_alignment)

## ---- remove_short------------------------------------------------------------
long_ali <- subset(ali, alen > 1e4 & mapq > 40)
long_ali

## ----dotplot_plain, fig.height=6----------------------------------------------
dotplot(prim_alignment)

## ----dot_qstart, fig.height=6-------------------------------------------------
dotplot(prim_alignment, label_seqs=TRUE, order_by="qstart") + theme_bw()

## ----dot_provided, fig.height=6-----------------------------------------------
to_keep <- list(
        c("Q_chr1", "Q_chr5", "Q_chr4", "Q_chr6"),
        c("T_chr2", "T_chr5", "T_chr3", "T_chr6")
)
dotplot(prim_alignment, label_seqs=TRUE, order_by="provided", ordering=to_keep)

## -----------------------------------------------------------------------------
path_to_centro <- system.file("extdata", "Q_centro.bed", package = "pafr")
centro <- read_bed(path_to_centro)
knitr::kable(head(centro))

## ----highlight, fig.height=6--------------------------------------------------
dotplot(prim_alignment, "qstart") + 
    highlight_query(bed=centro)

## ----highlight_t, fig.height=6------------------------------------------------
interval <- data.frame(chrom="T_chr3", start=2000000, end=3000000)
dotplot(prim_alignment, label_seqs=TRUE) + 
    highlight_target(interval)

## ---- fig.height = 4----------------------------------------------------------
plot_synteny(long_ali, q_chrom="Q_chr3", t_chrom="T_chr4", centre=TRUE) +
    theme_bw()

## ---- fig.height = 4----------------------------------------------------------
plot_synteny(long_ali, q_chrom="Q_chr5", t_chrom="T_chr5", centre=TRUE) +
    theme_bw()


## ---- fig.height = 4----------------------------------------------------------
plot_synteny(long_ali, q_chrom="Q_chr5", t_chrom="T_chr5", centre=TRUE, rc=TRUE) +
    theme_bw()

## ----cov_plot-----------------------------------------------------------------
plot_coverage(prim_alignment)   

## ---- cov_target--------------------------------------------------------------
plot_coverage(prim_alignment, target=FALSE)   

## ---- cov_colourful-----------------------------------------------------------
plot_coverage(prim_alignment, fill='qname') + scale_fill_brewer(palette="Set1")

