## ----setup, include = FALSE, echo = FALSE-------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
warning = FALSE, 
message = FALSE, 
fig.height = 7, 
fig.width=7, 
fig.align = "center")
library(knitr)
library(kableExtra)

## ---- echo=FALSE--------------------------------------------------------------
library(ggplot2)
library(ggpubr)
library(plotly)
library(usethis)

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("volcano3D")

## ---- eval = FALSE------------------------------------------------------------
#  library(devtools)
#  install_github("KatrionaGoldmann/volcano3D")

## -----------------------------------------------------------------------------
library(volcano3D)

## -----------------------------------------------------------------------------
data("example_data")

## -----------------------------------------------------------------------------
kable(table(syn_example_meta$Pathotype), col.names = c("Pathotype", "Count"))

## ---- echo=FALSE--------------------------------------------------------------
mytable = data.frame(
  outcome = c("outcome\ 
  \n\n(required)", 
  "Vector containing three-level factor indicating which of the three classes each sample belongs to."), 
  data = c("data\ 
              \n\n(required)", 
          "A dataframe or matrix containing data to be compared between the three classes (e.g. gene expression data). Note that variables are in columns, so gene expression data will need to be transposed. This \
           is used to calculate z-score and fold change, so for gene expression count data it should be \
           normalised such as log transformed or variance stabilised count transformation."),
  pvals = c("pvals\ 
              \n\n(optional)", 
              "the pvals matrix which contains the statistical\
                significance of probes or attributes between classes. This contains: \
              \n * the first column is a group test such as one-way ANOVA or Kruskal-Wallis test.
              \n * columns 2-4 contain p-values one for each comparison in the sequence A vs B, A vs C, B vs C, where A, B, C are the three levels in sequence in the outcome factor.
              For gene expression RNA-Seq count data, conduit functions
              using 'limma voom' or 'DESeq' pipelines to extract p-values for \
              analysis are provided in functions `deseq_polar()` and `voom_polar()`.\ 
              If p-values are not provided by the user, they can be calculated via the `polar_coords()` function.
              "),
  padj = c("padj\ 
              \n\n(optional)", 
              "Matrix containing the adjusted p-values matching the pvals matrix."),
  
  pcutoff = c("pcutoff", "Cut-off for p-value significance"),
  scheme = c("scheme", "Vector of colours starting with non-significant attributes"),
  labs = c("labs", 'Optional character vector for labelling classes. Default `NULL`
   leads to abbreviated labels based on levels in `outcome` using
   `abbreviate()`. A vector of length 3 with custom abbreviated names for the
   outcome levels can be supplied. Otherwise a vector length 7 is expected, of
   the form "ns", "B+", "B+C+", "C+", "A+C+", "A+", "A+B+", where "ns" means
   non-significant and A, B, C refer to levels 1, 2, 3 in `outcome`, and must
   be in the correct order.')
  
)

kable(t(mytable), row.names = FALSE, col.names = c("Variable", "Details")) %>%
  kable_styling(font_size=11)

## -----------------------------------------------------------------------------
data("example_data")

syn_polar <- polar_coords(outcome = syn_example_meta$Pathotype,
                          data = t(syn_example_rld))

## ----eval=FALSE---------------------------------------------------------------
#  library(DESeq2)
#  
#  # setup initial dataset from Tximport
#  dds <- DESeqDataSetFromTximport(txi = syn_txi,
#                                 colData = syn_metadata,
#                                 design = ~ Pathotype + Batch + Gender)
#  # initial analysis run
#  dds_DE <- DESeq(dds)
#  # likelihood ratio test on 'Pathotype'
#  dds_LRT <- DESeq(dds, test = "LRT", reduced = ~ Batch + Gender, parallel = TRUE)
#  
#  # create 'volc3d' class object for plotting
#  res <- deseq_polar(dds_DE, dds_LRT, "Pathotype")
#  
#  # plot 3d volcano plot
#  volcano3D(res)

## ----eval=FALSE---------------------------------------------------------------
#  library(limma)
#  library(edgeR)
#  
#  syn_tpm <- syn_txi$counts  # raw counts
#  
#  resl <- voom_polar(~ 0 + Pathotype + Batch + Gender, syn_metadata, syn_tpm)
#  
#  volcano3D(resl)

## ---- eval=FALSE--------------------------------------------------------------
#  radial_plotly(syn_polar)

## ---- eval=FALSE--------------------------------------------------------------
#  radial_plotly(syn_polar) %>% toWebGL()

## ---- fig.height=4.5, fig.width=7---------------------------------------------
radial_ggplot(syn_polar,
              marker_size = 2.3,
              legend_size = 10) +
  theme(legend.position = "right")

## ---- fig.height = 3.2, fig.width = 7-----------------------------------------
plot1 <- boxplot_trio(syn_polar,
                      value = "COBL",
                      text_size = 7,
                      test = "polar_padj",
                      my_comparisons=list(c("Lymphoid", "Myeloid"),
                                          c("Lymphoid", "Fibroid")))

plot2 <- boxplot_trio(syn_polar,
                      value = "COBL",
                      box_colours = c("violet", "gold2"),
                      levels_order = c("Lymphoid", "Fibroid"),
                      text_size = 7,
                      test = "polar_padj"
                      )

plot3 <- boxplot_trio(syn_polar,
                      value = "TREX2",
                      text_size = 7,
                      stat_size=2.5,
                      test = "polar_multi_padj",
                      levels_order = c("Lymphoid", "Myeloid", "Fibroid"),
                      box_colours = c("blue", "red", "green3"))

ggarrange(plot1, plot2, plot3, ncol=3)

## ---- eval=FALSE, fig.height=5------------------------------------------------
#  p <- volcano3D(syn_polar)
#  p

## ----volcano3D, echo = FALSE, message=FALSE, fig.align='center', out.width='80%', out.extra='style="border: 0;"'----
knitr::include_graphics("volcano3D.png")

## ---- eval=FALSE--------------------------------------------------------------
#  add_animation(p)

## ---- eval=FALSE--------------------------------------------------------------
#  p %>% plotly::config(toImageButtonOptions = list(format = "svg"))

## ---- eval=FALSE--------------------------------------------------------------
#  htmlwidgets::saveWidget(as_widget(p), "volcano3D.html")

## -----------------------------------------------------------------------------
citation("volcano3D")

