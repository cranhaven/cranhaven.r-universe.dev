## ----eval = FALSE-------------------------------------------------------------
#  library(MAAPER)
#  
#  pas_annotation = readRDS("./mouse.PAS.mm9.rds")
#  gtf = "./gencode.mm9.chr19.gtf"
#  # bam file of condition 1 (could be a vector if there are multiple samples)
#  bam_c1 = "./NT_chr19_example.bam"
#  # bam file of condition 2 (could be a vector if there are multiple samples)
#  bam_c2 = "./AS_4h_chr19_example.bam"
#  
#  maaper(gtf, # full path of the GTF file
#         pas_annotation, # PAS annotation
#         output_dir = "./", # output directory
#         bam_c1, bam_c2, # full path of the BAM files
#         read_len = 76, # read length
#         ncores = 12  # number of cores used for parallel computation
#        )

