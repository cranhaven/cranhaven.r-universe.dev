## ----runHaplot, eval=FALSE-----------------------------------------------
#  
#  library(microhaplot)
#  
#  run.label <- "sebastes"
#  
#  sam.path <- tempdir()
#  untar(system.file("extdata",
#                    "sebastes_sam.tar.gz",
#                    package="microhaplot"),
#        exdir = sam.path)
#  
#  
#  label.path <- file.path(sam.path, "label.txt")
#  vcf.path <- file.path(sam.path, "sebastes.vcf")
#  
#  mvShinyHaplot(tempdir())
#  app.path <- file.path(tempdir(), "microhaplot")
#  
#  haplo.read.tbl <- prepHaplotFiles(run.label = run.label,
#                              sam.path = sam.path,
#                              out.path = tempdir(),
#                              label.path = label.path,
#                              vcf.path = vcf.path,
#                              app.path = app.path)

