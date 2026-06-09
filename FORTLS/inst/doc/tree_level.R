## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include=FALSE------------------------------------------------------------
dir.data <- getwd()
download.file("https://www.dropbox.com/s/17yl25pbrapat52/PinusRadiata.laz?dl=1",
              destfile = file.path(dir.data, "PinusRadiata.laz"),
              mode = "wb")
download.file("https://www.dropbox.com/scl/fi/es5pfj87wj0g6y8414dpo/PiceaAbies.laz?rlkey=ayt21mbndc6i6fyiz2e7z6oap&dl=1",
              destfile = file.path(dir.data, "PiceaAbies.laz"),
              mode = "wb")
library(FORTLS)

## ----include=FALSE------------------------------------------------------------
singleLAS <- lidR::readLAS(paste(dir.data, "PinusRadiata.laz", sep = "/"))
lidR::plot(singleLAS, color = "RGB")

## ----eval = TRUE, include = TRUE----------------------------------------------
pcd.single <- normalize(las = "PinusRadiata.laz",
                        id = "PinusRadiata",
                        x.center = 0, y.center = 0,
                        max.dist = 10, 
                        scan.approach = "single",
                        threads = 2)

## ----eval = FALSE, include=FALSE----------------------------------------------
# head(pcd.single)

## ----echo=FALSE---------------------------------------------------------------
kableExtra::scroll_box(kable_input = kableExtra::kable(head(pcd.single), format = "html"), 
                       width = "100%")

## ----eval = FALSE, warning=FALSE, include=TRUE--------------------------------
# tls.resolution = list(point.dist = 6.34, tls.dist = 10)
# 
# tree.list.single.tls <- tree.detection.single.scan(data = pcd.single,
#                                                    tls.resolution = tls.resolution,
#                                                    threads = 2)

## ----eval = FALSE, include=FALSE----------------------------------------------
# head(tree.list.single.tls)

## ----echo=FALSE---------------------------------------------------------------
tree.list.single.tls <- read.csv(paste(dir.data, "tree.list.single.tls.csv", sep = "/"))
kableExtra::scroll_box(kable_input = kableExtra::kable(head(tree.list.single.tls), 
                                                       format = "html"), width = "100%")

## ----eval = FALSE, warning=FALSE, include=TRUE--------------------------------
# pcd.multi <- normalize(las = "PiceaAbies.laz",
#                        id = "PiceaAbies",
#                        x.center = 0, y.center = 0,
#                        scan.approach = "multi",
#                        threads = 2)
# 
# tree.list.multi.tls <- tree.detection.multi.scan(data = pcd.multi,
#                                                  d.mer = 20,
#                                                  threads = 2)

## ----eval = FALSE, include=TRUE-----------------------------------------------
# head(tree.list.multi.tls)

## ----echo=FALSE---------------------------------------------------------------
tree.list.multi.tls <- read.csv(paste(dir.data, "tree.list.multi.tls.csv", sep = "/"))
kableExtra::scroll_box(kable_input = kableExtra::kable(head(tree.list.multi.tls), 
                                                       format = "html"), width = "100%")

## ----eval = FALSE, include = FALSE--------------------------------------------
# diameter <- readLAS(paste(dir.data, "diameters.laz", sep = "/"))
# lidR::plot(singleLAS, color = "RGB", add = plot(diameter, color = "Intensity"))

## ----eval = FALSE, include= TRUE----------------------------------------------
# id <- c("PinusSylvestris1", "PinusSylvestris2")
# 
# center.coord <- data.frame(id = id,
#                            x = rep(0, length(id)),
#                            y = rep(0, length(id)))
# 
# tree.tls <- tree.detection.several.plots(las.list = c("PinusSylvestris1.laz",
#                                                       "PinusSylvestris2.laz"),
#                                          id.list = id,
#                                          center.coord = center.coord,
#                                          tls.resolution = list(point.dist = 7.67, tls.dist = 10),
#                                          max.dist = 7.5,
#                                          threads = 2)

