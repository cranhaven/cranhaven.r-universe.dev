## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  library("SC.MEB")

## ----eval=FALSE---------------------------------------------------------------
#  library(mvtnorm)
#  library(GiRaF)
#  library(SingleCellExperiment)
#  set.seed(100)
#  G <- 4
#  Bet <- 1
#  KK <- 5
#  p <- 15
#  mu <- matrix(c( c(-6, rep(-1.5, 14)),
#                 rep(0, 15),
#                 c(6, rep(1.5, 14)),
#                 c(rep(-1.5, 7), rep(1.5, 7), 6),
#                 c(rep(1.5, 7), rep(-1.5, 7), -6)), ncol = KK)
#  height <- 70
#  width <- 70
#  n <- height * width # # of cell in each indviduals

## ----eval=FALSE---------------------------------------------------------------
#  X <- sampler.mrf(iter = n, sampler = "Gibbs", h = height, w = width, ncolors = KK, nei = G, param = Bet,initialise = FALSE, view = TRUE)
#  x <- c(X) + 1
#  y <- matrix(0, nrow = n, ncol = p)
#  
#  for(i in 1:n)	{ # cell
#    mu_i <- mu[, x[i]]
#    Sigma_i <- ((x[i]==1)*2 + (x[i]==2)*2.5 + (x[i]==3)*3 +
#                  (x[i]==4)*3.5 + (x[i]==5)*4)*diag(1, p)*0.3
#    y[i, ] <- rmvnorm(1, mu_i, Sigma_i)
#  }
#  
#  pos <- cbind(rep(1:height, width), rep(1:height, each=width))

## ----eval=FALSE---------------------------------------------------------------
#  # -------------------------------------------------
#  # make SC-MEB metadata used in SC-MEB
#  counts <- t(y)
#  rownames(counts) <- paste0("gene_", seq_len(p))
#  colnames(counts) <- paste0("spot_", seq_len(n))
#  
#  ## Make array coordinates - filled rectangle
#  cdata <- list()
#  nrow <- height; ncol <- width
#  cdata$row <- rep(seq_len(nrow), each=ncol)
#  cdata$col <- rep(seq_len(ncol), nrow)
#  cdata <- as.data.frame(do.call(cbind, cdata))
#  ## Scale and jitter image coordinates
#  #scale.factor <- rnorm(1, 8);  n_spots <- n
#  #cdata$imagerow <- scale.factor * cdata$row + rnorm(n_spots)
#  #cdata$imagecol <- scale.factor * cdata$col + rnorm(n_spots)
#  cdata$imagerow <- cdata$row
#  cdata$imagecol <- cdata$col
#  ## Make SCE
#  ## note: scater::runPCA throws warning on our small sim data, so use prcomp
#  sce <- SingleCellExperiment(assays=list(counts=counts), colData=cdata)
#  reducedDim(sce, "PCA") <- y
#  # sce$spatial.cluster <- floor(runif(ncol(sce), 1, 3))
#  
#  metadata(sce)$SCMEB.data <- list()
#  metadata(sce)$SCMEB.data$platform <- "ST"
#  metadata(sce)$SCMEB.data$is.enhanced <- FALSE

## ----eval=FALSE---------------------------------------------------------------
#  platform = "ST"
#  beta_grid = seq(0,4,0.2)
#  K_set= 2:10
#  parallel=TRUE
#  num_core = 3
#  PX = TRUE
#  maxIter_ICM = 10
#  maxIter = 50

## ----eval=FALSE---------------------------------------------------------------
#  Adj_sp <- getneighborhood_fast(as.matrix(pos), cutoff = 1.2)

## ----eval=FALSE---------------------------------------------------------------
#  Adj_sp <- find_neighbors2(sce, platform = platform)
#  Adj_sp[1:10,1:10]

## ----eval=FALSE---------------------------------------------------------------
#  fit = SC.MEB(y, Adj_sp, beta_grid = beta_grid, K_set= K_set, parallel=parallel, num_core = num_core, PX = PX, maxIter_ICM=maxIter_ICM, maxIter=maxIter)
#  str(fit[,1])

## ----eval=FALSE---------------------------------------------------------------
#  selectKPlot(fit, K_set = K_set, criterion = "BIC")

## ----eval=FALSE---------------------------------------------------------------
#  selectKPlot(fit, K_set = K_set, criterion = "MBIC")

## ----eval=FALSE---------------------------------------------------------------
#  out = selectK(fit, K_set = K_set, criterion = "BIC")
#  ClusterPlot(out, pos)

## ----eval=FALSE---------------------------------------------------------------
#  ClusterPlot(out, pos) +
#  theme_bw() +
#  xlab("Row") +
#  ylab("Column") +
#  labs(title="Spatial clustering")

