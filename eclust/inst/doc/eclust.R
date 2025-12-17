## ----eval=FALSE, echo=TRUE-----------------------------------------------
#  install.packages("pacman")
#  pacman::p_install_gh("sahirbhatnagar/eclust")

## ----setup, message=FALSE, echo=FALSE------------------------------------
library(knitr)
library(eclust)
library(data.table)
options(scipen = 1, digits = 5)

## ---- echo=FALSE---------------------------------------------------------
pander::pander(data.frame(`function name` = grep("^r_",pacman::p_functions("eclust"), value = T)))

## ---- echo=FALSE---------------------------------------------------------
pander::pander(data.frame(`function name` = grep("^s_",pacman::p_functions("eclust"), value = T)))

## ---- echo=FALSE---------------------------------------------------------
pander::pander(data.frame(`function name` = grep("^u_",pacman::p_functions("eclust"), value = T)))

## ---- eval = TRUE--------------------------------------------------------
# load the data
data("tcgaov")
tcgaov[1:5,1:6, with = FALSE]

# use log survival as the response
Y <- log(tcgaov[["OS"]])

# specify the environment variable
E <- tcgaov[["E"]]

# specify the matrix of genes only
genes <- as.matrix(tcgaov[,-c("OS","rn","subtype","E","status"),with = FALSE])

# for this example the training set will be all subjects.
# change `p` argument to create a train and test set.
trainIndex <- drop(caret::createDataPartition(Y, p = 1, list = FALSE, times = 1))
testIndex <- trainIndex

## ------------------------------------------------------------------------
cluster_res <- r_cluster_data(data = genes,
                              response = Y,
                              exposure = E,
                              train_index = trainIndex,
                              test_index = testIndex,
                              cluster_distance = "corr",
                              eclust_distance = "diffcorr",
                              measure_distance = "euclidean",
                              clustMethod = "hclust",
                              cutMethod = "dynamic",
                              method = "average",
                              nPC = 1,
                              minimum_cluster_size = 30)

# the number of clusters determined by the similarity matrices specified
# in the cluster_distance and eclust_distance arguments. This will always be larger
# than cluster_res$clustersAll$nclusters which is based on the similarity matrix
# specified in the cluster_distance argument
cluster_res$clustersAddon$nclusters

# the number of clusters determined by the similarity matrices specified
# in the cluster_distance argument only
cluster_res$clustersAll$nclusters

# what's in the cluster_res object
names(cluster_res)

## ------------------------------------------------------------------------
# prepare data for use with earth function
avg_eclust_interaction <- r_prepare_data(data = cbind(cluster_res$clustersAddon$averageExpr, 
                                                     Y = Y[trainIndex],
                                                     E = E[trainIndex]),
                                        response = "Y", exposure = "E")

head(avg_eclust_interaction[["X"]])

## ------------------------------------------------------------------------
# install and load earth package
pacman::p_load(char = "earth")

fit_earth <- earth::earth(x = avg_eclust_interaction[["X"]], y = avg_eclust_interaction[["Y"]], 
                          pmethod = "backward", 
                          keepxy = TRUE, degree = 2, trace = 1, nk = 1000)

coef(fit_earth)

## ------------------------------------------------------------------------
u_extract_selected_earth(fit_earth)

## ------------------------------------------------------------------------
# Genes in cluster 5
cluster_res$clustersAddonMembership[cluster %in% 5]

# variable importance
earth::evimp(fit_earth)

## ------------------------------------------------------------------------
pacman::p_load(eclust)
d0 <- s_modules(n = 100, p = 1000, rho = 0, exposed = FALSE,
                modProportions = c(0.15,0.15,0.15,0.15,0.15,0.25),
                minCor = 0.01,
                maxCor = 1,
                corPower = 1,
                propNegativeCor = 0.3,
                backgroundNoise = 0.5,
                signed = FALSE,
                leaveOut = 1:4)

d1 <- s_modules(n = 100, p = 1000, rho = 0.9, exposed = TRUE,
                modProportions = c(0.15,0.15,0.15,0.15,0.15,0.25),
                minCor = 0.4,
                maxCor = 1,
                corPower = 0.3,
                propNegativeCor = 0.3,
                backgroundNoise = 0.5,
                signed = FALSE)

# get the true cluster labels
truemodule1 <- d1$setLabels
table(truemodule1)

## ------------------------------------------------------------------------
pacman::p_load(magrittr)

X <- rbind(d0$datExpr, d1$datExpr) %>%
  magrittr::set_colnames(paste0("Gene", 1:1000)) %>%
  magrittr::set_rownames(paste0("Subject",1:200))

## ---- fig.show='hold', tidy=FALSE----------------------------------------
pacman::p_load(pheatmap)
pacman::p_load(viridis)

pheatmap::pheatmap(cor(X[1:100,]),
                   show_rownames = F, 
                   show_colnames = F, 
                   color = viridis(100))

pheatmap::pheatmap(cor(X[101:200,]),
                   show_rownames = F, 
                   show_colnames = F, 
                   color = viridis(100))

## ---- eval=T-------------------------------------------------------------
betaMainEffect <- vector("double", length = 1000)
betaMainInteractions <- vector("double", length = 1000)

# the first 25 in the 3rd block are active
betaMainEffect[which(truemodule1 %in% 3)[1:50]] <- runif(50, 0.9, 1.1)

# the first 25 in the 4th block are active
betaMainEffect[which(truemodule1 %in% 4)[1:50]] <- runif(50, 0.9, 1.1)

# the interaction effects
betaMainInteractions[which(betaMainEffect!=0)] <- runif(50, 0.4, 0.6)

# the environment effect
betaE <- 2

# the total beta vector
beta <- c(betaMainEffect, betaE, betaMainInteractions)

## ------------------------------------------------------------------------
result <- s_generate_data(p = 1000, 
                          X = X,
                          beta = beta,
                          include_interaction = TRUE,
                          cluster_distance = "tom",
                          n = 200, 
                          n0 = 100,
                          eclust_distance = "difftom",
                          signal_to_noise_ratio = 1,
                          distance_method = "euclidean",
                          cluster_method = "hclust",
                          cut_method = "dynamic",
                          agglomeration_method = "average",
                          nPC = 1)
names(result)

## ---- echo=FALSE---------------------------------------------------------
pander::pander(data.frame(`function name` = c("s_pen_separate", "s_pen_clust", "s_mars_separate", "s_mars_clust"),
           `General Approach` = rep(c("SEPARATE","CLUST, ECLUST"), 2),
           `model` = c(rep(c("lasso, elasticnet, mcp, scad"), 2), rep("MARS",2))))

## ------------------------------------------------------------------------
# Provide ECLUST clustering results to the gene_groups argument
pen_res <- s_pen_clust(x_train = result[["X_train"]],
                       x_test = result[["X_test"]],
                       y_train = result[["Y_train"]],
                       y_test = result[["Y_test"]],
                       s0 = result[["S0"]],
                       gene_groups = result[["clustersAddon"]],
                       summary = "pc",
                       model = "lasso",
                       exp_family = "gaussian",
                       clust_type = "ECLUST",
                       include_interaction = TRUE)
unlist(pen_res)

## ---- echo=FALSE---------------------------------------------------------
pander::pander(data.frame(`object name` = c("tom_train_all","tom_train_diff","tom_train_e1","tom_train_e0","corr_train_all","corr_train_diff","corr_train_e1","corr_train_e0","fisherScore","corScor")))

## ------------------------------------------------------------------------
# check that the object is of class similarity
class(result$tom_train_e1)

# get clustering tree
hc <- hclust(as.dist(1 - result$tom_train_e1), method = "average")

plot(result$tom_train_e1, 
     truemodule = truemodule1, 
     cluster_rows = hc, 
     cluster_cols = hc,
     active = as.numeric(betaMainEffect!=0))

## ------------------------------------------------------------------------
class(cluster_res)

## ------------------------------------------------------------------------
plot(cluster_res, show_column_names = FALSE)

