# biclustermd
[![CRAN](http://www.r-pkg.org/badges/version-last-release/biclustermd)](https://cran.r-project.org/package=biclustermd) 
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/biclustermd)](https://cran.r-project.org/package=biclustermd)

See the vignette "Airports.rmd" for a walk-through of the package and how it is used.

Here is a toy example which is also found in the help page for `biclustermd()`. 

```r
devtools::install_github("jreisner/biclustermd")
library(biclustermd)
?biclustermd

# EXAMPLE 1 ----
data("synthetic")
# default parameters -- col_clusters = sqrt(ncol(synthetic)), row_clusters = sqrt(nrow(synthetic))
bc <- biclustermd(synthetic)
bc
autoplot(bc)

# providing the true number of row and column clusters
bc <- biclustermd(synthetic, col_clusters = 3, row_clusters = 2)
bc
autoplot(bc)
# plot the similarity indices
autoplot(bc$Similarites)
autoplot(bc$Similarites, facet = FALSE)

# view the decrease in SSE from iteration to iteration
autoplot(bc$SSE)

# one could use a linear model to predict the missing values in cell (1, 1):
bc_subset <- gather(bc) %>% filter(row_group == 1, col_group == 2)
bc_subset_model <- lm(value ~ col_name + row_name, data = bc_subset)
summary(bc_subset_model)
predict(bc_subset_model, bc_subset)
# this is a perfect biclustering so the variation in the cell is zero. 

# Another synthetic dataset with noise can demonstrate:
# EXAMPLE 2 ----
# first argument to kronecker() defines 4 row clusters and 4 column clusters
dat <- kronecker(matrix(1:16, nrow = 4, ncol = 4), matrix(5, nrow = 4, ncol = 4))
set.seed(29)
# make 35% of values missing
dat[sample(1:length(dat), 0.35 * length(dat))] <- NA
# randomly shuffle the matrix
dat <- dat[sample(1:nrow(dat), nrow(dat)), sample(1:ncol(dat), ncol(dat))]
# add N(0, 1) noise to each observation
dat <- dat + rnorm(prod(dim(dat)))

# repeat biclustering 50 times and keep the lowest SSE result
rep_dat_bc <- rep_biclustermd(dat, nrep = 50, col_clusters = 4, row_clusters = 4)
rep_dat_bc

plot(rep_dat_bc$rep_sse, type = 'o')
plot(cummin(rep_dat_bc$rep_sse), type = 'o')

autoplot(rep_dat_bc$best_bc)

# could choose any cell, but I'm picking (1, 3) since all rows and columns have
#   at least 2 observations
dat_bc_sub <- gather(rep_dat_bc$best_bc) %>% 
  filter(row_group == 1, col_group == 3)

dat_bc_sub_model <- lm(value ~ col_name + row_name, data = dat_bc_sub)
summary(dat_bc_sub_model)
anova(dat_bc_sub_model)
# neither covariate is useful, but that's because the biclustering did it's job

dat_bc_sub <- dat_bc_sub %>% 
  modelr::add_predictions(dat_bc_sub_model) %>% 
  modelr::add_residuals(dat_bc_sub_model)
sqrt(mean(dat_bc_sub$resid ^ 2, na.rm = TRUE))  

dat_bc_sub %>% 
  ggplot(aes(row_name, col_name, fill = pred)) +
  geom_tile() +
  ggtitle("predicted values")

dat_bc_sub %>% 
  ggplot(aes(row_name, col_name, fill = resid)) +
  geom_tile() +
  ggtitle("linear model residuals")

```
