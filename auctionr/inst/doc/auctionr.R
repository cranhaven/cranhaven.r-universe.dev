## ----setup, include = FALSE---------------------------------------------------
library(auctionr)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
set.seed(5)
dat <- auction_generate_data(obs = 100, mu = 10, alpha = 2,
                             sigma = 0.2, beta = c(-1,1),
                             new_x_mean= c(-1,1),
                             new_x_sd = c(0.5,0.8))
head(dat)

## -----------------------------------------------------------------------------
## Standard error calculation fails in the following single run
res <- auction_model(dat, 
                      init_param =  c(8, 2, .5, .4, .6),
                      num_cores = 1,
                      std_err = TRUE)

res

## -----------------------------------------------------------------------------
## Solving the issue with multiple runs
res_list <- list()
max_llik <- c()
init_param0 = c(8, 2, .5, .4, .6)

set.seed(100)
for (i in 1:4){
   init_param = c(abs(init_param0[1:3]*rnorm(3) + 5*rnorm(3)), init_param0[4:5] + .5*rnorm(2))
   res <- auction_model(dat, init_param = init_param, num_cores = 1, std_err = TRUE)
   print(res)
   
   ## Only keeping results with valid standard errors
   if (all(!is.na(res$std_err))){
       res_list <- c(res_list, list(res))
       max_llik = c(max_llik, res$value)
   }
}

## -----------------------------------------------------------------------------
res_final <- res_list[[which.max(max_llik)]]
res_final

