## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE,
                      fig.width=7, fig.height=5)

## ---- echo= TRUE, eval= FALSE-------------------------------------------------
#  library(data.table)
#  library(acs)
#  library(synthACS)
#  library(retry)
#  
#  ca_geo <- geo.make(state = "CA", county = "*")
#  ca_dat_SMSM <- pull_synth_data(2014, 5, ca_geo)

## ---- echo= TRUE, eval= FALSE-------------------------------------------------
#  # split()
#  n_splits <- 20
#  split_ca_dat <- split(ca_dat_SMSM, n_splits = n_splits)
#  tmp_opts <- vector("list", length= n_splits)
#  
#  for (i in 1:n_splits) {
#      # Section 3.3 of introduction: SMSM via simulated annealing
#      # derive synthetic datasets
#      tmp_synth <- derive_synth_datasets(split_ca_dat[[i]], leave_cores = 0)
#  
#      # create constraints for simulated annealing
#      a <- all_geog_constraint_age(tmp_synth, method = "macro.table")
#      g <- all_geog_constraint_gender(tmp_synth, method = "macro.table")
#      m <- all_geog_constraint_marital_status(tmp_synth, method = "macro.table")
#      r <- all_geog_constraint_race(tmp_synth, method = "synthetic")
#      e <- all_geog_constraint_edu(tmp_synth, method = "synthetic")
#  
#      cll <- all_geogs_add_constraint(attr_name = "age", attr_total_list = a,
#                                      macro_micro = tmp_synth)
#      cll <- all_geogs_add_constraint(attr_name = "gender", attr_total_list = g,
#                                      macro_micro = tmp_synth, constraint_list_list = cll)
#      cll <- all_geogs_add_constraint(attr_name = "marital_status", attr_total_list = m,
#                                      macro_micro = tmp_synth, constraint_list_list = cll)
#      cll <- all_geogs_add_constraint(attr_name = "race", attr_total_list = r,
#                                      macro_micro = tmp_synth, constraint_list_list = cll)
#      cll <- all_geogs_add_constraint(attr_name = "edu_attain", attr_total_list = e,
#                                      macro_micro = tmp_synth, constraint_list_list = cll)
#  
#      # anneal
#      tmp_opts[[i]] <- all_geog_optimize_microdata(tmp_synth, seed = 6550L, verbose = TRUE,
#                                            constraint_list_list = cll, p_accept = 0.4, max_iter = 10000L)
#  }
#  
#  # create the string needed for combine_smsm().
#  paste0("tmp_opts[[", 1:n_splits, "]]", sep= ", ", collapse= "")
#  # [1] "tmp_opts[[1]], tmp_opts[[2]], tmp_opts[[3]], tmp_opts[[4]], tmp_opts[[5]],
#  # tmp_opts[[6]], tmp_opts[[7]], tmp_opts[[8]], tmp_opts[[9]], tmp_opts[[10]],
#  # tmp_opts[[11]], tmp_opts[[12]], tmp_opts[[13]], tmp_opts[[14]], tmp_opts[[15]],
#  # tmp_opts[[16]], tmp_opts[[17]], tmp_opts[[18]], tmp_opts[[19]], tmp_opts[[20]], "
#  
#  # copy and paste the resulting string, excluding the final trailing comma
#  opt_ca <- combine_smsm(tmp_opts[[1]], tmp_opts[[2]], tmp_opts[[3]], tmp_opts[[4]], tmp_opts[[5]],
#                         tmp_opts[[6]], tmp_opts[[7]], tmp_opts[[8]], tmp_opts[[9]], tmp_opts[[10]],
#                         tmp_opts[[11]], tmp_opts[[12]], tmp_opts[[13]], tmp_opts[[14]],
#                         tmp_opts[[15]], tmp_opts[[16]], tmp_opts[[17]], tmp_opts[[18]],
#                         tmp_opts[[19]], tmp_opts[[20]])

