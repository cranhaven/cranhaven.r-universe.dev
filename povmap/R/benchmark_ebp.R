# Internal documentation -------------------------------------------------------

# Benchmark function for the EBP with a national value--------------------------

# This function is called within the EBP-function and the agruments benchmark
# and benchmark_type are documented there.

benchmark_ebp_national <- function (point_estim, framework, fixed, benchmark,
                           benchmark_type) {


  if (!is.numeric(benchmark)) {

    benchmark_ <- rep(NA, length(benchmark))
    names(benchmark_) <- benchmark

    if (is.list(point_estim)) {# for point_estimation.R

      estim <- as.list(point_estim$ind[benchmark])
      for (i in benchmark) {# weighted national level
        if (i == "Mean") {
          benchmark_[i] <- weighted.mean(framework$smp_data[[paste0(fixed[2])]],
                    framework$smp_data[[framework$benchmark_weights]])
        } else if (i == "Head_Count") {
          benchmark_[i] <-
            weighted.mean(framework$smp_data[[paste0(fixed[2])]] <
                            framework$threshold,
                          framework$smp_data[[framework$benchmark_weights]])
        }
      }

    } else {# for mse_estimation.R
      estim <- as.list(as.data.frame(point_estim)[benchmark])
      for (i in benchmark) {# MSE - no weights in bootstrap sample
        if (i == "Mean") {
          benchmark_[i] <- mean(framework$smp_data[[paste0(fixed[2])]])
        } else if (i == "Head_Count") {
          benchmark_[i] <- mean(framework$smp_data[[paste0(fixed[2])]] <
                                           framework$threshold)
        }
      }
    }

    benchmark <- benchmark_

  } else {# raking and ratio with fixed national value
    if (is.list(point_estim)) {
      estim <- as.list(point_estim$ind[names(benchmark)])
    } else {
      estim <- as.list(as.data.frame(point_estim)[names(benchmark)])
    }
  }

  EBP_bench <- as.list(as.data.frame(
    matrix(NA, nrow = length(estim[[1]]), ncol = length(benchmark))
  ))
  names(EBP_bench) <- names(benchmark)

  if (is.null(framework$pop_weights)) {
    if (is.null(framework$aggregate_to)) {
      share <- framework$n_pop / framework$N_pop
    } else {
      share <- as.numeric(table(framework$aggregate_to_vec)) / framework$N_pop
    }
  } else {
    if (is.null(framework$aggregate_to)) {
      share <- tapply(X = framework$pop_data[[framework$pop_weights]],
                      INDEX = framework$pop_data[[framework$smp_domains]],
                      FUN = sum) /
        sum(framework$pop_data[[framework$pop_weights]])
    } else {
      share <- tapply(X = framework$pop_data[[framework$pop_weights]],
                      INDEX = framework$aggregate_to_vec,
                      FUN = sum) /
        sum(framework$pop_data[[framework$pop_weights]])
    }
  }


  for(i in names(benchmark)) {

    if (benchmark_type == "raking") {
      EBP_bench[[i]] <- estim[[i]] + benchmark[[i]] - sum(share * estim[[i]])
    } else if (benchmark_type == "ratio") {
      phi <- share / estim[[i]]
      EBP_bench[[i]] <- estim[[i]] + (1 / (sum(share^2 / phi))) *
        (benchmark[[i]] - sum(share * estim[[i]])) * (share / phi)
    } else if (benchmark_type == "ratio_complement" && i == "Head_Count") {
      estim_complement[[i]] <- 1-estim[[i]]
      benchmark_complement[[i]] <-1-benchmark[[i]]
      EBP_bench_complement[[i]] <- estim[[i]] + (1 / (sum(share^2 / phi))) *
        (benchmark_complement[[i]] - sum(share * estim[[i]])) * (share / phi)
      EBP_bench[[i]] <- 1 - EBP_bench_complement
    }
  }

  names(EBP_bench) <- c(paste0(names(benchmark),"_bench"))

  if (is.list(point_estim)) {
    point_estim_bench <- data.frame(point_estim$ind, EBP_bench)
  } else {
    point_estim_bench <- as.matrix(data.frame(point_estim, EBP_bench))
  }

  return(point_estim_bench)
}

# Benchmark function for the EBP with a variable domain value ------------------

# This function is called within the EBP-function and the agruments benchmark
# and benchmark_type are documented there.

benchmark_ebp_level <- function (point_estim, framework, fixed, benchmark,
                                 benchmark_type, benchmark_level) {

  if (!(is.numeric(benchmark) || is.data.frame(benchmark))) {
    benchmark_ <- data.frame(unique(framework$pop_data[[benchmark_level]]),
                             as.data.frame(matrix(nrow=length(unique(framework$pop_data[[benchmark_level]])),ncol=length(benchmark))))

    names(benchmark_) <- c(benchmark_level, benchmark)

    if (is.list(point_estim)) {# for point_estimation.R

      estim <- as.list(point_estim$ind[benchmark])
      for (i in benchmark) {# weighted on benchmark_level
        for (j in benchmark_[[benchmark_level]]) {
          if (i == "Mean") {
            benchmark_[which(benchmark_[benchmark_level] == j), i] <-
              weighted.mean(
                framework$smp_data[
                  framework$smp_data[benchmark_level] == j, paste0(fixed[2])
                ],
                framework$smp_data[
                  framework$smp_data[benchmark_level] == j,
                  framework$benchmark_weights
                ])
          } else if (i == "Head_Count") {
            benchmark_[which(benchmark_[benchmark_level] == j), i] <-
              weighted.mean(
                framework$smp_data[
                  framework$smp_data[benchmark_level] == j, paste0(fixed[2])
                ] < framework$threshold,
                framework$smp_data[
                  framework$smp_data[benchmark_level] == j,
                  framework$benchmark_weights
                ])
          }
        }

      }

    } else {# for mse_estimation.R
      estim <- as.list(as.data.frame(point_estim)[benchmark])
      for (i in benchmark) {# MSE - no weights in bootstrap sample
        for (j in benchmark_[[benchmark_level]]) {
          if (i == "Mean") {
            benchmark_[which(benchmark_[benchmark_level] == j), i] <-
              weighted.mean(
                framework$smp_data[
                  framework$smp_data[benchmark_level] == j, paste0(fixed[2])
                ],
                framework$smp_data[
                  framework$smp_data[benchmark_level] == j,
                  framework$benchmark_weights
                ])
          } else if (i == "Head_Count") {
            benchmark_[which(benchmark_[benchmark_level] == j), i] <-
              weighted.mean(
                framework$smp_data[
                  framework$smp_data[benchmark_level] == j, paste0(fixed[2])
                ] < framework$threshold,
                framework$smp_data[
                  framework$smp_data[benchmark_level] == j,
                  framework$benchmark_weights
                ])
          }
        }
      }
    }

    benchmark <- benchmark_

  } else {# raking and ratio with fixed national value
    if (is.list(point_estim)) {
      if (is.numeric(benchmark)) {
        estim <- as.list(point_estim$ind[names(benchmark)])
      } else {
        estim <- as.list(point_estim$ind[names(benchmark)[-1]])
      }
    } else {
      if (is.numeric(benchmark)) {
        estim <- as.list(as.data.frame(point_estim)[names(benchmark)])
      } else {
        estim <- as.list(as.data.frame(point_estim)[names(benchmark)[-1]])
      }
    }
  }

  EBP_bench <- as.list(as.data.frame(
    matrix(NA, nrow = length(estim[[1]]), ncol = length(benchmark) - 1)
  ))

  names(EBP_bench) <- names(benchmark)[-1]

  for (j in benchmark[[benchmark_level]]) {

    pop_tmp <- framework$pop_data[framework$pop_data[[benchmark_level]] == j,]
    if (is.null(framework$pop_weights)) {
      if (is.null(framework$aggregate_to)) {
        share <- table(as.character(pop_tmp[[framework$smp_domains]])) / nrow(pop_tmp)
        estim_levels_num <- pmatch(names(share), unique(framework$pop_domains_vec))
      } else {
        share <- table(as.character(pop_tmp[[framework$aggregate_to]])) / nrow(pop_tmp)
        estim_levels_num <- pmatch(names(share), unique(framework$aggregate_to_vec))
      }
    } else {
      if (is.null(framework$aggregate_to)) {
        share <- tapply(X = pop_tmp[[framework$pop_weights]],
                        INDEX = pop_tmp[[framework$smp_domains]],
                        FUN = sum) / sum(pop_tmp[[framework$pop_weights]])
        share <- share[!is.na(share)]
        estim_levels_num <- pmatch(names(share), unique(framework$pop_domains_vec))
      } else {
        share <- tapply(X = pop_tmp[[framework$pop_weights]],
                        INDEX = pop_tmp[[framework$aggregate_to]],
                        FUN = sum) / sum(pop_tmp[[framework$pop_weights]])
        share <- share[!is.na(share)]
        estim_levels_num <- pmatch(names(share), unique(framework$aggregate_to_vec))
      }
    }


    for(i in names(benchmark)[-1]) {
      if (benchmark_type == "raking") {
        EBP_bench[[i]][estim_levels_num] <-
          estim[[i]][estim_levels_num] +
          benchmark[[i]][benchmark[[benchmark_level]] == j] -
          sum(share * estim[[i]][estim_levels_num])
      } else if (benchmark_type == "ratio") {
        EBP_bench[[i]][estim_levels_num] <- estim[[i]][estim_levels_num]*
          (benchmark[[i]][benchmark[[benchmark_level]] == j]/
             sum(share * estim[[i]][estim_levels_num]))
      } else if (benchmark_type == "ratio_complement" && i == "Head_Count") {
        EBP_bench[[i]][estim_levels_num] <- 1-((1-estim[[i]][estim_levels_num])*
          (1-benchmark[[i]][benchmark[[benchmark_level]] == j])/
             sum(share * (1-estim[[i]][estim_levels_num])))
      }
    }
  }

  names(EBP_bench) <- c(paste0(names(benchmark)[-1],"_bench"))

  if (is.list(point_estim)) {
    point_estim_bench <- data.frame(point_estim$ind, EBP_bench)
  } else {
    point_estim_bench <- as.matrix(data.frame(point_estim, EBP_bench))
  }

  return(point_estim_bench)
}
