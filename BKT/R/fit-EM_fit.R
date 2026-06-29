#' @import parallel

EM_fit <- function(model, data, tol = 0.005, maxiter = 100, parallel = TRUE, fixed = list()) {
  check_data(data)
  num_subparts <- nrow(data$data)
  num_resources <- length(model$learns)
  trans_softcounts <- array(0, dim = c(num_resources, 2, 2))
  emission_softcounts <- array(0, dim = c(num_subparts, 2, 2))
  init_softcounts <- matrix(0, nrow = 2, ncol = 1)
  log_likelihoods <- array(0, dim = c(maxiter, 1))

  result <- list(
    all_trans_softcounts = trans_softcounts,
    all_emission_softcounts = emission_softcounts,
    all_initial_softcounts = init_softcounts
  )

  for (i in seq_len(maxiter)) {
    result <- run(data, model, result$all_trans_softcounts, result$all_emission_softcounts, result$all_initial_softcounts, 1, parallel, fixed = fixed)

    for (j in seq_len(num_resources)) {
      result$all_trans_softcounts[j, , ] <- t(result$all_trans_softcounts[j, , ])
    }

    for (j in seq_len(num_subparts)) {
      result$all_emission_softcounts[j, , ] <- t(result$all_emission_softcounts[j, , ])
    }
    log_likelihoods[i, 1] <- result$total_loglike

    if ((i > 1) && (abs(log_likelihoods[i, 1] - log_likelihoods[i - 1, 1]) <= tol)) {
      break
    }

    model <- M_step_run(model, result$all_trans_softcounts, result$all_emission_softcounts, result$all_initial_softcounts, fixed = fixed)
  }

  return(list(model = model, log_likelihoods = log_likelihoods[1:i]))
}

# MARK: run
run <- function(data, model, trans_softcounts, emission_softcounts, init_softcounts, num_outputs, parallel = TRUE, fixed = list()) {
  # Processed Parameters
  alldata <- data$data
  bigT <- ncol(alldata)
  num_subparts <- nrow(alldata)
  allresources <- data$resources
  starts <- data$starts
  learns <- model$learns
  forgets <- model$forgets
  guesses <- model$guesses
  slips <- model$slips
  lengths <- data$lengths

  prior <- model$prior
  num_sequences <- length(starts)
  num_resources <- length(learns)
  normalizeLengths <- FALSE
  if (!is.null(fixed$prior)) {
    prior <- fixed$prior
  }
  initial_distn <- numeric(2)
  initial_distn[1] <- 1 - prior
  initial_distn[2] <- prior
  if (!is.null(fixed$learns)) {
    learns <- learns * (fixed$learns < 0) + fixed$learns * (fixed$learns >= 0)
  }
  if (!is.null(fixed$forgets)) {
    forgets <- forgets * (fixed$forgets < 0) + fixed$forgets * (fixed$forgets >= 0)
  }
  As <- matrix(0, nrow = 2, ncol = 2 * num_resources)
  As[1, seq(1, 2 * num_resources, by = 2)] <- 1 - learns
  As[1, seq(2, 2 * num_resources, by = 2)] <- forgets
  As[2, seq(1, 2 * num_resources, by = 2)] <- learns
  As[2, seq(2, 2 * num_resources, by = 2)] <- 1 - forgets

  if (!is.null(fixed$guesses)) {
    guesses <- guesses * (fixed$guesses < 0) + fixed$guesses * (fixed$guesses >= 0)
  }
  if (!is.null(fixed$slips)) {
    slips <- slips * (fixed$slips < 0) + fixed$slips * (fixed$slips >= 0)
  }
  Bn <- matrix(0, nrow = 2, ncol = 2 * num_subparts)
  Bn[1, seq(1, 2 * num_subparts, by = 2)] <- 1 - guesses
  Bn[1, seq(2, 2 * num_subparts, by = 2)] <- guesses
  Bn[2, seq(1, 2 * num_subparts, by = 2)] <- slips
  Bn[2, seq(2, 2 * num_subparts, by = 2)] <- 1 - slips
  # Outputs
  all_trans_softcounts <- matrix(0, nrow = 2, ncol = 2 * num_resources)
  all_emission_softcounts <- matrix(0, nrow = 2, ncol = 2 * num_subparts)
  all_initial_softcounts <- matrix(0, nrow = 2, ncol = 1)

  alpha_out <- matrix(0, nrow = 2, ncol = bigT)

  total_loglike <- 0

  input <- list(
    As = As,
    Bn = Bn,
    initial_distn = initial_distn,
    allresources = allresources,
    starts = starts,
    lengths = lengths,
    num_resources = num_resources,
    num_subparts = num_subparts,
    alldata = alldata,
    normalizeLengths = normalizeLengths,
    alpha_out = alpha_out
  )
  # parallel = FALSE

  get_x <- function(parallel, inner) {
    x <- list()  # Define x locally
    success_flag = FALSE
    if (parallel) {
      tryCatch(
        {
          num_threads <- if (parallel) parallel::detectCores() else 1
          thread_counts <- vector("list", num_threads)

          for (thread_num in seq_len(num_threads)) {
            blocklen <- 1 + ((num_sequences - 1) %/% num_threads)
            sequence_idx_start <- blocklen * (thread_num - 1)
            sequence_idx_end <- min(sequence_idx_start + blocklen, num_sequences)
            thread_counts[[thread_num]] <- c(
              list(sequence_idx_start = sequence_idx_start, sequence_idx_end = sequence_idx_end),
              input
            )
          }

          # Parallel block
          cl <- makeCluster(num_threads)
          x <- parLapply(cl, thread_counts, inner)  # Assign to x locally
          stopCluster(cl)

          success_flag = TRUE
        },
        error = function(e) {
          # Error handling block
          message(paste("Parallel computing error occurred:", conditionMessage(e), ". Automatically switching to serial computing."))
        }
      )
    } 
    
    if(!parallel || !success_flag) {
      num_threads <- if (parallel) parallel::detectCores() else 1
      thread_counts <- vector("list", num_threads)

      for (thread_num in seq_len(num_threads)) {
        blocklen <- 1 + ((num_sequences - 1) %/% num_threads)
        sequence_idx_start <- blocklen * (thread_num - 1)
        sequence_idx_end <- min(sequence_idx_start + blocklen, num_sequences)
        thread_counts[[thread_num]] <- c(
          list(sequence_idx_start = sequence_idx_start, sequence_idx_end = sequence_idx_end),
          input
        )
      }
      x <- lapply(thread_counts, inner)  # If no parallel, just serial
    }

    return(x)  # Return x after computation
  }
  x <- get_x(parallel, inner)

  for (i in x) {
    total_loglike <- total_loglike + i[[4]]
    all_trans_softcounts <- all_trans_softcounts + i[[1]]
    all_emission_softcounts <- all_emission_softcounts + i[[2]]
    all_initial_softcounts <- all_initial_softcounts + i[[3]]
    for (alpha in i[[5]]) {
      sequence_start <- alpha[[1]]
      T <- alpha[[2]]
      alpha_out[, sequence_start:(sequence_start + T - 1)] <- alpha_out[, sequence_start:(sequence_start + T - 1)] + alpha[[3]]
    }
  }

  all_trans_softcounts <- as.vector(all_trans_softcounts)
  all_emission_softcounts <- as.vector(all_emission_softcounts)

  total_loglike <- total_loglike
  all_trans_softcounts <- reshape_python(all_trans_softcounts, dim = c(num_resources, 2, 2))
  all_emission_softcounts <- reshape_python(all_emission_softcounts, dim = c(num_subparts, 2, 2))
  all_initial_softcounts <- all_initial_softcounts
  alpha_out <- reshape_python(as.vector(alpha_out), dim = dim(alpha_out))

  result <- list(
    total_loglike = total_loglike,
    all_trans_softcounts = all_trans_softcounts,
    all_emission_softcounts = all_emission_softcounts,
    all_initial_softcounts = all_initial_softcounts,
    alpha_out = alpha_out
  )

  return(result)
}


# MARK: inner (R)
inner <- function(x) {
  As <- x$As
  Bn <- x$Bn
  initial_distn <- x$initial_distn
  allresources <- x$allresources
  starts <- x$starts
  lengths <- x$lengths
  num_resources <- x$num_resources
  num_subparts <- x$num_subparts
  alldata <- x$alldata
  normalizeLengths <- x$normalizeLengths
  sequence_idx_start <- x$sequence_idx_start
  sequence_idx_start <- sequence_idx_start + 1 # handle index difference between R and Python
  sequence_idx_end <- x$sequence_idx_end

  N_R <- 2 * num_resources
  N_S <- 2 * num_subparts
  trans_softcounts_temp <- matrix(0, nrow = 2, ncol = N_R)
  emission_softcounts_temp <- matrix(0, nrow = 2, ncol = N_S)
  init_softcounts_temp <- matrix(0, nrow = 2, ncol = 1)
  loglike <- 0

  alphas <- list()
  for (sequence_index in sequence_idx_start:sequence_idx_end) {
    sequence_start <- starts[sequence_index]
    T <- lengths[sequence_index]
    # caculate the likelihoods
    likelihoods <- matrix(1, nrow = 2, ncol = T)
    alpha <- matrix(NA, nrow = 2, ncol = T)
    for (t in 0:(min(2, T) - 1)) {
      for (n in 0:(num_subparts - 1)) {
        data_temp <- alldata[1 + n, sequence_start + t]
        if (0 != data_temp) {
          sl <- Bn[, 1 + 2 * n + as.integer(data_temp == 2)]
          likelihoods[, 1 + t] <- likelihoods[, 1 + t] * ifelse(sl == 0, 1, sl)
        }
      }
    }
    # forward propagation - alpha
    alpha[, 1] <- initial_distn * likelihoods[, 1]
    norm <- sum(alpha[, 1])
    alpha[, 1] <- alpha[, 1] / norm
    contribution <- log(norm) / (if (normalizeLengths) T else 1)
    loglike <- loglike + contribution
    # 结合 t = 2 的情况
    if (T >= 2) {
      resources_temp <- allresources[sequence_start]
      k <- 2 * (resources_temp - 1) + 1
      alpha[, 2] <- As[1:2, k:(k + 1)] %*% alpha[, 1] * likelihoods[, 2]
      norm <- sum(alpha[, 2])
      alpha[, 2] <- alpha[, 2] / norm
      contribution <- log(norm) / (if (normalizeLengths) T else 1)
      loglike <- loglike + contribution
    }
    if (T > 2) {
      for (t in 2:(T - 1)) {
        for (n in 0:(num_subparts - 1)) {
          data_temp <- alldata[1 + n, sequence_start + t]
          if (0 != (data_temp)) {
            sl <- Bn[, 1 + 2 * n + as.integer(data_temp == 2)]
            likelihoods[, 1 + t] <- likelihoods[, 1 + t] * ifelse(sl == 0, 1, sl)
          }
        }
        resources_temp <- allresources[sequence_start + t - 1]
        k <- 2 * (resources_temp - 1) + 1
        alpha[, t + 1] <- As[1:2, k:(k + 1)] %*% alpha[, t] * likelihoods[, t + 1]
        norm <- sum(alpha[, t + 1])
        alpha[, t + 1] <- alpha[, t + 1] / norm
        loglike <- loglike + log(norm) / (if (normalizeLengths) T else 1)
      }
    }

    gamma <- matrix(NA, nrow = 2, ncol = T)
    gamma[, T] <- alpha[, T]

    As_temp <- As
    first_pass <- TRUE

    if (T > 1) {
      for (t in (T - 2):0) {
        resources_temp <- allresources[sequence_start + t]
        k <- 2 * (resources_temp - 1) + 1
        A <- As_temp[1:2, k:(k + 1)]
        pair <- A
        pair[1, ] <- pair[1, ] * alpha[, 1 + t]
        pair[2, ] <- pair[2, ] * alpha[, 1 + t]
        dotted <- A %*% alpha[, 1 + t]
        gamma_t <- gamma[, t + 2]
        pair[, 1] <- (pair[, 1] * gamma_t) / dotted
        pair[, 2] <- (pair[, 2] * gamma_t) / dotted
        pair[is.nan(pair)] <- 0
        trans_softcounts_temp[1:2, k:(k + 1)] <- trans_softcounts_temp[1:2, k:(k + 1)] + pair
        gamma[, 1 + t] <- colSums(pair)

        for (n in 0:(num_subparts - 1)) {
          data_temp <- alldata[n + 1, sequence_start + t]
          if (0 != (data_temp)) {
            emission_softcounts_temp[, (1 + 2 * n + as.integer(data_temp == 2))] <- emission_softcounts_temp[, (1 + 2 * n + as.integer(data_temp == 2))] + gamma[, t + 1]
          }
          if (first_pass) {
            data_temp_p <- alldata[n + 1, sequence_start + (T - 1)]

            if (0 != (data_temp_p)) {
              emission_softcounts_temp[, (1 + 2 * n + as.integer(data_temp_p == 2))] <- emission_softcounts_temp[, (1 + 2 * n + as.integer(data_temp_p == 2))] + gamma[, T]
            }
          }
        }
        first_pass <- FALSE
      }
    }

    init_softcounts_temp <- init_softcounts_temp + matrix(gamma[, 1], nrow = 2, ncol = 1)
    alphas[[length(alphas) + 1]] <- list(sequence_start = sequence_start, T = T, alpha = alpha)
  }
  return(list(trans_softcounts_temp = trans_softcounts_temp, emission_softcounts_temp = emission_softcounts_temp, init_softcounts_temp = init_softcounts_temp, loglike = loglike, alphas = alphas))
}
