# MARK: predict_onestep_run
predict_onestep_run <- function(model, data) {
    num_subparts <- nrow(data$data) # 数据的第一维表示每个子部分的数量
    num_resources <- length(model$learns)

    trans_softcounts <- array(0, dim = c(num_resources, 2, 2))
    emission_softcounts <- array(0, dim = c(num_subparts, 2, 2))
    init_softcounts <- array(0, dim = c(2, 1))

    result <- list(
        all_trans_softcounts = trans_softcounts,
        all_emission_softcounts = emission_softcounts,
        all_initial_softcounts = init_softcounts
    )
    result <- run(data, model, result$all_trans_softcounts, result$all_emission_softcounts, result$all_initial_softcounts, 1, parallel = FALSE)

    for (j in seq_len(num_resources)) {
        result$all_trans_softcounts[j, , ] <- t(result$all_trans_softcounts[j, , ])
    }

    for (j in seq_len(num_subparts)) {
        result$all_emission_softcounts[j, , ] <- t(result$all_emission_softcounts[j, , ])
    }
    state_predictions <- predict_onestep_states(data, model, result$alpha_out)
    correct_emission_predictions <- model$guesses %*% t(state_predictions[1, ]) +
        (1 - model$slips) %*% t(state_predictions[2, ])
    # flattened_predictions <- correct_emission_predictions[apply(data$data != 0, 2, which.max)]
    flattened_predictions <- correct_emission_predictions
    return(list(correct_predictions = as.vector(flattened_predictions), state_predictions = state_predictions))
}

# MARK: predict_onestep_states
predict_onestep_states <- function(data, model, forward_messages) {
    alldata <- data$data
    allresources <- data$resources
    starts <- data$starts
    lengths <- data$lengths
    learns <- model$learns
    forgets <- model$forgets
    guesses <- model$guesses
    slips <- model$slips
    prior <- model$prior

    bigT <- ncol(alldata)
    num_subparts <- nrow(alldata)
    num_sequences <- length(starts)
    num_resources <- length(learns)
    initial_distn <- c(1 - prior, prior)

    interleave <- function(m, v1, v2) {
        m[seq(1, length(m), 2)] <- v1
        m[seq(2, length(m), 2)] <- v2
        return(m)
    }

    As <- matrix(NA, nrow = 2, ncol = 2 * num_resources)

    As[1, ] <- interleave(As[1, ], 1 - learns, forgets)
    As[2, ] <- interleave(As[2, ], learns, 1 - forgets)

    fd_temp <- rep(NA, 2 * bigT)
    for (i in 1:2) {
        for (j in 1:bigT) {
            fd_temp[(i - 1) * bigT + j] <- forward_messages[i, j]
        }
    }

    # outputs
    all_predictions <- rep(NA, 2 * bigT)
    all_predictions <- seq(-1, -(2 * bigT), by = -1)

    for (sequence_index in seq_len(num_sequences)) {
        sequence_start <- starts[sequence_index] - 1
        T <- lengths[sequence_index]
        forward_messages_subset <- matrix(fd_temp[(2 * sequence_start + 1):(2 * (sequence_start + T))], nrow = 2, byrow = FALSE)
        predictions <- matrix(all_predictions[(2 * sequence_start + 1):(2 * (sequence_start + T))], nrow = 2, byrow = FALSE)
        predictions[, 1] <- initial_distn
        for (t in seq_len(T - 1)) {
            resources_temp <- allresources[sequence_start + t]
            k <- 2 * (resources_temp - 1)
            predictions[, t + 1] <- As[1:2, (k + 1):(k + 2)] %*% forward_messages_subset[, t]
        }

        all_predictions[(2 * sequence_start + 1):(2 * (sequence_start + T))] <- as.vector(predictions)
    }
    return(matrix(all_predictions, nrow = 2, byrow = FALSE))
}
