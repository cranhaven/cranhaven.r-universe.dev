# MARK: crossvalidate_single_skill
crossvalidate_single_skill <- function(model, data, skill, folds, metric, rand, use_folds = FALSE) {
    num_learns <- if ("resource_names" %in% names(data)) length(data$resource_names) else 1
    num_gs <- if ("gs_names" %in% names(data)) length(data$gs_names) else length(data$starts)

    shuffle <- if (is.numeric(rand)) sample(1:length(data$starts)) else rand$permutation(length(data$starts))

    all_true <- c()
    all_pred <- c()
    metrics <- NULL

    if (use_folds) {
        stop("not implemented")
        all_folds <- table(data$folds)
        folds <- length(all_folds)

        for (label in names(all_folds)) {
            count <- all_folds[[label]]
            training_data <- fix_data_specified(data, label, count)
            test_data <- fix_data_specified(data, label, count)

            model@fit_model[[skill]] <- ._fit(model, training_data, skill, model@forgets)
            metrics <- metrics + ._evaluate(model, list(skill = test_data), metric)
        }
    } else {
        split_size <- floor(length(data$starts) / folds)

        for (iteration in 1:folds) {
            model@fit_model <- list()

            test_start <- (iteration - 1) * split_size + 1
            total_length <- length(shuffle)
            test_end <- min(iteration * split_size, total_length)
            test <- cut_array(shuffle, test_start, test_end)
            train <- c(cut_array(shuffle, 1, test_start - 1), cut_array(shuffle, test_end + 1, total_length))
            training_data <- fix_data(data, train)
            model@fit_model[[skill]] <- ._fit(model, training_data, skill, model@forgets)
            test_data <- fix_data(data, test)
            skill_data <- setNames(list(test_data), skill)
            result <- ._evaluate(model, skill_data, metric)
            if (is.null(metrics)) {
                metrics <- result
            } else {
                metrics <- mapply(function(x, y) x + y, metrics, result)
            }
        }
    }

    return(metrics / folds)
}

# MARK: fix_data
fix_data <- function(data, indices) {
    training_data <- list()

    prev_starts <- data$starts[indices]
    lengths <- data$lengths[indices]
    total_length <- sum(lengths)

    d <- matrix(0, nrow = nrow(data$data), ncol = total_length)
    resources <- rep(1, total_length)

    if (!is.null(data$resource_names)) {
        training_data$resource_names <- data$resource_names
    }
    if (!is.null(data$gs_names)) {
        training_data$gs_names <- data$gs_names
    }

    starts <- rep(0, length(prev_starts))
    current_index <- 1
    for (i in seq_along(prev_starts)) {
        starts[i] <- current_index

        d[, starts[i]:(starts[i] + lengths[i] - 1)] <- data$data[, prev_starts[i]:(prev_starts[i] + lengths[i] - 1)]
        resources[starts[i]:(starts[i] + lengths[i] - 1)] <- data$resources[prev_starts[i]:(prev_starts[i] + lengths[i] - 1)]

        current_index <- current_index + lengths[i]
    }

    training_data$starts <- starts
    training_data$lengths <- lengths
    training_data$data <- d
    training_data$resources <- reshape_python(resources, dim = c(length(resources), 1))

    return(training_data)
}

# MARK: cut_array
cut_array <- function(array, l, r) {
    len <- length(array)
    if (l < 0) {
        l <- 0
    }
    if (r > len) {
        r <- len
    }
    if (l > r) {
        return(array[integer(0)])
    }
    return(array[l:r])
}

fix_data_specified <- function(data, label, count) {
    stop("not implemented")
}
