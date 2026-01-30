.assign.folds_one_site <-
function (y, family, nfolds) 
{
    folds = rep(NA, length(y))
    folds = switch(family, binomial = {
        for (i in 0:1) {
            indexs_i = which(y == i)
            folds[indexs_i] = .assign.folds_simple(indexs_i, 
                nfolds)
        }
        folds
    }, cox = {
        sorted_idx = sort.int(y[, 1], index.return = TRUE)$ix
        y_w_indx = cbind(y, sorted_idx)
        idx_g0 = y_w_indx[y[, 2] == 0, 3]
        idx_g1 = y_w_indx[y[, 2] == 1, 3]
        n_g0 = length(idx_g0)
        if (n_g0 > 0) {
            for (i in seq(0, n_g0 - 1, by = nfolds)) {
                n_i = min(c(nfolds, n_g0 - i))
                indexs_i = idx_g0[i + (1:n_i)]
                folds[indexs_i] = sample(n_i)
            }
        }
        n_g1 = length(idx_g1)
        if (n_g1 > 0) {
            for (i in seq(0, n_g1 - 1, by = nfolds)) {
                n_i = min(c(nfolds, n_g1 - i))
                indexs_i = idx_g1[i + (1:n_i)]
                folds[indexs_i] = sample(n_i)
            }
        }
        y_w_indx = data.frame(time = y_w_indx[, 1], status = y_w_indx[, 
            2], sorted_idx = y_w_indx[, 3], folds = folds)
        y_w_indx_original_order = y_w_indx[order(sorted_idx), 
            ]
        folds = y_w_indx_original_order$folds
        folds
    }, gaussian = {
        sorted_idx = order(y)
        n = length(y)
        for (i in seq(0, n - 1, by = nfolds)) {
            n_i = min(c(nfolds, n - i))
            indexs_i = sorted_idx[i + (1:n_i)]
            folds[indexs_i] = .assign.folds_simple(indexs_i, 
                nfolds)
        }
        table_y = table(y)
        if (length(table_y) > 2 || (length(table_y) == 2 && all(table_y) >= 
            2)) {
            fold_with_constant_training_sample = NA
            for (i in 1:nfolds) {
                if (var(y[which(folds != i)]) == 0) {
                  fold_with_constant_training_sample = i
                }
            }
            if (!is.na(fold_with_constant_training_sample)) {
                index_of_one_repeated_y_from_other_folds = sample(which(folds != 
                  fold_with_constant_training_sample), 1)
                index_of_one_different_y_from_the_fold = sample(which(y != 
                  y[index_of_one_repeated_y_from_other_folds]), 
                  1)
                tmp = folds[index_of_one_different_y_from_the_fold]
                folds[index_of_one_different_y_from_the_fold] = folds[index_of_one_repeated_y_from_other_folds]
                folds[index_of_one_repeated_y_from_other_folds] = tmp
            }
        }
        folds
    })
    folds = 1 + (folds + sample(1:nfolds, 1))%%nfolds
    sizes = sort(sample(table(factor(folds, levels = 1:nfolds))))
    while (diff(range(sizes)) > 1) {
        folds[sample(which(folds == as.numeric(names(sizes)[nfolds])), 
            1)] = as.numeric(names(sizes)[1])
        sizes = sort(sample(table(factor(folds, levels = 1:nfolds))))
    }
    folds
}
.assign.folds_simple <-
function (y, nfolds) 
{
    n = length(y)
    if (n < nfolds) {
        folds = sample(1:nfolds, n)
    }
    else {
        folds = sample(1 + (1:n%%nfolds))
    }
    1 + (folds + sample(1:nfolds, 1))%%nfolds
}
.is_binary_vector <-
function (x) 
{
    is.vector(x) && all(x %in% 0:1)
}
.is_boolean <-
function (x) 
{
    is.vector(x) && is.logical(x) && length(x) == 1
}
.is_family <-
function (family) 
{
    (is.vector(family) || is.factor(family)) && length(family) == 
        1 && family %in% c("binomial", "cox", "gaussian")
}
.is_number <-
function (x) 
{
    is.vector(x) && is.numeric(x) && length(x) == 1
}
.is_numeric_vector <-
function (x) 
{
    is.vector(x) && is.numeric(x)
}
.is_surv <-
function (x) 
{
    inherits(x, "Surv")
}
assign.folds <-
function (y, family = c("binomial", "cox", "gaussian"), site = NULL, 
    nfolds = 10) 
{
    if (!.is_family(family)) {
        stop("family must be \"binomial\", \"cox\", or \"gaussian\"")
    }
    if (family == "binomial" && !.is_binary_vector(y)) {
        stop("for \"binomial\", y must be a binary vector")
    }
    if (family == "cox" && !.is_surv(y)) {
        stop("for \"cox\", y must be a \"Surv\" object")
    }
    if (family == "gaussian" && !.is_numeric_vector(y)) {
        stop("for \"gaussian\", y must be a numeric vector")
    }
    if (family == "cox") {
        n = nrow(y)
    }
    else {
        n = length(y)
    }
    if (!(is.null(site) || ((is.vector(site) || is.factor(site)) && 
        length(site) == n))) {
        stop("site must be a vector with the same length as y, or NULL")
    }
    if (!(.is_number(nfolds) && nfolds > 0)) {
        stop("nfolds must be a positive number")
    }
    if (is.null(site)) {
        return(.assign.folds_one_site(y, family, nfolds))
    }
    folds = rep(NA, length(y))
    for (site_i in site) {
        i = which(site == site_i)
        folds[i] = .assign.folds_one_site(y[i], family, nfolds)
    }
    folds
}
coef.glmnet_fit <-
function (x) 
{
    c(`(Intercept)` = x$a0, x$beta)
}
cv <-
function (x, y, family = c("binomial", "cox", "gaussian"), fit_fun, 
    predict_fun, site = NULL, covar = NULL, nfolds = 10, pred.format = NA, 
    verbose = TRUE, ...) 
{
    if (!.is_family(family)) {
        stop("family must be \"binomial\", \"cox\", or \"gaussian\"")
    }
    if (family == "binomial" && !.is_binary_vector(y)) {
        stop("for \"binomial\", y must be a binary vector")
    }
    if (family == "cox" && !.is_surv(y)) {
        stop("for \"cox\", y must be a \"Surv\" object")
    }
    if (family == "gaussian" && !.is_numeric_vector(y)) {
        stop("for \"gaussian\", y must be a numeric vector")
    }
    if (family == "cox") {
        n = nrow(y)
    }
    else {
        n = length(y)
    }
    if (!((is.matrix(x) || is.data.frame(x)) && nrow(x) == n)) {
        stop("x must be a matrix or data.frame with the same height as y")
    }
    if (!is.function(fit_fun) || !is.function(predict_fun)) {
        stop("fit_fun and predict_fun must be functions")
    }
    if (!(is.null(site) || ((is.vector(site) || is.factor(site)) && 
        length(site) == n))) {
        stop("site must be a vector with the same length as y, or NULL")
    }
    if (!(is.null(site) || ((is.matrix(covar) && is.matrix(covar)) && 
        nrow(covar) == n))) {
        stop("covar must be a matrix or data.frame with the same height as y")
    }
    if (!(.is_number(nfolds) && nfolds > 0)) {
        stop("nfolds must be a positive number")
    }
    if (!((is.vector(pred.format) && length(pred.format) == 1 && 
        is.na(pred.format)) || (is.matrix(pred.format) && nrow(pred.format) == 
        1))) {
        stop("pred.format must be NA or a one-row matrix")
    }
    if (!.is_boolean(verbose)) {
        stop("verbose must be TRUE or FALSE")
    }
    if (!is.null(site)) {
        if (!is.null(covar)) {
            if (verbose) {
                cat("[cv] Cross-validation with sites and covariates\n")
            }
            type = "site+covar"
        }
        else {
            if (verbose) {
                cat("[cv] Cross-validation with sites\n")
            }
            type = "site"
        }
        site = as.factor(site)
    }
    else {
        if (!is.null(covar)) {
            if (verbose) {
                cat("[cv] Cross-validation with covariates\n")
            }
            type = "covar"
        }
        else {
            if (verbose) {
                cat("[cv] Simple cross-validation\n")
            }
            type = "simple"
        }
    }
    folds = assign.folds(y, family, site = site, nfolds = nfolds)
    models = list()
    y.pred = pred.format
    for (i in 2:length(folds)) {
        y.pred = rbind(y.pred, pred.format)
    }
    if (is.null(colnames(y.pred))) {
        if (ncol(y.pred) > 1) {
            colnames(y.pred) = paste0("y", 1:ncol(y.pred), ".pred")
        }
        else {
            colnames(y.pred) = "y.pred"
        }
    }
    rownames(y.pred) = NULL
    for (fold in 1:nfolds) {
        if (verbose) {
            cat("[cv] Fold", fold, " - Training\n")
        }
        training = which(folds != fold)
        x_training = x[training, ]
        x_training_constant = which(apply(x_training, 2, function(x) {
            length(unique(x)) == 1
        }))
        if (length(x_training_constant) > 0) {
            x_training = x_training[, -x_training_constant]
        }
        y_training = switch(family, cox = y[training, ], y[training])
        model = switch(type, simple = fit_fun(x_training, y_training, 
            ...), site = fit_fun(x_training, y_training, site[training], 
            NULL, ...), covar = fit_fun(x_training, y_training, 
            NULL, covar[training, ], ...), `site+covar` = fit_fun(x_training, 
            y_training, site[training], covar[training, ], ...))
        models[[fold]] = model
        if (verbose) {
            cat("[cv] Fold", fold, " - Test\n")
        }
        test = which(folds == fold)
        x_test = x[test, ]
        if (length(x_training_constant) > 0) {
            if (is.vector(x_test)) {
                x_test = matrix(x_test[-x_training_constant], 
                  nrow = 1, dimnames = list(NULL, dimnames(x)[[2]][-x_training_constant]))
            }
            else {
                x_test = x_test[, -x_training_constant]
            }
        }
        else {
            if (is.vector(x_test)) {
                x_test = matrix(x_test, nrow = 1, dimnames = list(NULL, 
                  dimnames(x)[[2]]))
            }
        }
        y.pred[test, ] = switch(type, simple = predict_fun(model, 
            x_test, ...), site = predict_fun(model, x_test, site[test], 
            NULL, ...), covar = predict_fun(model, x_test, NULL, 
            covar[test, ], ...), `site+covar` = predict_fun(model, 
            x_test, site[test], covar[test, ], ...), )
    }
    list(predictions = cbind(data.frame(fold = folds), y, y.pred), 
        models = models)
}
data.frame2glmnet.matrix <-
function (m, x) 
{
    if (!inherits(m, "data.frame2glmnet.matrix_fit")) {
        stop("m must be a \"data.frame2glmnet.matrix\" object")
    }
    if (!is.data.frame(x)) {
        stop("x must be a data.frame")
    }
    xp = NULL
    if (length(m) > 0) {
        for (i in 1:length(m)) {
            transf_i = m[[i]]
            xj = x[, match(transf_i[1], colnames(x))]
            xp = cbind(xp, switch(transf_i[2], constant = NULL, 
                factor = {
                  if (length(transf_i) == 4) {
                    xpj = matrix(as.numeric(xj == transf_i[4]))
                    colnames(xpj) = paste0(transf_i[1], ":", 
                      transf_i[4])
                  } else {
                    xpj = NULL
                    for (k in 3:length(transf_i)) {
                      xpj = cbind(xpj, as.numeric(xj == transf_i[k]))
                    }
                    colnames(xpj) = paste0(transf_i[1], ":", 
                      transf_i[3:length(transf_i)])
                  }
                  xpj
                }, numeric = {
                  xpj = matrix(xj)
                  colnames(xpj) = transf_i[1]
                  xpj
                }))
        }
    }
    xp
}
data.frame2glmnet.matrix_fit <-
function (x) 
{
    if (!is.data.frame(x)) {
        stop("x must be a data.frame")
    }
    m = list()
    if (ncol(x) > 0) {
        for (j in 1:ncol(x)) {
            xj_name = colnames(x)[j]
            xj_char = as.character(x[, j])
            xj_not_na_num = suppressWarnings(as.numeric(xj_char[which(!is.na(xj_char))]))
            xj_levels = sort(unique(xj_char))
            if (length(xj_levels) < 2) {
                m[[length(m) + 1]] = c(xj_name, "constant")
            }
            else if (any(is.na(xj_not_na_num))) {
                m[[length(m) + 1]] = c(xj_name, "factor", xj_levels)
            }
            else {
                m[[length(m) + 1]] = c(xj_name, "numeric")
            }
        }
    }
    class(m) = "data.frame2glmnet.matrix_fit"
    m
}
glmnet_fit <-
function (x, y, family = c("binomial", "cox", "gaussian"), nfolds = 10, 
    standardize = TRUE, min.beta = 9.9999999999999998e-13) 
{
    if (!.is_family(family)) {
        stop("family must be \"binomial\", \"cox\", or \"gaussian\"")
    }
    if (family == "binomial" && !.is_binary_vector(y)) {
        stop("for \"binomial\", y must be a binary vector")
    }
    if (family == "cox" && !.is_surv(y)) {
        stop("for \"cox\", y must be a Surv object")
    }
    if (family == "gaussian" && !.is_numeric_vector(y)) {
        stop("for \"gaussian\", y must be a numeric vector")
    }
    if (family == "cox") {
        n = nrow(y)
    }
    else {
        n = length(y)
    }
    if (!(is.matrix(x) && nrow(x) == n)) {
        stop("x must be a matrix with the same height as y")
    }
    if (!(.is_number(nfolds) && nfolds > 0)) {
        stop("nfolds must be a positive number")
    }
    if (!.is_boolean(standardize)) {
        stop("standardize must be TRUE or FALSE")
    }
    if (!(.is_number(min.beta) && min.beta > 0)) {
        stop("min.beta must be a positive number")
    }
    if (ncol(x) == 1) {
        coef = switch(family, binomial = coef(glm(y ~ x, family = binomial)), 
            cox = coef(coxph(Surv(time = y[, 1], event = y[, 
                2]) ~ x)), gaussian = coef(lm(y ~ x)))
        if (family == "cox") {
            a0 = NULL
            betas = coef
        }
        else {
            a0 = coef[1]
            betas = coef[2]
        }
        i = 1
        x_sd = sd(x)
    }
    else {
        type_measure = switch(family, binomial = "class", cox = "deviance", 
            gaussian = "mse")
        if (family == "cox") {
            colnames(y) = c("time", "status")
        }
        folds = assign.folds(y, family = family, nfolds = nfolds)
        if (family == "binomial" && min(table(y)) < 3) {
            stop("too few subjects of one group")
        }
        cv_ = cv.glmnet(x, y, type.measure = type_measure, family = family, 
            foldid = folds, standardize = standardize)
        idx_lambda = match(cv_$lambda.min, cv_$lambda)
        if (cv_$glmnet.fit$df[idx_lambda] == 0) {
            idx_lambda = which(cv_$glmnet.fit$df > 0)[1]
        }
        glmnet.control(fdev = 0)
        lasso = glmnet(x, y, family, lambda = cv_$lambda, standardize = standardize)
        a0 = lasso$a0[idx_lambda]
        betas = lasso$beta[, idx_lambda]
        betas[which(abs(betas) < min.beta)] = 0
        i = which(betas != 0)
        if (length(i) == 1) {
            x_sd = sd(x[, i])
        }
        else {
            x_sd = apply(x[, i], 2, sd)
        }
    }
    m = list(family = family, a0 = a0, i = i, beta = betas[i], 
        sd = x_sd)
    class(m) = "glmnet_fit"
    m
}
glmnet_get.items.relevance <-
function (x, childname = NULL) 
{
    if (inherits(x, "glmnet_fit")) {
        x = list(x)
    }
    else {
        if (!(is.list(x) && length(x) > 0)) {
            stop("x must be of class \"glmnet_fit\" or a non-empty list")
        }
        if (!is.null(childname)) {
            if (!(is.character(childname) && length(childname) == 
                1)) {
                stop("childname must be a character or NULL")
            }
            y = list()
            for (i in 1:length(x)) {
                xi = x[[i]][[childname]]
                if (inherits(xi, "glmnet_fit")) {
                  y[[length(y) + 1]] = xi
                }
                else if (is.list(xi)) {
                  for (j in 1:length(xi)) {
                    y[[length(y) + 1]] = xi[[j]]
                  }
                }
                else {
                  stop("the objects in x children must be of class \"glmnet_fit\" or lists")
                }
            }
            x = y
        }
        if (!inherits(x[[1]], "glmnet_fit")) {
            stop("the objects in x must be of class \"glmnet_fit\"")
        }
    }
    list_coefs = NULL
    for (i in 1:length(x)) {
        xi = x[[i]]
        if (length(xi$sd) != length(xi$beta) || any(names(xi$sd) != 
            names(xi$beta))) {
            stop("Coefficients names mismatch")
        }
        list_coefs_i = data.frame(coef = names(xi$beta), sbeta = xi$beta * 
            xi$sd)
        list_coefs_i$relevance = abs(list_coefs_i$sbeta)/sum(abs(list_coefs_i$sbeta))
        list_coefs = rbind(list_coefs, list_coefs_i)
    }
    y = by(list_coefs$relevance, list_coefs$coef, sum)/length(x)
    names(y) = names(y)
    sort(y, decreasing = TRUE)
}
glmnet_get.main.model <-
function (x, childname = NULL, verbose = TRUE) 
{
    if (!(is.list(x) && length(x) > 0)) {
        stop("x must be a non-empty list")
    }
    if (!is.null(childname)) {
        if (!(is.character(childname) && length(childname) == 
            1)) {
            stop("childname must be a character or NULL")
        }
        y = list()
        for (i in 1:length(x)) {
            xi = x[[i]][[childname]]
            if (inherits(xi, "glmnet_fit")) {
                y[[length(y) + 1]] = xi
            }
            else if (is.list(xi)) {
                for (j in 1:length(xi)) {
                  y[[length(y) + 1]] = xi[[j]]
                }
            }
            else {
                stop("the objects in x children must be of class \"glmnet_fit\" or lists")
            }
        }
        x = y
    }
    if (!inherits(x[[1]], "glmnet_fit")) {
        stop("the objects in x must be of class \"glmnet_fit\"")
    }
    if (length(x) == 1) {
        warning("The list contains only one model")
        return(x[[1]])
    }
    vars = c()
    for (i in 1:length(x)) {
        vars = c(vars, x[[i]]$i)
    }
    vars = unique(sort(vars))
    models = matrix(0, ncol = length(vars), nrow = length(x))
    for (i in 1:length(x)) {
        models[i, match(x[[i]]$i, vars)] = 1
    }
    if (verbose) {
        cat("[glmnet_get.main.model] - Calculating Dice coefficients...\n")
    }
    dice = matrix(NA, ncol = nrow(models), nrow = nrow(models))
    for (i1 in 1:(nrow(models) - 1)) {
        for (i2 in (i1 + 1):nrow(models)) {
            dice_ij = 2 * sum(models[i1, ] * models[i2, ])/(sum(models[i1, 
                ]) + sum(models[i2, ]))
            dice[i1, i2] = dice_ij
            dice[i2, i1] = dice_ij
        }
    }
    mean_dice = apply(dice, 1, mean, na.rm = TRUE)
    if (verbose) {
        cat("[glmnet_get.main.model] - Selecting the model with the highest Dice coefficient...\n")
    }
    selected = which(mean_dice == max(mean_dice))
    y = x[[selected[1]]]
    if (length(selected) > 1) {
        for (i in 2:length(selected)) {
            if (!is.null(y$a0)) {
                y$a0 = c(y$a0, x[[selected[i]]]$a0)
            }
            y$beta = rbind(y$beta, x[[selected[i]]]$beta)
            y$sd = rbind(y$sd, x[[selected[i]]]$sd)
        }
        if (!is.null(y$a0)) {
            y$a0 = mean(y$a0)
        }
        sbeta = apply(y$beta * y$sd, 2, mean)
        y$beta = apply(y$beta, 2, mean)
        y$sd = sbeta/y$beta
    }
    y
}
glmnet_predict <-
function (m, x) 
{
    if (!inherits(m, "glmnet_fit")) {
        stop("m must be a \"glmnet_fit\" object")
    }
    if (!is.matrix(x)) {
        stop("x must be a matrix")
    }
    y = matrix(x[, m$i], ncol = length(m$i)) %*% m$beta
    if (m$family != "cox") {
        y = m$a0 + y
    }
    if (m$family == "binomial") {
        y = 1/(1 + exp(-y))
    }
    y
}
impute.glmnet.matrix <-
function (m, x, nimp = 20, verbose = TRUE) 
{
    if (!inherits(m, "impute.glmnet.matrix_fit")) {
        stop("m must be a \"impute.glmnet.matrix_fit\" object")
    }
    if (!is.matrix(x)) {
        stop("x must be a matrix")
    }
    if (!(.is_number(nimp) && nimp > 0)) {
        stop("nimp must be a positive number")
    }
    if (!.is_boolean(verbose)) {
        stop("verbose must be TRUE or FALSE")
    }
    if (verbose) {
        cat("[impute.glmnet.matrix] Imputing missing values...\n")
    }
    start.time = Sys.time()
    X_na = is.na(x)
    x.imp = list()
    for (imp in seq_len(nimp)) {
        x.imp[[imp]] = x
    }
    for (j in seq_len(ncol(x))) {
        X_na_j = X_na[, j]
        mj = m[[j]]
        family = mj$family
        for (i in 1:length(mj$imp.models)) {
            if (any(X_na_j)) {
                imp.model = mj$imp.models[[i]]
                if (!is.null(imp.model)) {
                  x.x = matrix(x[, -j], ncol = ncol(x) - 1)
                  cols.used = imp.model$i
                  rows.to_predict.complete = which(X_na_j & apply(x.x, 
                    1, function(tmp) {
                      all(cols.used %in% which(!is.na(tmp)))
                    }))
                  x.x.complete = matrix(x.x[rows.to_predict.complete, 
                    ], ncol = ncol(x) - 1)
                  x.y.to_predict = glmnet_predict(imp.model, 
                    x.x.complete)
                  for (imp in 1:nimp) {
                    if (family == "binomial") {
                      Ximp = as.numeric(x.y.to_predict > runif(length(rows.to_predict.complete)))
                    }
                    else {
                      Ximp = x.y.to_predict + rnorm(length(rows.to_predict.complete), 
                        0, mj$errors[i])
                    }
                    x.imp[[imp]][rows.to_predict.complete, j] = Ximp
                  }
                  X_na_j[rows.to_predict.complete][which(!is.na(x.y.to_predict))] = FALSE
                }
            }
        }
        if (any(X_na_j)) {
            for (imp in 1:nimp) {
                Ximp = sample(mj$data, sum(X_na_j), replace = TRUE)
                x.imp[[imp]][which(X_na_j), j] = Ximp
            }
        }
    }
    if (verbose) {
        cat("[impute.glmnet.matrix] Running time:", difftime(Sys.time(), 
            start.time, units = "secs"), "seconds\n")
    }
    x.imp
}
impute.glmnet.matrix_fit <-
function (x, ncores = 1, verbose = TRUE) 
{
    if (!is.matrix(x)) {
        stop("x must be a matrix")
    }
    if (!(.is_number(ncores) && ncores > 0)) {
        stop("ncores must be a positive number")
    }
    if (!.is_boolean(verbose)) {
        stop("verbose must be TRUE or FALSE")
    }
    if (verbose) {
        cat("[impute.glmnet.matrix_fit] Estimating imputation models (it can take time!)...\n")
    }
    cl = makeCluster(ncores)
    registerDoParallel(cl)
    iterations = ncol(x)
    start.time = Sys.time()
    X_na = is.na(x)
    m = list()
    j = NULL
    m = foreach(j = 1:ncol(x), .export = c(".assign.folds_one_site", 
        ".assign.folds_simple", ".is_boolean", ".is_family", 
        ".is_number", ".is_numeric_vector", "assign.folds", "cv.glmnet", 
        "glmnet", "glmnet.control", "glmnet_fit", "glmnet_predict")) %dopar% 
        {
            X_na_j = X_na[, j]
            x.x = matrix(x[which(!X_na_j), -j], ncol = ncol(x) - 
                1)
            x.y = x[which(!X_na_j), j]
            family = ifelse(setequal(x.y, c(0, 1)), "binomial", 
                "gaussian")
            completeCols = apply(x.x, 1, function(tmp) {
                complete = which(!is.na(tmp))
                ifelse(length(complete) > 0, paste(complete, 
                  collapse = ","), NA)
            })
            completeCols = setdiff(unique(completeCols), NA)
            imp.models = list()
            errors = c()
            for (k in seq_len(length(completeCols))) {
                name = paste("col", j, "-set", k, sep = "")
                cols.complete = as.numeric(strsplit(completeCols[k], 
                  ",", fixed = TRUE)[[1]])
                rows.complete = which(apply(x.x, 1, function(tmp) {
                  all(cols.complete %in% which(!is.na(tmp)))
                }))
                x.x.complete = matrix(x.x[rows.complete, cols.complete], 
                  ncol = length(cols.complete))
                x.y.complete = x.y[rows.complete]
                if (length(unique(x.y.complete)) > 1 && ((family == 
                  "binomial" && length(table(x.y.complete, exclude = NULL)) == 
                  2 && min(table(x.y.complete, exclude = NULL)) > 
                  2) || (family == "gaussian" && (length(table(x.y.complete, 
                  exclude = NULL)) > 2 || (length(table(x.y.complete, 
                  exclude = NULL)) == 2 && min(table(x.y.complete, 
                  exclude = NULL)) > 2))))) {
                  imp.model = suppressWarnings(glmnet_fit(x.x.complete, 
                    x.y.complete, family))
                  imp.model$name = name
                  x.y.complete.pred = glmnet_predict(imp.model, 
                    x.x.complete)
                  imp.model$i = cols.complete[imp.model$i]
                  error = ifelse(family == "binomial", mean(((x.y.complete.pred > 
                    0.5) != x.y.complete)), sqrt(mean((x.y.complete.pred - 
                    x.y.complete)^2)))
                  names(error) = name
                }
                else {
                  imp.model = NULL
                  error = Inf
                }
                imp.models[[k]] = imp.model
                errors = c(errors, error)
            }
            list(family = family, data = x.y, imp.models = imp.models[order(errors)], 
                errors = errors[order(errors)])
        }
    stopCluster(cl)
    if (verbose) {
        cat("[impute.glmnet.matrix_fit] Running time:", difftime(Sys.time(), 
            start.time, units = "secs"), "seconds\n")
    }
    class(m) = "impute.glmnet.matrix_fit"
    m
}
surv2binary <-
function (x) 
{
    if (!inherits(x, "Surv")) {
        stop("x must be a \"Surv\" object")
    }
    times = sort(unique(x[, 1]))
    y = list()
    for (i in 1:length(times)) {
        x_i = x
        x_i[which(x_i[, 1] < times[i] & x_i[, 2] == 0), 2] = NA
        x_i[which(x_i[, 1] > times[i]), 2] = 0
        y[[i]] = list(time = times[i], status = x_i[, 2])
    }
    y
}
