#' @title Preprocess data
#' @description Preprocess data so they can be used as input for [train_frm()].
#' @param data dataframe with columns RT, NAME, SMILES
#' @param degree_polynomial defines how many polynomials get added (if 3 quadratic and cubic terms get added)
#' @param interaction_terms if TRUE all interaction terms get added to data set
#' @param verbose 0 == no output, 1 == show progress, 2 == show progress and warnings
#' @param nw number of workers to use for parallel processing
#' @return A dataframe with the preprocessed data
#' @keywords public
#' @examples
#' data <- head(RP, 3)
#' pre <- preprocess_data(data, verbose = 0)
#' @export
preprocess_data <- function(data = read_rp_xlsx(),
                            degree_polynomial = 1,
                            interaction_terms = FALSE,
                            verbose = 1,
                            nw = 1) {
    if (verbose == 0) catf <- function(...) invisible()

    if ("preprocess_data" %in% getOption("FastRet.mocks", c())) {
        catf("Mocking is enabled for 'preprocess_data'. Returning 'mockdata/RPCD_prepro.rds'.")
        return(readRDS(pkg_file("mockdata/RPCD_prepro.rds")))
    }
    catf("Preprocessing dataframe with dimension %d x %d", nrow(data), ncol(data))

    catf("Obtaining chemical descriptors using %d workers", nw)
    df_raw <- getCDs(data, verbose, nw) # nolint: object_usage_linter.
    catf("Resulting dataframe has dimension %d x %d", nrow(df_raw), ncol(df_raw))

    catf("Removing columns with NAs")
    has_nas <- apply(df_raw, 2, function(col) any(is.na(col)))
    df_noNAs <- df_raw[, !has_nas]
    catf("Resulting dataframe has dimension %d x %d", nrow(df_noNAs), ncol(df_noNAs))

    catf("Removing columns with variance close to zero")
    idx_zeroVar <- caret::nearZeroVar(df_noNAs)
    df <- df_noNAs[, -idx_zeroVar]
    catf("Resulting dataframe has dimension %d x %d", nrow(df), ncol(df))

    if (degree_polynomial >= 2 || interaction_terms) {
        catf("Moving columns RT, NAME, SMILES into a seperate dataframe")
        rt <- df[, which(colnames(df) %in% c("RT", "NAME", "SMILES"))]
        cd <- df[, -which(colnames(df) %in% c("RT", "NAME", "SMILES"))]

        cdp <- cd
        if (degree_polynomial >= 2) {
            catf("Adding polynomial predictors up to degree %d", degree_polynomial)
            npredictors <- ncol(cdp)
            for (p in c(2:degree_polynomial)) {
                for (predictor in c(2:npredictors)) {
                    new_name <- paste(colnames(cdp)[predictor], "^", p, sep = "")
                    cdp[, new_name] <- cdp[, predictor]^p
                }
            }
            catf("Resulting dataframe has dimension %d x %d", nrow(cdp), ncol(cdp))
        }

        cdpi <- cdp
        if (interaction_terms) {
            catf("Adding interaction terms (this can take a while)")
            npredictors <- ncol(cdpi)
            for (predictor in c(2:npredictors - 1)) {
                for (predictor2 in c((predictor + 1):npredictors)) {
                    new_name <- paste(colnames(cdpi)[predictor], "/", colnames(cdpi)[predictor2], sep = "")
                    cdpi[, new_name] <- cdpi[, predictor] / cdpi[, predictor2]
                }
            }
            cdpi[is.na.data.frame(cdpi)] <- 0
            catf("Resulting dataframe has dimension %d x %d", nrow(cdpi), ncol(cdpi))
        }

        catf("Readding RT, NAME, SMILES to dataframe")
        df <- cbind(rt, cdpi)
    }

    catf("Preprocessing finished")
    return(df)
}

#' @title Checks which chemical descriptors are suitable for linear models
#' @description This function checks which chemical descriptors are suitable for use in linear model. Chemical descriptors with missing values, near-zero variance or strong outlier values are considered as not suitable.
#' @param df Input data for performing the analysis. Must be a data frame with columns NAME, RT and SMILES.
#' @param verbose A logical value indicating whether to print verbose output.
#' @param nw The number of workers to use for parallel processing.
#' @return A data frame with the predictors and their suitability status.
#' @seealso [plot_lm_suitability()]
#' @keywords internal
#' @examples
#' x <- check_lm_suitability(head(RP, 3), verbose = FALSE, nw = 1)
#' @export
check_lm_suitability <- function(df = read_retip_hilic_data(),
                                 verbose = FALSE,
                                 nw = 2) {
    y <- df$RT
    cds <- getCDs(df, verbose = verbose, nw = nw)
    X <- cds[5:ncol(cds)]
    predictors <- colnames(X)
    n <- ncol(X)
    hasNAs <- apply(X, 2, function(x) any(is.na(x)))
    isAlmostConstant <- seq_len(ncol(X)) %in% caret::nearZeroVar(X)
    hasOutliers <- apply(X, 2, function(x) any(abs(x - median(x)) > 50 * mad(x)))
    isSuiteable <- !(hasNAs | isAlmostConstant | hasOutliers)
    V <- data.frame(predictors, hasNAs, isAlmostConstant, hasOutliers, isSuiteable)
    list(df = df, X = X, V = V)
}

#' @title Plot the suitability of predictors for linear models
#' @description This function creates one pdf page for every predictor inside `slist$X`. The pdf page consists of the following three plots shown next to each other:
#' 1. Histogram
#' 2. Density plot
#' 3. Scatterplot against `slist$df$RT`
#' The name of the predictor, its suitability, and the status of the checks for missing values, near-zero variance, and outliers are shown in the title of each plot.
#' @param slist A list containing the data frame `df`, the matrix `X`, and the data frame `V` from `check_lm_suitability()`.
#' @param pdfpath The path to the pdf file to save the plots.
#' @param descs Index of chemical descriptors to plot. Leave at NULL to plot all chemical descriptors.
#' @return No return value. The function is used for its side effect of creating a pdf file with the plots.
#' @seealso [check_lm_suitability()]
#' @keywords internal
#' @examples
#' df <- head(RP, 3)
#' slist <- check_lm_suitability(df, verbose = FALSE, nw = 1)
#' plot_lm_suitability(slist, descs = 1:5)
#' @export
plot_lm_suitability <- function(slist = check_lm_suitability(),
                                pdfpath = tempfile("lm_suitability", fileext = ".pdf"),
                                descs = NULL) {
    catf("Plotting suitability of predictors for linear models to file '%s'", pdfpath)
    pdf(pdfpath, width = 9, height = 3)  # A4 size in inches
    on.exit(dev.off(), add = TRUE)
    opar <- par(mfrow = c(1, 3), oma = c(2, 0, 2, 0), mar = c(1, 4, 1, 2))
    on.exit(par(opar), add = TRUE, after = FALSE)
    if (is.null(descs)) descs <- seq_len(ncol(slist$X))
    V <- slist$V[descs, ]
    X <- slist$X[, descs]
    RT <- slist$df$RT
    for (i in seq_len(ncol(X))) {
        x <- X[, i]
        name <- colnames(X)[i]
        state <- ifelse(V$isSuiteable[i], "Ok", "Bad")
        reasons <- c("Has NAs", "Almost Constant", "Has Outliers")
        reasons <- reasons[unlist(V[i, c("hasNAs", "isAlmostConstant", "hasOutliers")])]
        reasons <- paste(reasons, collapse = ", ")
        if (reasons != "") reasons <- sprintf(" (%s)", reasons)
        title <- sprintf("%s: %s%s", name, state, reasons)
        tryCatch({
            hist(x, main = "", xlab = "", xaxt = "n")  # removed x-axis
            plot(density(x), main = "", xlab = "", xaxt = "n")  # removed x-axis
            plot(x, RT, main = "", xlab = "", ylab = "RT", xaxt = "n")  # removed x-axis
            axis(
                side = 1,
                at = seq(min(x), max(x), length.out = 5),
                labels = seq(min(x), max(x), length.out = 5),
                outer = TRUE
            )  # added single x-axis at the bottom
            title(main = title, outer = TRUE)
            catf("[%d/%d] %s (PLOTTED SUCCESSFULLY)", i, ncol(X), name)
        }, error = function(e) {
            catf("[%d/%d] %s (FAILED)", i, ncol(X), name)
        })
    }
}