#' @description Create boxplots for up to 4 models to compare their performance measures
#' @param models A list of objects of class `frm` as returned by [train_frm()].
#' @param ptype A string specifying the plot type. Options are: "base" (default) and "ggplot2".
#' @noRd
plot_boxplot <- function(model = train_frm(), ptype = "base") {
    ptype <- match.arg(ptype, c("base", "ggplot2"))
    stats <- as.data.frame(collect(model$cv$stats))
    p <- if (ptype == "base") {
        opar <- par(mar = c(2, 2, 2, 0) + 0.5)
        on.exit(par(opar), add = TRUE)
        boxplot(stats, main = "CV Performance across folds", xlab = "", ylab = "", cex.axis = 0.8)
    } else {
        stats_lf <- reshape(stats, varying = names(stats), v.names = "Value", timevar = "Measure", times = names(stats), direction = "long")
        p <- ggplot(data = stats_lf, mapping = aes(x = .data$Measure, y = .data$Value))
        p <- p + geom_boxplot()
        p <- p + ggtitle("CV Performance across folds")
        p <- p + theme_minimal()
        p <- p + theme(
            axis.title = element_blank(),
            plot.title = element_text(hjust = 0.5)
        )
    }
}

#' @description Plot predictions of a FastRet Model (FRM) for its training data. If the FRM was adjusted, the original and adjusted predictions are plotted.
#' @param frm An object of class `frm` as returned by [train_frm()].
#' @param type A string specifying the plot type. Options are:
#' - "scatter.cv": Cross-validation predictions for the full training set
#' - "scatter.cv.adj": Cross-validation predictions for the adjustment set
#' - "scatter.train": Model predictions for the training set
#' - "scatter.train.adj": Adjusted model predictions for the adjustment set
#' @param trafo A string specifying the transformation to apply to the data before plotting. Options are:
#' - "identity": No transformation
#' - "log2": Apply the log2 transformation
#' @noRd
plot_frm <- function(frm = train_frm(verbose = 1),
                     type = "scatter.cv", # c("scatter.cv", "scatter.cv.adj", "scatter.train", "scatter.train.adj")
                     trafo = "identity" # c("identity", "log2")
                     ) {
    # Check args
    type <- match.arg(type, c("scatter.cv", "scatter.cv.adj", "scatter.train", "scatter.train.adj"))
    if (grepl("adj", type) && is.null(frm$adj)) stop(sprintf("type is `%s`, but the model has not been adjusted yet.\nSee `?adjust_model` for information on how to Adjust existing Models.", type))
    trafo <- match.arg(trafo, c("identity", "log2"))
    dotrafo <- switch(trafo, "identity" = identity, "log2" = log2)

    # Prepare data for plotting
    title <- switch(type, "scatter.cv" = "CV predictions for training data", "scatter.cv.adj" = "CV predictions for adjustment data", "scatter.train" = "Model predictions for training data", "scatter.train.adj" = "Adjusted model predictions for adjustment data")
    df <- if (grepl("adj", type)) frm$adj$df else frm$df
    x <- df$RT
    y <- switch(type, "scatter.cv" = frm$cv$preds, "scatter.cv.adj" = frm$adj$cv$preds, "scatter.train" = predict(frm, df, adjust = FALSE), "scatter.train.adj" = predict(frm, df, adjust = TRUE))
    is_within_1min <- (y > x - 1 & y < x + 1)
    is_min_outlier <- (y < (min(x) - mean(x)))
    is_max_outlier <- (y > (max(x) + mean(x)))
    is_outlier <- is_min_outlier | is_max_outlier
    col <- rep("blue", length(y))
    col[is_within_1min] <- "green"
    col[is_outlier] <- "red"
    y[is_min_outlier] <- min(x) - mean(x)
    y[is_max_outlier] <- max(x) + mean(x)
    R <- cor(x, y)
    MSE <- mean((x - y)^2)
    MAE <- mean(abs(x - y))
    x <- dotrafo(x) # Important: do the transformation after calculating the performance measures, as we only want to SHOW the transformed data, but not use it for calculations
    y <- dotrafo(y)

    # Plot data
    plot(
        x = x, y = y, col = col,
        xlab = if (trafo == "identity") "RT (Measured)" else "log2(RT) (Measured)",
        ylab = if (trafo == "identity") "RT (Predicted)" else "log2(RT) (Predicted)"
    )
    title(title)
    abline(a = 0, b = 1, col = "black")
    legend(
        x = "bottomright",
        legend = c(
            "Identity",
            sprintf("|y - x| < 1 min (n = %d)", sum(is_within_1min)),
            sprintf("|y - x| > 1 min (n = %d)", sum(!is_within_1min)),
            sprintf("y notin [min(x) - mu; max(x) + mu] (n = %d)", sum(is_outlier))
        ),
        col = c("black", "green", "blue", "red"),
        pch = c(NA, 1, 1, 1),
        lty = c(1, 0, 0, 0)
    )
    legend(
        x = "topleft", col = "black", lty = 0, pch = NA,
        legend = c(sprintf("R = %.2f", R), sprintf("MSE = %.2f", MSE), sprintf("MAE = %.2f", MAE))
    )
}
