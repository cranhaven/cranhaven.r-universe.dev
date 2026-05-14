#' Get plotting theme
#'
#' This function sets the plotting theme for ggplot.
#'
#' @param textsize numeric value for base text size. Default is 9.
#'
get_plottheme <- function(textsize){
    mytheme <- theme_bw() + theme(axis.text = element_text(size = textsize),
                                  axis.title = element_text(size = textsize + 2),
                                  legend.text = element_text(size = textsize),
                                  legend.title = element_text(size = textsize + 2),
                                  strip.text.x = element_text(size = textsize),
                                  strip.text.y = element_text(size = textsize))
    return(mytheme)
}

#' Plot data
#'
#' This function plots raw, filtered, or simulated data.
#'
#' @param data data frame of raw, filtered, or simulated data. Must include the following columns: 'id' - stating the unique identifier for each subject; 'vl' - numeric vector stating the viral load measurements for each subject; 'time'- numeric vector stating the time at which each measurement was taken.
#' @param textsize numeric value for base text size in ggplot. Default is 9.
#' @param pointsize numeric value for point size in ggplot. Default is 1.
#' @param linesize numeric value for line width in ggplot. Default is 0.5.
#' @param facet_col numeric value for number of columns to use when faceting subject panels. Defaults to NULL (i.e. ggplot default).
#' @param detection_threshold numeric value indicating the detection threshold of the assay used to measure viral load. Default value is 20.
#' @import ggplot2
#' @export
#' @examples
#'
#' set.seed(1234567)
#'
#' simulated_data <- simulate_data(nsubjects = 20)
#'
#' plot_data(simulated_data)
#'
plot_data <- function(data, textsize = 9, pointsize = 1, linesize = 0.5,
                      facet_col = NULL, detection_threshold = 20){

    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package \"ggplot2\" is required for automated plotting.
             Either install it, or plot manually.")
    }

    mytheme <- get_plottheme(textsize)

    data %>% ggplot(aes(x = time, y = vl)) + geom_point(size = pointsize) +
        geom_hline(aes(yintercept = detection_threshold), size = linesize, linetype = "dashed") +
        facet_wrap(~ id, ncol = facet_col) + mytheme +
        scale_y_log10("HIV viral load") + scale_x_continuous("Time")
}



#' Plot model fits
#'
#' This function plots the output from model fitting.
#'
#' @param model_output output from model fitting using ushr().
#' @param type character string indicating whether the biphasic or single phase fits should be plotted. Must be either "biphasic", "single", or "triphasic". Defaults to "biphasic".
#' @param detection_threshold numeric value indicating the detection threshold of the assay used to measure viral load. Default value is 20.
#' @param textsize numeric value for base text size in ggplot. Default is 9.
#' @param pointsize numeric value for point size in ggplot. Default is 1.
#' @param linesize numeric value for line width in ggplot. Default is 0.5.
#' @param facet_col numeric value for number of columns to use when faceting subject panels. Defaults to NULL (i.e. ggplot default).
#' @export
#' @examples
#'
#' set.seed(1234567)
#'
#' simulated_data <- simulate_data(nsubjects = 20)
#'
#' model_output <- ushr(data = simulated_data)
#'
#' plot_model(model_output, type = "biphasic")
#'
plot_model <- function(model_output, type = "biphasic", detection_threshold = 20,
                       textsize = 9, pointsize = 1, linesize = 0.5,
                       facet_col = NULL){

    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package \"ggplot2\" is required for automated plotting.
             Either install it, or plot manually.")
    }
    if (is.null(model_output)) {
        stop("Please include model output from 'ushr()' for plotting. If you want to plot data, use 'plot_data()'.")
    }
    # get desired fits for plotting
    if (type == "biphasic") {
        fits <- model_output$biphasic_fits
    } else if (type == "single") {
        fits <- model_output$single_fits
    } else if (type == "triphasic") {
        fits <- model_output$triphasic_fits
    } else {
        stop("Invalid 'type' argumement. Must be one of 'single', 'biphasic', or 'triphasic'.")
    }

    if (is.null(fits)) {
        stop("There are no fits of the type you have chosen. Try specifying a different type of fit with type = 'biphasic', 'single', or 'triphasic'.")
    }
    if (nrow(fits) == 0) {
        stop("There are no fits of the type you have chosen. Try specifying a different type of fit with type = 'biphasic', 'single', or 'triphasic'.")
    }

    filtered_data <- model_output$data_filtered %>% filter(id %in% unique(fits$id))

    mytheme <- get_plottheme(textsize)

    filtered_data %>% ggplot() + geom_point(aes(x = time, y = vl, group = id), size = pointsize) +
        geom_line(data = fits, aes(x = time, y = fit, group = id), lty = 1, col = "black", size = linesize) +
        facet_wrap(~id, ncol = facet_col) + mytheme +
        xlab("Time") + scale_y_log10("HIV viral load") +
        geom_hline(aes(yintercept = detection_threshold), linetype = "dashed")
}


#' Plot pairwise parameter distributions
#'
#' This function creates pairwise scatterplots of the estimates parameters. The default plotting method requires GGally; if this package is not available, base R is used instead.
#'
#' @param model_output output from model fitting using ushr().
#' @param type character string indicating whether the biphasic or single phase fits should be plotted. Must be either "biphasic", "single", or "triphasic". Defaults to "biphasic".
#' @param textsize numeric value for base text size. Default is 9.
#' @param pointsize numeric value for point size. Default is 1.
#' @param linesize numeric value for line width; only used for GGally plots. Default is 0.5.
#' @importFrom graphics pairs
#' @export
#' @examples
#'
#' set.seed(1234567)
#'
#' simulated_data <- simulate_data(nsubjects = 20)
#'
#' model_output <- ushr(data = simulated_data)
#'
#' plot_pairs(model_output)

plot_pairs <- function(model_output, type = "biphasic", textsize = 9, pointsize = 1, linesize = 0.5) {
    if (requireNamespace("GGally", quietly = TRUE)) {

        mytheme <- get_plottheme(textsize)

        if (type == "biphasic") {

            if(is.null(model_output$biphasicCI)){
                stop("There are no fits of the type you have chosen. Try specifying a different type of fit with type = 'biphasic', 'single', or 'triphasic'.")
            }

            model_output$biphasicCI %>% select(-lowerCI, -upperCI) %>%
                spread(param, estimate) %>% #mutate(A = log10(A), B = log10(B)) %>%
                select(-id) %>%
                GGally::ggpairs(., lower = "blank",
                                diag = list(continuous = GGally::wrap("densityDiag", size = linesize)),
                                upper = list(continuous = GGally::wrap("points", size = pointsize)),
                                axisLabels = "none",
                                labeller = label_parsed, progress = FALSE) +
                mytheme
        } else if (type == "single") {

            if(is.null(model_output$singleCI)){
                stop("There are no fits of the type you have chosen. Try specifying a different type of fit with type = 'biphasic', 'single', or 'triphasic'.")
            }

            model_output$singleCI %>% select(-lowerCI, -upperCI) %>%
                spread(param, estimate) %>%
                select(-id) %>%
                GGally::ggpairs(., lower = "blank",
                                upper = list(continuous = GGally::wrap("points", size = pointsize)),
                                axisLabels = "none",
                                columnLabels = c("hat(B)", "hat(gamma)"),
                                labeller = label_parsed, progress = FALSE) +
                mytheme
        } else if (type == "triphasic") {

            if(is.null(model_output$triphasicCI)){
                stop("There are no fits of the type you have chosen. Try specifying a different type of fit with type = 'biphasic', 'single', or 'triphasic'.")
            }


            model_output$triphasicCI %>% select(-lowerCI, -upperCI) %>%
                spread(param, estimate) %>%
                select(-id) %>%
                GGally::ggpairs(., lower = "blank",
                                upper = list(continuous = GGally::wrap("points", size = pointsize)),
                                axisLabels = "none",
                                columnLabels = c("A","A[b]", "B", "delta", "delta[b]", "gamma"),
                                labeller = label_parsed, progress = FALSE) +
                mytheme
        } else {
            stop("Invalid 'type' argumement. Must be one of 'single', 'biphasic', or 'triphasic'.")
        }
    } else {
        print("Could not find package GGally; plotting with base R instead")

        # convert textsize to ~O(1) for base R
        axistext <- textsize/9

        if (type == "biphasic") {

            if(is.null(model_output$biphasicCI)){
                stop("There are no fits of the type you have chosen. Try specifying a different type of fit with type = 'biphasic', 'single', or 'triphasic'.")
            }

            model_output$biphasicCI %>% select(-lowerCI, -upperCI) %>%
                spread(param, estimate) %>%
                select(-id) %>%
                pairs(., pch = 19, cex = pointsize, cex.axis = textsize,
                      labels = c("A", "B", expression(delta), expression(gamma)))

        } else if (type == "single") {

            if(is.null(model_output$singleCI)){
                stop("There are no fits of the type you have chosen. Try specifying a different type of fit with type = 'biphasic', 'single', or 'triphasic'.")
            }

            model_output$singleCI %>% select(-lowerCI, -upperCI) %>%
                spread(param, estimate) %>%
                select(-id) %>%
                pairs(., pch = 19, cex = pointsize, cex.axis = textsize,
                      labels = c(expression(hat(B)), expression(hat(gamma))))

        } else if (type == "triphasic") {

            if(is.null(model_output$triphasicCI)){
                stop("There are no fits of the type you have chosen. Try specifying a different type of fit with type = 'biphasic', 'single', or 'triphasic'.")
            }

            model_output$triphasicCI %>% select(-lowerCI, -upperCI) %>%
                spread(param, estimate) %>%
                select(-id) %>%
                pairs(., pch = 19, cex = pointsize, cex.axis = textsize,
                      labels = c("A", expression(A[b]), "B", expression(delta), expression(delta[b]), expression(gamma)))
        } else {
            stop("Invalid 'type' argumement. Must be one of 'single', 'biphasic', or 'triphasic'.")
        }
    }
}



#' Plot time to suppression distribution
#'
#' This function plots a histogram of the time to suppression estimates.
#'
#' @param TTS_output output from estimating time to suppression (TTS) values using get_TTS()..
#' @param textsize numeric value for base text size on ggplot. Default is 9.
#' @param bins numeric value indicating the number of bins for the histogram. Default is 20.
#' @export
#' @examples
#'
#' set.seed(1234567)
#'
#' simulated_data <- simulate_data(nsubjects = 20)
#'
#' TTSestimates <- get_TTS(data = simulated_data, parametric = FALSE)
#'
#' plot_TTS(TTSestimates, bins = 5)
#'
plot_TTS <- function(TTS_output, textsize = 9, bins = 20){
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package \"ggplot2\" is required for automated plotting.
             Either install it, or plot manually.")
    }
    mytheme <- get_plottheme(textsize)

    ggplot(data = TTS_output, aes(x = TTS)) +
        geom_histogram(bins = bins, fill = "grey", colour = "black") +
        mytheme + ylab("Frequency") + xlab("Time to suppression")
}



#' Summarize model output
#'
#' This function summarizes the output of model fitting..
#'
#' @param model_output output from model fitting using ushr().
#' @param data dataframe of original data used for model fitting. Must include named 'id' column with subject identifiers.
#' @param stats logical TRUE/FALSE: should the median and sd lifespans also be returned? Default is FALSE.
#' @import dplyr
#' @import tidyr
#' @importFrom stats median sd
#' @return a list containing (i) a summary of which subjects were successfully fit using the biphasic or single phase models, with their corresponding infected cell lifespan estimates ('summary'); (ii) if stats = TRUE: summary statistics for the estimated parameters from the biphasic model ('biphasicstats'); and (iii) if stats = TRUE: summary statistics for the estimated parameters from the single phase model ('singlestats').
#' @export
#' @examples
#'
#' set.seed(1234567)
#'
#' simulated_data <- simulate_data(nsubjects = 20)
#'
#' model_output <- ushr(data = simulated_data)
#'
#' summarize_model(model_output, data = simulated_data)
#'
summarize_model <- function(model_output, data, stats = FALSE){

    # get triphasic parameter estimates and summary statistics
    if (length(model_output$triphasicCI) > 0){

        triphasicfits <- model_output$triphasicCI %>%
            select(- lowerCI, - upperCI) %>% spread(param, estimate) %>%
            mutate(ShortLifespanProductive = 1/delta,
                   ShortLifespanNonProductive = 1/delta_b,
                   LongLifespanNonProductive = 1/gamma, Model = "Triphasic")

        triphasicstats <- model_output$triphasicCI %>%
            select(- lowerCI, - upperCI) %>% spread(param, estimate) %>%
            mutate(ShortLifespanProductive = 1/delta,
                   ShortLifespanNonProductive = 1/delta_b,
                   LongLifespanNonProductive = 1/gamma) %>%
            gather(Param, estimate, A:LongLifespanNonProductive) %>%
            group_by(Param) %>%
            summarize(Median = median(estimate), SD = sd(estimate)) %>%
            mutate(Median = signif(Median, 3), SD = signif(SD, 3), Model = "Triphasic")

    } else {
        # get biphasic parameter estimates and summary statistics
        if (length(model_output$biphasicCI) > 0){

            biphasicfits <- model_output$biphasicCI %>%
                select(- lowerCI, - upperCI) %>% spread(param, estimate) %>%
                mutate(ShortLifespan = 1/delta, LongLifespan = 1/gamma, Model = "Biphasic")

            biphasicstats <- model_output$biphasicCI %>%
                select(- lowerCI, - upperCI) %>% spread(param, estimate) %>%
                mutate(ShortLifespan = 1/delta, LongLifespan = 1/gamma) %>%
                gather(Param, estimate, A:LongLifespan) %>%
                group_by(Param) %>%
                summarize(Median = median(estimate), SD = sd(estimate)) %>%
                mutate(Median = signif(Median, 3), SD = signif(SD, 3), Model = "Biphasic")
        }

        # get single phase parameter estimates and summary statistics
        if (length(model_output$singleCI) > 0){

            singlephasefits <- model_output$singleCI  %>%
                select(- lowerCI, - upperCI) %>% spread(param, estimate) %>%
                mutate(SingleLifespan = 1/gammahat, Model = "Single phase")

            singlestats <- model_output$singleCI %>%
                select(- lowerCI, - upperCI) %>% spread(param, estimate) %>%
                mutate(SingleLifespan = 1/gammahat) %>%
                gather(Param, estimate, Bhat:SingleLifespan) %>%
                group_by(Param) %>%
                summarize(Median = median(estimate), SD = sd(estimate)) %>%
                ungroup() %>%
                mutate(Median = signif(Median, 3), SD = signif(SD, 3), Model = "Single phase")
        }
    }

    # join output

    if (length(model_output$triphasicCI) > 0){
        allfits <- triphasicfits

        allinfo <- data %>% distinct(id) %>%
            mutate(Included = ifelse(id %in% allfits$id, "Yes", "No")) %>%
            left_join(allfits) %>%
            select(id, Included,
                   Model, ShortLifespanProductive, ShortLifespanNonProductive,
                   LongLifespanNonProductive) %>%
            mutate_if(is.numeric, round, 2) %>% mutate_all(~replace(., is.na(.), ""))

    } else if (length(model_output$singleCI) > 0 & length(model_output$biphasicCI) > 0){
        allfits <- biphasicfits %>% full_join(singlephasefits)

        allinfo <- data %>% distinct(id) %>%
            mutate(Included = ifelse(id %in% allfits$id, "Yes", "No")) %>%
            left_join(allfits) %>%
            select(id, Included,
                   Model, ShortLifespan, LongLifespan, SingleLifespan) %>%
            mutate_if(is.numeric, round, 2) %>% mutate_all(~replace(., is.na(.), ""))

    } else if (length(model_output$singleCI) == 0 & length(model_output$biphasicCI) > 0){
        allfits <- biphasicfits

        allinfo <- data %>% distinct(id) %>%
            mutate(Included = ifelse(id %in% allfits$id, "Yes", "No")) %>%
            left_join(allfits) %>%
            select(id, Included,
                   Model, ShortLifespan, LongLifespan) %>%
            mutate_if(is.numeric, round, 2) %>% mutate_all(~replace(., is.na(.), ""))

    } else if (length(model_output$singleCI) > 0 & length(model_output$biphasicCI) == 0){
        allfits <- singlephasefits

        allinfo <- data %>% distinct(id) %>%
            mutate(Included = ifelse(id %in% allfits$id, "Yes", "No")) %>%
            left_join(allfits) %>%
            select(id, Included,
                   Model, SingleLifespan) %>%
            mutate_if(is.numeric, round, 2) %>% mutate_all(~replace(., is.na(.), ""))
    }


    # return information
    if (stats & length(model_output$singleCI) > 0 & length(model_output$biphasicCI) > 0) {
        return(list(summary = allinfo, biphasicstats = biphasicstats, singlestats = singlestats))
    } else if (stats & length(model_output$singleCI) == 0 & length(model_output$biphasicCI) > 0) {
        return(list(summary = allinfo, biphasicstats = biphasicstats))
    } else if (stats & length(model_output$singleCI) > 0 & length(model_output$biphasicCI) == 0) {
        return(list(summary = allinfo, singlestats = singlestats))
    } else if (stats & length(model_output$triphasicCI) > 0) {
        return(list(summary = allinfo, triphasicstats = triphasicstats))
    } else {
        return(allinfo)
    }
}
