#' Simulates sequential testing with evidence ratios
#'
#' Simulates one or many sequential testing with evidence ratios from independent two-groups
#' comparisons, as a function of sample size and standardized mean difference.
#' Evidence ratios are computed from the so-called Akaike weights from
#' either the Akaike Information Criterion or the Bayesian Information Criterion.
#'
#' @param cohensd Expected effect size
#' @param nmin Minimum sample size from which start computing ERs
#' @param nmax Maximum sample size at which stop computing ERs
#' @param boundary The Evidence Ratio (or its reciprocal) at which
#' the run is stopped as well
#' @param nsims Number of simulated samples (should be dividable by cores)
#' @param ic Indicates whether to use the aic or the bic
#' @param cores Number of parallel processes. If cores is set to 1, no parallel framework is used
#' (default is two cores).
#' @param verbose Show output about progress
#'
#' @return An object of class \code{data.frame}, which contains...
#'
#' @importFrom cowplot ggdraw insert_xaxis_grob insert_yaxis_grob theme_nothing
#' @importFrom parallel makeCluster stopCluster
#' @importFrom magrittr %>% set_names
#' @importFrom stats lm rnorm runif
#' @importFrom tidyr gather_
#' @import doParallel
#' @import foreach
#' @import ggplot2
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' sim <- simER(cohensd = 0.8, nmin = 20, nmax = 100, boundary = 10,
#' nsims = 100, ic = bic, cores = 2, verbose = TRUE)
#' plot(sim, log = TRUE, hist = TRUE)
#' }
#'
#' @author Ladislas Nalborczyk <\email{ladislas.nalborczyk@@gmail.com}>
#'
#' @seealso \code{\link{ictab}}, \code{\link{analysER}}
#'
#' @export

simER <- function(
    cohensd = 0, nmin = 20, nmax = 100, boundary = 10,
    nsims = 20, ic = bic, cores = 2, verbose = FALSE) {

    if (nmin == 0) {

        stop("nmin should be a positive integer")

    }

    if (nmin > nmax) {

        stop("n should be superior to nmin")

    }

    if (nsims %% cores != 0) {

        stop("nsims should be dividable by cores")

    }

    if (verbose) {

        cl <- makeCluster(cores, outfile = "")
        registerDoParallel(cl, cores)

        start <- Sys.time()
        print(paste0("Simulation started at ", start) )

        flush.console()

    } else {

        registerDoParallel(cores)

    }

    sim <-
        foreach(
            batch = 1:getDoParWorkers(), .combine = rbind) %dopar% {

            max_b <- round(nsims / getDoParWorkers() )
            res.counter <- 1

            res <-
                matrix(NA,
                    nrow = length(nmin:nmax) * max_b, ncol = 5,
                    dimnames =
                        list(
                            NULL,
                            c("id", "true.ES", "boundary", "n", "ER") ) )

            for (b in 1:max_b) {

                x <- cbind(rnorm(nmax / 2, 0, 1), rep(-0.5, nmax / 2) )
                y <- cbind(rnorm(nmax / 2, cohensd, 1), rep(0.5, nmax / 2) )

                df_pop <-
                    rbind(y, x) %>%
                    as.data.frame %>%
                    set_names(c("value", "group") ) %>%
                    sample_n(nrow(.) )

                if (verbose == TRUE)
                    print(
                        paste0(
                            Sys.time(), ": batch = ", batch,
                            "; Rep = ", b, "/",
                            round(nsims / getDoParWorkers() ) ) )

                res0 <-
                    matrix(
                        NA, nrow = length(nmin:nmax), ncol = ncol(res),
                        dimnames = dimnames(res) )

                for (i in nmin:nmax) {

                    samp <- df_pop[1:i, ]

                    mod1 <- lm(value ~ 1, data = samp)
                    mod2 <- lm(value ~ group, data = samp)

                    mods <- list(mod1 = mod1, mod2 = mod2)
                    model_comp <- ictab(mods, ic)

                    ER <-
                        model_comp$ic_wt[model_comp$modnames == "mod2"] /
                        model_comp$ic_wt[model_comp$modnames == "mod1"]

                    res0[which(nmin:nmax == i), ] <-
                        c(
                            # id is a unique id for each trajectory
                            id = batch * 10 ^ (floor(log(max_b, base = 10) ) + 2) + b,
                            true.ES	= cohensd,
                            boundary = boundary,
                            n = i,
                            ER = ER
                        )

                    if (ER >= boundary | ER <= 1 / boundary) {break;}

                } # end of i

                res[res.counter:(res.counter + nrow(res0) - 1), ] <- res0
                res.counter <- res.counter + nrow(res0)

            } # end of b's

            batch <- NULL
            return(res)

        } # end of %dopar%

    res <- data.frame(sim, stringsAsFactors = FALSE)
    class(res) <- c("simER", "data.frame")

    if (verbose) {

        end <- Sys.time()
        print(paste0("Simulation finished at ", end) )
        cat("Duration: ")
        print(end - start)

        stopCluster(cl)

    }

    return(res)

}

#' Plotting the results of \code{simER}
#'
#' Plotting the results of \code{simER}.
#'
#' @param x A \code{simER} object
#' @param log Should the y-axis be log-transformed ?
#' @param hist Should plot the histogram of simulations hitting either the lower,
#' the upper boundary, or stopping at nmax ?
#' @param ... Further arguments passed to \code{plot.default}
#'
#' @author Ladislas Nalborczyk <\email{ladislas.nalborczyk@@gmail.com}>
#'
#' @export

plot.simER <- function(x, log = TRUE, hist = TRUE, ... ) {

    x <- na.omit(x)

    boundary <- unique(x$boundary)
    logboundary <- log(sort(c(boundary, 1 / boundary) ) )

    bound_hit <- function(x) {

        if (any(x > boundary) ) {

            first <- which(x > boundary)[1]
            x[first:length(x)] <- boundary

        } else if (any(x < (1 / boundary) ) ){

            first <- which(x < (1 / boundary) )[1]
            x[first:length(x)] <- 1 / boundary

        } else{

            x <- x

        }

        return(x)

    }

    bound_na <- function(x) {

        if (any(x == boundary) ) {

            first <- which(x == boundary)[1]

            if (first < length(x) ) {

                x[(first + 1):length(x)] <- NA

            }

        } else if (any(x == 1 / boundary) ) {

            first <- which(x == 1 / boundary)[1]

            if (first < length(x) ) {

                x[(first + 1):length(x)] <- NA

            }

        } else {

            x <- x

        }

        return(x)

    }

    y <-
        x %>%
        group_by(id) %>%
        mutate_(
            "ER" = "bound_na(bound_hit(ER))") %>%
        ungroup()

    lower_boundary_hit <-
        y %>%
        group_by(id) %>%
        mutate_("log_ER" = "log(ER)" ) %>%
        filter_(.dots = list(~log_ER == logboundary[1]) )

    upper_boundary_hit <-
        y %>%
        group_by(id) %>%
        mutate_("log_ER" = "log(ER)" ) %>%
        filter_(.dots = list(~log_ER == logboundary[2]) )

    final_point_boundary <-
        y %>%
        group_by(id) %>%
        mutate_("log_ER" = "log(ER)" ) %>%
        filter_(.dots = list(~log_ER %in% logboundary) )

    "%!in%" <- function(x, y) !("%in%" (x, y) )

    nmax <-
        y %>%
        group_by(id) %>%
        mutate_("log_ER" = "log(ER)" ) %>%
        filter(n == max(n) ) %>%
        na.omit() %>%
        filter_(.dots = list(~log_ER %!in% logboundary) )

    aes_lines <- sqrt(sqrt(1 / n_distinct(y$id) ) )
    total <- max(n_distinct(final_point_boundary$id), n_distinct(nmax$id) )

    pMain <-
        y %>%
        ggplot(aes_string(
            x = "n", y = "ER",
            group = "id") ) +
        geom_line(alpha = aes_lines, size = aes_lines, na.rm = TRUE) +
        geom_point(data = final_point_boundary,
            aes_string(x = "n", y = "ER", group = "id"),
            alpha = 0.6) +
            {if (log) scale_y_log10()} +
            {if (log) annotation_logticks(sides = "l")} +
        theme_bw(base_size = 12) +
        theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            legend.title = element_blank() ) +
        {if (nrow(final_point_boundary) > 0) geom_hline(yintercept = boundary, lty = 2)} +
        {if (nrow(final_point_boundary) > 0) geom_hline(yintercept = 1 / boundary, lty = 2)} +
        xlab("Sample size") +
        ylab(expression(Evidence~ ~Ratio~ ~ (ER[10]) ) )

    if (hist) {

        n_xlim <- layer_scales(pMain)$x$range$range

        pTop <-
            ggplot(upper_boundary_hit, aes_string(x = "n") ) +
            geom_histogram(
                aes_string(y = "..count.."), na.rm = TRUE, binwidth = 1) +
            scale_x_continuous(
                limits = c(NA, n_xlim[2]) ) +
            scale_y_continuous(
                    limits = c(NA, total) ) +
            theme_nothing()

        pLow <-
            ggplot(lower_boundary_hit, aes_string(x = "n") ) +
            geom_histogram(
                aes_string(y = "..count.."), na.rm = TRUE, binwidth = 1) +
            scale_x_continuous(
               limits = c(n_xlim[1], n_xlim[2]) ) +
            scale_y_reverse(limits = c(NA, total) ) +
            theme_nothing()

        pRight <-
            ggplot(nmax, aes_string(x = "ER") ) +
            geom_histogram(
                aes_string(y = "..count.."), na.rm = TRUE, binwidth = 0.05) +
            coord_flip() +
            scale_x_log10(limits = c(1 / boundary, boundary) ) +
            scale_y_continuous(
                limits = c(NA, total) ) +
            theme_nothing()

        if (nrow(lower_boundary_hit) > 0 & nrow(upper_boundary_hit) > 0 & nrow(nmax) > 0 ) {

            p1 <-
                insert_xaxis_grob(
                    pMain, pTop, unit(.2, "null"), position = "top")

            p2 <-
                insert_yaxis_grob(
                    p1, pRight, unit(.2, "null"), position = "right")

            p3 <-
                insert_xaxis_grob(
                    p2, pLow, unit(.2, "null"), position = "bottom")

        ggdraw(p3)

        } else if (nrow(lower_boundary_hit) > 0 & nrow(upper_boundary_hit) > 0) {

            p1 <-
                insert_xaxis_grob(
                    pMain, pTop, unit(.2, "null"), position = "top")

            p2 <-
                insert_xaxis_grob(
                    p1, pLow, unit(.2, "null"), position = "bottom")

            ggdraw(p2)

        } else if (nrow(lower_boundary_hit) > 0 & nrow(nmax) > 0) {

            p1 <-
                insert_xaxis_grob(
                    pMain, pLow, unit(.2, "null"), position = "bottom")

            p2 <-
                insert_yaxis_grob(
                    p1, pRight, unit(.2, "null"), position = "right")

            ggdraw(p2)

        } else if (nrow(upper_boundary_hit) > 0 & nrow(nmax) > 0) {

            p1 <-
                insert_xaxis_grob(
                    pMain, pTop, unit(.2, "null"), position = "top")

            p2 <-
                insert_yaxis_grob(
                    p1, pRight, unit(.2, "null"), position = "right")

            ggdraw(p2)

        } else if (nrow(lower_boundary_hit) > 0) {

            p1 <-
                insert_xaxis_grob(
                    pMain, pLow, unit(.2, "null"), position = "bottom")

            ggdraw(p1)

        } else if (nrow(upper_boundary_hit) > 0) {

            p1 <-
                insert_xaxis_grob(
                    pMain, pTop, unit(.2, "null"), position = "top")

            ggdraw(p1)

        } else if (nrow(nmax) > 0 ) {

            p1 <-
                insert_yaxis_grob(
                    pMain, pRight, unit(.2, "null"), position = "right")

            ggdraw(p1)

        }

    } else {

        pMain

    }

}
