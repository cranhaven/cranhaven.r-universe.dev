#' Computes sequential evidence ratios for a given data set and permutation samples
#'
#' Computes sequential evidence ratios for a given data set as well as
#' for \code{order_nb} random permutations of this dataset. When data involve repeated
#' measures (and so multiple lines per subject), a column indicating the
#' subject "id" should be provided to the \code{id} argument. If nothing
#' is passed to the \code{id} argument, \code{seqERboot} will suppose
#' that there is only one observation (i.e., one line) per subject.
#'
#' @inheritParams seqER
#' @param order_nb Number of permutation samples to evaluate.
#'
#' @importFrom stats family formula lm
#' @importFrom lme4 lmer glmer
#' @importFrom magrittr %>%
#' @import ggplot2
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' mod1 <- lm(mpg ~ cyl, mtcars)
#' mod2 <- lm(mpg ~ cyl + disp, mtcars)
#' seqERboot(ic = bic, mod1, mod2, nmin = 10, order_nb = 20)}
#'
#' @author Ladislas Nalborczyk <\email{ladislas.nalborczyk@@gmail.com}>
#'
#' @seealso \code{\link{seqER}}
#'
#' @export

seqERboot <- function(
    ic, mod1, mod2, nmin, id = NULL, order_nb) {

    .Deprecated("seqER")

    if (class(mod1) == "lm") {

        data <- data.frame(eval(mod1$call[["data"]]) )

    }

    if (class(mod1) == "lmerMod" | class(mod1) == "glmerMod") {

        data <- data.frame(eval(mod1@call$data) )

    }

    erb <-
        seqER(ic, mod1, mod2, nmin, id) %>%
        mutate(ERi = rep("er", max(.$ppt) - nmin + 1) ) %>%
        select_(~ERi, ~ppt, ~ER)

    if (is.null(id) == TRUE) {

        id2 <- as.character(formula(mod1)[[2]] )
        nobs <- 1
        data$ppt <- rep(seq(1, length(data[, id2]), 1), each = nobs)

    } else {

        id2 <- id

        # count frequencies
        count <- data.frame(table(data[, id2]) )

        # count number of observations by subject
        nobs <- max(count$Freq)

        # identify subjects with less than nobs
        a <- as.vector(count$Var1[count$Freq < nobs])

        data$ppt <-
            rep(seq(1, length(unique(data[, id2]) ), 1), each = nobs)

        if (length(a) > 0) {

            # if needed, remove subjects with less than nobs
            for (i in 1:length(a) ) {

                data <- data[!data[, id2] == as.numeric(a[i]), ]

            }

        }

    }

    pb <- txtProgressBar(min = 0, max = order_nb, initial = 0, style = 3)

    for (i in 1:order_nb) {

        data_temp <- data[sample(nrow(data), replace = FALSE), ]

        if (nobs > 1) {

            data_temp <-
                data_temp[order(factor(data_temp$ppt,
                    levels = unique(data_temp$ppt) ) ), ]

        }

        if ( (class(mod1) == "glmerMod") ) {

            mod1 <- lme4::lmer(formula(mod1),
                family = family(mod1)$family, data_temp)

            mod2 <- lme4::lmer(formula(mod2),
                family = family(mod2)$family, data_temp)

        }

        if ( (class(mod1) == "lmerMod") ) {

            mod1 <- lme4::lmer(formula(mod1), REML = FALSE, data_temp)

            mod2 <- lme4::lmer(formula(mod2), REML = FALSE, data_temp)

        }

        if ( (class(mod1) == "lm") ) {

            mod1 <- lm(formula(mod1), data_temp)

            mod2 <- lm(formula(mod2), data_temp)

        }

        temp_erb <-
            seqER(ic, mod1, mod2, nmin, id) %>%
            mutate(ERi = rep(paste0("er", i), max(.$ppt) - nmin + 1) ) %>%
            select_(~ERi, ~ppt, ~ER)

        if (!exists("erb") ) erb <- temp_erb else erb <- rbind(erb, temp_erb)

        rm(temp_erb)
        setTxtProgressBar(pb, i)

    }

    class(erb) <- c("ERboot", "data.frame")

    return(erb)

}

#' @export

plot.ERboot <- function(x, ... ) {

    raw <- x[, 2:3][x[, 1] == "er", ]

    ggplot(x, aes_string(x = "ppt", y = "ER", group = "ERi") ) +
        scale_y_log10() +
        geom_line(alpha = 0.2) +
        geom_line(aes_string(x = "ppt", y = "ER", group = NULL),
            data = raw, size = 0.75) +
        theme_bw(base_size = 12) +
        xlab("Sample size") +
        ylab(expression(Evidence~ ~Ratio~ ~ (ER[10]) ) )

}
