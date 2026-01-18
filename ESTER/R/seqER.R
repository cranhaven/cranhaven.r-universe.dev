#' Computes sequential evidence ratios
#'
#' Computes sequential evidence ratios, either based on the AIC or the BIC.
#' Supported models currently include \code{lm}, \code{merMod}, or \code{brmsfit} models.
#' When data involve repeated measures (and so multiple lines per subject),
#' a column indicating the subject "id" should be provided to the \code{id} argument.
#' If nothing is passed to the \code{id} argument, \code{seqER} will suppose
#' that there is only one observation (i.e., one line) per subject.
#'
#' @param ic Indicates whether to use the aic or the bic.
#' @param mod1 A model of class \code{lm} or \code{lmerMod}.
#' @param mod2 A model of class \code{lm} or \code{lmerMod} (of the same class of mod1).
#' @param nmin Minimum sample size from which start to compute sequential evidence ratios.
#' @param id If applicable (i.e., repeated measures), name of the "id" column of your
#' dataframe, in character string.
#' @param boundary The Evidence Ratio (or its reciprocal) at which
#' the run is stopped as well
#' @param blind If true, the function only returns a "continue or stop" message
#' @param nsims Number of permutation samples to evaluate (is ignored if blind = TRUE)
#'
#' @importFrom stats family formula lm update
#' @importFrom lme4 lmer glmer
#' @importFrom magrittr %>%
#' @importFrom rlang f_lhs
#' @importFrom dplyr n_distinct
#' @import ggplot2
#' @import utils
#' @import brms
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' mod1 <- lm(mpg ~ cyl, mtcars)
#' mod2 <- lm(mpg ~ cyl + disp, mtcars)
#' seqER(ic = bic, mod1, mod2, nmin = 10)
#'
#' # Example with ten permutation samples
#' data(mtcars)
#' mod1 <- lm(mpg ~ cyl, mtcars)
#' mod2 <- lm(mpg ~ cyl + disp, mtcars)
#' seqER(ic = bic, mod1, mod2, nmin = 10, nsims = 10)
#'
#' # Example with blinding
#' data(mtcars)
#' mod1 <- lm(mpg ~ cyl, mtcars)
#' mod2 <- lm(mpg ~ cyl + disp, mtcars)
#' seqER(ic = bic, mod1, mod2, nmin = 10, boundary = 10, blind = TRUE)
#'
#' # Example with repeated measures
#' library(lme4)
#' data(sleepstudy)
#' mod1 <- lmer(Reaction ~ Days + (1|Subject), sleepstudy)
#' mod2 <- lmer(Reaction ~ Days + I(Days^2) + (1|Subject), sleepstudy)
#' seqER(ic = bic, mod1, mod2, nmin = 10, id = "Subject", nsims = 10)
#'
#' # Example with brmsfit models
#' library(brms)
#' mod1 <- brm(Reaction ~ Days + (1|Subject), sleepstudy)
#' mod2 <- brm(Reaction ~ Days + I(Days^2) + (1|Subject), sleepstudy)
#' seqER(ic = WAIC, mod1, mod2, nmin = 10, id = "Subject", nsims = 10)
#' }
#'
#' @author Ladislas Nalborczyk <\email{ladislas.nalborczyk@@gmail.com}>
#'
#' @seealso \code{\link{simER}}
#'
#' @export

seqER <-
    function(
        ic = bic, mod1, mod2, nmin = 10, id = NULL, boundary = Inf,
        blind = FALSE, nsims = NULL) {

    if (!class(mod1) == class(mod2) ) {

        stop("Error: mod1 and mod2 have to be of the same class")

    }

    if (nmin < 10) {

        warning("nmin should usually be set above 10...")

    }

    if (class(mod1) == "lm") {

        data <- data.frame(eval(mod1$call[["data"]], envir = parent.frame() ) )

    }

    if (class(mod1) == "glmerMod" | class(mod1) == "lmerMod") {

        data <- data.frame(eval(mod1@call$data, envir = parent.frame() ) )

    }

    if (class(mod1) == "brmsfit") {

        data <- data.frame(eval(mod1$data, envir = parent.frame() ) )

    }

    if (is.null(id) == TRUE) {

        id <- deparse(f_lhs(formula(mod1) ) )
        nobs <- 1
        data$ppt <- rep(seq(1, length(data[, id]), 1), each = nobs)

    } else {

        # count frequencies
        count <- data.frame(table(data[, id]) )

        # count number of observations by subject
        nobs <- max(count$Freq)

        # identify subjects with less than nobs
        a <- as.vector(count$Var1[count$Freq < nobs])

        data$ppt <-
            rep(seq(1, length (unique(data[, id]) ), 1), each = nobs)

        if (length(a) > 0) {

            # if needed, remove subjects with less than nobs
            for (i in 1:length(a) ) {

                data <- data[!data[, id] == as.numeric(a[i]), ]

            }

            warning("Different numbers of observation by subject.
                          Subjects with less than max(nobs)
                          have been removed.")
            }

    }

    startrow <- min(which(as.numeric(as.character(data$ppt) ) == nmin) )
    endrow <- nrow(data)

    for (i in seq(startrow, endrow, nobs) ) {

        maxrow <- i - 1 + nobs

        if ( (class(mod1) == "glmerMod") ) {

            mod1 <- glmer(formula(mod1),
                family = family(mod1)$family, data[1:maxrow, ])

            mod2 <- glmer(formula(mod2),
                family = family(mod2)$family, data[1:maxrow, ])

        }

        if ( (class(mod1) == "lmerMod") ) {

            mod1 <- lmer(formula(mod1),
                REML = FALSE, data[1:maxrow, ])

            mod2 <- lmer(formula(mod2),
                REML = FALSE, data[1:maxrow, ])

        }

        if ( (class(mod1) == "lm") ) {

            mod1 <- lm(formula(mod1), data[1:maxrow, ])

            mod2 <- lm(formula(mod2), data[1:maxrow, ])

        }

        if ( (class(mod1) == "brmsfit") ) {

            mod1 <- update(mod1, newdata = data[1:maxrow, ])

            mod2 <- update(mod2, newdata = data[1:maxrow, ])

        }

        tabtab <- ictab(list(mod1 = mod1, mod2 = mod2), ic)

        temp_er <- data.frame(cbind(data$ppt[i],
            tabtab$ic_wt[tabtab$modnames == "mod2"] /
                tabtab$ic_wt[tabtab$modnames == "mod1"]) )

        if (!exists("er") ) er <- temp_er else er <- rbind(er, temp_er)

        rm(temp_er)

    }

    colnames(er) <- c("ppt", "ER")

    if (blind == TRUE) {

        if (tail(abs(log(er$ER) ), 1) >= log(boundary) ) {

            return("stop the recruitment")

        } else {

            return("continue the recruitment")

        }
    }

    erb <-
        er %>%
        mutate(ERi = rep("er", max(.$ppt) - nmin + 1) ) %>%
        select_(~ERi, ~ppt, ~ER)

    if (!is.null(nsims) ) {

        for (i in 1:nsims) {

            data_temp <- data[sample(nrow(data), replace = FALSE), ]

            if (nobs > 1) {

                data_temp <-
                    data_temp[order(factor(data_temp$ppt,
                        levels = unique(data_temp$ppt) ) ), ]

                data_temp$ppt <- data$ppt

            } else {

                data_temp$ppt <- data$ppt

            }

            for (j in seq(startrow, endrow, nobs) ) {

                maxrow <- j - 1 + nobs

                if ( (class(mod1) == "glmerMod") ) {

                    mod1 <- glmer(formula(mod1),
                        family = family(mod1)$family, data_temp[1:maxrow, ])

                    mod2 <- glmer(formula(mod2),
                        family = family(mod2)$family, data_temp[1:maxrow, ])

                }

                if ( (class(mod1) == "lmerMod") ) {

                    mod1 <- lmer(formula(mod1),
                        REML = FALSE, data_temp[1:maxrow, ])

                    mod2 <- lmer(formula(mod2),
                        REML = FALSE, data_temp[1:maxrow, ])

                }

                if ( (class(mod1) == "lm") ) {

                    mod1 <- lm(formula(mod1), data_temp[1:maxrow, ])

                    mod2 <- lm(formula(mod2), data_temp[1:maxrow, ])

                }

                if ( (class(mod1) == "brmsfit") ) {

                    mod1 <- update(mod1, newdata = data_temp[1:maxrow, ])

                    mod2 <- update(mod2, newdata = data_temp[1:maxrow, ])

                }

                tabtab <- ictab(list(mod1 = mod1, mod2 = mod2), ic)

                temp_temp_erb <-
                    data.frame(
                        cbind(data_temp$ppt[j],
                            tabtab$ic_wt[tabtab$modnames == "mod2"] /
                                tabtab$ic_wt[tabtab$modnames == "mod1"]) )

                if (!exists("temp_erb") ) {

                    temp_erb <- temp_temp_erb

                    } else {

                        temp_erb <- rbind(temp_erb, temp_temp_erb)

                    }

                rm(temp_temp_erb)

            }

            temp_erb <-
                temp_erb %>%
                mutate(ERi = rep(paste0("er", i), nrow(.) ) ) %>%
                select(3, 1, 2) %>%
                set_names(c("ERi", "ppt", "ER") )

            erb <- rbind(erb, temp_erb)
            rm(temp_erb)

            set.seed(NULL)

        }

    }

    class(erb) <- c("seqER", "data.frame")
    return(erb)

}

#' @export

plot.seqER <- function(x, ... ) {

    aes_lines <- sqrt(0.75 / n_distinct(x$ERi) )

    ggplot(x, aes_string(x = "ppt", y = "ER", group = "ERi") ) +
        scale_y_log10() +
        geom_line(alpha = aes_lines, size = aes_lines) +
        geom_line(
            aes_string(x = "ppt", y = "ER", group = NULL),
            data = x[x$ERi == "er", ], size = 0.75) +
        theme_bw(base_size = 12) +
        xlab("Sample size") +
        ylab(expression(Evidence~ ~Ratio~ ~ (ER[10]) ) )

}
