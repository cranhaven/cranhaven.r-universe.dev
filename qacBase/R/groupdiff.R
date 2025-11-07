#' @title
#' Test of group differences
#'
#' @description
#' One-way analysis (ANOVA or Kruskal-Wallis Test)
#' with post-hoc comparisons and plots
#'
#' @param y a numeric response variable
#' @param x a categorical explanatory variable. It will coerced to be
#' a factor.
#' @param data a data frame.
#' @param method character. Either \code{"anova"}, or \code{"kw"} (see details).
#' @param digits Number of significant digits to print.
#' @param horizontal logical. If \code{TRUE}, boxplots are plotted horizontally.
#' @param posthoc logical. If \code{TRUE}, the default, perform pairwise post-hoc comparisons
#' (TukeyHSD for ANOVA and Conover Test for Kuskal Wallis). This test
#' will only be performed if there are 3 or more levels for X.
#' @import dplyr
#' @import ggplot2
#' @importFrom multcompView multcompLetters
#' @importFrom PMCMRplus kwAllPairsConoverTest
#' @importFrom stats TukeyHSD aov dist hclust kruskal.test mad median sd
#' @return a list with 3 components:
#' \describe{
#' \item{\code{result}}{omnibus test}
#' \item{\code{summarystats}}{summary statistics}
#' \item{\code{plot}}{ggplot2 graph}
#' }
#' @export
#' @details
#' The \code{groupdiff} function performs one of two analyses:
#' \describe{
#' \item{\code{anova}}{A one-way analysis of variance, with TukeyHSD
#' post-hoc comparisons.}
#' \item{\code{kw}}{A Kruskal Wallis Rank Sum Test, with Conover
#' Test post-hoc comparisons.}
#' }
#' In each case, summary statistics and a grouped boxplots are
#' provided. In the parametric case, the statistics are n, mean, and
#' standard deviation. In the nonparametric case the statistics are
#' n, median, and median absolute deviation. If \code{posthoc = TRUE},
#' pairwise comparisons of superimposed on the boxplots.
#' Groups that share a letter are not significantly different (p < .05),
#' controlling for multiple comparisons.
#' @seealso \link[PMCMRplus]{kwAllPairsConoverTest},
#' \link[multcompView]{multcompLetters}.
#' @examples
#' # parametric analysis
#' groupdiff(cars74, hp, gear)
#'
#' # nonparametric analysis
#' groupdiff(cardata, popularity, vehicle_style, posthoc=TRUE,
#'           method="kw", horizontal=TRUE)
groupdiff <- function(data, y, x, 
                      method=c("anova", "kw"),
                      digits=2,
                      horizontal=FALSE,
                      posthoc=FALSE){

  y <- as.character(substitute(y))
  x <- as.character(substitute(x))

  data <- na.omit(data[c(x, y)])
  
  formula <- as.formula(paste(y, "~", x))

  Letters <- NULL

  data[x] <- factor(data[[x]])

  if (posthoc & length(levels(data[[x]])) < 3){
    message("Note: Post-hoc comparisons require 3 or more groups.\n")
    posthoc <- FALSE
  }

  data[x] <- reorder(data[[x]], data[[y]], mean, na.rm=TRUE)
  levels(data[[x]]) <- gsub("-", " ", levels(data[[x]]))

  # calculate offset
  if (posthoc){
    offset <- (max(data[[y]]) - min(data[[y]]))/10
  } else {
    offset <- 0
  }


  method <- match.arg(method)
  if(method == "anova"){

    # omnibus
    a <- aov(formula, data)
    fit <- unlist(summary(a))
    F <- round(fit[7], digits)
    df1 <- fit[1]
    df2 <- fit[2]
    p   <- fit[9]
    fit.result <- paste0("F(", df1, ", ", df2, ") = ", F,
                         ", p-value = ", format.pval(p, digits))

    # summary statistics
    mystats <-  data %>%
      group_by(.data[[x]]) %>%
      summarise(n = n(),
                mean = round(mean(.data[[y]], na.rm=TRUE), digits),
                sd = round(sd(.data[[y]], na.rm=TRUE), digits),
                .groups = 'drop') %>%
      as.data.frame()

    # posthoc comparisons (for more than 2 levels)
    if (posthoc){
      tHSD <- TukeyHSD(a, ordered = FALSE, conf.level = 0.95)
      Tukey.levels <- tHSD[[1]][,4]
      Tukey.labels <- multcompLetters(Tukey.levels)['Letters']
      plot.labels <- names(Tukey.labels[['Letters']])
      plot.levels <- data.frame(plot.labels, Tukey.labels,
                                stringsAsFactors = FALSE,
                                row.names=NULL)
      plot.levels$y <- max(data[y], na.rm=TRUE) + offset
    }


    # plot
    p <- ggplot(data, aes_string(x=x, y=y)) +
      geom_hline(yintercept=mean(data[[y]], na.rm=TRUE),
                 linetype="dashed",
                 color="darkgrey") +
      geom_boxplot(aes_string(fill=x)) +
      geom_point(data=mystats,
                 mapping=aes_string(x=x, y="mean"),
                 size=1, shape=3) +
      theme_minimal() +
      theme( plot.subtitle = element_text(size = 9),
             plot.caption = element_text(size=8),
             legend.position="none") +
      labs(title = "Group Differences",
           subtitle = paste("ANOVA:", fit.result),
           caption = "Crosses are group means. Dashed line is the mean value for all obs.")
    if(posthoc){
      p <- p +  geom_text(data = plot.levels,
                          aes(x = plot.labels,
                              y = y,
                              label = Letters),
                              size=3, fontface="italic") +
        labs(caption = "Boxplots that share letters are not significantly different (TukeyHSD p < 0.05).\nCrosses are group means. Dashed line is the mean value for all obs.")
    }


    if(horizontal) p <- p + coord_flip()

  }

  if(method == "kw"){

    # omnibus
    fit <- kruskal.test(formula, data)
    fit.result <- paste("Chi-squared = ",
                        round(fit$statistic, digits),
                        ", df =",
                        fit$parameter,
                        ", p-value =",
                        format.pval(fit$p.value, digits))


    # summary statistics
    mystats <-  data %>%
      group_by(.data[[x]]) %>%
      summarise(n = n(),
                median = round(median(.data[[y]], na.rm=TRUE), digits),
                mad = round(mad(.data[[y]], na.rm=TRUE), digits),
                .groups = 'drop') %>%
      as.data.frame()



    # posthoc
    if(posthoc){
      phtest <- kwAllPairsConoverTest(formula, data)
      n <- ncol(phtest$p.value)
      w <- matrix(nrow=n+1, ncol=n+1)
      nms <- c(dimnames(phtest$p.value)[[2]], dimnames(phtest$p.value)[[1]][n])
      dimnames(w) <- list(nms, nms)
      w[2:nrow(w), 1:n] <- phtest$p.value

      makeSymm <- function(m) {
        m[upper.tri(m)] <- t(m)[upper.tri(m)]
        return(m)
      }
      w <- makeSymm(w)
      plot.levels <- data.frame(
        plot.labels = rownames(w),
        Letters = as.list(multcompLetters(w))$Letters,
        y = max(data[y], na.rm=TRUE) + offset
      )
    }


    # plot
    p <- ggplot(data, aes_string(x=x, y=y)) +
      geom_hline(yintercept=median(data[[y]], na.rm=TRUE),
                 linetype="dashed",
                 color="darkgrey") +
      geom_boxplot(aes_string(fill = x)) +
      theme_minimal() +
      theme( plot.subtitle = element_text(size = 9),
             plot.caption = element_text(size=8),
             legend.position="none") +
      labs(title = "Group Differences",
           subtitle = paste("Kruskal-Wallis:", fit.result),
           caption = "Dashed line is the median value for all obs.")

    if(posthoc){
      p <- p + geom_text(data = plot.levels,
                aes(x = plot.labels,
                    y = y,
                    label = Letters),
                size=3, fontface="italic") +
        labs(caption = "Boxplots that share letters are not significantly different (Conover Test p < 0.05).\nDashed line is the median value for all obs.")
    }

    if(horizontal) p <- p + coord_flip()
  }

  # return results
  result <- list(result = fit.result, summarystats = mystats, plot=p)
  return(result)
}


