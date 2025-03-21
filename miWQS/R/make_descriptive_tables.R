# In makeJournalTables: Don't export
#
#'  Makes a typical Table 1 Covariate Summary in journal report.
#'
#' @family format
#' @keywords format
#'
#' @description  A summary of numerical and categorical covariates overall and across the treatment groups. Numbers and frequencies summarize categorical variables (using \code{formatZ}), means and sds summarize numeric variables (using \code{formatMeanSd}). Fisher's Exact Test (only for 2 levels), Pearson's general chi square ( > 2 levels), or Wilcoxon's Signed Rank Test were done for categorical or numerical ones respectively. P-values were rounded to 3 digits; the other statistics can be adjusted using the digits option. The data frame produced, summary.cov, can be copied and pasted into a Table 1 in a report. The dataframe also consists a column of names that makes it easy to subset and resort as desired.
#'
#' @details
#'  A dataset needs to be split up into numeric covariates and categorical covariates. This can be done using summary commands. If there are no numeric covariates, either enter NULL or have a dataset be with 0 columns. Similar is true with categorical covariates.
#'
#' With respect to the output, the user can easily export using write.csv(), and using the first column, sort by covariate name to obtain those desired. The other columns are formatted to be copied and paste into a peer-reviewed descriptive table.
#'
#' @note
#' \code{make.descriptive.table} uses most of the other functions to achieve this result.
#'
#' Please specify at least one of covariate.num or covariate.fac; if they are both missing, there is nothing to summarize.
#'
#' The "..." argument consists of the digits argument and other arguments as listed in \code{\link[base]{format}}.
#' The format.pval function is used to format P-values, which is imported from Hmisc.
#'
#' @param covariate.num A dataframe or matrix consisting of columns for the numeric covariates. Check with a summary. If there are none, put in NULL
#' @param covariate.fac A dataframe or matrix consisting of columns for character covariates. Check with a summary. If there are none, put in NULL
#' @param treatment     The treatment variable or case/control variable -- whatever variable used to distinguish between groups. Must be a character or factor.
#' @param digits   --Number of digits to be passed.
#' @inheritParams format.pval
#' @param ...  Additional arguments passed to code{\link[base]{format}} in formatting means/sd, percentages,and p-values. (except for drop0trailing is always FALSE)
#  Additional arguments passed to \code{\link[base]{format}}
#  (See functions \code{formatMeanSd}, \code{format.mean.z}, \code{\link[Hmisc]{format.pval}})
#  for help on these options.

#' @return A data frame with columns consisting of: \describe{
#'   \item{name}{The name of covariate : useful in sorting the table}
#'   \item{covariate}{ The levels of covariate}
#'   \item{...}{Varies; Overall and levels of treatment variable; the statistics are printed here.}
#'   \item{p.value}{The P-value of test calculated}
#'   \item{which.test}{ Which test is used (Wilcoxon, Chi Square, or Fisher's Exact Test). Can easily transport this using footnotes}
#'   \item{sig}{ A mark indicating if P-value is significant. An * is given if P-value < 0.05; NS (non-significant) is given if P-value > 0.05. Some journals like to mark significance so it is included here.} }
#'
#' @examples
#' \dontrun{
#'  #Example 1
#'  #Use the CO2 dataset from dataset package. Edit the CO2 dataset
#'  #so that the treatment has unequal number of chilled and unchilled.
#'  data(CO2)
#'  co2 <- CO2
#'  co2$Treatment[1:10] <- "chilled"
#'  summary(co2)
#'
#'  #Example 1a: numerical summary only
#'  make.descriptive.table( covariate.num =  co2[ ,4:5], treatment = co2$Treatment)
#'
#'  #Example 1b: categorical summary only
#'  make.descriptive.table( covariate.num = NULL,
#'                      covariate.fac =  co2[ , 2, drop = FALSE], co2$Treatment)
#'
#'  #Example 1c: Both are missing: Nothing to analyze.
#'  \dontrun{make.descriptive.table(NULL, NULL, co2$Treatment)}

#'  #Example 1d: Both types of covariates.
#'  summary.cov <- make.descriptive.table( covariate.num =  co2[ ,4:5],
#'                                        covariate.fac = co2[ , 2, drop = FALSE],
#'                                        treatment = co2$Treatment)
#'  unique(warnings())
#'  print(summary.cov)
#'  #Can write to csv and share with collaborators.
#'
#'  #Example 2: Alter the example to include missing values and do a summary.
#'  co2[ 42,"conc"] <- NA
#'  co2[ 45, "uptake"] <- NA
#'  summary(co2)
#'  make.descriptive.table( covariate.num =  co2[ ,4:5],
#'                         covariate.fac = co2[ , 2, drop = FALSE],   co2$Treatment)
#'  }
#'
#' @section warning:
#'   There will be issues if covariate.num or covariate.fac has 0 columns. If this is the case,
#'   please reformat by setting it to NULL.
#'
#' @importFrom Hmisc format.pval
# #' @export
#' @noRd

make.descriptive.table <- function(
  covariate.num = NULL, covariate.fac = NULL, treatment,
  digits = max(1, .Options$digits - 2),
  na.form = "NA", ...) {
 # require(Hmisc) #for format.pval function

  ## Check at least one of covariate.num and covariate.fac should not be NULL
  if (is.null(covariate.num) & is.null(covariate.fac)) {
    stop("There is nothing to summarize. At least one of covariate.num or covariate.fac must
          be non-null.", call. = FALSE)
  }

message("A summary of numerical and categorical covariates overall and across the treatment groups. Numbers and frequencies summarize categorical variables, means and sds summarize numeric variables. Fisher's Exact Test (only for 2 levels), Pearson's general chi square ( > 2 levels),  or Wilcoxon Signed Rank Test were done for categorical or numerical ones respectively. P-values were rounded to 3 digits. \n\n ")


  ## Initalize data frame to return
  summary.cov <- NA

  ## Summarize numeric covariates, only if non-null or has 0 columns.
  if (is.null(covariate.num)) {
    warning("There are no numeric covariates to summarize. \n")
  } else {
    # Convert covariate.num into a matrix if data.frame
    if (is.data.frame(covariate.num) | is.vector(covariate.num)) { covariate.num <- as.matrix(covariate.num) }

    # Numeric summary as a function used in aggregate.
    f <- function(x) {
      A <- rbind(mean(x, na.rm = TRUE),  sd(x, na.rm = TRUE), missing = sum(is.na(x)))
      rownames(A) <- c("mean", "sd", "missing")
      return(A)
    }

    # For each covariate.num:
    for (i in 1:ncol(covariate.num)) {

      # Dissect pieces needed.
      name <- colnames(covariate.num)[i]
      overall <-  f(covariate.num[, i])

      tmp <-  data.frame(numb = as.numeric(covariate.num[, i]), caco = treatment)
      by.treat <- aggregate(numb ~ caco, data = tmp, f)
      row.names(by.treat[, 2]) <- by.treat$caco
      A <- data.frame(overall = overall, t(by.treat[, 2]))
      formatted.A <- suppressWarnings(
        formatMeanSd(t(A[1:2, ]),  digits = digits, ...)
      )
      stats <-  rbind(t(formatted.A),
                       paste0(as.character(A["missing", ]),  ",")
      )
      p.value <- wilcox.test(numb ~ caco, data = tmp)$p.value

      # Save as a data frame
      p.val.form <- c(
        format.pval(p.value, digits = 3, eps = 0.001, na.form = na.form, ...),
        " "
        )
      df <- data.frame(
        name = paste0(rep(name, 2), ","),
        covariate =  paste0(c(name, "missing"), ","),
        stats,  # must leave alone so names from stats would form columns of data frame.
        p.value = paste0(p.val.form, ",")
      )
      summary.cov <- rbind(summary.cov, df)
    } # end for loop
     summary.cov$which.test <- "wilcox.test,"
  } # end numeric summary

  ## Factor Summaries if non-null.
  if (is.null(covariate.fac)) {
    warning("There are no categorical covariates to summarize. \n")
  } else {
    # Calculate Freqs & Percents differently if have missing values. Used twice in missing if/else
    # statement.
    # #' @param freq The frequency of a categorical variable
    # #' @inheritDotParams formatZ
    calculate.stats <- function(freq, digits, ...) {
      proportion <- prop.table (freq, margin = 2) * 100
      v <- matrix(NA, nrow = nrow(freq), ncol = 3)
      for (j in 1:3) {
        v[, j] <- formatZ(freq[, j], proportion[, j], digits, ...)
      }
      # v <- mapply( formatZ, data, MoreArgs = list( ... ))
      stats <- matrix(v, nrow(proportion), ncol(proportion), byrow = FALSE, dimnames(proportion))
      return(stats)
    }

    which.test <- rep("", ncol(covariate.fac));  names(which.test) <- colnames(covariate.fac)
    for (i in 1:ncol(covariate.fac)) {
      # Dissect pieces needed.
      cov.name <- colnames(covariate.fac)[i]
      freq <- cbind(
        overall   = table(covariate.fac[, i], useNA = "ifany"),
        table(covariate.fac[, i], treatment, useNA = "ifany")
        )
      # print( table(covariate.fac[ ,i]))

      # Summarize Freqs & Percents differently if have missing values.
      # If no missing values:
      if (!anyNA(row.names(freq))) {
        stats <- calculate.stats(freq)
      # If there are missing values, calculate percentages based on observed values. Attach the missing frequencies at the end.
      } else {
        index <- which(is.na(row.names(freq)))
        stats <- rbind(calculate.stats(freq[-index, ]),
                        missing = freq[index, ])
      }

      # Find the p-value depending on the levels of covariate.fac
      print(nlevels(covariate.fac[, i]))
      # covariate.fac[,i] <- if( is.character(covariate.fac[ ,i]) ) { as.factor(covariate.fac[,i]) }
      if (nlevels(covariate.fac[, i]) == 2) {
        which.test <- "Fisher"
        p.val <- fisher.test(table(covariate.fac[, i], treatment))$p.value
      } else { if (nlevels(covariate.fac[, i]) > 2) {
        which.test <- "ChiSquare"
        test <- chisq.test(table(covariate.fac[, i], treatment), correct = TRUE)
        # Expected cell count is 5 or more in 80% of cells in larger tables.
        expect.assumption <- sum(test$expected > 5) > 0.8 * length(test$expected > 5)
        if (!expect.assumption) {
          warning("Expected cell count is not 5 or more in 80% of cells in larger tables")
          print(test$expected)
          # p.val <- NA
        } # end if expect.assumption
        p.val <- test$p.val
        # Check expected number.
        if (any(test$expected == 0)) {
          warning("0 is observed in at least one of cells. Contunity correction is applied.")
        }
      } else { # if( nlevels(covariate.fac[ ,i]) < 2 )
        p.val <- NA
      } } # end if for nlevels

      # Save as a data frame, combining with summary.cov in past.
      df <-  data.frame(
        name = paste0(rep(cov.name, nrow(stats) + 1), collapse = ","),
        covariate =  paste0(
          c(cov.name, as.character(row.names(stats))),
          collapse = ","),
        rbind(",", stats),  # must leave alone so names from stats would form columns of data frame.
        p.value = paste0(
          c(format.pval(p.val, digits = 3, eps = 0.001, na.form = na.form, ...),
             rep(" ", nrow(stats))
          ),
          collapse = ","),  # end paste0
        which.test = paste0(which.test, collapse = ",")
      ) # end df
      summary.cov <- rbind(summary.cov, df)
    } # end for loop for columns
  } # end if covariate.fac is.null

  # Modify summary.cov output and return.
  summary.cov <- summary.cov[-1, ]  # Remove first line as holder (NA).
  row.names(summary.cov) <- NULL    # Modify row.names from tables.

  # Include significance index
  p.val <- as.numeric(strsplit(as.vector(summary.cov$p.value), ","))
  summary.cov$sig <- ifelse(is.na(p.val), ",",
                         ifelse(p.val < 0.05, "*,", "NS,")
  )

  return(summary.cov)
}

# gsub("*", " ", summary.cov[summary.cov$name == treatment, 4:5], perl = TRUE)
# summary.cov$name <- sub( treatment, paste0("__", treatment), summary.cov$name)  #Rename treatment group. Need to test before uncomment

# summary.cov  <- apply(summary.cov, 2, as.character)
#

# save.image("Table1_covariate_function.RData")
# cat("Files are saved in: \n", getwd() )

## @inheritParams formatZ
# digits


# if(is.null(covariate.fac) ){
#  warning("There are no categorical covariates to summarize.")
# }else{
# Convert covariate.fac into a matrix if data.frame
# if( is.data.frame(covariate.fac) | is.factor(covariate.fac) ) {
#  covariate.fac <- as.matrix(covariate.fac)
# }
# numb.fac <- if( is.factor(covariate.fac) ) { 1 } else { ncol(covariate.fac) }
  # Calculate Freqs
