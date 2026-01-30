# (C) Philippe J.S. De Brouwer -- 2021
# licensed under GNU Affero General Public License v3.0

#' Function to calculate the paygap as a ratio.
#'
#' This function calculates the entropy of a system with discrete states
#' @param d tibble, a tibble with columns as definded
#' @param x the name of the columns that contains the factor object to be used as explaining dimension for the paygap (defaults to 'gender')
#' @param y the name of the columns that contains the numeric value to be used to calculate the paygap  (could be salary or bonus for example)
#' @param x_ctrl the value in the column defined by x that should be isolated (this versus the others), defaults to 'F'
#' @param ctrl_var a control variable to be added (shows median per group for that variable)
#' @keywords calculate the controlled paygap for a dataset d - controlled by grade (seniority) and jobID (type of role)
#' @returns dataframe (with columns grade, jobID, salary_x_ctrl, salary_others, n_x_ctrl, n_others, paygap, confidence) , where "confidence" is one of the following: NA = not available (numbers are too low), "" = no bias detectable, "." = there might be some bias, but we're not sure, "*" = bias detected wit some degree of confidence, "**" = quite sure there is bias, "***" = trust us, this is biased.
#' @export
#' @importFrom stats complete.cases median wilcox.test t.test
#' @importFrom dplyr filter summarise mutate select add_row
#' @examples
#' df <- div_paygap(div_fake_team())
#' df

div_paygap <- function(d,
                       x         = 'gender',
                       y         = 'salary',
                       x_ctrl    = 'F',
                       ctrl_var  = 'age') {

  # bindings to avoid 'no global definition for ...'
  med_y_ctrl <- med_y_oth <- n_ctrl <- n_oth <- med_age_ctrl <- med_age_oth <- paygap <- pvalue <- conf <- NULL
  xx <- med <- NULL
  grade <- jobID <- salary <- median_salary <- NULL
  tibble <- NULL

  # local function to assign the stars
    loc_conf <- function(p) {
      stars <- ifelse (is.na(p), NA,
                      ifelse(p <= 0.001, "***",
                             ifelse(p <= 0.01, "**",
                                    ifelse(p <= 0.05, "*",
                                           ifelse(p <= 0.1, ".", "")))))
      stars
    }


  # Add a column xx that contains the binary labels to be used
  if (is.character(d[[x]]) | is.factor(d[[x]])) {
    # x is a factor --> we just need to collapse all other factors into one, labelled "others"
    d <- d %>%
      mutate(xx = if_else(.data[[x]] == x_ctrl, x_ctrl, "others"))
    x_oth <- "others"
  } else {
    # x is continuous --> create two groups
    medians <- d %>% group_by(grade, jobID) %>% summarise(median(.data[[x]]))
    colnames(medians)[3] <- "med"
    d <-  dplyr::full_join(d, medians) %>%
      mutate(xx = if_else(.data[[x]] < med, "L", "H"))
    colnames(d)[ncol(d) - 1] <- paste0(x, "_median")
    x_ctrl <- "L"
    x_oth <- "H"
  }
 # xx is now the binary variable to be used as x in wilcox.test


  suppressMessages({
    # Add median salary, age, n
    pg0_left <- d %>%
      group_by(xx, grade, jobID)           %>%
      summarise(median_salary = median(salary)#,
                # JUST ONE - we add the others later
#                med_age       = median(get(ctrl_var)),
#                n             = dplyr::n(),
#                ci_width      = loc_ci_width(salary)
      )


# format pg0 as pagyap matrix (spread back)
pg0_left <- pg0_left %>% tidyr::spread(xx, median_salary)
#colnames(pg0)[3:4] <- c(paste0(y, "_", x_ctrl), paste0(y, "_", x_oth))

# for each row on pg0, add the other information
pg0_right <- tibble(
  med_y_ctrl = numeric(0),
  med_y_oth  = numeric(0),
  n_ctrl = numeric(0),
  n_oth  = numeric(0),
  med_age_ctrl = numeric(0),
  med_age_oth  = numeric(0),
  paygap = numeric(0),
  pvalue = numeric(0),
  conf   = character(0)
)

for(k in 1:nrow(pg0_left)) {
  d0 <- d %>% dplyr::filter(grade == pg0_left$grade[k] & jobID == pg0_left$jobID[k])
  d0_ctrl <- d0 %>% dplyr::filter(xx == x_ctrl)
  d0_oth  <- d0 %>% dplyr::filter(xx == x_oth)
  pg0_right <- pg0_right %>%
    add_row(
      med_y_ctrl = median(d0_ctrl[[y]]),
      med_y_oth = median(d0_oth[[y]]),
      n_ctrl = nrow(d0_ctrl),
      n_oth  = nrow(d0_oth),
      med_age_ctrl = median(d0_ctrl$age),
      med_age_oth = median(d0_oth$age),
      paygap = med_y_ctrl / med_y_oth,
      pvalue = ifelse(n_ctrl > 0 & n_oth > 0, wilcox.test(d0[[y]] ~ d0$xx)$p.value, NA),
      conf   = loc_conf(pvalue)
    )
  } #for
}) # suppress messages

pg <- dplyr::bind_cols(pg0_left, pg0_right)
pg <- pg %>% select(-3, -4)  # leave out the doubles of median salary

# prepare the metatdata:
col_names <- c("grade",
               "jobID",
               paste0(y, "_", x_ctrl),
               paste0(y, "_", x_oth),
               paste0("n_", x_ctrl),
               paste0("n_", x_oth),
               paste0("med_age_", x_ctrl),
               paste0("med_age_", x_oth),
               "paygap",
               "p-value",
               "conf."
)

ttest <- t.test(pg$paygap, alternative = "two.sided", mu = 1)
narr = paste0("There are ", sum((as.numeric(is.na(pg$paygap)) - 1) * (-1)), " paygaps that can be calculated.\nThe average is ",
             round(mean(pg$paygap, na.rm = TRUE),3), ". We can be ", round(100 - ttest$p.value*100,2), "% sure that this is the result of bias.")

l <- list(data      = pg,
          colnames  = col_names,
          roundable_cols = c(3:10),
          round_digits   = c(0, 0, 0, 0, 1, 1, 3, 6),
          pg_mean   = mean(pg$paygap, na.rm = TRUE),
          pg_sd     = sd(pg$paygap, na.rm = TRUE),
          t_test    = ttest,
          narrative = narr,
          x         = x,
          y         = y,
          x_ctrl    = x_ctrl,
          x_oth     = x_oth,
          ctrl_var  = ctrl_var
          )
class(l) <- "paygap"
return(l)
}
