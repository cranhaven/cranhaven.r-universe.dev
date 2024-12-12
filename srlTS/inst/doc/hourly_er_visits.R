## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  fig.width = 7, 
  fig.height = 5
)
pvalr <- function(pvals, sig.limit = .001, digits = 3, html = FALSE) {
  
  roundr <- function(x, digits = 1) {
    res <- sprintf(paste0('%.', digits, 'f'), x)
    zzz <- paste0('0.', paste(rep('0', digits), collapse = ''))
    res[res == paste0('-', zzz)] <- zzz
    res
  }
  
  sapply(pvals, function(x, sig.limit) {
    if(is.na(x))
      return(x)
    if (x < sig.limit)
      if (html)
        return(sprintf('&lt; %s', format(sig.limit))) else
      return(sprintf('< %s', format(sig.limit)))
    if (x > .1)
      return(roundr(x, digits = 2)) else
    return(roundr(x, digits = digits))
  }, sig.limit = sig.limit)
}

## ----setup, include = FALSE---------------------------------------------------
library(dplyr)
library(kableExtra)
library(yardstick)
library(srlTS)

## ----getdata------------------------------------------------------------------
data("uihc_ed_arrivals")
str(uihc_ed_arrivals)

## ----plot_outcome-------------------------------------------------------------
y <- uihc_ed_arrivals$Arrivals
plot(y, type = "l")

## ----plot_pacf----------------------------------------------------------------
# number of maximum lags to consider
n_lags_max <- 24*7*5 # consider 5 weeks' data lags
pacfs <- pacf(ts(y), lag.max = n_lags_max, plot = F)
plot(pacfs)

## ----fit_endo-----------------------------------------------------------------
srlpac <- srlTS(y, n_lags_max = n_lags_max)

## ----print_endo---------------------------------------------------------------
srlpac

## ----plot_endo----------------------------------------------------------------
plot(srlpac)

## ----summary_endo-------------------------------------------------------------
summary(srlpac)

## ----getX---------------------------------------------------------------------
X_day <- as.matrix(dplyr::select(uihc_ed_arrivals, xmas:game_day))
X_month <- model.matrix(~relevel(factor(Month), ref = 3) + I(temp/10), 
                        data = uihc_ed_arrivals)[,-1]
X <- cbind(X_month, X_day)
colnames(X) <- gsub("relevel.factor.Month., ref = 3.", "Month", colnames(X))
head(X)

## ----fit_exo------------------------------------------------------------------
srlpacx <- srlTS(y, X=X, n_lags_max = n_lags_max, w_exo = "unpenalized")

## ----print_exo----------------------------------------------------------------
srlpacx

## ----plot_exo-----------------------------------------------------------------
plot(srlpacx)

## ----summary_exo, results="hide"----------------------------------------------
s <- summary(srlpacx)
s$unpenTable 

## ----format_exo_table, echo = FALSE-------------------------------------------
s$unpenTable%>% 
  mutate(p.value = pvalr(p.value)) %>% 
  kable(digits = 2) %>% 
  kable_minimal(full_width = FALSE)

## ----plot_exo_coefs-----------------------------------------------------------
b <- s$unpenTable[,1]
se_b <- s$unpenTable[,2]

ci_lb <- b - se_b * 1.96
ci_ub <- b + se_b * 1.96

old <- par(mar = c(5,9,4,2) + .1)
plot(b, length(se_b):1, xlim = range(ci_lb, ci_ub), pch = 20,
     col = 4, yaxt = "n", ylab = "", xlab = "Coefficient (Change in Hourly ER visits)")
abline(v = 0, lty = 2)
segments(x0 = ci_lb, x1 = ci_ub, y0 = length(se_b):1, lty = 2)
labs <- gsub("factor\\(Month\\)", "", names(b))
labs <- c(month.name[-3], "10-degree (F)", "Christmas", "Christmas+1",
          "New Year's Eve", "New Years Day",
          "Thanksgiving", "Thanksgiving+1", "Independence Day",
          "Hawkeye Gameday")
axis(2, length(se_b):1, labs, las = 2)
par(old)

## ----get_1s_predictions-------------------------------------------------------
p_1step_endo <- predict(srlpac)
p_1step_exo <- predict(srlpacx)

## ----get_ks_preds-------------------------------------------------------------
p_2step_endo <- predict(srlpac, n_ahead = 2)
p_2step_exo <- predict(srlpacx, n_ahead = 2)

p_10step_endo <- predict(srlpac, n_ahead = 10)
p_10step_exo <- predict(srlpacx, n_ahead = 10)

## ----pcors--------------------------------------------------------------------
preds <- cbind(p_1step_endo, p_2step_endo, p_10step_endo, 
               p_1step_exo, p_2step_exo, p_10step_exo)
cor(preds, use = "pairwise")

## ----pred_cumulative----------------------------------------------------------
y_c10hr <- RcppRoll::roll_sum(y, 10, align = "right", fill = NA)
p_10step_csum_endo <- predict(srlpac, n_ahead = 10, cumulative = 10)
p_10step_csum_exo <- predict(srlpacx, n_ahead = 10, cumulative = 10)

## ----mae_overall--------------------------------------------------------------
mae_vec(y_c10hr, p_10step_csum_endo)
mae_vec(y_c10hr, p_10step_csum_exo)

## ----mae_test-----------------------------------------------------------------
mae_vec(y_c10hr[-srlpac$train_idx], p_10step_csum_endo[-srlpac$train_idx])
mae_vec(y_c10hr[-srlpacx$train_idx], p_10step_csum_exo[-srlpacx$train_idx])

## ----rsq_overall--------------------------------------------------------------
rsq_vec(y_c10hr, p_10step_csum_endo)
rsq_vec(y_c10hr, p_10step_csum_exo)

## ----rsq_test-----------------------------------------------------------------
rsq_vec(y_c10hr[-srlpac$train_idx], p_10step_csum_endo[-srlpac$train_idx])
rsq_vec(y_c10hr[-srlpacx$train_idx], p_10step_csum_exo[-srlpacx$train_idx])

