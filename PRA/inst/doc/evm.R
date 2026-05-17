## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(PRA)

## -----------------------------------------------------------------------------
bac <- 500000
schedule <- c(0.10, 0.25, 0.50, 0.75, 1.00)
time_period <- 3

## ----results='asis'-----------------------------------------------------------
pv_val <- pv(bac, schedule, time_period)
cat("Planned Value (PV): $", format(pv_val, big.mark = ","), "\n")

## ----results='asis'-----------------------------------------------------------
actual_per_complete <- 0.40
ev_val <- ev(bac, actual_per_complete)
cat("Earned Value (EV): $", format(ev_val, big.mark = ","), "\n")

## ----results='asis'-----------------------------------------------------------
period_costs <- c(45000, 110000, 135000)
ac_val <- ac(period_costs, time_period, cumulative = FALSE)
cat("Actual Cost (AC): $", format(ac_val, big.mark = ","), "\n")

## ----results='asis'-----------------------------------------------------------
sv_val <- sv(ev_val, pv_val)
cv_val <- cv(ev_val, ac_val)
spi_val <- spi(ev_val, pv_val)
cpi_val <- cpi(ev_val, ac_val)

cat("Schedule Variance (SV):          $", format(sv_val, big.mark = ","), "\n")
cat("Cost Variance (CV):              $", format(cv_val, big.mark = ","), "\n")
cat("Schedule Performance Index (SPI):", round(spi_val, 3), "\n")
cat("Cost Performance Index (CPI):    ", round(cpi_val, 3), "\n")

## ----results='asis'-----------------------------------------------------------
eac_typical <- eac(bac, method = "typical", cpi = cpi_val)
eac_atypical <- eac(bac, method = "atypical", ac = ac_val, ev = ev_val)
eac_combined <- eac(bac,
  method = "combined", cpi = cpi_val, ac = ac_val,
  ev = ev_val, spi = spi_val
)

cat("EAC (typical):  $", format(round(eac_typical), big.mark = ","), "\n")
cat("EAC (atypical): $", format(round(eac_atypical), big.mark = ","), "\n")
cat("EAC (combined): $", format(round(eac_combined), big.mark = ","), "\n")

## -----------------------------------------------------------------------------
eac_table <- data.frame(
  Method = c("Typical", "Atypical", "Combined"),
  EAC = c(round(eac_typical), round(eac_atypical), round(eac_combined)),
  Overrun = c(
    round(eac_typical - bac),
    round(eac_atypical - bac),
    round(eac_combined - bac)
  ),
  Assumption = c(
    "Current CPI continues",
    "Future work at planned rate",
    "CPI and SPI both factor in"
  )
)
knitr::kable(eac_table,
  format.args = list(big.mark = ","),
  caption = "EAC Comparison by Method"
)

## ----results='asis'-----------------------------------------------------------
etc_val <- etc(bac, ev_val, cpi_val)
vac_val <- vac(bac, eac_typical)

# TCPI to meet original BAC
tcpi_bac <- tcpi(bac, ev_val, ac_val, target = "bac")

# TCPI to meet revised EAC (typical)
tcpi_eac <- tcpi(bac, ev_val, ac_val, target = "eac", eac = eac_typical)

cat("Estimate to Complete (ETC):         $", format(round(etc_val), big.mark = ","), "\n")
cat("Variance at Completion (VAC):       $", format(round(vac_val), big.mark = ","), "\n")
cat("TCPI (to meet BAC):                ", round(tcpi_bac, 3), "\n")
cat("TCPI (to meet EAC):                ", round(tcpi_eac, 3), "\n")

## ----warning=FALSE------------------------------------------------------------
time_periods <- c(1, 2, 3)
actual_pct <- c(0.08, 0.22, 0.40)
p_costs <- c(45000, 110000, 135000)

pv_vals <- sapply(time_periods, function(t) pv(bac, schedule, t))
ac_vals <- cumsum(p_costs)
ev_vals <- sapply(actual_pct, function(a) ev(bac, a))

trend_data <- data.frame(
  Period = time_periods,
  PV     = pv_vals,
  AC     = ac_vals,
  EV     = ev_vals
)

p <- ggplot2::ggplot(trend_data, ggplot2::aes(x = Period)) +
  ggplot2::geom_line(ggplot2::aes(y = PV, color = "Planned Value (PV)"), linewidth = 1.2) +
  ggplot2::geom_line(ggplot2::aes(y = AC, color = "Actual Cost (AC)"), linewidth = 1.2) +
  ggplot2::geom_line(ggplot2::aes(y = EV, color = "Earned Value (EV)"), linewidth = 1.2) +
  ggplot2::geom_point(ggplot2::aes(y = PV, color = "Planned Value (PV)"), size = 3) +
  ggplot2::geom_point(ggplot2::aes(y = AC, color = "Actual Cost (AC)"), size = 3) +
  ggplot2::geom_point(ggplot2::aes(y = EV, color = "Earned Value (EV)"), size = 3) +
  ggplot2::geom_hline(yintercept = bac, linetype = "dashed", color = "black", linewidth = 0.8) +
  ggplot2::geom_hline(yintercept = eac_typical, linetype = "dotted", color = "darkred", linewidth = 0.8) +
  ggplot2::annotate("text", x = 2.8, y = bac + 12000, label = "BAC = $500K", size = 3.5) +
  ggplot2::annotate("text", x = 2.8, y = eac_typical + 12000, label = "EAC = $725K", size = 3.5) +
  ggplot2::scale_color_manual(values = c(
    "Planned Value (PV)" = "steelblue",
    "Actual Cost (AC)"   = "tomato",
    "Earned Value (EV)"  = "forestgreen"
  )) +
  ggplot2::scale_y_continuous(labels = scales::label_dollar(scale = 1e-3, suffix = "K")) +
  ggplot2::labs(
    title  = "Earned Value Management - Performance Trend",
    x      = "Time Period",
    y      = "Value",
    color  = NULL
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")

print(p)

