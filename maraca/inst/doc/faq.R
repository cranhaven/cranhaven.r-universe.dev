## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, collapse = TRUE)
library(maraca)

## ----fig.width = 7, fig.height = 6--------------------------------------------
library(hce)

Rates_A <- c(1.72, 1.74, 0.58, 1.5, 1)
Rates_P <- c(2.47, 2.24, 2.9, 4, 6)

hce_dat <- simHCE(n = 2500, TTE_A = Rates_A, TTE_P = Rates_P,
                  CM_A = -3, CM_P = -6, CSD_A = 16, CSD_P = 15, fixedfy = 3,
                  seed = 242424)

winRatio <- calcWINS(hce_dat)$WR1

plot <- plot(hce_dat, compute_win_odds = FALSE)
plot <-
  plot  +
  ggplot2::annotate(
    geom = "label",
    x = 0,
    y = Inf,
    label = paste(
      "Win ratio: ", round(winRatio[1,"WR"], 2),
      "\n95% CI: ", round(winRatio[1,"LCL1"], 2), " - ",
      round(winRatio[1,"UCL1"], 2), "\n",
      "p-value: ", format.pval(winRatio[1,"Pvalue1"], digits = 3, eps = 0.001),
      sep = ""
    ),
    hjust = 0, vjust = 1.4, size = 3
  )

plot

## ----fig.width = 7, fig.height = 6--------------------------------------------
Rates_A <- c(10, 15)
Rates_P <- c(12, 15)
dat <- simHCE(n = 2500, TTE_A = Rates_A, TTE_P = Rates_P,
              CM_A = 6, CM_P = 10, CSD_A = 16, CSD_P = 15, fixedfy = 3, seed = 1)

plot(dat, lowerBetter = TRUE, trans = "reverse")

## ----fig.width = 7, fig.height = 6--------------------------------------------
data(hce_scenario_a, package = "maraca")
data <- hce_scenario_a

column_names <- c(
    outcome = "GROUP",
    arm = "TRTP",
    value = "AVAL0"
)
step_outcomes <- c(
  "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
)

last_outcome <- "Continuous outcome"

arm_levels = c(active = "Active", control = "Control")

# We will only include a few patients with outcome III
data2 <- data[data$GROUP == "Outcome II",]
data3 <- data[data$GROUP == "Outcome III",]
data <- rbind(data2[sample(1:nrow(data2),5),],
              data3[sample(1:nrow(data3),5),],
              data[!(data$GROUP %in% c("Outcome II","Outcome III")),])

mar <- maraca(
  data, step_outcomes, last_outcome, arm_levels, column_names, 
  fixed_followup_days = 3*365,
  compute_win_odds = TRUE
)

# Now the x-axis labels are overlapping
plot(mar)

## ----fig.width = 7, fig.height = 6--------------------------------------------
data[data$GROUP == "Outcome II","GROUP"] <- "Outcome II\n"
step_outcomes <- c(
  "Outcome I", "Outcome II\n", "Outcome III", "Outcome IV"
)
mar <- maraca(
  data, step_outcomes, last_outcome, arm_levels, column_names, 
  fixed_followup_days = 3*365,
  compute_win_odds = TRUE
)

plot(mar)

## ----error = TRUE-------------------------------------------------------------
data(hce_scenario_a, package = "maraca")
data <- hce_scenario_a

column_names <- c(
    outcome = "GROUP",
    arm = "TRTP",
    value = "AVAL0"
)
step_outcomes <- c(
  "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
)

last_outcome <- "Continuous outcome"

arm_levels = c(active = "Active", control = "Control")

# Let's pretend no one in the study had outcome II
data <- data[data$GROUP != "Outcome II", ]

# Now we will get an error
mar <- maraca(
  data, step_outcomes, last_outcome, arm_levels, column_names, 
  fixed_followup_days = 3*365,
  compute_win_odds = TRUE
)

## ----fig.width = 7, fig.height = 6--------------------------------------------
step_outcomes <- c(
  "Outcome I", "Outcome III", "Outcome IV"
)

# Now we will get an error
mar <- maraca(
  data, step_outcomes, last_outcome, arm_levels, column_names, 
  fixed_followup_days = 3*365,
  compute_win_odds = TRUE
)

plot(mar) +
  labs(caption = paste("No patient experienced Outcome II",
                       "and it is therefore not included in the graph."))

