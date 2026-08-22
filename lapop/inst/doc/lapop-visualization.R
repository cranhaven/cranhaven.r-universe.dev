## ----fig setup, include=FALSE-------------------------------------------------

knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
knitr::opts_chunk$set(fig.width = 9, fig.height = 5)


## ----lapop package------------------------------------------------------------
library(lapop)

## ----load data----------------------------------------------------------------
# Single-country Single-year AmericasBarometer 2023 Brazil
data(bra23)

# AmericasBarometer 2006-2023 Brazil Merge
data(cm23)

# AmericasBarometer 2023 Year Merge
data(ym23)

## ----weight datasets, eval = TRUE---------------------------------------------
# Single-Country Single-Year
bra23w <- lpr_data(bra23, wt = TRUE)

print(bra23w)

# Single-Country Multi-Year
cm23w <- lpr_data(cm23)

print(cm23w)

# Multi-Country Single-Year
ym23w <- lpr_data(ym23)

print(ym23w)

## ----lapop fonts, eval=F------------------------------------------------------
# lapop::lapop_fonts() # Re-run font registration explicitly if needed

## ----time series, eval = TRUE, fig.alt = "Support for democracy decline a decade ago and remains comparatively low in Brazil"----
fig1.1_data <- lpr_ts(cm23w, outcome = "ing4", rec = c(5, 7), use_wave = TRUE)

lapop_ts(fig1.1_data,
         ymin = 50,
         ymax = 80,
         main_title = "Support for democracy decline a decade ago and remains comparatively low in Brazil",
         subtitle = "% who support democracy")

## ----cross country, eval = TRUE, fig.alt = "In many countries, only about one in two adults support democracy"----
fig1.2_data <- lpr_cc(ym23w, outcome = "ing4", rec = c(5, 7))

lapop_cc(fig1.2_data,
         main_title = "In many countries, only about one in two adults support democracy",
         subtitle = "% who support democracy")

## ----mline, eval = TRUE, warnings=F, fig.alt = "Trust in executives has declined to a level similar to other political institutions"----
fig2.1_data <- lpr_mline(cm23w,
                         outcome = c("b13", "b21", "b31"),
                         rec = c(5, 7), rec2 = c(5, 7), rec3 = c(5, 7))

# Changing legend for readibility
fig2.1_data$varlabel <- ifelse(fig2.1_data$varlabel == "b13", "National Legislature", 
                          ifelse(fig2.1_data$varlabel=="b21", "Political Parties",
                                 "Supreme Court"))

lapop_mline(fig2.1_data,
            main_title = "Trust in executives has declined to a level similar to other \npolitical institutions",
            subtitle = "% who trust...")

## ----ccm, warnings = FALSE, eval = TRUE, fig.alt = "The public typically reports more trust in the armed forces than the police, but levels vary"----

fig2.3_data <- lpr_ccm(ym23w,
                       outcome_vars = c("b12", "b18"),
                       rec = c(5, 7),
                       rec2 = c(5, 7))

# Changing legend for readibility
fig2.3_data$var <- ifelse(fig2.3_data$var == "b12", "Armed Forces", "National Police")

lapop_ccm(fig2.3_data,
          main_title = "The public typically reports more trust in the armed forces than the police,\nbut levels vary",
          subtitle = "% who trust...")


## ----mover, eval = TRUE, fig.alt = "Satisfaction with democracy is significantly lower among women, those with higher educational attainment, and the middle class"----
library(dplyr)

ym23w$variables <- ym23w$variables %>%
    mutate(
    edrer = as.numeric(edre),
    gender = as.numeric(q1tc_r)
  ) %>%
  mutate(edrer = case_when(
    edrer <= 2 ~ "None/Primary",
    edrer %in% c(3, 4) ~ "Secondary",
    edrer > 4 ~ "Superior",
    TRUE ~ NA_character_
  ), 
  edrer = factor(edrer, levels = c("None/Primary", "Secondary", "Superior")),
  gender = ifelse(q1tc_r == 3, NA, q1tc_r)) %>%
  mutate(gender = case_when(
    gender == 1 ~ "Men",
    gender == 2 ~ "Women",
     TRUE ~ NA_character_
  ))

# Changing legend for readibility 
attributes(ym23w$variables$gender)$label <- "Gender"
attributes(ym23w$variables$edrer)$label <- "Education"
attributes(ym23w$variables$wealth)$label <- "Wealth"

fig_mover <- lpr_mover(ym23w,
                       outcome = "pn4",
                       grouping_vars = c("gender", "edrer", "wealth"),
                       rec = c(1, 2)) 

lapop_mover(fig_mover,
            main_title = "Satisfaction with democracy is significantly lower among women,\nthose with higher educational attainment, and the middle class",
            subtitle = "% who are satisfied with democracy")

## ----histogram, eval = TRUE, fig.alt = "On average in the LAC region, one in three say voting is the best way to influence change"----
spot32_data <- lpr_hist(ym23w, outcome = "vb21n")

# Translating to English
spot32_data$cat <- c("Vote", "Run for\noffice", 
                     "Protest", 
                     "Participate\nin local orgs.", 
                     "Other", 
                     "Change is\nimpossible")

lapop_hist(spot32_data,
           main_title = "On average in the LAC region, one in three say voting is the best way \nto influence change",
           subtitle = "How do you believe you can best influence change?")

## ----dumbell, eval = TRUE, fig.alt = "Among those with emigration intentions, the percentage who say they are very likely to emigrate increased in Nicaragua and Guatemala"----
fig3.5_data <- lpr_dumb(ym23w,
                        outcome = "q14f",
                        rec = c(1, 1),
                        over = c(2018, 2023),
                        xvar = "pais",
                        ttest = TRUE)

# Select Countries
fig3.5_data <- fig3.5_data[c(1:3,5:6), ]

lapop_dumb(fig3.5_data,
           main_title = "Among those with emigration intentions, the percentage who say they are \nvery likely to emigrate increased in Nicaragua and Guatemala",
           subtitle = "% who say it is very likely they will emigrate",
           source = "LAPOP Lab, AmericasBarometer 2018/19 and 2023")

## ----stack, eval= TRUE, fig.alt = "Nicaragua has the highest percentage of individuals with migration intentions, while Haiti has the lowest"----
attributes(ym23w$variables$q14f)$label <- "Migration intentions"

fig3.8_data <- lpr_stack(ym23w,
                         outcome = "q14f",
                         xvar = "pais",
                         order = "hi-lo")

lapop_stack(fig3.8_data,
            xvar = "xvar_label",
            source = "LAPOP Lab, AmericasBarometer 2023",
            main_title = "Nicaragua has the highest percentage of individuals with migration intentions,\nwhile Haiti has the lowest")

## ----reverse, eval = TRUE, warnings=F-----------------------------------------
# For data.frames
cm23$ing4r <- lpr_resc(cm23$ing4, reverse = TRUE, map=TRUE)

# For LPR_DATA() objects
cm23w$variables$ing4r <- lpr_resc(cm23w$variables$ing4, reverse = TRUE, map=TRUE)

