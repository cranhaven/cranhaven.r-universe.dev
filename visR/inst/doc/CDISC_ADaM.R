## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----imports, echo=TRUE, warning=FALSE, message=FALSE-------------------------
library(ggplot2)
library(visR)

## ----globalSetup--------------------------------------------------------------
# Metadata Title
DATASET <- paste0("Analyis Data Time-To-Event (ADTTE)")

# Save original options()
old <- options()  

# Global formatting options
options(digits = 3)

# Global ggplot settings
theme_set(theme_bw())

# Global table settings 
options(DT.options = list(pageLength = 10, 
                          language = list(search = 'Filter:'), 
                          scrollX = TRUE))

# load ADTTE from CDISC pilot 
data(adtte)

# Restore original options()
options(old)


## ----table1_get_default-------------------------------------------------------
# Display a summary table (e.g. tableone)
visR::tableone(adtte[,c("TRTP", "AGE")],
         title = "Demographic summary" , datasource = DATASET)

## ----km_est-------------------------------------------------------------------
# Estimate a survival object
survfit_object <-  adtte %>%
  visR::estimate_KM(data = ., strata = "TRTP")
survfit_object

## ----km_tab_options_1---------------------------------------------------------
# Display test statistics associated with the survival estimate

visR::render(survfit_object %>% get_pvalue(), title = "P-values", datasource = DATASET)


## ----km_plot_1, fig.align='center', fig.width= 6, fig.height=4----------------
# Create and display a Kaplan-Meier from the survival object and add a risktable
visr(survfit_object) %>% 
  visR::add_CI() %>%
  visR::add_risktable()

