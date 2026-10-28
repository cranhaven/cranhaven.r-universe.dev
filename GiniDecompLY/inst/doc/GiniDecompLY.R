## ----include = FALSE--------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

options(width = 150)

## ----setup------------------------------------------------------------------------------------------------------------------------------------------
library(GiniDecompLY)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
dplyr::glimpse(sample_income_data)


## ---------------------------------------------------------------------------------------------------------------------------------------------------
# Calculate the gini correlation between the salary and total income distributions

Salary_distribution = sample_income_data$wage
Total_income_distribution = rowSums(sample_income_data[3:6])

gini_corr(Salary_distribution, Total_income_distribution)


## ----include=FALSE----------------------------------------------------------------------------------------------------------------------------------
GINI = round(dineq::gini.wtd(Total_income_distribution, sample_income_data$sample_wgt),3)

## ----eval=FALSE, include=TRUE-----------------------------------------------------------------------------------------------------------------------
#  
#  sample_income_data |>
#    gini_decomp_source(wage, self_employment_rev, farming_rev, other_rev)
#  

## ---------------------------------------------------------------------------------------------------------------------------------------------------

gini_decomp_source(sample_income_data, 3:6, .by = region, .wgt = sample_wgt)


## ---------------------------------------------------------------------------------------------------------------------------------------------------
sample_income_data |> 
  gini_income_elasticity(wage, self_employment_rev, farming_rev, other_rev,
                         .by = region,
                         .wgt = sample_wgt)



## ---------------------------------------------------------------------------------------------------------------------------------------------------
sample_income_data |> 
   social_welfare_impact(wage, self_employment_rev, farming_rev, other_rev,
   .wgt = sample_wgt)


