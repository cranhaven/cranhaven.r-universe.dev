#' finalModelObject
#'
#' Trained MOCHA models - LOESS and linear regression
#'
#' @format A list of lists containing 2 items: "Loess" and "Linear" each with "Total" "Max" and "Intercept"
#' \describe{
#' \item{Loess}{LOESS model}
#' \item{Linear}{Linear model}
#' }
"finalModelObject"

#' youden_threshold
#'
#' Trained regression model for predicting a cutoff threshold
#' for peak calling.
#' Call:
#' loess(formula = OptimalCutpoint ~ Ncells, data = thresh_df)
#'
#' Number of Observations: 27
#' Equivalent Number of Parameters: 5.98
#' Residual Standard Error: 0.02121
#'
#' @format A list of 18 regression variables
#'
"youden_threshold"

#' exampleFragments
#'
#' Example input of ATAC fragments extracted from the PBMC_Small dataset
#' consisting of 2k cells and spanning chr1 and 2 (~2-300MB).
#' This subset consists of two cell populations: Clusters C2 and C5.
#' The data is publicly available with the ArchR package at
#' <https://www.archrproject.com/reference/getTestProject.html>
#'
#' @format A list of 2 GRanges objects
#'
"exampleFragments"

#' exampleCellColData
#'
#' Example input of cellColData extracted from the PBMC_Small dataset
#' consisting of 2k cells and spanning chr1 and 2 (~2-300MB).
#' The data is publicly available with the ArchR package at
#' <https://www.archrproject.com/reference/getTestProject.html>
#' @format A DataFrame with 2217 rows and 3 columns
#'
"exampleCellColData"

#' exampleBlackList
#'
#' Example input of a blackList extracted from the PBMC_Small dataset
#' consisting of 2k cells and spanning chr1 and 2 (~2-300MB).
#' The data is publicly available with the ArchR package at
#' <https://www.archrproject.com/reference/getTestProject.html>
#' @format A GRanges object with 210 ranges and 2 metadata columns
#'
"exampleBlackList"
