#' Population/Abundance data of OTUs over time for "list" method
#'
#' This data is a small artificial data which contains information about population/abundance of
#' some artificial OTUs. When data.method in Muller.plot function is "list" then population.data
#' must be a matrix or data frame with 3 columns.
#'
#' Here the first column contains OTU names:
#' "RBL636","rkd D5891 bcc","ylnC-48","iglK f12","BAeR G11","nuhj-25","HwrK-41","QecF*22"
#'
#' The second column contains time steps in which the corresponding OTUs are present in the dynamics.
#'
#' The third column contains populations/abundances of corresponding OTUs at corresponding times.
#'
#' @docType data
#' @keywords datasets
#' @name PopulationDataList
#' @usage data(PopulationDataList)
#' @format A data frame with 3 columns and 382 rows.
"PopulationDataList"

#' Population/Abundance data of OTUs over time for "table" method
#'
#' This data is a small artificial data which contains information about population/abundance of
#' some artificial OTUs. When data.method in Muller.plot function is "table" then population.data
#' must be a matrix or data frame with N rows and T columns, which N is the number of OTUs and T
#' is the number of time steps or generations with provided abundances (or the number of samples). Here
#' N=8 and T=101.
#'
#' "rownames" of this matrix/data frame must be OTU names:
#'
#' "RBL636","rkd D5891 bcc","ylnC-48","iglK f12","BAeR G11","nuhj-25","HwrK-41","QecF*22"
#'
#' and "colnames" must be time steps or generations.
#'
#' The Matrix contains abundance of each OTU at each time step.
#'
#' @docType data
#' @keywords datasets
#' @name PopulationDataTable
#' @usage data(PopulationDataTable)
#' @format A matrix with 101 columns and 8 rows.
"PopulationDataTable"

#' Attributes of OTUs
#'
#' A matrix with 3 columns and 8 rows.
#'
#' The first column contains OTU names:
#'
#' "RBL636","rkd D5891 bcc","ylnC-48","iglK f12","BAeR G11","nuhj-25","HwrK-41","QecF*22"
#'
#' The second column contains parents of the corresponding OTUs:
#'
#' NA ,"RBL636","RBL636","ylnC-48","rkd D5891 bc","iglK f12","rkd D5891 bc","nuhj-25"
#'
#' and the third column contains colors of corresponding OTUs.
#'
#' @docType data
#' @keywords datasets
#' @name Attributes
#' @usage data(Attributes)
#' @format A matrix with 3 columns and 8 rows.
"Attributes"
