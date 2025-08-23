#' Merging the three financial datasets
#'
#' Function to merge the three financial datasets from the three different platforms to obtain a complete dataset to have a comprhensive overview of the investements.
#'
#' @param data_RENDIS  Dataset of class 'data.frame'. Specify the dataset obtained from the ReNDiS database by the \code{get_data_RENDIS} function.
#' @param data_OBDAP Dataset of class 'data.frame'. Specify the dataset obtained from the OpenBDAP database by the \code{get_data_OBDAP} function.
#' @param data_OC Dataset of class 'data.frame'. Specify the dataset obtained from the OpenCoesione database by the \code{get_data_region_OC} or \code{get_data_theme_OC} function.
#'
#'
#' @returns Object of class \code{data.frame} showing 28 variables:
#'
#' Descriptive Variables:
#' \itemize{
#'\item{CUP (\code{character})}
#' \item{Intervention (\code{character})}
#'  \item{Source (\code{character})}
#'  }
#'
#'  Financial Variables:
#'  \itemize{
#'  \item{State Funding (\code{numeric})}
#'  \item{EU Funding (\code{numeric})}
#'  \item{Local Authorities Funding (\code{numeric})}
#'  \item{Private Funding (\code{numeric})}
#'  \item{Other Funding (\code{numeric})}
#'  \item{Finance (\code{numeric})}
#'  }
#'
#'  Geographical References:
#'  \itemize{
#'  \item{DEN_REGION (\code{character})}
#'  \item{DEN_PROVINCE (\code{character})}
#'  \item{DEN_MUNICIPALITY (\code{character})}
#'  \item{COD_REGION (\code{character})}
#'  \item{COD_PROVINCE (\code{character})}
#'  \item{COD_MUNICIPALITY (\code{character})}
#'  \item{geom (\code{character})}
#'  }
#'
#'  Legislative process main steps:
#'  \itemize{
#'  \item{Feasibility Study Starting Date (\code{character})}
#'  \item{Feasibility Study Ending Date (\code{character})}
#'  \item{Preliminary Design Starting Date (\code{character})}
#'  \item{Preliminary Design Ending Date (\code{character})}
#'  \item{Definitive Design Starting Date (\code{character})}
#'  \item{Definitive Design Ending Date (\code{character})}
#'  \item{Executive Design Starting Date (\code{character})}
#'  \item{Executive Design Ending Date (\code{character})}
#'  \item{Works Execution Starting Date (\code{character})}
#'  \item{Works Execution Ending Date  (\code{character})}
#'  \item{Conclusion Starting Date (\code{character})}
#'  \item{Conclusion Ending Date (\code{character})}
#'  }
#'
#'
#' @author Lorena Ricciotti
#'
#' @examples
#' data(OCpoint)
#' data(OBDAPpoint)
#' data(RENDISpoint)
#' data_all <- merge_data(RENDISpoint, OBDAPpoint, OCpoint)
#'
#' @export



merge_data <- function(data_RENDIS, data_OBDAP, data_OC) {
  # Rename columns and add the column 'Source'
  if(length(data_RENDIS) == 26){
    data_RENDIS <- data_RENDIS %>% dplyr::mutate("Operability" = NA) %>% dplyr::select(1:25, 27, dplyr::everything() )
  }
  data_RENDIS <- data_RENDIS %>%
    dplyr::mutate(Source = "Rendis") %>%
    dplyr::select(-.data$Type)

  data_OBDAP <- data_OBDAP %>%
    dplyr::mutate(Finance = rowSums(data_OBDAP[, 11:15], na.rm = T)) %>%
    dplyr::mutate(Source = "OpenBDAP", 'FeasibilityStudyStartingDate' = NA,
                  'FeasibilityStudyEndingDate' = NA, 'PreliminaryDesignStartingDate' = NA,
                  'PreliminaryDesignEndingDate' = NA, 'DefinitiveDesignStartingDate' = NA,
                  "DefinitiveDesignEndingDate" = NA, 'ExecutiveDesignStartingDate' = NA,
                  "ExecutiveDesignEndingDate" = NA,'InterventionClosed' =NA)


  data_OC <- data_OC %>% dplyr::mutate('StateFunding' = rowSums(data_OC[,16:20], na.rm = T),
                                       'LocalAuthoritiesFunding' = rowSums(data_OC[,21:23], na.rm=T),
                                       'OtherFunding' = rowSums(data_OC[,24:26], na.rm =T))
  data_OC <- data_OC %>%
    dplyr::rename(Finance = "TotalPublicFunding") %>%
    dplyr::mutate(Source = "OpenCoesione") %>%
    dplyr::mutate('InterventionClosed' = as.character(NA),"Operability" = as.character(NA))

  # Select columns
  data_OC <- data_OC %>% dplyr::select(colnames(data_RENDIS), colnames(data_OBDAP[11:15]))
  #data_OC <- data_OC %>% dplyr::mutate(dplyr::across(c(8:21), as.character))
  data_OBDAP <- data_OBDAP %>% dplyr::select(colnames(data_RENDIS), colnames(data_OBDAP[11:15]))
  #data_OC$Finance <- as.numeric(data_OC$Finance)
  data_OBDAP$COD_REGION <- as.character(data_OBDAP$COD_REGION)

  # Merge
  merged_data <- dplyr::bind_rows(data_OC, data_OBDAP, data_RENDIS)

  # Eliminate duplicated CUP
  merged_data_unique <- merged_data %>%  dplyr::distinct(dplyr::across(c(.data$CUP,.data$Finance)), .keep_all = TRUE)
  #%>% dplyr::ungroup()
  merged_data_unique <- merged_data_unique %>% dplyr::select(1,2,4:25,28:32,3, dplyr::everything())
  return(merged_data_unique)
}
