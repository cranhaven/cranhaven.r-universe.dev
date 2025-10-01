#' PBT Municipal 2004-2019
#' Join municipal PBT data for several years
#'
#' This function joins municipal total gross product for several years and
#' all federal entities
#'
#' @param state name of federal entitie
#' @return A data.frame of the infile
#'
#' @export
#'
PIBM_04_19 <- function( state=NA){
  if( state == "ags" |
      state == "bc" |
      state == "bcs" |
      state == "camp" |
      state == "coah" |
      state == "col" |
      state == "chis" |
      state == "chih" |
      state == "cdmx" |
      state == "dgo" |
      state == "gto" |
      state == "gro" |
      state == "hgo" |
      state == "jal" |
      state == "mex" |
      state == "mich" |
      state == "mor" |
      state == "nay" |
      state == "nl" |
      state == "pue" |
      state == "qro" |
      state == "qroo" |
      state == "slp" |
      state == "sin" |
      state == "son" |
      state == "tab" |
      state == "tamps" |
      state == "tlax" |
      state == "ver" |
      state == "yuc" |
      state == "zac" |
      state == "oax"){
    data <- economic_census(year = 2004, state = state)
    data04 <- pbt_mun(data)
    data <- economic_census(year = 2009, state = state)
    data09 <- pbt_mun(data)
    data <- economic_census(year = 2014, state = state)
    data14 <- pbt_mun(data)
    data14 <- data14[!is.na(data14[,3]), ]
    data14<- stats::aggregate(x= data14$a111a,
                       # Specify group indicator
                       by = list(data14$municipio),
                       # Specify function (i.e. mean)
                       FUN = sum)
    colnames(data14)<- c("municipio", "pbt14")
    data <- economic_census(year = 2019, state = state)
    data19 <- pbt_mun(data)
    data19 <- data19[!is.na(data19[,3]), ]
    data19<- stats::aggregate(x= data19$a111a,
                       # Specify group indicator
                       by = list(data19$municipio),
                       # Specify function (i.e. mean)
                       FUN = sum)
    colnames(data19)<- c("municipio", "pbt19")
    rm(data)
    data_04_19 <- dplyr::left_join(data04, data09, by = "municipio")
    data_04_19 <- dplyr::left_join(data_04_19, data14, by = "municipio")
    data_04_19 <- dplyr::left_join(data_04_19, data19, by = "municipio")
    rm(data04, data09, data14, data19)
    data_04_19 <- data_04_19[,c(1,2,3,5,6,7)]
    new.names <- c("Entidad","Municipio","pbt04","pbt09","pbt14","pbt19")
    names(data_04_19) <- new.names
    data_04_19
  } else {
    stop(message("\n parameters are incorrect")) }
}


