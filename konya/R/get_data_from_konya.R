#' @title get_data_from_bursa
#' @description  Given a data name, this function returns the data frame
#' @param data_name A data set name obtained from list_data_names()
#' @return A data frame that shows the observations for given data name
#' @examples
#' #NOT RUN
#'\donttest{
#'# It is just an example.
#' get_data_from_konya("bilgehane-merkez-konum-bilgileri")
#'}
#'
#' @export
#' @importFrom utils read.csv
get_data_from_konya<-function(data_name){
  #requireNamespace(dplyr)
  #requireNamespace(rvest)
  link <-rvest::read_html(gsub(" ","",paste("https://acikveri.konya.bel.tr/dataset/",data_name)))
  href_list <-link |> rvest::html_elements("a") |> rvest::html_attr("href")
  json_sum <- sum(grepl("json",  href_list))
  xlsx_sum <- sum(grepl("xlsx",  href_list))
  csv_sum <- sum(grepl("csv",  href_list))
  if(json_sum ==1 & xlsx_sum==1 & csv_sum ==1){
    selector <- "csv"
  } else if (json_sum ==1 & xlsx_sum==0 & csv_sum ==1){
    selector <- "csv"
  } else if (json_sum ==0 & xlsx_sum==0 & csv_sum ==1){
    selector <- "csv"
  } else if (json_sum ==1 & xlsx_sum==1 & csv_sum ==0){
    selector <- "xlsx"
  } else if (json_sum ==0 & xlsx_sum==1 & csv_sum ==0){
    selector <- "xlsx"
  } else if (json_sum ==1 & xlsx_sum==0 & csv_sum ==0){
    selector <- "json"
  } else if (json_sum ==0 & xlsx_sum==1 & csv_sum ==1){
    selector <- "csv"
  }
  if(selector =="json"){
    data <- dplyr::as_tibble(jsonlite::fromJSON(href_list|>stringr::str_subset("json")))
    #data<-encoding_data(data)
  }  else if(selector == "csv"){
    data <- dplyr::as_tibble(utils::read.csv(href_list|>stringr::str_subset("csv"),sep = ";"))
    data<-encoding_data(data)
  }  else if(selector == "xlsx"){
    data <- dplyr::as_tibble(openxlsx::read.xlsx(href_list|>stringr::str_subset(".xlsx")))
    data<-encoding_data(data)
  } else {
    #print(paste("This data cannot be downloaded. Please visit",link))
  }
  return(data)
}
