## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval = TRUE, warning = FALSE, message = FALSE, echo=FALSE----------------
library(statcanR)
library(dplyr)
library(DT)
library(curl)
library(dplyr)
library(ggplot2)
library(reshape2)


url <- paste0("https://warin.ca/datalake/statcanR/statcan_data.qs")

if (httr::http_error(url)) 
{ # network is down = message (not an error anymore)
  message("No tables with this combination of keywords")
} else{ 
  path <- file.path(tempdir(), "temp.qs")
  curl::curl_download(url, path)
  qs_file <- file.path(paste0(tempdir(), "/temp.qs"))
  statcandata <- qs::qread(qs_file)
  
}
# Temporary display of code for command (because not yet part of package)

statcan_search <- function(keywords,lang) {
  
  # Loading data
  if (lang == "eng")
  {
    
      # Creating the keyword matches
      keyword_regex <- paste0("(", paste(keywords, collapse = "|"), ")", collapse = ".*")
      
      matches <- apply(statcandata, 1, function(row) {
        all(sapply(keywords, function(x) {
          grepl(x, paste(as.character(row), collapse = " "))
        }))
      })
      
      # Keep only obs with matched keywords and create datatable 
      filtered_data <- statcandata[matches, ]
      return(datatable(filtered_data,options = list(pageLength = 5)))  

}
  
  if (lang == "fra") {
      
      # Creating the keyword matches
      keyword_regex <- paste0("(", paste(keywords, collapse = "|"), ")", collapse = ".*")
      
      matches <- apply(statcandata, 1, function(row) {
        all(sapply(keywords, function(x) {
          grepl(x, paste(as.character(row), collapse = " "))
        }))
      })
      
      
      # Keep only obs with matched keywords and create datatable 
      filtered_data <- statcandata[matches, ]
      return(datatable(filtered_data,options = list(pageLength = 5)))
  }
} 

## -----------------------------------------------------------------------------
library(statcanR)
library(dplyr)
library(DT)
library(curl)
library(dplyr)
library(ggplot2)
library(reshape2)


# Enter keywords and preferred language ("eng" for Enlgish or "fra" for French)
statcan_search(c("expenditure","research and development","province"),"eng")

## ----eval = TRUE, warning = FALSE, message = FALSE, echo=TRUE-----------------
# Enter tableNumber and preferred language ("eng" for Enlgish or "fra" for French)
rd <- statcan_download_data(tableNumber = "27-10-0360-01",lang="eng")  %>%
  
  # Remove unneeded variables
  subset(.,REF_DATE == "2018-01-01") %>%
  select(-c(REF_DATE,DGUID,UOM_ID,SCALAR_FACTOR,SCALAR_ID,VECTOR,STATUS,COORDINATE,SYMBOL,TERMINATED,DECIMALS,UOM,INDICATOR)) %>%
  rename(percent_total = VALUE)
datatable(rd)

## ---- fig.height=5, fig.width=12.5, fig.align = 'center',eval = TRUE, warning = FALSE, message = FALSE, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Filter for COVID barrier
rd_cangov <-rd %>% 
  subset(.,`Performing sector` == "Government, total") %>%
  subset(., GEO != "Canada" & GEO != "France" & GEO != "Germany" & GEO != "Italy" & GEO != "Japan" & GEO != "United Kingdom" & GEO != "United States") 
  

options(width = 300)
# Plot data
ggplot(rd_cangov,aes(x = GEO,y = percent_total)) + 
    geom_bar(fill = "darkred",stat = "identity",position = "dodge", width = 0.5) +
    theme(axis.text = element_text(size = 7)) +
    ylab("Percent of Canadian Gov Investment") +
    ggtitle("Percent of Canadian Government Investment in R&D, by Province") 
    

## ---- fig.align = "center", fig.height=5, fig.width=13, eval = TRUE, warning = FALSE, message = FALSE, echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Filter for COVID barrier
rd_fedprov <-rd %>% 
  subset(.,`Performing sector` == "Federal government" | `Performing sector` == "Provincial governments") %>%
  subset(., GEO != "France" & GEO != "Germany" & GEO != "Italy" & GEO != "Japan" & GEO != "United Kingdom" & GEO != "United States") %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
  mutate(perf = `Performing sector`)
  

# Plot data
ggplot(rd_fedprov,aes(x = GEO,y = percent_total, fill = perf)) +
  geom_col(position = "stack")+
    geom_bar(stat = "identity",position = "stack", width = 0.5) +
    theme(axis.text = element_text(size = 7)) +
    ylab("Percent of Canadian Gov Investment") +
    ggtitle("Percent of Canadian Government Investment in R&D, by Province") 
    

## ---- fig.align = "center", fig.height = 5, fig.width = 8, eval = TRUE, warning = FALSE, message = FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rd_countries <-rd %>% 
  subset(., GEO == "Canada" | GEO == "France" |GEO == "Germany" |GEO == "Japan" |GEO == "United Kingdom" |GEO == "United States" |GEO == "Italy") %>%
  subset(.,`Performing sector` == "Government, total") 

# Plot data
ggplot(rd_countries,aes(x = GEO,y = percent_total)) + 
    geom_bar(fill = "darkred",stat = "identity",position = "dodge", width = 0.5) +
    theme(axis.text = element_text(size = 7)) +
    ylab("Percent of R&D Spending by Governments") +
    ggtitle("Percent of R&D Spending by Governments, by G-7 Countries") 


