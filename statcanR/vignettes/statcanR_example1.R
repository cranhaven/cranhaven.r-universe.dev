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

## ----eval = TRUE, warning = FALSE, message = FALSE, echo=TRUE-----------------
library(statcanR)
library(dplyr)
library(DT)
library(curl)
library(dplyr)
library(ggplot2)
library(reshape2)

# Search for data table with keywords 
statcan_search(c("green","business"),"eng")

## ----eval = TRUE, warning = FALSE, message = FALSE, echo=TRUE-----------------
# Enter tableNumber and preferred language ("eng" for Enlgish or "fra" for French)
biz_barriers <- statcan_download_data(tableNumber = "33-10-0548-01",lang="eng")  %>%
  rename(potential_barriers = `Potential barriers of the business or organization when adopting more green practices over the next 12 months`) %>%
  
  # Remove unneeded variables
  select(-c(REF_DATE,DGUID,UOM_ID,SCALAR_FACTOR,SCALAR_ID,VECTOR,COORDINATE,SYMBOL,TERMINATED,DECIMALS))

datatable(biz_barriers, options = list(pageLength = 5))


## ---- fig.height=5, fig.width=13, eval = TRUE, warning = FALSE, message = FALSE, echo=TRUE----

# Filter for COVID barrier
covid_barrier <-biz_barriers %>% 
  subset(.,potential_barriers == "COVID-19") %>%
  
  
# Filter for data status
  subset(.,STATUS == "A" | STATUS == "B" | STATUS == "C" | STATUS == "D") %>%
  
# Filter for province
  subset(.,GEO != "Canada" & GEO != "Nunavut" & GEO != "Northwest Territories" & GEO != "Yukon") %>%
  distinct(GEO,STATUS,`Business characteristics`, .keep_all = T)

# Construction vs fin/insr
ind_comparison <- subset(covid_barrier, `Business characteristics` == "Finance and insurance [52]"| `Business characteristics` == "Construction [23]") %>%
  mutate(biz_type = as.factor(`Business characteristics`)) %>%
  select(biz_type,GEO,VALUE)

# Plot data
ggplot2::ggplot(ind_comparison,aes(x = GEO,y = VALUE),) + 
    geom_bar(aes(fill = biz_type),stat = "identity",position = "dodge", width = 0.5) +
    theme(axis.text = element_text(size = 7)) +
    ylab("Percent of businesses") +
    ggtitle("Percent of businesses facing sustainability barriers due to COVID") 
    

## ---- fig.height=5, fig.width=13, eval = TRUE, warning = FALSE, message = FALSE, echo=TRUE----
biz_size <- biz_barriers %>%
  subset(.,`Business characteristics` == "1 to 4 employees" | `Business characteristics` == "5 to 19 employees" | `Business characteristics` == "20 to 99 employees" | `Business characteristics` == "100 or more employees") %>%
  subset(.,GEO == "Canada") %>%
  mutate(biz_type = as.factor(`Business characteristics`)) %>%
  mutate(biz_type = ordered(biz_type, levels = c("100 or more employees",  "20 to 99 employees", "5 to 19 employees" ,"1 to 4 employees"))) %>%
  select(biz_type, potential_barriers, VALUE)

biz_size$potential_barriers[biz_size$potential_barriers == "Potential barriers of the business or organization when adopting more green practices over the next 12 months, other"] <- "Other"
biz_size$potential_barriers[biz_size$potential_barriers == "Potential barriers of the business or organization when adopting more green practices over the next 12 months, none"] <- "None"


 ggplot(arrange(biz_size, -biz_type),aes(x = potential_barriers,y = VALUE),) + 
    geom_bar(aes(fill = biz_type),stat = "identity",position = "dodge", width = 0.5) +
    theme(axis.text = element_text(size = 8)) +
    ylab("Percent of businesses") +
    xlab("") +
    coord_flip()+
    ggtitle("Percent of businesses facing sustainability barriers due to COVID") 
 

