#This package retrieves information on parts in the Registry of Standard Biological Parts
#Author: Adam Santone, PhD
#' Retrieve information for a part from the Registry of Standard Biological Parts
#'
#' @param id A part name.
#' @return A data frame containing part information.
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom purrr map_df
#' @examples
#' getPart("BBa_R0040")
#' @section Information Guidelines:
#' The provided information follows the guidelines set forth by iGEM Foundation at http://parts.igem.org/Registry_API/Guidelines.
#' @section Content License:
#' All Registry content falls under Creative Commons Attribution-ShareAlike: https://creativecommons.org/licenses/by-sa/4.0/.
#' @export
getPart<-
  function(id)
  {

    #load api access link
    head<-"http://parts.igem.org/cgi/xml/part.cgi?part="
    
    #assign part query to id
    tail<-id
    
    #assemble head and tail text into a full query url
    query<-paste(head,tail,sep = "", collapse = NULL)

    #read xml from link
    xml <- read_xml(query)

    #parse xml components
    id<-xml_find_all( xml, ".//part/part_id" ) %>% xml_text()
    name<-xml_find_all( xml, ".//part/part_name" ) %>% xml_text()
    shortName<- xml_find_all( xml, ".//part/part_short_name" ) %>% xml_text()
    seq<-xml_find_all( xml, ".//part/sequences/seq_data" ) %>% xml_text()
    type<-xml_find_all( xml, ".//part/part_type" ) %>% xml_text()
    results<-xml_find_all( xml, ".//part/part_results" ) %>% xml_text()
    url<- xml_find_all( xml, ".//part/part_url" ) %>% xml_text()
    entered<-xml_find_all( xml, ".//part/part_entered" ) %>% xml_text()
    desc<-xml_find_all( xml, ".//part/part_short_desc" ) %>% xml_text()
    author<-xml_find_all( xml, ".//part/part_author" ) %>% xml_text()

    #remove '\n' from string
    seq<-gsub("\n", "", seq)

    #organize info into a tibble 
    t<-tibble(id=as.numeric(as.character(id)),
              name=as.character(name),
              shortName=as.character(shortName),
              seq=as.character(seq),
              type=as.character(type),
              results=as.character(results),
              url=as.character(url),
              entered=as.Date(entered),
              desc=as.character(desc),
              author=as.character(author)
              )
    
    #return tibble
    return(t)
  }
