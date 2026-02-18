#' Access the Eschmeyer's Catalog of Fishes Classification
#' 
#' @description This function is used to access the Eschmeyer's Catalog of Fishes classification of fishes
#' 
#' @details This function displays the classification tab of Eschmeyer's Catalog of Fishes. The data are returned in a tabular framework that progresses from most to least inclusive from left to right. The function will return the class, order, suborder, family, subfamily, author, and common name of the family or subfamily.
#' @return A dataframe with seven columns. From left to right (and most inclusive to least inclusive), the columns represent class, order, suborder, family, subfamily, author, and the common name of the family or subfamily.
#' @examples
#' #Obtain the Eschmeyer's Catalog of Fishes Classification
#' \donttest{
#' myClassification <- rcatfish_classification()
#' }
#' @author Samuel R. Borstein
#' @references 
#' van der Laan, R., Fricke, R. & Eschmeyer, W.N. (Year Accessed). Eschmeyer's Catalog of Fishes: Classification. https://www.calacademy.org/scientists/catalog-of-fishes-classification/.
#' @export

rcatfish_classification <- function() {
  url <- "https://www.calacademy.org/scientists/catalog-of-fishes-classification/"
  pagedat <- rvest::read_html(url,encoding = "utf-8") # Read html from page, create xml document object
  lists <- rvest::html_elements(pagedat, 'ul') # Obtain all lists, creates nodesets
  text <- rvest::html_text(lists) # Convert lists to text
  
  index <- which(grepl("^Class", text))
  
  Classes <- c()
  for (i in seq_along(index)) {
    start <- index[i]
    end <- index[i+1]-1
    if (is.na(end)) {end <- length(text)}
    output <- paste(text[start:end], collapse = "\\\n")
    Classes <- c(Classes, output)
  }
  
  splitClasses <- unique(unlist(str_split(Classes, "\\\n")))
  taxonomicIndices <- which(grepl("^Class |^Order |^Suborder |^Family |^Subfamily ", splitClasses))
  entries <- splitClasses[taxonomicIndices]
  
  results <- data.frame(matrix(nrow = 0, ncol = 7)) #Create dataframe to store data
  colnames(results) <- c("Class", "Order", "Suborder", "Family", "Subfamily", "Author","CommonName")
  
  class <- order <- subord <- family <- subfam <- author <- comname <- NA
  
  for (i in seq_along(entries)) {
    current.results <- data.frame(matrix(nrow = 1, ncol = 7))
    colnames(current.results) <- c("Class", "Order", "Suborder", "Family", "Subfamily", "Author","CommonName")
    
    if (grepl(pattern = "^Class", entries[i])) {
      class <- substr(entries[i], 7, nchar(entries[i]))
      order <- suborder <- family <- subfamily <- author <- commonname <- NA
    } else if (grepl(pattern = "^Order", entries[i])) {
      order <- substr(entries[i], 7, nchar(entries[i]))
      suborder <- family <- subfamily <- author <- commonname <- NA
    } else if (grepl(pattern = "^Suborder", entries[i])){
      suborder <- substr(entries[i], 10, nchar(entries[i]))
      family <- subfamily <- author <- commonname <- NA
    } else if (grepl(pattern = "^Family", entries[i])) {
      family <- substr(entries[i], 8, unlist(gregexpr("idae", entries[i]))+4)
      subfamily <- NA
      author <- unlist(qdapRegex::ex_between(left = "idae ", right = "(", text.var = entries[i]))
      commonname <- gsub("\\(","", x = str_extract(string = entries[i], pattern = "(?=\\([:alpha:]).+?(?=\\))"))
    } else if (grepl(pattern = "^Subfamily", entries[i])) {
      subfamily <- substr(entries[i], 11, unlist(gregexpr("inae", entries[i]))+4)
      author <- unlist(qdapRegex::ex_between(left = "inae ", right = "(", text.var = entries[i]))
      commonname <- gsub("\\(","", x = str_extract(string = entries[i], pattern = "(?=\\([:alpha:]).+?(?=\\))"))
    }
    
    current.results$Class <- trimws(class)
    current.results$Order <- trimws(order)
    current.results$Suborder <- trimws(suborder)
    current.results$Family <- trimws(family)
    current.results$Subfamily <- trimws(subfamily)
    current.results$Author <- trimws(author)
    current.results$CommonName <- trimws(commonname)
    
    results <- rbind.data.frame(results, current.results)
  }
  
  return(results)
}
