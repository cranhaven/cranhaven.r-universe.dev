#' Access the Eschmeyer's Catalog of Fishes Glossary
#' 
#' @description This function is used to access the Eschmeyer's Catalog of Fishes Glossary.
#' 
#' @details This function displays the Glossary tab of Eschmeyer's Catalog of Fishes. The glossary contains definitions for various terms that are important to the discussion of taxonomy and are mentioned throughout the database.
#' @return A dataframe with three columns. Some terms have sub-terms (e.g. synonym will have definitions for junior, objective, and subjective in addition to defining synonym itself). The first column represents the name of the main term, the second column represents the name of sub-terms, and the final column provides the definition for these terms. 
#' @examples
#' # Obtain the Eschmeyer's Catalog of Fishes Glossary
#' myGlossary <- rcatfish_glossary()
#' @author Samuel R. Borstein
#' @references 
#' van der Laan, R., Fricke, R. & Fong, J. (Year Accessed). Eschmeyer's Catalog of Fishes: Glossary. https://www.calacademy.org/scientists/catalog-of-fishes-glossary/.
#' @export

rcatfish_glossary <- function(){
  url <- "https://www.calacademy.org/scientists/catalog-of-fishes-glossary/"
  page.html <- xml2::read_html(url) # Read in html from lin
  PageFields <- as.character(rvest::html_elements(page.html, '.field-items')[3])
  PageFieldsCleaned <- gsub("<div class=\"field-items\"><div class=\"field-item even\">\n<p>", "",PageFields)
  PageFieldsCleaned2 <- gsub("\n</ul>\n</div></div>", "",PageFieldsCleaned)
  Definitions <- str_split(PageFieldsCleaned2, "\n<p>")[[1]]
  Definitions <- str_replace(Definitions, "&amp;", "&")
  GlossaryData <- as.data.frame(matrix(nrow = 0, ncol = 3))
  colnames(GlossaryData) <- c("Term","SubTerm","Definition")
  for(i in 1:length(Definitions)){
    CurrentTerm <- qdapRegex::ex_between(left = "^<strong>",right = "</strong>",text.var = Definitions[i], fixed = F)
    if(length(grep(pattern = "li>\n<", Definitions[i])) > 0){
      SubTerms <- str_split(Definitions[i],pattern = "<li>\n<")
      SubTermsCleaned <- SubTerms[[1]][-1]
      CurrentData <- as.data.frame(matrix(nrow = length(unlist(SubTerms)), ncol = 3))
      colnames(CurrentData) <- c("Term","SubTerm","Definition")
      CurrentData$Term <- as.vector(CurrentTerm)
      SubTermName <- qdapRegex::ex_between(text.var = SubTermsCleaned, left = "strong>", right = "</strong>.")
      CurrentData$SubTerm[2:length(unlist(SubTerms))] <- as.vector(SubTermName)
      CurrentData$Definition[1] <- qdapRegex::ex_between(text.var = SubTerms[[1]][1], left = "</strong>. ", right = "</p>")[[1]][1]
      CurrentData$Definition[2:length(unlist(SubTerms))] <- unlist(qdapRegex::ex_between(text.var = SubTermsCleaned, left = "</strong>. ", right = "</li>"))
      GlossaryData <- rbind(GlossaryData, CurrentData)
    }else{
      CurrentData <- as.data.frame(matrix(nrow = 1, ncol = 3))
      colnames(CurrentData) <- c("Term","SubTerm","Definition")
      CurrentData$Term <- CurrentTerm
      CurrentData$Definition <- qdapRegex::ex_between(text.var = Definitions[i], left = "</strong>. |</strong> ", right = "</p>")[[1]][1]
      CurrentData$Definition <- gsub("^. ","", qdapRegex::ex_between(text.var = Definitions[i], left = "</strong>", right = "</p>")[[1]][1])
      GlossaryData <- rbind(GlossaryData, CurrentData)
    }
  }
  return(GlossaryData)
}
