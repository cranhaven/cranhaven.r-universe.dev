
#' Access catalog changes and additions
#' 
#' @description This function is used to access the Eschmeyer's Catalog of Fishes Summary of Changes and Additions.
#' 
#' @param changes Logical. Should the function return the table of changes to the catalog? Default is TRUE.
#' @param author.changes Logical. Should the function return the table of author and year changes to the catalog? Default is TRUE.
#' @param spell.changes Logical. Should the function return the table of spelling changes to the catalog? Default is TRUE.
#' @param added.genera Logical. Should the function return genera added to the catalog? Default is TRUE.
#' @param added.species Logical. Should the function return species added to the catalog? Default is TRUE.
#' @details This function displays the information on the "Changes and Additions" tab of Eschmeyer's Catalog of Fishes. The catalog updates this information monthly with changes to the catalog, new genus information, and new species information. Each updates removes the last month's information, so data obtained through this function is only representative of the changes and additions made since the previous update.
#' @return A named list. Names include UpdateDate, Changes, AuthorshipChanges, AddedGenera, and AddedSpecies. UpdateDate will always be returned and is a character string of the date the catalog was updated. Changes, AddedGenera, and AddedSpecies summarize changes to the catalog, changes to authorship information, genera added to the catalog, and species added to the catalog respectively. The elements returned are based on which parameters are set to TRUE. An itemized list describing the contents in the columns of the data returned is described below.
#' \itemize{
#'   \item Original.Taxon - Character. The original taxonomic name in the database.
#'   \item Was - Character. Previous status of the taxon in the database.
#'   \item Previous.Taxon - Character. Previous taxonomic name in the database.
#'   \item Now - Character. Current status of the taxon in the database following the update.
#'   \item Current.Taxon - Character. Current taxonomic name in the database for the taxon following the update.
#'   \item Taxon - Character. Taxon (of any rank) whose citation was updated.
#'   \item Previous.Author - Character. Authorship information of the citation before the update.
#'   \item Current.Author - Character. Authorship information of the citation following the update.
#'   \item Species.Name - Character. Current taxonomic name in the database for the taxon following the update.
#'   \item Genus.Name - Character. Name of the new genus added to the database following the update.
#'   \item Authors - Character. Authorship for new taxonomic names added to the database following the update.
#'   \item Year   - Character. Year for new taxonomic names added to the database following the update.
#'   \item Region - Character. Region new taxa added to the database are known to occur in.
#'   \item Was.Spelled - Character. Previous spelling of the taxon in the database.
#'   \item Now.Spelled - Character. Revised spelling of the taxon in the database.
#'   }
#' @examples
#' # return all available data
#' myList <- rcatfish_updates(changes = TRUE, added.genera = TRUE, added.species = TRUE)
#' @author Brandon E. Dominy, Samuel R. Borstein
#' @references 
#' Fricke, R., van der Laan, R. & Fong, J.D. (Year Accessed). Eschmeyer’s Catalog of Fishes: Changes and Additions. https://researcharchive.calacademy.org/research/ichthyology/catalog/ChangeSummary.asp.
#' @export
#' @importFrom xml2 read_html
#' @importFrom magrittr %>%
#' @import rvest
#' @import stringr

rcatfish_updates <- function(changes = TRUE, author.changes = TRUE, spell.changes = TRUE, added.genera = TRUE, added.species = TRUE) {
  
  # returns an error if parameters are not type 'logical'
  # if(!is.logical(c(changes, author.changes, spell.changes, added.genera, added.species))){
  #   stop("'changes', 'author.changes', 'added.genera', and 'added.species' must be provided as logical values")
  #   
  # }
  if(any(c(is.logical(changes), is.logical(author.changes), is.logical(spell.changes),is.logical(added.genera),is.logical(added.species))==FALSE)){
    stop("'changes', 'author.changes', 'added.genera', and 'added.species' must be provided as logical values")
  }

  ##############################################################################
  
  ########################### Accessing Catalog HTML ###########################
  tryCatch(expr = {url <- "https://researcharchive.calacademy.org/research/ichthyology/catalog/ChangeSummary.asp" # Link to page in catalog of fish
  page.html <- xml2::read_html(url) # Read in html from link
  
  
  ###################### Retrieve Date of Catalog Update #######################        
  page.paras <- rvest::html_elements(page.html, 'p') %>% # Extract paragraphs into vector
    rvest::html_text()  # Get paragraph text for each item in vector
  
  update.index <- grep(pattern = "update", page.paras) # Retrieve index of item containing update information
  
  updatedOn <- page.paras[update.index] %>% # Retrieve only vector item containing 'update' text
    str_extract(string = , pattern = "update of .*\\d{4}") %>% # Separate out all text except 'update of [date]' from vector
    str_replace(string = , pattern = 'update of', replacement = 'Updated on') %>% # Reformat text for user readability
    trimws() # Trim white space from date text, assign to character vector for output
  
  
  ######################## Retrieve Changes and Authorship Updates to Catalog #########################         
  page.tbls <- rvest::html_table(page.html) # Scrape data from site tables
  changesData <- as.data.frame(page.tbls[4]) # Convert table of changes (always fourth table from site) to dataframe for output
  changesAuthor <- as.data.frame(page.tbls[5]) # Convert table of authorship changes (always fifth table from site) to dataframe for output
  changesSpelling <- as.data.frame(page.tbls[6]) # Convert table of authorship changes (always fifth table from site) to dataframe for output
  
  if (length(changesAuthor) == 0) {author.changes <- FALSE} # If there are no authorship changes, do not return author data
  if (length(changesSpelling) == 0) {spell.changes <- FALSE} # If there are no authorship changes, do not return author data
  
  table.html <- rvest::html_elements(page.html, 'table') # Get the raw html of all tables on the page
  updates.html <- table.html[4] # Pull the raw html of specifically the changes table
  changes.html <- as.character(rvest::html_elements(updates.html, ".changes")) # Retrieve individual table cells as raw html
  changes.html <- changes.html[6:length(changes.html)] # Strip out header row
  changes.html <- str_remove_all(changes.html, "<td.*?>|</td>|<a.*?>|</a>|\\\n|<i>") # Remove html tags
  changes.html <- str_replace_all(changes.html, "&amp;", "&") # Fix ampersand issues
  changes.indices <- 1:length(changes.html) # Get vector of total cells indices
  origTax <- which((changes.indices-1) %% 5 == 0) # Get vector of indices of all orginal taxon fields
  was <- which((changes.indices-2) %% 5 == 0) # Get vector of indices of all was fields 
  prevTax <- which((changes.indices-3) %% 5 == 0) # Get vector of indices of all previous taxon fields
  now <- which((changes.indices-4) %% 5 == 0) # Get vector of indices of all now fields
  currTax <- which(changes.indices %% 5 == 0) # Get vector of indices of all current taxon fields
  split.previous <- str_split(changes.html[prevTax], "</i>") # Separate authors of previous taxa
  split.current <- str_split(changes.html[currTax], "</i>") # Separate authors of current taxa
  tbl.changes.split.author <- as.data.frame(matrix(nrow = nrow(changesData), ncol = 7)) # Create table to store data
  colnames(tbl.changes.split.author) <- c("Original.Taxon", "Was", "Previous.Taxon", "Previous.Author", "Now", "Current.Taxon", "Current.Author")
  for (i in 1:nrow(tbl.changes.split.author)) { # Fill in output table
    tbl.changes.split.author[i, 1] <- str_remove(changes.html[origTax[i]], "</i>")
    tbl.changes.split.author[i, 2] <- changes.html[was[i]]
    tbl.changes.split.author[i, 3:4] <- split.previous[[i]]
    tbl.changes.split.author[i, 5] <- changes.html[now[i]]
    tbl.changes.split.author[i, 6:7] <- split.current[[i]]
  }
  
  
  ################## Retrieve Genera/Species Added to Catalog ##################      
  page.links <- rvest::html_elements(page.html, "a") # Scrape all links on site
  page.linkText <- rvest::html_text2(page.links) # Strip links of tags (now only link text)
  link.break.index <- min(which(page.linkText == changesData[nrow(changesData), 1])) # Find which position in link text vector matches last entry in table of changes (all links after this point are added genera/species)
  added.data <- page.linkText[-1:-link.break.index] # Create vector from link text using only positions of added genera and species
  
  
  page.text <- page.html %>% # Create variable page_text, start pipe with html data
    rvest::html_children() %>% # Creates nodeset with two objects: data stored in <head> tags (metadata) and data in <body> tags (visisble content)
    rvest::html_text2() %>% # Convert all data into text, without tags. Creates vector with two objects: head data as text, body data as text
    strsplit( , split = "Added Genera") %>% # Split text at "Added Genera" label. Returns list with 2 items: [1] head data as text, [2] contains two objects: [1] body data before "Added Genera", [2] body data after
    unlist() %>% # Split list into vector length 3: [1] head data, [2] data before "Added Genera", [3] data after "Added Genera"
    strsplit( , split = "Added Species") %>% # Perform same split as above, this time on "Added Species". Returns list of length 3, with the third object split into [1] data before "Added Species", [2] data after
    unlist() # Split list into vector length 4: [1] head data, [2] data before "Added Genera", [3] data between "Added Genera" and "Added Species", [4] data after "Added Species"
  # Ends up with vector page_text of length 4. [1] contains header data, [2] contains change table data, [3] contains Added Genera data, [4] contains Added Species data
  
  gen.data <- page.text[3] # Data for added genera
  spec.data <- page.text[4] # Data for added species
  
  .dataFix <- function(criteria, TaxData) { # Internal helper function for processing genera data and species data
    splitData <- strsplit(TaxData, split = '\\\n') %>% # split input on newline character '\n'
      unlist()
    indices <- unlist(sapply(criteria, grep, splitData, fixed = T))#sapply to grep across all names and find positions
    dataValues <- splitData[indices]
  }
  
  frmt.gen.data <- .dataFix(criteria = added.data[grep(" ",added.data,invert = TRUE, fixed = TRUE)], TaxData = gen.data) # Formatted genera data
  frmt.spec.data <- .dataFix(criteria = added.data[grep(" ",added.data, invert = FALSE, fixed = TRUE)], TaxData = spec.data) # Formatted species data
  
  newGenera <- as.data.frame(matrix(nrow = 0, ncol = 3)) # Create matrix to store scraped genus data
  
  if (length(frmt.gen.data) == 0) { # If there are no added genera, fill matrix with NA values
    name <- author <- year <- NA
    
    newGenera <- cbind.data.frame(name, author, year)
  } else { # If there are added genera, fill matrix with proper data
    frmt.gen.data <- str_replace_all(string = frmt.gen.data, pattern = '\\\r', replacement = '') # Remove carriage return characters at start of genera (this is specific to genus table)
    
    name <- trimws(stringr::str_extract(pattern = '^[A-Z][a-z]* ', string = frmt.gen.data))
    
    year <- str_extract(pattern = '\\d{4}', string = frmt.gen.data)
    
    authorStart <- stringr::str_locate(pattern = name, string = frmt.gen.data)
    authorEnd <- stringr::str_locate(pattern = year, string = frmt.gen.data)
    author <- stringr::str_sub(string = frmt.gen.data, start = authorStart[,2]+1, end = authorEnd[,1]-1) %>%
      trimws()
    
    newGenera <- cbind.data.frame(name, author, as.numeric(year)) 
  }
  
  colnames(newGenera) <- c('Genus.Name', 'Authors', 'Year') # Set matrix column names
  
  
  newSpecies <- as.data.frame(matrix(nrow = 0, ncol = 4)) # Create matrix to store scraped species data
  
  if (length(frmt.spec.data) == 0) { # If there are no added species, fill matrix with NA values
    name <- author <- year <- region <- NA
    
    newSpecies <- cbind.data.frame(name, author, as.numeric(year), region)
  } else { # If there are added species, fill matrix with proper data
    frmt.spec.data <- str_replace_all(string = frmt.spec.data, pattern = '\\\r', replacement = '') # Remove carriage return characters at start of genera (this is specific to genus table)
    
    name <- name <- trimws(stringr::str_extract(pattern = '^[A-Z][a-z]* [a-z]* [a-z]*|^[A-Z][a-z]* \\([A-Z][a-z]*\\)* [a-z]*', string = frmt.spec.data))
    
    year <- stringr::str_extract(pattern = '\\d{4}', string = frmt.spec.data)
    
    authorStart <- stringr::str_locate(pattern = fixed(name), string = frmt.spec.data)
    authorEnd <- stringr::str_locate(pattern = year, string = frmt.spec.data)
    author <- stringr::str_sub(string = frmt.spec.data, start = authorStart[,2]+1, end = authorEnd[,1]-1) %>%
      trimws()
    
    regionStart <- stringr::str_locate(pattern = year, string = frmt.spec.data)
    regionEnd <- stringr::str_length(frmt.spec.data)
    region <- stringr::str_sub(string = frmt.spec.data, start = regionStart[,2]+2, end = regionEnd-1) %>%
      trimws()
    for (i in seq_along(region)) { # Replace missing regions with NA values
      if (region[i] == '') {
        region[i] <- NA
      }
    }
    
    newSpecies <- cbind.data.frame(name, author, as.numeric(year), region)
  }
  colnames(newSpecies) <- c('Species.Name', 'Authors', 'Year', 'Region') # Set matrix column names
  
  
  #############################  Parse Function Output  #############################
  outputList <- list(updatedOn) # Always add date of update to output list
  names(outputList) <- "UpdateDate"
  
  if (changes) { # If changes == TRUE, add catalog changes to output list
    OutputNames <- c(names(outputList),"Changes")
    outputList <- append(outputList, values = list(tbl.changes.split.author))
    names(outputList) <- OutputNames
  }
  if (author.changes) {
    OutputNames <- c(names(outputList),"AuthorshipChanges")
    outputList <- append(outputList, values = list(changesAuthor))
    names(outputList) <- OutputNames
  }
  if (spell.changes) {
    OutputNames <- c(names(outputList),"SpellingChanges")
    outputList <- append(outputList, values = list(changesSpelling))
    names(outputList) <- OutputNames
  }
  if (added.genera) { # If added.genera == TRUE, add new genera to output list
    OutputNames <- c(names(outputList),"AddedGenera")
    outputList <- append(outputList, values = list(newGenera))
    names(outputList) <- OutputNames
  }
  if (added.species) { # If added.genera == TRUE, add new species to output list
    OutputNames <- c(names(outputList),"AddedSpecies")
    outputList <- append(outputList, values = list(newSpecies))
    names(outputList) <- OutputNames
    
  }
  
  return(outputList) # Return output list
  }, error = function(cond) { # Error handling
    if(!curl::has_internet()) { # Error for bad internet connection
      message(paste('Please check your internet connection and try again'))
    } else { # Generic error
      message(paste0("Undefined Error: ", cond))
      
      return(NA)
    }
  }
  )
}
