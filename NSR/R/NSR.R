#'Check the native status for plant species in a political region
#'
#'NSR returns information on native status for species within a political region.
#' @param occurrence_dataframe A properly formatted dataframe, see https://bien.nceas.ucsb.edu/bien/tools/nsr/batch-mode/
#' @param ... Additional arguments passed to internal functions.
#' @return Dataframe containing NSR results.
#' @export
#' @examples \dontrun{
#'
#' results <- NSR(occurrence_dataframe = nsr_testfile)
#'   
#' # Inspect the results
#' head(results, 10)
#' # That's a lot of columns. Let's display one row vertically
#' # to get a better understanding of the output fields
#' results.t <- t(results[,2:ncol(results)]) 
#' results.t[,1,drop =FALSE]
#' # Summarize the main results
#' results[ 1:10, 
#' c("species", "country", "state_province", "native_status", "native_status_reason")]
#' 
#' # Compare summary flag isIntroduced to more detailed native_status values
#' # and inspect souces consulted
#' results[ 1:10, 
#' c("species", "country", "state_province", "native_status", "isIntroduced", "native_status_sources")]
#' 
#' 
#' 
#' 
#' }
NSR <- function(occurrence_dataframe,
                ...){
  
  #check that input is a data.frame
  if(!inherits(occurrence_dataframe,"data.frame")){
    stop("occurrence_dataframe should be a data.frame")
  }
  
  #Check that user_id is populated properly, and populate if not
  
  if(all(is.na(occurrence_dataframe$user_id))){
    
    occurrence_dataframe$user_id <- 
      1:nrow(occurrence_dataframe)
    
    # occurrence_dataframe <- 
    #   occurrence_dataframe[c("user_id",
    #                          "taxon",
    #                          "country",
    #                          "state_province",
    #                          "county_parish")]
    
  }
  
  
  if(any(duplicated(occurrence_dataframe$user_id))) {
    stop("user_id should be either null or populated by unique values")
  }
  

  # Convert the data to JSON
  data_json <- toJSON(unname(occurrence_dataframe))
  
  results <- nsr_core(mode = "resolve",
                       data_json = data_json, ...)
  
  # results <- nsr_core(mode = "resolve",
  #                     data_json = data_json)

  
  #If the API didn't work, return a null
  if(is.null(results)){
    return(invisible(NULL))
  }
  
  #Reformat results
  rownames(results) <- results[,1]# Get header names from first row
  results <- t(results)# Transpose
  results <- as.data.frame(results)	# Convert to dataframe (again)
  results <- results[-1, ] # Remove the first row.
  rownames(results) <- NULL	# Reset row numbers
  
  #If the results are properly formatted, but don't have the right columns, return a null
  if(!"country" %in% colnames(results)){
    message("There appears to be a problem with the API, improperly formatted data were returned.")
    return(invisible(NULL))
  }
  
  #Re-order results to match original data
  results <- results[match(table = results$user_id,
                           x = occurrence_dataframe$user_id),]
  
  #reset the row numbers
  rownames(results) <- NULL 
  
  return(results)
  
}






