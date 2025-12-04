#' Metadata Extraction from NCBI/GISAID (EpiFlu/EpiCoV/EpiPox/EpiArbo) FASTA file
#'
#' This function retrieves metadata (ID, region, date) from the input FASTA file, with the source of, either 
#' NCBI (with default FASTA header) or GISAID (with default FASTA header). The function will return a dataframe
#' that has three columns consisting ID, collected region and collected date. Records that do not have region or date
#' information will be excluded from the output dataframe.
#'
#' @param file_path path of fasta file
#' @param source the source of fasta file, either "NCBI" or "GISAID"
#' @return  A dataframe that has three columns consisting ID, collected region and collected date
#' @examples filepath <- system.file('extdata','GISAID_EpiCoV.faa', package = 'vDiveR')
#' @examples meta_gisaid <- metadata_extraction(filepath, 'GISAID')
#' @export
metadata_extraction <- function(file_path, source){
    if (tolower(source) == 'ncbi') {
        meta <- extract_from_NCBI(file_path)
    } else if (tolower(source) == 'gisaid') {
        meta <- extract_from_GISAID(file_path)
    } else {
        stop("Invalid source specified. Please use 'NCBI' or 'GISAID'.")
    }
    return(meta)
}

#' Extract metadata via fasta file from GISAID
#'
#' This function get the metadata from each header of GISAID fasta file
#' @param file_path path of fasta file
#' @importFrom stringr str_extract
extract_from_GISAID <- function(file_path){
    lines <- readLines(file_path, warn = FALSE)
    headers <- lines[grepl('^>', lines)]
    
    #================= Extract metadata from headers =====================#
    #e.g: >Spike|hCoV-19/Wuhan/WIV04/2019|2019-12-30|EPI_ISL_402124|
    # id: EPI_ISL_402124
    # region: Wuhan
    # date: 2019-12-30

    ids <- str_extract(headers, "EPI[^|]*")
    regions <- sapply(strsplit(headers, "/"), function(x) if(length(x) > 1) x[2] else NA)
    dates <- str_extract(headers, "\\d{4}-\\d{2}-\\d{2}")
    
    return(data.frame(ID = ids, region = regions, date = dates, stringsAsFactors = FALSE))
}

#' Extract metadata via fasta file from ncbi
#'
#' This function get the metadata from each head of fasta file
#' @param file_path path of fasta file
#' @importFrom rentrez entrez_search entrez_fetch
extract_from_NCBI <- function(file_path){
    #================= Extract metadata from headers =====================#
    #e.g: >QQL10871.1 Rh230 [Rhesus cytomegalovirus strain 68-1_FL]
    DATE_FORMATS <- c('%Y-%m-%d', '%d-%B-%Y', '%m/%d/%Y', '%B %d, %Y', '%d-%m-%Y')
    
    lines <- readLines(file_path, warn=FALSE) # read lines from fasta file into vector
    # extract id from header
    headers <- lines[grepl('^>', lines)]
    headers <- substring(headers, 2)
    IDs <- sapply(strsplit(headers, ' '), function(x) x[1])
    
    
    regions <- character()
    dates <- character()
    drop_sample_not_found <- character()
    drop_sample_no_date_or_region <- character()
    drop_sample_date_format_issue <- character()
    keep_sample <- character()

    # regions <- c(); dates <- c(); drop_sample <- c(); keep_sample <- c()
    for(ID in IDs){
        if(startsWith(ID, 'pdb')){
            drop_sample_not_found <- c(drop_sample_not_found, ID)
            next
        }

        # keep_sample <- c(keep_sample, ID)
        search_result <- rentrez::entrez_search(db = "protein", term = ID, retmax = 1)
        if(search_result$count == 0){
            drop_sample_not_found <- c(drop_sample_not_found, ID)
            next
        }

        uid <- search_result$ids[1] #get unique identifier
        # fetch genbank entry
        genbank_entry <- rentrez::entrez_fetch(db = "protein", id = uid, rettype = "gb", retmode = "text")
        entry_info <- tryCatch(strsplit(genbank_entry, '\n')[[1]], error = function(e) "")
        
        # get region and date index
        region_index <- grep('/country=',  entry_info)
        date_index <- grep('/collection_date=',  entry_info)

        if (length(region_index) == 0){region_index <- grep('/geo_loc_name=',  entry_info)}
        if(length(region_index) == 0 | length(date_index) == 0){
            drop_sample_no_date_or_region <- c(drop_sample_no_date_or_region, ID)
            next
        }

        # extract region and date from the identified line
        region_info <- entry_info[region_index]
        date_info <- entry_info[date_index]

        region <- tryCatch(strsplit(region_info, '\\"')[[1]][2], error = function(e) "NA")
        if(grepl(':', region)){region <- strsplit(region,':')[[1]][1]}
        date <- tryCatch(strsplit(date_info, '\\"')[[1]][2], error = function(e) "NA")
        
        regions <- c(regions, region)
        dates <- c(dates, date)
        keep_sample <- c(keep_sample, ID)
    }

    if (length(regions) == 0 | length(dates) == 0) {
        stop("No valid records found in the input file.")
    }  

    if (length(drop_sample_not_found) > 0) {
        drop_sample_not_found <- unique(drop_sample_not_found)
        warning_info <- paste(c('\nExcluded records:\n',drop_sample_not_found, '\nID not found in the database.'), collapse = " ")
        warning(warning_info)
    }
    if (length(drop_sample_no_date_or_region) > 0) {
        drop_sample_no_date_or_region <- unique(drop_sample_no_date_or_region)
        warning_info <- paste(c('\nExcluded records:\n',drop_sample_no_date_or_region, '\nDate and/or region information is not available.'), collapse = " ")
        warning(warning_info)
    }    

    result_df <- data.frame('ID' = keep_sample, 'region' = regions, 'date' = dates, stringsAsFactors = FALSE)
    result_df$date <- sapply(result_df$date, function(d) { # format and validate dates
        final_date <- NA

        for(fmt in DATE_FORMATS) { # attempt to parse the date with each format
            parsed_date <- as.Date(d, format = fmt)
            if (!is.na(parsed_date)) { # if successful, convert to the standard format '%Y-%m-%d'
                final_date <- as.character(parsed_date)
                break
            }
        }
           
        if (is.na(final_date)) { # if no format matched, add to drop_sample_date_format_issue
            drop_sample_date_format_issue <- c(drop_sample_date_format_issue, result_df$ID[result_df$date == d])
        }
  
        return(final_date)
    })

    if (length(drop_sample_date_format_issue) > 0) {
        drop_sample_date_format_issue <- unique(drop_sample_date_format_issue)
        warning_info <- paste(c('\nExcluded records:\n',drop_sample_date_format_issue, '\nDate in the entry is not in these formats: .', DATE_FORMATS, '\n.'), collapse = " ")
        warning(warning_info)
    }

    all_drops <- c(drop_sample_not_found, drop_sample_no_date_or_region, drop_sample_date_format_issue)
    result_df <- result_df[! result_df$ID %in% all_drops, ] # filter the result_df to exclude IDs in all_drops
    
    return(result_df)
}

