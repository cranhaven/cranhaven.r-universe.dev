# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~ TCGAretriever ver 1.10.1 ~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#' Core Query Engine
#' 
#' Submit a query by getting or posting to the URL provided as argument 
#' (typically a cbioportal.org URL). The function attempts the same query recursively
#' until the content has been completely downloaded. 
#' If `body` is NULL, the query is submitted via GET. Otherwise, 
#' the query is submitted via POST.
#'   
#' @param my_url String. The URL pointing to the cBioPortal API. 
#' @param body String. Parameters to be passed via POST to the query URL. Can be NULL.
#' @param max_attempts Numeric, maximum number of attempts that will be
#' performed to fetch a response from the server. This is useful
#' to avoid infinite loops shall a data become unavailable.
#' 
#' @return Data.frame including data retrieved from cBioPortal.
#' 
#' @details This is a core function invoked by other functions in the package.
#'
#' @author Damiano Fantini, \email{damiano.fantini@@gmail.com}
#' @references 
#' \url{https://www.data-pulse.com/dev_site/TCGAretriever/}
#' 
#'  
#' @examples
#' # The example below requires an active Internet connection.
#' \dontrun{
#' my_url <- "https://www.cbioportal.org/api/studies"
#' x <- TCGAretriever:::cb_query(my_url)
#' }
#'  
#' @importFrom httr GET content timeout
#' @importFrom jsonlite fromJSON
#' 
#' 
#' @keywords internal
cb_query <- function(my_url, body = NULL, max_attempts = 6) {

  # initial arg check
  stopifnot(
    is.character(my_url), length(my_url) == 1, !is.na(my_url), 
    is.numeric(max_attempts), length(max_attempts) == 1, 
    ! is.na(max_attempts), max_attempts >= 1)
  
  if (!is.null(body)) {
    stopifnot(is.character(body), length(body) == 1, !is.na(body))
  }
  
  # Handle warns 
  curWarn <- options()$warn
  options(warn = -1)
  
  # init temp result
  my_get <- list("error", 400)
  my_post <- list("error", 400)
  max_attempts <- as.integer(max_attempts)
  cur_attempt <- 0
  
  # GET
  if (is.null(body)) {
    y <- tryCatch({
      while (cur_attempt < max_attempts && 
             (my_get[[1]] == "error" || my_get[[2]] != 200)) {
        my_get <- tryCatch(httr::GET(my_url, httr::timeout(10)), 
                           error = function(e) { c("error", 301) }) 
        
        cur_attempt <- cur_attempt + 1
      }
      
      my_cnt <- httr::content(my_get, "text", encoding = 'UTF-8')
      result <- jsonlite::fromJSON(my_cnt, simplifyVector = FALSE, 
                                   simplifyDataFrame = TRUE)
      #as.data.frame(result)
      result
    }, error = function(e) { data.frame() })
    
  } else {
    
    y <- tryCatch({
      while (cur_attempt < max_attempts && 
             (my_post[[1]] == "error" || my_post[[2]] != 200)) {
        
        my_post <- tryCatch({
          httr::POST(url = my_url, 
                     httr::add_headers('accept'='application/json', 
                                       'Content-Type'='application/json'), 
                     body = body,  httr::timeout(10))}, 
          error = function(e) { c("error", 301) }) 
        
        cur_attempt <- cur_attempt + 1
      }
      
      my_cnt <- httr::content(my_post, "text", encoding = 'UTF-8')
      result <- jsonlite::fromJSON(my_cnt, simplifyVector = FALSE, 
                                   simplifyDataFrame = TRUE)
      #as.data.frame(result)
      result
    }, error = function(e) { data.frame() })
  }
    
  # Wrap and return
  options(warn = curWarn)
  return(y)
}



#' Retrieve the List of Cancer Studies Available at cbioportal.
#' 
#' Retrieve information about the studies or datasets available at cbioportal.org. 
#' Information include a `studyId`, description, references, and more. 
#' 
#' 
#' @param dryrun Logical. If TRUE, all other arguments (if any) are ignored and 
#' a representative example is returned as output. No Internet connection is 
#' required for executing the operation when `dryrun` is TRUE. 
#' 
#' @author Damiano Fantini, \email{damiano.fantini@@gmail.com}
#' @references 
#' \url{https://www.data-pulse.com/dev_site/TCGAretriever/}
#' 
#'  
#' @return 
#' Data Frame including cancer study information. 
#' 
#' @examples
#' # Set `dryrun = FALSE` (default option) in production!
#' all_studies <- get_cancer_studies(dryrun = TRUE)
#' utils::head(all_studies)
#' 
#' 
#' 
#' @export
get_cancer_studies <- function(dryrun = FALSE) {
  
  # Handle dryrun
  stopifnot(is.logical(dryrun), length(dryrun) == 1, !is.na(dryrun))
  if (dryrun) {
    y <- tryCatch({TCGAretriever::blcaOutputExamples$exmpl_1}, 
                  error = function(e) { NULL })
    return(y)  
  }
  
  curWarn <- options()$warn
  options(warn = -1)
  
  my_url <- "https://www.cbioportal.org/api/studies"
  OUT <- tryCatch({as.data.frame(
    cb_query(my_url = my_url))}, 
                  error = function(e) { data.frame() })
  
  options(warn = curWarn)
  return(OUT)
}



#' Retrieve Cancer Types.
#' 
#' Retrieve information about cancer types and corresponding 
#' abbreviations from cbioportal.org. 
#' Information include identifiers, names, and parental cancer type.
#' 
#' @param dryrun Logical. If TRUE, all other arguments (if any) are ignored and 
#' a representative example is returned as output. No Internet connection is 
#' required for executing the operation when `dryrun` is TRUE. 
#' 
#' @author Damiano Fantini, \email{damiano.fantini@@gmail.com}
#' @references 
#' \url{https://www.data-pulse.com/dev_site/TCGAretriever/}
#' 
#'  
#' 
#' @return 
#' A data.frame including cancer type information.
#' 
#' @examples
#' # Set `dryrun = FALSE` (default option) in production!
#' all_canc <- get_cancer_types(dryrun = TRUE)
#' utils::head(all_canc)
#' 
#' @export
get_cancer_types <- function(dryrun = FALSE) {
  
  # Handle dryrun
  stopifnot(is.logical(dryrun), length(dryrun) == 1, !is.na(dryrun))
  if (dryrun) {
    y <- tryCatch({TCGAretriever::blcaOutputExamples$exmpl_2}, 
                  error = function(e) { NULL })
    return(y)  
  }
  
  curWarn <- options()$warn
  options(warn = -1)
  
  my_url <- "https://www.cbioportal.org/api/cancer-types?"
  OUT <- tryCatch({as.data.frame(
    cb_query(my_url = my_url))}, 
                  error = function(e) { data.frame() })
  
  options(warn = curWarn)
  return(OUT)
}


#' Retrieve Case List Available for a Specific Cancer Study.
#' 
#' Each study includes one or more "case lists". Each case list is a collection
#' of samples that were analyzed using one or more platforms/assays. 
#' It is possible to obtain a list of case list identifiers from cbioportal.org for a 
#' cancer study of interest. Identifier, name, description and category are
#' returned for each entry.
#' 
#' @param csid String corresponding to the Identifier of the Study of Interest
#' @param dryrun Logical. If TRUE, all other arguments (if any) are ignored and 
#' a representative example is returned as output. No Internet connection is 
#' required for executing the operation when `dryrun` is TRUE. 
#' 
#' 
#' @author Damiano Fantini, \email{damiano.fantini@@gmail.com}
#' @references 
#' \url{https://www.data-pulse.com/dev_site/TCGAretriever/}
#' 
#'  
#' @return Data Frame including Case List information. 
#' 
#' @examples 
#' # Set `dryrun = FALSE` (default option) in production!
#' blca_case_lists <- get_case_lists("blca_tcga", dryrun = TRUE)  
#' blca_case_lists
#' 
#' 
#' 
#' @export
get_case_lists <- function(csid, dryrun=FALSE) {
  
  # Handle dryrun
  stopifnot(is.logical(dryrun), length(dryrun) == 1, !is.na(dryrun))
  if (dryrun) {
    y <- tryCatch({TCGAretriever::blcaOutputExamples$exmpl_3}, 
                  error = function(e) { NULL })
    return(y)  
  }
  
  # Check CSID
  stopifnot(!is.null(csid), length(csid) == 1, 
            !is.na(csid), is.character(csid))
  
  curWarn <- options()$warn
  options(warn = -1)
  
  my_url <- "https://www.cbioportal.org/api/studies/"
  my_url <- paste(my_url, tolower(as.character(csid)), 
                  '/sample-lists?', sep = "")
  
  OUT <- tryCatch({as.data.frame(
    cb_query(my_url = my_url))}, 
                  error = function(e) { data.frame() })
  
  options(warn = curWarn)
  return(OUT)
}



#' Samples Included in a List of Samples. 
#' 
#' Each study includes one or more "case lists". Each case list is a collection
#' of samples that were analyzed using one or more platforms/assays. 
#' It is possible to obtain a list of all sample identifiers for each
#' case list of interest.   
#' 
#' @param csid String corresponding to a TCGA Cancer Study identifier.
#' @param dryrun Logical. If TRUE, all other arguments (if any) are ignored and 
#' a representative example is returned as output. No Internet connection is 
#' required for executing the operation when `dryrun` is TRUE.
#' 
#' @return list containing as many elements as TCGA case lists 
#' available for a given TCGA Study. Each element is named after a case list identifier.
#' Also, each element is a character vector including all sample identifiers
#' (case ids) corresponding to the corresponding case list identifier. 
#' 
#' @examples
#' # Set `dryrun = FALSE` (default option) in production!
#' x <- expand_cases("blca_tcga", dryrun = TRUE)
#' lapply(x, utils::head)
#' 
#' 
#' @author Damiano Fantini, \email{damiano.fantini@@gmail.com}
#' @references 
#' \url{https://www.data-pulse.com/dev_site/TCGAretriever/}
#' 
#' 
#' @export
expand_cases <-function(csid, dryrun = FALSE) {

  # Handle dryrun
  stopifnot(is.logical(dryrun), length(dryrun) == 1, !is.na(dryrun))
  if (dryrun) {
    y <- tryCatch({TCGAretriever::blcaOutputExamples$exmpl_4}, 
                  error = function(e) { NULL })
    return(y)  
  }
  
  # Check CSID
  stopifnot(!is.null(csid),
            length(csid) == 1,
            !is.na(csid), 
            is.character(csid))
  
  curWarn <- options()$warn
  options(warn = -1)

  # first, get casse lists
  tmp_csli <- tryCatch({
    get_case_lists(csid = csid)}, 
    error = function(e) { NULL })
  
  # init collector
  OUT <- list()
  
  if (!is.null(tmp_csli) && nrow(tmp_csli) > 0 &&
      'sampleListId' %in% colnames(tmp_csli)) {
    
    tmp_cli <- unique(tmp_csli$sampleListId)
    
    for (id in tmp_cli) {
      tryCatch({
        
        my_url <- "https://www.cbioportal.org/api/sample-lists/"
        my_url <- paste(my_url, tolower(id), '/sample-ids?', sep = "")
        
        tii <- tryCatch({unique(
          do.call(c, cb_query(my_url = my_url)))}, 
                        error = function(e) { c() })
        
        OUT[[ id ]] <- tii
        
      }, error = function(e) { NULL })
    }
  }
  
  options(warn = curWarn)
  return(OUT)
}



#' Retrieve Clinical Information for a Cancer Study.
#' 
#' Retrieve Clinical Information about the samples included in a cancer study of interest. 
#' For each sample/case, information about the corresponding cancer patient are returned. 
#' These may include sex, age, therapeutic regimen, tumor stage, 
#' survival status, as well as other information.  
#' 
#' @param csid String corresponding to a TCGA Cancer Study identifier.
#' @param case_list_id String corresponding to the case_list identifier of interest. This Can be NULL.
#' @param dryrun Logical. If TRUE, all other arguments (if any) are ignored and 
#' a representative example is returned as output. No Internet connection is 
#' required for executing the operation when `dryrun` is TRUE.
#' 
#' @author Damiano Fantini, \email{damiano.fantini@@gmail.com}
#' @references 
#' \url{https://www.data-pulse.com/dev_site/TCGAretriever/}
#' 
#'
#' 
#' @return data.frame including clinical information of a list of 
#' samples/cases of interest. 
#' 
#' @examples 
#' # Set `dryrun = FALSE` (default option) in production!
#' clinic_data <- get_clinical_data("blca_tcga", dryrun = TRUE)  
#' utils::head(clinic_data[, 1:7])
#'  
#' 
#' 
#' @importFrom reshape2 dcast
#' @importFrom stats as.formula 
#' 
#' 
#' @export
get_clinical_data <- function(csid, case_list_id = NULL, dryrun = FALSE) {

  # Handle dryrun
  stopifnot(is.logical(dryrun), length(dryrun) == 1, !is.na(dryrun))
  if (dryrun) {
    y <- tryCatch({TCGAretriever::blcaOutputExamples$exmpl_5}, 
                  error = function(e) { NULL })
    return(y)  
  }
  
  curWarn <- options()$warn
  options(warn = -1)
  
  # check case id
  stopifnot(!is.null(csid), length(csid) == 1, 
            !is.na(csid), is.character(csid), 
            nchar(csid) > 0)
  
  # check case_list_id
  if (!is.null(case_list_id)) {
    
    exp_cs <- expand_cases(csid = csid)
    
    stopifnot(length(case_list_id) == 1, 
              !is.na(case_list_id), is.character(case_list_id), 
              nchar(case_list_id) > 0, case_list_id %in% names(exp_cs))
    
    sel_cases <- exp_cs[[case_list_id]]
  } else {
    sel_cases <- NULL
  }
  
  # Get clinical data (sample)
  tryCatch({
    my_url <- "https://www.cbioportal.org/api/studies/"
    my_url <- paste0(my_url, csid, "/clinical-data?", 
                     "clinicalDataType=SAMPLE&projection=DETAILED")
    TMP0 <- as.data.frame(
      cb_query(my_url = my_url))
    
    TMP1 <- tryCatch({
      z <- TMP0$clinicalAttribute[, c('clinicalAttributeId', 'datatype')]
      z <- z[!duplicated(z), ]
      z <- z[z$datatype == 'NUMBER', ]
      z
      }, error = function(e) { NULL })
    
    try_frml <- 'sampleId+patientId+studyId~clinicalAttributeId'
    TMP2 <- reshape2::dcast(data = TMP0, formula = stats::as.formula(try_frml), 
                            value.var = 'value')
    
    if (!is.null(TMP1) && nrow(TMP1) > 0) {
      num_atts <- c()
      num_atts <- TMP1$clinicalAttributeId
      num_atts <- num_atts [ num_atts %in% colnames(TMP2)]
      
      for (ci in num_atts) {
        TMP2[, ci] <- as.numeric(as.character(TMP2[, ci]))  
      }
    }
    TMP2 <- TMP2[!duplicated(TMP2$sampleId), ]
    
  }, error = function(e) { NULL })
    

  # Get clinical data (patient)
  tryCatch({
    my_url <- "https://www.cbioportal.org/api/studies/"
    my_url <- paste0(my_url, csid, "/clinical-data?", 
                     "clinicalDataType=PATIENT&projection=DETAILED")
    TMP5 <- as.data.frame(
      cb_query(my_url = my_url))
    
    TMP6 <- tryCatch({
      z <- TMP5$clinicalAttribute[, c('clinicalAttributeId', 'datatype')]
      z <- z[!duplicated(z), ]
      z <- z[z$datatype == 'NUMBER', ]
      z
    }, error = function(e) { NULL })
    
    try_frml <- 'patientId+studyId~clinicalAttributeId'
    TMP7 <- reshape2::dcast(data = TMP5, formula = stats::as.formula(try_frml), 
                            value.var = 'value')
    
    if (!is.null(TMP6) && nrow(TMP6) > 0) {
      num_atts <- c()
      num_atts <- TMP6$clinicalAttributeId
      num_atts <- num_atts [ num_atts %in% colnames(TMP7)]
      
      for (ci in num_atts) {
        TMP7[, ci] <- as.numeric(as.character(TMP7[, ci]))  
      }
    }      
    TMP7 <- TMP7[!duplicated(TMP7$patientId), ]
    
  }, error = function(e) { NULL })
  
  # Merge sample info and patient info
  OUT <- tryCatch({
    dplyr::left_join(TMP2, TMP7,
                     by = c('studyId'='studyId', 'patientId'='patientId'))
  }, error = function(e) { NULL })
  
  # Filter if needed
  if (!is.null(sel_cases)) {
    OUT <- dplyr::left_join(
      data.frame(sampleId = sel_cases, stringsAsFactors = FALSE), 
      OUT, by = c('sampleId' = 'sampleId'))
  }

  tryCatch({
    rownames(OUT) <- NULL
  }, error = function(e) { NULL })
  
  options(warn = curWarn)
  return(OUT)
}



#' Retrieve Genetic Profiles for a TCGA Study of Interest
#' 
#' Retrieve Information about all genetic profiles associated with a cancer study of interest.
#' Each cancer study includes one or more types of molecular analyses.  The corresponding assays or 
#' platforms are referred to as genetic profiles. 
#' A genetic profile identifier is required to download molecular data.
#' 
#' @param csid String corresponding to the cancer study id of interest
#' @param dryrun Logical. If TRUE, all other arguments (if any) are ignored and 
#' a representative example is returned as output. No Internet connection is 
#' required for executing the operation when `dryrun` is TRUE.
#' 
#' @author Damiano Fantini, \email{damiano.fantini@@gmail.com}
#' @references 
#' \url{https://www.data-pulse.com/dev_site/TCGAretriever/}
#' 
#' 
#' @return 
#' data.frame including information about genetic profiles.
#' 
#' @examples 
#' # Set `dryrun = FALSE` (default option) in production!
#' get_genetic_profiles("blca_tcga", dryrun = TRUE)  
#' 
#' 
#' @export
get_genetic_profiles <- function(csid = NULL, dryrun = FALSE){
  
  # Handle dryrun
  stopifnot(is.logical(dryrun), length(dryrun) == 1, !is.na(dryrun))
  if (dryrun) {
    y <- tryCatch({TCGAretriever::blcaOutputExamples$exmpl_6}, 
                  error = function(e) { NULL })
    return(y)  
  }
  
  stopifnot(!is.null(csid), length(csid) == 1, 
            !is.na(csid), is.character(csid)) 
  
  curWarn <- options()$warn
  options(warn = -1)
  # options(warn = curWarn)
  
  if(!is.null(csid)){
    
    my_url <- "https://www.cbioportal.org/api/studies/"
    my_url <- paste0(my_url, csid, '/molecular-profiles?')
    OUT <- cb_query(my_url = my_url)
    
  } else {
    message("Missing cancer study ID")
    OUT <- NULL
  }
  
  options(warn = curWarn)
  return(OUT)
}



#' Retrieve All Gene Identifiers
#' 
#' Obtain all valid gene identifiers, including ENTREZ gene identifiers and 
#' HUGO gene symbols. Genes are classified according to the gene type 
#' (*e.g.*, 'protein-coding', 'pseudogene', 'miRNA', ...). Note that 
#' miRNA and phosphoprotein genes are associated with a negative entrezGeneId. 
#' 
#' 
#' @param dryrun Logical. If TRUE, all other arguments (if any) are ignored and 
#' a representative example is returned as output. No Internet connection is 
#' required for executing the operation when `dryrun` is TRUE.
#' 
#' @author Damiano Fantini, \email{damiano.fantini@@gmail.com}
#' @references 
#' \url{https://www.data-pulse.com/dev_site/TCGAretriever/}
#' 
#'  
#' @return Data Frame including gene identifiers. 
#' 
#' @examples 
#' # Set `dryrun = FALSE` (default option) in production!
#' x <- get_gene_identifiers(dryrun = TRUE)
#' 
#' 
#' @export
get_gene_identifiers <- function(dryrun = FALSE) {
  
  # Handle dryrun
  stopifnot(is.logical(dryrun), length(dryrun) == 1, !is.na(dryrun))
  if (dryrun) {
    y <- tryCatch({TCGAretriever::blcaOutputExamples$exmpl_7}, 
                  error = function(e) { NULL })
    return(y)  
  }
  
  curWarn <- options()$warn
  options(warn = -1)
  
  my_url <- paste0("https://www.cbioportal.org/api/genes?", 
                   "projection=DETAILED&pageSize=1000000", 
                   "&pageNumber=0&direction=ASC")

  OUT <- tryCatch({as.data.frame(
    cb_query(my_url = my_url))}, 
                  error = function(e) { data.frame() })
  
  OUT <- OUT[order(abs(as.numeric(OUT$entrezGeneId))), ]
  OUT <- OUT[order(OUT$type), ]
  rownames(OUT) <- NULL
    
  options(warn = curWarn)
  return(OUT)
}



#' Retrieve Molecular Data corresponding to a Genetic Profile of Interest.
#' 
#' Retrieve Data corresponding to a Genetic Profile of interest from a cancer study of interest.
#' This function is the workhorse of the TCGAretriever package and can be used to fetch data 
#' concerning several genes at once. For retrieving mutation data, please use the `get_mutation_data()` function. 
#' For large queries (more than 500 genes), please use the `fetch_all_tcgadata()` 
#' function.
#' 
#' @param case_list_id String corresponding to the Identifier of a list of cases. 
#' @param gprofile_id String corresponding to the Identifier of a genetic Profile of interest. 
#' @param glist Vector including one or more gene identifiers (ENTREZID or OFFICIAL_SYMBOL). ENTREZID
#' gene identifiers should be passed as numeric.
#' @param dryrun Logical. If TRUE, all other arguments (if any) are ignored and 
#' a representative example is returned as output. No Internet connection is 
#' required for executing the operation when `dryrun` is TRUE.
#' 
#' @author Damiano Fantini, \email{damiano.fantini@@gmail.com}
#' 
#' @references 
#' \url{https://www.data-pulse.com/dev_site/TCGAretriever/}
#' 
#'  
#' @return 
#' data.frame including the molecular data of interest. Rows are genes, columns are samples. 
#' 
#' @examples 
#' # Set `dryrun = FALSE` (default option) in production!
#' x <- get_molecular_data(case_list_id = 'blca_tcga_3way_complete',
#'                         gprofile_id = 'blca_tcga_rna_seq_v2_mrna',
#'                         glist = c("TP53", "E2F1"), dryrun = TRUE)
#' x[, 1:10]
#' 
#' 
#' @importFrom reshape2 dcast
#' 
#' @export
get_molecular_data <- function(case_list_id, gprofile_id, 
                               glist = c("TP53", "E2F1"), 
                               dryrun = FALSE){

  # Handle dryrun
  stopifnot(is.logical(dryrun), length(dryrun) == 1, !is.na(dryrun))
  if (dryrun) {
    y <- tryCatch({TCGAretriever::blcaOutputExamples$exmpl_8}, 
                  error = function(e) { NULL })
    return(y)  
  }
  
  # Check Args, part 1
  stopifnot(!is.null(case_list_id), length(case_list_id) == 1, 
            !is.na(case_list_id), 
            !is.null(gprofile_id), length(gprofile_id) == 1, 
            !is.na(gprofile_id), !is.null(glist))

  if (grepl('mutation', tolower(gprofile_id))) {
    stop ('For mutation data, please use the get_mutation_data() function.')
  }
  
  # Check Args, part 2
  glist <- unique(glist)
  glist <- glist[!is.na(glist)]
  stopifnot(length(glist) > 0, length(glist) <= 500)  
  
  curWarn <- options()$warn
  options(warn = -1)

  # Parse glist
  all_genes <- get_gene_identifiers()
  if (is.numeric(glist)) {
    my_genes <- all_genes[all_genes$entrezGeneId %in% glist, ]
    my_genes <- my_genes[!is.na(my_genes$entrezGeneId), ]
    my_genes <- my_genes[!duplicated(my_genes$entrezGeneId), ]
    rownames(my_genes) <- NULL

  } else {
    my_genes <- all_genes[all_genes$hugoGeneSymbol %in% glist, ]
    my_genes <- my_genes[!is.na(my_genes$entrezGeneId), ]
    my_genes <- my_genes[!duplicated(my_genes$entrezGeneId), ]
    rownames(my_genes) <- NULL
  }
  
  # Compile BODY for POST
  bod <- paste0(
    '{"sampleListId": "', case_list_id, 
    '", "entrezGeneIds": [',  
    paste(my_genes$entrezGeneId, collapse = ', '), ']}')

  # Build Query
  my_url <- paste0("https://www.cbioportal.org/api/molecular-profiles/", 
                   gprofile_id,
                   "/molecular-data/fetch?projection=DETAILED")
  
  result <- tryCatch({as.data.frame(
    cb_query(my_url = my_url, body = bod))}, 
                     error = function(e) { data.frame() })
  tryCatch({
    rry <- list()
    for (i in 1:ncol(result)) {
      if (is.data.frame(result[, i])) {
        rry[[i]] <- result[, i]
      } else {
        tmpp <- data.frame(x = result[, i], stringsAsFactors = FALSE)
        colnames(tmpp) <- colnames(result)[i]
        rry[[i]] <- tmpp
      }
    }
    result <- do.call(cbind, rry)
    result <- result[, !duplicated(colnames(result))]
  }, error = function(e) { NULL })

  tryCatch({
    my_frml <- 'entrezGeneId+hugoGeneSymbol+type~sampleId'
    result <- reshape2::dcast(data = result, 
                              formula = stats::as.formula(my_frml), 
                              value.var = 'value')
  }, error = function(e) { NULL })

  options(warn = curWarn)
  return(result)
}





#' Retrieve Mutation Data corresponding to a Genetic Profile of Interest.
#' 
#' Retrieve DNA Sequence Variations (Mutations) identified by exome sequencing projects.
#' This function is the workhorse of the TCGAretriever package for mutation data and can be used to fetch data 
#' concerning several genes at once. For retrieving non-mutation data, please use the `get_molecular_data()` function. 
#' For large queries (more than 500 genes), please use the `fetch_all_tcgadata()` 
#' function.
#'
#' @param case_list_id String corresponding to the Identifier of a list of cases.
#' @param gprofile_id String corresponding to the Identifier of a genetic Profile of interest.
#' @param glist Vector including one or more gene identifiers (ENTREZID or OFFICIAL_SYMOL). ENTREZID
#' gene identifiers should be passed as numeric.
#' @param dryrun Logical. If TRUE, all other arguments (if any) are ignored and 
#' a representative example is returned as output. No Internet connection is 
#' required for executing the operation when `dryrun` is TRUE.
#' 
#' @author Damiano Fantini, \email{damiano.fantini@@gmail.com}
#' @references 
#' \url{https://www.data-pulse.com/dev_site/TCGAretriever/}
#' 
#'  
#' 
#' @return data Frame inluding one row per mutation
#' 
#' @examples 
#' # Set `dryrun = FALSE` (default option) in production!
#' x <- get_mutation_data(case_list_id = 'blca_tcga_sequenced',
#'                        gprofile_id = 'blca_tcga_mutations',
#'                        glist = c('TP53', 'PTEN'), dryrun = TRUE)
#' utils::head(x[, c(4, 7, 23, 15, 16, 17, 24, 18, 21)]) 
#'  
#' 
#' @export
get_mutation_data <- function(case_list_id, gprofile_id, 
                              glist = c("TP53", "E2F1"), 
                              dryrun = FALSE){
  
  # Handle dryrun
  stopifnot(is.logical(dryrun), length(dryrun) == 1, !is.na(dryrun))
  if (dryrun) {
    y <- tryCatch({TCGAretriever::blcaOutputExamples$exmpl_9}, 
                  error = function(e) { NULL })
    return(y)  
  }
  
  # Check Args, part 1
  stopifnot(!is.null(case_list_id), length(case_list_id) == 1, 
            !is.na(case_list_id), 
            !is.null(gprofile_id), length(gprofile_id) == 1, 
            !is.na(gprofile_id), !is.null(glist))
  
  # Check Args, part 2
  glist <- unique(glist)
  glist <- glist[!is.na(glist)]
  stopifnot(length(glist) > 0, length(glist) <= 500)  
  
  curWarn <- options()$warn
  options(warn = -1)
  
  # Parse glist
  all_genes <- get_gene_identifiers()
  if (is.numeric(glist)) {
    my_genes <- all_genes[all_genes$entrezGeneId %in% glist, ]
    my_genes <- my_genes[!is.na(my_genes$entrezGeneId), ]
    my_genes <- my_genes[!duplicated(my_genes$entrezGeneId), ]
    rownames(my_genes) <- NULL
    
  } else {
    my_genes <- all_genes[all_genes$hugoGeneSymbol %in% glist, ]
    my_genes <- my_genes[!is.na(my_genes$entrezGeneId), ]
    my_genes <- my_genes[!duplicated(my_genes$entrezGeneId), ]
    rownames(my_genes) <- NULL
  }
  
  # Compile BODY for POST
  bod <- paste0(
    '{"sampleListId": "', case_list_id, 
    '", "entrezGeneIds": [',  
    paste(my_genes$entrezGeneId, collapse = ', '), ']}')
  
  # Build Query
  my_url <- paste0("https://www.cbioportal.org/api/molecular-profiles/", 
                   gprofile_id,
                   "/mutations/fetch?projection=DETAILED")
  
  result <- tryCatch({as.data.frame(
    cb_query(my_url = my_url, body = bod))}, 
    error = function(e) { data.frame() })
  
  tryCatch({
    rry <- list()
    for (i in 1:ncol(result)) {
      if (is.data.frame(result[, i])) {
        rry[[i]] <- result[, i]
      } else {
        tmpp <- data.frame(x = result[, i], stringsAsFactors = FALSE)
        colnames(tmpp) <- colnames(result)[i]
        rry[[i]] <- tmpp
      }
    }
    result <- do.call(cbind, rry)
    result <- result[, !duplicated(colnames(result))]
  }, error = function(e) { NULL })
  
  options(warn = curWarn)
  return(result)
}




#' Split Numeric Vectors in Groups.
#' 
#' Assign each element of a numeric vector to a group. Grouping is based on ranks: 
#' numeric values are sorted and then split in 2 or more groups. Values may be sorted 
#' in an increasing or decreasing fashion. The vector is returned in the original order. 
#' Labels may be assigned to each group.
#' 
#' @param num_vector numeric vector. It includes the values to be assigned to the different groups
#' @param groups integer. The number of groups that will be generated
#' @param group_labels character vector. Labels for each group. Note that 
#' the length of group_labels has to be equal to the number of groups
#' @param desc logical. If TRUE, the sorting is applied in a decreasing fashion
#' 
#' 
#' @author Damiano Fantini, \email{damiano.fantini@@gmail.com}
#' @references 
#' \url{https://www.data-pulse.com/dev_site/TCGAretriever/}
#' 
#'  
#' @return 
#' data.frame including the vector provided as argument in the original order ("value") 
#' and the grouping vector ("rank"). If labels are provided as an argument, group labels 
#' are also included in the data.frame ("labels"). 
#' 
#' @examples
#' exprs_geneX <- c(19.1,18.4,22.4,15.5,20.2,17.4,9.4,12.4,31.2,33.2,18.4,22.1)
#' groups_num <- 3
#' groups_labels <- c("high", "med", "low")
#' make_groups(exprs_geneX, groups_num, groups_labels, desc = TRUE)
#' 
#' @export
make_groups <- function(num_vector, groups, group_labels = NULL, desc = FALSE) {
  
  curWarn <- options()$warn
  options(warn = -1)
  
  OUT <- tryCatch({
    if(is.numeric(num_vector) & is.numeric(groups) & 
       length(num_vector) > 5 & groups[1] > 1){
      #
      my_order <- order(num_vector, decreasing = desc)
      groups <- as.integer(groups)
      batch_size <- as.integer(length(num_vector)/groups)
      my_ranks <- do.call(c,lapply(1:groups, (function(x){
        if(x != groups){
          rep(x,batch_size)  
        } else {
          rep(x, length(num_vector) - (length(1:(groups - 1)) * batch_size))
        }
      })))
      tmp_ranks <- rep(0, length(num_vector))
      tmp_ranks[my_order] <- my_ranks
      result <- cbind(num_vector, tmp_ranks)
      colnames(result) <- c("value", "rank")
      if(!is.null(names(num_vector))){
        rownames(result) <- names(num_vector)
      }
      result <- data.frame(result, stringsAsFactors = FALSE)
      if (!is.null(group_labels) & length(group_labels) == groups){
        lab_ranks <- sapply(tmp_ranks, (function(i){
          group_labels[i]
        }))
        result$labels <- lab_ranks
      }
    } 
    result
  }, error = function(e) { NULL })
  
  options(warn = curWarn)
  return(OUT)
}





#' Fetch All Molecular Data for a Cancer Profile of Interest.
#' 
#' Recursively query cbioportal to retrieve data corresponding to all 
#' available genes. Data are returned as a `data.frame` 
#' that can be easily manipulated for downstream analyses. 
#' 
#' @param case_list_id string corresponding to the identifier of the TCGA Case List of interest 
#' @param gprofile_id string corresponding to the identifier of the TCGA Profile of interest 
#' @param mutations logical. If TRUE, extended mutation data are fetched instead of the standard TCGA data 
#' 
#' @author Damiano Fantini, \email{damiano.fantini@@gmail.com}
#' @references 
#' \url{https://www.data-pulse.com/dev_site/TCGAretriever/}
#' 
#'  
#' 
#' @return 
#' A data.frame is returned, including the desired TCGA data. 
#' Typically, rows are genes and columns are cases. 
#' If "extended mutation" data are retrieved (mutations = TRUE), 
#' rows correspond to individual mutations 
#' while columns are populated with mutation features
#' 
#' 
#' @examples 
#' # The examples below require an active Internet connection.
#' # Note: execution may take several minutes.
#' \dontrun{
#' # Download all brca_pub mutation data (complete samples)
#' all_brca_MUT <- fetch_all_tcgadata(case_list_id = "brca_tcga_pub_complete", 
#'                                    gprofile_id = "brca_tcga_pub_mutations", 
#'                                    mutations = TRUE)
#' 
#' # Download all brca_pub RNA expression data (complete samples)
#' all_brca_RNA <- fetch_all_tcgadata(case_list_id = "brca_tcga_pub_complete", 
#'                                    gprofile_id = "brca_tcga_pub_mrna", 
#'                                    mutations = FALSE)
#' }
#' 
#' 
#' 
#' @export
fetch_all_tcgadata <- function (case_list_id, gprofile_id, mutations = FALSE) {
  
  # Check 1  
  stopifnot(!is.null(case_list_id), is.character(case_list_id), 
            length(case_list_id) == 1, 
            !is.na(case_list_id), nchar(case_list_id) > 0,
            !is.null(gprofile_id), is.character(gprofile_id), 
            length(gprofile_id) == 1, !is.na(gprofile_id), 
            nchar(gprofile_id) > 0)
  
  # proceed  
  curWarn <- options()$warn
  options(warn = -1)
  
  # init dummy output
  final_out <- NULL
  
  try({

    # Check 2
    glist <- get_gene_identifiers()
    glist <- as.numeric(glist$entrezGeneId)
    glist <- glist[!is.na(glist)]
    glist <- unique(glist)
    stopifnot(length(glist) > 0)
    
    
    # Handle batches
    chunk_gene <- split(glist, ceiling(seq_along(glist)/200))
    
    tmp_output <- lapply(1:length(chunk_gene), (function(i) {
      
      if (mutations) {
        tmp_res <- get_mutation_data(case_list_id = case_list_id, 
                                     gprofile_id = gprofile_id, 
                                     glist = chunk_gene[[i]])
      } else {
        tmp_res <- get_molecular_data(case_list_id = case_list_id, 
                                      gprofile_id = gprofile_id, 
                                      glist = chunk_gene[[i]]) 
      }
      
      if (i %% 10 == 0) {
        message(paste("Genes processed: ", i * 200, "...", 
                      sep = ""))
      }
      
      tmp_res
    }))
    message("Aggregating data together...")
    
    keep <- vapply(tmp_output, FUN.VALUE = 1,
                   FUN = function(z) { 
                     tryCatch({as.numeric(ncol(z) > 0 & nrow(z) > 0)}, 
                              error = function(e) { 0 })})
    tmp_output <- tmp_output[keep == 1]
    all_clnms <- unique(do.call(c, lapply(tmp_output, colnames)))
    tmp_output <- lapply(tmp_output, function(z) {
      tmp <- data.frame(iiindex = seq_len(nrow(z)), stringsAsFactors = FALSE)
      for (ci in all_clnms) {
        if (ci %in% colnames(z)) {
          tmp[, ci] <- z[, ci]
        } else {
          tmp[, ci] <- NA
        }
      }
      tmp[, -1]
    })
    final_out <- do.call(rbind, tmp_output)
    
    r2rem <- do.call(c, lapply(1:nrow(final_out), (function(i) {
      sum(is.na(final_out[i, ])) == (ncol(final_out))
    })))
    final_out <- final_out[!r2rem, ]
    rownames(final_out) <- NULL
  }, silent = TRUE)
  
  options(warn = curWarn)
  return(final_out)
}






#' TCGAretriever Examples
#' 
#' A list of objects including examples of the output returned by 
#' different `TCGAretriever` functions. The objects were obtained
#' from the `"blca_tcga"` study (bladder cancer).
#' 
#' @usage data(blcaOutputExamples)
#' 
#' @format A list including 7 elements.
#' \describe{
#'   \item{exmpl_1}{data.frame (dimensions: 10 by 13). Sample output of the `get_cancer_studies()` function.}
#'   \item{exmpl_2}{data.frame (dimensions: 10 by 5). Sample output of the `get_cancer_types()` function.}
#'   \item{exmpl_3}{data.frame (dimensions: 9 by 5). Sample output of the `get_case_lists()` function.}
#'   \item{exmpl_4}{list including 9 elements. Sample output of the `expand_cases()` function.}
#'   \item{exmpl_5}{data.frame (dimensions: 10 by 94). Sample output of the `get_clinical_data()` function.}
#'   \item{exmpl_6}{data.frame (dimensions: 9 by 8). Sample output of the `get_genetic_profiles()` function.}
#'   \item{exmpl_7}{data.frame (dimensions: 10 by 3). Sample output of the `get_gene_identifiers()` function.}
#'   \item{exmpl_8}{data.frame (dimensions: 2 by 10). Sample output of the `get_molecular_data()` function.}
#'   \item{exmpl_9}{data.frame (dimensions: 6 by 27). Sample output of the `get_mutation_data()` function.}
#' }
#' 
#'
#' @details
#' The object was built using the following lines of code.
#' \code{
#' blcaOutputExamples <- list(
#'   exmpl_1 = head(get_cancer_studies(), 10),
#'   exmpl_2 = head(get_cancer_types(), 10),
#'   exmpl_3 = head(get_case_lists("blca_tcga"), 10)  ,
#'   exmpl_4 = expand_cases("blca_tcga"),
#'   exmpl_5 = head(get_clinical_data("blca_tcga"), 10),
#'   exmpl_6 = head(get_genetic_profiles("blca_tcga") , 10),
#'   exmpl_7 = head(get_gene_identifiers(), 10), 
#'   exmpl_8 = get_molecular_data(case_list_id = 'blca_tcga_3way_complete',
#'                                gprofile_id = 'blca_tcga_rna_seq_v2_mrna',
#'                                glist = c("TP53", "E2F1"))[, 1:10],
#'   exmpl_9 = head(get_mutation_data(case_list_id = 'blca_tcga_sequenced',
#'                                    gprofile_id = 'blca_tcga_mutations',
#'                                    glist = c('TP53', 'PTEN'))))
#' }
#' 
#' 
#' @examples
#' data(blcaOutputExamples)
#' blcaOutputExamples$exmpl_1
#' 
#' @name blcaOutputExamples
"blcaOutputExamples"


#' Retrieve Genomic and Clinical Data from CBioPortal
#'
#' @description 
#' The Cancer Genome Atlas (TCGA) is a scientific and medical program 
#' aimed at improving our understanding of Cancer Biology. 
#' Part of the TCGA Datasets (free-access tier) are hosted
#' on cBioPortal, which is an open-access, open-source resource for 
#' interactive exploration of multidimensional cancer genomics data sets. 
#' TCGAretriever helps accessing and downloading TCGA data via the 
#' cBioPortal API. Features of TCGAretriever are: 
#' \itemize{
#'   \item it is simple and reliable
#'   \item it is tailored for downloading large volumes of data
#' }
#' 
#' @author Damiano Fantini, \email{damiano.fantini@@gmail.com}
#' @references 
#' \itemize{
#'   \item \url{https://www.data-pulse.com/dev_site/TCGAretriever/}
#'   \item \url{http://www.cbioportal.org/} 
#'   \item \url{https://www.cancer.gov/ccg/research/genome-sequencing/tcga}
#' }
#'  
#' @examples
#' # List available Adenoid Cystic Carcinoma (acyc) Studies.
#' # Set `dryrun = FALSE` (default option) in production!
#' all_studies <- get_cancer_studies(dryrun = TRUE)
#' acyc_ids <- grep('acyc', all_studies$studyId, value = TRUE)
#' print(acyc_ids)
#' 
#' 
#' # List blca_tcga profiles.
#' # Set `dryrun = FALSE` (default option) in production!
#' get_genetic_profiles(csid = 'blca_tcga', dryrun = TRUE)
#' 
#' 
#' # List blca_tcga case lists.
#' # Set `dryrun = FALSE` (default option) in production!
#' get_case_lists(csid = 'blca_tcga', dryrun = TRUE)
#' 
#' 
#' # Retrieve expression data.
#' # Set `dryrun = FALSE` (default option) in production!
#' my_genes <- c('PTEN', 'TP53')
#' get_molecular_data(case_list_id = 'blca_tcga_3way_complete', 
#'                    gprofile_id = 'blca_tcga_rna_seq_v2_mrna', 
#'                    glist = my_genes, dryrun = TRUE)
#' 
#' 
#' @keywords internal
"_PACKAGE"

