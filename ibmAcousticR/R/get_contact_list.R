#' Get Export of a Database or contact List
#' 
#' This function submits a job to Acoustic that exports a particular
#' database or contact list based on the list id.  Various criteria 
#' are available to filter the export. Some, but not all, of these
#' have been built into the parameters of this function. Reading the 
#' IBM Acoustic documentation is useful:
#' https://developer.ibm.com/customer-engagement/tutorials/
#' export-from-a-database/
#' 
#' Job results are available as exports in the Silverpop portal by
#' going to Resources -> Data Jobs.
#' 
#' It is not recommended that these authentication parameters be 
#' stored directly in your code. There are various methods and 
#' packages available that are more secure; this package does not 
#' require you to use any one in particular.
#' 
#' @param pod_number Pod number is the number in the URL, e.g. 
#' engage1.silverpop.com.
#' @param session_access_token Access token obtained during this session.
#' @param list_id Acoustic id for the database or contact list (string).
#' @param start_date Filter for emails sent on or after this date.
#' @param end_date Filter for emails sent on or before this date.
#' @param export_format Acoustic provides three delimiter file types: 
#' CSV, PIPE, TAB. CSV is the default used here.
#' @param move_to_ftp If TRUE (default is FALSE) will send files to SFTP server
#' instead of being able to download manually from the portal.
#' @param confirm_email Optional argument to specify an email address
#' where IBM will let you know when the job has completed. 
#' 
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "RETRY"
#' @importFrom httr "POST"
#' @importFrom httr "content"
#' @importFrom httr "add_headers"
#' 
#' @return A vector with the Job Id.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' access_token <- acoustic_auth(org_client_id = "abc",
#' org_client_secret = "xyz",
#' my_refresh_token = "123")
#' 
#' job_id <- get_contact_list(pod_number, access_token, list_id,
#' "2020-01-01", "2020-01-05", "PIPE")
#' }


get_contact_list <- function(pod_number, session_access_token, list_id, start_date, end_date,
                             export_format = "CSV", move_to_ftp = FALSE, confirm_email = "") {

  # Reformat the dates
  start_date2 <- as.character(format(as.Date(start_date), "%m/%d/%Y %H:%M:%S"))
  end_date2 <- as.character(format(as.Date(end_date) + 1, "%m/%d/%Y %H:%M:%S"))
    
  # Build the XML request
  xml_parameters <- paste0("
    <Envelope>
      <Body>
        <ExportList>",
          "<DATE_START>", start_date2, "</DATE_START>",
          "<DATE_END>", end_date2, "</DATE_END>",
          "<EXPORT_FORMAT>", export_format, "</EXPORT_FORMAT>",
          "<LIST_ID>", list_id, "</LIST_ID>",
          ifelse(move_to_ftp == FALSE, "<ADD_TO_STORED_FILES/>", ""),
          ifelse(confirm_email != "", paste0("<EMAIL>", confirm_email, "</EMAIL>"), ""),
          "<INCLUDE_LEAD_SOURCE/>
          <INCLUDE_RECIPIENT_ID/>
          <EXPORT_TYPE>ALL</EXPORT_TYPE>",
        "</ExportList>
      </Body>
    </Envelope>")

  # Submit the request
  request <- httr:: RETRY("POST",
                        url = paste0("https://api-campaign-us-", pod_number, ".goacoustic.com/XMLAPI"),
                        httr::add_headers("Content-Type" = "text/xml;charset=utf-8",
                                          "Authorization" = paste0("Bearer ", session_access_token)),
                        body = xml_parameters,
                        encode = "json",
                        times = 4,
                        pause_min = 10,
                        terminate_on = NULL,
                        terminate_on_success = TRUE,
                        pause_cap = 5)
  
  check_request_status(request)
  check_for_faulty_xml(request)

  # Get and return the Job Id
  job_id <- get_job_id(request, "//Envelope/Body/RESULT/JOB_ID")
  return(job_id)
  
}
