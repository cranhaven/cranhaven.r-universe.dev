#' Get Export of All Email Contact Events
#' 
#' This function submits a job to Acoustic that exports all email contact
#' events. Various criteria are available to filter the export. Some,
#' but not all, of these have been built into the parameters of this 
#' function. Reading the IBM Acoustic documentation is useful:
#' https://developer.ibm.com/customer-engagement/tutorials/
#' export-raw-contact-events/
#'
#' The date type is set to EVENT by default. If you filter by the sent
#' date you may not get all applicable events, as some events (a future
#' click) will not yet have happened. If you do filter by SENT date and
#' are incrementally updating your data you should plan to go back and
#' retroactively update past dates.
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
#' @param start_date Filter for emails sent on or after this date.
#' @param end_date Filter for emails sent on or before this date.
#' @param date_type Select whether the date filters should be on the event
#' date or the email sent date ("EVENT" or "SENT").
#' @param event_types There are 18 different events. By default all event
#' types are returned. This parameter takes XML arguments where you can
#' override the default and specify all of the events you want. See the
#' Acoustic documentation for the full list.
#' @param export_format Acoustic provides three delimiter file types: 
#' 0 (CSV), 1 (PIPE), or 2 (TAB). CSV is the default used here.
#' @param move_to_ftp If TRUE (default is FALSE) will send files to SFTP 
#' server instead of being able to download manually from the portal.
#' @param exclude_deleted Do you want to exclude contacts that have been
#' deleted, can be TRUE/FALSE. Per Acoustic, "Inclusion of this 
#' element can greatly decrease the time to generate the metrics file and 
#' is useful whenever metrics for deleted contacts are not required."
#' @param optional_columns Do you want to include six optional columns
#' in the results, can be TRUE/FALSE. These columns are the mailing name,
#' mailing subject, from email address, from email name, CRM campaign Id,
#' and program Id.
#' @param file_name_prefix Optional argument that should be used if you 
#' want to add a particular prefix to the file that you will download
#' from your portal.
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
#' job_id <- get_all_contacts(pod_number, access_token,
#' "2020-01-01", "2020-01-05", event_types = "<CLICKS/>",
#' 1, exclude_deleted = TRUE, optional_columns = TRUE)
#' }


get_all_contacts <- function(pod_number, session_access_token, start_date, 
                             end_date, date_type = "EVENT", 
                             event_types = "<ALL_EVENT_TYPES/>", 
                             export_format = 0, move_to_ftp = FALSE, 
                             exclude_deleted = FALSE, optional_columns = TRUE,
                             file_name_prefix = "", confirm_email = "") {
  
  # Reformat the dates
  start_date2 <- as.character(format(as.Date(start_date), "%m/%d/%Y"))
  end_date2 <- as.character(format(as.Date(end_date), "%m/%d/%Y"))
  
  # Build the XML request
  xml_parameters <- paste0("
    <Envelope>
      <Body>
        <RawRecipientDataExport>",
          ifelse(date_type == "EVENT", paste0(
              "<EVENT_DATE_START>", start_date2, "</EVENT_DATE_START>",
              "<EVENT_DATE_END>", end_date2, "</EVENT_DATE_END>"
            ), paste0(
              "<SEND_DATE_START>", start_date2, "</SEND_DATE_START>",
              "<SEND_DATE_END>", end_date2, "</SEND_DATE_END>"
            )
          ),
          "<EXPORT_FORMAT>", export_format, "</EXPORT_FORMAT>",
          ifelse(move_to_ftp == FALSE, "","<MOVE_TO_FTP/>"),
          ifelse(file_name_prefix != "", paste0("<EXPORT_FILE_NAME>", file_name_prefix, "</EXPORT_FILE_NAME>"), ""),
          ifelse(confirm_email != "", paste0("<EMAIL>", confirm_email, "</EMAIL>"), ""),
          event_types,
          ifelse(exclude_deleted == TRUE, "<EXCLUDE_DELETED/>", ""),
          ifelse(optional_columns == TRUE, 
            "<RETURN_MAILING_NAME/>
            <RETURN_SUBJECT/>
            <RETURN_FROM_ADDRESS/>
            <RETURN_FROM_NAME/>
            <RETURN_CRM_CAMPAIGN_ID/>
            <RETURN_PROGRAM_ID/>", ""),
        "</RawRecipientDataExport>
      </Body>
    </Envelope>
    ")

  # Submit the request
  request <- httr::RETRY("POST",
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
  job_id <- get_job_id(request, "//Envelope/Body/RESULT/MAILING/JOB_ID")
  return(job_id)
  
}
