#' Get the Status of a Submitted Job
#' 
#' Prior to attempting this you must authenticate and obtain an
#' access token, and then submit a call that is processed as a 
#' job to retrieve from the Acoustic portal. The function used
#' to submit that job will provide the Job Id.
#' 
#' @param pod_number Pod number is the number in the URL, e.g. 
#' engage1.silverpop.com.
#' @param session_access_token Access token obtained during this session.
#' @param desired_job_id Id for job for which you want the status.
#' 
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "RETRY"
#' @importFrom httr "POST"
#' @importFrom httr "content"
#' @importFrom httr "add_headers"
#' @importFrom XML "xmlParse"
#' @importFrom XML "xmlValue"
#' @importFrom XML "xpathSApply"
#' 
#' @return A vector with the session's access token.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' access_token <- acoustic_auth(org_client_id = "abc",
#' org_client_secret = "xyz",
#' my_refresh_token = "123")
#'
#' job_id <- get_all_contacts(access_token)
#' get_job_status(1, access_token, "123456789")
#' }

get_job_status <- function(pod_number, session_access_token, desired_job_id) {
  
  # Build the XML request
  xml_parameters <- paste0("
    <Envelope>
      <Body>
      <GetJobStatus>
      <JOB_ID>", desired_job_id, "</JOB_ID>
      </GetJobStatus>
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
  
  # Extract and return the job status
  request_content <- httr::content(request, "text", encoding = "ISO-8859-1")
  request_xml <- XML::xmlParse(request_content)
  job_status <- XML::xpathSApply(request_xml, "//Envelope/Body/RESULT/JOB_STATUS", XML::xmlValue)
  return(job_status)
}
