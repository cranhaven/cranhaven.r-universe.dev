#' Get List of Programs
#' 
#' Get list of all programs in a particular date range. Prior
#' to attempting this you must authenticate and obtain an
#' access token. 
#' 
#' @param pod_number Pod number is the number in the URL, e.g. 1 in
#' engage1.silverpop.com.
#' @param session_access_token Access token obtained during this session.
#' @param start_date Filter for programs created on or after this date.
#' @param end_date Filter for programs created on or before this date.
#' 
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "RETRY"
#' @importFrom httr "POST"
#' @importFrom httr "content"
#' @importFrom httr "add_headers"
#' @importFrom XML "xmlParse"
#' @importFrom XML "getNodeSet"
#' @importFrom XML "xmlToDataFrame"
#' 
#' @return A data frame with the programs and program details.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' access_token <- acoustic_auth(org_client_id = "abc",
#' org_client_secret = "xyz",
#' my_refresh_token = "123")
#'
#' get_programs(1, access_token, "2020-01-01", "2020-05-31")
#' }

get_programs <- function(pod_number, session_access_token, start_date, end_date) {
  
  # Reformat the dates
  start_date2 <- as.character(format(as.Date(start_date), "%m/%d/%Y"))
  end_date2 <- as.character(format(as.Date(end_date), "%m/%d/%Y"))  
  
  # Build the XML request
  xml_parameters <- paste0("
    <Envelope>
      <Body>
        <GetPrograms>
          <INCLUDE_ACTIVE>True</INCLUDE_ACTIVE>
          <INCLUDE_INACTIVE>True</INCLUDE_INACTIVE>
          <CREATED_DATE_RANGE>",
            "<BEGIN_DATE>", start_date2, "</BEGIN_DATE>",
            "<END_DATE>", end_date2, "</END_DATE>",
          "</CREATED_DATE_RANGE>
        </GetPrograms>
      </Body>
    </Envelope>")
  

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
  request_df <- XML::xmlToDataFrame(nodes = XML::getNodeSet(request_xml, "//PROGRAM"))
  return(request_df)
}
