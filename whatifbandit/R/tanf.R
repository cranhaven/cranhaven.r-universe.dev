#' Public TANF Recipient Data From Washington D.C
#'
#' A modified version of the data set used in <https://thelabprojects.dc.gov/benefits-reminder-letter>
#' with one additional column added for analysis.
#'
#' Variables are as follows:
#'
#'
#' \describe{
#' \item{ic_case_id}{Unique, anonymized case identifier.}
#' \item{service_center}{DC Department of Human Services Center assigned each case.}
#' \item{condition}{The assigned letter condition: "No Letter", "Open Appointment", or "Specific Appointment".}
#' \item{recert_month}{Recertification Month.}
#' \item{letter_sent_date}{Date the second (treatment) letter was sent.}
#' \item{recert_id}{Administrative recertification identifier.}
#' \item{return_to_sender}{Indicates whether letter was returned as undeliverable}
#' \item{pdc_status}{PDC Status}
#' \item{renewal_date}{Date by which renewal must be completed.}
#' \item{notice_date.x}{Date the first notice was sent (initial legal communication)}
#' \item{days_betwn_notice_and_recert_due}{Number of days between the first notice and the recertification due date.}
#' \item{cert_period_start}{Start date of the recertification period.}
#' \item{cert_period_end}{End date of recertification period.}
#' \item{recert_status}{Status of recertification process (Pending, Denied, etc.)}
#' \item{denial_reason}{Reason for denial if recertification was not approved.}
#' \item{recert_month_year}{Combined recertification month and year.}
#' \item{notice_date.y}{Alternate record of first notice date.}
#' \item{recert_status_dcas}{Official recertification status from DCAS}
#' \item{date_of_recert}{Date the recertification was successfully submitted (if applicable).}
#' \item{success}{Binary variable indicating successful recertification based on recert_status (newly added column).}
#' }
#'
#' @source <https://github.com/thelabdc/DHS-TANFRecertification-Public/blob/main/data/df_replication_anonymized.csv>
#'
#' @docType data
#' @name tanf
#' @usage data(tanf)
"tanf"
