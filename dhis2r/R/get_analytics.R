#
#
# Dhis2r$set("public", "get_analytics", overwrite = T,
#            function(analytic,org_unit ,period, output_scheme= c("UID", "NAME")) {
#
#              output_scheme <- match.arg(output_scheme)
#
#              analytic <- paste0("dx:", paste0(analytic,collapse = ";"))
#              org_unit <- paste0("dimension=ou:", paste0(org_unit,collapse = ";"))
#              period <- paste0("dimension=pe:",  paste0(period,collapse = ";"))
#
#                reponse <- self$request_sent |>
#                  req_url_path_append("analytics") |>
#                  req_url_query(dimension= I(paste(analytic, org_unit, period, sep = "&"))) |>
#                  req_url_query(outputIdScheme = output_scheme) |>
#                  req_perform()
#
#
#                response_data  <-  reponse |>
#                   resp_body_json(simplifyVector = TRUE, flatten = TRUE)
#
#
#                as.data.frame(response_data$rows) |>
#                       setNames(c("analytic", "org_unit", "period", "value")) |>
#                  tibble::as_tibble()
#
#              })
#
#
#
#

