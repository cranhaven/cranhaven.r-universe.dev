# Dhis2r$set("public", "get_user_info",
#            function() {
#              reponse <- self$request_sent |>
#                req_url_path_append("me") |>
#                req_perform()
#
#              response_data  <-  reponse |>
#                resp_body_json(simplifyVector = TRUE)
#
#              self$access_rights <- unlist(response_data[["access"]])
#
#              self$account_info <- unlist(list(response_data[["userCredentials"]][["createdBy"]][c("name", "username")], response_data["created"]))
#
#              unlist( list( response_data["name"],
#                            response_data["phoneNumber"],
#                            response_data["email"]))
#
#            }
#            )

