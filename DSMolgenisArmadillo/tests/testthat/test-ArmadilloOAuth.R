test_that("get_token returns the id_token from credentials", {
  mock_credentials <- new("ArmadilloCredentials",
                          access_token = "access123",
                          expires_in = 3600,
                          expires_at = Sys.time() + 3600,
                          id_token = "abcd-abcd",
                          refresh_token = "refresh123",
                          token_type = "Bearer"
  )

  with_mock(
    "DSMolgenisArmadillo::armadillo.get_credentials" = function(server) mock_credentials,
    token <- armadillo.get_token("https://example.org")
  )

  expect_equal(token, "abcd-abcd")
})

test_that("armadillo.get_credentials returns a valid ArmadilloCredentials object", {
  server <- "https://example.org"

  dummy_auth_info <- list(auth = list(
    clientId = "dummy-client-id",
    issuerUri = "https://auth.example.org"
  ))

  dummy_endpoint <- httr::oauth_endpoint(
    authorize = "auth_url",
    access = "token_url"
  )

  dummy_credentials <- list(
    access_token = "access123",
    expires_in = 3600,
    id_token = "id123",
    refresh_token = "refresh123",
    token_type = "Bearer",
    userId = "user123"
  )

  with_mock(
    "DSMolgenisArmadillo:::.get_oauth_info" = function(server) dummy_auth_info,
    "MolgenisAuth::discover" = function(issuer) dummy_endpoint,
    "MolgenisAuth::device_flow_auth" = function(endpoint, client_id) dummy_credentials,
    {
      result <- armadillo.get_credentials(server)
    }
  )

  expect_s4_class(result, "ArmadilloCredentials")
  expect_equal(result@access_token, "access123")
  expect_equal(result@expires_in, 3600)
  expect_equal(result@id_token, "id123")
  expect_equal(result@refresh_token, "refresh123")
  expect_equal(result@token_type, "Bearer")
  expect_true(abs(as.numeric(result@expires_at - (Sys.time() + 3600))) < 2)
})

test_that(".refresh_token returns success message if new credentials are not null and option fusionauth", {
  server <- "https://example.org"
  credentials_in <- methods::new("ArmadilloCredentials",
                                 access_token = "access123",
                                 expires_in = 3600,
                                 expires_at = Sys.time(),
                                 id_token = "id123",
                                 refresh_token = "refresh123",
                                 token_type = "Bearer",
                                 auth_type = "fusionauth"
  )
  credentials_out <- methods::new("ArmadilloCredentials",
                                  access_token = "access456",
                                  expires_in = -70.20557,
                                  expires_at = as.POSIXct("2025-04-29 09:45:08 CEST"),
                                  id_token = "id123",
                                  refresh_token = "refresh456",
                                  token_type = "Bearer",
                                  auth_type = "fusionauth")

  content_mock_fusion <- list(
    refreshToken = "refresh456",
    token = "access456")

  dummy_auth_info <- list(auth = list(issuerUri = "https://auth.example.org"))
  dummy_response <- structure(list(status_code = 400), class = "response")

  with_mock(
    "DSMolgenisArmadillo:::.get_oauth_info" = function(server) dummy_auth_info,
    "DSMolgenisArmadillo:::.get_updated_expiry_date" = function(auth_info, token) as.POSIXct("2025-04-29 09:45:08 CEST"),
    "httr::POST" = function(...) dummy_response,
    "httr::content" = function(response) content_mock_fusion,
    {

      expect_message(
        DSMolgenisArmadillo:::.refresh_token(server, credentials_in),
        regexp = "Refresh successful"
      )

      expect_equal(
        DSMolgenisArmadillo:::.refresh_token(server, credentials_in),
        credentials_out,
        tolerance = 0.2)
    }
  )
})

test_that(".refresh_token returns success message if new credentials are not null and option keycloak", {
  server <- "https://example.org"
  credentials_in <- methods::new("ArmadilloCredentials",
                                 access_token = "access123",
                                 expires_in = 3600,
                                 expires_at = as.POSIXct("2025-07-08 14:52:44 CEST"),
                                 id_token = "id123",
                                 refresh_token = "refresh123",
                                 token_type = "Bearer",
                                 auth_type = "keycloak"
  )

  credentials_out <- methods::new("ArmadilloCredentials",
                                  access_token = "access456",
                                  expires_in = 300,
                                  expires_at = as.POSIXct(Sys.time() + 300),
                                  id_token = "id123",
                                  refresh_token = "refresh123",
                                  token_type = "Bearer",
                                  auth_type = "keycloak"
  )

  content_mock_keycloak <- list(
    access_token = "access456",
    expires_in = 300,
    refresh_expires_in = 0,
    refresh_token = "refresh123",
    token_type = "Bearer",
    id_token = "id123",
    `not-before-policy` = 0,
    session_state = "abc123",
    scope = "openid profile email"
  )

  dummy_auth_info <- list(auth = list(issuerUri = "https://auth.example.org"))
  dummy_response <- structure(list(status_code = 400), class = "response")

  with_mock(
    "DSMolgenisArmadillo:::.get_oauth_info" = function(server) dummy_auth_info,
    "DSMolgenisArmadillo:::.get_updated_expiry_date" = function(auth_info, token) as.POSIXct("2025-04-29 09:45:08 CEST"),
    "httr::POST" = function(...) dummy_response,
    "httr::content" = function(response) content_mock_keycloak,
    {

      expect_message(
        DSMolgenisArmadillo:::.refresh_token(server, credentials_in),
        regexp = "Refresh successful"
      )

      print(DSMolgenisArmadillo:::.refresh_token(server, credentials_in))
      expect_equal(
        DSMolgenisArmadillo:::.refresh_token(server, credentials_in),
        credentials_out,
        tolerance = 0.10)
    }
  )
})

test_that(".refresh_token stops with message if fieldErrors returned with fusionauth flow", {
  server <- "https://example.org"
  credentials <- methods::new("ArmadilloCredentials",
                                 access_token = "access123",
                                 expires_in = 3600,
                                 expires_at = Sys.time(),
                                 id_token = "id123",
                                 refresh_token = "refresh123",
                                 token_type = "Bearer",
                                 auth_type = "fusionauth"
  )


  dummy_auth_info <- list(auth = list(issuerUri = "https://auth.example.org"))
  dummy_response <- structure(list(status_code = 400), class = "response")
  dummy_content <- list(
      refreshToken = "refresh456",
      token = "access456",
      fieldErrors = list(message = "Invalid refresh token")
  )

  with_mock(
    "DSMolgenisArmadillo:::.get_oauth_info" = function(server) dummy_auth_info,
    "DSMolgenisArmadillo:::.get_updated_expiry_date" = function(auth_info, token) as.POSIXct("2025-04-29 09:45:08 CEST"),
    "httr::POST" = function(...) dummy_response,
    "httr::content" = function(response) dummy_content,
    {
      expect_error(
        DSMolgenisArmadillo:::.refresh_token(server, credentials),
        regexp = "Invalid refresh token"
      )
    }
  )
})

test_that(".refresh_token stops with generic message if refresh fails silently fusionauth flow", {
  server <- "https://example.org"
  credentials <- new("ArmadilloCredentials",
                     access_token = "access123",
                     refresh_token = "refresh123",
                     expires_in = 3600,
                     expires_at = Sys.time() + 3600,
                     id_token = "id123",
                     token_type = "Bearer",
                     auth_type = "fusionauth")

  dummy_auth_info <- list(auth = list(issuerUri = "https://auth.example.org"))
  dummy_response <- structure(list(status_code = 200), class = "response")
  dummy_empty_response <- list()  # no refreshToken, no fieldErrors

  with_mock(
    "DSMolgenisArmadillo:::.get_oauth_info" = function(server) dummy_auth_info,
    "DSMolgenisArmadillo:::.get_updated_expiry_date" = function(auth_info, token) Sys.time(),
    "httr::POST" = function(...) dummy_response,
    "httr::content" = function(response) dummy_empty_response,
    {
      expect_error(
        DSMolgenisArmadillo:::.refresh_token(server, credentials),
        "Refresh failed"
      )
    }
  )
})

test_that(".refresh_token stops with message if fieldErrors returned with keycloak flow", {
  server <- "https://example.org"
  credentials <- methods::new("ArmadilloCredentials",
                              access_token = "access123",
                              expires_in = 3600,
                              expires_at = Sys.time(),
                              id_token = "id123",
                              refresh_token = "refresh123",
                              token_type = "Bearer",
                              auth_type = "keycloak"
  )

    dummy_auth_info <- list(auth = list(issuerUri = "https://auth.example.org"))
  dummy_response <- structure(list(status_code = 400), class = "response")
  dummy_content <- list(
    access_token = "access456",
    expires_in = 300,
    refresh_expires_in = 0,
    refresh_token = "refresh123",
    token_type = "Bearer",
    id_token = "id123",
    `not-before-policy` = 0,
    session_state = "abc123",
    scope = "openid profile email",
    fieldErrors = list(message = "Invalid refresh token")
  )

  with_mock(
    "DSMolgenisArmadillo:::.get_oauth_info" = function(server) dummy_auth_info,
    "DSMolgenisArmadillo:::.get_updated_expiry_date" = function(auth_info, token) as.POSIXct("2025-04-29 09:45:08 CEST"),
    "httr::POST" = function(...) dummy_response,
    "httr::content" = function(response) dummy_content,
    {
      expect_error(
        DSMolgenisArmadillo:::.refresh_token(server, credentials),
        regexp = "Invalid refresh token"
      )
    }
  )
})

test_that(".refresh_token stops with generic message if refresh fails silently keycloak flow", {
  server <- "https://example.org"
  credentials <- new("ArmadilloCredentials",
                     access_token = "access123",
                     refresh_token = "refresh123",
                     expires_in = 3600,
                     expires_at = Sys.time() + 3600,
                     id_token = "id123",
                     token_type = "Bearer",
                     auth_type = "keycloak")

  dummy_auth_info <- list(auth = list(issuerUri = "https://auth.example.org"))
  dummy_response <- structure(list(status_code = 200), class = "response")
  dummy_empty_response <- list()  # no refreshToken, no fieldErrors

  with_mock(
    "DSMolgenisArmadillo:::.get_oauth_info" = function(server) dummy_auth_info,
    "DSMolgenisArmadillo:::.get_updated_expiry_date" = function(auth_info, token) Sys.time(),
    "httr::POST" = function(...) dummy_response,
    "httr::content" = function(response) dummy_empty_response,
    {
      expect_error(
        DSMolgenisArmadillo:::.refresh_token(server, credentials),
        "Refresh failed"
      )
    }
  )
})

test_that(".get_oauth_info returns content when request is successful", {
  dummy_response <- structure(list(status_code = 200), class = "response")
  dummy_content <- list(auth = list(clientId = "abc123", issuerUri = "https://auth.example.org"))

  with_mock(
    "httr::GET" = function(url) dummy_response,
    "httr::stop_for_status" = function(response, task) invisible(response),
    "httr::content" = function(response) dummy_content,
    {
      result <- DSMolgenisArmadillo:::.get_oauth_info("https://example.org")
    }
  )

  expect_equal(result$auth$clientId, "abc123")
  expect_equal(result$auth$issuerUri, "https://auth.example.org")
})

test_that(".get_oauth_info stops if server info fetch fails", {
  dummy_response <- structure(list(status_code = 404), class = "response")

  with_mock(
    "httr::GET" = function(url) dummy_response,
    "httr::stop_for_status" = function(response, task) {
      stop("404 Not Found")
    },
    {
      expect_error(
        DSMolgenisArmadillo:::.get_oauth_info("https://example.org"),
        "404 Not Found"
      )
    }
  )
})


test_that(".get_all_armadillo_credentials finds all credentials in test environment", {
  test_1 <- new(
    "ArmadilloCredentials",
    access_token = "aaa-a",
    expires_in = 29,
    expires_at = Sys.time(),
    id_token = "original_id_token_3",
    refresh_token = "aaa-b",
    token_type = "Bearer"
  )

  test_2 <- new(
    "ArmadilloCredentials",
    access_token = "bbb-a",
    expires_in = 29,
    expires_at = Sys.time(),
    id_token = "original_id_token_3",
    refresh_token = "bbb-b",
    token_type = "Bearer"
  )

  expect_equal(
    .get_all_armadillo_credentials(env = environment()),
    list(
      test_1 = test_1,
      test_2 = test_2
    )
  )
})

test_that(".get_all_armadillo_credentials returns nothing if no connections present", {

  expect_null(
    .get_all_armadillo_credentials(env = new.env())
  )

})


test_that("get_matching_credential returns correct match when there is one credentials object and it has a matching token", {
  credentials_1 <-new(
    "ArmadilloCredentials",
    access_token = "aaa-a",
    expires_in = 29,
    expires_at = as.POSIXct("2025-03-26 11:15:36", tz = "CET"),
    id_token = "original_id_token_3",
    refresh_token = "aaa-b",
    token_type = "Bearer"
  )

  cohort_1_cred <- list(cohort_1 = credentials_1)

  conn_1 <-  new(
    "ArmadilloConnection",
    name = "cohort_1",
    handle = handle,
    user = "",
    cookies = list(
      domain = "#HttpOnly_localhost",
      flag = FALSE,
      path = "/",
      secure = FALSE,
      expiration = "Inf",
      name = "JSESSIONID",
      value = "12345"),
    token = "aaa-a"
  )

  expect_equal(
    .get_matching_credential(cohort_1_cred, conn_1),
    list(
      name = "cohort_1",
      object = cohort_1_cred$cohort_1
    )
  )
})

test_that("get_matching_credential returns correct match when at least two credentials object but only has a matching token", {
  credentials_1 <-new(
    "ArmadilloCredentials",
    access_token = "aaa-a",
    expires_in = 29,
    expires_at = as.POSIXct("2025-03-26 11:15:36", tz = "CET"),
    id_token = "original_id_token_3",
    refresh_token = "aaa-b",
    token_type = "Bearer"
  )

  credentials_2 <-new(
    "ArmadilloCredentials",
    access_token = "bbb-a",
    expires_in = 29,
    expires_at = as.POSIXct("2025-03-26 11:15:36", tz = "CET"),
    id_token = "original_id_token_3",
    refresh_token = "bbb-b",
    token_type = "Bearer"
  )

  cohort_mult_cred <- list(
    cohort_1 = credentials_1,
    cohort_2 = credentials_2
  )

  conn_1 <-  new(
    "ArmadilloConnection",
    name = "cohort_2",
    handle = handle,
    user = "",
    cookies = list(
      domain = "#HttpOnly_localhost",
      flag = FALSE,
      path = "/",
      secure = FALSE,
      expiration = "Inf",
      name = "JSESSIONID",
      value = "12345"),
    token = "bbb-a"
  )

  expect_equal(
    .get_matching_credential(cohort_mult_cred, conn_1),
    list(
      name = "cohort_2",
      object = cohort_mult_cred$cohort_2
    )
  )
})


test_that("get_matching_credential returns first match when there is are two identical credentials objects and both have a matching token", {
  credentials_1 <-new(
    "ArmadilloCredentials",
    access_token = "aaa-a",
    expires_in = 29,
    expires_at = as.POSIXct("2025-03-26 11:15:36", tz = "CET"),
    id_token = "original_id_token_3",
    refresh_token = "aaa-b",
    token_type = "Bearer"
  )

  cohort_identical_cred <- list(
    cohort_1 = credentials_1,
    cohort_2 = credentials_1
  )

  conn_1 <-  new(
    "ArmadilloConnection",
    name = "cohort_2",
    handle = handle,
    user = "",
    cookies = list(
      domain = "#HttpOnly_localhost",
      flag = FALSE,
      path = "/",
      secure = FALSE,
      expiration = "Inf",
      name = "JSESSIONID",
      value = "12345"),
    token = "aaa-a"
  )

  expect_equal(
    .get_matching_credential(cohort_identical_cred, conn_1),
    list(
      name = "cohort_1",
      object = cohort_identical_cred$cohort_1
    )
  )
})

test_that("get_matching_credential returns NULL there is one credentials object and no matching token", {
  credentials_1 <-new(
    "ArmadilloCredentials",
    access_token = "aaa-a",
    expires_in = 29,
    expires_at = as.POSIXct("2025-03-26 11:15:36", tz = "CET"),
    id_token = "original_id_token_3",
    refresh_token = "aaa-b",
    token_type = "Bearer"
  )

  cohort_1_cred <- list(cohort_1 = credentials_1)

  conn_1 <-  new(
    "ArmadilloConnection",
    name = "cohort_1",
    handle = handle,
    user = "",
    cookies = list(
      domain = "#HttpOnly_localhost",
      flag = FALSE,
      path = "/",
      secure = FALSE,
      expiration = "Inf",
      name = "JSESSIONID",
      value = "12345"),
    token = "bbb-b"
  )

  expect_null(
    .get_matching_credential(cohort_1_cred, conn_1)
  )
})

test_that("get_matching_credential returns NULL when there are two credentials object and no matching token", {
  credentials_1 <-new(
    "ArmadilloCredentials",
    access_token = "aaa-a",
    expires_in = 29,
    expires_at = as.POSIXct("2025-03-26 11:15:36", tz = "CET"),
    id_token = "original_id_token_3",
    refresh_token = "aaa-b",
    token_type = "Bearer"
  )

  credentials_2 <-new(
    "ArmadilloCredentials",
    access_token = "bbb-a",
    expires_in = 29,
    expires_at = as.POSIXct("2025-03-26 11:15:36", tz = "CET"),
    id_token = "original_id_token_3",
    refresh_token = "bbb-b",
    token_type = "Bearer"
  )

  cohort_mult_cred <- list(
    cohort_1 = credentials_1,
    cohort_2 = credentials_2
  )

  conn_1 <-  new(
    "ArmadilloConnection",
    name = "cohort_1",
    handle = handle,
    user = "",
    cookies = list(
      domain = "#HttpOnly_localhost",
      flag = FALSE,
      path = "/",
      secure = FALSE,
      expiration = "Inf",
      name = "JSESSIONID",
      value = "12345"),
    token = "ccc-c"
  )

  expect_null(
    .get_matching_credential(cohort_mult_cred, conn_1)
  )
})

test_that(".get_armadillo_credentials returns the correct match", {
  env <- new.env()

  cred <- new(
    "ArmadilloCredentials",
    access_token = "abc123",
    expires_in = 29,
    expires_at = Sys.time(),
    id_token = "id-abc",
    refresh_token = "ref-abc",
    token_type = "Bearer"
  )

  assign("test_cred", cred, envir = env)

  conn <- new(
    "ArmadilloConnection",
    name = "cohort_1",
    handle = handle,
    user = "",
    cookies = list(),
    token = "abc123"
  )

  result <- .get_armadillo_credentials(conn, env = env)

  expect_type(result, "list")
  expect_equal(result$name, "test_cred")
  expect_identical(result$object, cred)
})

test_that(".get_armadillo_credentials returns NULL when there are no credentials", {
  env <- new.env()
  expect_null(
    .get_armadillo_credentials(conn = new("ArmadilloConnection", token = "abc"), env = env)
  )
})

test_that(".get_armadillo_credentials works with custom env and multiple credentials", {
  env <- new.env()

  cred1 <- new(
    "ArmadilloCredentials",
    access_token = "token-1",
    expires_in = 29,
    expires_at = Sys.time(),
    id_token = "id-1",
    refresh_token = "ref-1",
    token_type = "Bearer"
  )

  cred2 <- new(
    "ArmadilloCredentials",
    access_token = "token-2",
    expires_in = 29,
    expires_at = Sys.time(),
    id_token = "id-2",
    refresh_token = "ref-2",
    token_type = "Bearer"
  )

  assign("cred_one", cred1, envir = env)
  assign("cred_two", cred2, envir = env)

  conn <- new(
    "ArmadilloConnection",
    name = "cohort_X",
    handle = handle,
    user = "",
    cookies = list(),
    token = "token-2"
  )

  result <- .get_armadillo_credentials(conn, env = env)

  expect_type(result, "list")
  expect_equal(result$name, "cred_two")
  expect_identical(result$object, cred2)
})

test_that(".reset_armadillo_credentials correctly updates tokens", {
  test_env <- new.env()

  # Original credentials
  old_cred <- new(
    "ArmadilloCredentials",
    access_token = "old-token",
    expires_in = 29,
    expires_at = as.POSIXct("2025-03-26 11:00:00", tz = "CET"),
    id_token = "id_token_x",
    refresh_token = "old-refresh",
    token_type = "Bearer"
  )

  assign("cohort_1", old_cred, envir = test_env)

  old_credentials <- list(
    name = "cohort_1",
    object = old_cred
  )

  new_credentials <- old_cred
  new_credentials@access_token = "new-token"
  new_credentials@refresh_token = "new-refresh"
  new_credentials@expires_at = as.POSIXct("2035-03-26 11:00:00", tz = "CET")

  .reset_armadillo_credentials(old_credentials, new_credentials, env = test_env)

  updated <- get("cohort_1", envir = test_env)

  expect_s4_class(updated, "ArmadilloCredentials")
  expect_equal(updated@access_token, "new-token")
  expect_equal(updated@refresh_token, "new-refresh")

  # Check that other slots remain unchanged
  expect_equal(updated@id_token, "id_token_x")
  expect_equal(updated@token_type, "Bearer")
})

test_that(".reset_armadillo_credentials modifies globalenv by default", {
  # Set up in globalenv (clean up after test)
  on.exit(rm("cohort_2", envir = globalenv()), add = TRUE)

  old_cred <- new(
    "ArmadilloCredentials",
    access_token = "global-token",
    expires_in = 10,
    expires_at = as.POSIXct("2025-03-26 11:00:00", tz = "CET"),
    id_token = "id_token_y",
    refresh_token = "global-refresh",
    token_type = "Bearer"
  )

  assign("cohort_2", old_cred, envir = globalenv())

  old_credentials <- list(
    name = "cohort_2",
    object = old_cred
  )

  new_credentials <- old_cred
  new_credentials@access_token = "new-global-token"
  new_credentials@refresh_token = "new-global-refresh"
  new_credentials@expires_at = as.POSIXct("2035-03-26 11:00:00", tz = "CET")

  .reset_armadillo_credentials(old_credentials, new_credentials)

  updated <- get("cohort_2", envir = globalenv())

  expect_equal(updated@access_token, "new-global-token")
  expect_equal(updated@refresh_token, "new-global-refresh")
})

test_that(".reset_connections_object updates token in correct ArmadilloConnection", {
  test_env <- new.env()

  conn_1 <-  new(
    "ArmadilloConnection",
    name = "cohort_1",
    handle = handle,
    user = "",
    cookies = list(
      domain = "#HttpOnly_localhost",
      flag = FALSE,
      path = "/",
      secure = FALSE,
      expiration = "Inf",
      name = "JSESSIONID",
      value = "12345"),
    token = "aaa-old"
  )

  if (!"methods" %in% loadedNamespaces()) library(methods)
  setClass("opal", contains = "environment")
  setClass("OpalConnection",
           slots = c(name = "character", opal = "opal"))

  conn_2 <- new("OpalConnection",
                name = "cohort_2",
                opal = new("opal"))

  conns <- list(conn_1, conn_2)
  names(conns) <- c("cohort_1", "cohort_2")

  assign("conns", conns, envir = test_env)

  old_cred <- new("ArmadilloCredentials",
                  access_token = "aaa-old",
                  expires_in = 3600,
                  expires_at = Sys.time(),
                  id_token = "id_token_dummy",
                  refresh_token = "refresh_old",
                  token_type = "Bearer")

  old_credentials <- list(name = "cohort_1", object = old_cred)
  new_credentials <- old_cred
  new_credentials@access_token = "aaa-new"

  # Run test
  .reset_connections_object(old_credentials, new_credentials, conn = cohort_1, env = test_env)

  updated_conns <- get("conns", envir = test_env)
  expect_equal(updated_conns$cohort_1@token, "aaa-new")
  expect_equal(updated_conns$cohort_2@name, "cohort_2")
  expect_s4_class(updated_conns$cohort_2, "OpalConnection")
})

test_that(".reset_connections_object only updates first matching ArmadilloConnection", {
  test_env <- new.env()

  if (!"methods" %in% loadedNamespaces()) library(methods)
  setClass("opal", contains = "environment")

  conn_1 <- new("ArmadilloConnection",
                name = "cohort_1",
                handle = handle,
                user = "",
                cookies = list(
                  domain = "#HttpOnly_localhost",
                  flag = FALSE,
                  path = "/",
                  secure = FALSE,
                  expiration = "Inf",
                  name = "JSESSIONID",
                  value = "12345"),
                token = "aaa-old")

  conn_2 <- new("ArmadilloConnection",
                name = "cohort_2",
                handle = handle,
                user = "",
                cookies = list(
                  domain = "#HttpOnly_localhost",
                  flag = FALSE,
                  path = "/",
                  secure = FALSE,
                  expiration = "Inf",
                  name = "JSESSIONID",
                  value = "67890"),
                token = "aaa-old")

  conn_3 <- new("OpalConnection",
                name = "server2",
                opal = new("opal"))

  conns <- list(cohort_1 = conn_1, cohort_2 = conn_2, server2 = conn_3)
  assign("conns", conns, envir = test_env)

  old_cred <- new("ArmadilloCredentials",
                  access_token = "aaa-old",
                  expires_in = 3600,
                  expires_at = Sys.time(),
                  id_token = "id_token_dummy",
                  refresh_token = "refresh_old",
                  token_type = "Bearer")

  old_credentials <- list(name = "cohort_1", object = old_cred)
  new_credentials <- old_cred
  new_credentials@access_token = "new-token"

  .reset_connections_object(old_credentials, new_credentials, conn = conn_1, env = test_env)

  updated <- get("conns", envir = test_env)
  expect_equal(updated$cohort_1@token, "new-token")
  expect_equal(updated$cohort_2@token, "aaa-old")
  expect_s4_class(updated$server2, "OpalConnection")
})

test_that(".reset_token_global_env updates credentials and connection", {
  test_env <- new.env()

  conn_1 <-  new(
    "ArmadilloConnection",
    name = "cohort_1",
    handle = handle,
    user = "",
    cookies = list(
      domain = "#HttpOnly_localhost",
      flag = FALSE,
      path = "/",
      secure = FALSE,
      expiration = "Inf",
      name = "JSESSIONID",
      value = "12345"),
    token = "aaa-old"
  )

  if (!"methods" %in% loadedNamespaces()) library(methods)
  setClass("opal", contains = "environment")
  conn_2 <- new("OpalConnection",
                name = "cohort_2",
                opal = new("opal"))

  conns <- list(conn_1, conn_2)
  names(conns) <- c("cohort_1", "cohort_2")

  assign("conns", conns, envir = test_env)

  old_cred <- new("ArmadilloCredentials",
                  access_token = "aaa-old",
                  expires_in = 3600,
                  expires_at = Sys.time(),
                  id_token = "id_token_dummy",
                  refresh_token = "refresh_old",
                  token_type = "Bearer")

  assign("cohort_1", old_cred, envir = test_env)

  old_credentials <- list(name = "cohort_1", object = old_cred)

  new_credentials <- old_cred
  new_credentials@access_token = "aaa-new"
  new_credentials@refresh_token = "refresh_new"
  new_credentials@expires_at = as.POSIXct("2035-03-26 11:00:00", tz = "CET")

  .reset_token_global_env(old_credentials, new_credentials, conn, env = test_env)

  updated_conns <- get("conns", envir = test_env)
  updated_creds <- get("cohort_1", envir = test_env)

  expect_equal(updated_conns$cohort_1@token, "aaa-new")
  expect_equal(updated_creds@access_token, "aaa-new")
  expect_equal(updated_creds@refresh_token, "refresh_new")
})

library(mockery)

test_that(".reset_token_if_expired refreshes and updates token if expired", {
  if (!"methods" %in% loadedNamespaces()) library(methods)

  if (!isClass("opal")) setClass("opal", contains = "environment")

  handle <- structure(list(handle = new("externalptr"), url = "https://localhost"), class = "handle")

  expired_cred <- new("ArmadilloCredentials",
                      access_token = "expired-token",
                      expires_in = 3600,
                      expires_at = Sys.time() - 10,
                      id_token = "id_token_dummy",
                      refresh_token = "old-refresh",
                      token_type = "Bearer")

  conn <- new("ArmadilloConnection",
              name = "cohort_1",
              handle = handle,
              user = "",
              cookies = list(
                domain = "#HttpOnly_localhost",
                flag = FALSE,
                path = "/",
                secure = FALSE,
                expiration = "Inf",
                name = "JSESSIONID",
                value = "12345"),
              token = "expired-token")

  updated_conn_env <- new.env()

  stub(.reset_token_if_expired, ".get_armadillo_credentials", function(conn) {
    list(name = "cohort_1", object = expired_cred)
  })

  stub(.reset_token_if_expired, ".refresh_token", function(url, credentials) {
    new("ArmadilloCredentials",
        access_token = "new-token",
        expires_in = 3600,
        expires_at = Sys.time() - 10,
        id_token = "id_token_dummy",
        refresh_token = "new-refresh",
        token_type = "Bearer")
  })

  stub(.reset_token_if_expired, ".reset_token_global_env", function(old_credentials, new_credentials, conn, env = updated_conn_env) {
    conn@token <- new_credentials@access_token
    assign("updated_conn_result", conn, envir = env)
  })

  .reset_token_if_expired(conn, env = updated_conn_env)

  updated_conn <- get("updated_conn_result", envir = updated_conn_env)
  expect_s4_class(updated_conn, "ArmadilloConnection")
  expect_equal(updated_conn@token, "new-token")
})

library(mockery)

test_that(".reset_token_if_expired returns warning when refresh fails", {
  if (!"methods" %in% loadedNamespaces()) library(methods)

  if (!isClass("opal")) setClass("opal", contains = "environment")

  handle <- structure(list(handle = new("externalptr"), url = "https://localhost"), class = "handle")

  expired_cred <- new("ArmadilloCredentials",
                      access_token = "expired-token",
                      expires_in = 3600,
                      expires_at = Sys.time() - 10,
                      id_token = "id_token_dummy",
                      refresh_token = "old-refresh",
                      token_type = "Bearer")

  conn <- new("ArmadilloConnection",
              name = "cohort_1",
              handle = handle,
              user = "",
              cookies = list(
                domain = "#HttpOnly_localhost",
                flag = FALSE,
                path = "/",
                secure = FALSE,
                expiration = "Inf",
                name = "JSESSIONID",
                value = "12345"),
              token = "expired-token")

  stub(.reset_token_if_expired, ".get_armadillo_credentials", function(conn) {
    list(name = "cohort_1", object = expired_cred)
  })

  stub(.reset_token_if_expired, ".refresh_token", function(url, credentials) {
    stop("mocked refresh failure")
  })

  expect_warning(
    tryCatch(
      .reset_token_if_expired(conn),
      error = function(e) warning(paste0("Failed to reset token: ", e$message))
    ),
    "Failed to reset token: mocked refresh failure"
  )
})

test_that(".get_updated_expiry_date returns correct expiry time", {
  auth_info <- list(auth = list(issuerUri = "https://auth.example.org"))
  token <- "dummy_token"

  dummy_response <- structure(list(), class = "response")
  expiry_time <- Sys.time() + 3600  # 1 hour from now
  dummy_content <- list(jwt = list(exp = expiry_time))

  with_mock(
    "httr::GET" = function(...) dummy_response,
    "httr::content" = function(response) dummy_content,
    {
      result <- DSMolgenisArmadillo:::.get_updated_expiry_date(auth_info, token)
      expect_s3_class(result, "POSIXct")
      expect_equal(result, expiry_time)
    }
  )
})

test_that(".refresh_token_safely returns refreshed connection if successful", {
  conn <- structure(list(), class = "DSConnection")
  refreshed_conn <- structure(list(), class = "ArmadilloConnection")

  with_mock(
    "DSMolgenisArmadillo:::.reset_token_if_expired" = function(conn, env) refreshed_conn,
    {
      result <- DSMolgenisArmadillo:::.refresh_token_safely(conn)
      expect_s3_class(result, "ArmadilloConnection")
    }
  )
})

test_that(".refresh_token_safely returns original connection if refresh returns non-connection", {
  conn <- structure(list(), class = "DSConnection")
  non_connection <- NULL  # or any object not of class "ArmadilloConnection"

  with_mock(
    "DSMolgenisArmadillo:::.reset_token_if_expired" = function(conn, env) non_connection,
    {
      result <- DSMolgenisArmadillo:::.refresh_token_safely(conn)
      expect_identical(result, conn)
    }
  )
})

test_that(".refresh_token_safely returns original connection and warns if error occurs", {
  conn <- structure(list(), class = "DSConnection")

  with_mock(
    "DSMolgenisArmadillo:::.reset_token_if_expired" = function(conn, env) stop("token error"),
    {
      expect_warning(
        result <- DSMolgenisArmadillo:::.refresh_token_safely(conn),
        regexp = "Failed to reset token: token error"
      )
      expect_identical(result, conn)
    }
  )
})

test_that(".reset_token_if_expired returns NULL if credentials are NULL", {
  conn <- structure(list(), class = "DSConnection")

  with_mock(
    "DSMolgenisArmadillo:::.get_armadillo_credentials" = function(conn) NULL,
    {
      result <- DSMolgenisArmadillo:::.reset_token_if_expired(conn)
      expect_null(result)
    }
  )
})

test_that(".reset_token_if_expired returns NULL if token has not expired", {
  conn <- structure(list(
    handle = list(url = "https://example.org"),
    token = "token123"
  ), class = "DSConnection")

  # Create a dummy S4 object with an expires_at slot
  DummyCredentials <- methods::setClass("DummyCredentials", slots = c(expires_at = "POSIXct"))
  dummy_object <- DummyCredentials(expires_at = Sys.time() + 3600)  # token valid for 1 hour

  valid_credentials <- list(object = dummy_object)

  with_mock(
    "DSMolgenisArmadillo:::.get_armadillo_credentials" = function(conn) valid_credentials,
    {
      result <- DSMolgenisArmadillo:::.reset_token_if_expired(conn)
      expect_null(result)
    }
  )
})

test_that(".check_multiple_conns throws error when multiple connections are found", {
  env <- new.env()

  with_mock(
    "DSMolgenisArmadillo:::.getDSConnectionsMod" = function(env) list(flag = 2, conns = list(conn1 = 1, conn2 = 2)),
    {
      expect_error(
        DSMolgenisArmadillo:::.check_multiple_conns(env),
        regexp = "Token has expired however it was not possible to refresh token because multiple DataSHIELD connection objects found in environment"
      )
    }
  )
})

test_that(".get_all_armadillo_credentials returns NULL when no matching objects found", {
  env <- new.env()
  assign("not_creds", list(some = "thing"), envir = env)

  result <- DSMolgenisArmadillo:::.get_all_armadillo_credentials(env)
  expect_null(result)
})

test_that(".getDSConnectionsMod returns flag = 2 when multiple DSConnection lists are found", {
  env <- new.env()

  # Create dummy lists (structure doesn't matter, since we mock .isDSConnection to always return TRUE)
  assign("conn_list1", list("dummy_conn1"), envir = env)
  assign("conn_list2", list("dummy_conn2"), envir = env)

  with_mock(
    "DSMolgenisArmadillo:::.isDSConnection" = function(x) TRUE,
    {
      result <- DSMolgenisArmadillo:::.getDSConnectionsMod(env)
      expect_equal(result$flag, 2)
      expect_setequal(result$conns, c("conn_list1", "conn_list2"))
    }
  )
})

test_that(".isDSConnection returns FALSE for non-S4 objects", {
  non_s4_obj <- list(a = 1)
  result <- DSMolgenisArmadillo:::.isDSConnection(non_s4_obj)
  expect_false(result)
})

test_that(".isDSConnection returns FALSE for S4 object not inheriting from DSConnection", {
  DummyClass <- methods::setClass("DummyClass", slots = c(x = "numeric"))
  obj <- DummyClass(x = 123)

  result <- DSMolgenisArmadillo:::.isDSConnection(obj)
  expect_false(result)
})
