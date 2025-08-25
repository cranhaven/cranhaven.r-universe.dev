test_that("dsDisconnect calls /logout endpoint ", {
  post <- mock()
  with_mock(
    "httr::POST" = post,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsDisconnect(connection)
  )
  expect_called(post, 1)
  expect_args(post, 1,
              handle = handle,
              path = "/logout",
              config = httr::add_headers("Authorization" = "Bearer token"))
})

test_that("dsDisconnect saves the workspace", {
  post <- mock(list(status_code = 200), cycle = TRUE)
  with_mock(
    "httr::POST" = post,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsDisconnect(connection, save = "keepit")
  )
  expect_called(post, 2)
  expect_args(post, 1,
              handle = handle,
              path = "/workspaces/keepit",
              config = httr::add_headers("Authorization" = "Bearer token"))
})

test_that("dsListProfiles retrieves profiles", {
  profiles <- list(
                   available = list("default", "exposome"),
                   current = "exposome")
  get <- mock(list(status_code = 200))
  content <- mock(profiles)
  result <- with_mock(
    "httr::GET" = get,
    "httr::content" = content,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsListProfiles(connection)
  )
  expect_args(get, 1,
              handle = handle,
              path = "/profiles",
              config = httr::add_headers("Authorization" = "Bearer token"))
  expect_equal(result, profiles)
})


test_that("dsListProfiles returns default result if none found", {
  get <- mock(list(status_code = 404))
  content <- mock(profiles)
  result <- with_mock(
    "httr::GET" = get,
    "httr::content" = content,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsListProfiles(connection)
  )
  expect_args(get, 1,
              handle = handle,
              path = "/profiles",
              config = httr::add_headers("Authorization" = "Bearer token"))
  expect_equal(result, list(
    available = "default",
    current = "default"
  ))
})

test_that("dsListTables retrieves tables", {
  tables <- list("a", "b")
  get <- mock(list(status_code = 200))
  content <- mock(tables)
  result <- with_mock(
    "httr::GET" = get,
    "httr::content" = content,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsListTables(connection)
  )
  expect_args(get, 1,
              handle = handle,
              path = "/tables",
              config = httr::add_headers("Authorization" = "Bearer token"))
  expect_equal(result, unlist(tables))
})

test_that("dsListResources retrieves resources", {
  resources <- list("a", "b")
  get <- mock(list(status_code = 200))
  content <- mock(resources)
  result <- with_mock(
    "httr::GET" = get,
    "httr::content" = content,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsListResources(connection)
  )
  expect_args(get, 1,
              handle = handle,
              path = "/resources",
              config = httr::add_headers("Authorization" = "Bearer token"))
  expect_equal(result, unlist(resources))
})

test_that("dsHasTable returns TRUE if table exists", {
  head <- mock(list(status_code = 200))
  with_mock(
    "httr::HEAD" = head,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    expect_true(dsHasTable(connection, "project/folder/name.parquet"))
  )
  expect_args(head, 1,
    handle = handle,
    path = "/tables/project/folder/name.parquet",
    config = httr::add_headers("Authorization" = "Bearer token")
  )
})

test_that("dsHasTable returns FALSE if table doesnot exist", {
  head <- mock(list(status_code = 404))
  with_mock(
    "httr::HEAD" = head,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    expect_false(dsHasTable(connection, "project/folder/name.parquet"))
  )
  expect_args(head, 1,
    handle = handle,
    path = "/tables/project/folder/name.parquet",
    config = httr::add_headers("Authorization" = "Bearer token")
  )
})

test_that("dsHasResource returns TRUE if resource exists", {
  head <- mock(list(status_code = 200))
  with_mock(
    "httr::HEAD" = head,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    expect_true(dsHasResource(connection, "project/folder/name"))
  )
  expect_args(head, 1,
    handle = handle,
    path = "/resources/project/folder/name",
    config = httr::add_headers("Authorization" = "Bearer token")
  )
})

test_that("dsHasResource returns FALSE if table doesnot exist", {
  head <- mock(list(status_code = 404))
  with_mock(
    "httr::HEAD" = head,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    expect_false(dsHasResource(connection, "project/folder/name"))
  )
  expect_args(head, 1,
    handle = handle,
    path = "/resources/project/folder/name",
    config = httr::add_headers("Authorization" = "Bearer token")
  )
})

test_that("dsIsAsync returns boolean list", {
  expect_equal(
    dsIsAsync(connection),
    list(
      aggregate = TRUE,
      assignTable = TRUE,
      assignResource = TRUE,
      assignExpr = TRUE
    )
  )
})

test_that("dsListSymbols returns symbols", {
  symbols <- list("a", "b")
  get <- mock(list(status_code = 200))
  content <- mock(symbols)
  result <- with_mock(
    "httr::GET" = get,
    "httr::content" = content,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsListSymbols(connection)
  )
  expect_args(get, 1,
              handle = handle,
              path = "/symbols",
              config = httr::add_headers("Authorization" = "Bearer token"))
  expect_equal(result, unlist(symbols))
})

test_that("dsRmSymbol removes symbol", {
  delete <- mock(list(status_code = 200))
  result <- with_mock(
    "httr::DELETE" = delete,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsRmSymbol(connection, "D")
  )
  expect_args(delete, 1,
              handle = handle,
              path = "/symbols/D",
              config = httr::add_headers("Authorization" = "Bearer token"))
})

test_that("dsAssignTable assigns table to symbol", {
  post <- mock(list(status_code = 200))
  result <- with_mock(
    "httr::POST" = post,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsAssignTable(connection, "D", "project/folder/name.parquet")
  )
  expect_args(post, 1,
    handle = handle,
    path = "/load-table",
    query = list(
      table = "project/folder/name.parquet",
      symbol = "D",
      async = TRUE
    ),
    config = httr::add_headers("Authorization" = "Bearer token")
  )
  expect_s4_class(result, "ArmadilloResult")
})

test_that("dsAssignTable allows variable selection", {
  post <- mock(list(status_code = 200))
  result <- with_mock(
    "httr::POST" = post,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsAssignTable(connection, "D",
      "project/folder/name.parquet",
      variables = c("foo", "bar")
    )
  )
  expect_args(post, 1,
    handle = handle,
    path = "/load-table",
    query = list(
      table = "project/folder/name.parquet",
      symbol = "D",
      async = TRUE,
      variables = "foo,bar"
    ),
    config = httr::add_headers("Authorization" = "Bearer token")
  )
  expect_s4_class(result, "ArmadilloResult")
})

test_that("dsAssignTable, when called synchronously, waits for result", {
  post <- mock(list(status_code = 200))
  retry <- mock(list(status_code = 200))
  httr_content <- mock(NULL)
  result <- with_mock(
    "httr::POST" = post,
    "httr::RETRY" = retry,
    "httr::content" = httr_content,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsAssignTable(connection, "D", "project/folder/name.parquet")
  )
  expect_args(post, 1,
    handle = handle,
    path = "/load-table",
    query = list(
      table = "project/folder/name.parquet", symbol = "D",
      async = TRUE
    ),
    config = httr::add_headers("Authorization" = "Bearer token")
  )
  expect_s4_class(result, "ArmadilloResult")
})

test_that("dsAssignResource assigns resource to symbol", {
  post <- mock(list(status_code = 200))
  result <- with_mock(
    "httr::POST" = post,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsAssignResource(connection, "D", "project/folder/name")
  )
  expect_args(post, 1,
    handle = handle,
    path = "/load-resource",
    query = list(
      resource = "project/folder/name",
      symbol = "D",
      async = TRUE
    ),
    config = httr::add_headers("Authorization" = "Bearer token")
  )
  expect_s4_class(result, "ArmadilloResult")
})

test_that("dsAssignResource, when called synchronously, waits for result", {
  post <- mock(list(status_code = 200))
  retry <- mock(list(status_code = 200))
  httr_content <- mock(NULL)
  result <- with_mock(
    "httr::POST" = post,
    "httr::RETRY" = retry,
    "httr::content" = httr_content,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsAssignResource(connection, "D", "project/folder/name")
  )
  expect_args(post, 1,
    handle = handle,
    path = "/load-resource",
    query = list(
      resource = "project/folder/name", symbol = "D",
      async = TRUE
    ),
    config = httr::add_headers("Authorization" = "Bearer token")
  )
  expect_s4_class(result, "ArmadilloResult")
})

test_that("dsListMethods returns assign methods", {
  methods <- list(list(
    name = "foo",
    "function" = "dsBase::foo",
    version = "6.0.0-03",
    package = "dsBase"
  ), list(
    name = "bar",
    "function" = "dsBase::bar",
    version = "6.0.0-03",
    package = "dsBase"
  ))
  get <- mock(list(status_code = 200))
  content <- mock(methods)

  result <- with_mock(
    "httr::GET" = get,
    "httr::content" = content,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsListMethods(connection, type = "assign")
  )

  expect_args(get, 1,
              handle = handle,
              path = "/methods/assign",
              config = httr::add_headers("Authorization" = "Bearer token"))

  expected <- tibble(
    name = c("foo", "bar"),
    value = c("dsBase::foo", "dsBase::bar"),
    version = c("6.0.0-03", "6.0.0-03"),
    package = c("dsBase", "dsBase"),
    type = "assign",
    class = "function"
  )

  expect_equivalent(result, expected)
})

test_that("dsListPackages extracts name and version from packages", {
  packages <- list(list(
    built = "3.6.3",
    libPath = "/usr/local/lib/R/site-library",
    name = "minqa",
    version = "1.2.4"
  ), list(
    built = "3.6.3",
    libPath = "/usr/local/lib/R/site-library",
    name = "RANN",
    version = "2.6.1"
  ))
  get <- mock(list(status_code = 200))
  content <- mock(packages)

  result <- with_mock(
    "httr::GET" = get,
    "httr::content" = content,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsListPackages(connection)
  )

  expect_equivalent(result, tibble(
    name = c("minqa", "RANN"),
    version = c("1.2.4", "2.6.1")
  ))
})

test_that("dsListWorkspaces lists workspaces", {
  workspaces <- list(
    list(
      name = "hello",
      size = 48L,
      ETag = "\"ca5d3a000723844e874b93a65c3888a1-1\"",
      lastModified = "2020-05-06T13:09:00.725Z"
    ),
    list(
      name = "world",
      size = 378L,
      ETag = "\"1f45d1a6f13adda1d05adf1f3da4c4ca-1\"",
      lastModified = "2020-05-06T13:09:07.617Z"
    )
  )
  get <- mock(list(status_code = 200))
  content <- mock(workspaces)

  result <- with_mock(
    "httr::GET" = get,
    "httr::content" = content,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsListWorkspaces(connection)
  )

  expect_equivalent(result, tibble(
    name = c("hello", "world"),
    size = c(48L, 378L),
    ETag = c(
      "\"ca5d3a000723844e874b93a65c3888a1-1\"",
      "\"1f45d1a6f13adda1d05adf1f3da4c4ca-1\""
    ),
    lastAccessDate = c(
      "2020-05-06T13:09:00.725Z",
      "2020-05-06T13:09:07.617Z"
    ),
    user = NA_character_
  ))
})

test_that("dsSaveWorkspace saves workspace", {
  post <- mock(list(status_code = 201))

  with_mock(
    "httr::POST" = post,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsSaveWorkspace(connection, "keepit")
  )

  expect_args(post, 1,
    handle = handle,
    path = "/workspaces/keepit",
    config = httr::add_headers("Authorization" = "Bearer token")
  )
})

test_that("dsRmWorkspace removes workspace", {
  delete <- mock(list(status_code = 204))

  with_mock(
    "httr::DELETE" = delete,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsRmWorkspace(connection, "keepit")
  )

  expect_args(delete, 1,
    handle = handle,
    path = "/workspaces/keepit",
    config = httr::add_headers("Authorization" = "Bearer token")
  )
})

test_that("dsAssignExpr assigns expression to symbol", {
  post <- mock(list(status_code = 200))
  result <- with_mock(
    "httr::POST" = post,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsAssignExpr(connection, "D", "ls()")
  )

  expect_args(post, 1,
    handle = handle,
    query = list(async = TRUE),
    path = "/symbols/D",
    body = "ls()",
    config = httr::add_headers(c("Content-Type" = "text/plain",
                                 "Authorization" = "Bearer token"))
  )
  expect_s4_class(result, "ArmadilloResult")
})

test_that("dsAssignExpr deparses function calls in expression", {
  post <- mock(list(status_code = 200))
  result <- with_mock(
    "httr::POST" = post,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsAssignExpr(connection, "D", call("ls"))
  )
  expect_args(post, 1,
    handle = handle,
    query = list(async = TRUE),
    path = "/symbols/D",
    body = "ls()",
    config = httr::add_headers(c("Content-Type" = "text/plain",
                                 "Authorization" = "Bearer token"))
  )
})

test_that("dsAssignExpr, when called synchronously, waits for result", {
  post <- mock(list(status_code = 200))
  retry <- mock(list(status_code = 200))
  httr_content <- mock(NULL)
  result <- with_mock(
    "httr::POST" = post,
    "httr::RETRY" = retry,
    "httr::content" = httr_content,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsAssignExpr(connection, "D", "ls()", async = FALSE)
  )
  expect_args(post, 1,
    handle = handle,
    query = list(async = FALSE),
    path = "/symbols/D",
    body = "ls()",
    config = httr::add_headers(c("Content-Type" = "text/plain",
                                 "Authorization" = "Bearer token"))
  )
  expect_args(retry, 1,
    verb = "GET",
    handle = handle,
    path = "/lastresult",
    terminate_on = c(200, 404, 401),
    config = httr::add_headers(c("Accept" = "application/octet-stream",
                                 "Authorization" = "Bearer token"))
  )
  expect_s4_class(result, "ArmadilloResult")
})

test_that("dsAggregate executes deparsed query", {
  post <- mock(list(status_code = 200))
  result <- with_mock(
    "httr::POST" = post,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsAggregate(connection, call("ls"))
  )

  expect_args(post, 1,
    handle = handle,
    query = list(async = TRUE),
    path = "/execute",
    body = "ls()",
    config = httr::add_headers(c("Content-Type" = "text/plain",
                                 "Authorization" = "Bearer token"))
  )
  expect_s4_class(result, "ArmadilloResult")
})

test_that("dsAssignExpr, when called synchronously, waits for result", {
  post <- mock(list(status_code = 200))
  retry <- mock(list(status_code = 200))
  httr_content <- mock(base::serialize("Hello World!", NULL))
  result <- with_mock(
    "httr::POST" = post,
    "httr::RETRY" = retry,
    "httr::content" = httr_content,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsAggregate(connection, "ls()", async = FALSE)
  )
  expect_args(post, 1,
    handle = handle,
    query = list(async = FALSE),
    path = "/execute",
    body = "ls()",
    config = httr::add_headers(c("Content-Type" = "text/plain",
                                 "Authorization" = "Bearer token"))
  )
  expect_args(retry, 1,
    verb = "GET",
    handle = handle,
    path = "/lastresult",
    terminate_on = c(200, 404, 401),
    config = httr::add_headers(c("Accept" = "application/octet-stream",
                                 "Authorization" = "Bearer token"))
  )
  expect_s4_class(result, "ArmadilloResult")
  expect_equal(dsFetch(result), "Hello World!")
})

test_that("dsAssignExpr handles error when called synchronously", {
  post <- mock(list(status_code = 500))
  get <- mock(list(status_code = 200))
  content <- mock(list(
    status = "FAILED",
    message = "Error"
  ))
  error <- expect_error(with_mock(
    "httr::POST" = post,
    "httr::GET" = get,
    "httr::content" = content,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsAggregate(connection, "ls()", async = FALSE)
  ), "Internal server error: Error")
})

test_that("dsGetInfo returns server info", {
  get <- mock(list(status_code = 200))
  server_info <- list(
    git = list(
      branch = "master",
      commit = list(
        id = "1a8ec4e",
        time = "2020-04-29T10:32:51Z"
      )
    ),
    build = list(
      java = list(version = "11"),
      version = "0.0.1-SNAPSHOT",
      artifact = "datashield",
      name = "datashield",
      time = "2020-04-29T13:18:59.833Z",
      group = "org.molgenis"
    )
  )
  httr_content <- mock(server_info)
  result <- with_mock(
    "httr::GET" = get,
    "httr::content" = httr_content,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsGetInfo(connection)
  )

  expected <- list(
    git = server_info$git,
    build = server_info$build,
    url = connection@handle$url,
    name = connection@name,
    cookies = connection@cookies
  )
  expect_equivalent(result, expected)
  expect_args(get, 1,
    handle = handle,
    path = "/actuator/info",
    config = httr::add_headers("Authorization" = "Bearer token")
  )
})

test_that("dsKeepAlive pings server info endpoint", {
  get <- mock(list(status_code = 200))
  result <- with_mock(
    "httr::GET" = get,
    "DSMolgenisArmadillo:::.refresh_token_safely" = function(conn) connection,
    dsKeepAlive(connection)
  )

  expect_args(get, 1,
    handle = handle,
    path = "/actuator/info",
    config = httr::add_headers("Authorization" = "Bearer token")
  )
})
