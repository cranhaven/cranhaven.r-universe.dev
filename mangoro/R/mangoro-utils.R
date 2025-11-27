# Null coalescing operator for convenience
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Pack a 32-bit integer to raw bytes (big-endian)
#'
#' @param x An integer value
#' @return A raw vector of length 4
#' @export
mangoro_pack_int32 <- function(x) {
  as.raw(c(
    (x %/% 16777216) %% 256,
    (x %/% 65536) %% 256,
    (x %/% 256) %% 256,
    x %% 256
  ))
}

#' Unpack a 32-bit integer from raw bytes (big-endian)
#'
#' @param bytes A raw vector of length 4
#' @return An integer value
#' @export
mangoro_unpack_int32 <- function(bytes) {
  val <- as.numeric(bytes[1]) *
    16777216 +
    as.numeric(bytes[2]) * 65536 +
    as.numeric(bytes[3]) * 256 +
    as.numeric(bytes[4])
  as.integer(val)
}

#' Create an RPC manifest request message
#'
#' @return A raw vector containing the manifest request
#' @export
mangoro_rpc_manifest_request <- function() {
  c(as.raw(0), mangoro_pack_int32(0), mangoro_pack_int32(0))
}

#' Create an RPC function call message
#'
#' @param func_name Name of the function to call
#' @param data Data frame or Arrow stream to send as arguments
#' @return A raw vector containing the RPC call message
#' @export
mangoro_rpc_call_message <- function(func_name, data) {
  tmp_arrow <- rawConnection(raw(0), "wb")
  nanoarrow::write_nanoarrow(data, tmp_arrow)
  arrow_bytes <- rawConnectionValue(tmp_arrow)
  close(tmp_arrow)

  name_bytes <- charToRaw(func_name)
  name_len <- length(name_bytes)

  c(
    as.raw(1),
    mangoro_pack_int32(name_len),
    name_bytes,
    mangoro_pack_int32(0),
    arrow_bytes
  )
}

#' Parse an RPC response message
#'
#' @param response Raw vector containing the RPC response
#' @return A list with components: type, func_name, error_msg, data
#' @export
mangoro_rpc_parse_response <- function(response) {
  msg_type <- as.integer(response[1])
  name_len <- mangoro_unpack_int32(response[2:5])

  # Handle NA from unpacking
  if (is.na(name_len)) {
    name_len <- 0L
  }

  func_name <- ""
  if (name_len > 0) {
    func_name <- rawToChar(response[6:(5 + name_len)])
  }

  error_start <- 6L + as.integer(name_len)
  error_len <- mangoro_unpack_int32(response[error_start:(error_start + 3L)])

  # Handle NA from unpacking
  if (is.na(error_len)) {
    error_len <- 0L
  }

  error_msg <- ""
  if (error_len > 0) {
    error_msg <- rawToChar(response[
      (error_start + 4L):(error_start + 3L + error_len)
    ])
  }

  data_start <- error_start + 4L + as.integer(error_len)

  # Handle case where there's no data (empty Arrow IPC or error response)
  if (data_start > length(response)) {
    data_bytes <- raw(0)
  } else {
    data_bytes <- response[data_start:length(response)]
  }

  list(
    type = msg_type,
    func_name = func_name,
    error_msg = error_msg,
    data = data_bytes
  )
}

#' Send a message with retries
#'
#' @param sock A nanonext socket
#' @param msg Message to send (raw vector)
#' @param max_attempts Maximum number of retry attempts (default 20)
#' @return The result from nanonext::send
#' @export
mangoro_rpc_send <- function(sock, msg, max_attempts = 20) {
  send_result <- nanonext::send(sock, msg, mode = "raw")
  attempt <- 1
  while (nanonext::is_error_value(send_result) && attempt < max_attempts) {
    Sys.sleep(1)
    send_result <- nanonext::send(sock, msg, mode = "raw")
    attempt <- attempt + 1
  }
  send_result
}

#' Receive a message with retries
#'
#' @param sock A nanonext socket
#' @param max_attempts Maximum number of retry attempts (default 20)
#' @return The received message as a raw vector
#' @export
mangoro_rpc_recv <- function(sock, max_attempts = 20) {
  response <- nanonext::recv(sock, mode = "raw")
  attempt <- 1
  while (nanonext::is_error_value(response) && attempt < max_attempts) {
    Sys.sleep(1)
    response <- nanonext::recv(sock, mode = "raw")
    attempt <- attempt + 1
  }
  response
}

#' Get the manifest of registered functions from an RPC server
#'
#' @param sock A nanonext socket connected to the RPC server
#' @return A list of function signatures
#' @export
mangoro_rpc_get_manifest <- function(sock) {
  msg <- mangoro_rpc_manifest_request()
  mangoro_rpc_send(sock, msg)
  response <- mangoro_rpc_recv(sock)
  parsed <- mangoro_rpc_parse_response(response)

  if (parsed$type == 3) {
    stop("RPC error: ", parsed$error_msg)
  }

  jsonlite::fromJSON(rawToChar(parsed$data))
}

#' Call a remote function via RPC
#'
#' @param sock A nanonext socket connected to the RPC server
#' @param func_name Name of the function to call
#' @param data Data frame or Arrow stream to send as arguments
#' @return The result from nanoarrow::read_nanoarrow (typically a nanoarrow_array_stream)
#' @export
mangoro_rpc_call <- function(sock, func_name, data) {
  msg <- mangoro_rpc_call_message(func_name, data)
  mangoro_rpc_send(sock, msg)
  response <- mangoro_rpc_recv(sock)
  parsed <- mangoro_rpc_parse_response(response)

  if (parsed$type == 3) {
    stop("RPC error: ", parsed$error_msg)
  }

  nanoarrow::read_nanoarrow(parsed$data)
}

#' Start an HTTP file server via RPC
#'
#' @param sock A nanonext socket connected to the HTTP server controller
#' @param addr Address to bind server to (e.g., "127.0.0.1:8080")
#' @param dir Directory to serve (default: current directory)
#' @param prefix URL prefix for the server (default: "/")
#' @param cors Enable CORS headers (default: FALSE)
#' @param coop Enable Cross-Origin-Opener-Policy (default: FALSE)
#' @param tls Enable TLS (default: FALSE)
#' @param cert Path to TLS certificate file (required if tls = TRUE)
#' @param key Path to TLS key file (required if tls = TRUE)
#' @param silent Suppress server logs (default: FALSE)
#' @return List with status and message
#' @export
mangoro_http_start <- function(
  sock,
  addr,
  dir = ".",
  prefix = "/",
  cors = FALSE,
  coop = FALSE,
  tls = FALSE,
  cert = NULL,
  key = NULL,
  silent = FALSE
) {
  input_df <- data.frame(
    addr = addr,
    dir = dir,
    prefix = prefix,
    cors = cors,
    coop = coop,
    tls = tls,
    cert = cert %||% "",
    key = key %||% "",
    silent = silent,
    stringsAsFactors = FALSE
  )

  result <- mangoro_rpc_call(sock, "startServer", input_df)
  as.data.frame(result)
}

#' Stop the HTTP file server via RPC
#'
#' @param sock A nanonext socket connected to the HTTP server controller
#' @return List with status and message
#' @export
mangoro_http_stop <- function(sock) {
  # Create minimal data frame with dummy column for no-argument call
  input_df <- data.frame(dummy = integer(0))
  result <- mangoro_rpc_call(sock, "stopServer", input_df)
  as.data.frame(result)
}

#' Get HTTP server status via RPC
#'
#' @param sock A nanonext socket connected to the HTTP server controller
#' @return List with status and message
#' @export
mangoro_http_status <- function(sock) {
  # Create minimal data frame with dummy column for no-argument call
  input_df <- data.frame(dummy = integer(0))
  result <- mangoro_rpc_call(sock, "serverStatus", input_df)
  as.data.frame(result)
}
