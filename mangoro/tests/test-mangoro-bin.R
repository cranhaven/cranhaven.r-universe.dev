library(nanonext)
library(processx)
library(nanoarrow)
library(mangoro)

# skip test if Sys.which cannot find go
if (nchar(Sys.which("go")) == 0) {
  quit(status = 0)
}
# vendored mangos version
get_mangos_version()
go_echo_code <- paste(
  "package main",
  "import (",
  '  "fmt"',
  '  "os"',
  '  "go.nanomsg.org/mangos/v3/protocol/rep"',
  '  _ "go.nanomsg.org/mangos/v3/transport/ipc"',
  ")",
  "func main() {",
  "  url := os.Args[1]",
  "  sock, err := rep.NewSocket()",
  "  if err != nil { fmt.Println(\"NewSocket error:\", err); os.Exit(1) }",
  "  if err := sock.Listen(url); err != nil { fmt.Println(\"Listen error:\", err); os.Exit(1) }",
  "  for {",
  "    msg, err := sock.Recv()",
  "    if err != nil { fmt.Println(\"Recv error:\", err); break }",
  '    newMsg := append(msg, []byte(" [echoed by Go]")...)',
  "    if err := sock.Send(newMsg); err != nil { fmt.Println(\"Send error:\", err); break }",
  "  }",
  "}",
  sep = "\n"
)

tmp_go <- tempfile(fileext = ".go")
writeLines(go_echo_code, tmp_go)

tmp_bin <- if (.Platform$OS.type == "windows") {
  tempfile(fileext = ".exe")
} else {
  tempfile()
}
mangoro_go_build(tmp_go, tmp_bin)

ipc_url <- create_ipc_path()
ipc_url
echo_proc <- processx::process$new(
  tmp_bin,
  args = ipc_url,
  stdout = "|",
  stderr = "|"
)
on.exit(message(echo_proc$read_output()))
on.exit(message(echo_proc$read_error()), add = TRUE)
on.exit(echo_proc$kill(), add = TRUE)
Sys.sleep(4)
if (!echo_proc$is_alive()) {
  message("Go process output:\n")
  message(echo_proc$read_output())
  message("Go process error:\n")
  message(echo_proc$read_error())
  break
}
sock <- nanonext::socket("req", dial = ipc_url)
msg <- charToRaw("hello from R")
send_result <- nanonext::send(sock, msg, mode = "raw")
# Retry send up to 35 times if error
max_attempts <- 35
attempt <- 1
while (nanonext::is_error_value(send_result) && attempt < max_attempts) {
  Sys.sleep(1)
  send_result <- nanonext::send(sock, msg, mode = "raw")
  attempt <- attempt + 1
  message(echo_proc$is_alive())
  if (!echo_proc$is_alive()) {
    message("Go process output:\n")
    message(echo_proc$read_output())
    message("Go process error:\n")
    message(echo_proc$read_error())
    break
  }
}
message(send_result)
# Retry recv up to 35 times if error
rep <- nanonext::recv(sock, mode = "raw")
attempt <- 1
while (nanonext::is_error_value(rep) && attempt < max_attempts) {
  Sys.sleep(1)
  rep <- nanonext::recv(sock, mode = "raw")
  attempt <- attempt + 1
}
Sys.sleep(3)
message(rawToChar(rep))
close(sock)
