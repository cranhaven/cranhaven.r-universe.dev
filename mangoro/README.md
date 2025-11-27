
[![mangoro status
badge](https://sounkou-bioinfo.r-universe.dev/mangoro/badges/version)](https://sounkou-bioinfo.r-universe.dev/mangoro)

# mangoro <a href="https://sounkou-bioinfo.github.io/mangoro/"><img src="man/figures/logo.svg" alt="" width="180" align="right"/></a>

R/Go IPC with Nanomsg Next Gen.

## What is mangoro?

Beside being the way mangos is said in
[bambara](https://bm.wikipedia.org/wiki/Mangoro) (derived from portugese
as it happens), in this package we vendor the
[mangos/v3](https://github.com/nanomsg/mangos) and
[arrow-go](https://github.com/apache/arrow-go) Go packages for IPC
between R and Go processes using the `nanonext` and `nanoarrow` R
packages on the R side. The package provides helper functions to build
Go binaries that use mangos and Arrow for IPC. This is a basic setup
that can be used as a starting point for more complex R/Go IPC
applications. In our opinion, this approach avoids the complexities and
limitations of cgo’s c-shared mode, which can lead to issues with
loading multiple Go runtimes in the same R session as discussed in this
R-package-devel mailing list thread: [CRAN Policy on Go using
Packages](https://hypatia.math.ethz.ch/pipermail/r-package-devel/2025q4/012067.html).

## Installation

``` r
# via r-universe
install.packages('mangoro', repos = c('https://sounkou-bioinfo.r-universe.dev', 'https://cloud.r-project.org'))
# For CRAN release (not available yet)
install.packages('mangoro')
```

## On-the-fly Go compilation and echo

Compile some go code on-the-fly from R using the `mangoro_go_build()`
function. This uses the vendored go code in
[inst/go/vendor](inst/go/vendor)

``` r

library(nanonext)
library(processx)
library(nanoarrow)
library(mangoro)

# vendored mangos version
get_mangos_version()
#> [1] "v3.4.3-0.20250905144305-2c434adf4860"
go_echo_code <- paste(
  'package main',
  'import (',
  '  "os"',
  '  "go.nanomsg.org/mangos/v3/protocol/rep"',
  '  _ "go.nanomsg.org/mangos/v3/transport/ipc"',
  ')',
  'func main() {',
  '  url := os.Args[1]',
  '  sock, _ := rep.NewSocket()',
  '  sock.Listen(url)',
  '  for {',
  '    msg, _ := sock.Recv()',
  '    newMsg := append(msg, []byte(" [echoed by Go]")...)',
  '    sock.Send(newMsg)',
  '  }',
  '}',
  sep = "\n"
)

tmp_go <- tempfile(fileext = ".go")
writeLines(go_echo_code, tmp_go)

tmp_bin <- tempfile()
mangoro_go_build(tmp_go, tmp_bin)
#> [1] "GOMAXPROCS=1 /usr/lib/go-1.22/bin/go 'build' '-mod=vendor' '-o' '/tmp/RtmpHIQD9Z/file1b015e24cbd2cd' '/tmp/RtmpHIQD9Z/file1b015e6fb6f0c9.go'"
#> [1] "/tmp/RtmpHIQD9Z/file1b015e24cbd2cd"
```

create IPC path and send/receive message

``` r
ipc_url <- create_ipc_path()
echo_proc <- processx::process$new(tmp_bin, args = ipc_url)
Sys.sleep(1)
echo_proc$is_alive()
#> [1] TRUE
sock <- nanonext::socket("req", dial = ipc_url)
msg <- charToRaw("hello from R")

max_attempts <- 20
send_result <- nanonext::send(sock, msg, mode = "raw")
attempt <- 1
while (nanonext::is_error_value(send_result) && attempt < max_attempts) {
  Sys.sleep(1)
  send_result <- nanonext::send(sock, msg, mode = "raw")
  attempt <- attempt + 1
}

response <- nanonext::recv(sock, mode = "raw")
attempt <- 1
while (nanonext::is_error_value(response) && attempt < max_attempts) {
  Sys.sleep(1)
  response <- nanonext::recv(sock, mode = "raw")
  attempt <- attempt + 1
}

rawToChar(response)
#> [1] "hello from R [echoed by Go]"
close(sock)
echo_proc$kill()
#> [1] TRUE
```

## Arrow IPC with nanoarrow for serialization

Compile go code this time that uses Arrow IPC for (de)serialization
between R and Go.

``` r

cfg <- nanonext::serial_config(
  "ArrowTabular",
  nanoarrow::write_nanoarrow,
  nanoarrow::read_nanoarrow
)
ipc_url <- create_ipc_path()
go_code <- '
package main
import (
  "os"
  "bytes"
  "fmt"
  "go.nanomsg.org/mangos/v3/protocol/rep"
  _ "go.nanomsg.org/mangos/v3/transport/ipc"
  "github.com/apache/arrow/go/v18/arrow/ipc"
  "github.com/apache/arrow/go/v18/arrow/memory"
)
func main() {
  url := os.Args[1]
  sock, _ := rep.NewSocket()
  sock.Listen(url)
  for {
    msg, _ := sock.Recv()
    reader, err := ipc.NewReader(bytes.NewReader(msg), ipc.WithAllocator(memory.DefaultAllocator))
    if err != nil {
      fmt.Println("Arrow IPC error:", err)
      continue
    }
    var buf bytes.Buffer
    writer := ipc.NewWriter(&buf, ipc.WithSchema(reader.Schema()))
    for reader.Next() {
      rec := reader.Record()
      fmt.Println(rec)
      if err := writer.Write(rec); err != nil {
        fmt.Println("Arrow IPC write error:", err)
      }
      rec.Release()
    }
    if err := writer.Close(); err != nil {
      fmt.Println("Arrow IPC writer close error:", err)
    }
    reader.Release()
    sock.Send(buf.Bytes())
  }
}
'
tmp_go <- tempfile(fileext = ".go")
writeLines(go_code, tmp_go)
tmp_bin <- tempfile()
mangoro_go_build(tmp_go, tmp_bin)
#> [1] "GOMAXPROCS=1 /usr/lib/go-1.22/bin/go 'build' '-mod=vendor' '-o' '/tmp/RtmpHIQD9Z/file1b015e6dbf9c59' '/tmp/RtmpHIQD9Z/file1b015e47fbb389.go'"
#> [1] "/tmp/RtmpHIQD9Z/file1b015e6dbf9c59"

echo_proc <- processx::process$new(tmp_bin, args = ipc_url, stdout = "|", stderr = "|"  )
Sys.sleep(3)
```

Configure the socket and send/receive an Arrow IPC data. Note that we
use a loop with retries to handle potential timing issues when the Go
echo server is not yet ready to receive messages.

``` r
echo_proc$is_alive()
#> [1] TRUE
sock <- nanonext::socket("req", dial = ipc_url)
nanonext::opt(sock, "serial") <- cfg

example_stream <- nanoarrow::example_ipc_stream()
max_attempts <- 20
send_result <- nanonext::send(sock, example_stream, mode = "raw")
attempt <- 1
while (nanonext::is_error_value(send_result) && attempt < max_attempts) {
  Sys.sleep(1)
  send_result <- nanonext::send(sock, example_stream, mode = "raw")
  attempt <- attempt + 1
}
send_result
#> [1] 0
echo_proc$is_alive()
#> [1] TRUE
Sys.sleep(1)
received <- nanonext::recv(sock, mode = "serial")
#> Warning: received data could not be unserialized
attempt <- 1
while (nanonext::is_error_value(received) && attempt < max_attempts) {
  Sys.sleep(1)
  received <- nanonext::recv(sock, mode = "serial")
  attempt <- attempt + 1
}
sent_df <- as.data.frame(read_nanoarrow(example_stream))
received_df <- as.data.frame(read_nanoarrow(received))
sent_df
#>   some_col
#> 1        0
#> 2        1
#> 3        2
received_df
#>   some_col
#> 1        0
#> 2        1
#> 3        2
identical(sent_df, received_df)
#> [1] TRUE
close(sock)
echo_proc$kill()
#> [1] TRUE
```

## Simple RPC with Function Registration

The package includes `rgoipc`, a Go package for building RPC servers
with function registration. Functions are registered in the Go main
application and called by R.

### RPC Message Structure

The RPC protocol wraps Arrow IPC data in a simple envelope:

    [type:1byte][name_len:4bytes][name][error_len:4bytes][error][arrow_ipc_data]

- **Type**: Message type (0=manifest, 1=call, 2=result, 3=error)
- **Name length + Name**: Function name (empty for manifest requests)
- **Error length + Error**: Error message (empty on success)
- **Arrow IPC data**:
  - For **manifest response**: JSON bytes describing available functions
  - For **function calls**: Arrow IPC stream containing input arguments
    (typically RecordBatch)
  - For **function results**: Arrow IPC stream containing output
    (typically RecordBatch)

Both the input and output data are serialized using Arrow IPC format.
The Go server receives Arrow IPC (as `arrow.Record`), processes it, and
returns Arrow IPC. On the R side, you can work with data frames,
nanoarrow streams, or any Arrow-compatible structure. The thin RPC
envelope only adds metadata (function name, error handling) around the
Arrow data.

**Key concept**: R data.frames naturally map to Arrow RecordBatch
(tabular data with multiple columns). Each column in a data.frame
becomes an Arrow Array. Functions receive and return `arrow.Record`
objects, which represent this tabular structure.

``` r

rpc_server_path <- file.path(system.file("go", package = "mangoro"), "cmd", "rpc-example", "main.go")
rpc_bin <- tempfile()
mangoro_go_build(rpc_server_path, rpc_bin)
#> [1] "GOMAXPROCS=1 /usr/lib/go-1.22/bin/go 'build' '-mod=vendor' '-o' '/tmp/RtmpHIQD9Z/file1b015e51db764f' '/usr/local/lib/R/site-library/mangoro/go/cmd/rpc-example/main.go'"
#> [1] "/tmp/RtmpHIQD9Z/file1b015e51db764f"

ipc_url <- create_ipc_path()
rpc_proc <- processx::process$new(rpc_bin, args = ipc_url, stdout = "|", stderr = "|")
Sys.sleep(2)
rpc_proc$is_alive()
#> [1] TRUE
```

Request the manifest of registered functions:

``` r
sock <- nanonext::socket("req", dial = ipc_url)
manifest <- mangoro_rpc_get_manifest(sock)
manifest
#> $add
#> $add$Args
#>   Name Type.Type Type.Nullable Type.StructDef Type.ListSchema Optional Default
#> 1    x   float64          TRUE             NA              NA    FALSE      NA
#> 2    y   float64          TRUE             NA              NA    FALSE      NA
#> 
#> $add$ReturnType
#> $add$ReturnType$Type
#> [1] "float64"
#> 
#> $add$ReturnType$Nullable
#> [1] TRUE
#> 
#> $add$ReturnType$StructDef
#> NULL
#> 
#> $add$ReturnType$ListSchema
#> NULL
#> 
#> 
#> $add$Vectorized
#> [1] TRUE
#> 
#> $add$Metadata
#> $add$Metadata$description
#> [1] "Add two numeric vectors"
#> 
#> 
#> 
#> $echoString
#> $echoString$Args
#>   Name Type.Type Type.Nullable Type.StructDef Type.ListSchema Optional Default
#> 1    s    string          TRUE             NA              NA    FALSE      NA
#> 
#> $echoString$ReturnType
#> $echoString$ReturnType$Type
#> [1] "string"
#> 
#> $echoString$ReturnType$Nullable
#> [1] TRUE
#> 
#> $echoString$ReturnType$StructDef
#> NULL
#> 
#> $echoString$ReturnType$ListSchema
#> NULL
#> 
#> 
#> $echoString$Vectorized
#> [1] TRUE
#> 
#> $echoString$Metadata
#> $echoString$Metadata$description
#> [1] "Echo back a string vector"
#> 
#> 
#> 
#> $echoStruct
#> $echoStruct$Args
#>     Name Type.Type Type.Nullable
#> 1 person    struct         FALSE
#>                                                      Type.Fields
#> 1 name, age, string, int32, FALSE, FALSE, NA, NA, NA, NA, NA, NA
#>   Type.ListSchema Optional Default
#> 1              NA    FALSE      NA
#> 
#> $echoStruct$ReturnType
#> $echoStruct$ReturnType$Type
#> [1] "struct"
#> 
#> $echoStruct$ReturnType$Nullable
#> [1] FALSE
#> 
#> $echoStruct$ReturnType$StructDef
#> $echoStruct$ReturnType$StructDef$Fields
#>   Name Type.Type Type.Nullable Type.StructDef Type.ListSchema Metadata
#> 1 name    string         FALSE             NA              NA       NA
#> 2  age     int32         FALSE             NA              NA       NA
#> 
#> 
#> $echoStruct$ReturnType$ListSchema
#> NULL
#> 
#> 
#> $echoStruct$Vectorized
#> [1] TRUE
#> 
#> $echoStruct$Metadata
#> $echoStruct$Metadata$description
#> [1] "Echo back a struct column (nested data)"
#> 
#> 
#> 
#> $transposeMatrix
#> $transposeMatrix$Args
#> list()
#> 
#> $transposeMatrix$ReturnType
#> $transposeMatrix$ReturnType$Type
#> [1] "struct"
#> 
#> $transposeMatrix$ReturnType$Nullable
#> [1] FALSE
#> 
#> $transposeMatrix$ReturnType$StructDef
#> $transposeMatrix$ReturnType$StructDef$Fields
#> list()
#> 
#> 
#> $transposeMatrix$ReturnType$ListSchema
#> NULL
#> 
#> 
#> $transposeMatrix$Vectorized
#> [1] FALSE
#> 
#> $transposeMatrix$Metadata
#> $transposeMatrix$Metadata$description
#> [1] "Transpose a matrix (columns <-> rows)"
close(sock)
```

Call the `add` function with Arrow IPC data:

``` r
sock <- nanonext::socket("req", dial = ipc_url)

input_df <- data.frame(x = c(1.5, 2.5, 3.5, NA), y = c(0.5, 1.5, 2.5, 4.5))
result <- mangoro_rpc_call(sock, "add", input_df)
result_df <- as.data.frame(result)

input_df
#>     x   y
#> 1 1.5 0.5
#> 2 2.5 1.5
#> 3 3.5 2.5
#> 4  NA 4.5
result_df
#>   result
#> 1      2
#> 2      4
#> 3      6
#> 4     NA
input_df$x + input_df$y
#> [1]  2  4  6 NA

close(sock)
```

Call the `echoString` function to test string handling:

``` r
sock <- nanonext::socket("req", dial = ipc_url)

input_df <- data.frame(s = c("hello", "world", NA, "mangoro"))
result <- mangoro_rpc_call(sock, "echoString", input_df)
result_df <- as.data.frame(result)

input_df
#>         s
#> 1   hello
#> 2   world
#> 3    <NA>
#> 4 mangoro
result_df
#>    result
#> 1   hello
#> 2   world
#> 3    <NA>
#> 4 mangoro
identical(input_df$s, result_df$result)
#> [1] TRUE

close(sock)
```

Call the `transposeMatrix` function to demonstrate matrix handling. In
R, we send a matrix as a data.frame (each column is a column), and Go
transposes it:

``` r
sock <- nanonext::socket("req", dial = ipc_url)

# Create a 3x4 matrix and convert to data.frame for Arrow IPC
mat <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), nrow = 3, ncol = 4)
"Input matrix (3x4):"
#> [1] "Input matrix (3x4):"
mat
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    4    7   10
#> [2,]    2    5    8   11
#> [3,]    3    6    9   12

# Convert to data.frame - each column becomes a column
input_df <- as.data.frame(mat)
colnames(input_df) <- paste0("V", 1:ncol(mat))

# Transpose via RPC
result <- mangoro_rpc_call(sock, "transposeMatrix", input_df)
result_df <- as.data.frame(result)

# Compare values (ignore dimnames)
all.equal(as.matrix(result_df), t(mat), check.attributes = FALSE)
#> [1] TRUE

close(sock)
```

Call the `echoStruct` function to demonstrate nested/struct column
handling

``` r
sock <- nanonext::socket("req", dial = ipc_url)

# Create a struct column: a single column named 'person' 
# where each element is a struct with 'name' and 'age' fields
person_df <- data.frame(
    name = c("Alice", "Bob", "Charlie"),
    age = c(30L, 25L, 35L),
    stringsAsFactors = FALSE
)

# Wrap in a data frame with I() to create a nested structure
# I() preserves person_df as a single nested column (Arrow struct type)
# Without I(), R would flatten it into person.name and person.age columns
input_df <- data.frame(person = I(person_df))

# Call the RPC function
result <- mangoro_rpc_call(sock, "echoStruct", input_df)
result_df <- as.data.frame(result)

str(input_df)
#> 'data.frame':    3 obs. of  1 variable:
#>  $ person:Classes 'AsIs' and 'data.frame':   3 obs. of  2 variables:
#>   ..$ name: chr  "Alice" "Bob" "Charlie"
#>   ..$ age : int  30 25 35

str(result_df)
#> 'data.frame':    3 obs. of  1 variable:
#>  $ person:'data.frame':  3 obs. of  2 variables:
#>   ..$ name: chr  "Alice" "Bob" "Charlie"
#>   ..$ age : int  30 25 35

result_df
#>   person.name person.age
#> 1       Alice         30
#> 2         Bob         25
#> 3     Charlie         35

# Nested data roundtrip successful (AsIs class not preserved)
all.equal(input_df$person, result_df$person, check.attributes = FALSE)
#> [1] TRUE

close(sock)
rpc_proc$kill()
#> [1] TRUE
```

## HTTP(S) File Server with RPC Control

The package includes an HTTP file server that can be controlled via RPC,
demonstrating a slighly more complex use case.

``` r
# Build the HTTP server controller
http_server_path <- file.path(system.file("go", package = "mangoro"), "cmd", "http-server", "main.go")
http_bin <- tempfile()
mangoro_go_build(http_server_path, http_bin, gomaxprocs = 4)
#> [1] "GOMAXPROCS=4 /usr/lib/go-1.22/bin/go 'build' '-mod=vendor' '-o' '/tmp/RtmpHIQD9Z/file1b015e653805ea' '/usr/local/lib/R/site-library/mangoro/go/cmd/http-server/main.go'"
#> [1] "/tmp/RtmpHIQD9Z/file1b015e653805ea"

# Start the RPC controller (not the HTTP server itself yet)
ipc_url <- create_ipc_path()
http_ctl_proc <- processx::process$new(http_bin, args = ipc_url, stdout = "|", stderr = "|")
Sys.sleep(2)
http_ctl_proc$is_alive()
#> [1] TRUE
http_ctl_proc$read_output_lines()
#> [1] "Registered functions: [startServer stopServer serverStatus]"                             
#> [2] "HTTP server controller listening on ipc:///tmp/RtmpHIQD9Z/mangoro-echo1b015e3dd417b2.ipc"
```

Control the HTTP server via RPC:

``` r
sock <- nanonext::socket("req", dial = ipc_url)

# Get server status (should be stopped initially)
status <- mangoro_http_status(sock)
status
#>   status message
#> 1 status stopped

# Start HTTP server on port 8080 serving current directory
result <- mangoro_http_start(sock, "127.0.0.1:8080", dir = ".", cors = TRUE)
result
#>   status                               message
#> 1     ok HTTP server started on 127.0.0.1:8080

# Check status again
status <- mangoro_http_status(sock)
status
#>   status                   message
#> 1 status running at 127.0.0.1:8080

# Test accessing the HTTP server
readLines("http://127.0.0.1:8080/", n = 3, warn = FALSE)
#> [1] "<pre>"                                      
#> [2] "<a href=\"..Rcheck/\">..Rcheck/</a>"        
#> [3] "<a href=\".Rbuildignore\">.Rbuildignore</a>"

# Stop the server
result <- mangoro_http_stop(sock)
result
#>   status             message
#> 1     ok HTTP server stopped

# Verify it stopped
status <- mangoro_http_status(sock)
status
#>   status message
#> 1 status stopped

# Verify it stopped
status <- mangoro_http_status(sock)
status
#>   status message
#> 1 status stopped
```

### HTTPS/TLS Support

The HTTP server supports HTTPS with TLS certificates. Start an HTTPS
server by providing certificate and key files:

``` r
# Start HTTPS server with certificates (reusing the existing server controller)
result <- mangoro_http_start(
  sock,
  addr = "127.0.0.1:8443",
  dir = ".",
  tls = TRUE,
  cert = ".certs_test/cert.pem" |> normalizePath(),
  key = ".certs_test/key.pem" |> normalizePath(),
  cors = TRUE
)
result
#>   status                               message
#> 1     ok HTTP server started on 127.0.0.1:8443

# Check status
status <- mangoro_http_status(sock)
status
#>   status                   message
#> 1 status running at 127.0.0.1:8443

# Download a file from HTTPS server using R's download.file
temp_file <- tempfile()
download.file("https://127.0.0.1:8443/.Rbuildignore", 
              destfile = temp_file,
              method = "curl",
              extra = "-k",
              quiet = TRUE)
# Read the downloaded file
readLines(temp_file, n = 3)
#> [1] "LICENSE"            "^\\.certs_test/.*$" "^misc/.*$"
# Stop the HTTPS server
result <- mangoro_http_stop(sock)
result
#>   status             message
#> 1     ok HTTP server stopped
http_ctl_proc$read_output_lines()
#> [1] "[mangoro server] 2025/11/17 19:13:25 Starting HTTP server on 127.0.0.1:8080 serving /root/mangoro at /" 
#> [2] "[mangoro server] 2025/11/17 19:13:27 GET / 127.0.0.1:34570 66.696µs"                                    
#> [3] "[mangoro server] 2025/11/17 19:13:27 HTTP server stopped"                                               
#> [4] "[mangoro server] 2025/11/17 19:13:30 Starting HTTPS server on 127.0.0.1:8443 serving /root/mangoro at /"
#> [5] "[mangoro server] 2025/11/17 19:13:32 GET /.Rbuildignore 127.0.0.1:59298 1.586772ms"                     
#> [6] "[mangoro server] 2025/11/17 19:13:32 HTTP server stopped"
close(sock)
http_ctl_proc$kill()
#> [1] TRUE
```

The `rgoipc` package provides the interfaces for type-safe function
registration with Arrow schema validation. See
[inst/go/pkg/rgoipc](inst/go/pkg/rgoipc) for the Go package
implementation and [inst/go/cmd/rpc-example](inst/go/cmd/rpc-example)
and [inst/go/cmd/http-server](inst/go/cmd/http-server) for complete
server examples.

## Some issues to investigate

There is some conversion overhead now when sending data to go processes
because we are sending the arrow data as bytes after convertion of the R
objects using nanoarrow. Moroever for some reason, we cannot send
directly matrices since we have some cryptic `arrow` package
requirement.

## LLM Usage Disclosure

Code and documentation in this project have been generated with the
assistance of the github Copilot LLM tools. While we have reviewed and
edited the generated content, we acknowledge that LLM tools were used in
the creation process and accordingly (since these models are trained on
GPL code and other commons + proprietary software license is fake
anyway) the code is released under GPL-3. So if you use this code in any
way, you must comply with the GPL-3 license.
