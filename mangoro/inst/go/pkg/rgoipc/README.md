# rgoipc - R/Go IPC Package

The `rgoipc` package provides a framework for building RPC server## Supported Types

The following types are supported for function arguments and returns:

| rgoipc Type | Arrow Type | Usage |
|-------------|------------|-------|
| TypeInt32   | int32      | Single column of integers |
| TypeFloat64 | float64    | Single column of numeric values |
| TypeString  | string     | Single column of character values |
| TypeBool    | bool       | Single column of logical values |
| TypeList    | list\<T\>  | Variable-length arrays (rare) |
| TypeStruct  | struct     | Named fields - used for RecordBatch schema |

**Important**: 
- **Primary data exchange**: R data.frames ↔ Arrow RecordBatch (represented as `arrow.Record` in Go)
- **Function handlers** always work with `arrow.Record` (tabular data), not individual structs
- **Multi-column returns**: Just create an `arrow.Record` with multiple columns - no need for TypeStruct
- **R integer limitation**: R integers are 32-bit only. Use TypeInt32 for R integers, TypeFloat64 for larger values.

### When to Use TypeStruct vs Multi-Column Records

**TypeStruct (rare)** - For a **single column** containing nested records:
```r
# In R: One column with nested structure (list column)
data <- arrow_table(
  person = struct_array(
    name = c("Alice", "Bob"),
    age = c(30L, 25L)
  )
)
# Result: 1 column named "person", each row is a struct with name/age fields
```

**Multi-column Record (common)** - For regular tabular data:
```r
# In R: Multiple separate columns (normal data.frame)
data <- data.frame(
  name = c("Alice", "Bob"),
  age = c(30, 25)
)
# Result: 2 columns named "name" and "age"
```

See `misc/test_struct_column.R` for a working example of struct columns.an be called from R using Arrow IPC serialization and nanomsg messaging.

## Overview

This package enables:
- Type-safe function registration with Arrow schema validation
- Function discovery (Go servers broadcast available functions to R)
- Minimal overhead RPC protocol for function calls
- Vectorized operations with batch processing support

## Architecture

```
R Process                    Go Process
---------                    ----------
nanonext + nanoarrow    <->  mangos + arrow-go + rgoipc
```

The communication is entirely through separate processes connected via nanomsg IPC sockets. No CGo is used in the Go process.

## Core Types

### Registry

The `Registry` holds registered functions that can be called from R:

```go
registry := rgoipc.NewRegistry()
```

### Function Registration

Register a function with type signatures:

```go
err := registry.Register("add", addHandler, rgoipc.FunctionSignature{
    Args: []rgoipc.ArgSpec{
        {Name: "x", Type: rgoipc.TypeSpec{Type: rgoipc.TypeFloat64, Nullable: true}},
        {Name: "y", Type: rgoipc.TypeSpec{Type: rgoipc.TypeFloat64, Nullable: true}},
    },
    ReturnType: rgoipc.TypeSpec{Type: rgoipc.TypeFloat64, Nullable: true},
    Vectorized: true,
})
```

### Function Handler

Implement the `FunctionHandler` interface:

```go
func addHandler(input arrow.Record) (arrow.Record, error) {
    x := input.Column(0).(*array.Float64)
    y := input.Column(1).(*array.Float64)
    
    // Build result
    builder := array.NewFloat64Builder(memory.NewGoAllocator())
    defer builder.Release()
    
    for i := 0; i < x.Len(); i++ {
        if x.IsNull(i) || y.IsNull(i) {
            builder.AppendNull()
        } else {
            builder.Append(x.Value(i) + y.Value(i))
        }
    }
    
    result := builder.NewArray()
    defer result.Release()
    
    schema := arrow.NewSchema([]arrow.Field{
        {Name: "result", Type: arrow.PrimitiveTypes.Float64}
    }, nil)
    
    return array.NewRecord(schema, []arrow.Array{result}, int64(result.Len())), nil
}
```

## RPC Protocol

### Message Types

- `MsgTypeManifest` (0): Request function list from Go → R
- `MsgTypeCall` (1): Call function from R → Go
- `MsgTypeResult` (2): Return result from Go → R
- `MsgTypeError` (3): Return error from Go → R

### Wire Format

```
[type:1byte][name_len:4bytes][name][error_len:4bytes][error][arrow_ipc_data]
```

## Supported Types

The following Arrow types map to R types for **individual columns**:

| rgoipc Type | Arrow Type | R Type (column) | Notes |
|-------------|------------|-----------------|-------|
| TypeInt32   | int32      | integer         | R's integer is 32-bit only |
| TypeFloat64 | float64    | numeric         | Use for large numbers and decimals |
| TypeString  | string     | character       | Text data |
| TypeBool    | bool       | logical         | TRUE/FALSE values |
| TypeList    | list<T>    | list            | Variable-length arrays per row (advanced) |
| TypeStruct  | struct     | named list      | Single row with named fields (advanced) |

**Important**: R's integer type is 32-bit only. Arrow int64 values will be converted to R's numeric (double) type, not integer. Use TypeInt32 for R integers and TypeFloat64 for larger numeric values.

### Data Exchange Model

The **primary data exchange** format is **Arrow RecordBatch ↔ R data.frame**:

- **From R to Go**: A data.frame is serialized as an Arrow RecordBatch where each column becomes an Arrow Array
- **From Go to R**: An `arrow.Record` (tabular data) is deserialized into an R data.frame
- **Function handlers** receive and return `arrow.Record` with one or more columns

**Single-column results** (most common):
```go
// Returns a single numeric vector
schema := arrow.NewSchema([]arrow.Field{{Name: "result", Type: arrow.PrimitiveTypes.Float64}}, nil)
return array.NewRecord(schema, []arrow.Array{resultArray}, numRows)
```

**Multi-column results** (for structured data):
```go
// Returns multiple columns like a data.frame with 2 columns
schema := arrow.NewSchema([]arrow.Field{
    {Name: "status", Type: arrow.BinaryTypes.String},
    {Name: "value", Type: arrow.PrimitiveTypes.Float64},
}, nil)
return array.NewRecord(schema, []arrow.Array{statusArray, valueArray}, numRows)
```

In this case, you don't use `TypeStruct` in the signature - the return type describes a single column. For multi-column returns, the actual schema is determined dynamically by the handler function.

## Example Server

See [cmd/rpc-example/main.go](../../cmd/rpc-example/main.go) for a complete example.

Basic server structure:

```go
func main() {
    registry := rgoipc.NewRegistry()
    
    // Register functions
    registry.Register("add", addHandler, signature)
    
    // Setup socket
    sock, _ := rep.NewSocket()
    sock.Listen(url)
    
    // Handle requests
    for {
        msgBytes, _ := sock.Recv()
        msg, _ := rgoipc.UnmarshalRPCMessage(msgBytes)
        
        switch msg.Type {
        case rgoipc.MsgTypeManifest:
            handleManifest(sock, registry)
        case rgoipc.MsgTypeCall:
            handleCall(sock, registry, msg)
        }
    }
}
```

## R Client Usage

From R, you can call registered Go functions using the mangoro package helpers and the RPC protocol. See the README.Rmd for examples.

## Testing

Run tests with:

```bash
cd inst/go
go test ./pkg/rgoipc/...
```
