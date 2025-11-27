// Simple RPC server example with function registration
package main

import (
	"bytes"
	"fmt"
	"os"

	"mangoro.local/pkg/rgoipc"

	"github.com/apache/arrow/go/v18/arrow"
	"github.com/apache/arrow/go/v18/arrow/array"
	"github.com/apache/arrow/go/v18/arrow/ipc"
	"github.com/apache/arrow/go/v18/arrow/memory"
	"go.nanomsg.org/mangos/v3"
	"go.nanomsg.org/mangos/v3/protocol/rep"
	_ "go.nanomsg.org/mangos/v3/transport/ipc"
)

func die(format string, v ...interface{}) {
	fmt.Fprintf(os.Stderr, format+"\n", v...)
	os.Exit(1)
}

// addHandler adds two numeric vectors element-wise
func addHandler(input arrow.Record) (arrow.Record, error) {
	if input.NumCols() != 2 {
		return nil, fmt.Errorf("expected 2 columns, got %d", input.NumCols())
	}

	x := input.Column(0).(*array.Float64)
	y := input.Column(1).(*array.Float64)

	if x.Len() != y.Len() {
		return nil, fmt.Errorf("length mismatch: %d vs %d", x.Len(), y.Len())
	}

	// Build result
	pool := memory.NewGoAllocator()
	builder := array.NewFloat64Builder(pool)
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

	schema := arrow.NewSchema([]arrow.Field{{Name: "result", Type: arrow.PrimitiveTypes.Float64}}, nil)
	return array.NewRecord(schema, []arrow.Array{result}, int64(result.Len())), nil
}

// echoStringHandler echoes back a string vector unchanged
func echoStringHandler(input arrow.Record) (arrow.Record, error) {
	if input.NumCols() != 1 {
		return nil, fmt.Errorf("expected 1 column, got %d", input.NumCols())
	}

	strCol := input.Column(0).(*array.String)

	// Build result
	pool := memory.NewGoAllocator()
	builder := array.NewStringBuilder(pool)
	defer builder.Release()

	for i := 0; i < strCol.Len(); i++ {
		if strCol.IsNull(i) {
			builder.AppendNull()
		} else {
			builder.Append(strCol.Value(i))
		}
	}

	result := builder.NewArray()
	defer result.Release()

	schema := arrow.NewSchema([]arrow.Field{{Name: "result", Type: arrow.BinaryTypes.String}}, nil)
	return array.NewRecord(schema, []arrow.Array{result}, int64(result.Len())), nil
}

// echoStructHandler echoes back a struct column (nested data)
// Input: data.frame with one column of type struct (nested list in R)
// Output: same struct column echoed back
//
// Note: This demonstrates actual Arrow Struct type usage - a column where each row
// contains a nested record with named fields. In R, this maps to a list column.
// This is different from a regular multi-column data.frame!
func echoStructHandler(input arrow.Record) (arrow.Record, error) {
	if input.NumCols() != 1 {
		return nil, fmt.Errorf("expected 1 column, got %d", input.NumCols())
	}

	structCol, ok := input.Column(0).(*array.Struct)
	if !ok {
		return nil, fmt.Errorf("expected struct column, got %T", input.Column(0))
	}

	// Get the struct type to reconstruct it
	structType := structCol.DataType().(*arrow.StructType)
	
	// Build output struct column
	pool := memory.NewGoAllocator()
	builder := array.NewStructBuilder(pool, structType)
	defer builder.Release()

	// Copy each struct row
	for i := 0; i < structCol.Len(); i++ {
		if structCol.IsNull(i) {
			builder.AppendNull()
		} else {
			builder.Append(true)
			// Copy each field
			for fieldIdx := 0; fieldIdx < structCol.NumField(); fieldIdx++ {
				fieldArray := structCol.Field(fieldIdx)
				fieldBuilder := builder.FieldBuilder(fieldIdx)
				
				// Copy value based on type
				switch fb := fieldBuilder.(type) {
				case *array.StringBuilder:
					strArray := fieldArray.(*array.String)
					if strArray.IsNull(i) {
						fb.AppendNull()
					} else {
						fb.Append(strArray.Value(i))
					}
				case *array.Int32Builder:
					intArray := fieldArray.(*array.Int32)
					if intArray.IsNull(i) {
						fb.AppendNull()
					} else {
						fb.Append(intArray.Value(i))
					}
				case *array.Float64Builder:
					floatArray := fieldArray.(*array.Float64)
					if floatArray.IsNull(i) {
						fb.AppendNull()
					} else {
						fb.Append(floatArray.Value(i))
					}
				default:
					return nil, fmt.Errorf("unsupported field type: %T", fb)
				}
			}
		}
	}

	result := builder.NewArray()
	defer result.Release()

	schema := arrow.NewSchema([]arrow.Field{{Name: "person", Type: structType}}, nil)
	return array.NewRecord(schema, []arrow.Array{result}, int64(structCol.Len())), nil
}

// transposeMatrixHandler transposes a matrix passed as a data.frame (columns become rows)
// Input: data.frame with N numeric columns of length M (sent as arrow.Record)
// Output: data.frame with M numeric columns of length N (returned as arrow.Record)
// 
// Note: This demonstrates how arrow.Record (tabular data) naturally maps to R data.frames.
// The input data.frame is deserialized as an arrow.Record where each df column is an Arrow column.
// The output arrow.Record is serialized back to R as a data.frame with transposed dimensions.
func transposeMatrixHandler(input arrow.Record) (arrow.Record, error) {
	nCols := int(input.NumCols())
	if nCols == 0 {
		return nil, fmt.Errorf("expected at least 1 column")
	}

	nRows := int(input.NumRows())
	if nRows == 0 {
		return nil, fmt.Errorf("expected at least 1 row")
	}

	// Read all input columns as float64 arrays
	inputCols := make([]*array.Float64, nCols)
	for i := 0; i < nCols; i++ {
		col, ok := input.Column(i).(*array.Float64)
		if !ok {
			return nil, fmt.Errorf("column %d is not float64", i)
		}
		inputCols[i] = col
	}

	// Build transposed output: nRows columns, each with nCols rows
	pool := memory.NewGoAllocator()
	outputCols := make([]arrow.Array, nRows)
	fields := make([]arrow.Field, nRows)

	for rowIdx := 0; rowIdx < nRows; rowIdx++ {
		builder := array.NewFloat64Builder(pool)
		
		for colIdx := 0; colIdx < nCols; colIdx++ {
			if inputCols[colIdx].IsNull(rowIdx) {
				builder.AppendNull()
			} else {
				builder.Append(inputCols[colIdx].Value(rowIdx))
			}
		}
		
		outputCols[rowIdx] = builder.NewArray()
		defer outputCols[rowIdx].Release()
		fields[rowIdx] = arrow.Field{Name: fmt.Sprintf("V%d", rowIdx+1), Type: arrow.PrimitiveTypes.Float64}
		builder.Release()
	}

	schema := arrow.NewSchema(fields, nil)
	return array.NewRecord(schema, outputCols, int64(nCols)), nil
}

func main() {
	if len(os.Args) != 2 {
		die("Usage: %s <ipc_path>", os.Args[0])
	}
	url := os.Args[1]

	// Create registry and register functions
	registry := rgoipc.NewRegistry()

	err := registry.Register("add", addHandler, rgoipc.FunctionSignature{
		Args: []rgoipc.ArgSpec{
			{Name: "x", Type: rgoipc.TypeSpec{Type: rgoipc.TypeFloat64, Nullable: true}},
			{Name: "y", Type: rgoipc.TypeSpec{Type: rgoipc.TypeFloat64, Nullable: true}},
		},
		ReturnType: rgoipc.TypeSpec{Type: rgoipc.TypeFloat64, Nullable: true},
		Vectorized: true,
		Metadata:   map[string]string{"description": "Add two numeric vectors"},
	})
	if err != nil {
		die("Failed to register add function: %s", err)
	}

	err = registry.Register("echoString", echoStringHandler, rgoipc.FunctionSignature{
		Args: []rgoipc.ArgSpec{
			{Name: "s", Type: rgoipc.TypeSpec{Type: rgoipc.TypeString, Nullable: true}},
		},
		ReturnType: rgoipc.TypeSpec{Type: rgoipc.TypeString, Nullable: true},
		Vectorized: true,
		Metadata:   map[string]string{"description": "Echo back a string vector"},
	})
	if err != nil {
		die("Failed to register echoString function: %s", err)
	}

	// Register echoStruct - demonstrates actual Arrow Struct type (nested data)
	err = registry.Register("echoStruct", echoStructHandler, rgoipc.FunctionSignature{
		Args: []rgoipc.ArgSpec{
			{
				Name: "person",
				Type: rgoipc.TypeSpec{
					Type: rgoipc.TypeStruct,
					StructDef: &rgoipc.StructDef{
						Fields: []rgoipc.FieldDef{
							{Name: "name", Type: rgoipc.TypeSpec{Type: rgoipc.TypeString}},
							{Name: "age", Type: rgoipc.TypeSpec{Type: rgoipc.TypeInt32}},
						},
					},
				},
			},
		},
		ReturnType: rgoipc.TypeSpec{
			Type: rgoipc.TypeStruct,
			StructDef: &rgoipc.StructDef{
				Fields: []rgoipc.FieldDef{
					{Name: "name", Type: rgoipc.TypeSpec{Type: rgoipc.TypeString}},
					{Name: "age", Type: rgoipc.TypeSpec{Type: rgoipc.TypeInt32}},
				},
			},
		},
		Vectorized: true,
		Metadata:   map[string]string{"description": "Echo back a struct column (nested data)"},
	})
	if err != nil {
		die("Failed to register echoStruct function: %s", err)
	}

	err = registry.Register("transposeMatrix", transposeMatrixHandler, rgoipc.FunctionSignature{
		Args: []rgoipc.ArgSpec{},
		ReturnType: rgoipc.TypeSpec{
			Type: rgoipc.TypeStruct,
			StructDef: &rgoipc.StructDef{
				Fields: []rgoipc.FieldDef{}, // Empty - dynamic structure (schema determined at runtime)
			},
		},
		Vectorized: false,
		Metadata:   map[string]string{"description": "Transpose a matrix (columns <-> rows)"},
	})
	if err != nil {
		die("Failed to register transposeMatrix function: %s", err)
	}

	fmt.Println("Registered functions:", registry.List())

	// Setup socket
	sock, err := rep.NewSocket()
	if err != nil {
		die("can't get new rep socket: %s", err)
	}
	if err = sock.Listen(url); err != nil {
		die("can't listen on rep socket: %s", err)
	}

	fmt.Printf("RPC server listening on %s\n", url)

	// Main loop - REP socket can handle multiple REQ clients
	// Process each request in a goroutine for concurrency
	for {
		msgBytes, err := sock.Recv()
		if err != nil {
			fmt.Fprintf(os.Stderr, "receive error: %s\n", err)
			continue
		}

		// Handle each request concurrently
		// REP routing ensures response goes to correct client
		go handleRequest(sock, registry, msgBytes)
	}
}

func handleRequest(sock mangos.Socket, registry *rgoipc.Registry, msgBytes []byte) {
	msg, err := rgoipc.UnmarshalRPCMessage(msgBytes)
	if err != nil {
		fmt.Fprintf(os.Stderr, "unmarshal error: %s\n", err)
		sendError(sock, "", fmt.Sprintf("unmarshal error: %s", err))
		return
	}

	switch msg.Type {
	case rgoipc.MsgTypeManifest:
		handleManifest(sock, registry)
	case rgoipc.MsgTypeCall:
		handleCall(sock, registry, msg)
	default:
		sendError(sock, "", "unknown message type")
	}
}

func handleManifest(sock mangos.Socket, registry *rgoipc.Registry) {
	manifest, err := registry.Manifest()
	if err != nil {
		sendError(sock, "", fmt.Sprintf("manifest error: %s", err))
		return
	}

	response := &rgoipc.RPCMessage{
		Type:      rgoipc.MsgTypeManifest,
		ArrowData: manifest,
	}
	sock.Send(response.Marshal())
}

func handleCall(sock mangos.Socket, registry *rgoipc.Registry, msg *rgoipc.RPCMessage) {
	fn, ok := registry.Get(msg.FuncName)
	if !ok {
		sendError(sock, msg.FuncName, "function not found")
		return
	}

	// Parse Arrow IPC input
	reader, err := ipc.NewReader(bytes.NewReader(msg.ArrowData))
	if err != nil {
		sendError(sock, msg.FuncName, fmt.Sprintf("arrow read error: %s", err))
		return
	}
	defer reader.Release()

	if !reader.Next() {
		sendError(sock, msg.FuncName, "no records in input")
		return
	}

	inputRecord := reader.Record()
	defer inputRecord.Release()

	// Execute function
	result, err := fn.Handler(inputRecord)
	if err != nil {
		sendError(sock, msg.FuncName, fmt.Sprintf("execution error: %s", err))
		return
	}
	defer result.Release()

	// Serialize result
	var buf bytes.Buffer
	writer := ipc.NewWriter(&buf, ipc.WithSchema(result.Schema()))
	defer writer.Close()

	if err := writer.Write(result); err != nil {
		sendError(sock, msg.FuncName, fmt.Sprintf("arrow write error: %s", err))
		return
	}

	response := &rgoipc.RPCMessage{
		Type:      rgoipc.MsgTypeResult,
		FuncName:  msg.FuncName,
		ArrowData: buf.Bytes(),
	}
	sock.Send(response.Marshal())
}

func sendError(sock mangos.Socket, funcName, errMsg string) {
	response := &rgoipc.RPCMessage{
		Type:     rgoipc.MsgTypeError,
		FuncName: funcName,
		ErrorMsg: errMsg,
	}
	sock.Send(response.Marshal())
}
