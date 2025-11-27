// Package rgoipc provides interfaces and types for R/Go IPC using Arrow and mangos
package rgoipc

import (
	"github.com/apache/arrow/go/v18/arrow"
)

// ArrowType represents supported Arrow type mappings for R types
type ArrowType string

const (
	TypeInt32   ArrowType = "int32"   // R integer (single column)
	TypeFloat64 ArrowType = "float64" // R numeric (single column)
	TypeString  ArrowType = "string"  // R character (single column)
	TypeBool    ArrowType = "bool"    // R logical (single column)
	
	// Complex types - rarely used in typical data processing
	TypeList    ArrowType = "list"    // Arrow List<T> - variable-length arrays (R list column)
	TypeStruct  ArrowType = "struct"  // Arrow Struct - nested record with named fields (R struct column)
	
	// Note: The primary data exchange format is Arrow RecordBatch (tabular data),
	// which naturally maps to R data.frame. Functions receive/return arrow.Record,
	// which represents tabular data with multiple columns and rows.
	//
	// TypeStruct is for creating a COLUMN of structs (nested data), NOT for describing
	// multi-column results. For multi-column returns, just create arrow.Record with
	// multiple columns directly - see cmd/rpc-example for examples.
	//
	// Example struct column usage: misc/test_struct_column.R
)

// TypeSpec describes a type for function arguments or returns
type TypeSpec struct {
	Type       ArrowType
	Nullable   bool
	StructDef  *StructDef  // For struct types
	ListSchema *arrow.Schema // For list types
}

// StructDef defines structure for complex types
type StructDef struct {
	Fields []FieldDef
}

// FieldDef defines a field in a struct
type FieldDef struct {
	Name     string
	Type     TypeSpec
	Metadata map[string]string
}

// ArgSpec describes a function argument
type ArgSpec struct {
	Name     string
	Type     TypeSpec
	Optional bool
	Default  interface{}
}

// FunctionSignature describes a Go function callable from R
type FunctionSignature struct {
	Args       []ArgSpec
	ReturnType TypeSpec
	Vectorized bool // Can process batches
	Metadata   map[string]string
}

// FunctionHandler processes Arrow record batches
// Input: Arrow Record containing function arguments as columns (like a data.frame row)
// Output: Arrow Record containing function results as columns (like a data.frame row)
// 
// The Record can have:
// - Single column (scalar/vector return) - most common case
// - Multiple columns (structured return) - for complex results
// - Multiple rows (batch processing) - when Vectorized=true
//
// Example for scalar return (single column, single row):
//   schema := arrow.NewSchema([]arrow.Field{{Name: "result", Type: arrow.PrimitiveTypes.Float64}}, nil)
//   return array.NewRecord(schema, []arrow.Array{resultArray}, numRows)
//
// Example for structured return (multiple columns, single row):
//   schema := arrow.NewSchema([]arrow.Field{
//     {Name: "status", Type: arrow.BinaryTypes.String},
//     {Name: "message", Type: arrow.BinaryTypes.String}
//   }, nil)
//   return array.NewRecord(schema, []arrow.Array{statusArray, messageArray}, 1)
type FunctionHandler func(input arrow.Record) (arrow.Record, error)

// RegisteredFunction represents a function registered for RPC
type RegisteredFunction struct {
	Name         string
	Handler      FunctionHandler
	InputSchema  *arrow.Schema
	OutputSchema *arrow.Schema
	Signature    FunctionSignature
}
