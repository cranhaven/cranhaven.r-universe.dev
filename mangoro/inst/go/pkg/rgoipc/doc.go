// Package rgoipc provides types and utilities for building RPC servers in Go
// that can be called from R using Arrow IPC and nanomsg messaging.
//
// This package is part of the mangoro project, which enables R/Go IPC without
// requiring CGo in the Go process. Communication happens through separate
// processes connected via nanomsg IPC sockets, with Arrow IPC used for data
// serialization.
//
// # Data Exchange Model
//
// The primary data format is Arrow RecordBatch ↔ R data.frame:
//   - R data.frames are serialized as Arrow RecordBatches (tabular data)
//   - Go functions receive/return arrow.Record (multiple columns, multiple rows)
//   - Each column in the data.frame becomes an Arrow Array in the Record
//
// For simple single-column results, specify the column type in ReturnType.
// For multi-column results (like returning status + message), use TypeStruct
// to describe the schema, but understand that this describes the columns of
// the RecordBatch, not an Arrow Struct type per se.
//
// # Basic Usage
//
// Create a registry and register functions:
//
//	registry := rgoipc.NewRegistry()
//	registry.Register("add", addHandler, rgoipc.FunctionSignature{
//	    Args: []rgoipc.ArgSpec{
//	        {Name: "x", Type: rgoipc.TypeSpec{Type: rgoipc.TypeFloat64}},
//	        {Name: "y", Type: rgoipc.TypeSpec{Type: rgoipc.TypeFloat64}},
//	    },
//	    ReturnType: rgoipc.TypeSpec{Type: rgoipc.TypeFloat64},
//	    Vectorized: true,
//	})
//
// Implement a function handler:
//
//	func addHandler(input arrow.Record) (arrow.Record, error) {
//	    x := input.Column(0).(*array.Float64)
//	    y := input.Column(1).(*array.Float64)
//	    // ... perform computation and return arrow.Record
//	}
//
// See the README.md and cmd/rpc-example for complete examples.
package rgoipc
