package rgoipc_test

import (
	"testing"

	"mangoro.local/pkg/rgoipc"

	"github.com/apache/arrow/go/v18/arrow"
	"github.com/apache/arrow/go/v18/arrow/array"
	"github.com/apache/arrow/go/v18/arrow/memory"
)

func TestRegistry(t *testing.T) {
	registry := rgoipc.NewRegistry()

	// Register a simple function
	err := registry.Register("test_add", testAddHandler, rgoipc.FunctionSignature{
		Args: []rgoipc.ArgSpec{
			{Name: "x", Type: rgoipc.TypeSpec{Type: rgoipc.TypeFloat64}},
			{Name: "y", Type: rgoipc.TypeSpec{Type: rgoipc.TypeFloat64}},
		},
		ReturnType: rgoipc.TypeSpec{Type: rgoipc.TypeFloat64},
		Vectorized: true,
	})
	if err != nil {
		t.Fatalf("Failed to register function: %v", err)
	}

	// Check function exists
	fn, ok := registry.Get("test_add")
	if !ok {
		t.Fatal("Function not found after registration")
	}
	if fn.Name != "test_add" {
		t.Errorf("Expected name test_add, got %s", fn.Name)
	}

	// Check list
	funcs := registry.List()
	if len(funcs) != 1 {
		t.Errorf("Expected 1 function, got %d", len(funcs))
	}

	// Test manifest generation
	manifest, err := registry.Manifest()
	if err != nil {
		t.Fatalf("Failed to generate manifest: %v", err)
	}
	if len(manifest) == 0 {
		t.Error("Manifest is empty")
	}
}

func TestMessageMarshalUnmarshal(t *testing.T) {
	msg := &rgoipc.RPCMessage{
		Type:      rgoipc.MsgTypeCall,
		FuncName:  "test_func",
		ArrowData: []byte{1, 2, 3, 4},
		ErrorMsg:  "",
	}

	// Marshal
	data := msg.Marshal()
	if len(data) == 0 {
		t.Fatal("Marshaled message is empty")
	}

	// Unmarshal
	decoded, err := rgoipc.UnmarshalRPCMessage(data)
	if err != nil {
		t.Fatalf("Failed to unmarshal: %v", err)
	}

	if decoded.Type != msg.Type {
		t.Errorf("Type mismatch: expected %d, got %d", msg.Type, decoded.Type)
	}
	if decoded.FuncName != msg.FuncName {
		t.Errorf("FuncName mismatch: expected %s, got %s", msg.FuncName, decoded.FuncName)
	}
	if len(decoded.ArrowData) != len(msg.ArrowData) {
		t.Errorf("ArrowData length mismatch: expected %d, got %d", len(msg.ArrowData), len(decoded.ArrowData))
	}
}

func testAddHandler(input arrow.Record) (arrow.Record, error) {
	x := input.Column(0).(*array.Float64)
	y := input.Column(1).(*array.Float64)

	pool := memory.NewGoAllocator()
	builder := array.NewFloat64Builder(pool)
	defer builder.Release()

	for i := 0; i < x.Len(); i++ {
		builder.Append(x.Value(i) + y.Value(i))
	}

	result := builder.NewArray()
	defer result.Release()

	schema := arrow.NewSchema([]arrow.Field{{Name: "result", Type: arrow.PrimitiveTypes.Float64}}, nil)
	return array.NewRecord(schema, []arrow.Array{result}, int64(result.Len())), nil
}
