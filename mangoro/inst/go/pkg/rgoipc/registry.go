package rgoipc

import (
	"encoding/json"
	"fmt"
	"sync"

	"github.com/apache/arrow/go/v18/arrow"
	"github.com/apache/arrow/go/v18/arrow/memory"
)

// Registry holds Go functions that can be called from R
type Registry struct {
	mu        sync.RWMutex
	functions map[string]*RegisteredFunction
	allocator memory.Allocator
}

// NewRegistry creates a new function registry
func NewRegistry() *Registry {
	return &Registry{
		functions: make(map[string]*RegisteredFunction),
		allocator: memory.DefaultAllocator,
	}
}

// Register adds a function to the registry
func (r *Registry) Register(name string, fn FunctionHandler, sig FunctionSignature) error {
	r.mu.Lock()
	defer r.mu.Unlock()

	if _, exists := r.functions[name]; exists {
		return fmt.Errorf("function %s already registered", name)
	}

	inputSchema, err := buildInputSchema(sig.Args, r.allocator)
	if err != nil {
		return fmt.Errorf("failed to build input schema: %w", err)
	}

	outputSchema, err := buildOutputSchema(sig.ReturnType, r.allocator)
	if err != nil {
		return fmt.Errorf("failed to build output schema: %w", err)
	}

	r.functions[name] = &RegisteredFunction{
		Name:         name,
		Handler:      fn,
		InputSchema:  inputSchema,
		OutputSchema: outputSchema,
		Signature:    sig,
	}

	return nil
}

// Get retrieves a registered function
func (r *Registry) Get(name string) (*RegisteredFunction, bool) {
	r.mu.RLock()
	defer r.mu.RUnlock()
	fn, ok := r.functions[name]
	return fn, ok
}

// List returns all registered function names
func (r *Registry) List() []string {
	r.mu.RLock()
	defer r.mu.RUnlock()
	names := make([]string, 0, len(r.functions))
	for name := range r.functions {
		names = append(names, name)
	}
	return names
}

// Manifest returns function signatures as JSON for R discovery
func (r *Registry) Manifest() ([]byte, error) {
	r.mu.RLock()
	defer r.mu.RUnlock()

	manifest := make(map[string]FunctionSignature)
	for name, fn := range r.functions {
		manifest[name] = fn.Signature
	}
	return json.Marshal(manifest)
}

// buildInputSchema creates an Arrow schema from function arguments
func buildInputSchema(args []ArgSpec, alloc memory.Allocator) (*arrow.Schema, error) {
	fields := make([]arrow.Field, len(args))
	for i, arg := range args {
		dt, err := arrowTypeToDataType(arg.Type)
		if err != nil {
			return nil, fmt.Errorf("invalid type for arg %s: %w", arg.Name, err)
		}
		fields[i] = arrow.Field{
			Name:     arg.Name,
			Type:     dt,
			Nullable: arg.Optional,
		}
	}
	return arrow.NewSchema(fields, nil), nil
}

// buildOutputSchema creates an Arrow schema for function return
func buildOutputSchema(spec TypeSpec, alloc memory.Allocator) (*arrow.Schema, error) {
	dt, err := arrowTypeToDataType(spec)
	if err != nil {
		return nil, err
	}
	field := arrow.Field{
		Name:     "result",
		Type:     dt,
		Nullable: spec.Nullable,
	}
	return arrow.NewSchema([]arrow.Field{field}, nil), nil
}

// arrowTypeToDataType converts TypeSpec to arrow.DataType
func arrowTypeToDataType(spec TypeSpec) (arrow.DataType, error) {
	switch spec.Type {
	case TypeInt32:
		return arrow.PrimitiveTypes.Int32, nil
	case TypeFloat64:
		return arrow.PrimitiveTypes.Float64, nil
	case TypeString:
		return arrow.BinaryTypes.String, nil
	case TypeBool:
		return arrow.FixedWidthTypes.Boolean, nil
	case TypeList:
		if spec.ListSchema != nil {
			return arrow.ListOf(spec.ListSchema.Field(0).Type), nil
		}
		return nil, fmt.Errorf("list type requires schema")
	case TypeStruct:
		if spec.StructDef != nil {
			fields := make([]arrow.Field, len(spec.StructDef.Fields))
			for i, f := range spec.StructDef.Fields {
				dt, err := arrowTypeToDataType(f.Type)
				if err != nil {
					return nil, err
				}
				fields[i] = arrow.Field{Name: f.Name, Type: dt}
			}
			return arrow.StructOf(fields...), nil
		}
		return nil, fmt.Errorf("struct type requires definition")
	default:
		return nil, fmt.Errorf("unknown type: %s", spec.Type)
	}
}
