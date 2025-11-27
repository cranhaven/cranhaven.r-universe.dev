package rgoipc

import "errors"

var (
	// ErrFunctionNotFound is returned when a function is not registered
	ErrFunctionNotFound = errors.New("function not found")

	// ErrInvalidMessage is returned when message cannot be parsed
	ErrInvalidMessage = errors.New("invalid message format")

	// ErrInvalidSchema is returned when Arrow schema is invalid
	ErrInvalidSchema = errors.New("invalid Arrow schema")

	// ErrExecutionFailed is returned when function execution fails
	ErrExecutionFailed = errors.New("function execution failed")
)
