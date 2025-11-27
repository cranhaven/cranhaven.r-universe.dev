package rgoipc

import (
	"bytes"

	"github.com/apache/arrow/go/v18/arrow"
	"github.com/apache/arrow/go/v18/arrow/ipc"
	"github.com/apache/arrow/go/v18/arrow/memory"
)

// MessageType indicates the kind of RPC message
type MessageType uint8

const (
	MsgTypeManifest MessageType = iota // Go → R: function list
	MsgTypeCall                        // R → Go: function call
	MsgTypeResult                      // Go → R: result
	MsgTypeError                       // Go → R: error
)

// RPCMessage wraps Arrow IPC data with metadata
type RPCMessage struct {
	Type      MessageType
	FuncName  string
	ArrowData []byte // Arrow IPC stream format
	ErrorMsg  string // For error messages
}

// Marshal serializes RPC message to wire format
// Format: [type:1byte][name_len:4bytes][name][error_len:4bytes][error][arrow_data]
func (m *RPCMessage) Marshal() []byte {
	nameBytes := []byte(m.FuncName)
	nameLen := uint32(len(nameBytes))
	errorBytes := []byte(m.ErrorMsg)
	errorLen := uint32(len(errorBytes))

	buf := make([]byte, 1+4+int(nameLen)+4+int(errorLen)+len(m.ArrowData))
	pos := 0

	// Type
	buf[pos] = byte(m.Type)
	pos++

	// Function name length + name
	buf[pos] = byte(nameLen >> 24)
	buf[pos+1] = byte(nameLen >> 16)
	buf[pos+2] = byte(nameLen >> 8)
	buf[pos+3] = byte(nameLen)
	pos += 4
	copy(buf[pos:], nameBytes)
	pos += len(nameBytes)

	// Error length + error
	buf[pos] = byte(errorLen >> 24)
	buf[pos+1] = byte(errorLen >> 16)
	buf[pos+2] = byte(errorLen >> 8)
	buf[pos+3] = byte(errorLen)
	pos += 4
	copy(buf[pos:], errorBytes)
	pos += len(errorBytes)

	// Arrow data
	copy(buf[pos:], m.ArrowData)

	return buf
}

// UnmarshalRPCMessage deserializes RPC message from wire format
func UnmarshalRPCMessage(data []byte) (*RPCMessage, error) {
	if len(data) < 9 { // minimum: 1 + 4 + 0 + 4 + 0
		return nil, ErrInvalidMessage
	}

	msg := &RPCMessage{}
	pos := 0

	// Type
	msg.Type = MessageType(data[pos])
	pos++

	// Function name
	nameLen := uint32(data[pos])<<24 | uint32(data[pos+1])<<16 | uint32(data[pos+2])<<8 | uint32(data[pos+3])
	pos += 4
	if pos+int(nameLen) > len(data) {
		return nil, ErrInvalidMessage
	}
	msg.FuncName = string(data[pos : pos+int(nameLen)])
	pos += int(nameLen)

	// Error message
	errorLen := uint32(data[pos])<<24 | uint32(data[pos+1])<<16 | uint32(data[pos+2])<<8 | uint32(data[pos+3])
	pos += 4
	if pos+int(errorLen) > len(data) {
		return nil, ErrInvalidMessage
	}
	msg.ErrorMsg = string(data[pos : pos+int(errorLen)])
	pos += int(errorLen)

	// Arrow data
	msg.ArrowData = data[pos:]

	return msg, nil
}

// NewArrowReader creates an Arrow IPC reader from bytes
func NewArrowReader(data []byte) (*ipc.Reader, error) {
	return ipc.NewReader(bytes.NewReader(data), ipc.WithAllocator(memory.DefaultAllocator))
}

// WriteArrowRecord writes an Arrow record to bytes
func WriteArrowRecord(record arrow.Record) ([]byte, error) {
	var buf bytes.Buffer
	writer := ipc.NewWriter(&buf, ipc.WithSchema(record.Schema()))
	defer writer.Close()

	if err := writer.Write(record); err != nil {
		return nil, err
	}

	if err := writer.Close(); err != nil {
		return nil, err
	}

	return buf.Bytes(), nil
}
