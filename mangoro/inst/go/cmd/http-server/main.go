// HTTP file server with RPC control interface
package main

import (
	"context"
	"crypto/tls"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"time"

	"go.nanomsg.org/mangos/v3"
	"go.nanomsg.org/mangos/v3/protocol/rep"
	_ "go.nanomsg.org/mangos/v3/transport/ipc"

	"mangoro.local/pkg/rgoipc"

	"github.com/apache/arrow/go/v18/arrow"
	"github.com/apache/arrow/go/v18/arrow/array"
	"github.com/apache/arrow/go/v18/arrow/memory"
)

func die(format string, v ...interface{}) {
	fmt.Fprintf(os.Stderr, format+"\n", v...)
	os.Exit(1)
}

var (
	httpServer *http.Server
	serverLog  *log.Logger
)

// startServerHandler starts an HTTP server with given configuration
func startServerHandler(input arrow.Record) (arrow.Record, error) {
	if input.NumCols() != 9 {
		return nil, fmt.Errorf("expected 9 columns, got %d", input.NumCols())
	}

	// Extract parameters
	addrCol := input.Column(0).(*array.String)
	dirCol := input.Column(1).(*array.String)
	prefixCol := input.Column(2).(*array.String)
	corsCol := input.Column(3).(*array.Boolean)
	coopCol := input.Column(4).(*array.Boolean)
	tlsCol := input.Column(5).(*array.Boolean)
	certCol := input.Column(6).(*array.String)
	keyCol := input.Column(7).(*array.String)
	silentCol := input.Column(8).(*array.Boolean)

	if addrCol.Len() == 0 {
		return nil, fmt.Errorf("no server address provided")
	}

	addr := addrCol.Value(0)
	dir := "."
	if dirCol.Len() > 0 && !dirCol.IsNull(0) {
		dir = dirCol.Value(0)
	}
	prefix := "/"
	if prefixCol.Len() > 0 && !prefixCol.IsNull(0) {
		prefix = prefixCol.Value(0)
	}
	cors := corsCol.Len() > 0 && !corsCol.IsNull(0) && corsCol.Value(0)
	coop := coopCol.Len() > 0 && !coopCol.IsNull(0) && coopCol.Value(0)
	useTLS := tlsCol.Len() > 0 && !tlsCol.IsNull(0) && tlsCol.Value(0)
	certFile := ""
	if certCol.Len() > 0 && !certCol.IsNull(0) {
		certFile = certCol.Value(0)
	}
	keyFile := ""
	if keyCol.Len() > 0 && !keyCol.IsNull(0) {
		keyFile = keyCol.Value(0)
	}
	silent := silentCol.Len() > 0 && !silentCol.IsNull(0) && silentCol.Value(0)

	// Check if server is already running
	if httpServer != nil {
		return buildResponse("error", "HTTP server already running")
	}

	// Setup logger
	var logWriter io.Writer
	if silent {
		logWriter = io.Discard
	} else {
		logWriter = os.Stdout
	}
	serverLog = log.New(logWriter, "[mangoro server] ", log.LstdFlags)

	// Get absolute directory path
	absDir, err := filepath.Abs(dir)
	if err != nil {
		return buildResponse("error", fmt.Sprintf("invalid directory: %v", err))
	}

	// Setup HTTP handler
	mux := http.NewServeMux()
	fileHandler := http.FileServer(http.Dir(absDir))

	if cors {
		fileHandler = enableCORS(fileHandler)
	}
	if coop {
		fileHandler = enableCOOP(fileHandler)
	}

	fileHandler = serveLogger(serverLog, fileHandler)

	if prefix == "/" {
		mux.Handle("/", fileHandler)
	} else {
		mux.Handle(prefix+"/", http.StripPrefix(prefix, fileHandler))
	}

	httpServer = &http.Server{
		Addr:    addr,
		Handler: mux,
	}

	if useTLS {
		httpServer.TLSConfig = &tls.Config{
			MinVersion:               tls.VersionTLS12,
			PreferServerCipherSuites: true,
		}
	}

	// Channel to capture startup errors
	errChan := make(chan error, 1)

	// Start server in background
	go func() {
		var err error
		if useTLS {
			// Validate cert and key files are provided
			if certFile == "" || keyFile == "" {
				serverLog.Printf("TLS error: certificate and key files required")
				errChan <- fmt.Errorf("certificate and key files required for TLS")
				return
			}
			serverLog.Printf("Starting HTTPS server on %s serving %s at %s", addr, absDir, prefix)
			err = httpServer.ListenAndServeTLS(certFile, keyFile)
		} else {
			serverLog.Printf("Starting HTTP server on %s serving %s at %s", addr, absDir, prefix)
			err = httpServer.ListenAndServe()
		}
		if err != nil && err != http.ErrServerClosed {
			serverLog.Printf("HTTP server error: %v", err)
			errChan <- err
		}
	}()

	// Wait briefly to detect immediate startup errors (like port already in use)
	select {
	case err := <-errChan:
		httpServer = nil
		return buildResponse("error", fmt.Sprintf("failed to start server: %v", err))
	case <-time.After(500 * time.Millisecond):
		// Server seems to have started successfully
	}

	return buildResponse("ok", fmt.Sprintf("HTTP server started on %s", addr))
}

// stopServerHandler stops the running HTTP server
func stopServerHandler(input arrow.Record) (arrow.Record, error) {
	if httpServer == nil {
		return buildResponse("error", "No HTTP server is running")
	}

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	if err := httpServer.Shutdown(ctx); err != nil {
		return buildResponse("error", fmt.Sprintf("shutdown failed: %v", err))
	}

	httpServer = nil
	serverLog.Printf("HTTP server stopped")

	return buildResponse("ok", "HTTP server stopped")
}

// statusHandler returns the status of the HTTP server
func statusHandler(input arrow.Record) (arrow.Record, error) {
	if httpServer == nil {
		return buildResponse("status", "stopped")
	}
	return buildResponse("status", "running at "+httpServer.Addr)
}

func buildResponse(status, message string) (arrow.Record, error) {
	pool := memory.NewGoAllocator()
	statusBuilder := array.NewStringBuilder(pool)
	messageBuilder := array.NewStringBuilder(pool)
	defer statusBuilder.Release()
	defer messageBuilder.Release()

	statusBuilder.Append(status)
	messageBuilder.Append(message)

	statusArr := statusBuilder.NewArray()
	messageArr := messageBuilder.NewArray()
	defer statusArr.Release()
	defer messageArr.Release()

	// Build a RecordBatch with 2 columns (status, message) and 1 row
	// This will be deserialized in R as a data.frame with 2 columns and 1 row
	schema := arrow.NewSchema([]arrow.Field{
		{Name: "status", Type: arrow.BinaryTypes.String},
		{Name: "message", Type: arrow.BinaryTypes.String},
	}, nil)

	return array.NewRecord(schema, []arrow.Array{statusArr, messageArr}, 1), nil
}

func serveLogger(logger *log.Logger, next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		start := time.Now()
		next.ServeHTTP(w, r)
		logger.Printf("%s %s %s %s", r.Method, r.RequestURI, r.RemoteAddr, time.Since(start))
	})
}

func enableCORS(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Access-Control-Allow-Origin", "*")
		w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
		w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Authorization, Range")
		w.Header().Set("Access-Control-Expose-Headers", "Content-Length, Content-Range, Accept-Ranges")

		if r.Method == "OPTIONS" {
			w.WriteHeader(http.StatusOK)
			return
		}
		next.ServeHTTP(w, r)
	})
}

func enableCOOP(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Cross-Origin-Opener-Policy", "same-origin")
		next.ServeHTTP(w, r)
	})
}

func main() {
	if len(os.Args) != 2 {
		die("Usage: %s <ipc_path>", os.Args[0])
	}
	url := os.Args[1]

	registry := rgoipc.NewRegistry()

	// Define the return type for all HTTP server functions
	// Note: TypeStruct here is used to describe the schema of the returned RecordBatch,
	// not an Arrow Struct type. The handler returns an arrow.Record with 2 columns
	// (status and message), which gets deserialized as a 1-row, 2-column data.frame in R.
	returnType := rgoipc.TypeSpec{
		Type: rgoipc.TypeStruct,
		StructDef: &rgoipc.StructDef{
			Fields: []rgoipc.FieldDef{
				{Name: "status", Type: rgoipc.TypeSpec{Type: rgoipc.TypeString}},
				{Name: "message", Type: rgoipc.TypeSpec{Type: rgoipc.TypeString}},
			},
		},
	}

	// Register server control functions
	err := registry.Register("startServer", startServerHandler, rgoipc.FunctionSignature{
		Args: []rgoipc.ArgSpec{
			{Name: "addr", Type: rgoipc.TypeSpec{Type: rgoipc.TypeString}},
			{Name: "dir", Type: rgoipc.TypeSpec{Type: rgoipc.TypeString, Nullable: true}},
			{Name: "prefix", Type: rgoipc.TypeSpec{Type: rgoipc.TypeString, Nullable: true}},
			{Name: "cors", Type: rgoipc.TypeSpec{Type: rgoipc.TypeBool, Nullable: true}},
			{Name: "coop", Type: rgoipc.TypeSpec{Type: rgoipc.TypeBool, Nullable: true}},
			{Name: "tls", Type: rgoipc.TypeSpec{Type: rgoipc.TypeBool, Nullable: true}},
			{Name: "cert", Type: rgoipc.TypeSpec{Type: rgoipc.TypeString, Nullable: true}},
			{Name: "key", Type: rgoipc.TypeSpec{Type: rgoipc.TypeString, Nullable: true}},
			{Name: "silent", Type: rgoipc.TypeSpec{Type: rgoipc.TypeBool, Nullable: true}},
		},
		ReturnType: returnType,
		Metadata:   map[string]string{"description": "Start HTTP file server"},
	})
	if err != nil {
		die("Failed to register startServer: %s", err)
	}

	err = registry.Register("stopServer", stopServerHandler, rgoipc.FunctionSignature{
		Args:       []rgoipc.ArgSpec{},
		ReturnType: returnType,
		Metadata:   map[string]string{"description": "Stop HTTP file server"},
	})
	if err != nil {
		die("Failed to register stopServer: %s", err)
	}

	err = registry.Register("serverStatus", statusHandler, rgoipc.FunctionSignature{
		Args:       []rgoipc.ArgSpec{},
		ReturnType: returnType,
		Metadata:   map[string]string{"description": "Get HTTP server status"},
	})
	if err != nil {
		die("Failed to register serverStatus: %s", err)
	}

	fmt.Println("Registered functions:", registry.List())

	// Setup RPC socket
	sock, err := rep.NewSocket()
	if err != nil {
		die("can't get new rep socket: %s", err)
	}
	if err = sock.Listen(url); err != nil {
		die("can't listen on rep socket: %s", err)
	}

	fmt.Printf("HTTP server controller listening on %s\n", url)

	// Main RPC loop
	for {
		msgBytes, err := sock.Recv()
		if err != nil {
			fmt.Fprintf(os.Stderr, "receive error: %s\n", err)
			continue
		}

		msg, err := rgoipc.UnmarshalRPCMessage(msgBytes)
		if err != nil {
			fmt.Fprintf(os.Stderr, "unmarshal error: %s\n", err)
			sendError(sock, "", fmt.Sprintf("unmarshal error: %s", err))
			continue
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
	var inputRecord arrow.Record
	reader, err := rgoipc.NewArrowReader(msg.ArrowData)
	if err != nil {
		sendError(sock, msg.FuncName, fmt.Sprintf("arrow read error: %s", err))
		return
	}
	defer reader.Release()

	// Check if there's a record batch in the stream
	if !reader.Next() {
		// No records - create an empty record with the schema from the reader
		// or an empty schema if the function expects no args
		if len(fn.Signature.Args) == 0 {
			schema := arrow.NewSchema([]arrow.Field{}, nil)
			inputRecord = array.NewRecord(schema, []arrow.Array{}, 0)
		} else {
			// Empty record batch with the schema from input
			inputRecord = array.NewRecord(reader.Schema(), []arrow.Array{}, 0)
		}
		defer inputRecord.Release()
	} else {
		inputRecord = reader.Record()
		defer inputRecord.Release()
	}

	// Execute function
	result, err := fn.Handler(inputRecord)
	if err != nil {
		sendError(sock, msg.FuncName, fmt.Sprintf("execution error: %s", err))
		return
	}
	defer result.Release()

	// Serialize result
	buf, err := rgoipc.WriteArrowRecord(result)
	if err != nil {
		sendError(sock, msg.FuncName, fmt.Sprintf("arrow write error: %s", err))
		return
	}

	response := &rgoipc.RPCMessage{
		Type:      rgoipc.MsgTypeResult,
		FuncName:  msg.FuncName,
		ArrowData: buf,
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
