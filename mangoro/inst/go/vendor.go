// vendor.go: Import all mangos protocols and transports to ensure they are vendored
package main

import (
	// Protocols
	_ "go.nanomsg.org/mangos/v3/protocol/bus"
	_ "go.nanomsg.org/mangos/v3/protocol/pair"
	_ "go.nanomsg.org/mangos/v3/protocol/pub"
	_ "go.nanomsg.org/mangos/v3/protocol/pull"
	_ "go.nanomsg.org/mangos/v3/protocol/push"
	_ "go.nanomsg.org/mangos/v3/protocol/rep"
	_ "go.nanomsg.org/mangos/v3/protocol/req"
	_ "go.nanomsg.org/mangos/v3/protocol/respondent"
	_ "go.nanomsg.org/mangos/v3/protocol/star"
	_ "go.nanomsg.org/mangos/v3/protocol/sub"
	_ "go.nanomsg.org/mangos/v3/protocol/surveyor"

	// Transports
	_ "go.nanomsg.org/mangos/v3/transport/inproc"
	_ "go.nanomsg.org/mangos/v3/transport/ipc"
	_ "go.nanomsg.org/mangos/v3/transport/tcp"
	_ "go.nanomsg.org/mangos/v3/transport/tlstcp"
	_ "go.nanomsg.org/mangos/v3/transport/ws"
	_ "go.nanomsg.org/mangos/v3/transport/wss"

	// Arrow IPC package (and core)
	_ "github.com/apache/arrow/go/v18/arrow"
	_ "github.com/apache/arrow/go/v18/arrow/array"
	_ "github.com/apache/arrow/go/v18/arrow/ipc"
	_ "github.com/apache/arrow/go/v18/arrow/memory"
)

func main() {}
