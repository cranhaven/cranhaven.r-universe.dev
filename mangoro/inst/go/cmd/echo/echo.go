// Echo server using mangos REP socket over IPC
// Usage: echo <ipc_path>
package main

import (
	"fmt"
	"os"

	"go.nanomsg.org/mangos/v3/protocol/rep"
	_ "go.nanomsg.org/mangos/v3/transport/ipc"
)

func die(format string, v ...interface{}) {
	fmt.Fprintf(os.Stderr, format+"\n", v...)
	os.Exit(1)
}

func main() {
	if len(os.Args) != 2 {
		die("Usage: %s <ipc_path>", os.Args[0])
	}
	url := os.Args[1]
	sock, err := rep.NewSocket()
	if err != nil {
		die("can't get new rep socket: %s", err)
	}
	if err = sock.Listen(url); err != nil {
		die("can't listen on rep socket: %s", err)
	}
	for {
		msg, err := sock.Recv()
		if err != nil {
			die("cannot receive on rep socket: %s", err)
		}
		// Echo back
		err = sock.Send(msg)
		if err != nil {
			die("can't send reply: %s", err)
		}
	}
}
