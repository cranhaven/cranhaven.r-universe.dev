/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2025, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(json_rpc_client,
          [ json_call/4,          % +Stream, +Goal, -Result, +Options
            json_notify/3,        % +Stream, +Goal, +Options
            json_batch/5,         % +Stream, +Notifications, +Calls, -Results, +Options
            json_full_duplex/2    % +Stream, :Options
          ]).
:- autoload(library(json), [json_write_dict/3, json_read_dict/3]).
:- autoload(library(option), [option/2]).
:- use_module(library(debug), [debug/3]).
:- autoload(library(apply), [maplist/4, maplist/3]).
:- autoload(library(lists), [append/3, member/2]).
:- autoload(library(terms), [mapsubterms/3]).
:- autoload(library(http/http_stream), [stream_range_open/3]).

:- meta_predicate
    json_full_duplex(+, :).

/** <module> JSON RPC client

This module implements a JSON RPC compliant client. The three predicates
require a _stream pair_ (see stream_pair/2) that   connects us to a JSON
RPC server.

*/

:- dynamic
    json_result_queue/2,                        % Stream, Queue
    failed_id/2.                                % Queue, Id

%!  json_call(+Stream, +Goal, -Result, +Options) is det.
%
%   Run Goal on a JSON RPC  service   identified  by Stream and wait for
%   Result. This predicate may  be  called   from  multiple  threads. As
%   replies come in in arbitrary order,   this predicate starts a thread
%   the reads the replies from  Stream   and  informs the calling thread
%   using a Prolog message queue.
%
%   If Stream is closed this library   terminates the thread and related
%   message queue.
%
%   @arg Goal is a callable term.  The   functor  name is the method. If
%   there is a single argument that  is   a  dict,  we invoke a JSON-RPC
%   method using _named arguments_. If there   is a single argument that
%   is a list, use the elements of   the list as _positional arguments_.
%   If  there  are  zero  or  more  than  one  arguments  use  these  as
%   _positional arguments_.  Examples:
%
%     |Term             |Method|Type       |JSON (`params`) |
%     -------------------------------------------------------
%     |f(#{a:1,b:2})      | f | named      | {"a":1, "b":2} |
%     |f(["a", 42])       | f | positional | ["a", 42]      |
%     |f([#{"a":1}])      | f | positional | [{"a":1}]      |
%     |f()                | f | positional | []             |
%     |f("a", 42)         | f | positional | ["a", 42]      |
%
%     Options processed:
%

json_call(Stream, Goal, Result, Options) :-
    Goal =.. [Name|Args0],
    call_args(Args0, Args),
    client_id(Id, Options),
    debug(json_rpc, 'Sending request ~p', [Id]),
    json_send(Stream,
              #{ jsonrpc: "2.0",
                 id: Id,
                 method: Name,
                 params: Args
               }, Options),
    setup_call_catcher_cleanup(
        true,
        json_wait_reply(Stream, Id, Result, Options),
        Catcher,
        client_cleanup(Catcher, Stream, Id)).

call_args([Arg], Args), is_dict(Arg) =>
    Args = Arg.
call_args([Args0], Args), is_list(Args0) =>
    Args = Args0.
call_args(Args0, Args) =>
    Args = Args0.

json_wait_reply(Stream, Id, Result, Options) :-
    with_mutex(json_rpc_client,
               get_json_result_queue(Stream, Queue, Options)),
    debug(json_rpc, 'Waiting for reply', []),
    (   thread_get_message(Queue, done(Id, Result0), Options)
    ->  map_reply(Result0, Result1, Options),
        debug(json_rpc, 'Got reply for ~p', [Id]),
        (   Result1 = throw(Error)
        ->  throw(Error)
        ;   Result1 = true(Result)
        )
    ;   assertz(failed_id(Queue, Id)),
        fail
    ).

map_reply(Reply0, Reply, Options) :-
    option(value_string_as(atom), Options),
    !,
    mapsubterms(map_string, Reply0, Reply).
map_reply(Reply, Reply, _).

map_string(String, Atom) :-
    string(String),
    atom_string(Atom,String).

client_id(Id, Options) :-
    option(id(Id), Options),
    !.
client_id(Id, _Options) :-
    flag(json_client_id, Id, Id+1).

client_cleanup(exit, _, _) =>
    true.
client_cleanup(_, Stream, Id) =>
    json_result_queue(Stream, Queue),
    assertz(failed_id(Queue, Id)).

%!  json_notify(+Stream, +Goal, +Options) is det.
%
%   Run Goal on a JSON RPC service  identified by Stream without waiting
%   for the result.

json_notify(Stream, Goal, Options) :-
    Goal =.. [Name|Args0],
    call_args(Args0, Args),
    json_send(Stream,
              #{ jsonrpc: "2.0",
                 method: Name,
                 params: Args
               }, Options).

%!  json_batch(+Stream, +Notifications:list, +Calls:list, -Results:list,
%!             +Options) is det.
%
%   Run a batch of notifications and normal  calls on the JSON server at
%   the other end of Stream. On  success,   Result  is unified to a list
%   with the same length as Calls. Each element either contains a value,
%   similar to json_call/4 or a term error(Dict), where `Dict` holds the
%   `code`, `message` and optional `data`   field. Note that error(Dict)
%   is not a valid JSON type  and   this  is thus unambiguous. While the
%   JSON RPC standard allows the server to   process the messages in any
%   order and allows for concurrent processing,  all results are sent in
%   one message and this client ensures the elements of the Results list
%   are in the same order as the Calls  list. If the Calls list is empty
%   this predicate does not wait for a reply.

json_batch(Stream, Notifications, Calls, Results, Options) :-
    maplist(call_to_json_request, Calls, IDs, Requests1),
    maplist(call_to_json_notification, Notifications, Requests2),
    append(Requests1, Requests2, Batch),
    json_send(Stream, Batch, Options),
    flush_output(Stream),
    (   IDs == []
    ->  true
    ;   batch_id(IDs, Id),
        json_wait_reply(Stream, Id, Results0, Options),
        sort(id, <, Results0, Results1),
        maplist(batch_result, Results1, Results)
    ).

call_to_json_request(Goal, Id, Request) :-
    Goal =.. [Name|Args],
    client_id(Id, []),
    Request = #{ jsonrpc: "2.0",
                 id: Id,
                 method: Name,
                 params: Args
               }.

call_to_json_notification(Goal, Notification) :-
    Goal =.. [Name|Args],
    Notification = #{ jsonrpc: "2.0",
                      method: Name,
                      params: Args
                    }.

batch_id(IDs, Id) :-
    sort(IDs, Canonical),
    variant_sha1(Canonical, Id).

batch_result(Reply, Result), Result0 = Reply.get(result) =>
    Result = Result0.
batch_result(Reply, Result), Result0 = Reply.get(error) =>
    Result = error(Result0).

%!  json_send(+Stream, +Dict, +Options)

json_send(Stream, Dict, Options) :-
    option(header(true), Options),
    !,
    with_output_to(string(Msg),
                   json_write_dict(current_output, Dict, Options)),
    utf8_length(Msg, Len),
    format(Stream,
           'Content-Length: ~d\r\n\r\n~s', [Len, Msg]),
    flush_output(Stream).
json_send(Stream, Dict, Options) :-
    with_output_to(Stream,
                   json_write_dict(Stream, Dict, Options)),
    flush_output(Stream).

utf8_length(String, Len) :-
    setup_call_cleanup(
        open_null_stream(Null),
        (   set_stream(Null, encoding(utf8)),
            format(Null, '~s', [String]),
            flush_output(Null),
            byte_count(Null, Len)
        ),
        close(Null)).

                /*******************************
                *        INCOMMING DATA        *
                *******************************/

%!  json_full_duplex(+Stream, :Options) is det.
%
%   Start the thread for incomming data   and on requests, dispatch them
%   using library(jso_rpc_server) in the module derived from the Options
%   list.

json_full_duplex(Stream, Options) :-
    with_mutex(json_rpc_client, json_full_duplex_(Stream, Options)).

json_full_duplex_(Stream, _) :-
    json_result_queue(Stream, _Queue),
    !,
    permission_error(json, full_duplex, Stream).
json_full_duplex_(Stream, M:Options) :-
    get_json_result_queue(Stream, _Queue,
                          [server_module(M)|Options]).


%!  get_json_result_queue(+Stream, -Queue, +Options) is det.
%
%   Find the result queue associated to Stream.  If this does not exist,
%   create one, as well as a thread   that handles the incomming results
%   and dispatches these to the queue.

get_json_result_queue(Stream, Queue, _Options) :-
    json_result_queue(Stream, Queue),
    !.
get_json_result_queue(Stream, Queue, Options) :-
    message_queue_create(Queue),
    asserta(json_result_queue(Stream, Queue)),
    flag(json_rpc_client_dispatcher, N, N+1),
    format(atom(Alias), 'json_rpc_client:~w', [N]),
    thread_create(
        handle_result_loop(Stream, Options),
        _Id,
        [ detached(true),
          alias(Alias),
          inherit_from(main),
          at_exit(cleanup_client(Stream))
        ]).

handle_result_loop(Stream, Options) :-
    handle_result(Stream, EOF, Options),
    (   EOF == true
    ->  true
    ;   handle_result_loop(Stream, Options)
    ).

handle_result(Stream, EOF, Options) :-
    Error = error(Formal, _),
    catch(json_receive(Stream, Reply, Options),
          Error,
          true),
    debug(json_rpc, 'Received ~p', [Reply]),
    (   Reply == end_of_file(true)
    ->  EOF = true
    ;   var(Formal)
    ->  handle_reply(Stream, Reply, Options)
    ;   handle_error(Error, EOF)
    ).

json_receive(Stream, Reply, Options) :-
    option(header(true), Options),
    !,
    read_header(Stream, Lines),
    (   Lines == []
    ->  Reply = end_of_file(true)
    ;   header_content_length(Lines, Length),
        setup_call_cleanup(
            stream_range_open(Stream, Data, [size(Length)]),
            json_read_dict(Data,
                           Reply,
                           Options),
            close(Data))
    ).
json_receive(Stream, Reply, Options) :-
    json_read_dict(Stream,
                   Reply,
                   [ end_of_file(end_of_file(true))
                   | Options
                   ]).

read_header(Stream, Lines) :-
    read_string(Stream, "\n", "\r\t ", Sep, Line),
    (   (Line == "" ; Sep == -1)
    ->  Lines = []
    ;   Lines = [Line|Rest],
        read_header(Stream, Rest)
    ).

header_content_length(Lines, Length) :-
    member(Line, Lines),
    split_string(Line, ":", "\t\s", [Field,Value]),
    string_lower(Field, "content-length"),
    !,
    number_string(Length, Value).

handle_reply(Stream, Batch, _Options),
    is_list(Batch) =>
    maplist(get_dict(id), Batch, IDs),
    batch_id(IDs, Id),
    json_result_queue(Stream, Queue),
    send_done(Queue, Id, true(Batch)).
handle_reply(Stream, Reply, _Options),
    #{ jsonrpc: "2.0",
       result: Result,
       id: Id } :< Reply =>
    json_result_queue(Stream, Queue),
    send_done(Queue, Id, true(Result)).
handle_reply(Stream, Reply, _Options),
    #{ jsonrpc: "2.0",
       error: Error,
       id: Id } :< Reply =>
    json_result_queue(Stream, Queue),
    send_done(Queue, Id, throw(error(json_rpc_error(Error), _))).
handle_reply(Stream, Request, Options),
    #{ jsonrpc: "2.0",
       method: _Method,
       params: _Params } :< Request =>
    option(server_module(M), Options),
    json_rpc_server:json_rpc_dispatch_request(M, Stream, Request, Options).


send_done(Queue, Id, _Data) :-
    retract(failed_id(Queue, Id)),
    !.
send_done(Queue, Id, Data) :-
    thread_send_message(Queue, done(Id, Data)),
    clean_dead_requests(Queue).

clean_dead_requests(Queue) :-
    forall(failed_id(Queue, Id),
           cleanup_dead_id(Queue, Id)).

cleanup_dead_id(Queue, Id) :-
    (   thread_get_message(Queue, done(Id, _), [timeout(0)])
    ->  retract(failed_id(Queue, Id))
    ;   true
    ).

handle_error(error(existence_error(stream, _), _), EOF) =>
    EOF = true.
handle_error(Error, _EOF) =>
    print_message(error, Error).

%!  cleanup_client(+Stream) is det.
%
%   Thread exit handler to remove the registration and queue.

cleanup_client(Stream) :-
    forall(retract(json_result_queue(Stream, Queue)),
           do_cleanup(Stream, Queue)).

do_cleanup(Stream, Queue) :-
    close(Stream, [force(true)]),
    message_queue_destroy(Queue).

