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
:- use_module(library(json_rpc_common)).
:- autoload(library(json), [json_read_dict/3]).
:- autoload(library(option), [option/2, meta_options/3]).
:- use_module(library(debug), [debug/3]).
:- autoload(library(apply), [maplist/4, maplist/3]).
:- autoload(library(lists), [append/3, member/2]).
:- autoload(library(terms), [mapsubterms/3]).
:- autoload(library(http/http_stream), [stream_range_open/3]).
:- autoload(library(error), [permission_error/3]).

:- meta_predicate
    json_call(+, +, -, :),
    json_full_duplex(+, :).

/** <module> JSON RPC client

This module implements a JSON RPC compliant client. The three predicates
require a _stream pair_ (see stream_pair/2) that   connects us to a JSON
RPC server.

*/

:- dynamic
    json_result_queue/2,              % Stream, Queue
    pending/3.                        % Id, Stream, Action

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
%   Options are passed to   json_write_dict/3  and thread_get_message/3.
%   Additional options:
%
%     - async(:Closure)
%       Do not wait for the request to complete.  Instead, call
%       call(Closure, Data) from the client reading thread when
%       the request is completed.  If Closure is `true`, ignore
%       the reply.   As we cannot inject errors as exceptions in
%       the calling thread, possible errors are printed.
%     - thread_alias(+Atom)
%       Alias name to use for the thread that deals with incomming
%       replies and requests.   Defaults to ``json_rpc_client:<N>``,
%       where _N_ is a unique number.
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

json_call(Stream, Goal, Result, Options0) :-
    meta_options(is_meta, Options0, Options),
    Goal =.. [Name|Args0],
    call_args(Args0, Args),
    client_id(Id, Options),
    debug(json_rpc, 'Sending request ~p', [Id]),
    (   option(async(AsyncGoal), Options)
    ->  (   AsyncGoal = _:true
        ->  true
        ;   asserta(pending(Id, Stream, call(AsyncGoal)))
        ),
        Async = true
    ;   asserta(pending(Id, Stream, reply))
    ),
    (   Args == []
    ->  json_rpc_send(Stream,
                      #{ jsonrpc: "2.0",
                         id: Id,
                         method: Name
                       }, Options)
    ;   json_rpc_send(Stream,
                      #{ jsonrpc: "2.0",
                         id: Id,
                         method: Name,
                         params: Args
                       }, Options)
    ),
    (   Async == true
    ->  true
    ;   json_wait_reply(Stream, Id, Result, Options)
    ).

is_meta(async).

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
    ;   fail
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

%!  json_notify(+Stream, +Goal, +Options) is det.
%
%   Run Goal on a JSON RPC service  identified by Stream without waiting
%   for the result.

json_notify(Stream, Goal, Options) :-
    Goal =.. [Name|Args0],
    call_args(Args0, Args),
    (   Args == []
    ->  json_rpc_send(Stream,
                      #{ jsonrpc: "2.0",
                         method: Name
                       }, Options)
    ;   json_rpc_send(Stream,
                      #{ jsonrpc: "2.0",
                         method: Name,
                         params: Args
                       }, Options)
    ).

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
    (   IDs == []
    ->  true
    ;   batch_id(IDs, BatchId),
        asserta(pending(BatchId, Stream, reply))
    ),
    json_rpc_send(Stream, Batch, Options),
    flush_output(Stream),
    (   var(BatchId)
    ->  true
    ;   json_wait_reply(Stream, BatchId, Results0, Options),
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
    (   option(thread_alias(Alias), Options)
    ->  true
    ;   flag(json_rpc_client_dispatcher, N, N+1),
        format(atom(Alias), 'json_rpc_client:~w', [N])
    ),
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
    send_done(Stream, Id, true(Batch)).
handle_reply(Stream, Reply, _Options),
    #{ jsonrpc: "2.0",
       result: Result,
       id: Id } :< Reply =>
    send_done(Stream, Id, true(Result)).
handle_reply(Stream, Reply, _Options),
    #{ jsonrpc: "2.0",
       error: Error,
       id: Id } :< Reply =>
    send_done(Stream, Id, throw(error(json_rpc_error(Error), _))).
handle_reply(Stream, Request, Options),
    #{ jsonrpc: "2.0",
       method: _Method,
       params: _Params } :< Request =>
    option(server_module(M), Options),
    json_rpc_server:json_rpc_dispatch_request(M, Stream, Request, Options).

send_done(Stream, Id, Data) :-
    retract(pending(Id, Stream, Action)),
    !,
    reply_done(Action, Id, Stream, Data).
send_done(_Stream, Id, throw(error(json_rpc_error(Error), _))) :-
    !,
    print_message(error, error(json_rpc_error(Error, Id), _)).
send_done(_Stream, _Id, _Result).

reply_done(reply, Id, Stream, Data) =>
    json_result_queue(Stream, Queue),
    thread_send_message(Queue, done(Id, Data)).
reply_done(call(Goal), _Id, _Stream, true(Data)) =>
    catch_with_backtrace(
        call(Goal, Data),
        Error,
        print_message(error, Error)).
reply_done(call(_Goal), Id, _Stream,
           throw(error(json_rpc_error(Error), _))) =>
    print_message(error, error(json_rpc_error(Error, Id), _)).

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

