/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
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

:- module(test_json_rpc,
          [ test_json_rpc/0,
            bench_json_rpc/1
          ]).
:- use_module(library(json_rpc_server)).
:- use_module(library(json_rpc_client)).
:- use_module(library(socket)).
:- use_module(library(debug)).
:- use_module(library(plunit)).
:- use_module(library(gensym)).
:- use_module(library(statistics)).

test_json_rpc :-
    run_tests([ json_rpc
              ]).

bench_json_rpc(N) :-
    setup,
    time(forall(between(1,N,_),
                json_call(subtract(10,3), _X, []))).

json_rpc_debug :-
    set_prolog_flag(message_context, [thread,time('%T.%5f')]),
    debug(json_rpc),
    debug(json_rpc(server)).

setup :-
    client_connection(_),
    !.
setup :-
    server(Port),
    connect(Port).

:- begin_tests(json_rpc,
               [ setup(setup)
               ]).

test(simple, X == 7) :-
    json_call(subtract(10,3), X, []).
test(div_zero, error(json_rpc_error(
                         #{code: -32001,
                           message: "Execution error",
                           data: _PrologMessage
                          }))) :-
    json_call(divide(10,0), _, []).
test(batch, X == [7,5]) :-
    json_batch([],
               [ subtract(10,3),
                 subtract(12,7)
               ], X, []).
test(app_error, error(json_rpc_error(
                          #{code:2,
                            message:"Account does not exist"}))) :-
    json_call(deposit(#{account:no_such_account, amount:10}),
              _Balance, []).
test(open, [Open,Close] == [true,true]) :-
    json_call(open_account(bob), Open, []),
    json_call(close_account(bob), Close, []).
test(deposit, [Open,Balance1,Balance2,Close] == [true,10,15.5,true]) :-
    gensym(account, Name),
    json_call(open_account(Name), Open, []),
    json_call(deposit(#{account:Name, amount:10}), Balance1, []),
    json_call(deposit(#{account:Name, amount:5.5}), Balance2, []),
    json_call(close_account(Name), Close, []).
test(deposit, [Open,Balance2,Close] == [true,15.5,true]) :-
    gensym(account, Name),
    json_call(open_account(Name), Open, []),
    json_notify(deposit(#{account:Name, amount:10}), []),
    json_call(deposit(#{account:Name, amount:5.5}), Balance2, []),
    json_call(close_account(Name), Close, []).
test(type_error, error(json_rpc_error(
                           #{code: -32602,
                             message: "Invalid params",
                             data: _}))) :-
    json_call(open_account(3.14), _, []).
test(argument_count, error(json_rpc_error(
                               #{code: -32602,
                                 message: "Invalid params",
                                 data: _}))) :-
    json_call(open_account(bob, 3.14), _, []).
test(no_method, error(json_rpc_error(
                          #{code: -32601,
                            message: "Method not found",
                            data: "no_such_method"
                           }))) :-
    json_call(no_such_method, _, []).
test(no_method, error(json_rpc_error(
                          #{code: -32601,
                            message: 'Method not found',
                            data: 'no_such_method'
                           }))) :-
    json_call(no_such_method, _, [ value_string_as(atom) ]).
test(notification, Noticed == "Hello world") :-
    json_notify(notice("Hello world"), []),
    thread_wait(test_json_rpc:noticed(_),
                [ timeout(10),
                  wait_preds([noticed/1]),
                  module(test_json_rpc)
                ]),
    retract(test_json_rpc:noticed(Noticed)).
test(no_result, [Noticed,Result] == ["Hello world",true]) :-
    json_call(notice("Hello world"), Result, []),
    thread_wait(test_json_rpc:noticed(_),
                [ timeout(10),
                  wait_preds([noticed/1]),
                  module(test_json_rpc)
                ]),
    retract(test_json_rpc:noticed(Noticed)).


:- end_tests(json_rpc).


                /*******************************
                *        IMPLEMENTATION        *
                *******************************/

:- json_method
    subtract(#{type:number}, #{type:number}): #{type:number},
    divide(#{type:number}, #{type:number}): #{type:number},
    open_account([#{type: string}]): #{type:boolean},
    close_account([#{type: string}]): #{type:boolean},
    deposit(#{ properties:
                 #{ account: #{type:string},
                    amount:  #{type:number}
                  }}): #{type:number},
    notice([#{type: string}]).

subtract(A, B, C) :-
    C is A - B.

divide(A, B, C) :-
    C is A/B.

:- dynamic
    account/2,
    noticed/1.

open_account(Name, true) :-
    transaction((   account(Name, _Balance)
                ->  json_rpc_error(1, "Account exists")
                ;   asserta(account(Name, 0))
                )).

close_account(Name, true) :-
    transaction((   account(Name, Balance)
                ->  retractall(account(Name, Balance))
                ;   json_rpc_error(2, "Account does not exist")
                )).

deposit(Request, Reply),
    #{account: Account, amount: Amount} :< Request =>
    transaction((   retract(account(Account, Old))
                ->  New is Old+Amount,
                    asserta(account(Account, New))
                ;   json_rpc_error(2, "Account does not exist")
                )),
    Reply = New.

notice(String) :-
    assertz(noticed(String)).


                /*******************************
                *            SERVER            *
                *******************************/

%!  server(?Port) is det.

server(Port) :-
    socket_create(Socket, []),
    tcp_setopt(Socket, reuseaddr),
    tcp_bind(Socket, Port),
    tcp_listen(Socket, 5),
    tcp_open_socket(Socket, StreamPair),
    stream_pair(StreamPair, AcceptFd, _),
    format(atom(Alias), 'json_rpc_server@~w', [Port]),
    thread_create(json_rpc_connection_manager(Port, AcceptFd), _,
                  [ alias(Alias),
                    inherit_from(main)
                  ]).

json_rpc_connection_manager(Port, AcceptFd) :-
    tcp_accept(AcceptFd, Socket, Peer),
    flag(json_rpc_server, Count, Count+1),
    format(atom(Alias), 'json_rpc_server@~w:~w', [Port, Count]),
    thread_create(rpc_server(Socket, Peer), _,
                  [ detached(true),
                    alias(Alias),
                    inherit_from(main)
                  ]),
    json_rpc_connection_manager(Port, AcceptFd).

rpc_server(Socket, _Peer) :-
    setup_call_cleanup(
        tcp_open_socket(Socket, StreamPair),
        handle_service(StreamPair),
        close(StreamPair)).

handle_service(StreamPair) :-
    json_rpc_dispatch(StreamPair, []).


                /*******************************
                *            CLIENT            *
                *******************************/

:- dynamic
    client_connection/1.

connect(Port) :-
    tcp_connect(localhost:Port, Stream, []),
    asserta(client_connection(Stream)).

json_call(Goal, Result, Options) :-
    client_connection(Stream),
    json_call(Stream, Goal, Result, Options).

json_notify(Goal, Options) :-
    client_connection(Stream),
    json_notify(Stream, Goal, Options).

json_batch(Notifications, Goals, Results, Options) :-
    client_connection(Stream),
    json_batch(Stream, Notifications, Goals, Results, Options).
