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

:- module(json_rpc_server,
          [ (json_method)/1,                    % M1,M2,...
            json_rpc_dispatch/2,                % :Stream, +Options
            json_rpc_error/2,                   % +Code, +Message
            json_rpc_error/3,                   % +Code, +Message, +Data

            op(1100, fx, json_method)
          ]).
:- autoload(library(json),
            [json_read_dict/3, json_write_dict/3]).
:- autoload(library(apply), [maplist/3, include/3]).
:- autoload(library(error), [must_be/2]).
:- autoload(library(json_schema), [json_compile_schema/3, json_check/3]).
:- autoload(library(lists), [append/3]).
:- autoload(library(prolog_code), [extend_goal/3]).
:- use_module(library(debug), [debug/3, assertion/1]).

:- meta_predicate
    json_rpc_dispatch(:, +).

:- public
    json_rpc_dispatch_request/4.   % +M, +Stream, +Request, +Options

/** <module> JSON RPC Server

This module implements an JSON RPC server. It provides declarations that
bind Prolog predicates to JSON RPC methods and a dispatch loop that acts
on a bi-directional stream. This module assumes a two-directional stream
and provides json_rpc_dispatch/2 that  receiveds   JSON  messages on the
input side of this stream and sends the replies through the output. This
module does not implement obtaining such   a  stream. Obvious candidates
for obtaining a stream are:

  - Using standard I/O to a child process.  See process_create/3.
  - Using sockets.  See library(socket).  Using the SSL package this
    also provides secure sockets.
  - Using the HTTP package to extablish a _web socket_.

This library defines json_method/1 for declaring  predicates to act as a
JSON method. The  declaration  accepts   a  JSON  Schema  specification,
represented as a SWI-Prolog dict to specify the input parameters as well
as the output.

@see [JSON-RPC](https://www.jsonrpc.org/specification)
*/

                /*******************************
                *         DECLARATIONS         *
                *******************************/

%!  json_method(+Methods)
%
%   Methods is a comma-list of JSON RPC method declarations.   Each
%   declaration takes one of the forms below:
%
%      - Callable:Reply
%        Here, Callable is a Prolog callable term whose name and number
%        of argument match a predicate in this module.  The arguments
%        are JSON Schema types and Reply is a JSON Schema type.
%      - Callable
%        Callable is as above, but there is no return value.  This
%        implements JSON RPC _notifications_, i.e., asynchronously
%        processed messages for which we do not wait for a reply.
%
%   For example:
%
%   ```
%   :- json_method
%       subtract(#{type:number}, #{type:number}): #{type:number}.
%
%   subtract(A, B, R) :- R is A-B.
%   ```
%
%   Methods with _named arguments_ can  be   implemented  using a single
%   argument that is an object with   specified properties. For example,
%   the program below implements a  depositing   to  a bank account. The
%   method takes an `account` and `amount` parameter and returns the new
%   balance. The json_rpc_error/2 throws a JSON RPC _application error_.
%
%   ```
%   :- json_method
%       deposit(#{ properties:
%                  #{ account: #{type:string},
%                     amount:  #{type:number}
%                   }}): #{type:number},
%
%   deposit(Request, Reply),
%       #{account: Account, amount: Amount} :< Request =>
%       transaction((   retract(account(Account, Old))
%                   ->  New is Old+Amount,
%                       asserta(account(Account, New))
%                   ;   json_rpc_error(2, "Account does not exist")
%                   )),
%       Reply = New.
%   ```

json_method(Methods) :-
    throw(error(context_error(nodirective, json_method(Methods)), _)).

compile_methods((A,B)) ==>
    compile_methods(A),
    compile_methods(B).
compile_methods(M:Reply), callable(M) ==>
    { M =.. [Name|Args],
      argv_type(Args, Type),
      arg_type(Reply, RType)
    },
    [ '$json_method'(Name, Type, RType) ].
compile_methods(M), callable(M) ==>
    { M =.. [Name|Args],
      argv_type(Args, Type)
    },
    [ '$json_method'(Name, Type) ].

argv_type([Named], QType), is_dict(Named) =>
    arg_type(Named.put(type, "object"), Type),
    QType = named(Type).
argv_type([Args], Type), is_list(Args) =>
    maplist(arg_type, Args, Types),
    Type = positional(Types).
argv_type(Args, Type) =>
    maplist(arg_type, Args, Types),
    Type = positional(Types).

arg_type(Schema, Type) =>
    json_compile_schema(Schema, Type, []).

:- multifile system:term_expansion/2.

system:term_expansion((:- json_method(Methods)), Clauses) :-
    \+ current_prolog_flag(xref, true),
    phrase(compile_methods(Methods), Clauses).


                /*******************************
                *         DISPATCHING          *
                *******************************/

%!  json_rpc_dispatch(:Stream, +Options) is det.
%
%   Run the JSON RPC dispatch  loop  until   end  of  file is reached on
%   Stream.
%
%   @arg Stream is stream pair (see stream_pair/2). Normally, the stream
%   should use `utf8` encoding. If the  stream   is  a binary stream, it
%   will be processed as if `utf8` encoding is  enabled. If it is a text
%   stream the encoding of the stream is respected.

json_rpc_dispatch(M:Stream, Options) :-
    json_rpc_dispatch_1(M, Stream, EOF, Options),
    (   EOF == true
    ->  true
    ;   json_rpc_dispatch(M:Stream, Options)
    ).

:- det(json_rpc_dispatch_1/4).
json_rpc_dispatch_1(M, Stream, EOF, Options) :-
    Error = error(Formal,_),
    catch(json_read_dict(Stream, Request,
                         [ end_of_file(end_of_file(true))
                         | Options
                         ]),
          Error,
          true),
    debug(json_rpc(server), 'Request: ~p', [Request]),
    (   Request == end_of_file(true)
    ->  EOF = true
    ;   var(Formal)
    ->  json_rpc_dispatch_request(M, Stream, Request, Options)
    ;   print_message(error, Error)
    ).


%!  json_rpc_dispatch_request(+Module, +Stream, +Request, +Options)
%
%   Handle a request that has been read  from Stream, possibly sending a
%   reply to Stream.

json_rpc_dispatch_request(M, Stream, Requests, Options) :-
    is_list(Requests),
    !,                                          % batch processing
    maplist(json_rpc_result_r(M, Options), Requests, AllResults),
    include(nonvar, AllResults, Results),
    json_rpc_reply(Stream, Results, Options).
json_rpc_dispatch_request(M, Stream, Request, Options) :-
    json_rpc_result(M, Request, Result, Options),
    json_rpc_reply(Stream, Result, Options).

%!  json_rpc_reply(+Stream, +Result, +Options) is det.

json_rpc_reply(Stream, Result, Options),
    is_dict(Result),
    Id = Result.get(id) =>
    debug(json_rpc(server), 'Replying ~p for request ~p', [Result,Id]),
    with_output_to(Stream, json_write_dict(Stream, Result, Options)),
    flush_output(Stream).
json_rpc_reply(Stream, Results, Options), is_list(Results) =>
    debug(json_rpc(server), 'Replying batch results: ~p', [Results]),
    with_output_to(Stream, json_write_dict(Stream, Results, Options)),
    flush_output(Stream).
json_rpc_reply(_Stream, Result, _Options), var(Result) =>
    true.                                       % notification

json_rpc_result(M, Request, Result, Options) :-
    Error = error(_,_),
    catch(json_rpc_result_(M, Request, Result, Options),
          Error,
          json_exception_to_reply(Error, Request, Result)).

json_rpc_result_r(M, Options, Request, Result) :-
    json_rpc_result(M, Request, Result, Options).

:- det(json_rpc_result_/4).
json_rpc_result_(M, Request, Result, Options) :-
    (   #{jsonrpc: "2.0", method:MethodS, params:Params} :< Request
    ->  atom_string(Method, MethodS),
        (   Id = Request.get(id)
        ->  json_rpc_result(M, Method, Params, Id, Result, Options)
        ;   json_rpc_notify(M, Method, Params, Options)
        )
    ;   Id = Request.get(id)
    ->  Result = #{ jsonrpc: "2.0",
                    id: Id,
                    error: #{code: -32600,
                             message: "Invalid Request"}
                  }
    ;   print_message(error, json_rpc(invalid_request(Request)))
    ).

json_rpc_result(M, Method, Params0, Id, Reply, Options) :-
    M:'$json_method'(Method, Types, RType),
    !,
    check_params(Params0, Types, Params, Options),
    debug(json_rpc(server), 'Calling method ~q for request ~p', [Method,Id]),
    run_method(M:Method, Params, Result),
    json_check_result(RType, Result, Options),
    Reply = #{ jsonrpc: "2.0",
               result: Result,
               id: Id
             }.
json_rpc_result(M, Method, Params0, Id, Reply, Options) :-
    M:'$json_method'(Method, Types),
    !,
    check_params(Params0, Types, Params, Options),
    debug(json_rpc(server), 'Calling method ~q for request ~p', [Method,Id]),
    (   apply(M:Method, Params)
    ->  Result = true
    ;   Result = false
    ),
    Reply = #{ jsonrpc: "2.0",
               result: Result,
               id: Id
             }.
json_rpc_result(_M, Method, _Params, Id, Reply, _Options) :-
    Reply = #{ jsonrpc: "2.0",
               id: Id,
               error: #{ code: -32601,
                         message: "Method not found",
                         data: Method
                       }
             }.

check_params(Params, positional(Types), Params, Options) :-
    must_be(list, Params),
    maplist(json_check_param(Options), Types, Params),
    !.
check_params(Params, positional(Types), _Params, _Options) :-
    length(Types, Expected),
    length(Params, Found),
    format(string(Msg), "Expected ~d parameters, found ~d", [Expected, Found]),
    raise_param_error_data(Msg).
check_params(Param, named(Type), [Param], Options) :-
    json_check_param(Options, Type, Param).

json_rpc_notify(M, Method, Params0, Options) :-
    M:'$json_method'(Method, Types),
    !,
    check_params(Params0, Types, Params, Options),
    apply(M:Method, Params).
json_rpc_notify(M, Method, Params0, Options) :-
    M:'$json_method'(Method, Types, _RType),
    !,
    check_params(Params0, Types, Params, Options),
    run_method(M:Method, Params, _Result).

%!  json_exception_to_reply(+Error, +Request, -Reply) is det.
%
%   Turn an exception into a JSON RPC   error document if Request has an
%   `id` field. Else it is  a  notification,   so  we  simply  print the
%   message in the server.

:- det(json_exception_to_reply/3).
json_exception_to_reply(error(json_rpc_error(Dict),_), Request, Reply),
    Id = Request.get(id) =>
    assertion(#{code:_, message:_} :< Dict),
    Reply = #{ jsonrpc: "2.0",
               id: Id,
               error: Dict
             }.
json_exception_to_reply(Error, Request, Reply),
    Id = Request.get(id) =>
    message_to_string(Error, Msg),
    Reply = #{ jsonrpc: "2.0",
               id: Id,
               error: #{ code: -32603,
                         message: "Internal error",
                         data: Msg}
             }.
json_exception_to_reply(Error, _Request, _Reply) =>
    print_message(error, Error).

json_check_param(Option, Schema, Data) :-
    catch(json_check(Schema, Data, Option),
          Error,
          raise_param_error(Error)).

raise_param_error(Error) :-
    message_to_string(Error, Msg),
    raise_param_error_data(Msg).

raise_param_error_data(Msg) :-
    throw(error(json_rpc_error(#{ code: -32602,
                                  message: "Invalid params",
                                  data: Msg
                                }),
                _)).

json_check_result(Schema, Data, Options) :-
    catch(json_check(Schema, Data, Options),
          Error,
          raise_result_error(Error)).

raise_result_error(Error) :-
    message_to_string(Error, Msg),
    throw(error(json_rpc_error(#{ code: -32000,
                                  message: "Invalid return",
                                  data: Msg
                                }),
                _)).

run_method(Method, Params, Result) :-
    append(Params, [Result], Args),
    Error = error(_,_),
    (   catch(apply(Method, Args), Error,
              raise_run_error(Error))
    ->  true
    ;   throw(error(json_rpc_error(#{ code: -32002,
                                      message: "Execution failed"
                                    }),
                    _))
    ).

%!  raise_run_error(+Error)
%
%   Raised an error generated while running the   method. This can be an
%   application error raised  by  json_rpc_error/2,3   or  an  arbitrary
%   error.

raise_run_error(Error),
    Error = error(json_rpc_error(_),_) =>
    throw(Error).
raise_run_error(Error) =>
    message_to_string(Error, Msg),
    throw(error(json_rpc_error(#{ code: -32001,
                                  message: "Execution error",
                                  data: Msg
                                }),
                _)).


%!  json_rpc_error(+Code, +Message).
%!  json_rpc_error(+Code, +Message, +Data).
%
%   Normally  called  from  a   method    implementation   to  raise  an
%   _application error_.
%
%   @arg Code is an integer.  The range -32768 to -32000 is reserved for
%   JSON RPC server errors.
%   @arg Message is a short string decribing the error
%   @arg Data is optional JSON data that provides context for the error.
%   @error json_rpc_error(Dict), where `Dict` contains the JSON RPC
%   defined fields `code`, `message` and optionally `data`.

json_rpc_error(Code, Message) :-
    throw(error(json_rpc_error(#{ code: Code,
                                  message: Message
                                }),
                _)).
json_rpc_error(Code, Message, Data) :-
    throw(error(json_rpc_error(#{ code: Code,
                                  message: Message,
                                  data: Data
                                }),
                _)).


                /*******************************
                *           MESSAGES           *
                *******************************/

:- multifile prolog:error_message//1.

prolog:error_message(json_rpc_error(Obj)) -->
    { is_dict(Obj) },
    json_rpc_error_message(Obj).

json_rpc_error_message(Obj),
    Data = Obj.get(Data) ==>
    json_rpc_error_message_(Obj),
    [ nl, '   Data: ~p'-[Data] ].
json_rpc_error_message(Obj) ==>
    json_rpc_error_message_(Obj).

json_rpc_error_message_(Obj),
    #{code:Code, message:Message} :< Obj,
    between(-32768, -32000, Code) ==>
    [ 'JSON RPC error ~d: ~s'-[Code, Message] ].
json_rpc_error_message_(Obj),
    #{code:Code, message:Message} :< Obj ==>
    [ 'JSON RPC application error ~d: ~s'-[Code, Message] ].

                /*******************************
                *              IDE             *
                *******************************/

:- multifile
    prolog_colour:directive_colours/2,
    prolog:called_by/4.

prolog_colour:directive_colours(json_method(Decl),
                                expanded-[Colour]) :-
    decl_colours(Decl, Colour).

decl_colours((A,B), Colour) =>
    Colour = punctuation-[CA, CB],
    decl_colours(A, CA),
    decl_colours(B, CB).
decl_colours(Head:_Type, Colour) =>
    extend_goal(Head, [_Ret], ExHead),
    Colour = punctuation-[body(ExHead),classify].
decl_colours(Head, Colour), callable(Head) =>
    Colour = body.
decl_colours(_Error, Colour) =>
    Colour = error(method_expected).

prolog:called_by(json_method(Decl), _M, _C, Called) :-
    phrase(json_rpc_called_by(Decl), Called).

json_rpc_called_by((A,B)) ==>
    json_rpc_called_by(A),
    json_rpc_called_by(B).
json_rpc_called_by(Head:_Type) ==>
    { extend_goal(Head, [_Ret], ExHead)
    },
    [ExHead].
json_rpc_called_by(Head), callable(Head) ==>
    [Head].
json_rpc_called_by(_) ==>
    [].
