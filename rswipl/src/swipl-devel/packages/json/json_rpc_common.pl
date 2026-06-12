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

:- module(json_rpc_common,
          [ json_rpc_send/3                 % +Stream, +Dict, +Options
          ]).
:- autoload(library(json), [json_write_dict/3]).
:- autoload(library(option), [option/2]).

%!  json_rpc_send(+Stream, +Dict, +Options)

json_rpc_send(Stream, Dict, Options) :-
    option(header(true), Options),
    !,
    with_output_to(string(Msg),
                   json_write_dict(current_output, Dict, Options)),
    utf8_length(Msg, Len),
    format(Stream,
           'Content-Length: ~d\r\n\r\n~s', [Len, Msg]),
    flush_output(Stream).
json_rpc_send(Stream, Dict, Options) :-
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
                *           MESSAGES           *
                *******************************/

:- multifile
    prolog:error_message//1,
    prolog:message//1.

prolog:error_message(json_rpc_error(Obj)) -->
    { is_dict(Obj) },
    json_rpc_error_message(Obj).
prolog:error_message(json_rpc_error(Obj, Id)) -->
    { is_dict(Obj) },
    [ '(async ~p) '-[Id] ],
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

prolog:message(json_rpc(Msg)) -->
    json_rpc_message(Msg).

json_rpc_message(not_implemented(Method, Params)) -->
    [ 'No implementation for ~p using paramenters ~p'-
      [Method, Params]
    ].
