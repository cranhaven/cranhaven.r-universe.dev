/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021-2025, SWI-Prolog Solutions b.v.
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

:- module(json_schema,
          [ json_validate/3,                 % +Schema, +DataDict, +Options
            json_compile_schema/3,           % +Schema, -CompiledType, +Options
            json_check/3                     % +CompiledType, +Data, +Options
          ]).
:- use_module(library(apply_macros), []).
:- use_module(library(debug), [assertion/1, debug/3]).
:- autoload(library(apply), [maplist/3, partition/4, maplist/2, include/3]).
:- autoload(library(base64), [base64/2]).
:- autoload(library(error),
            [ is_of_type/2,
              existence_error/3,
              type_error/2,
              must_be/2,
              permission_error/3
            ]).
:- autoload(library(lists),
            [ member/2,
              nth0/3,
              append/3,
              select/3,
              selectchk/4,
              selectchk/3,
              delete/3
            ]).
:- autoload(library(option),
              [option/2, option/3, merge_options/3, select_option/4]).
:- autoload(library(pcre), [re_match/2]).
:- autoload(library(sgml), [xsd_time_string/3]).
:- autoload(library(uri),
            [ uri_file_name/2,
              uri_normalized/3,
              uri_components/2,
              uri_data/3,
              uri_encoded/3,
              uri_data/4,
              uri_authority_components/2,
              uri_authority_data/3
            ]).
:- if(exists_source(library(http/http_open))).
:- autoload(library(http/http_open), [http_open/3]).
:- endif.
:- autoload(library(url), [is_absolute_url/1]).
:- autoload(library(dicts), [dict_keys/2, mapdict/3, mapdict/2]).
:- autoload(library(json), [json_read_dict/2]).
:- autoload(library(pairs), [pairs_keys/2]).

:- multifile
    json_schema/2.                                % +URL, -Schema

/** <module> JSON Schema reader and validator

This module provides a JSON Schema reader  and validator. This module is
based on the 2020-12 draft of the specification.

The API consists of two primitives  and   a  simple high level predicate
(json_validate/3):

  - json_compile_schema/3 translates a file or parsed JSON schema data
    into a Prolog term that represents the schema in a way that is more
    suitable for checking a JSON document.
  - json_check/3
    validates a document against the compiled schema.

## Status {#json-schema-status}

The         implementation         is          validated         against
https://github.com/json-schema-org/JSON-Schema-Test-Suite.git.  It fails
4 out of 1,261 tests.  Issues:

  - ``$dynamicRef`` fails 3 tests, probably mixing up physical context
    and resolution context that defines the _dynamic scopes_.
  - There is no support for non-default vocabulary selection.

The current implementation is the result   of an incremental process. It
should  be  refactored  to  make  it    a  cleaner  translation  of  the
specification.

## Predicates {#json-schema-predicates}
*/

:- meta_predicate
    findall_vars(?, 0, ?, -).

%!  json_validate(+SchemaFile, +DataDict, +Options) is det.
%
%   Given a file holding a JSON Schema   and  a Prolog dict holding JSON
%   data, validate the data against the   schema.  Options are passed to
%   json_compile_schema/3 and json_check/3.
%
%   @throws error(Formal, json_path(Path)), where `Path`   is  a list of
%   properties from the root element to   the culprit element. Formal is
%   typically a type, domain or existence  error. This file contains the
%   message  hooks  to  generate  a  human  readable  error  from  these
%   exceptions using print_message/2.

json_validate(SchemaFile, DataDict, Options) :-
    json_compile_schema(SchemaFile, Type, Options),
    merge_options(Options,
                  [ on_error(error),
                    value_string_as(atom)
                  ], Options1),
    json_check(Type, DataDict, Options1).

%!  json_compile_schema(+Input, -Type, +Options) is det.
%
%   Load and translated a JSON Schema. Input   is  either a file name, a
%   specification   for   absolute_file_name/3   or    the   output   of
%   json_read_dict/2.
%
%   If Input is a file name, the   loaded and compiled schema is cached.
%   Reusing the cache validates the modification file of the schema file
%   and reloads it if the  file's  time   stamp  has  changed. Note that
%   `true` and `false` are valid schemas  and   cannot  be  used as file
%   names.

:- dynamic
    schema_cache/5.

json_compile_schema(JSON, Type, Options) :-
    is_dict(JSON),
    !,
    merge_options(Options, [base_uri('')], Options1),
    option(base_uri(Base), Options1),
    json_root_type(JSON, Base, Type, Options).
json_compile_schema(Bool, Type, _Options) :-
    boolean(Bool, Prolog),
    !,
    boolean_schema(Prolog, Type).
json_compile_schema(Spec, Type, Options) :-
    schema_cache(Spec, File, Options, Time, Type0),
    catch(time_file(File, Modified), error(_,_), fail),
    abs(Time-Modified) < 1,
    !,
    Type = Type0.
json_compile_schema(Spec, Type, Options) :-
    absolute_file_name(Spec, File,
                       [ extensions(['', json]),
                         access(read)
                       ]),
    uri_file_name(URI, File),
    json_read_file(File, JSON),
    json_root_type(JSON, URI, Type0, Options),
    time_file(File, Time),
    retractall(schema_cache(_,_,_,_,_)),
    asserta(schema_cache(Spec, File, Options, Time, Type0)),
    Type = Type0.

%!  json_root_type(+JSONSpec, +URI, -PrologType, +Options)

json_root_type(JSONSpec, URI, PrologType, Options) :-
    Id = JSONSpec.get('$id'), !,
    uri_normalized(Id, URI, Base),
    json_root_type_(JSONSpec, Base, PrologType, Options).
json_root_type(JSONSpec, URI, PrologType, Options) :-
    json_root_type_(JSONSpec, URI, PrologType, Options).

json_root_type_(JSON, URI, PrologType, Options) :-
    push_dynamic_scope(JSON, [], Scopes),
    json_type(JSON, PrologType,
              [ base_uri(URI),
                schema(JSON),           % The schema we are working on
                root(PrologType),       % Its type (may be incomplete)
                types(#{}),             % $id --> Type map
                schemas([URI-JSON|_]),  % URL --> JSON map (open list)
                dynamic_scopes(Scopes)  % Handle $dynamicRef
              | Options
              ]).

%!  json_type(+JSONSpec, -PrologType, +Options)
%
%   True when PrologType is a  canonical   Prolog  representation of the
%   JSON schema defines in JSONSpec.

json_type(true, any, _) :-
    !.
json_type(false, nothing, _) :-
    !.
json_type(JSON, Prolog, Options) :-
    Id = JSON.get('$id'),
    atomic(Id),                         % Avoid error on meta schema
    !,
    option(base_uri(Base0), Options),
    option(types(Types0), Options),
    option(dynamic_scopes(Scopes0), Options),
    uri_normalized(Id, Base0, Base),
    (   \+ JSON.get('$$nolink') == true,
        option(types(Types), Options),
        Type = Types.get(Base)
    ->  Prolog = Type
    ;   push_dynamic_scope(JSON, Scopes0, Scopes),
        merge_options([ base_uri(Base),
                        types(Types0.put(Base,Prolog)),
                        dynamic_scopes(Scopes)
                      ], Options, Options1),
        json_type_(JSON.put('$$nolink', true), Prolog, Options1)
    ).
json_type(JSON, Prolog, Options) :-
    json_type_(JSON, Prolog, Options).

%!  json_type_(+JSON, -PrologType, +Options)

json_type_(Spec, const(Const), Options) :-
    select_dict(#{const:Const}, Spec, Rest),
    !,
    json_type_(Rest, Type, Options),
    (   json_check(Type, Const, Options)
    ->  true
    ;   type_error(Type, Const)
    ).
json_type_(Spec, unevaluated_properties(Type, UnevalType), Options) :-
    select_dict(#{unevaluatedProperties:Uneval}, Spec, Rest),
    !,
    bool_or_type(Uneval, UnevalType, Options),
    json_type_(Rest, Type, Options).
json_type_(Spec, unevaluated_items(Type, UnevalType), Options) :-
    select_dict(#{unevaluatedItems:Uneval}, Spec, Rest),
    !,
    bool_or_type(Uneval, UnevalType, Options),
    json_type_(Rest, Type, Options).
json_type_(Spec, allOf([RestType, if_then_else(If,Then,Else)]), Options) :-
    select_dict(#{if:IfSpec, then:ThenSpec, else:ElseSpec},
                Spec, Rest,

                #{then: true, else: true}),
    !,
    json_type(IfSpec, If, Options),
    json_type(ThenSpec, Then, Options),
    json_type(ElseSpec, Else, Options),
    json_type_(Rest, RestType, Options).
json_type_(Spec, not(Type), Options) :-
    Disallow = Spec.get(disallow),
    !,
    explicit_type(Disallow, Spec, Type, Options).
json_type_(Spec, allOf(Types), Options) :-
    select_dict(#{allOf:List}, Spec, Rest),
    !,
    maplist(opts_json_type(Options), List, Types0),
    json_type_(Rest, Type, Options),
    (   Type == untyped
    ->  Types = Types0
    ;   append(Types0, [Type], Types)
    ).
json_type_(Spec, Type, Options) :-
    select_dict(#{anyOf:List}, Spec, Rest),
    !,
    maplist(opts_json_type(Options), List, Types),
    json_type_(Rest, RestType, Options),
    (   RestType == untyped
    ->  Type = anyOf(Types)
    ;   Type = allOf([RestType,anyOf(Types)])
    ).
json_type_(Spec, Type, Options) :-
    select_dict(#{oneOf:List}, Spec, Rest),
    !,
    maplist(opts_json_type(Options), List, Types),
    json_type_(Rest, RestType, Options),
    (   RestType == any
    ->  Type = oneOf(Types)
    ;   Type = allOf([RestType,oneOf(Types)])
    ).
json_type_(Spec, Type, Options) :-
    select_dict(#{not:NSpec}, Spec, Rest),
    !,
    json_type(NSpec, NType, Options),
    json_type_(Rest, RestType, Options),
    (   RestType == untyped
    ->  Type = not(NType)
    ;   Type = allOf([RestType,not(NType)])
    ).
json_type_(Spec, enum(Values), _) :-
    _{enum:Values} :< Spec,
    !.
json_type_(Spec, Type, _Options) :-
    empty_schema(Spec),
    !,
    Type = untyped.
json_type_(Spec, Type, Options) :-
    _{'$ref':_Ref} :< Spec,
    !,
    type_ref(Spec, Type, Options).
json_type_(Spec, Type, Options) :-
    _{'$dynamicRef':_Ref} :< Spec,
    !,
    dynamic_type_ref(Spec, Type, Options).
json_type_(Spec, Type, Options) :-
    SType = Spec.get(type),
    !,
    explicit_type(SType, Spec, Type, Options).
json_type_(Spec, Dict, Options) :-
    findall_vars(JSONType-Type,
                 ( basic_type(JSONType),
                   json_type(JSONType, Spec, Type, Options)
                 ), Options, Pairs),
    dict_pairs(Dict, any, Pairs).

basic_type(integer).
basic_type(number).
basic_type(string).
basic_type(object).
basic_type(array).

bool_or_type(BoolSpec, Bool, _Options) :-
    boolean(BoolSpec, Bool),
    !.
bool_or_type(TypeSpec, type(Type), Options) :-
    json_type(TypeSpec, Type, Options).

%!  explicit_type(+SpecifiedType, +Spec, -Type, +Options)
%
%   Deal with expicitly  specified  types   using  `{"type":  Type}`  or
%   `{disallow: Type}`

explicit_type(_SType, Spec, Type, Options) :-
    _{'$ref':_Ref} :< Spec,
    !,
    type_ref(Spec, Type, Options).
explicit_type(SType, Spec, FinalType, Options) :-
    (   is_list(SType)
    ->  partition(atomic, SType, Atomics, Schemas),
        maplist(atom_string, ATypes, Atomics),
        findall_vars(JSONType-Type,
                     ( member(JSONType, ATypes),
                       json_type(JSONType, Spec, Type, Options)
                     ), Options, Pairs),
        maplist(opts_json_type(Options), Schemas, SchemaTypes),
        join_types(Pairs, SchemaTypes, type-one_of(ATypes), FinalType)
    ;   atom_string(JSONType, SType),
        json_type(JSONType, Spec, FinalType, Options)
    ).

join_types(Pairs, [], TypePair, AType) :-
    !,
    dict_pairs(AType, type, [TypePair|Pairs]).
join_types([], SchemaTypes, _, anyOf(SchemaTypes)) :-
    !.
join_types(Pairs, SchemaTypes, TypePair, anyOf([AType|SchemaTypes])) :-
    dict_pairs(AType, type, [TypePair|Pairs]).

%!  json_type(+BasicType, +JSON, -PrologType, +Options) is semidet.
%
%   Produce a type description if JSON  contains the attributes demanded
%   by BasicType.  See basic_type/1.

json_type(object, Spec, Type, Options) =>
    Type = object(Constraints),
    object_properties(Spec, Constraints, Options).
json_type(array, Spec, Type, Options) =>
    Type = array(Constraints),
    array_properties(Spec, Constraints, Options).
json_type(JSONType, _, Type, _),
    nonvar(JSONType), simple_type(JSONType, Type0) =>
    Type = Type0.
json_type(JSONType, Spec, Type, _), var(JSONType) =>
    (   FormatS = Spec.get(format)
    ->  atom_string(Format, FormatS)
    ;   Format = '-'
    ),
    api_type(_, JSONType, Format, Type1),
    numeric_domain(Spec, JSONType, Type1, Type),
    Type \= numeric(_, []).
json_type(JSONType, Spec, Type, _),
    api_type(_, JSONType, -, Type1) =>
    numeric_domain(Spec, JSONType, Type1, Type).
json_type(_, _, _, _) =>
    fail.

%!  json_type_r(+Options, +JSON, -Prolog)
%
%   Maplist support predicate.

json_type_r(Options, JSON, Prolog) :-
    json_type(JSON, Prolog, Options).

simple_type(null, null).
simple_type(any, any).

opts_json_type(Options, Spec, Type) :-
    json_type(Spec, Type, Options).

pattern_property(Options, Pattern-Spec, Pattern-Type) :-
    json_type(Spec, Type, Options).

schema_property(Options, Name-Spec, p(Name, Type, _Req)) :-
    json_type(Spec, Type, Options).

%!  object_properties(+Spec, -Constraints, +Options) is semidet.
%
%   True when Constraints is a set of constraints to apply on an object.
%   Fails if there are no constraints on objects.

object_properties(Spec, Constraints, Options) :-
    findall_vars(C, object_property(Spec, C, Options), Options, Constraints0),
    split_dependencies(Constraints0, Constraints1, Options),
    join_required(Constraints1, C0),
    select_constraint(properties(Props0),            [],       C0, C1),
    select_constraint(additionalProperties(Add),     implicit, C1, C2),
    select_constraint(patternProperties(Patterns),   [],       C2, C3),
    apply_patterns(Patterns, Props0, Props),
    Constraints = [ properties(Props, Add, Patterns) | C3 ].

object_property(Spec, Constraint, Options) :-
    object_property_decl(P, Cvt, Options),
    Value0 = Spec.get(P),
    call(Cvt, Value0, Value),
    Constraint =.. [P,Value].

object_property_decl(properties,            cvt_property(Options),          Options).
object_property_decl(required,              cvt_required,                   _).
object_property_decl(additionalProperties,  cvt_bool_or_type(Options),      Options).
object_property_decl(patternProperties,     cvt_pattern_props(Options),     Options).
object_property_decl(dependencies,          =,								_).
object_property_decl(dependentRequired,     cvt_dependent_required,         _).
object_property_decl(dependentSchemas,      cvt_dependent_schemas(Options), Options).
object_property_decl(minProperties,         integral_min,                   _).
object_property_decl(maxProperties,         integral_max,                   _).
object_property_decl(propertyNames,         cvt_property_names(Options),    Options).

cvt_property(Options, PropSpecs, Props) :-
    dict_pairs(PropSpecs, _, Pairs),
    maplist(schema_property(Options), Pairs, Props0),
    sort(Props0, Props).

cvt_bool_or_type(Options, Spec, Type) :-
    bool_or_type(Spec, Type, Options).

cvt_required(Strings, Properties) :-
    maplist(atom_string, Properties, Strings).

cvt_pattern_props(Options, PProps, Patterns) :-
    dict_pairs(PProps, _, PropPairs),
    maplist(pattern_property(Options), PropPairs, Patterns).

%!  cvt_dependent_required(+Spec, -Required) is det.
%
%   Convert `dependentRequired`, which is a   dict with property-name ->
%   list of required property names. This  states   that  if  the key is
%   present as property, the dependendents must be present as well.

:- det(cvt_dependent_required/2).
cvt_dependent_required(Dict0, Dict) :-
    mapdict(dep_required_prop, Dict0, Dict).

dep_required_prop(_, Strings, Atoms) :-
    must_be(list, Strings),
    maplist(atom_string, Atoms, Strings).

%!  cvt_dependent_schemas(+Options, +Dict0, -Dict) is det.
%
%   Deal with `dependentSchemas`. The specification is an object mapping
%   property names to schemas.

:- det(cvt_dependent_schemas/3).
cvt_dependent_schemas(Options, Dict0, Dict) :-
    mapdict(dep_required_schema(Options), Dict0, Dict).

dep_required_schema(Options, _Key, TypeSpec, Type) :-
    json_type(TypeSpec, Type, Options).

cvt_property_names(Options, TypeSpec, Type) :-
    json_type(TypeSpec, Type, Options).

%!  split_dependencies(+ConstraintsIn, -Constraints, +Options) is det.
%
%   Up  to  draft-7,  `dependentRequired`  and  `dependentSchemas`  were
%   combine as `dependencies`. This splits the `dependencies` in the two
%   sections such the remainder of this code   need  not be aware of the
%   deprecated `dependencies` keyword.

:- det(split_dependencies/3).
split_dependencies(Constraints0, Constraints, Options) :-
    select(dependencies(Deps), Constraints0, Constraints1),
    !,
    dict_pairs(Deps, _, Pairs),
    partition(is_required_dependency, Pairs, RequiredP, SchemasP),
    dict_pairs(Required, #, RequiredP),
    dict_pairs(Schemas, #, SchemasP),
    cvt_dependent_required(Required, Required1),
    cvt_dependent_schemas(Options, Schemas, Schemas1),
    Constraints = [ dependentRequired(Required1),
                    dependentSchemas(Schemas1)
                  | Constraints1
                  ].
split_dependencies(Constraints, Constraints, _).

is_required_dependency(_Key-Value) :-
    maplist(atomic, Value).

%!  join_required(+Constraints0, -Constraints) is det.
%
%   Combine the `properties` and `required` constraints   into a list or
%   ordered p(Name,Type,Required) triples. Note that   this assumes that
%   these    two    properties     are      ordered     according     to
%   object_property_decl/3.

:- det(join_required/2).
join_required([properties(Props0), required(Reqs)|Rest],
              [properties(Props)|Rest]) :-
    !,
    join_required_(Props0, Reqs, RestReqs),
    maplist(required_prop, RestReqs, Props1),
    append(Props0, Props1, Props2),
    sort(Props2, Props).
join_required([properties(Props)|Rest],
              [properties(Props)|Rest]) :-
    !,
    maplist(not_required, Props).
join_required([required(Reqs)|Rest],
              [properties(Props)|Rest]) :-
    !,
    maplist(required_prop, Reqs, Props0),
    sort(Props0, Props).
join_required(Constraints, Constraints).

join_required_([], Reqs, Reqs).
join_required_([p(Name,_Type,true)|Props], Reqs0, Reqs) :-
    selectchk(Name, Reqs0, Reqs1),
    !,
    join_required_(Props, Reqs1, Reqs).
join_required_([p(_Name,_Type,false)|Props], Reqs0, Reqs) :-
    join_required_(Props, Reqs0, Reqs).

required_prop(Name, p(Name,any,true)).

not_required(p(_,_,false)).

%!  apply_patterns(+Patterns, +Props0, -Props) is det.
%
%   The `patternProperties` keyword also applies to explicit properties.
%   Here, we change  the  type  into   an  allOf()  type  containing the
%   explicit property and all matching patternProperty statements.

:- det(apply_patterns/3).
apply_patterns([], Props0, Props) =>
    Props = Props0.
apply_patterns(Patterns, Props0, Props) =>
    maplist(apply_patterns_to_prop(Patterns), Props0, Props).

apply_patterns_to_prop(Patterns,
                       p(Name, Type, Req),
                       p(Name, allOf([Type|PTypes]), Req)) :-
    findall(PType, ( member(Pattern-PType, Patterns),
                     re_match(Pattern, Name) ), PTypes),
    PTypes \== [],
    !.
apply_patterns_to_prop(_, Prop, Prop).

:- det(select_constraint/4).
select_constraint(C, _, Cs0, Cs) :-
    selectchk(C, Cs0, Cs),
    !.
select_constraint(C, Default, Cs, Cs) :-
    arg(1, C, Default).


%!  array_properties(+Spec, -Constraints, +Options) is det.
%
%   True when Constraints is a list of constraints on an array according
%   to Spec. The `contains`, `maxContains`   and  `minContains` keywords
%   are  translated  to  a  single    term  contains(Type,  MinContains,
%   MaxContains). The `prefixItems` and `items` are combined to a single
%   term items(PrefixItems, Items).

array_properties(Spec, Constraints, Options) :-
    select_dict(_{items: PrefixItems}, Spec, Spec1),
    is_list(PrefixItems),
    !,                                   % Deprecated (draft-7 and older)
    (   select_dict(_{additionalItems: Items}, Spec1, Spec2)
    ->  true
    ;   Items = true,
        Spec2 = Spec1
    ),
    array_properties(Spec2.put(#{prefixItems:PrefixItems, items:Items}),
                     Constraints, Options).
array_properties(Spec, Constraints, Options) :-
    findall_vars(C, array_property(Spec, C, Options), Options, Constraints0),
    (   select(minContains(Min), Constraints0, Constraints1)
    ->  true
    ;   Min = 1,
        Constraints1 = Constraints0
    ),
    (   select(maxContains(Max), Constraints1, Constraints2)
    ->  true
    ;   Max = infinite,
        Constraints2 = Constraints1
    ),
    (   selectchk(contains(Type), Constraints2, contains(Type,Min,Max), Constraints3)
    ->  true
    ;   Constraints3 = Constraints2
    ),
    select_constraint(prefixItems(PrefixItems), [],      Constraints3, Constraints4),
    select_constraint(items(Items),             untyped, Constraints4, Constraints5),
    Constraints = [items(PrefixItems, Items)|Constraints5].

%!  array_property(+JSONSpec, -Property, +Options) is nondet.
%
%   True when Property is an array constraint defined in JSONSpec.

array_property(Spec, Property, Options) :-
    is_dict(Spec),
    array_property_decl(P, Convert, Options),
    Value0 = Spec.get(P),
    call(Convert, Value0, Value),
    Property =.. [P,Value].

array_property_decl(items,       json_type_r(Options), Options).
array_property_decl(uniqueItems, boolean,              _).
array_property_decl(prefixItems, cvt_types(Options),   Options).
array_property_decl(minItems,    integral_min,         _).
array_property_decl(maxItems,    integral_max,         _).
array_property_decl(contains,    json_type_r(Options), Options).
array_property_decl(minContains, integral_min,         _).
array_property_decl(maxContains, integral_max,         _).

:- public boolean/2.
boolean(true,     true).
boolean(false,    false).
boolean(@(true),  true).
boolean(@(false), false).

boolean_schema(true, any).
boolean_schema(false, nothing).

cvt_types(Options, Specs, Types) :-
    maplist(json_type_r(Options), Specs, Types).

%!  integral_max(+Spec, -Max) is det.
%!  integral_min(+Spec, -Min) is det.
%
%   Deal with the argument of `minItems`   and  `maxItems`. These may be
%   unspecified. They must be non-negative. They can be floats, implying
%   the first integer below/above the given value.

integral_max(Var, Max), var(Var)             => Max = infinite.
integral_max(Num, Max), number(Num), Num >=0 => Max is floor(Num).
integral_max(Trm, _)                         => type_error(nonneg, Trm).

integral_min(Var, Min), var(Var)             => Min = 0.
integral_min(Num, Min), number(Num), Num >=0 => Min is ceil(Num).
integral_min(Trm, _)                         => type_error(nonneg, Trm).

%!  numeric_domain(+Spec, +JSONType, +TypeIn, -TypeOut)

numeric_domain(Spec, Type0, Type1, Type) :-
    numeric_type(Type0),
    !,
    (   _{minimum:Min, maximum:Max} :< Spec
    ->  (   _{exclusiveMaximum:true,
              exclusiveMinimum:true} :< Spec
        ->  Type = numeric(Type1, [>(Min), <(Max)|More])
        ;   _{exclusiveMaximum:true} :< Spec
        ->  Type = numeric(Type1, [>=(Min), <(Max)|More])
        ;   _{exclusiveMinimum:true} :< Spec
        ->  Type = numeric(Type1, [>(Min), =<(Max)|More])
        ;   Type = numeric(Type1, [>=(Min), =<(Max)|More])
        )
    ;   _{minimum:Min, exclusiveMinimum:true} :< Spec % deprecated
    ->  Type = numeric(Type1, [>(Min)|More])
    ;   _{exclusiveMinimum:Min} :< Spec, number(Min)  % 2020-12
    ->  Type = numeric(Type1, [>(Min)|More])
    ;   _{minimum:Min} :< Spec
    ->  Type = numeric(Type1, [>=(Min)|More])
    ;   _{maximum:Max, exclusiveMaximum:true} :< Spec % deprecated
    ->  Type = numeric(Type1, [<(Max)|More])
    ;   _{exclusiveMaximum:Max} :< Spec, number(Max)  % 2020-12
    ->  Type = numeric(Type1, [<(Max)|More])
    ;   _{maximum:Max} :< Spec
    ->  Type = numeric(Type1, [=<(Max)|More])
    ;   Type = numeric(Type1, More)
    ),
    (   (   _{divisibleBy:Num} :< Spec % old
        ;   _{multipleOf:Num} :< Spec  % current (2020-12)
        )
    ->  More = [multiple_of(Num)]
    ;   More = []
    ).
numeric_domain(Spec, string, Format, string(Format, Constraints)) :-
    !,
    findall(C, string_property(Spec, C), Constraints).
numeric_domain(_, _Type0, Type, Type).

numeric_type(integer).
numeric_type(number).

string_property(Spec, Prop) :-
    string_property_decl(P, Normalize),
    get_dict(P, Spec, Value0),
    call(Normalize, Value0, Value),
    Prop =.. [P,Value].

string_property_decl(maxLength,        integral_max).
string_property_decl(minLength,        integral_min).
string_property_decl(pattern,          =).
string_property_decl(contentEncoding,  =).
string_property_decl(contentMediaType, =).
string_property_decl(contentSchema,    =).

%!  type_ref(+Spec, -Type, +Options).
%!  type_ref(+Ref:string, +Spec, -Type, +Options)
%
%   Handle a ``$ref`` attribute.
%
%   @arg Spec is the dict that contains  `"$ref": Ref`. We must also use
%   the properties thereof.

type_ref(Spec, Type, Options) :-
    select_dict(_{'$ref':Ref}, Spec, Rest),
    type_ref(Ref, Rest, Type, Options).

type_ref("#", _, Type, Options) :-
    option(root(RootType), Options),
    !,
    Type = RootType.
type_ref(URLS, Spec, Type, Options) :-
    atom_concat(#, Fragment, URLS),
    option(schema(Schema), Options),
    !,
    json_fragment(Fragment, Schema, SubDoc, [ref(relative)|Options]),
    join_type_spec(Spec, SubDoc, Combined),
    push_dynamic_scope_option(SubDoc, Options, Options1),
    json_type(Combined, Type, Options1).
type_ref(URLS, _, Type, Options) :-
    option(base_uri(Base), Options),
    uri_normalized(URLS, Base, URL),
    option(types(Types), Options),
    Type = Types.get(URL),
    !.
type_ref(URLS, Spec, Type, Options) :-
    option(base_uri(Base), Options),
    uri_normalized(URLS, Base, URL),
    (   url_type_doc(URL, Doc, SubDoc, Options)
    ->  atom_string(NewBase, URL),
        update_id(Doc, SubDoc, SubDoc1, NewBase),
        join_type_spec(Spec, SubDoc1, Combined),
        select_option(root(_), Options, Options1, _),
        (   SubDoc1 == Doc
        ->  Extra = [root(Type)]
        ;   Extra = []
        ),
        merge_options([ base_uri(NewBase),
                        schema(Doc)
                      | Extra
                      ], Options1, Options2),
        push_dynamic_scope_option(SubDoc1, Options2, Options3),
        json_type(Combined, Type, Options3)
    ;   Type = url(URL)
    ).

%!  update_id(+Doc, -SubDoc0, -SubDoc, +BaseURI) is det.
%
%   We extracted Doc at BaseURI and Doc has a `'$id'` holding a relative
%   URL. Now, if SubDoc0 == Doc,   json_type/3  will process the `'$id'`
%   again. This is fine if the relative URI is a normal file, but not if
%   it is a directory. In that case we will end up with the relative URI
%   being applied twice. We solve this by  injecting the absolute URI as
%   `'$id'`.

update_id(Doc, Doc, SubDoc, NewBase) :-
    _Id = Doc.get('$id'),
    !,
    SubDoc = Doc.put('$id', NewBase).
update_id(_, SubDoc, SubDoc, _).

%!  dynamic_type_ref(+Spec, -Type, +Options) is det.
%
%   Handle ``$dynamicRef``.

dynamic_type_ref(Spec, Type, Options) :-
    select_dict(_{'$dynamicRef':Ref}, Spec, Rest),
    dynamic_type_ref(Ref, Rest, Type, Options).

dynamic_type_ref(Ref, Spec, Type, Options) :-
    atom_concat(#, Anchor, Ref),
    \+ sub_atom(Anchor, _, _, _, /),
    option(dynamic_scopes([Self|_]), Options),
    json_fragment(Anchor, Self, RefSubDoc, [ref(relative)|Options]),
    !,
    dynamic_type_ref_2(Anchor, Spec, RefSubDoc, Type, Options).
dynamic_type_ref(Ref, Spec, Type, Options) :-
    sub_atom(Ref, 0, _, _, '#/'),      %" $dynamicRef": "#/<pointer>" is "$ref"
    !,
    type_ref(Ref, Spec, Type, Options).
dynamic_type_ref(Ref, Spec, Type, Options) :-
    option(base_uri(Base), Options),
    uri_fragment(Ref, Anchor),
    uri_normalized(Ref, Base, URL),
    url_type_doc(URL, _Doc,  RefSubDoc, Options),
    !,
    dynamic_type_ref_2(Anchor, Spec, RefSubDoc, Type, Options).

dynamic_type_ref(Ref, Spec, Type, Options) :-
    type_ref(Ref, Spec, Type, Options).

dynamic_type_ref_2(Anchor, Spec, RefSubDoc, Type, Options) :-
    option(dynamic_scopes([Self|Scopes]), Options),
    (   atom_string(Anchor, RefSubDoc.get('$dynamicAnchor')),
        (   empty_schema(RefSubDoc)
        ->  debug(json_schema(dynamic_ref), 'Empty: ~p', [RefSubDoc])
        ;   json_contains([Self|Scopes], InScope),
            InScope == RefSubDoc
        ->  debug(json_schema(dynamic_ref), 'In scope: ~p', [RefSubDoc])
        ;   debug(json_schema(dynamic_ref), 'Non-empty: ~p', [RefSubDoc]),
            fail
        ),
        member(Doc, Scopes),
        json_fragment(Anchor, Doc, SubDoc,
                      [ dynamic(Self),
                        ref(relative),
                        on_error(silent)
                      | Options
                      ]),
        RefSubDoc \== SubDoc           % already resolved to this
    ->  true
    ;   SubDoc = RefSubDoc
    ),
    join_type_spec(Spec, SubDoc, Combined),
    push_dynamic_scope_option(SubDoc, Options, Options1),
    json_type(Combined, Type, Options1).


%!  push_dynamic_scope_option(+Spec, +OptionsIn, =Options)
%
%   Update the option dynamic_scopes(List) to by pushing the scope Spec.
%   We must push a scope while (1)   following a '$ref' or '$dynamicRef'
%   or (2) entering a node  with  a   '$id'.  We  should  not push twice
%   however if we follow to a ref holding a '$id'.

push_dynamic_scope_option(JSONSpec, Options0, Options) :-
    option(dynamic_scopes(Scopes0), Options0, []),
    push_dynamic_scope(JSONSpec, Scopes0, Scopes),
    merge_options([dynamic_scopes(Scopes)], Options0, Options).

push_dynamic_scope(JSON, Scopes, Scopes) :-
    Scopes = [Current|_],
    JSON == Current,                   % Already pushed.
    !.
push_dynamic_scope(JSON, Scopes0, Scopes) :-
    Scopes = [JSON|Scopes0].

%!  join_type_spec(+Local, +Reffed, -Spec) is det.
%
%   Local is a dict holding the  properties   of  the '$ref' dict except
%   '$ref' itself. Reffed is the references  type. We must combine these
%   to get the final type.

join_type_spec(Local, Reffed, Reffed) :-
    empty_schema(Local),
    !.
join_type_spec(Local, true, Local) :-
    !.
join_type_spec(_Local, false, false) :-
    !.
join_type_spec(Local, Reffed, Spec) :-
    Spec = #{allOf: [Reffed,Local]}.

%!  empty_schema(+Schema) is semidet.
%
%   True if Schema contains no JSON schema semantics. Currently detected
%   by excluding properties that have no   semantics. Possibly we should
%   have a set of properties that do have semantics?

empty_schema(_{}) :-
    !.
empty_schema(Dict) :-
    dict_keys(Dict, Keys),
    maplist(annotation_property, Keys),
    !.

%!  annotation_property(?Property)
%
%   True when Property is a  property  that   adds  no  semantics to the
%   schema node.

annotation_property('$id').
annotation_property('$anchor').
annotation_property('$dynamicAnchor').
annotation_property('$schema').
annotation_property('$comment').
annotation_property('$vocabulary').
annotation_property('$recursiveRef').
annotation_property('$recursiveAnchor').
annotation_property('title').
annotation_property('description').
annotation_property('examples').


%!  url_type_doc(+URL, -Doc, -SubDoc, +Options) is semidet.
%
%   Fetch the JSON document referenced by   URL, returning both the full
%   document and the SubDoc pointed at by the (optional) fragment.

url_type_doc(URL, Doc, SubDoc, Options) :-
    uri_components(URL, Components),
    uri_data(scheme, Components, file),
    !,
    uri_data(path, Components, FileEnc),
    uri_data(fragment, Components, Fragment),
    uri_data(fragment, Components, _, Components2),
    uri_components(FileURL, Components2),
    uri_encoded(path, File, FileEnc),
    json_read_file(File, Doc),
    json_fragment(Fragment, Doc, SubDoc,
                  [ base_uri(FileURL),
                    ref(absolute)
                  | Options
                  ]).
url_type_doc(URL, Doc, SubDoc, Options) :-
    uri_components(URL, Components),
    uri_data(fragment, Components, Fragment),
    uri_data(fragment, Components, _, Components2),
    uri_components(FetchURL, Components2),
    url_fetch_json(FetchURL, Doc, Options),
    json_fragment(Fragment, Doc, SubDoc,
                  [ base_uri(FetchURL),
                    ref(absolute)
                  | Options
                  ]).

%!  url_fetch_json(+FetchURL:atom, -Doc, +Options) is det.
%
%   Fetch a JSON document referenced by a   URI.  This document can be a
%   sub-document using ``$id: URI``.

url_fetch_json(FetchURL, Doc, Options) :-
    option(schemas(Schemas), Options),
    open_list_member(_UIR-Schema, Schemas),
    option(base_uri(Base0), Options),
    json_contains(Schema, Doc, Base0, Base),
    is_dict(Doc),
    _{'$id': Id} :< Doc,
    atomic(Id),
    (   Base == FetchURL
    ->  true	% Fixes refRemote test schema "base URI change - change folder"
    ;   uri_normalized(Id, Base, FetchURL)
    ),
    !.
url_fetch_json(FetchURL, Doc, Options) :-
    option(schemas(Schemas), Options, _),
    (   open_list_member(FetchURL-JSON, Schemas)
    ->  Doc = JSON
    ;   fake_url(FetchURL)
    ->  permission_error(access, url, FetchURL)
    ;   load_json_from_http(FetchURL, JSON),
        open_list_add(FetchURL-JSON, Schemas)
    ->  Doc = JSON
    ).

:- if(current_predicate(http_open/3)).
load_json_from_http(URL, JSON) :-
    setup_call_cleanup(
        http_open(URL, In, []),
        json_read_dict(In, JSON),
        close(In)).
:- endif.
load_json_from_http(URL, _) :-
    permission_error(access, url, URL).


fake_url(URL) :-
    uri_components(URL, Components),
    uri_data(authority, Components, Authority),
    nonvar(Authority),
    uri_authority_components(Authority, AuthComponents),
    uri_authority_data(host, AuthComponents, Host),
    fake_host(Host).

fake_host('www.example.com').
fake_host('example.com').

%!  json_fragment(+Fragment, +Document, -SubDoc, +Options) is semidet.
%
%   Find a sub document from  an   Fragment  locator. If Fragment starts
%   with `/`, we must follow the property   path from the root. Else, we
%   must look for documents holding ``$anchor`` or ``$dynamicAnchor``.
%   Specific options:
%
%     - ref(Type)
%       One of `relative` or `absolute`.

json_fragment(Fragment, Doc, SubDoc, _) :-
    var(Fragment),
    !,
    SubDoc = Doc.
json_fragment(FragmentEnc, Doc, SubDoc, Options) :-
    uri_encoded(fragment, Fragment, FragmentEnc),
    atomic_list_concat(SegmentsEnc, /, Fragment),
    maplist(tilde_unescape, SegmentsEnc, Segments),
    json_subdoc(Segments, Doc, SubDoc, Options).

tilde_unescape(In, Out) :-
    sub_atom(In, _, _, _, ~),
    !,
    atom_codes(In, Codes),
    phrase(tilde_unescape(OutCodes), Codes),
    atom_codes(Out, OutCodes).
tilde_unescape(In, In).

tilde_unescape([H|T]) -->
    "~", tunescape(H), !,
    tilde_unescape(T).
tilde_unescape([H|T]) -->
    [H], !,
    tilde_unescape(T).
tilde_unescape([]) -->
    [].

tunescape(0'~) --> "0".
tunescape(0'/) --> "1".

%!  json_subdoc(+Segments, +Doc, -SubDoc, +Options) is semidet.
%
%   Find a JSON sub document of Doc   according to Segments. If there is
%   only one segment, we must  find  an   anchored  node.  Else  we must
%   dereference a path.

json_subdoc([''], Doc, SubDoc, _Options) =>
    SubDoc = Doc.
json_subdoc([Anchor], Doc, SubDoc, Options) =>
    json_anchored(Anchor, Doc, SubDoc, Options).
json_subdoc([''|Segments], Doc, SubDoc, Options) =>
    json_extract_path(Segments, Doc, SubDoc, Options).

json_extract_path([], Doc, Doc, _).
json_extract_path([H|T], Doc, Sub, Options) :-
    (   is_dict(Doc)
    ->  Sub0 = Doc.H
    ;   is_list(Doc),
        atom_number(H, N),
        nth0(N, Doc, Sub0)
    ->  true
    ;   option(on_error(silent), Options)
    ->  fail
    ;   existence_error(json_path, H, Doc)
    ),
    json_extract_path(T, Sub0, Sub, Options).

%!  json_anchored(+Anchor:atom, +Doc, -SubDoc, +Options) is semidet.
%
%   Find an anchored sub schema.  Options
%
%     - ref(Type)
%       If `relative`, we are dereferencing `#anchor`, which we can find
%       anywhere in the document. If `absolute`, we must find the anchor
%       in the referenced document.
%     - dynamic(Self)
%       Find a dynamic anchor.

json_anchored(Anchor, Doc, SubDoc, Options) :-
    option(base_uri(Base0), Options),
    option(dynamic(Self), Options, NotDynamic),
    json_contains(Doc, SubDoc, Base0, Base),
    is_dict(SubDoc),
    (   Self == NotDynamic
    ->  (   atom_string(Anchor, SubDoc.get('$anchor'))
        ;   atom_string(Anchor, SubDoc.get('$dynamicAnchor'))
        )
    ;   \+ ( json_contains(Self, Sub),
             Sub == SubDoc
           ),
        atom_string(Anchor, SubDoc.get('$dynamicAnchor'))
    ),
    (   option(ref(relative), Options)
    ->  true
    ;   Base == Base0
    ),
    !.

%!  json_contains(+Doc, -SubDoc, +Base0, -Base) is nondet.
%
%   True when SubDoc is a node (possibly a   leave) from Doc and Base is
%   the base URI of SubDoc. This  predicate   is  used  to find anchored
%   nodes.
%
%   @arg Base0 is the context base uri.

json_contains(Doc, SubDoc, Base0, Base) :-
    update_base_uri(Doc, Base0, Base1),
    json_contains_(Doc, SubDoc, Base1, Base).

json_contains_(Doc, Doc, Base, Base).
json_contains_(Doc, SubDoc, Base0, Base) :-
    is_dict(Doc),
    !,
    get_dict(_, Doc, SubDoc0),
    json_contains(SubDoc0, SubDoc, Base0, Base).
json_contains_(Doc, SubDoc, Base0, Base) :-
    is_list(Doc),
    member(SubDoc0, Doc),
    json_contains(SubDoc0, SubDoc, Base0, Base).

update_base_uri(Doc, Base0, Base) :-
    is_dict(Doc),
    Id = Doc.get('$id'),
    atomic(Id),           % Avoid error on property definition in meta schema
    !,
    uri_normalized(Id, Base0, Base).
update_base_uri(_, Base, Base).

json_contains(Doc, Doc).
json_contains(Doc, SubDoc) :-
    is_dict(Doc),
    !,
    get_dict(_, Doc, SubDoc0),
    json_contains(SubDoc0, SubDoc).
json_contains(Doc, SubDoc) :-
    is_list(Doc),
    member(SubDoc0, Doc),
    json_contains(SubDoc0, SubDoc).

%!  json_read_file(+File, -Term) is det.
%
%   Read a JSON Scheme file.

json_read_file(File, Term) :-
    setup_call_cleanup(
        open(File, read, In, [encoding(utf8)]),
        json_read_dict(In, Term),
        close(In)).

		 /*******************************
		 *            TYPES		*
		 *******************************/

%!  api_type(?Name, ?Type, ?Format, ?TypeID)
%
%   Define the formats defined by the OAS.
%
%   @arg Name is unused
%   @arg Type is the basic JSON type
%   @arg Format is the JSON schema _format_
%   @arg TypeID is a local alias for the type that encodes both the
%   JSON type and format.

api_type(integer,  integer,       int32,           int32).
api_type(long,     integer,       int64,           int64).
api_type(long,     integer,       -,               integer).
api_type(float,    number,        float,           float).
api_type(double,   number,        double,          float).
api_type(double,   number,        -,               number).
api_type(string,   string,        -,               string).
api_type(byte,     string,        byte,            base64).
api_type(binary,   string,        binary,          binary).
api_type(boolean,  boolean,       -,               boolean).
api_type(date,     string,        date,            date).
api_type(dateTime, string,        'date-time',     date_time).
api_type(password, string,        password,        password).
api_type(regex,    string,        regex,           regex).
api_type(email,    string,        email,           email).
api_type(uri,      string,        uri,             uri).
api_type(uri,      string,        'uri-reference', uri_reference).

%!  json_check(+Spec, ?JSON, +Options) is semidet.
%
%   Validate a JSON object.  Spec  is   a  Prolog  representation of the
%   schema that is optimized  for   validation.  This  representation is
%   derived from JSON data using json_compile_schema/3.  Options:
%
%     - on_error(Mode)
%       What to do if an error is found.  Defined modes are
%       - error
%         Raise an exception.  This is the default.  Note that
%         only the first error is reported this way.
%       - warning
%         Print a message
%       - silent
%         Fail
%     - value_string_as(Type)
%       Same as for json_read/3.
%
%   This predicate is often  used   through  validate_json_dict/3, which
%   mantains a cached mapping from the JSON Schema to Spec.

json_check(Spec, In, Options) :-
    json_check_(Spec, In, Options).

json_check_silent(Type, In, Options) :-
    dict_create(Dict, options, Options),
    json_check(Type, In, [], Dict.put(on_error, silent)).

json_check_(Spec, In, Options) :-
    dict_create(Dict, options, Options),
    json_check(Spec, In, [], Dict).

%!  json_check(+Type, +Data, +Path, +Options:dict) is semidet.
%
%   Check Data to satisfy Type.  Path  is   a  list  representing a JSON
%   pointer from the root. The  Path  is   used  for  error  message. By
%   default, this predicate raises an exception  on error. If the option
%   #{on_error: silent} is present it fails silently.
%
%   @throw error(Error, json_path(Path)).

json_check(unevaluated_properties(Type, UnevalType), Data, Path, Options) :-
    is_dict(Data),
    !,
    Data2 = Data.put('$evaluated$', []),
    json_check2(Type, Data2, Path, Options),
    check_unevaluated(UnevalType, Data2, Path, Options),
    join_has_evaluated(Data, Data2).
json_check(unevaluated_properties(Type, _UnevalType), Data, Path, Options) :-
    !,
    json_check2(Type, Data, Path, Options).
json_check(unevaluated_items(Type, Uneval), Data, Path, Options) :-
    !,
    (   is_list(Data)
    ->  array_type(Type, AType),
        json_check2(AType, array_evaluated(Data, Evd), Path, Options),
        check_unevaluated_items(Uneval, Data, Evd, Path, Options)
    ;   Data = array_evaluated(Data2, Evd0)
    ->  array_type(Type, AType),
        json_check2(AType, array_evaluated(Data2, Evd), Path, Options),
        check_unevaluated_items(Uneval, Data2, Evd, Path, Options),
        Evd = Evd0     % join evaluated
    ;   json_check2(Type, Data, Path, Options)
    ).
json_check(Type, Data, Path, Options) :-
    json_check2(Type, Data, Path, Options).

json_check2(Dict, In, Path, Options) :-            % Explicit types
    is_dict(Dict),
    #{type: one_of(TypeNames)} :< Dict,
    !,
    (   member(TypeName, TypeNames),
        Type = Dict.get(TypeName),
        json_check_silent(Type, In, Options)
    ->  true
    ;   json_error(type_error(oneOf(TypeNames), In), Path, Options)
    ).
json_check2(Dict, In, Path, Options) :-            % only constraints
    is_dict(Dict, How),
    !,
    (   integer(In),
        Type = Dict.get(integer)
    ->  json_check(Type, In, Path, Options)
    ;   number(In),
        Type = Dict.get(number)
    ->  json_check(Type, In, Path, Options)
    ;   string(In),
        Type = Dict.get(string)
    ->  json_check(Type, In, Path, Options)
    ;   atom(In),
        not_json_special(In, Options),
        Type = Dict.get(string)
    ->  json_check(Type, In, Path, Options)
    ;   atom(In), boolean(In),
        Type = Dict.get(boolean)
    ->  json_check(Type, In, Path, Options)
    ;   In == null,
        Type = Dict.get(null)
    ->  json_check(Type, In, Path, Options)
    ;   is_dict(In, _),
        Type = Dict.get(object)
    ->  json_check(Type, In, Path, Options)
    ;   is_array(In)
    ->  Type = Dict.get(array),
        json_check(Type, In, Path, Options)
    ;   How == any
    ->  true
    ;   How == untyped
    ->  true
    ;   json_error(type_error(Dict.type, In), Path, Options)
    ).
json_check2(url(URL), In, Path, Options) :-
    !,
    (   json_schema(URL, Type)
    ->  json_check(Type, In, Path, Options)
    ;   json_error(existence_error(json_schema, URL), Path, Options)
    ).
json_check2(object(Constraints), In, Path, Options) :-
    !,
    json_must_be(object, In, Path, Options),
    json_check_object_constraints(Constraints, In, Path, Options).
json_check2(array(Constraints), array_evaluated(In, Evd), Path, Options) :-
    !,
    json_must_be(array, In, Path, Options),
    json_check_array_constraints(Constraints, In, Evd, Path, Options).
json_check2(array(Constraints), In, Path, Options) :-
    !,
    json_must_be(array, In, Path, Options),
    json_check_array_constraints(Constraints, In, _Evd, Path, Options).
json_check2(oneOf(Types), In, Path, Options) :-
    !,
    (   SOptions = Options.put(on_error, silent),
        E = error(_,_),
        append(_, [Type|Rest], Types),
        catch(json_check(Type, In, Path, SOptions), E, fail)
    ->  (   member(T2, Rest),
            catch(json_check(T2, In, Path, SOptions), E, fail)
        ->  json_error(type_error(oneOf(Types), In), Path, Options)
        ;   true
        )
    ;   json_error(type_error(oneOf(Types), In), Path, Options)
    ).
json_check2(allOf(Types), In, Path, Options) :-
    !,
    maplist(json_check_in_type(In, Path, Options), Types).
json_check2(anyOf(Types), In, Path, Options) :-
    !,
    (   SOptions = Options.put(on_error, silent),
        (   is_dict(In),
            _ = In.get('$evaluated$')
        ->  include(try_check(In, Path, SOptions), Types, Satisfied),
            Satisfied \== []
        ;   In = array_evaluated(_, _)
        ->  include(try_check(In, Path, SOptions), Types, Satisfied),
            Satisfied \== []
        ;   E = error(_,_),
            member(Type, Types),
            catch(json_check(Type, In, Path, SOptions), E, fail)
        )
    ->  true
    ;   json_error(type_error(anyOf(Types), In), Path, Options)
    ).
json_check2(not(Type), In, Path, Options) :-
    !,
    (   SOptions = Options.put(on_error, silent),
        E = error(_,_),
        \+ catch(json_check(Type, In, Path, SOptions), E, fail)
    ->  true
    ;   json_error(type_error(not(Type), In), Path, Options)
    ).
json_check2(if_then_else(If,Then,Else), In, Path, Options) :-
    (   json_check_silent(If, In, Options)
    ->  json_check(Then, In, Path, Options)
    ;   json_check(Else, In, Path, Options)
    ).
json_check2(enum(Values), In, Path, Options) :-
    !,
    (   memberchk(In, Values)
    ->  true
    ;   member(Candidate, Values),
        json_equal(Candidate, In, Options)
    ->  true
    ;   json_error(domain_error(oneof(Values), In), Path, Options)
    ).
json_check2(numeric(Type, Constraints), In, Path, Options) :-
    !,
    json_must_be(Type, In, Path, Options),
    maplist(numeric_constraint(In, Path, Options), Constraints).
json_check2(null, In, Path, Options) :-
    !,
    json_must_be(null, In, Path, Options).
json_check2(any, _, _, _) :-
    !.
json_check2(untyped, _, _, _) :-
    !.
json_check2(nothing, In, Path, Options) :-
    !,
    json_error(type_error(nothing, In), Path, Options).
json_check2(string(Format, Constraints), In, Path, Options) :-
    !,
    json_must_be(Format, In, Path, Options),
    maplist(check_string_constraint(In, Path, Options), Constraints).
json_check2(const(Const), In, Path, Options) :-
    !,
    (   json_equal(Const, In, Options)
    ->  true
    ;   json_error(type_error(const(Const), In), Path, Options)
    ).
json_check2(Type, In, Path, Options) :-
    json_must_be(Type, In, Path, Options).

json_check_in_type(In, Path, Options, Type) :-
    json_check(Type, In, Path, Options).

%!  try_check(+In, +Path, +Options, +Type) is semidet.
%
%   Helper   for   anyOf(Types).    It    we     are    running    under
%   unevaluatedProperties testing, we must  evaluate   all  types of the
%   anyOf.

try_check(In, Path, Options, Type) :-
    E = error(_,_),
    catch(json_check(Type, In, Path, Options), E, fail).

%!  json_check_array_items(+Array:list, -Evaluated, +Type, +Path, +Nth,
%!                         +Options)
%
%   Verify that all elements of Array satisfy Type.

json_check_array_items([], [], _Type, _Path, _Nth, _Options).
json_check_array_items([H|T], [EH|Evd], Type, Path, Nth, Options) :-
    json_check(Type, H, [Nth|Path], Options),
    has_evaluated_item(Type, EH),
    Nth1 is Nth+1,
    json_check_array_items(T, Evd, Type, Path, Nth1, Options).

has_evaluated_item(untyped, _Bool) =>
    true.
has_evaluated_item(_Type, Bool) =>
    Bool = true.

%!  json_check_prefix_items(+Types, +ArrayIn, -Evaluated, -ArrayLeft,
%!                          -LeftEvaluated, +Path, +Nth0, -Nth,
%!                          +Options) is det.

json_check_prefix_items([], Items0, Evd0, Items, Evd, _Path, Nth0, Nth, _Options) =>
    Items = Items0,
    Evd = Evd0,
    Nth = Nth0.
json_check_prefix_items([Type|Types], [H|T], Evd0, Rest, Evd, Path,
                        Nth0, Nth, Options) =>
    json_check(Type, H, [Nth0|Path], Options),
    Evd0 = [EH|Evd1],
    has_evaluated_item(Type, EH),
    Nth1 is Nth0+1,
    json_check_prefix_items(Types, T, Evd1, Rest, Evd, Path, Nth1, Nth, Options).
json_check_prefix_items(_Types, [], Evd0, Items, Evd, _Path, Nth0, Nth, _) =>
    Items = [],
    Evd = Evd0,
    Nth = Nth0.

numeric_constraint(In, Path, Options, Constraint) :-
    (   number_in_domain(Constraint, In)
    ->  true
    ;   json_error(domain_error(Constraint, In), Path, Options)
    ).

number_in_domain(>=(Min), Value) :-
    Value >= Min.
number_in_domain(>(Min), Value) :-
    Value > Min.
number_in_domain(=<(Max), Value) :-
    Value =< Max.
number_in_domain(<(Max), Value) :-
    Value < Max.
number_in_domain(multiple_of(N), Value) :-
    (   integer(N),
        integer(Value)
    ->  Value mod N =:= 0
    ;   Times is Value/N,
        Times =:= round(Times)
    ).

check_string_constraint(String, Path, Options, minLength(MinLen)) =>
    string_length(String, Len),
    (   Len >= MinLen
    ->  true
    ;   json_error(domain_error(min_length(MinLen), String), Path, Options)
    ).
check_string_constraint(String, Path, Options, maxLength(MaxLen)) =>
    string_length(String, Len),
    (   Len =< MaxLen
    ->  true
    ;   json_error(domain_error(max_length(MaxLen), String), Path, Options)
    ).
check_string_constraint(String, Path, Options, pattern(Regex)) =>
    (   re_match(Regex, String)
    ->  true
    ;   json_error(domain_error(pattern(Regex), String), Path, Options)
    ).
check_string_constraint(_String, _Path, _Options, contentEncoding(_Enc)) =>
    true.
check_string_constraint(_String, _Path, _Options, contentMediaType(_Type)) =>
    true.
check_string_constraint(_String, _Path, _Options, contentSchema(_Schema)) =>
    true.


%!  check_property(+Obj, +Path, +Options, +PropertyDescr)

check_property(Obj, Path, Options, p(Name,Type,Req)) :-
    (   get_dict(Name, Obj, Value)
    ->  json_check(Type, Value, [Name|Path], Options),
        has_evaluated(Obj, Name)
    ;   Req == false
    ->  true
    ;   json_error(existence_error(json_property, Name), Path, Options)
    ).

%!  json_check_object_constraints(+Constraints, +In, +Path, +Options)
%
%   Verify Constraints on the object In.

json_check_object_constraints(Constraints, In, Path, Options) :-
    maplist(json_check_object_constraint_(In, Path, Options), Constraints).

json_check_object_constraint_(In, Path, Options, Constraint) :-
    json_check_object_constraint(Constraint, In, Path, Options).

json_check_object_constraint(properties(Props, implicit, []),
                             In, Path, Options) =>
    maplist(check_property(In, Path, Options), Props).
json_check_object_constraint(properties(Props, Addn, Patterns),
                             In, Path, Options) =>
    dict_pairs(In, _, Pairs0),
    delete(Pairs0, '$evaluated$'-_, Pairs),
    obj_properties(Pairs, Props, In, Addn, Patterns, Path, Options).
json_check_object_constraint(minProperties(Min), In, Path, Options) =>
    property_count(In, Count),
    (   Count >= Min
    ->  true
    ;   json_error(json_error(minProperties(Min), In), Path, Options)
    ).
json_check_object_constraint(maxProperties(Max), In, Path, Options) =>
    property_count(In, Count),
    (   Count =< Max
    ->  true
    ;   json_error(json_error(maxProperties(Max), In), Path, Options)
    ).
json_check_object_constraint(dependentRequired(Dict), In, Path, Options) =>
    mapdict(check_dependent_required(In, Path, Options), Dict).
json_check_object_constraint(dependentSchemas(Dict), In, Path, Options) =>
    mapdict(check_dependent_schema(In, Path, Options), Dict).
json_check_object_constraint(propertyNames(Type), In, Path, Options) =>
    dict_keys(In, Properties),
    maplist(check_property_name(Type, Path, Options), Properties).

property_count(Data, Count) :-
    dict_keys(Data, Keys),
    delete(Keys, '$evaluated$', Keys1),
    length(Keys1, Count).

%!  check_dependent_required(+In, +Path, +Options, +Property, +Required)
%
%   If In has Property, it also must have all properties in Required.

check_dependent_required(In, Path, Options, Prop, Required) :-
    (   _ = In.get(Prop)
    ->  maplist(check_dependent_prop_exists(In, Path, Options), Required)
    ;   true
    ).

check_dependent_prop_exists(In, Path, Options, Required) :-
    (   _ = In.get(Required)
    ->  true
    ;   json_error(existence_error(json_property, Required), Path, Options)
    ).

%!  check_dependent_schema(+In, +Path, +Options, +Prop, +Type) is det.
%
%   Verify that In satisfies Type if In has Prop.

check_dependent_schema(In, Path, Options, Prop, Type) :-
    (   _ = In.get(Prop)
    ->  json_check(Type, In, Path, Options)
    ;   true
    ).

%!  check_property_name(+Type, +Path, +Options, +PropertyName) is det.
%
%   Verify that PropertyName satisfies Type.

check_property_name(_Type, _Path, _Options, '$evaluated$') :-
    !.
check_property_name(Type, Path, Options, PropertyName) :-
    atom_string(PropertyName, String),            % make sure specials are handled
    (   json_check_silent(Type, String, Options)
    ->  true
    ;   json_error(json_error(propertyNames(Type), String), Path, Options)
    ).

%!  obj_properties(+InPairs, +Spec, +Data:dict, +Additional,
%!                 +Patterns, +Path, +Options)
%
%   Verify that all object properties constraints are respected.
%
%   @arg InPairs is a set of `Prop-Value`   pairs  created from the data
%   object to tbe verified.
%   @arg Spec is a list of p(Name,Type,Required) triples that specify
%   the constraints for the property `Name`.
%   @arg Additional is either a boolean or type(Type), indicating there
%   may be any number of additional properties as long as thety satisfy
%   `Type`.

obj_properties([], Spec, _In, _Addn, _Pats, Path, Options) :-
    !,
    check_missing(Spec, Path, Options).
obj_properties(More, [], In, Addn, Pats, Path, Options) :-
    !,
    maplist(additional_property(Addn, In, Pats, Path, Options), More).
obj_properties([NV|T], PL, In, Addn, Pats, Path, Options) :-
    PL = [p(P,_,_)|_],
    NV = N-_,
    N @< P,
    !,
    additional_property(Addn, In, Pats, Path, Options, NV),
    obj_properties(T, PL, In, Addn, Pats, Path, Options).
obj_properties([N-V|T], [p(N,Type,_Req)|PT], In, Addn, Pats, Path, Options) :-
    !,
    json_check(Type, V, [N|Path], Options),
    has_evaluated(In, N),
    obj_properties(T, PT, In, Addn, Pats, Path, Options).
obj_properties(T, [p(N,_Type,Req)|PT], In, Addn, Pats, Path, Options) :-
    (   Req == false
    ->  true
    ;   json_error(existence_error(json_property, N), Path, Options)
    ),
    obj_properties(T, PT, In, Addn, Pats, Path, Options).

additional_property(Addn, In, Pats, Path, Options, Name-Value) :-
    (   append(_, [Pattern-Type|Rest], Pats),
        re_match(Pattern, Name)
    ->  json_check(Type, Value, [Name|Path], Options),
        forall(( member(Pattern2-Type2, Rest),
                 re_match(Pattern2, Name)
               ),
               json_check(Type2, Value, [Name|Path], Options)),
        has_evaluated(In, Name)
    ;   Addn == false
    ->  json_error(json_error(additional_property, Name), Path, Options)
    ;   Addn = type(Type)
    ->  json_check(Type, Value, [Name|Path], Options),
        has_evaluated(In, Name)
    ;   Addn == true              % specified "additionalProperties": true
    ->  has_evaluated(In, Name)
    ;   Addn == implicit          % Not explicitly specified
    ->  true
    ;   assertion(false)
    ).

check_missing([], _, _).
check_missing([p(N,_Type,Req)|T], Path, Options) :-
    (   Req == false
    ->  check_missing(T, Path, Options)
    ;   json_error(existence_error(json_property, N), Path, Options)
    ).

%!  has_evaluated(!Data:dict, +Prop:atom) is det.
%
%   Add Prop to Data.'$evaluated$' using destructive assignment.

has_evaluated(Data, Prop) :-
    Evaluated = Data.get('$evaluated$'),
    \+ memberchk(Prop, Evaluated),
    !,
    b_set_dict('$evaluated$', Data, [Prop|Evaluated]).
has_evaluated(_, _).

%!  join_has_evaluated(!Outer, +Inner) is det.
%
%   Inner  was  subject  to  unevaluatedProperties   evaluation.  If  it
%   evaluated   properties   and   Outer    is     also    subject    to
%   unevaluatedProperties, we must join the evaluated properties.

join_has_evaluated(Outer, Inner) :-
    OuterEvaluated = Outer.get('$evaluated$'),
    InnerEvaluated = Inner.get('$evaluated$'),
    !,
    append(InnerEvaluated, OuterEvaluated, All),
    sort(All, New),
    b_set_dict('$evaluated$', Outer, New).
join_has_evaluated(_, _).


%!  check_unevaluated(+Type, +Data:dict, +Path, +Options)
%
%   Check unevaluated properties of Data against Type.

check_unevaluated(Type, Data, Path, Options) :-
    Evaluated = Data.'$evaluated$',
    dict_pairs(Data, _, Pairs),
    include(unevaluated(Evaluated), Pairs, UnEvaluated),
    (   UnEvaluated == []
    ->  true
    ;   Type == false
    ->  pairs_keys(UnEvaluated, PropNames),
        json_error(json_error(unevaluatedProperties, PropNames), Path, Options)
    ;   Type = type(T)
    ->  maplist(json_check_pair(T, Path, Options), UnEvaluated)
    ;   Type == true
    ->  pairs_keys(UnEvaluated, PropNames),
        maplist(has_evaluated(Data), PropNames)
    ).

unevaluated(_, '$evaluated$'-_) =>
    fail.
unevaluated(Evaluated, Prop-_) =>
    \+ memberchk(Prop, Evaluated).

json_check_pair(Type, Path, Options, Prop-Value) :-
    json_check(Type, Value, [Prop|Path], Options).


                /*******************************
                *            ARRAYS            *
                *******************************/

array_type(untyped, Type) =>
    Type = array([]).
array_type(Type0, Type) =>
    Type = Type0.

is_array(array_evaluated(_,_)) => true.
is_array(List) => is_list(List).

%!  json_check_array_constraints(+Constraints:list, +In,
%!                               -Evaluated, +Path, +Options)
%
%   Check the constraints on an array as   generated  from the schema by
%   array_property/3.

json_check_array_constraints(Constraints, In, Evd, Path, Options) :-
    maplist(json_check_array_constraint_r(In, Evd, Path, Options), Constraints).

json_check_array_constraint_r(In, Evd, Path, Options, Constraint) :-
    json_check_array_constraint(Constraint, In, Evd, Path, Options).

json_check_array_constraint(minItems(MinLen), Array, _Evd, Path, Options) =>
    length(Array, Len),
    (   Len >= MinLen
    ->  true
    ;   json_error(json_error(minItems(MinLen), Array), Path, Options)
    ).
json_check_array_constraint(maxItems(MaxLen), Array, _Evd, Path, Options) =>
    length(Array, Len),
    (   Len =< MaxLen
    ->  true
    ;   json_error(json_error(maxItems(MaxLen), Array), Path, Options)
    ).
json_check_array_constraint(items(PrefixItems, Type), Array,
                            Evd, Path, Options) =>
    json_check_prefix_items(PrefixItems, Array, Evd, Rest, REvd, Path, 0, I, Options),
    json_check_array_items(Rest, REvd, Type, Path, I, Options).
json_check_array_constraint(uniqueItems(Unique), Array,
                            _Evd, Path, Options) =>
    json_check_array_unique(Unique, Array, Path, Options).
json_check_array_constraint(contains(Type, Min, Max),
                            Array, Evd, Path, Options) =>
    eval_contained(Array, Evd, Matched, Type, Options),
    length(Matched, NMatched),
    (   between(Min, Max, NMatched)
    ->  true
    ;   Min == 1, Max == infinite
    ->  json_error(json_error(contains(Type), Array), Path, Options)
    ;   Max == infinite
    ->  json_error(json_error(minContains(Min,Type), Array), Path, Options)
    ;   NMatched < Min
    ->  json_error(json_error(minContains(Min,Type), Array), Path, Options)
    ;   json_error(json_error(maxContains(Max, Type), Array), Path, Options)
    ).

%!  eval_contained(+Array, -Evaluated, -Matched, +Type, +Options)
%
%   True when Matched contains the items of Array that match Type.

eval_contained([], [], [], _Type, _Options).
eval_contained([H|T], [true|EvdT], [H|MatchT], Type, Options) :-
    json_check_silent(Type, H, Options),
    !,
    eval_contained(T, EvdT, MatchT, Type, Options).
eval_contained([_|T], [_|EvdT], Match, Type, Options) :-
    eval_contained(T, EvdT, Match, Type, Options).

json_check_array_unique(false, _List, _Path, _Option) :-
    !.
json_check_array_unique(true, List, Path, Options) :-
    msort(List, Sorted),
    (   append(_, [X,Y|_], Sorted),
        X =@= Y
    ->  json_error(json_error(unique, List), Path, Options)
    ;   true
    ).

%!  check_unevaluated_items(+Uneval, +In, +Evd, +Path, +Options)
%
%   Check unevaluatedItems.
%
%   @arg In is the array
%   @arg Evd is a, possibly partial, list holding `true` for any
%   element that has been evaluated and a variable for the non-evaluated
%   elements.

check_unevaluated_items(true, In, Evd, _Path, _Options) =>
    maplist(mark_evaluated_item, In, Evd).
check_unevaluated_items(false, In, Evd, Path, Options) =>
    unevaluated_items(In, Evd, Unevaluated),
    (   Unevaluated == []
    ->  true
    ;   json_error(json_error(unevaluatedItems, Unevaluated), Path, Options)
    ).
check_unevaluated_items(type(Type), In, Evd, Path, Options) =>
    eval_unevaluated_items(In, Evd, Type, 0, Path, Options).

mark_evaluated_item(_, true).

unevaluated_items([], _, Unevaluated) =>
    Unevaluated = [].
unevaluated_items([_|T], [true|Evd], Unevaluated) =>
    unevaluated_items(T, Evd, Unevaluated).
unevaluated_items([H|T], [_|Evd], Unevaluated) =>
    Unevaluated = [H|UT],
    unevaluated_items(T, Evd, UT).
unevaluated_items(Items, Evd, Unevaluated), var(Evd) =>
    Unevaluated = Items.

%!  eval_unevaluated_items(+Array, ?Evaluated, +Type, +Nth, +Path,
%!                         +Options)
%
%   Evaluate the non-evaluated elements of Array.

eval_unevaluated_items([], _Evd, _Type, _N, _Path, _Options) =>
    true.
eval_unevaluated_items([_|T], [true|Evd], Type, Nth, Path, Options) =>
    Nth1 is Nth+1,
    eval_unevaluated_items(T, Evd, Type, Nth1, Path, Options).
eval_unevaluated_items([H|T], [U|Evd], Type, Nth, Path, Options) =>
    json_check(Type, H, [Nth|Path], Options),
    U = true,
    Nth1 is Nth+1,
    eval_unevaluated_items(T, Evd, Type, Nth1, Path, Options).
eval_unevaluated_items([H|T], Evd, Type, Nth, Path, Options), var(Evd) =>
    json_check(Type, H, [Nth|Path], Options),
    Evd = [true|ET],
    Nth1 is Nth+1,
    eval_unevaluated_items(T, ET, Type, Nth1, Path, Options).

%!  json_equal(+Const, +In, +Options) is semidet.
%
%   True if Const and In represent the  same JSON data. This is normally
%   Prolog  equivalence,  but  we  take    care  of  possible  different
%   representations. Also, numbers are compared by  value, so 2.0 equals
%   2 in JSON.

json_equal(Const, Const, _) =>
    true.
json_equal(J1, J2, _), number(J1), number(J2) =>
    J1 =:= J2.
json_equal(@(X), X, _) =>
    true.
json_equal(X, @(X), _) =>
    true.
json_equal(S, A, _), string(S), atom(A) =>
    atom_string(S, A).
json_equal(A, S, _), string(S), atom(A) =>
    atom_string(S, A).
json_equal(J1, J2, _), atomic(J1) =>
    J1 == J2.
json_equal(J1, J2, _), atomic(J2) =>
    J1 == J2.
json_equal(J1, J2, Options), is_dict(J1), is_dict(J2) =>
    dict_pairs(J1, _, P10),
    dict_pairs(J2, _, P20),
    delete(P10, '$evaluated$'-_, P1),
    delete(P20, '$evaluated$'-_, P2),
    maplist(equal_kv(Options), P1, P2).
json_equal(J1, J2, Options), is_list(J1), is_list(J2) =>
    maplist(equal_v(Options), J1, J2).
json_equal(_, _, _) =>
    false.

equal_kv(Options, K-V1, K-V2) :-
    json_equal(V1, V2, Options).

equal_v(Options, V1, V2) :-
    json_equal(V1, V2, Options).


%!  json_error(+Error, +Path, +Options)
%
%

json_error(Error, Path, Options) :-
    ErrorTerm = error(Error, json_path(Path)),
    option(on_error(Mode), Options, error),
    (   Mode == error
    ->  throw(ErrorTerm)
    ;   Mode == silent
    ->  fail
    ;   print_message(warning, ErrorTerm)
    ).

%!  json_must_be(+Format, +Value, +Path, +Options)
%

json_must_be(date, Value, Path, Options) :-
    !,
    (   catch(xsd_time_string(_,
                              'http://www.w3.org/2001/XMLSchema#date',
                              Value),
              error(_, _),
              fail)
    ->  true
    ;   json_error(type_error(date, Value), Path, Options)
    ).
json_must_be(date_time, Value, Path, Options) :-
    !,
    (   catch(xsd_time_string(_,
                              'http://www.w3.org/2001/XMLSchema#dateTime',
                              Value),
              error(_, _),
              fail)
    ->  true
    ;   json_error(type_error(date_time, Value), Path, Options)
    ).
json_must_be(base64, Value, Path, Options) :-
    !,
    (   catch(base64(_, Value), error(_,_), fail)
    ->  true
    ;   json_error(type_error(base64, Value), Path, Options)
    ).
json_must_be(email, Value, Path, Options) :-
    !,
    (   is_json_string(Value, Options)
    ->  regex(email, Re),
        re_match(Re, Value)
    ;   json_error(type_error(email, Value), Path, Options)
    ).
json_must_be(uri, Value, Path, Options) :-
    !,
    (   is_json_string(Value, Options),
        is_absolute_url(Value)
    ->  true
    ;   json_error(type_error(uri, Value), Path, Options)
    ).
json_must_be(string, Value, Path, Options) :-
    !,
    (   is_json_string(Value, Options)
    ->  true
    ;   json_error(type_error(string, Value), Path, Options)
    ).
json_must_be(null, Value, Path, Options) :-
    !,
    (   Value == null
    ->  true
    ;   json_error(type_error(null, Value), Path, Options)
    ).
json_must_be(JSONType, Value, Path, Options) :-
    (   (   json_type_alias(JSONType, Type)
        ->  true
        ;   Type = JSONType
        ),
        json_is_of_type(Type, Value)
    ->  true
    ;   json_error(type_error(JSONType, Value), Path, Options)
    ).

%!  regex(+Format, -Re) is det.
%
%   Quick and dirty way to validate some formats. Probably better to
%   write as DCG.

:- det(regex/2).
regex(email, '^[A-Za-z0-9.!#$%&\'*+/=?^_`{|}~-]+\c
              @[A-Za-z0-9](?:[A-Za-z0-9-]{0,61}[A-Za-z0-9])?(?:\\.[A-Za-z0-9](?:[A-Za-z0-9-]{0,61}[A-Za-z0-9])?)*$').

json_is_of_type(between(Low, High), Value), integer(Low), integer(High) =>
    json_is_integer(Value),
    Value >= Low, Value =< High.
json_is_of_type(integer, Value) =>
    json_is_integer(Value).
json_is_of_type(Type, Value) =>
    is_of_type(Type, Value).

json_is_integer(Value) :-
    integer(Value),
    !.
json_is_integer(Value) :-
    float(Value),
    0.0 =:= float_fractional_part(Value).

%!  is_json_string(+Value, +Options) is semidet.
%
%   True if Value is a  valid  Prolog   term  for  a JSON string. Prolog
%   strings are always valid. Atoms if they   are  not `null`, `true` or
%   `false`, depending on Options.

is_json_string(Value, _) :-
    string(Value),
    !.
is_json_string(Value, Options) :-
    atom(Value),
    not_json_special(Value, Options).

json_type_alias(int32,         between(-2147483648, 2147483647)).
json_type_alias(int64,         between(-9223372036854775808, 9223372036854775807)).
json_type_alias(binary,        string).
json_type_alias(password,      string).
json_type_alias(uri,           string).
json_type_alias(uri_reference, string).
json_type_alias(regex,         string).
json_type_alias(object,        dict).
json_type_alias(array,         list).

not_json_special(Atom, _Options) :-
    \+ json_special(Atom),
    !.
not_json_special(_Atom, Options) :-
    option(value_string_as(atom), Options, string).

json_special(true).
json_special(false).
json_special(null).

boolean(true).
boolean(false).

open_list_member(_, Var) :-
    var(Var),
    !,
    fail.
open_list_member(E, [E|_]).
open_list_member(E, [_|T]) :-
    open_list_member(E, T).

open_list_add(E, List) :-
    memberchk(E, List).

%!  select_dict(+Select:dict, +From:dict, -Rest:dict, +Default) is semidet.
%
%   As select_dict/3, providing defaults. Succeeds   if keys from Select
%   appear either (or both) in From and Default.

select_dict(Select, From, Rest, Default) :-
    select_dict(Select, Default.put(From), Rest).

%!  findall_vars(?Skeleton, :Goal, +Preserve, =List) is det.
%
%   Same as findall/3, but  rebinds  variables   from  the  context that
%   appear in Preserve.

findall_vars(Skeleton, Goal, Preserve, List) :-
    term_variables(Preserve, Vars),
    V =.. [v|Vars],
    findall(V-Skeleton, Goal, Pairs),
    maplist(fa_rebind(V), Pairs, List).

fa_rebind(V, V-Sol, Sol).

uri_fragment(URI, Fragment) :-
    uri_components(URI, Components),
    uri_data(fragment, Components, Fragment),
    nonvar(Fragment).

		 /*******************************
		 *           MESSAGES		*
		 *******************************/

:- multifile
    prolog:message_context//1,
    prolog:error_message//1.

prolog:message_context(json_path(Path)) -->
    [ nl, '  in JSON object at '-[] ],
    path(Path).

path([]) -->
    [ '/'-[] ].
path([H|T]) -->
    (   {T==[]}
    ->  []
    ;   path(T)
    ),
    [ '/~w'-[H] ].

prolog:error_message(json_error(Error, Context)) -->
    json_error_message(Error, Context).

json_error_message(unique, _List) -->
    [ 'Array contains duplicate items'-[] ].
json_error_message(length(0,Max), List) -->
    { length(List, Len) },
    [ 'Array contains more than ~D items (found ~D)'-[Max, Len] ].
json_error_message(length(Min,infinite), List) -->
    { length(List, Len) },
    [ 'Array contains less than ~D items (found ~D)'-[Min, Len] ].
json_error_message(length(Min,Max), List) -->
    { length(List, Len) },
    [ 'Array must contain betweem ~D and ~D items (found ~D)'-[Min,Max,Len] ].
json_error_message(additional_property, Name) -->
    [ 'Property ~p is not allowed'-[Name] ].
json_error_message(additional_items, Extra) -->
    [ 'Additional item ~p is not allowed'-[Extra] ].
json_error_message(unevaluatedProperties, PropNames) -->
    [ 'Unevaluated properties: ~p'-[PropNames] ].
json_error_message(unevaluatedItems, Items) -->
    [ 'Unevaluated items: ~p'-[Items] ].
json_error_message(minContains(Min,Type), _Array) -->
    [ 'Array contains less than ~D items of type ~p'-[Min,Type] ].
json_error_message(maxContains(Min,Type), _Array) -->
    [ 'Array contains more than ~D items of type ~p'-[Min,Type] ].
json_error_message(minProperties(Min), _Object) -->
    [ 'Object must have at least ~D properties'-[Min] ].
json_error_message(maxProperties(Max), _Object) -->
    [ 'Object must have at most ~D properties'-[Max] ].
json_error_message(propertyNames(_Type), Key) -->
    [ 'Invalid property name: ~p'-[Key] ].
