/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.

    This file is **deliberately** confusing.  It is a fixture for
    library(unicode_security)'s confusable-identifier linter
    (list_confusable_identifiers/0); the code is valid Prolog but is
    semantically nonsense.  Loading it from a normal program is not
    useful — see test_uts39.pl, suite uts39_check_integration.
*/

:- module(confusable_demo,
          [ paypal/1,
            pаypal/1,      %#  Cyrillic а (U+0430) — look-alike of Latin a
            helloΩ/1,      %#  Latin + Greek Omega — moderately restrictive
            中文/1,         %#  Pure Han — single-script, should NOT warn
            hello/1,       %#  Pure ASCII — should NOT warn

            account_status/2,
            preferred_payment/1,
            user_tags/1,
            log_anomaly/0,
            record_Ω/1     %#  Mixed-script predicate name; called below.
          ]).
:- encoding(utf8).

%   --- Predicate names (functor of clause head).  Original cases.

paypal(_).
pаypal(_).
helloΩ(_).
中文(_).
hello(_).

%   --- Atom literals as arguments (sub_term path; not a predicate name).
%   `visa` and `vіsa` (with Cyrillic і, U+0456) form a skeleton-collision
%   pair visible only via the arguments.

account_status(visa,   active).
account_status(vіsa,   suspended).

%   --- Functor name inside a nested compound (functor walk path; the
%   confusable functor does not appear as any clause head).
%   `payment_paypal` vs `payment_pаypal` (Cyrillic а).

preferred_payment(payment_paypal(default)).
preferred_payment(payment_pаypal(legacy)).

%   --- Atom literal inside a list, again exercising sub_term but
%   through a list spine.  `secureΡay` mixes Latin with Greek capital
%   Rho (U+03A1) — looks like a Latin P.

user_tags([trusted, secureΡay, vip]).

%   --- Mixed-script atom literal AND mixed-script functor in a clause
%   body.

log_anomaly :-
    Tag = signalΩ,            % mixed-script atom literal (Greek capital Omega)
    record_Ω(Tag).            % functor name also mixed-script

record_Ω(_).
