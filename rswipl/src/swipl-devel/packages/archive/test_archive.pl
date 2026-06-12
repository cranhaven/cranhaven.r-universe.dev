/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Peter Ludemann
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017-2022, VU University Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(test_archive,
	  [ test_archive/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(archive)).
:- use_module(library(apply), [maplist/3, maplist/2]).
:- use_module(library(filesex), [directory_file_path/3, relative_file_name/3]).
:- use_module(library(lists), [nth1/3]).

/* This is a very minimal test suite, which was written when fixing
   some memory leak issues. */

test_archive :-
    run_tests([ archive
              ]).

can_test :-
    archive_has_format(zip),
    \+ current_prolog_flag(wine_version, _).

%!  no_close_tests
%
%   Do not run tests where we do not   close  all streams. In that cases
%   the closing is left to PL_cleanup(), but   this  is unsafe and makes
%   address sanitizer complain. Note that normal   release builds do not
%   cleanup anyway.
%
%   @tbd Fix affected tests. This is  not   easy  as  the order in which
%   objects are deleted during PL_cleanup() is not defined.

no_close_tests :-
    \+ current_prolog_flag(asan, true).

:- begin_tests(archive,
               [ condition(can_test)
               ]).

% The following is derived from check_installation/0 for archive:

test(smoke_test_open) :-
    create_tmp_file(ArchivePath),
    % archive_open should error because the file is empty.
    catch(archive_open(ArchivePath, A, []), E, true),
    (   var(E)
    ->  archive_close(A)
    ;   true
    ),
    delete_file(ArchivePath).

test(create_and_entries,
     [FilesOut == Entries,
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, _, FilesOut, _),
    archive_entries(ArchivePath, Entries).

test(create_and_open_named,
     [setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, SrcDir, _, ExampleSourceFile),
    file_contents(SrcDir, ExampleSourceFile, Contents1),
    archive_open_named(ArchivePath, ExampleSourceFile, TestArchiveStream),
    read_string(TestArchiveStream, _Len, Contents1),
    close(TestArchiveStream).

test(create_and_open_named_no_close, % same as above but without close/1
     [ setup(create_tmp_file(ArchivePath)),
       cleanup(delete_file(ArchivePath)),
       condition(no_close_tests)
     ]) :-
    create_archive_file(ArchivePath, SrcDir, _, ExampleSourceFile),
    file_contents(SrcDir, ExampleSourceFile, Contents1),
    archive_open_named(ArchivePath, ExampleSourceFile, TestArchiveStream),
    read_string(TestArchiveStream, _Len, Contents1).

test(create_and_open_named_twice_no_close,
     [ setup(create_tmp_file(ArchivePath)),
       cleanup(delete_file(ArchivePath)),
       condition(no_close_tests)
     ]) :-
    create_archive_file(ArchivePath, SrcDir, _, ExampleSourceFile),
    file_contents(SrcDir, ExampleSourceFile, Contents1),
    archive_open_named(ArchivePath, 'boot.prc', _Stream0),
    archive_open_named(ArchivePath, ExampleSourceFile, TestArchiveStream),
    read_string(TestArchiveStream, _Len, Contents1).

% TODO: following test causes memory leak:
test(create_and_open_named_fail, % Same as above but with bad EntryName
     [ fail,
       setup(create_tmp_file(ArchivePath)),
       cleanup(delete_file(ArchivePath)),
       condition(no_close_tests)
     ]) :-
    create_archive_file(ArchivePath, _, _, _),
    archive_open_named(ArchivePath, 'XXX', _TestArchiveStream).

% TODO: following test causes memory leak:
test(create_and_open_archive_entry,
     [ setup(create_tmp_file(ArchivePath)),
       cleanup(delete_file(ArchivePath)),
       condition(no_close_tests)
     ]) :-
    create_archive_file(ArchivePath, SrcDir, _, ExampleSourceFile),
    file_contents(SrcDir, ExampleSourceFile, Contents1),
    open_archive_entry(ArchivePath, ExampleSourceFile, TestArchiveStream),
    read_string(TestArchiveStream, _Len, Contents1),
    close(TestArchiveStream).

% TODO: following test causes memory leak:
test(create_and_open_archive_entry_no_close, % same as above but without close/1
     [ setup(create_tmp_file(ArchivePath)),
       cleanup(delete_file(ArchivePath)),
       condition(no_close_tests)
     ]) :-
    create_archive_file(ArchivePath, SrcDir, _, ExampleSourceFile),
    file_contents(SrcDir, ExampleSourceFile, Contents1),
    open_archive_entry(ArchivePath, ExampleSourceFile, TestArchiveStream),
    read_string(TestArchiveStream, _Len, Contents1).

% TODO: following test causes memory leak:
test(create_and_open_archive_entry_no_close, % same as above but bad EntryName
     [ fail,
       setup(create_tmp_file(ArchivePath)),
       cleanup(delete_file(ArchivePath)),
       condition(no_close_tests)
     ]) :-
    create_archive_file(ArchivePath, _, _, _),
    open_archive_entry(ArchivePath, 'XXXl', _TestArchiveStream).

% TODO: following test causes memory leak:
test(create_and_entries_error,
     [error(existence_error(file, 'foobar-qqsv'), _),
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    FilesOut = ['foobar-qqsv'], % doesn't exist
    archive_create(ArchivePath, FilesOut, [format(zip)]).

test(bad_unify_blob,
     [fail,
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    archive_open(ArchivePath, read, not_an_archive_blob, []).

test(bad_mode,
     [error(domain_error(io_mode, neither_read_nor_write), _),
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    archive_open(ArchivePath, neither_read_nor_write, _Archive, []).

:- end_tests(archive).

create_tmp_file(Path) :-
    tmp_file_stream(utf8, Path, Out),
    close(Out).

%!  create_archive_file(+ArchiveFile, -RootDir, -Files, -Example) is det.
%
%   Create  a  `zip`  archive  using  three  files  from  the  installed
%   SWI-Prolog tree.

create_archive_file(ArchivePath, ArchiveSourceDir, FilesOut, ExampleSourceFile) :-
    Files = [swi('include/SWI-Prolog.h'), library('archive.pl'), swi('boot.prc')],
    absolute_file_name(swi(.), ArchiveSourceDir, [file_type(directory), access(read)]),
    maplist(ar_input(ArchiveSourceDir), Files, FilesOut),
    nth1(2, FilesOut, ExampleSourceFile),
    archive_create(ArchivePath, FilesOut,
                   [ format(zip),
                     directory(ArchiveSourceDir)
                   ]).

ar_input(Dir, Spec, File) :-
    directory_file_path(Dir, dummy, RelTo),
    absolute_file_name(Spec, AbsFile, [access(read)]),
    relative_file_name(AbsFile, RelTo, File).

archive_has_format(Format) :-
    create_tmp_file(Path),
    catch(archive_open(Path, A, [format(Format)]), E, true),
    (   var(E)
    ->  archive_close(A),
        delete_file(Path)
    ;   true
    ),
    \+ subsumes_term(error(domain_error(format, _),_), E).

file_contents(SrcDir, File, Contents) :-
    directory_file_path(SrcDir, File, Path),
    setup_call_cleanup(
        open(Path, read, In, [type(binary)]),
        read_string(In, _Len, Contents),
        close(In)).

% Code from documentation of archive_close/1.
archive_open_named(ArchiveFile, EntryName, Stream) :-
    archive_open(ArchiveFile, Archive, []),
    archive_next_header(Archive, EntryName),
    archive_open_entry(Archive, Stream),
    archive_close(Archive).

% Code from documentation of archive_close/1.
open_archive_entry(ArchiveFile, EntryName, Stream) :-
    open(ArchiveFile, read, In, [type(binary)]),
    archive_open(In, Archive, [close_parent(true)]),
    archive_next_header(Archive, EntryName),
    archive_open_entry(Archive, Stream).

% Code from documentation of module (1)

list_archive(File) :-
    setup_call_cleanup(
        archive_open(File, Archive, []),
        (   repeat,
            (   archive_next_header(Archive, Path)
            ->  format('~w~n', [Path]),
                fail
            ;   !
            )
        ),
        archive_close(Archive)).

% Code from documentation of module (2)

list_archive2(File) :-
    list_archive2(File, Headers),
    maplist(writeln, Headers).

list_archive2(File, Headers) :-
    archive_foldl(add_header, File, Headers, []).

add_header(Path, _, [Path|Paths], Paths).

% Code from documentation of module (3)

print_entry(Path, Handle, Cnt0, Cnt1) :-
    archive_header_property(Handle, filetype(Type)),
    format('File ~w is of type ~w~n', [Path, Type]),
    Cnt1 is Cnt0 + 1.

list_archive_headers(File) :-
    archive_foldl(print_entry, File, 0, FileCount),
    format('We have ~w files', [FileCount]).
