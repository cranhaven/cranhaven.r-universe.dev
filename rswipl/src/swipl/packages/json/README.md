# SWI-Prolog JSON package

This package provides `library(json)` and related libraries for managing
JSON documents in SWI-Prolog.  The packages contains:

  - library(json) for reading and writing JSON
  - library(json_schema) for JSON Schema validation
  - library(http_json) as plugin for the HTTP package.
  - library(json_convert) to convert JSON documents to native Prolog
    terms and back.
  - library(json_grammar) is used for Quasi Quotation support.

These files used to be part of the  `http` package. They have been moved
to a new package as minimal installations  may want to have JSON support
without  needing  HTTP  support.   This    package   installs   backward
compatibility libraries in the  old   locations.  These libraries merely
load the library  from  the  new   location  and  prints  a _deprecated_
message.
