#' rock: A Reproducible Open Coding Kit
#'
#' This package implements an open standard for working with
#' qualitative data, as such, it has two parts: a file format/convention
#' and this R package that facilitates working with .rock files.
#'
#' @section The ROCK File Format:
#'
#' The .rock files are plain text files where a number of conventions are used
#' to add metadata. Normally these are the following conventions:
#'
#' - The smallest 'codeable unit' is called an utterance, and utterances are separated by newline characters (i.e. every line of the file is an utterance);
#' - Codes are in between double square brackets: `[[code1]]` and `[[code2]]`;
#' - Hierarchy in inductive code trees can be indicated using the greater than sign (`>`): `[[parent1>child1]]`;
#' - Utterances can have unique identifiers called 'utterance identifiers' or 'UIDs', which are unique short alphanumeric strings placed in between double square brackets after 'uid:', e.g. `[[uid:73xk2q07]]`;
#' - Deductive code trees can be specified using YAML
#'
#' @section The `rock` R Package Functions:
#'
#' The most important functions are [parse_source()] to parse one source and [parse_sources()]
#' to parse multiple sources simultaneously. [clean_source()] and [clean_sources()] can be used
#' to clean sources, and [prepend_ids_to_source()] and [prepend_ids_to_sources()] can be
#' used to quickly generate UIDs and prepend them to each utterance in a source.
#'
#' For analysis, [create_cooccurrence_matrix()], [collapse_occurrences()], and
#'  [collect_coded_fragments()] can be used.
#'
#' @docType package
#' @name rock
NULL
