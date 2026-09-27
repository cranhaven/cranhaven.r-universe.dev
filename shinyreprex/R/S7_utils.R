#' Custom S7 Classes
#'
#' @description
#' Additional classes to include in S7 to use in `repro_code` and
#' `repro_code_chunk` methods:
#'
#' ## Reactives
#'
#' These variables need to be handled in a specific way to extract non-static values
#' stored in reactive calls.
#'
#' \describe{
#' \item{class_reactive}{The class capturing [shiny::reactive()] calls}
#' \item{class_event_cache}{The class capturing [shiny::bindCache()] calls}
#' \item{class_event_reactive}{The class capturing [shiny::bindEvent()] calls}
#' \item{class_bind_reactive}{
#' The union of `class_event_cache` and `class_event_reactive`
#' }
#' }
#'
#' ## Special Functions
#'
#' When determining evaluating a chunk, the function name gets attached to the class of the
#' chunk, these are special cases that need to be handled in a non-standard way.
#'
#' \describe{
#' \item{class_call_function}{The class capturing anonymous function definitions}
#' \item{class_call_reactive}{The class capturing evaluated [shiny::reactive()] objects}
#' \item{class_call_reactval}{
#' The class capturing evaluated [shiny::reactiveValues()] objects
#' }
#' \item{class_call_reactval_setter}{
#' The class capturing [shiny::reactiveVal()] setter calls, which are skipped
#' with a warning as they cannot be reproduced outside of Shiny
#' }
#' \item{class_call_if}{The class capturing `if` calls}
#' \item{class_call_switch}{The class capturing `switch` calls}
#' \item{class_call_null}{The class capturing undefined calls, such as `pkg::fn`}
#' \item{class_call_shiny}{
#' The class capturing ignorable shiny function calls such as
#' [shiny::req()] and [shiny::validate()]
#' }
#' \item{class_call_subset}{The class capturing a subset (`$`) or a double-bracket subset (`[[`) call}
#' }
#'
#' @usage NULL
#' @format NULL
#'
#' @keywords internal
#' @rdname s7_classes
class_reactive <- S7::new_S3_class("reactiveExpr")

class_event_cache <- S7::new_S3_class("reactive.cache")
class_event_reactive <- S7::new_S3_class("reactive.event")
class_bind_reactive <- S7::new_union(class_event_reactive, class_event_cache)

class_call_function <- S7::new_S3_class("function")
class_call_reactive <- S7::new_S3_class(".__reactive")
class_call_reactval <- S7::new_S3_class(".__reactval")
class_call_reactval_setter <- S7::new_S3_class(".__reactval_setter")
class_call_if <- S7::new_S3_class("if")
class_call_switch <- S7::new_S3_class("switch")
class_call_null <- S7::new_S3_class("NULL")
class_call_subset <- S7::new_union(S7::new_S3_class("$"), S7::new_S3_class("[["))

IGNORED_SHINY_CALLS <- c("req", "validate")
class_call_shiny <- do.call(S7::new_union, purrr::map(IGNORED_SHINY_CALLS, S7::new_S3_class))
