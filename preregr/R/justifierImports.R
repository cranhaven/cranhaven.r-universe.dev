#' Programmatically constructing justifier elements
#'
#' These functions can be used to programmatically construct decision,
#' justifications, assertions, and sources using the `justifier` package.
#'
#' @param label A human-readable label for the `decision`, `justification`,
#' `assertion`, or `source`. Labels are brief summaries of the core of the
#' decision, justification, assertion, or source. More details, background
#' information, context, and other comments can be placed in the description.
#' @param description A human-readable description. This can be used to
#' elaborate on the label. Note that the label should be reader-friendly and
#' self-contained; but because they also have to be as short as possible,
#' descriptions can be used to provide definitions, context, background
#' information, or add any other metadata or comments.
#' @param type Types are used when working with a framework. Frameworks define
#' type identifiers, consisting of letters, digits, and underscores. By
#' specifying these identifiers the type of a decision, justification,
#' assertion, or source. Source types can be, for example, types of documents
#' or other data providers, such as "empirical evidence', 'expert consensus',
#' 'personal opinion', or 'that one meeting that we had in May'. Assertion
#' types can be, for example, data types or types of facts, such as 'number',
#' 'prevalence', 'causal relationship', or 'contact information'.
#' Justification types can be, for example, types of reasoning or logical
#' expressions, such as 'deduction', 'induction', or 'intersection'. Decision
#' types are the most framework-specific, heavily depend on the specific
#' context of the decision, and are used by frameworks to organise the
#' decisions in a project. Examples of decision types are the decision to
#' recruit a certain number of participants in a scientific study; the decision
#' to target a certain belief in a behavior change intervention; the decision
#' to merge two codes in a qualitative study; the decision to hire a staff
#' member; or the decision to make a certain purchase.
#' @param xdoi For `source`s, XDOI identifier (a DOI, or, if that does not
#' exist, ISBN or other unique identifier of the source).
#' @param source In assertions, the source (or sources) that the assertion
#' is based on can be specified using `srce()`.
#' @param alternatives The alternatives that were considered in a decision.
#' @param assertion In justifications, the assertion (or assertions) that
#' the justification is based on can be specified using `asrt()`.
#' @param justification In decisions, the justification (or justifications)
#' that the decision is based on can be specified using `jstf()`.
#' @param id The identifier (randomly generated if omitted).
#' @param ... Additional fields and values to store in the element.
#'
#' @rdname constructingJustifications
#' @aliases dcsn jstf asrt srce decide decision assert justify source
#' @return The generated object.
#'
#' @examples ### Programmatically create a partial justification object
#' exampleAssertion <-
#'   preregr::assert(
#'     "This is an assertion",
#'     source = c(
#'       preregr::source('This is a first source'),
#'       preregr::source('This is a second source')));
#'
#' ### Programmatically create a justification with two assertions
#' ### but without sources
#' exampleJustification <-
#'   preregr::justify(
#'     "Icecream will make me feel less fit",
#'     assertion = c(
#'       preregr::assert('Icecream is rich in energy'),
#'       preregr::assert('Consuming high-energy foods makes me feel less fit')
#'     ),
#'     weight = -.5
#'   );
#'
#' ### Show it
#' exampleJustification;
#'
#' ### Programmatically create a simple decision
#' simpleDecision <-
#'   preregr::decide(
#'     "decision",
#'     justification = preregr::jstf(
#'       "justification",
#'       assertion = exampleAssertion
#'     )
#'   );
#'
#' ### Programmatically create a justification object for a full decision
#' fullJustifierObject <-
#'   preregr::decide(
#'     "I decide to go get an icecream",
#'     justification = c(
#'       preregr::justify(
#'         "Having an icecream now would make me happy",
#'         assertion = c(
#'           preregr::assert(
#'             "Decreasing hunger increases happiness",
#'             source = preregr::source(
#'               "My past experiences"
#'             )
#'           ),
#'           preregr::assert(
#'             "I feel hungry",
#'             source = preregr::source(
#'               "Bodily sensations"
#'             )
#'           )
#'         ),
#'         weight = 1
#'       ),
#'       exampleJustification,
#'       preregr::justify(
#'         "I can afford to buy an icecream.",
#'         assertion = c(
#'           preregr::assert(
#'             "My bank account balance is over 300 euro.",
#'             source = preregr::source(
#'               "My bank app"
#'             )
#'           ),
#'           preregr::assert(
#'             "I need to keep at least 100 euro in my bank account.",
#'             source = preregr::source(
#'               "Parental advice"
#'             )
#'           )
#'         ),
#'         weight = .3
#'       )
#'     )
#'   );
#'
#' ### Show the full object
#' fullJustifierObject;
#'
#' ### Combine both into a list of decisions
#' twoDecisions <-
#'   c(simpleDecision,
#'     fullJustifierObject);
#'
#' ### Show the combination
#' twoDecisions;
#'
#' @export source
#' @export srce
source <-
  srce <- function(label,
                   description = NULL,
                   type = NULL,
                   id = NULL,
                   xdoi = NULL,
                   ...) {

    if (!requireNamespace('justifier', quietly = TRUE)) {
      stop(
        cli::format_error(
          c(
            "You can't justify without the `justifier` package.",
            "i" = "You can install it with:\n\ninstall.packages('justifier');\n"
          )
        )
      );
    }

    return(
      justifier::srce(
        id=id,
        label = label,
        description = description %||% "",
        xdoi = xdoi %||% "",
        type = type %||% "",
        ...
      )
    );

  }

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

#' @export assert
#' @export asrt
#' @rdname constructingJustifications
assert <-
  asrt <-
  function(label,
           description = "",
           type = NULL,
           id = NULL,
           source = NULL,
           ...) {

    if (!requireNamespace('justifier', quietly = TRUE)) {
      stop(
        cli::format_error(
          c(
            "You can't justify without the `justifier` package.",
            "i" = "You can install it with:\n\ninstall.packages('justifier');\n"
          )
        )
      );
    }

    return(
      justifier::asrt(
        id=id,
        label = label,
        description = description %||% "",
        type = type %||% "",
        source = source %||% NULL,
        ...
      )
    );

  }

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

#' @export justify
#' @export jstf
#' @rdname constructingJustifications
justify <-
  jstf <- function(label,
                   description = "",
                   type = NULL,
                   id = NULL,
                   assertion = NULL,
                   ...) {

    if (!requireNamespace('justifier', quietly = TRUE)) {
      stop(
        cli::format_error(
          c(
            "You can't justify without the `justifier` package.",
            "i" = "You can install it with:\n\ninstall.packages('justifier');\n"
          )
        )
      );
    }

    return(
      justifier::jstf(
        id=id,
        label = label,
        description = description %||% "",
        type = type %||% "",
        assertion = assertion %||% NULL,
        ...
      )
    );

  }

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

#' @export dcsn
#' @export decision
#' @export decide
#' @rdname constructingJustifications
decide <-
  decision <-
  dcsn <- function(label,
                   description = NULL,
                   type = NULL,
                   id = NULL,
                   alternatives = NULL,
                   justification = NULL,
                   ...) {

    if (!requireNamespace('justifier', quietly = TRUE)) {
      stop(
        cli::format_error(
          c(
            "You can't justify without the `justifier` package.",
            "i" = "You can install it with:\n\ninstall.packages('justifier');\n"
          )
        )
      );
    }

    return(
      justifier::dcsn(
        id=id,
        label = label,
        description = description %||% "",
        type = type %||% "",
        justification = justification %||% "",
        ...
      )
    );

  }

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
