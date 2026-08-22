#' Create a coding scheme
#'
#' This function can be used to specify a coding scheme that can then
#' be used in analysis.
#'
#' A number of coding schemes for cognitive interviews are provided:
#'
#' \describe{
#'   \item{codingScheme_peterson}{Coding scheme from Peterson, Peterson &
#'   Powell, 2017}
#'   \item{codingScheme_levine}{Coding scheme from Levine, Fowler &
#'   Brown, 2005}
#'   \item{codingScheme_willis}{Coding scheme from Willis, 1999}
#' }
#'
#' @param id An identifier for this coding scheme, consisting only of
#' letters, numbers, and underscores (and not starting with a number).
#' @param label A short human-readable label for the coding scheme.
#' @param codes A character vector with the codes in this scheme.
#' @param codingInstructions Coding instructions; a named character vector,
#' where each element is a code's coding instruction, and each element's name
#' is the corresponding code.
#' @param description A description of this coding scheme (i.e. for information
#' that does not fit in the label).
#' @param source Optionally, a description, reference, or URL of a source
#' for this coding scheme.
#'
#' @return The coding scheme object.
#' @export
#' @rdname codingSchemes
create_codingScheme <- function(id,
                                label,
                                codes,
                                codingInstructions = NULL,
                                description = "",
                                source = "") {

  res <-
    list(
      label = label,
      codes = codes,
      codingInstructions = codingInstructions,
      description = description,
      source = source
    );

  class(res) <-
    "rock_codingScheme";

  return(res);

}

#' @export
#' @rdname codingSchemes
codingScheme_peterson <-
  create_codingScheme(
    id = "peterson",
    label = "Peterson, Peterson & Powell",
    source =
      paste0("Christina Hamme Peterson, N. Andrew Peterson & Kristen Gilmore ",
             "Powell (2017) Cognitive Interviewing for Item Development: ",
             "Validity Evidence Based on Content and Response Processes, ",
             "Measurement and Evaluation in Counseling and Development, ",
             "50:4, 217-223, https://10.1080/07481756.2017.1339564"),
    codes =
      c("understanding", "retrieval", "judgment",
        "response", "content_adequacy"),
    codingInstructions =
      c(
        understanding = "Is the item wording, terminology, and structure clear and easy to understand?",
        retrieval = "Has the respondent ever formed an attitude about the topic? Does the respondent have the necessary knowledge to answer the question? Are the mental calculations or long-term memory retrieval requirements too great?",
        judgment = "Is the question too sensitive to yield an honest response? Is the question relevant to the respondent? Is the answer likely to be a constant?",
        response = "Is the desired response available and/or accurately reflected in the response options? Are the response options clear?",
        content_adequacy = "Do all of the items combined adequately represent the construct? Are there items that do not belong?"
      )
  );

#' @export
#' @rdname codingSchemes
codingScheme_levine <-
  create_codingScheme(
    id = "levine",
    label = "Levine, Fowler & Brown",
    source = paste0("Levine, R., Fowler, F., Jr., & Brown, J. (2005).",
                    "Role of cognitive testing in the development of the ",
                    "CAHPS hospital survey. Health Research and Educational ",
                    "Trust, 40, 2037-2056. ",
                    "https://doi.org/10.1111/j.1475-6773.2005.00472.x"),
    codes = c("comprehension", "knowledge", "inapplicable",
              "construct", "subtle", "general"),
    codingInstructions =
      c(
        comprehension = "Items with unclear or ambigous terms, failed to understand the questions consistently.",
        knowledge = "Items for which respondents lacked information to answer a question.",
        inapplicable = "Items measuring construct that are inapplicable for many respondents (e.g. made assumptions).",
        construct = "Items failed to measure the intended construct.",
        subtle = "Items making discriminations that are too subtle for many respondents.",
        general = "Several other general issues associated with the development of a questionnaire."
      )
  );

#' @export
#' @rdname codingSchemes
codingScheme_willis <-
  create_codingScheme(
    id = "willis",
    label = "Willis, 1999",
    codes =   c("clarity",
                "clarity>wording",
                "clarity>technical_term",
                "clarity>vague",
                "clarity>reference_periods",
                "knowledge",
                "knowledge>knowledge",
                "knowlede>recall",
                "knowledge>computation",
                "assumptions",
                "assumptions>inappropriate",
                "assumptions>constant_behavior",
                "assumptions>double_barrelled",
                "response_categories",
                "response_categories>missing",
                "response_categories>mismatch",
                "response_categories>vague",
                "response_categories>open_ended",
                "response_categories>overlapping",
                "response_categories>order",
                "sensitivity",
                "sensitivity>general_content",
                "sensitivity>specific_wording",
                "sensitivity>social_acceptability",
                "instructions",
                "formatting"),
    codingInstructions =
      c(
        clarity = "Problems with intent or meaning of a question.",
        knowledge = "Likely not to know or have trouble remembering information.",
        assumptions = "Problems with assumptions or underlying logic.",
        response_categories = "Problems with the response categories.",
        sensitivity = "Sensitive nature or wording/bias.",
        instructions = "Problems with introductions, instructions, or explanations.",
        formatting = "Problems with lay-out or formatting."
      )
  );
