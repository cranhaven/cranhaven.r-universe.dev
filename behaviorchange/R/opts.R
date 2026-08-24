#' Options for the behaviorchange package
#'
#' The `behaviorchange::opts` object contains three functions to set, get, and reset
#' options used by the escalc package. Use `behaviorchange::opts$set` to set options,
#' `behaviorchange::opts$get` to get options, or `behaviorchange::opts$reset` to reset specific or
#' all options to their default values.
#'
#' It is normally not necessary to get or set `behaviorchange` options.
#'
#' The following arguments can be passed:
#'
#' \describe{
#'   \item{...}{For `behaviorchange::opts$set`, the dots can be used to specify the options
#'   to set, in the format `option = value`, for example,
#'   `EFFECTSIZE_POINTESTIMATE_NAME_IN_DF = "\n"`. For
#'   `behaviorchange::opts$reset`, a list of options to be reset can be passed.}
#'   \item{option}{For `behaviorchange::opts$set`, the name of the option to set.}
#'   \item{default}{For `behaviorchange::opts$get`, the default value to return if the
#'   option has not been manually specified.}
#' }
#'
#' To see the full list of options and their default values,
#' use `behaviorchange::opts$default()`. Some examples are:
#'
#' \describe{
#'
#'   \item{aabbcc}{A color theme for [behaviorchange::abcd()].}
#'
#'   \item{complecs_*}{The worksheet and columns names for [behaviorchange::complecs()].}
#'
#'   \item{silent}{Whether to be chatty or silent.}
#'
#' }
#'
#' @aliases opts set get reset
#'
#' @usage opts
#'
#' @examples ### Get the default utteranceMarker
#' behaviorchange::opts$get(complecs_entitySheet);
#'
#' ### Set it to a custom version, so that every line starts with a pipe
#' behaviorchange::opts$set(complecs_entitySheet = "sheet_with_entities");
#'
#' ### Check that it worked
#' behaviorchange::opts$get(complecs_entitySheet);
#'
#' ### Reset this option to its default value
#' behaviorchange::opts$reset(complecs_entitySheet);
#'
#' ### Check that the reset worked, too
#' behaviorchange::opts$get(complecs_entitySheet);
#'
#' @export
opts <- list();

opts$set <- function(...) {
  dots <- list(...);
  dotNames <- names(dots);
  names(dots) <-
    paste0("behaviorchange.", dotNames);
  if (all(dotNames %in% names(opts$defaults))) {
    do.call(options,
            dots);
  } else {
    stop("Option '", option, "' is not a valid (i.e. existing) option for behaviorchange!");
  }
}

opts$get <- function(option, default=FALSE) {
  option <- as.character(substitute(option));
  if (!option %in% names(opts$defaults)) {
    stop("Option '", option, "' is not a valid (i.e. existing) option for behaviorchange!");
  } else {
    return(getOption(paste0("behaviorchange.", option),
                     opts$defaults[[option]]));
  }
}

opts$reset <- function(...) {
  optionNames <-
    unlist(lapply(as.list(substitute(...())),
                  as.character));
  if (length(optionNames) == 0) {
    do.call(opts$set,
            opts$defaults);
  } else {
    prefixedOptionNames <-
      paste0("behaviorchange.", optionNames);
    if (all(optionNames %in% names(opts$defaults))) {
      do.call(opts$set,
              opts$defaults[optionNames]);
    } else {
      invalidOptions <-
        !(optionNames %in% names(opts$defaults));
      stop("Option(s) ", vecTxtQ(optionNames[invalidOptions]),
           "' is/are not a valid (i.e. existing) option for behaviorchange!");
    }
  }
}

opts$defaults <-
  list(
       ### An Accessible Book of Behavior Change Color palette (AABBCC palette)
       aabbcc =
         list(
           ultimate_goal = c(fill = "#000000", stroke = "#000000", text = "#FFFFFF"),
           problem = c(fill = "#404040", stroke = "#000000", text = "#FFFFFF"),
           environmental_condition = c(fill = "#443399", stroke = "#000000", text = "#FFFFFF"),
           target_behavior = c(fill = "#84315e", stroke = "#000000", text = "#FFFFFF"),
           sub_behavior = c(fill = "#a4519e", stroke = "#000000", text = "#FFFFFF"),
           determinant = c(fill = "#158415", stroke = "#000000", text = "#FFFFFF"),
           sub_determinant = c(fill = "#1f9e82", stroke = "#1f9e82", text = "#FFFFFF"),
           application = c(fill = "#3f7ab5", stroke = "#000000", text = "#FFFFFF"),
           condition_for_effectiveness = c(fill = "#d0edfe", stroke = "#000000", text = "#000000"),
           bcp = c(fill = "#8ee9ff", stroke = "#000000", text = "#000000"),
           target_individual = c(fill = "#ffeea8", stroke = "#000000", text = "#000000"),
           environmental_agent = c(fill = "#ffeded", stroke = "#000000", text = "#000000"),
           implementer = c(fill = "#edebfc", stroke = "#000000", text = "#000000")
         ),

       complecs_entitySheet = "entities",
       complecs_connectionsSheet = "connections",
       complecs_entityTypesSheet = "entity_types",
       complecs_connectionTypesSheet = "connection_types",

       complecs_entityCols = c(entity_id = "entity_id",
                               entity_label = "entity_label",
                               entity_type_id = "entity_type_id",
                               parent_id = "parent_id"),
       complecs_connectionsCols = c(from_entity_id = "from_entity_id",
                                    to_entity_id = "to_entity_id",
                                    connection_type_id = "connection_type_id"),
       complecs_entityTypesCols = c(entity_type_id = "entity_type_id",
                                    entity_type_label = "entity_type_label"),
                                    # entity_type_stroke = "entity_type_stroke",
                                    # entity_type_shape = "entity_type_shape",
                                    # entity_type_fill = "entity_type_fill",
                                    # entity_type_style = "entity_type_style",
                                    # entity_type_text = "entity_type_text"),
       complecs_connectionTypesCols = c(connection_type_id = "connection_type_id",
                                        connection_type_label = "connection_type_label",
                                        connection_label = "label"),
                                        # connection_type_stroke = "connection_type_stroke",
                                        # connection_type_style = "connection_type_style",
                                        # connection_type_dir = "connection_type_dir")



       defaultGraphTheme = list(
         c("fontname", "Arial", "graph"),
         c("fontname", "Arial", "node"),
         c("fontname", "Arial", "edge"),
         c("layout", "dot", "graph"),
         c("rankdir", "LR", "graph"),
         c("outputorder", "edgesfirst", "graph"),
         c("fixedsize", "false", "node"),
         c("headclip", "true", "edge"),
         c("tailclip", "false", "edge")
       ),

       diagrammerSanitization = list(c("\\\"", "`"),
                                     c("\\'", "`"),
                                     c("\\\\", "/"),
                                     c("[^a-zA-Z0-9;)(,._/`-]", " ")),

       ### From the {see} package:
       ### https://easystats.github.io/see/reference/okabeito_colors.html
       ### Specifically, dput(see::okabeito_colors());

       palette_okabe_ito =
         c(orange = "#E69F00", `light blue` = "#56B4E9", green = "#009E73",
           amber = "#F5C710", blue = "#0072B2", red = "#D55E00", purple = "#CC79A7",
           grey = "#999999", black = "#000000"),

       palette_okabe_ito_reorganized =
         c(blue = "#0072B2", orange = "#E69F00", purple = "#CC79A7",
           green = "#009E73", red = "#D55E00", `light blue` = "#56B4E9",
           amber = "#F5C710", grey = "#999999", black = "#000000"),

       ### Where to print tables; 'console', 'viewer', and/or
       ### one or more filenames in existing directories
       tableOutput = c("console", "viewer"),

       silent = TRUE,

       ### Whether you want extra information, as for debugging
       debugging = FALSE

  )

