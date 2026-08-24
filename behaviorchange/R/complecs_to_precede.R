#' Represent a COMPLECS specification as a PRECEDE model
#'
#' This function reads in a complecs specification and draw a PRECEDE model,
#' with a number of assumptions (see Details section).
#'
#' Only entities with the following entity types are used from the COMPLECS
#' specification:
#'
#' - `person`
#' - `organization`
#' - `environmental_condition`
#' - `behavior`
#' - `determinant`
#' - `outcome`
#'
#' Furthermore, it will be assumed that the only direct connections from
#' `behavior` entities to `outcome` entities belong to the focal population;
#' therefore, if behaviors of environmental actors are important for an
#' outcome, those behaviors' effects must be represented as
#' `environmental_condition` entities - otherwise the relevant `person`s or
#' `organizations`s will be erroneously considered as focal population members.
#'
#' @param input Either a link to a Google Sheet, or a path to an Excel file.
#' @param title The title of the COMPLECS graph.
#' @param layout The layout to use; has to be one of the `DiagrammeR` layout
#' types (`dot`, `neato`, `circo` and `twopi`).
#' @param graph_styling Additional styling to apply; a list with three-element
#' vectors, where the three elements correspond to, respectively, the `attr`,
#' `value`, and `attr_type` arguments for [DiagrammeR::add_global_graph_attrs()].
#' Note that these attributes may override attributes specified in the COMPLECS
#' specification.
#' @param directed Whether to draw directed arrows or not.
#' @param outputFile A character vector where each element is one path (including
#' filename) to write the graph to.
#' @param outputWidth,outputHeight If not `NULL`, a way to override the width and
#' height when calling `complecs` to generate a COMPLECS overview.
#   #' @param width,height If not `NULL`, a way to override the width and
#   #' height when calling `print` to print a COMPLECS overview.
#' @param returnDotOnly Whether to only return the produced DOT code.
#' @param returnSvgOnly Whether to only return the SVG in a character vector.
#' @param returnGraphOnly Whether to only return the produced graph.
#' @param maxLabelLength The number of characters where to wrap the labels.
#' @param regExReplacements A list of pairs of regular
#'   expressions that will be applied to the specifications
#'   before generating the ABCD. This can be used to sanitize
#'   problematic characters (e.g. ', " and \).
#  #' @param x The object to print (i.e. a result of a call to `complecs`).
#' @param silent Whether to be chatty or silent.
#   #' @param ... Any additional arguments for the [print()] method are passed
#   #' to [DiagrammeR::render_graph()].
#'
#' @return A `complecs` object that includes the graph and the graph in SVG in
#' `output$graph` and `output$graphSvg`.
#' @export
#'
#' @examples \dontrun{
#' ### Path in the package with example COMPLECS
#' exampleCOMPLECS <-
#'   system.file(
#'     "extdata",
#'     "COMPLECS-spec-example.xlsx",
#'     package = "behaviorchange"
#'   );
#'
#' behaviorchange::complecs_to_precede(
#'   exampleCOMPLECS
#' );
#'
#' ### Loading that COMPLECS from a google sheet - but note that
#' ### this requires an internet connection!
#' behaviorchange::complecs_to_precede(
#'   paste0(
#'     "https://docs.google.com/spreadsheets/d/",
#'     "1WMO15xroy4a0RfpuZ8GhT-NfDoxwS34w9PrWp8rGjjk"
#'   )
#' );
#' }
complecs_to_precede <- function(input,
                                title = "PRECEDE diagram",
                                layout = "fdp",
                                graph_styling = list(c("outputorder", "edgesfirst", "graph"),
                                                     c("rankdir", "LR", "graph"),
                                                     c("overlap", "false", "graph"),
                                                     c("fixedsize", "false", "node"),
                                                     c("fontname", "Arial", "graph"),
                                                     c("fontname", "Arial", "node"),
                                                     c("fillcolor", "White", "node"),
                                                     c("shape", "box", "node"),
                                                     c("style", "filled", "node"),
                                                     c("fontname", "Arial", "edge"),
                                                     c("headclip", "true", "edge"),
                                                     c("tailclip", "false", "edge")),
                                directed = TRUE,
                                outputFile = NULL,
                                outputWidth=1600,
                                outputHeight=NULL,
                                returnDotOnly = FALSE,
                                returnSvgOnly = FALSE,
                                returnGraphOnly = TRUE,
                                maxLabelLength=60,
                                regExReplacements = opts$get("diagrammerSanitization"),
                                silent = opts$get("silent")) {

  entitySheet <- opts$get("complecs_entitySheet");
  connectionsSheet <- opts$get("complecs_connectionsSheet");
  entityTypesSheet <- opts$get("complecs_entityTypesSheet");
  connectionTypesSheet <- opts$get("complecs_connectionTypesSheet");
  entityCols <- opts$get("complecs_entityCols");
  connectionsCols <- opts$get("complecs_connectionsCols");
  entityTypesCols <- opts$get("complecs_entityTypesCols");
  connectionTypesCols <- opts$get("complecs_connectionTypesCols");

  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());

  if (is.list(input)) {

    if (!all(unlist(lapply(input, is.data.frame)))) {
      stop("As `input`, you must pass the path to an existing spreadsheet, ",
           "a Google Sheets URL, or a list of dataframes, where each data ",
           "frame is the worksheet in a COMPLECS specification. You passed ",
           "an object of class(es) ", vecTxtQ(class(input)), ".");
    }

    worksheetData <- input;
    worksheetNames <- names(worksheetData);

    msg("As `input`, you passed a list of data frames; will assume this is ",
        "an imported spreadsheet.\n", silent=silent);

  } else if (file.exists(input)) {

    msg("`input` specified an existing file, will import it now... ",
        silent=silent);

    ### Read the file
    if (!requireNamespace("openxlsx", quietly=TRUE)) {
      stop("To read Excel files, you need to have {openxlsx} installed!");
    }

    wb <- openxlsx::loadWorkbook(input);

    worksheetNames <- names(wb);

    worksheetData <-
      lapply(worksheetNames,
             function(i) {
               openxlsx::read.xlsx(wb,
                                   sheet=i);
             });
    names(worksheetData) <- trimws(worksheetNames);

    msg("Imported spreadsheet!\n", silent=silent);

  } else if ((length(input) == 1) && (grepl('^http.?://', input))) {

    msg("`input` specified an URL, will try to import it now... ", silent=silent);

    if (!requireNamespace("googlesheets4", quietly=TRUE)) {
      stop("To read Google Sheets, you need to have {googlesheets4} installed!");
    }

    ### Indicate that we want to access a public sheet
    googlesheets4::gs4_deauth();

    ### Read the google sheets workbook
    registeredWS <-
      googlesheets4::gs4_get(input);

    ### Read the list of worksheets
    worksheetNames <-
      trimws(googlesheets4::sheet_names(registeredWS));

    worksheetData <-
      lapply(worksheetNames,
             function(i) {
               googlesheets4::read_sheet(registeredWS, sheet=i);
             });
    names(worksheetData) <- trimws(worksheetNames);

    msg("Imported spreadsheet!\n", silent=silent);

  } else {
    stop("As `input`, provide either the path to a file or the URL of a google spreadsheet.");
  }

  res$intermediate$worksheetData <-
    worksheetData;

  msg("Checking for the presence of all required worksheets... ",
      silent=silent);

  ### Check for presence of required worksheets
  if (!all(c(entitySheet,
             connectionsSheet,
             entityTypesSheet,
             connectionTypesSheet) %in% names(worksheetData))) {
    stop("Not all required worksheets exist in the COMPLECS ",
         "specification! The currently configured worksheet ",
         "names are ", vecTxtQ(c(entitySheet,
                                 connectionsSheet,
                                 entityTypesSheet,
                                 connectionTypesSheet)), ", ",
         "and the worksheets in the spreadsheet you specified ",
         "are ", vecTxtQ(worksheetNames), ".");
  }

  entities <- as.data.frame(worksheetData[[entitySheet]]);
  connections <- as.data.frame(worksheetData[[connectionsSheet]]);
  entityTypes <- as.data.frame(worksheetData[[entityTypesSheet]]);
  connectionTypes <- as.data.frame(worksheetData[[connectionTypesSheet]]);

  msg(
    "Found all required worksheets! These have the following columns:\n\n",
    "- **entities**: ", vecTxtQ(names(entities)), "\n",
    "- **connections**: ", vecTxtQ(names(connections)), "\n",
    "- **entityTypes**: ", vecTxtQ(names(entityTypes)), "\n",
    "- **connectionTypes**: ", vecTxtQ(names(connectionTypes)), "\n",
    silent = silent
  );

  ### Remove empty rows (all rows without identifier)

  msg("Starting to sanitize the input... ", silent=silent);

  entities <- entities[(nchar(entities$entity_id) > 0) &
                         (nchar(entities$entity_type_id) > 0), ];
  connections <- connections[(nchar(connections$from_entity_id) > 0) &
                               (nchar(connections$to_entity_id) > 0), ];
  entityTypes <- entityTypes[(nchar(entityTypes$entity_type_id) > 0), ];
  connectionTypes <- connectionTypes[(nchar(connectionTypes$connection_type_id) > 0), ];

  ### Sanitize attributes and identifiers

  entities <- sanitizeForDiagrammer(
    entities,
    columns = names(entities),
    #columns = entityCols['entity_label'],
    regExReplacements = regExReplacements);

  entityTypes <- sanitizeForDiagrammer(
    entityTypes,
    columns = entityTypesCols['entity_type_label'],
    regExReplacements = regExReplacements);

  connectionTypes <- sanitizeForDiagrammer(
    connectionTypes,
    columns = connectionTypesCols['connection_type_label'],
    regExReplacements = regExReplacements);

  entities <- sanitizeForDiagrammer(
    entities,
    columns = entityCols[c('entity_id', 'parent_id')],
    regExReplacements = list(c("[^a-zA-Z0-9_]", "")));

  connections <- sanitizeForDiagrammer(
    connections,
    columns = connectionsCols[c('from_entity_id', 'to_entity_id')],
    regExReplacements = list(c("[^a-zA-Z0-9_]", "")));

  msg("Sanitized the input!\n", silent=silent);

  ### Checking integrity

  msg("Looking for nonexistent entities...\n", silent=silent);

  fromConnections_nonexistentEntities <-
    connections$from_entity_id[
      !(connections$from_entity_id %in%
        entities$entity_id)];

  toConnections_nonexistentEntities <-
    connections$to_entity_id[
      !(connections$to_entity_id %in%
          entities$entity_id)];

  nonexistentEntityTypes <-
    entities$entity_type_id[
      !(entities$entity_type_id %in%
          entityTypes$entity_type_id)];

  nonexistentConnectionTypes <-
    connections$connection_type_id[
      !(connections$connection_type_id %in%
          connectionTypes$connection_type_id)];

  if (length(fromConnections_nonexistentEntities) > 0) {
    message <-
      paste0("In the connections sheet, connections are specified ",
             "from the following entities that are absent from ",
             "the entities sheet: ",
             vecTxtQ(unique(fromConnections_nonexistentEntities)),
             ".\n");
    if (!silent) {
      msg(message, silent=silent);
    } else {
      warning(message);
    }
  }

  if (length(toConnections_nonexistentEntities) > 0) {
    message <-
      paste0("In the connections sheet, connections are specified ",
             "to the following entities that are absent from ",
             "the entities sheet: ",
             vecTxtQ(unique(toConnections_nonexistentEntities)),
             ".\n");
    if (!silent) {
      msg(message, silent=silent);
    } else {
      warning(message);
    }
  }

  if ((length(fromConnections_nonexistentEntities) == 0) &
      (length(toConnections_nonexistentEntities) == 0)) {
    msg("All entities specified in the connections worksheet exist!\n",
        silent=silent);
  }

  msg("Selecting entity types that can be collapsed, discarding the rest.\n",
      silent=silent);

  selectedEntityIds <-
    entities[
      entities[, entityCols['entity_type_id']] %in%
        c("person",
          "organization",
          "environmental_condition",
          "behavior",
          "determinant",
          "outcome"),
      entityCols['entity_id']
    ];

  selectedEntities <-
    entities[
      entities[, entityCols['entity_id']] %in% selectedEntityIds
      ,
    ]

  selectedConnections <-
    connections[
      (connections[, connectionsCols['from_entity_id']] %in% selectedEntityIds) &
      (connections[, connectionsCols['to_entity_id']] %in% selectedEntityIds)
      ,
    ];

  msg("Collapsing entity specifications in the PRECEDE boxes.\n",
      silent=silent);

  outcomeIds <-
    selectedEntities[
      entities[, entityCols['entity_type_id']] %in%
        c("outcome"),
      entityCols['entity_id']
    ];

  entityIds_linkedToOutcomes <-
    selectedConnections[
      selectedConnections[, connectionsCols['to_entity_id']] %in% outcomeIds,
      connectionsCols['from_entity_id']
    ];

  ### Behaviors directly linked to outcomes (i.e. target behaviors).

  behaviorIds_linkedToOutcomes <-
    selectedEntities[
      (selectedEntities[, entityCols['entity_id']] %in% entityIds_linkedToOutcomes) &
        (selectedEntities[, entityCols['entity_type_id']] == "behavior"),
      entityCols['entity_id']
    ];

  persons_encompassingTargetBehaviors <-
    selectedEntities[
      selectedEntities[, entityCols['entity_id']] %in% behaviorIds_linkedToOutcomes,
      entityCols['parent_id']
    ];

  ### Entities that are linked to outcomes but that aren't behaviors (e.g.
  ### environmental conditions).

  otherEntities_linkedToOutcomes <-
    setdiff(entityIds_linkedToOutcomes,
            behaviorIds_linkedToOutcomes);

  ### Determinants linked to target behaviors

  entityIds_linkedToTargetBehaviors <-
    selectedConnections[
      selectedConnections[, connectionsCols['to_entity_id']] %in% behaviorIds_linkedToOutcomes,
      connectionsCols['from_entity_id']
    ];

  determinants_linkedToTargetBehaviors <-
    selectedEntities[
      (selectedEntities[, entityCols['entity_id']] %in% entityIds_linkedToTargetBehaviors) &
        (selectedEntities[, entityCols['entity_type_id']] == "determinant"),
      entityCols['entity_id']
    ];

  persons_encompassingTargetDeterminants <-
    selectedEntities[
      (selectedEntities[, entityCols['entity_id']] %in% determinants_linkedToTargetBehaviors),
      entityCols['parent_id']
    ];

  ### Determinants linked to environmental conditions

  determinants_linkedToEnvironmentalConditions <-
    setdiff(
      selectedEntities[
        selectedEntities[, entityCols['entity_type_id']] == "determinant",
        entityCols['entity_id']
      ],
      determinants_linkedToTargetBehaviors
    );

  ### Behaviors linked to environmental conditions

  behaviors_linkedToEnvironmentalConditions <-
    setdiff(
      selectedEntities[
        selectedEntities[, entityCols['entity_type_id']] == "behavior",
        entityCols['entity_id']
      ],
      behaviorIds_linkedToOutcomes
    );

  ### Environmental conditions

  environmentalConditions <-
    selectedEntities[
      selectedEntities[, entityCols['entity_type_id']] == "environmental_condition",
      entityCols['entity_id']
    ];

  ### All other entities
  allOtherEntities <-
    setdiff(
      selectedEntities[, entityCols['entity_id']],
      c(
        outcomeIds,
        behaviorIds_linkedToOutcomes,
        determinants_linkedToTargetBehaviors,
        determinants_linkedToEnvironmentalConditions,
        behaviors_linkedToEnvironmentalConditions,
        environmentalConditions,
        unique(c(persons_encompassingTargetBehaviors,
                 persons_encompassingTargetDeterminants))
      )
    );

  outcomeBox_entityIds <-
    outcomeIds;
  targetBehaviorBox_entityIds <-
    behaviorIds_linkedToOutcomes;
  targetDeterminantBox_entityIds <-
    determinants_linkedToTargetBehaviors;
  environmentalFactorBox_entityIds <-
    unique(c(
      behaviors_linkedToEnvironmentalConditions,
      environmentalConditions,
      allOtherEntities));
  environmentalDeterminantBox_entityIds <-
    determinants_linkedToEnvironmentalConditions;

  ### Get human-readable labels
  outcomeBox_Labels <-
    selectedEntities[
      selectedEntities[, entityCols['entity_id']] %in% outcomeBox_entityIds,
      entityCols['entity_label']];
  targetBehaviorBox_Labels <-
    selectedEntities[
      selectedEntities[, entityCols['entity_id']] %in% targetBehaviorBox_entityIds,
      entityCols['entity_label']];
  targetDeterminantBox_Labels <-
    selectedEntities[
      selectedEntities[, entityCols['entity_id']] %in% targetDeterminantBox_entityIds,
      entityCols['entity_label']];
  environmentalFactorBox_Labels <-
    selectedEntities[
      selectedEntities[, entityCols['entity_id']] %in% environmentalFactorBox_entityIds,
      entityCols['entity_label']];
  environmentalDeterminantBox_Labels <-
    selectedEntities[
      selectedEntities[, entityCols['entity_id']] %in% environmentalDeterminantBox_entityIds,
      entityCols['entity_label']];

  ### Wrap labels
  outcomeBox_Labels <- wrapVector(outcomeBox_Labels, maxLabelLength, sep="    \n");
  targetBehaviorBox_Labels <- wrapVector(targetBehaviorBox_Labels, maxLabelLength, sep="    \n");
  targetDeterminantBox_Labels <- wrapVector(targetDeterminantBox_Labels, maxLabelLength, sep="    \n");
  environmentalFactorBox_Labels <- wrapVector(environmentalFactorBox_Labels, maxLabelLength, sep="    \n");
  environmentalDeterminantBox_Labels <- wrapVector(environmentalDeterminantBox_Labels, maxLabelLength, sep="    \n");

  ### Collapse entity labels into one string (i.e. the node labels)
  outcomeBox <- paste0(outcomeBox_Labels, collapse="\n");
  targetBehaviorBox <- paste0(targetBehaviorBox_Labels, collapse="\n");
  targetDeterminantBox <- paste0(targetDeterminantBox_Labels, collapse="\n");
  environmentalFactorBox <- paste0(environmentalFactorBox_Labels, collapse="\n");
  environmentalDeterminantBox <- paste0(environmentalDeterminantBox_Labels, collapse="\n");

  msg("Collapsed entity specifications in the PRECEDE boxes.\n",
      silent=silent);

  ###---------------------------------------------------------------------------

  msg("Creating DOT code.\n", silent=silent);

  dot_code <- ifelse(
    directed,
    "digraph",
    "graph"
  );

  dot_code <- paste0(dot_code, " PRECEDE {\n\n");

  dot_code <- paste0(dot_code, "  layout=dot;\n");

  dot_code <- paste0(dot_code,
                     "  labelloc=\"t\";\n",
                     "  label=\"", title, "\";\n");

  ### General attributes

  for (i in seq_along(graph_styling)) {
    if (graph_styling[[i]][3] == "graph") {
      dot_code <- paste0(dot_code, "  ", graph_styling[[i]][1],
                         " = \"", graph_styling[[i]][2], "\";\n");
    } else {
      dot_code <- paste0(dot_code, "  ", graph_styling[[i]][3],
                         " [", graph_styling[[i]][1],
                         " = \"", graph_styling[[i]][2],
                         "\"];\n");
    }
  }

  dot_code <- paste0(dot_code, "\n");

  ### Add PRECEDE boxes

  dot_code <-
    paste0(
      dot_code,
      "  outcomeBox [label=\"",
      outcomeBox,
      "\"];\n\n"
    );

  dot_code <-
    paste0(
      dot_code,
      "  targetBehaviorBox [label=\"",
      targetBehaviorBox,
      "\"];\n\n"
    );

  dot_code <-
    paste0(
      dot_code,
      "  targetDeterminantBox [label=\"",
      targetDeterminantBox,
      "\"];\n\n"
    );

  dot_code <-
    paste0(
      dot_code,
      "  environmentalFactorBox [label=\"",
      environmentalFactorBox,
      "\"];\n\n"
    );

  dot_code <-
    paste0(
      dot_code,
      "  environmentalDeterminantBox [label=\"",
      environmentalDeterminantBox,
      "\"];\n\n"
    );

  ### Connections

  dot_code <-
    paste0(
      dot_code,
      "  targetDeterminantBox -> targetBehaviorBox;\n",
      "  targetBehaviorBox -> outcomeBox;\n",
      "  environmentalDeterminantBox -> environmentalFactorBox;\n",
      "  environmentalFactorBox -> targetBehaviorBox;\n",
      "  environmentalFactorBox -> outcomeBox;\n"
    );

  ### Set ranks

  dot_code <-
    paste0(
      dot_code,
      "\n  {rank = min; targetDeterminantBox; environmentalDeterminantBox;}",
      "\n  {rank = same; targetBehaviorBox; environmentalFactorBox;}\n",
      "\n  {rank = max; outcomeBox;}\n"
    );

  ### Close graph

  dot_code <- paste0(dot_code, "\n}\n");

  # exampleCOMPLECS <-system.file("extdata","COMPLECS-spec-example.xlsx",package = "behaviorchange");

  # devtools::load_all(); behaviorchange::complecs_to_precede(exampleCOMPLECS);

  ###---------------------------------------------------------------------------

  if (returnDotOnly) {
    return(dot_code);
  }

  graph <- DiagrammeR::grViz(dot_code,
                             engine = layout);
  graphSvg <-
    DiagrammeRsvg::export_svg(graph);
  graphSvg <-
    sub(".*\n<svg ", "<svg ", graphSvg);
  graphSvg <- gsub('<svg width=\"[0-9]+pt\" height=\"[0-9]+pt\"\n viewBox=',
                   '<svg viewBox=',
                   graphSvg);

  if (!is.null(outputFile)) {
    for (currentFile in outputFile) {

      msg("Saving graph to disk.\n", silent=silent);

      if (!dir.exists(dirname(currentFile))) {
        warning("Directory specified to save output file to, '",
                dirname(currentFile), "', does not exist! Have not written output file.");
      } else {

        export_dotCode(dot_code,
                       file_name = currentFile,
                       file_type = tools::file_ext(currentFile),
                       width = outputWidth,
                       height = outputHeight);

        # DiagrammeR::export_graph(graph,
        #                          file_name = currentFile,
        #                          file_type = tools::file_ext(currentFile),
        #                          width = outputWidth,
        #                          height = outputHeight,
        #                          title = DiagrammeR::get_graph_name(graph));
      }
    }
  }

  res$output$dot_code <- dot_code;
  res$output$graph <- graph;
  res$output$graphSvg <- graphSvg;

  class(res) <- "complecs";

  msg("Returning result.\n", silent=silent);

  if (returnSvgOnly) {
    return(graphSvg);
  } else if (returnGraphOnly) {
    return(graph);
  } else {
    return(res);
  }

}

#' @rdname complecs
#' @method print complecs
#' @export
print.complecs <- function(x,
                           width=x$input$width,
                           height=x$input$height,
                           title = DiagrammeR::get_graph_name(x$output$graph),
                           ...) {
  print(DiagrammeR::render_graph(x$output$graph,
                                 title=title,
                                 width=width,
                                 height=height,
                                 ...));
}

# exampleCOMPLECS <-
#   system.file(
#     "extdata",
#     "COMPLECS-spec-example.xlsx",
#     package = "behaviorchange"
#   );
#
# wb <- openxlsx::loadWorkbook(exampleCOMPLECS);
#
# worksheetNames <- names(wb);
#
# exampleCOMPLECS <-
#   lapply(worksheetNames,
#          function(i) {
#            openxlsx::read.xlsx(wb,
#                                sheet=i);
#          });
# names(exampleCOMPLECS) <- trimws(worksheetNames);
#
# saveRDS(exampleCOMPLECS, here::here("inst", "extdata", "COMPLECS-spec-example.Rds"));
