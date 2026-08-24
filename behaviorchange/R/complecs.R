#' Create a COMPLECS graph
#'
#' COMPLECS was developed to help make sense of complex systems. It reads data from a
#' number of worksheets in a spreadsheet and generates a diagram according to those
#' specifications. Originally, COMPLECS was developed to visualise a problem during
#' the needs assessment phase of intervention development.
#'
#' COMPLECS is a recursive acronym for COMPLECS Organises Multiple Players & Linked
#' Environments using Connected Specifications.
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
#' @param width,height If not `NULL`, a way to override the width and
#' height when calling `print` to print a COMPLECS overview.
#' @param returnDotOnly Whether to only return the produced DOT code.
#' @param returnSvgOnly Whether to only return the SVG in a character vector.
#' @param returnGraphOnly Whether to only return the produced graph.
#' @param maxLabelLength The number of characters where to wrap the labels.
#' @param regExReplacements A list of pairs of regular
#'   expressions that will be applied to the specifications
#'   before generating the ABCD. This can be used to sanitize
#'   problematic characters (e.g. ', " and \).
#' @param x The object to print (i.e. a result of a call to `complecs`).
#' @param silent Whether to be chatty or silent.
#' @param ... Any additional arguments for the [print()] method are passed
#' to [DiagrammeR::render_graph()].
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
#' behaviorchange::complecs(
#'   exampleCOMPLECS
#' );
#'
#' ### Loading that COMPLECS from a google sheet - but note that
#' ### this requires an internet connection!
#' behaviorchange::complecs(
#'   paste0(
#'     "https://docs.google.com/spreadsheets/d/",
#'     "1WMO15xroy4a0RfpuZ8GhT-NfDoxwS34w9PrWp8rGjjk"
#'   )
#' );
#' }
complecs <- function(input,
                     title = "COMPLECS overview",
                     layout = "fdp",
                     graph_styling = list(c("outputorder", "edgesfirst", "graph"),
                                          c("overlap", "false", "graph"),
                                          c("fixedsize", "false", "node"),
                                          c("fontname", "Arial", "graph"),
                                          c("fontname", "Arial", "node"),
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
                     maxLabelLength=20,
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

  msg("Merging entity and type information with ",
      "entity and connection specifications.\n", silent=silent);

  ### Merge columns from type dataframes into regular dataframes

  mergedEntities <-
    merge(x = entities,
          y = entityTypes,
          by.x = entityCols['entity_type_id'],
          by.y = entityTypesCols['entity_type_id']);
  mergedConnections <-
    merge(x = connections,
          y = connectionTypes,
          by.x = connectionsCols['connection_type_id'],
          by.y = connectionTypesCols['connection_type_id']);

  ### Wrap labels in node_df

  mergedEntities[[entityCols['entity_label']]] <-
    wrapVector(
      mergedEntities[[entityCols['entity_label']]],
      maxLabelLength
    );

  ### Get names of specified attributes

  entityAttributes <-
    setdiff(names(mergedEntities),
            c(entityCols['entity_id'],
              entityCols['entity_type_id'],
              entityCols['entity_label'],
              entityTypesCols['entity_type_id'],
              entityTypesCols['entity_type_label']));

  connectionAttributes <-
    setdiff(names(mergedConnections),
            c(connectionsCols["from_entity_id"],
              connectionsCols["to_entity_id"],
              connectionsCols["connection_type_id"]));

  ### Check for parent entities

  if (!(entityCols['parent_id'] %in% names(mergedEntities))) {
    msg("No parent entities specified.\n", silent=silent);
    parentIds <- NULL;
    noParentId <- rep(TRUE, nrow(mergedEntities));
  } else {

    entityAttributes <-
      setdiff(entityAttributes,
              entityCols['parent_id']);

    if (all(is.na(mergedEntities[, entityCols['parent_id']])) |
      all(nchar(trimws(mergedEntities[, entityCols['parent_id']])) == 0)) {

      msg("No parent entities specified.\n", silent=silent);
      parentIds <- NULL;
      noParentId <- rep(TRUE, nrow(mergedEntities));

    } else {

      parentIds <- unique(mergedEntities[, entityCols['parent_id']]);

      noParentId <-
        !((!is.na(mergedEntities[, entityCols['parent_id']])) &
         (nchar(trimws(mergedEntities[, entityCols['parent_id']])) > 0)) &
        !(mergedEntities[, entityCols['entity_id']] %in%
            mergedEntities[, entityCols['parent_id']]);

      parentIds <-
        parentIds[!is.na(parentIds)];

      msg("Identified ", length(parentIds),
          " parent entities.\n", silent=silent);
    }

  }

  ###---------------------------------------------------------------------------

  msg("Creating DOT code.\n", silent=silent);

  dot_code <- ifelse(
    directed,
    "digraph",
    "graph"
  );

  dot_code <- paste0(dot_code, " COMPLECS {\n\n  compound = true;\n");

  dot_code <- paste0(dot_code, "  layout=", layout, ";\n");

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

  ### Nodes in subgraphs

  if (!is.null(parentIds)) {
    for (currentParent in parentIds) {

      currentParentRow <-
        which(mergedEntities[, entityCols['entity_id']] == currentParent);

      dot_code <-
        paste0(
          dot_code,
          "\n",
          "  subgraph cluster_", currentParent, " {\n\n"
        );

      dot_code <-
        paste0(
          dot_code,
          "    label = \"",
          mergedEntities[currentParentRow, entityCols['entity_label']],
          "\";\n",
          paste0("    ", entityAttributes, " = \"",
                 mergedEntities[currentParentRow, entityAttributes],
                 "\"",
                 collapse=";\n"),
          ";\n\n"
        );

      for (currentRow in which(mergedEntities[, entityCols['parent_id']] == currentParent)) {

        dot_code <-
          paste0(
            dot_code,
            "    ",
            mergedEntities[currentRow, entityCols['entity_id']],
            " [label=\"",
            mergedEntities[currentRow, entityCols['entity_label']],
            "\", ",
            paste0(entityAttributes, " = \"",
                   mergedEntities[currentRow, entityAttributes],
                   "\"",
                   collapse=","),
            "]\n\n"
          );

      }

      dot_code <-
        paste0(
          dot_code,
          "  }\n"
        );

    }
  }

  dot_code <- paste0(dot_code, "\n");

  ### Regular nodes

  for (currentRow in which(noParentId)) {

    dot_code <-
      paste0(
        dot_code,
        "  ",
        mergedEntities[currentRow, entityCols['entity_id']],
        " [label=\"",
        mergedEntities[currentRow, entityCols['entity_label']],
        "\", ",
        paste0(entityAttributes, "=\"",
               mergedEntities[currentRow, entityAttributes],
               "\"",
               collapse=","),
        "]\n\n"
      );

  }

  ### Connections

  ### First fix NAs
  mergedConnections[, connectionTypesCols['connection_label']] <-
    ifelse(
      is.na(mergedConnections[, connectionTypesCols['connection_label']]),
      "",
      mergedConnections[, connectionTypesCols['connection_label']]
    );

  for (currentRow in 1:nrow(mergedConnections)) {

    dot_code <-
      paste0(
        dot_code,
        "  ",
        mergedConnections[currentRow, connectionsCols['from_entity_id']],
        " -> ",
        mergedConnections[currentRow, connectionsCols['to_entity_id']],
        " [label=\"",
        mergedConnections[currentRow, connectionTypesCols['connection_label']],
        "\", ",
        paste0(connectionAttributes, "=\"",
               mergedConnections[currentRow, connectionAttributes],
               "\"",
               collapse=","),
        "]\n\n"
      );

  }

  dot_code <- paste0(dot_code, "\n}\n");

  ###---------------------------------------------------------------------------

  # msg("Creating node and edge data frames.\n", silent=silent);
  #
  ### Convert merged dataframes into lists
  # entitiesAsList <-
  #   c(list(n = length(mergedEntities[, entityCols['entity_type_id']]),
  #          type = mergedEntities[, entityCols['entity_type_id']],
  #          label = mergedEntities[, entityCols['entity_label']]),
  #     as.list(mergedEntities[, entityAttributes]));
  #
  # nodes_df <-
  #   do.call(DiagrammeR::create_node_df,
  #           entitiesAsList);
  #
  # ### Create a vector to conveniently convert entity_ids to the node_df ids
  # node_ids <- stats::setNames(nodes_df$id,
  #                             nm=nodes_df$entity_id);
  #
  # connectionsAsList <-
  #   c(list(from = node_ids[mergedConnections[, "from_entity_id"]],
  #          to = node_ids[mergedConnections[, "to_entity_id"]],
  #          rel = mergedConnections[, connectionsCols["connection_type_id"]]),
  #     as.list(mergedConnections[, connectionAttributes]));
  #
  # edges_df <-
  #   do.call(DiagrammeR::create_edge_df,
  #           connectionsAsList);
  #
  # ### Wrap labels in node_df
  # nodes_df$label <-
  #   wrapVector(
  #     nodes_df$label,
  #     maxLabelLength
  #   );
  #
  # res$intermediate$nodes_df <-
  #   nodes_df;
  # res$intermediate$edges_df <-
  #   edges_df;
  #
  # msg("Creating graph.\n", silent=silent);
  #
  # ### Combine node and edge dataframes into a graph
  # graph <-
  #   DiagrammeR::create_graph(nodes_df = nodes_df,
  #                            edges_df = edges_df,
  #                            graph_name = title,
  #                            directed = directed);
  #
  # graph <-
  #   do.call(apply_graph_theme,
  #           c(list(graph = graph,
  #                  directed = directed),
  #             list(c("layout", layout, "graph")),
  #             graph_styling));
  #
  # ### From DiagrammeR::export_graph
  # dot_code <- DiagrammeR::generate_dot(graph);

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
