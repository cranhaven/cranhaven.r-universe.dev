#' Acyclic Behavior Change Diagram
#'
#' This function generates an acyclic behavior change diagram (ABCD)
#' from a specification in a google sheet or .csv file. An ABCD is
#' a logic model that illustrates the assumptions underlying a
#' behavior change intervention. Specifically, the ABCD shows the
#' assumed causal and structural assumptions, thereby showing what is
#' assumed to cause what (e.g. which elements of the intervention are
#' assumed to influence which aspects of the target population's
#' psychology?) and what is assumed to consist of what (e.g. which
#' determinants are assumed to contain which specific aspects of the
#' target population's psychology?).
#'
#' Specifically, a full ABCD is a model that shows the following
#' elements:
#'
#' - **Behavior Change Principles (BCPs)**: The specific
#'     psychological principles engaged to influence the relevant
#'     sub-determinants, usually selected using the determinants
#'     to which the sub-determinants 'belong'. These are also
#'     known as methods of behavior change in the Intervention
#'     Mapping framework, or behavior change techniques, BCTs,
#'     in the Behavior Change Wheel approach. For a list of 99
#'     BCPs, see Kok et al. (2016).
#' - **Conditions for effectiveness**: The conditions
#'     that need to be met for a Behavior Change Principle (BCP) to
#'     be effective. These conditions depend on the specific
#'     underlying Evolutionary Learning Processes (ELPs) that the
#'     BCP engages (Crutzen & Peters, 2018). If the conditions for
#'     effectiveness (called *parameters* for effectiveness in the
#'     Intervention Mapping framework) are not met, the method will
#'     likely not be effective, or at least, not achieve its
#'     maximum effectiveness.
#' - **Applications**: Since BCP's describe aspects of
#'     human psychology in general, they are necessarily formulated
#'     on a generic level. Therefore, using them in an intervention
#'     requires translating them to the specific target population,
#'     culture, available means, and context. The result of this
#'     translation is the application of the BCP. Multiple BCPs can
#'     be combined into one application; and one BCP can be applied in
#'     multiple applications (see Kok, 2014).
#' - **Sub-determinants**: Behavior change interventions
#'     engage specific aspects of the human psychology (ideally, they
#'     specifically, target those aspects found most important in
#'     predicting the target behavior, as can be established with
#'     \code{\link[behaviorchange]{CIBER}} plots. These aspects are
#'     called sub-determinants (the Intervention Mapping framework
#'     references *Change Objectives*, which are sub-determinants
#'     formulated according to specific guidelines). In some
#'     theoretical traditions, sub-determinants are called *beliefs*.
#' - **Determinants**: The overarching psychological constructs that
#'     are defined as clusters of specific aspects of the human
#'     psychology that explain humans' behavior (and are targeted
#'     by behavior change interventions). Psychological theories
#'     contain specific definitions of such determinants, and make
#'     statements about how they relate to each other and to human
#'     behavior. There are also theories (and exists empirical
#'     evidence) on how these determinants can be changed (i.e. BCPs),
#'     so althought the sub-determinants are what is targeted in an
#'     intervention, the selection of feasible BCPs requires knowing
#'     to which determinants those sub-determinants belong.
#' - **Performance objectives**: The specific sub-behaviors that often
#'     underlie (or make up) the ultimate target behavior. These are
#'     distinguished from the overarching target behavior because
#'     the relevant determinants of these sub-behaviors can be
#'     different: for example, the reasons why people do or do not
#'     *buy* condoms can be very different from the reasons why they
#'     do or do not *carry* condoms or why they do or do not
#'     *negotiate* condom use with a sexual partner.
#' - **Behavior**: The ultimate target behavior of the intervention,
#'     usually an umbrella that implicitly contains multiple
#'     performance objectives.
#'
#' For details, see Peters et al. (2019).
#'
#' @param specs The specifications: either a google sheets URL,
#'   the path to a local file, a character vector with both,
#'   or a matrix or data frame
#' @param specCols The order of the columns. This character vector
#'   specified the order of the elements of an ABCD. In the default
#'   order, from left to right, these are (see below for definitions
#'    and more details):
#'   - `bcps` = **Behavior Change Principles (BCPs)**;
#'   - `cnds` = **Conditions for effectiveness**;
#'   - `apps` = **Applications**;
#'   - `sdts` = **Sub-determinants**;
#'   - `dets` = **Determinants**;
#'   - `pobs` = **Performance Objectives**;
#'   - `behs` = **Behaviors**;
#' @param localBackup Whether to write the specifications
#'   to a local backup
#' @param title The title of the diagram
#' @param outputFile If specified, the ABCD is written to
#'   this file using [DiagrammeR::export_graph].
#' @param outputWidth,outputHeight If an `outputFile` is
#'   specified, these determine its width and height (in pixels)
#' @param includeColNames Whether to include the column names
#'   as titles/legend for the entities in each 'column'
#'   of the ABCD.
#' @param maxLabelLength At which width to word wrap the
#'   labels.
#' @param nodeFontSize,edgeFontSize,colNameFontSize Font sizes of the nodes (i.e.
#' the text in boxes), edges (basically the conditions for effectiveness) and the
#' column names (at the bottom).
#' @param grayscale Whether to use the `colorTheme` or produce a grayscale ABCD.
#' @param colorTheme The color theme, a named list containing the colors,
#' each a character vector with three HTML (hex) color values. The list elements
#' have to be named `bcp`, `condition_for_effectiveness`, `application`,
#' `sub_determinant`, `determinant`, `sub_behavior`, and `target_behavior`, and
#' each must contain a named vector with two elements named `fill`, `stroke`,
#' and `text`, containing the color codes for the fill, stroke, and text,
#' respectively; see `behaviorchange::opts$get("aabbcc")` for an example.
#' @param penWidth The width of the pen to draw the strokes.
#' @param silent Whether to suppress (`TRUE`) or show
#'   (`FALSE`) more detailed information.
#' @param returnGraphOnly,returnSvgOnly Whether to return the full results
#'   object or only either the [DiagrammeR::DiagrammeR] graph or a one-value
#'   character vector containing a Scalable Vector Graphic as produced by
#'   [DiagrammeRsvg::export_svg()].
#' @param columnWarning Can be used to suppress the warning if the number of
#' columns is too large.
#' @param graphTheme Specific settings to apply to the graph
#' using [apply_graph_theme()]; a list of vectors, where each vector has
#' three elements: the setting, the value, and what to apply it to ('node',
#' 'edge', or 'graph').
#' @param regExReplacements A list of pairs of regular
#'   expressions that will be applied to the specifications
#'   before generating the ABCD. This can be used to sanitize
#'   problematic characters (e.g. ', " and \).
#' @param x The ABCD object to print (as generated by a call to `abcd`).
#' @param width,height Width and height to use when printing the ABCD.
#' @param \dots Any additional arguments are passed on to
#' [DiagrammeR::render_graph()].
#'
#' @return A list consisting of an `input`, `intermediate`, and
#'   `output` list, where the ABCD is stored in the `output` list
#'   as a [DiagrammeR::DiagrammeR] called `graph`.
#' @author
#'   Gjalt-Jorn Peters, \email{gjalt-jorn@@a-bc.eu}, with
#'   contributions from Matti Heino and Sander Eggers.
#' @references
#'   Crutzen, R., & Peters, G.-J. Y. (2018). Evolutionary
#'   learning processes as the foundation for behaviour change.
#'   *Health Psychology Review,* 12(1), 43–57.
#'   https://doi.org/10.1080/17437199.2017.1362569
#'
#'   Kok, G. (2014). A practical guide to effective behavior
#'   change: How to apply theory- and evidence-based behavior
#'   change methods in an intervention. *European Health Psychologist*,
#'   16(5), 156–170. https://doi.org/10.31234/osf.io/r78wh
#'
#'   Kok, G., Gottlieb, N. H., Peters, G.-J. Y., Mullen,
#'   P. D., Parcel, G. S., Ruiter, R. A. C., … Bartholomew,
#'   L. K. (2016). A taxonomy of behavior change methods:
#'   an Intervention Mapping approach. *Health Psychology
#'   Review*, 10(3), 297–312.
#'   https://doi.org/10.1080/17437199.2015.1077155
#'
#'   Peters, G.-J. Y., et al. (2019) The core of
#'   behavior change: introducing the Acyclic Behavior Change
#'   Diagram to report and analyze interventions.
#' @examples ### Load one of the ABCD matrices supplied
#' ### with the behaviorchange package
#' data(abcd_specification_example_xtc);
#'
#' ### Create ABCD matrix (using 'print' to allow pkgdown() to print properly).
#' print(behaviorchange::abcd(abcd_specification_example_xtc));
#'
#' ### Other examples not executed during testing as creating ABCDs takes long
#'
#' \dontrun{
#' ### Change the appearance; note that many attributes are specified
#' ### for specific elements, and element-level settings always override
#' ### the global settings that can be specified here.
#' print(
#'   behaviorchange::abcd(
#'     abcd_specification_example_xtc,
#'     graphTheme = list(
#'       c("fontname", "Courier New", "node")
#'     )
#'   )
#' );
#' }
#'
#' @rdname abcd
#' @export
abcd <- function(specs,
                 specCols = c('bcps', 'cnds', 'apps', 'sdts', 'dets', 'pobs', 'behs'),
                 localBackup = NULL,
                 title = "Acyclic Behavior Change Diagram\n\n",
                 outputFile = NULL,
                 outputWidth=3000,
                 outputHeight=1500,
                 includeColNames = TRUE,
                 maxLabelLength = 30,
                 nodeFontSize = 10,
                 edgeFontSize = 8,
                 colNameFontSize = nodeFontSize,
                 grayscale = FALSE,
                 colorTheme = behaviorchange::opts$get("aabbcc"),
                 penWidth = 1,
                 silent = FALSE,
                 returnGraphOnly = FALSE,
                 returnSvgOnly = FALSE,
                 columnWarning = TRUE,
                 graphTheme = list(c("fontname", "Arial", "node")),
                 regExReplacements =
                   behaviorchange::opts$get("diagrammerSanitization")) {

  if (length(specCols) < 7) {
    stop("An ABCD matrix must always have at least seven columns.");
  }

  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());

  loadedDatasheet <- FALSE;
  gs_url <- NULL;

  if (is.character(specs) && length(specs) == 2) {
    whichIsURL <- grepl('^http.?://', specs);
    if (!(TRUE %in% whichIsURL && FALSE %in% whichIsURL)) {
      stop("If argument 'specs' has two values, one should be a Google Sheets URL and one should be a filename!");
    }
    gs_url <- specs[whichIsURL];
    file <- specs[!whichIsURL];
  } else if (is.character(specs) && length(specs) == 1) {
    if (grepl('^http.?://', specs)) {
      gs_url <- specs[grepl('^http.?://', specs)];
    } else {
      file <- specs;
    }
  } else if (is.matrix(specs) || is.data.frame(specs)) {
    datasheet <- specs;
    loadedDatasheet <- TRUE;
  } else {
    stop("Argument 'specs' was not a character value or vector, a matrix, or a data frame!");
  }

  ### Import sheets, if sheets identifier (gs_url) was provided
  if (!loadedDatasheet && !is.null(gs_url)) {
    tryCatch({
      googlesheets4::gs4_deauth();
      gsObject <- googlesheets4::gs4_get(gs_url);
      datasheet <- googlesheets4::read_sheet(gsObject);
      loadedDatasheet <- TRUE;
      if (!silent) {
        cat("Successfully read the data from Google sheets.\n");
      }
    },
    error = function(e) {
      if (!silent) {
        cat("You specified a google sheet, but I have problems",
            "accessing it - trying to access a local file.\n");
      }
    });
  }

  ### If the sheets identifier was not provided, or loading it failed,
  ### load from a local file
  if (!loadedDatasheet) {

    ### Check whether the files exist
    if (!is.null(file)) {
      if (!file.exists(file)) {
        stop("You specified a filename for 'file' ('",
             file, "'), but it does not exist.");
      }
    } else {
      stop("Did not manage to load the specifications!");
    }

    if (grepl("\\.xls", file)) {
      if (requireNamespace("openxlsx", quietly=TRUE)) {
        datasheet <- openxlsx::read.xlsx(file);
      } else {
        stop("To read Excel files, you need to have `openxlsx` installed. ",
             "You can install it with:\n\n  install.packages('openxlsx');\n");
      }
    } else {
      datasheet <- utils::read.csv(file, stringsAsFactors = FALSE);
    }

    if (!silent) {
      cat("Succesfully read the ABCD specifications from local file '",
          file, "'.\n", sep="");
    }

  }

  ### Write local backup, if need be
  if (!is.null(localBackup)) {
    utils::write.csv(datasheet,
                     row.names=FALSE,
                     localBackup);
    if (!silent) {
      cat("Stored local backup to '", localBackup, "'.\n", sep="");
    }
  }

  ### Save original names for use later on (since
  ### conversion to a regular data frame will replace
  ### e.g. spaces etc)
  originalColNames <- names(datasheet);

  res$intermediate$datasheet <-
    datasheet <-
    as.data.frame(lapply(datasheet,
                         function(column) {
                           for (i in seq_along(regExReplacements)) {
                             column <- gsub(regExReplacements[[i]][1],
                                            regExReplacements[[i]][2],
                                            column);
                            }
                            return(column);
                          }),
                  stringsAsFactors=FALSE);

  res$intermediate$datasheet <-
    datasheet <-
    as.data.frame(lapply(datasheet,
                         function(column) {
                           return(ifelse(is.na(column) | (column == "NA"),
                                         "This still has to be specified",
                                         column));
                         }),
                  stringsAsFactors=FALSE);

  ### Get number of columns with data
  useCols <- ncol(datasheet);

  ### This has been removed to enforce consistent 7-column ABCD matrices
  omitColOrder <- c();

  ### Check for problematic numbers of columns
  if (useCols > length(specCols)) {
    if (columnWarning) {
      warning("The specification contains ", useCols,
              "columns, but I can use at most  ",
              (length(specCols) - length(omitColOrder)),
              "columns: ingnoring the right-most columns.");
    }
    useCols <- length(specCols);
  } else if (useCols < (length(specCols) - length(omitColOrder))) {
    stop("The specification contains ", useCols,
         "columns, but I need at least ",
         (length(specCols) - length(omitColOrder)),
         "columns.");
  }

  ### If this is not a complete ABCD specification,
  ### omit the required number of columns
  usedCols <- setdiff(specCols, utils::head(omitColOrder,
                                            length(specCols) - useCols));

  ### Process column names; first get them from the list of
  ### original column names, omitting the column for conditions
  colNames <- originalColNames[which(usedCols %in% setdiff(specCols, 'cnds'))];
  col_ids <- seq_along(colNames);
  names(col_ids) <- colNames;
  colNames <- sapply(colNames, function(x)
    return(sapply(x, function(xx)
      return(paste0(strwrap(xx, maxLabelLength), collapse="\n")))));

  ### Extract the columns from the datasheet
  cols <- list();
  cols <- lapply(usedCols,
                 function(colname) {
                   return(unique(datasheet[, which(usedCols == colname)]));
                 });





  # if (ncol(datasheet) < 5) {
  #   stop("The loaded data sheet does not have at least five columns.");
  # } else if (ncol(datasheet) == 5) {
  #   includeBehavior <- FALSE;
  #   useCols <- 5;
  # } else if (ncol(datasheet) == 6) {
  #   includeBehavior <- TRUE;
  #   useCols <- 6;
  # } else if (ncol(datasheet) > 6) {
  #   includeBehavior <- TRUE;
  #   useCols <- 6;
  # }


  ### Extract entities of each type and set their IDs
  bcps <- unique(datasheet[, which(usedCols == 'bcps')]);
  bcp_ids <- 1:length(bcps) + max(col_ids);
  names(bcp_ids) <- bcps;
  bcps <- sapply(bcps, function(x)
    return(sapply(x, function(xx)
      return(paste0(strwrap(xx, maxLabelLength), collapse="\n")))));

  if ('cnds' %in% usedCols) {
    cnds <-
      unique(
        datasheet[, c(which(usedCols=='cnds'),
                      which(usedCols == 'bcps'),
                      which(usedCols == 'apps')
                      )]
      );
    cnds <- sapply(cnds, function(x)
      return(sapply(x, function(xx)
        return(paste0(strwrap(xx, maxLabelLength), collapse="\n")))));
  }

  apps <- unique(datasheet[, which(usedCols == 'apps')]);
  app_ids <- 1:length(apps) + max(bcp_ids);
  names(app_ids) <- apps;
  apps <- sapply(apps, function(x)
    return(sapply(x, function(xx)
      return(paste0(strwrap(xx, maxLabelLength), collapse="\n")))));

  sdts <- unique(datasheet[, which(usedCols == 'sdts')]);
  sdt_ids <- 1:length(sdts) + max(app_ids);
  names(sdt_ids) <- sdts;
  sdts <- sapply(sdts, function(x)
    return(sapply(x, function(xx)
      return(paste0(strwrap(xx, maxLabelLength), collapse="\n")))));

  dets <- unique(datasheet[, c(which(usedCols == 'dets'),
                               which(usedCols == 'pobs'))])[, 1];
  det_ids <- 1:length(dets) + max(sdt_ids);
  ### For the names, attach the performance objectives
  names(det_ids) <- apply(unique(datasheet[, c(which(usedCols == 'dets'),
                                               which(usedCols == 'pobs'))]),
                          1, paste, collapse="_");
  dets <- sapply(dets, function(x)
    return(sapply(x, function(xx)
      return(paste0(strwrap(xx, maxLabelLength), collapse="\n")))));

  pobs <- unique(datasheet[, which(usedCols == 'pobs')]);
  pob_ids <- 1:length(pobs) + max(det_ids);
  names(pob_ids) <- pobs;
  pobs <- sapply(pobs, function(x)
    return(sapply(x, function(xx)
      return(paste0(strwrap(xx, maxLabelLength), collapse="\n")))));

  if ('behs' %in% usedCols) {
    behs <- unique(datasheet[, which(usedCols == 'behs')]);
    beh_ids <- 1:length(behs) + max(pob_ids);
    names(beh_ids) <- behs;
    behs <- sapply(behs, function(x)
      return(sapply(x, function(xx)
        return(paste0(strwrap(xx, maxLabelLength), collapse="\n")))));
  }

  ### Combine into one list for vectorized processing
  entity_labels <- list(bcps = bcps,
                        apps = apps,
                        sdts = sdts,
                        dets = dets,
                        pobs = pobs);
  if ('behs' %in% usedCols) {
    entity_labels <- c(entity_labels,
                       list(behs = behs));
  }

  entity_ids <- list(bcps = bcp_ids,
                     apps = app_ids,
                     sdts = sdt_ids,
                     dets = det_ids,
                     pobs = pob_ids);
  if ('behs' %in% usedCols) {
    entity_ids <- c(entity_ids,
                    list(behs = beh_ids));
  }

  node_names <- c('bcps', 'apps', 'sdts', 'dets', 'pobs');
  edge_names <- c('bcp_app_edges',
                  'app_sdt_edges',
                  'sdt_det_edges',
                  'det_pob_edges');
  if ('behs' %in% usedCols) {
    node_names <- c(node_names,
                    'behs');
    edge_names <- c(edge_names,
                    'pob_beh_edges');
  }

  ### Create a datasheet with IDs instead of labels
  datasheet_ids <- data.frame(bcps = bcp_ids[datasheet[, which(usedCols == 'bcps')]],
                              apps = app_ids[datasheet[, which(usedCols == 'apps')]],
                              sdts = sdt_ids[datasheet[, which(usedCols == 'sdts')]],
                              dets = det_ids[apply(datasheet[,
                                                             c(which(usedCols == 'dets'),
                                                               which(usedCols == 'pobs'))],
                                                   1, paste, collapse="_")],
                              pobs = pob_ids[datasheet[, which(usedCols == 'pobs')]]);

  if ('behs' %in% usedCols) {
    datasheet_ids[, 'behs'] <- beh_ids[datasheet[, which(usedCols == 'behs')]];
  }

  bcp_app_edges <- unique(datasheet_ids[, 1:2]);
  app_sdt_edges <- unique(datasheet_ids[, 2:3]);
  sdt_det_edges <- unique(datasheet_ids[, 3:4]);
  det_pob_edges <- unique(datasheet_ids[, 4:5]);

  if ('behs' %in% usedCols) {
    pob_beh_edges <- unique(datasheet_ids[, 5:6]);
  }

  ###---------------------------------------------------------------------------
  ### Start on the node data frames
  ###---------------------------------------------------------------------------

  if (grayscale) {
    colorTheme <-
      list(
        target_behavior = c(fill = "#FFFFFF", stroke = "#000000", text = "#000000"),
        sub_behavior = c(fill = "#FFFFFF", stroke = "#000000", text = "#000000"),
        determinant = c(fill = "#FFFFFF", stroke = "#000000", text = "#000000"),
        sub_determinant = c(fill = "#EEEEEE", stroke = "#EEEEEE", text = "#000000"),
        application = c(fill = "#FFFFFF", stroke = "#000000", text = "#000000"),
        condition_for_effectiveness = c(fill = "#FFFFFF", stroke = "#000000", text = "#000000"),
        bcp = c(fill = "#FFFFFF", stroke = "#000000", text = "#000000")
      )
  }

  nodeAttributes <- list(bcps = list(shape = 'box',
                                     color = colorTheme$bcp['stroke'],
                                     fillcolor = colorTheme$bcp['fill'],
                                     fontcolor = colorTheme$bcp['text'],
                                     style="rounded,filled",
                                     fontsize=nodeFontSize,
                                     penwidth=penWidth),
                         apps = list(shape = 'box',
                                     color = colorTheme$application['stroke'],
                                     fillcolor = colorTheme$application['fill'],
                                     fontcolor = colorTheme$application['text'],
                                     style="filled",
                                     fontsize=nodeFontSize,
                                     penwidth=penWidth),
                         sdts = list(shape = 'box',
                                     color = colorTheme$sub_determinant['stroke'],
                                     fillcolor = colorTheme$sub_determinant['fill'],
                                     fontcolor = colorTheme$sub_determinant['text'],
                                     style="filled",
                                     fontsize=nodeFontSize,
                                     penwidth=penWidth),
                         dets = list(shape = 'ellipse',
                                     color = colorTheme$determinant['stroke'],
                                     fillcolor = colorTheme$determinant['fill'],
                                     fontcolor = colorTheme$determinant['text'],
                                     style = "filled",
                                     fontsize=nodeFontSize,
                                     penwidth=penWidth),
                         pobs = list(shape = 'box',
                                     color = colorTheme$sub_behavior['stroke'],
                                     fillcolor = colorTheme$sub_behavior['fill'],
                                     fontcolor = colorTheme$sub_behavior['text'],
                                     style="rounded,filled",
                                     fontsize=nodeFontSize,
                                     penwidth=penWidth));

  if ('behs' %in% usedCols) {
    nodeAttributes <-
      c(nodeAttributes,
        list(behs = list(shape = 'box',
                         color = colorTheme$target_behavior['stroke'],
                         fillcolor = colorTheme$target_behavior['fill'],
                         fontcolor = colorTheme$target_behavior['text'],
                         style="rounded,filled",
                         fontsize=nodeFontSize,
                         penwidth = penWidth)));
  }

  node_dfs <-
    lapply(node_names,
           function(i) {
             return(DiagrammeR::create_node_df(n=length(entity_ids[[i]]),
                                               label=entity_labels[[i]],
                                               type=i,
                                               style=nodeAttributes[[i]]$style,
                                               color=nodeAttributes[[i]]$color,
                                               fillcolor=nodeAttributes[[i]]$fillcolor,
                                               penwidth=nodeAttributes[[i]]$penwidth,
                                               fontsize=nodeAttributes[[i]]$fontsize,
                                               fontcolor=nodeAttributes[[i]]$fontcolor,
                                               fixedsize=FALSE,
                                               shape=nodeAttributes[[i]]$shape));
           });

  if (includeColNames) {
    colNames_node_df <-
      list(DiagrammeR::create_node_df(n=length(col_ids),
                                      label=colNames,
                                      type="colName",
                                      style="filled",
                                      color="#FFFFFF",
                                      fillcolor="#FFFFFF",
                                      penwidth=penWidth,
                                      fontcolor="#000000",
                                      fixedsize=FALSE,
                                      shape="plaintext",
                                      fontsize=colNameFontSize));
    final_nodeDf <- do.call(DiagrammeR::combine_ndfs,
                            c(colNames_node_df,
                              node_dfs));
  } else {
    final_nodeDf <- do.call(DiagrammeR::combine_ndfs,
                            node_dfs);
  }

  ######################################################################
  ### Start on the edge data frames
  ######################################################################

  edges_from <- c(bcp_app_edges[, 1],
                  app_sdt_edges[, 1],
                  sdt_det_edges[, 1],
                  det_pob_edges[, 1]);
  edges_to <- c(bcp_app_edges[, 2],
                app_sdt_edges[, 2],
                sdt_det_edges[, 2],
                det_pob_edges[, 2]);

  edges_from <- list(bcp_app_edges = bcp_app_edges[, 1],
                     app_sdt_edges = app_sdt_edges[, 1],
                     sdt_det_edges = sdt_det_edges[, 1],
                     det_pob_edges = det_pob_edges[, 1]);
  edges_to <- list(bcp_app_edges = bcp_app_edges[, 2],
                   app_sdt_edges = app_sdt_edges[, 2],
                   sdt_det_edges = sdt_det_edges[, 2],
                   det_pob_edges = det_pob_edges[, 2]);

  if ('behs' %in% usedCols) {
    edges_from <- c(edges_from,
                    pob_beh_edges[, 1]);
    edges_to <- c(edges_to,
                  pob_beh_edges[, 2]);
    edges_from <- c(edges_from,
                    list(pob_beh_edges = pob_beh_edges[, 1]));
    edges_to <- c(edges_to,
                  list(pob_beh_edges = pob_beh_edges[, 2]));
  }

  ### Set edge attributes so they can be different
  ### for the edges from and to different types of nodes
  edgeAttributes <-
    list(bcp_app_edges = list(arrowhead = 'icurve',
                              label=letters[seq_along(edges_to$bcp_app_edges)],
                              tooltip="The parameters for use have to be explained separately",
                              color = "#000000",
                              fontsize=edgeFontSize,
                              penwidth=penWidth),
         app_sdt_edges = list(arrowhead = 'normal',
                              label="",
                              tooltip="Influence",
                              color = "#000000",
                              fontsize=edgeFontSize,
                              penwidth=penWidth),
         sdt_det_edges = list(arrowhead = 'dot',
                              label="",
                              tooltip="Is a part of",
                              color = "#000000",
                              fontsize=edgeFontSize,
                              penwidth=penWidth),
         det_pob_edges = list(arrowhead = 'normal',
                              label="",
                              tooltip="Influences",
                              color = "#000000",
                              fontsize=edgeFontSize,
                              penwidth=penWidth));

  if ('cnds' %in% usedCols) {
    edgeAttributes$bcp_app_edges$label <- cnds;
    edgeAttributes$bcp_app_edges$tooltip <- cnds;
  }

  ### If the ABCD includes both PO's and behavior, add
  ### the edges to the behavior
  if ('behs' %in% usedCols) {
    edgeAttributes <-
      c(edgeAttributes,
        list(pob_beh_edges = list(arrowhead = 'dot',
                                  label="",
                                  tooltip="Is a part of",
                                  color = "#000000",
                                  fontsize=edgeFontSize,
                                  penwidth=penWidth)));
  }

  edge_dfs <-
    lapply(edge_names,
           function(i) {
             return(DiagrammeR::create_edge_df(from=edges_from[[i]],
                                               to=edges_to[[i]],
                                               label=edgeAttributes[[i]]$label,
                                               tooltip=edgeAttributes[[i]]$tooltip,
                                               fontsize=edgeAttributes[[i]]$fontsize,
                                               penwidth=edgeAttributes[[i]]$penwidth,
                                               arrowhead=edgeAttributes[[i]]$arrowhead,
                                               color=edgeAttributes[[i]]$color));
           });

  if (includeColNames) {
    colNames_edge_df <-
      list(DiagrammeR::create_edge_df(from=1:(length(col_ids)-1),
                                      to=2:length(col_ids),
                                      color = "#FFFFFF",
                                      penwidth=penWidth));
    final_edgeDf <- do.call(DiagrammeR::combine_edfs,
                            c(colNames_edge_df,
                              edge_dfs));
  } else {
    final_edgeDf <- do.call(DiagrammeR::combine_edfs,
                            edge_dfs);
  }

  ###---------------------------------------------------------------------------
  ### Create final graph, set attributes, and return the result
  ###---------------------------------------------------------------------------

  graph <-
    DiagrammeR::create_graph(nodes_df = final_nodeDf,
                             edges_df = final_edgeDf,
                             graph_name = title);

  graphTheme <-
    supplementDefaultGraphTheme(
      graphTheme
    );

  graph <-
    do.call(
      apply_graph_theme,
      c(
        list(graph = graph),
        graphTheme
      )
    );

  if (!is.null(outputFile)) {
    for (currentFile in outputFile) {
      DiagrammeR::export_graph(graph,
                               file_name = currentFile,
                               file_type = tools::file_ext(currentFile),
                               width=outputWidth,
                               height=outputHeight,
                               title = DiagrammeR::get_graph_name(graph));
    }
  }

  ### From DiagrammeR::export_graph
  dot_code <- DiagrammeR::generate_dot(graph);
  graphSvg <-
    DiagrammeRsvg::export_svg(DiagrammeR::grViz(dot_code));
  graphSvg <-
    sub(".*\n<svg ", "<svg ", graphSvg);
  graphSvg <- gsub('<svg width=\"[0-9]+pt\" height=\"[0-9]+pt\"\n viewBox=',
                   '<svg viewBox=',
                   graphSvg);

  res$output$graph <- graph;
  res$output$svg <- graphSvg;

  if (returnGraphOnly) {
    return(graph);
  } else if (returnSvgOnly) {
    return(graphSvg);
  } else {
    class(res) <- "abcdiagram";
    return(res);
  }

}

#' @rdname abcd
#' @method print abcdiagram
#' @export
print.abcdiagram <- function(x,
                             width=x$input$width,
                             height=x$input$height,
                             title = DiagrammeR::get_graph_name(x$output$graph),
                             ...) {
  print(DiagrammeR::render_graph(x$output$graph,
                                 width=width,
                                 height=height,
                                 title=title,
                                 ...));
}

#' Simple example datasets for ABCDs
#'
#' This are three (nested) datasets illustrating the logic model of change for
#' a simple condom use intervention in a way that can be visualised using
#' the [abcd] function. The full dataset is `abcd_specs_full`, a subset
#' that does not explicitly include the conditions for effectiveness
#' (instead showing letters that can then be explained in, for example,
#' the manuscript text) is called `abcd_specs_without_conditions`, and
#' a version that only contains the information about one sub-behavior
#' (performance objective) is available as
#' `abcd_specs_single_po_without_conditions`. The variables in the full
#' dataset are:
#'
#' * `Behavior Change Principles`: The behavior change principles (BCPs), also known as methods for behavior change or 'behavior change techniques' (BCTs), that describe the psychological principles that are assumed to realise the change in the (sub-)determinants.
#' * `Conditions for effectiveness\\n(e.g. parameters for use)`: The conditions for effectiveness that describe the constraints and considerations taken into account in the translation of the BCPs to practical applications for the relevant target population, context, culture, etc.
#' * `Applications`: The applications of these BCPs. Where the BCPs describe theoretical principles, the applications are more or less tangible intervention elements.
#' * `Sub-determinants\\n(e.g. beliefs; can be formulated as Change Objectives)`: The specific aspects of teh target population's psychology that are targeted by the BCPs (e.g. beliefs, or in Intervention Mapping vocabulary, Change Objectives).
#' * `Determinants`: The determinants, psychological constructs, that the targeted sub-determinants are a part of, and that together predict the Performance Objectives (sub-behaviors).
#' * `Performance Objectives`: Explicitly defined sub-behaviors at a level of specificity that distinguishes them from other sub-behaviors, and that together form the target behavior.
#' * `Target Behavior`: The ultimate target behavior, usually defined at a relatively general level.
#'
#' In addition to these three datasets, a Dutch example specification
#' is included named `abcd_specs_dutch_xtc`, and the same in English as `abcd_specification_example_xtc`.
#'
#' Finally, `abcd_specification_empty` is an empty 'template' ABCD matrix.
#'
#' @docType data
#' @aliases abcd_specs_complete abcd_specs_without_conditions abcd_specs_single_po_without_conditions
#' abcd_specs_dutch_xtc abcd_specification_example_xtc abcd_specification_empty
#' @keywords data
#' @name abcd_specs_examples
#' @usage data(abcd_specs_complete)
#' @usage data(abcd_specs_without_conditions)
#' @usage data(abcd_specs_single_po_without_conditions)
#' @usage data(abcd_specification_example_xtc)
#' @usage data(abcd_specs_dutch_xtc)
#' @usage data(abcd_specification_empty)
#' @format For `abcd_specs_complete`, a data frame with 7 variables and 7 rows;
#' for `abcd_specs_without_conditions`, a data frame with 6 variables and 7 rows;
#' for `abcd_specs_single_po_without_conditions`, a data frame with 5 variables and 4 rows;
#' for `abcd_specification_example_xtc` and `abcd_specs_dutch_xtc`,
#' a data frame with 7 variables and 5 rows' and
#' for `abcd_specification_empty`, a data frame with 7 variables and 1 row.
c("abcd_specs_complete",
  "abcd_specs_without_conditions",
  "abcd_specs_single_po_without_conditions",
  "abcd_specification_example_xtc",
  "abcd_specs_dutch_xtc",
  "abcd_specification_empty");

# abcd_specs_complete <-
#   data.frame(c("Persuasive communication",
#                "Persuasive communication",
#                "Persuasive communication",
#                "Social comparison",
#                "Persuasive communication",
#                "Modeling",
#                "Guided practice"),
#              c("Research in this target population showed that infographics are perceived as trustworthy and are generally well-received.",
#                "Research in this target population showed that infographics are perceived as trustworthy and are generally well-received.",
#                "Showing such quotations yielded promising effects in a similar interventions in this population.",
#                "Chart allow comparing percentages of people using condoms allow showing frequencies, which people can process better then probabilities.",
#                "Showing such quotations yielded promising effects in a similar interventions in this population.",
#                "We selected role models from the target populations, and the modeling scripts were developed to demonstrate strategies to deal with the most common scenarios.",
#                "A mini-game allows for the desired interactivity, and this mini-game can piggy-back on coordinated efforts from this other campaign that people are doing."),
#              c("An infographic explains how condoms work.",
#                "An infographic explains how condoms work.",
#                "Quotations from people expressing approval.",
#                "A chart showing percentage of people using condoms.",
#                "Quotations from people expressing approval.",
#                "A role model illustrates bringing up condoms in different settings.",
#                "In a mini-game, target population individuals practice negotiation."),
#              c("Condoms help prevent HIV.",
#                "Condoms help prevent pregnancy.",
#                "Most people think buying condoms is normal.",
#                "Most people use condoms.",
#                "Most people approve of me suggesting to use condoms.",
#                "It is easy for me to bring up condoms.",
#                "If my partner is not enthusiastic, I know I can persuade them."),
#              c("Attitude",
#                "Attitude",
#                "Perceived Norms",
#                "Perceived Norms",
#                "Perceived Norms",
#                "Self-efficacy",
#                "Self-efficacy"),
#              c("Buy condoms",
#                "Buy condoms",
#                "Buy condoms",
#                "Negotiate condom use",
#                "Negotiate condom use",
#                "Negotiate condom use",
#                "Negotiate condom use"),
#              c("Condom use",
#                "Condom use",
#                "Condom use",
#                "Condom use",
#                "Condom use",
#                "Condom use",
#                "Condom use"),
#              stringsAsFactors = FALSE);
#
# names(abcd_specs_complete) <-
#   c('Behavior Change Principles',
#     'Conditions for effectiveness\n(e.g. parameters for use)',
#     'Applications',
#     'Sub-determinants\n(e.g. beliefs; can be formulated as Change Objectives)',
#     'Determinants',
#     'Performance Objectives',
#     'Target Behavior');
#
# abcd_specs_without_conditions <-
#   abcd_specs_complete[, c(1, 3:6)];
#
# abcd_specs_single_po_without_conditions <-
#   abcd_specs_complete[5:7, c(1, 3:6)];
#
# names(abcd_specs_single_po_without_conditions)[5] <-
#   'Behavior';
#
# devtools::use_data(abcd_specs_complete,
#                    abcd_specs_without_conditions,
#                    abcd_specs_single_po_without_conditions,
#                    overwrite=TRUE);
#
# abcd_complete <-
#   abcd(specs=c("https://docs.google.com/spreadsheets/d/1U1j-VoiK3WmfveJ7VpUMY_H9WNXDh85a8jKbM67AQSI/edit?usp=sharing",
#                "C:/Sync/Data/R/tmp/library/tmp/abcd-complete.csv"),
#        localBackup="C:/Sync/Data/R/tmp/abcd-complete.csv",
#        outputFile=c("C:/Sync/Data/R/tmp/abcd-complete.svg",
#                     "C:/Sync/Data/R/tmp/abcd-complete.png"));
#
# abcd_specs_without_conditions <-
#   abcd(specs=c("https://docs.google.com/spreadsheets/d/13VE1_1Oa38CidDDbiuIw7ZP8DIJ2qzs_i8wlJ63YuMI",
#                "C:/Sync/Data/R/tmp/abcd-without-conditions.csv"),
#        localBackup="C:/Sync/Data/R/tmp/abcd-without-conditions.csv",
#        outputFile=c("C:/Sync/Data/R/tmp/abcd-without-conditions.svg",
#                     "C:/Sync/Data/R/tmp/abcd-without-conditions.png"));
#
# abcd_specs_single_po_without_conditions <-
#   abcd(specs=c("https://docs.google.com/spreadsheets/d/1ib4CJlWUYcShwwue8kXq2519tTefK6T-orLarhuk0q0",
#                "C:/Sync/Data/R/tmp/abcd_specs_single_po_without_conditions.csv"),
#        localBackup="C:/Sync/Data/R/tmp/abcd_specs_single_po_without_conditions.csv",
#        outputFile=c("C:/Sync/Data/R/tmp/abcd_specs_single_po_without_conditions.svg",
#                     "C:/Sync/Data/R/tmp/abcd_specs_single_po_without_conditions.png"));

### Dutch example
# abcd_specs_dutch_xtc <- googlesheets::gs_read(googlesheets::gs_url("https://docs.google.com/spreadsheets/d/1EKVqtG1kmf0ZxEvFUOBqUmakXTd9Ye3aSyZucBqiyRM/edit?usp=sharing"))
#
#
# abcd_specs_dutch_xtc <-
#   data.frame(c("Persuasieve communicatie",
#                "Persuasieve communicatie",
#                "Persuasieve communicatie",
#                "Informatie over de goedkeuring van anderen",
#                "Rolmodellen"),
#              c("Boodschappen moeten relevant zijn, en niet teveel afwijken van wat de doelgroep gelooft; kan worden gestimuleerd met verrassing en herhaling; bevat argumenten.",
#                "Boodschappen moeten relevant zijn, en niet teveel afwijken van wat de doelgroep gelooft; kan worden gestimuleerd met verrassing en herhaling; bevat argumenten.",
#                "Boodschappen moeten relevant zijn, en niet teveel afwijken van wat de doelgroep gelooft; kan worden gestimuleerd met verrassing en herhaling; bevat argumenten.",
#                "Anderen staan ook echt positief tegenover het doelgedrag.",
#                "De doelgroep moet zich kunnen identificeren met het model; het model moet worstelen met het wenselijke gedrag; het model moet worden beloond voor het wenselijke gedrag."),
#              c("Een infographic laat zien hoe de effecten van XTC veranderen als de dosis verandert.",
#                "Een infographic laat zien hoe de effecten van XTC veranderen als de dosis verandert.",
#                "Een infographic laat zien hoe de effecten van XTC veranderen als de dosis verandert.",
#                "Het noemen van het Party Panel resultaat dat de meeste mensen lager willen doseren.",
#                "Een comic met voorbeelden van gesprekken over dosering."),
#              c("Als ik een hoge dosis XTC gebruik, dan voel ik me minder verbonden met anderen.",
#                "Als ik een hoge dosis XTC gebruik, dan voel ik me meer geisoleerd.",
#                "Als ik een hoge dosis XTC gebruik, dan onthoud ik minder.",
#                "De meeste mensen staan goedkeurend tegenover het vermijden van een hoge dosis MDMA.",
#                "Ik kan uitleggen waarom ik volgens de richtlijnen wil doseren."),
#              c("Attitude", "Attitude", "Attitude", "Waargenomen norm", "Waargenomen gedragscontrole"),
#              c("Besluiten een hoge dosis MDMA te willen vermijden",
#                "Besluiten een hoge dosis MDMA te willen vermijden", "Besluiten een hoge dosis MDMA te willen vermijden",
#                "Besluiten een hoge dosis MDMA te willen vermijden", "Met de uitgaansgroep van te voren bespreken welke dosis iedereen wil gebruiken"),
#              c("XTC doseren volgens de richtlijnen",
#                "XTC doseren volgens de richtlijnen",
#                "XTC doseren volgens de richtlijnen",
#                "XTC doseren volgens de richtlijnen",
#                "XTC doseren volgens de richtlijnen"));
#
# names(abcd_specs_dutch_xtc) <-
#   c("Gedragsveranderingsprincipes (`Behavior Change Principles`, BCPs, zoals methoden, BCTs, etc)",
#     "Voorwaarden voor effectiviteit (`parameters for use`)",
#     "Toepassingen (`applications`)",
#     "Sub-determinanten (opvattingen, bv. beliefs; kunnen worden geformuleerd als Change Objectives)",
#     "Determinanten",
#     "Sub-gedragingen (`Performance Objectives`)",
#     "Doelgedrag");

# abcd_specification_example_xtc <- data.frame(
#     `Behavior Change Principles` = c(
#       "Persuasive communication",
#       "Persuasive communication",
#       "Persuasive communication",
#       "Information about others' approval",
#       "Modeling (vicarious learning)"
#     ),
#     `Conditions for Effectiveness` = c(
#       "Messages must be relevant and not deviate too much from existing beliefs; can be stimulated with surprise and repetition; contains arguments.",
#       "Messages must be relevant and not deviate too much from existing beliefs; can be stimulated with surprise and repetition; contains arguments.",
#       "Messages must be relevant and not deviate too much from existing beliefs; can be stimulated with surprise and repetition; contains arguments.",
#       "Others do indeed approve of the target behavior.",
#       "The message recipient must identify with the model; the model has to be a coping model, struggling with the behavior, not a mastery model; the model must be positively reinforced."
#     ),
#     Applications = c(
#       "An infographic shows how the effects of ecstasy change as the dose increases.",
#       "An infographic shows how the effects of ecstasy change as the dose increases.",
#       "An infographic shows how the effects of ecstasy change as the dose increases.",
#       "Show the Party Panel result that illustrates that most people want to dose relatively low (compared to the strength of available ecstasy pills).",
#       "A comic with examples of how to discuss the dose you plan to take."
#     ),
#     `Sub-determinants (formulated as Change Objectives)` = c(
#       "If I use a high dose of ecstasy, I will feel less connected to others.",
#       "If I use a high dose of ecstasy, I will feel more isolated.",
#       "If I use a high dose of ecstasy, I will remember less",
#       "Most people approve of avoiding a high dose of ecstasy.",
#       "I can explain why I want to follow the dosing recommendations."
#     ),
#     Determinants = c(
#       "Attitude",
#       "Attitude",
#       "Attitude",
#       "Perceived norm",
#       "Perceived behavioral control"
#     ),
#     `Sub-behaviors (Performance Objectives)` = c(
#       "Decide to follow the dosing recommendations",
#       "Decide to follow the dosing recommendations",
#       "Decide to follow the dosing recommendations",
#       "Decide to follow the dosing recommendations",
#       "In advance, with the groups of friends, discuss everybody's planned dose."
#     ),
#     `Target behavior` = c(
#       "Following ecstasy dosing recommendations",
#       "Following ecstasy dosing recommendations",
#       "Following ecstasy dosing recommendations",
#       "Following ecstasy dosing recommendations",
#       "Following ecstasy dosing recommendations"
#     )
#   );
#
# names(abcd_specification_example_xtc) <-
#   c("Behavior Change Principles",
#     "Conditions for Effectiveness",
#     "Applications",
#     "Sub-determinants (formulated as Change Objectives)",
#     "Determinants",
#     "Sub-behaviors (Performance Objectives)",
#     "Target behavior");
#
# write.table(abcd_specification_example_xtc,
#             file=here::here("data", "abcd_specification_example_xtc.csv"),
#             quote=TRUE,
#             sep=";",
#             na="NA",
#             row.names=FALSE,
#             fileEncoding="UTF-8");

# abcd_specification_empty <- data.frame(
#     `Behavior Change Principles` = c(
#       "Enter the behavior change principle that is the active ingredient in this causal-structural chain here (e.g. a BCT)"
#     ),
#     `Conditions for Effectiveness` = c(
#       "Specify the conditions for effectiveness for that BCP and the target population and context here"
#     ),
#     Applications = c(
#       "Enter the application here (i.e. the BCPs 'tangible' implementation in the intervention)"
#     ),
#     `Sub-determinants` = c(
#       "Enter the sub-determinant underlying the relevant determinant here (e.g. a belief)"
#     ),
#     Determinants = c(
#       "Enter the determinant predicting the sub-behavior here"
#     ),
#     `Sub-behaviors (Performance Objectives)` = c(
#       "Enter the sub-behavior of the target behavior that is targeted in this causal-structural chain here"
#     ),
#     `Target behavior` = c(
#       "Enter the target behavior that is the ultimate target of this causal-structural chain here"
#     )
#   );
#
# names(abcd_specification_empty) <-
#   c("Behavior Change Principles",
#     "Conditions for Effectiveness",
#     "Applications",
#     "Sub-determinants (formulated as Change Objectives)",
#     "Determinants",
#     "Sub-behaviors (Performance Objectives)",
#     "Target behavior");
#
# write.table(abcd_specification_empty,
#             file=here::here("data", "abcd_specification_empty.csv"),
#             quote=TRUE,
#             sep=";",
#             na="NA",
#             row.names=FALSE,
#             fileEncoding="UTF-8");
