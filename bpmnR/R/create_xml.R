
#' create_xml.bpmn <- function(bpmn, ...) {
#'   # Defines every data structure that can be changed
#'   singular_of_bpmn_elements <- list(
#'     tasks = "task",
#'     sequenceFlows = "sequenceFlow",
#'     gateways = "gateway",
#'     startEvent = "startEvent",
#'     endEvent = "endEvent"
#'   )
#'   plural_of_bpmn_elements <- list(
#'     task = "tasks",
#'     sequenceFlow = "sequenceFlows",
#'     gateway = "gateways",
#'     startEvent = "startEvent",
#'     endEvent = "endEvent"
#'   )
#'   bpmn_shape_dimensions <- list(
#'     task = list(height = "80", width = "100"),
#'     gateway = list(height = "50", width = "50"),
#'     startEvent = list(height = "36", width = "36"),
#'     endEvent = list(height = "36", width = "36")
#'   )
#'   elements_empty_allowed <- c("gateways")
#'   attributes_to_factors <- c("gatewayType", "gatewayDirection")
#'   xml_attributes <-
#'     c("id", "name", "sourceRef", "targetRef", "gatewayDirection")
#'   type_attributes <- c("gatewayType")
#'
#'   # Converts certain attributes from a factor back to character type
#'   for (element in names(bpmn)) {
#'     for (attribute in names(bpmn[[element]])) {
#'       if (attribute %in% attributes_to_factors) {
#'         bpmn[[element]][, attribute] <-
#'           as.character(bpmn[[element]][, attribute])
#'       }
#'     }
#'   }
#'
#'   # Creates "defitions" node
#'   bpmn_xml <- .xml.create.definitions.node()
#'
#'   # Creates "process" node as a child from "definitions" node
#'   process_node <-
#'     .xml.create.process.node(
#'       bpmn_xml,
#'       bpmn,
#'       xml_attributes,
#'       type_attributes,
#'       singular_of_bpmn_elements,
#'       plural_of_bpmn_elements
#'     )
#'
#'   # Creates "BPMNDiagram" node as a child from "definitions" node
#'   BPMNDiagram_node <- .xml.create.BPMNDiagram.node(bpmn_xml,
#'                                                    bpmn,
#'                                                    process_node,
#'                                                    plural_of_bpmn_elements,
#'                                                    bpmn_shape_dimensions)
#'
#'   return(bpmn_xml)
#' }
#'
#' # ============================= HELPER FUNCTIONS ===============================
#'
#' # Creates "defitions" node
#' .xml.create.definitions.node <- function() {
#'   # Creates new XML document and assigns root node "definitions" in one step
#'   bpmn_xml <- xml_new_root("definitions")
#'
#'   # Sets namespaces and other "definitions" attributes
#'   xml_set_attrs(
#'     bpmn_xml,
#'     c(
#'       "id" = paste0("definitions-", UUIDgenerate()),
#'       "xmlns:bpmn" = "http://www.omg.org/spec/BPMN/20100524/MODEL",
#'       "xmlns:bpmndi" = "http://www.omg.org/spec/BPMN/20100524/DI",
#'       "xmlns:dc" = "http://www.omg.org/spec/DD/20100524/DC",
#'       "xmlns:di" = "http://www.omg.org/spec/DD/20100524/DI",
#'       "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
#'       "targetNamespace" = "http://bupar.net",
#'       "exporter" = "bupaR-suite bpmnR, https://github.com/bupaverse/bpmnR",
#'       "exporterVersion" = "0.0.1"
#'     )
#'   )
#'
#'   # Sets namespace prefix "bpmn" to root node
#'   # (which could not be done before namespace "bpmn" was defined)
#'   xml_set_namespace(bpmn_xml, prefix = "bpmn")
#'
#'   return(bpmn_xml)
#' }
#'
#' # Adds child to XML node and returns this child
#' .xml.add.and.return.child <-
#'   function(parent, child_to_add) {
#'     xml_add_child(parent, child_to_add)
#'     xml_child(parent, search = length(xml_children(parent)))
#'   }
#'
#' # Sets attributes to, changes name of and adds possible children to the BPMN element node
#' .xml.modifies.bpmn.element.node <-
#'   function(bpmn_element_node,
#'            individual_bpmn_element,
#'            xml_attributes,
#'            type_attributes) {
#'     for (attributes in individual_bpmn_element) {
#'       attribute_names <- names(attributes)
#'       for (j in seq_along(attributes)) {
#'         if (attribute_names[[j]] %in% xml_attributes) {
#'           # Sets attribute to the BPMN element node
#'           xml_set_attr(bpmn_element_node,
#'                        attribute_names[[j]],
#'                        attributes[[j]])
#'         } else if (attribute_names[[j]] %in% type_attributes) {
#'           # Changes name of the BPMN element node
#'           xml_name(bpmn_element_node) <-
#'             attributes[[j]]
#'         } else {
#'           # Adds child to the BPMN element node
#'           xml_add_child(bpmn_element_node,
#'                         paste("bpmn", attribute_names[[j]], sep = ":"),
#'                         attributes[[j]])
#'         }
#'       }
#'     }
#'   }
#'
#' # Finds incoming and outgoing sequence flows for every BPMN element
#' .find.incoming.outgoing.sequenceFlows <-
#'   function(bpmn, bpmn_element) {
#'     bpmn[[bpmn_element]] %>%
#'       left_join(bpmn[["sequenceFlows"]], by = c("id" = "targetRef")) %>%
#'       select(id, .data$id.y) %>%
#'       rename(incoming = .data$id.y) %>%
#'       left_join(bpmn[["sequenceFlows"]], by = c("id" = "sourceRef")) %>%
#'       select(id, .data$incoming, .data$id.y) %>%
#'       rename(outgoing = .data$id.y)
#'   }
#'
#' # Retrieves incoming and outgoing elements for every BPMN element node
#' .retrieve.incoming.outgoing.elements <-
#'   function(bpmn, plural_of_bpmn_elements) {
#'     # Finds every non-empty BPMN element
#'     retrieve_empty_data_frames <- as_mapper(~ nrow(.x) == 0)
#'     bpmn_elements_empty <- bpmn %>%
#'       map_lgl(retrieve_empty_data_frames)
#'     bpmn_elements <- names(bpmn)
#'     bpmn_elements <- bpmn_elements[!bpmn_elements_empty]
#'
#'     # Retrieves plural of every non-empty BPMN element
#'     plural_of_bpmn_elements_non_empty <-
#'       plural_of_bpmn_elements[!bpmn_elements_empty]
#'
#'     # Finds incoming and outgoing sequence flows for every non-empty BPMN element
#'     incoming_outgoing_elements <- bpmn_elements %>%
#'       map(~ .find.incoming.outgoing.sequenceFlows(bpmn, .x))
#'     names(incoming_outgoing_elements) <-
#'       names(plural_of_bpmn_elements_non_empty)
#'
#'     # Retrieves incoming and outgoing BPMN elements for every sequence flow
#'     incoming_outgoing_elements[["sequenceFlow"]][["incoming"]] <-
#'       bpmn[["sequenceFlows"]][["sourceRef"]]
#'     incoming_outgoing_elements[["sequenceFlow"]][["outgoing"]] <-
#'       bpmn[["sequenceFlows"]][["targetRef"]]
#'
#'     # Adds name of BPMN element to every row of the data.frame
#'     for (element in names(incoming_outgoing_elements)) {
#'       element_list <- list()
#'       for (i in 1:nrow(incoming_outgoing_elements[[element]])) {
#'         element_list[[length(element_list) + 1]] <- element
#'       }
#'       incoming_outgoing_elements[[element]][["element"]] <-
#'         element_list
#'     }
#'
#'     return(incoming_outgoing_elements)
#'   }
#'
#' # Creates incoming and outgoing sequence flows for every BPMN element node
#' .xml.create.incoming.outgoing.sequenceFlows <-
#'   function(bpmn,
#'            process_node,
#'            plural_of_bpmn_elements) {
#'     # Retrieves incoming and outgoing elements for every BPMN element node
#'     incoming_outgoing_elements <-
#'       .retrieve.incoming.outgoing.elements(bpmn, plural_of_bpmn_elements)
#'
#'     # Adds incoming and outgoing sequence flows for every BPMN element node
#'     bpmn_element_nodes <- xml_children(process_node)
#'     for (bpmn_element_node in bpmn_element_nodes) {
#'       element <- xml_name(bpmn_element_node)
#'       if (grepl("Gateway", element, fixed = TRUE)) {
#'         element <- "gateway"
#'       }
#'
#'       # Splits all incoming and outgoing BPMN elements into two groups
#'       incoming_outgoing_elements_per_individual_bpmn_element <-
#'         incoming_outgoing_elements[[element]][which(incoming_outgoing_elements[[element]] == xml_attr(bpmn_element_node, "id")),]
#'       incoming_elements <-
#'         unique(incoming_outgoing_elements_per_individual_bpmn_element[["incoming"]])
#'       outgoing_elements <-
#'         unique(incoming_outgoing_elements_per_individual_bpmn_element[["outgoing"]])
#'
#'       # Adds incoming and outgoing sequence flows for the BPMN element node
#'       if (element != "sequenceFlow") {
#'         for (incoming_element in incoming_elements) {
#'           if (!is.na(incoming_element)) {
#'             xml_add_child(bpmn_element_node,
#'                           "bpmn:incoming",
#'                           incoming_element)
#'           }
#'         }
#'         for (outgoing_element in outgoing_elements) {
#'           if (!is.na(outgoing_element)) {
#'             xml_add_child(bpmn_element_node,
#'                           "bpmn:outgoing",
#'                           outgoing_element)
#'           }
#'         }
#'       }
#'     }
#'   }
#'
#' # Creates BPMN element nodes as children from "process" node
#' .xml.create.bpmn.element.nodes <- function(bpmn,
#'                                            process_node,
#'                                            xml_attributes,
#'                                            type_attributes,
#'                                            singular_of_bpmn_elements,
#'                                            plural_of_bpmn_elements) {
#'   # Adds BPMN element nodes as children from "process" node
#'   for (bpmn_element in names(bpmn)) {
#'     transposed_bpmn_element <- transpose(bpmn[[bpmn_element]])
#'     for (i in seq_along(transposed_bpmn_element)) {
#'       individual_bpmn_element <- list(transposed_bpmn_element[[i]])
#'       names(individual_bpmn_element) <-
#'         paste("bpmn", singular_of_bpmn_elements[[bpmn_element]], sep = ":")
#'
#'       # Adds BPMN element node as a child from "process" node
#'       bpmn_element_node <-
#'         .xml.add.and.return.child(process_node, names(individual_bpmn_element))
#'
#'       # Sets attributes to, changes name of and adds possible children to the BPMN element node
#'       .xml.modifies.bpmn.element.node(bpmn_element_node,
#'                                       individual_bpmn_element,
#'                                       xml_attributes,
#'                                       type_attributes)
#'     }
#'   }
#'
#'   # Creates incoming and outgoing sequence flows for every BPMN element node
#'   .xml.create.incoming.outgoing.sequenceFlows(bpmn, process_node, plural_of_bpmn_elements)
#' }
#'
#' # Creates "process" node as a child from "definitions" node
#' .xml.create.process.node <-
#'   function(bpmn_xml,
#'            bpmn,
#'            xml_attributes,
#'            type_attributes,
#'            singular_of_bpmn_elements,
#'            plural_of_bpmn_elements) {
#'     # Adds "process" node as a child from "definitions" node
#'     process_node <-
#'       .xml.add.and.return.child(bpmn_xml, "bpmn:process")
#'     xml_set_attr(process_node, "id", paste0("process-", UUIDgenerate()))
#'
#'     # Creates BPMN element nodes as children from "process" node
#'     .xml.create.bpmn.element.nodes(
#'       bpmn,
#'       process_node,
#'       xml_attributes,
#'       type_attributes,
#'       singular_of_bpmn_elements,
#'       plural_of_bpmn_elements
#'     )
#'
#'     return(process_node)
#'   }
#'
#' # Creates data.frame of incoming and outgoing elements for every BPMN element node
#' .create.incoming.outgoing.elements.df <-
#'   function(bpmn, plural_of_bpmn_elements) {
#'     # Retrieves incoming and outgoing elements for every BPMN element node
#'     incoming_outgoing_elements <-
#'       .retrieve.incoming.outgoing.elements(bpmn, plural_of_bpmn_elements)
#'
#'     # Binds the data.frames of every BPMN element into one data.frame
#'     incoming_outgoing_elements_df <-
#'       bind_rows(incoming_outgoing_elements)
#'   }
#'
#' # Computes x and y coordinates for every BPMN element except sequence flows
#' .compute.bpmn.element.coordinates <-
#'   function(bpmn, bpmn_shape_dimensions) {
#'     # Gets edges from BPMN object (we assume that all nodes are connected to at least one edge)
#'     edges <- bpmn$sequenceFlows
#'
#'     # Transforms edges to long format and gives each unique id a number from 1 to n_edges, using as.numeric(factor())
#'     # (This "node_id" is needed for DiagrammeR.)
#'     edge_list_long <- edges %>%
#'       gather("key", "original_id", "sourceRef", "targetRef") %>%
#'       mutate(node_id = as.numeric(factor(.data$original_id)))
#'
#'     # Creates key table that maps "original_id" to "node_id"
#'     node_keys <- edge_list_long %>%
#'       select(.data$original_id, .data$node_id) %>%
#'       unique()
#'
#'     # Removes old id ("original_id") and recreates wide format of edges with new id ("node_id")
#'     edges <- edge_list_long %>%
#'       select(-.data$original_id) %>%
#'       spread(.data$key, .data$node_id)
#'
#'     # Uses "sourceRef" and "targetRef" (which are now simple ids from 1 till n) to build edge data.frame
#'     edge_df <-
#'       create_edge_df(from = edges$sourceRef, to = edges$targetRef)
#'
#'     # Creates node data.frame with correct number of nodes (which is the number of rows in "node_keys")
#'     node_df <- create_node_df(nrow(node_keys))
#'
#'     # Creates graph, sets appropriate layout options, renders graph and saves SVG/dot notation
#'     dot <- create_graph(node_df, edge_df) %>%
#'       add_global_graph_attrs(attr = "rankdir",
#'                              value = "LR",
#'                              attr_type = "graph") %>%
#'       add_global_graph_attrs(attr = "layout",
#'                              value = "dot",
#'                              attr_type = "graph") %>%
#'       render_graph(layout = "tree") %>%
#'       export_svg()
#'
#'     # Reads dot notation and selects all SVG <g> elements
#'     g_elements <- read_html(dot) %>%
#'       html_nodes("g")
#'
#'     # Subsets nodes from "g_elements" (which are elements that have a node attribute)
#'     nodes <-
#'       g_elements[map_lgl(g_elements, ~ ("node" %in% html_attrs(.x)))]
#'
#'     # Creates table with the numerical node id and coordinates from the node
#'     coordinates <-
#'       tibble(
#'         node_id = as.numeric(nodes %>% html_node("title") %>% html_text()),
#'         x = as.numeric(nodes %>% html_node("ellipse") %>% html_attr("cy")),
#'         y = as.numeric(nodes %>% html_node("ellipse") %>% html_attr("cx")),
#'         .name_repair = "unique"
#'       )
#'
#'     # Retrieves heights and widths of BPMN elements
#'     bpmn_shape_heights <- names(bpmn_shape_dimensions) %>%
#'       map(~ bpmn_shape_dimensions[[.x]][["height"]])
#'     bpmn_shape_widths <- names(bpmn_shape_dimensions) %>%
#'       map(~ bpmn_shape_dimensions[[.x]][["width"]])
#'
#'     # Joins "coordinates" with original ids
#'     output <- node_keys %>%
#'       inner_join(coordinates, by = "node_id") %>%
#'       # Removes "node_id" and renames "original_id" to "id"
#'       select(id = .data$original_id, .data$x, .data$y) %>%
#'       # Rescales x and y
#'       # (probably to be further optimized / corrected for height/width of elements)
#'       mutate(
#'         y = 3 * .data$y - 3 * min(.data$y) + as.numeric(max(unlist(
#'           bpmn_shape_heights
#'         ))) / 2,
#'         x = 2 * .data$x - 2 * min(.data$x) + as.numeric(max(unlist(
#'           bpmn_shape_widths
#'         ))) / 2
#'       )
#'
#'     # bpmn$sequenceFlows %>%
#'     #   select(sourceRef, targetRef) %>%
#'     #   as.matrix() %>%
#'     #   igraph::graph_from_edgelist() %>%
#'     #   get_layout() %>%
#'     #   select(id = node, x = x_offsetnode, y = y_offsetnode) %>%
#'     #   mutate(across(c(x,y), ~.x*1000)) %>%
#'     #   mutate(y = abs(y)) %>%
#'     #   filter(!str_detect(id, "block_")) %>%
#'     #   filter(!id %in% c("START","END_1")) %>%
#'     #   as.data.frame()
#'   }
#'
#' # Creates "Bounds" node as a child from "BPMNPShape" node
#' .xml.create.bounds.node <-
#'   function(child_BPMNPlane_node,
#'            bpmn_element_node,
#'            element,
#'            x_y_coordinates,
#'            bpmn_shape_dimensions) {
#'     # Adds "Bounds" node as a child from "BPMNPlane" node
#'     bounds_node <-
#'       .xml.add.and.return.child(child_BPMNPlane_node, "dc:Bounds")
#'
#'     # Adds "BPMNLabel" node as a child from "BPMNPlane" node (which is not required)
#'     BPMNLabel_node <-
#'       .xml.add.and.return.child(child_BPMNPlane_node, "bpmndi:BPMNLabel")
#'
#'     # Sets height, width, x and y attribute to the "Bounds" node
#'     xml_set_attr(bounds_node,
#'                  "height",
#'                  bpmn_shape_dimensions[[element]][["height"]])
#'     xml_set_attr(bounds_node,
#'                  "width",
#'                  bpmn_shape_dimensions[[element]][["width"]])
#'     xml_set_attr(bounds_node,
#'                  "x",
#'                  x_y_coordinates[["x"]][which(x_y_coordinates[["id"]] == xml_attr(bpmn_element_node, "id"))] - as.numeric(bpmn_shape_dimensions[[element]][["width"]]) / 2)
#'     xml_set_attr(bounds_node,
#'                  "y",
#'                  x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == xml_attr(bpmn_element_node, "id"))] - as.numeric(bpmn_shape_dimensions[[element]][["height"]]) / 2)
#'   }
#'
#' # Sets coordinates of first "waypoint" node
#' .xml.set.coordinates.first.waypoint.node <-
#'   function(first_waypoint_node,
#'            x_y_coordinates,
#'            id_incoming,
#'            id_outgoing,
#'            element_incoming,
#'            bpmn_shape_dimensions) {
#'     if (element_incoming == "gateway" &&
#'         x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_outgoing)] > x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_incoming)]) {
#'       # Attaches starting point of sequence flow to the top of the gateway
#'       # to eventually make a 90-degree angle to the right
#'       x_extra <- 0
#'       y_extra <-
#'         as.numeric(bpmn_shape_dimensions[[element_incoming]][["height"]]) / 2
#'     } else if (element_incoming == "gateway" &&
#'                x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_outgoing)] < x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_incoming)]) {
#'       # Attaches starting point of sequence flow to the bottom of the gateway
#'       # to eventually make a 90-degree angle to the right
#'       x_extra <- 0
#'       y_extra <-
#'         -as.numeric(bpmn_shape_dimensions[[element_incoming]][["height"]]) / 2
#'     } else {
#'       # Attaches starting point of sequence flow to the right side of the element
#'       x_extra <-
#'         as.numeric(bpmn_shape_dimensions[[element_incoming]][["width"]]) / 2
#'       y_extra <- 0
#'     }
#'
#'     # Sets coordinates of first "waypoint" node
#'     xml_set_attr(first_waypoint_node,
#'                  "x",
#'                  x_y_coordinates[["x"]][which(x_y_coordinates[["id"]] == id_incoming)] + x_extra)
#'     xml_set_attr(first_waypoint_node,
#'                  "y",
#'                  x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_incoming)] + y_extra)
#'   }
#'
#' # Sets coordinates of second "waypoint" node
#' .xml.set.coordinates.second.waypoint.node <-
#'   function(second_waypoint_node,
#'            child_BPMNPlane_node,
#'            x_y_coordinates,
#'            id_outgoing,
#'            id_incoming,
#'            element_incoming,
#'            element_outgoing,
#'            bpmn_shape_dimensions) {
#'     if (x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_outgoing)] == x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_incoming)]) {
#'       # Sets coordinates of second "waypoint" node by attaching end of sequence flow
#'       # to the left side of the next element
#'       xml_set_attr(
#'         second_waypoint_node,
#'         "x",
#'         x_y_coordinates[["x"]][which(x_y_coordinates[["id"]] == id_outgoing)] - as.numeric(bpmn_shape_dimensions[[element_outgoing]][["width"]]) / 2
#'       )
#'       xml_set_attr(second_waypoint_node,
#'                    "y",
#'                    x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_outgoing)])
#'
#'       # Sets logical variable to FALSE
#'       # (because a 90-degree angle was not needed to attach sequence flow horizontally)
#'       third_waypoint_node_needed <- FALSE
#'     } else {
#'       if (element_incoming == "gateway") {
#'         # Sets coordinates of second "waypoint" node to x coordinate of the element
#'         # and y coordinate of the next element
#'         xml_set_attr(second_waypoint_node,
#'                      "x",
#'                      x_y_coordinates[["x"]][which(x_y_coordinates[["id"]] == id_incoming)])
#'         xml_set_attr(second_waypoint_node,
#'                      "y",
#'                      x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_outgoing)])
#'       } else {
#'         # Sets coordinates of second "waypoint" node to x coordinate of the next element
#'         # and y coordinate of the element
#'         xml_set_attr(second_waypoint_node,
#'                      "x",
#'                      x_y_coordinates[["x"]][which(x_y_coordinates[["id"]] == id_outgoing)])
#'         xml_set_attr(second_waypoint_node,
#'                      "y",
#'                      x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_incoming)])
#'       }
#'
#'       # Sets logical variable to TRUE
#'       # (because a 90-degree angle was needed to attach sequence flow horizontally)
#'       third_waypoint_node_needed <- TRUE
#'     }
#'
#'     return(third_waypoint_node_needed)
#'   }
#'
#' # Sets coordinates of third "waypoint" node
#' .xml.set.coordinates.third.waypoint.node <-
#'   function(third_waypoint_node,
#'            child_BPMNPlane_node,
#'            x_y_coordinates,
#'            id_incoming,
#'            id_outgoing,
#'            element_incoming,
#'            element_outgoing,
#'            bpmn_shape_dimensions) {
#'     if (element_outgoing == "gateway" &&
#'         element_incoming != "gateway") {
#'       # Attaches end point of sequence flow to the bottom of the gateway
#'       if (x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_outgoing)] > x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_incoming)]) {
#'         x_extra <- 0
#'         y_extra <-
#'           -as.numeric(bpmn_shape_dimensions[[element_outgoing]][["height"]]) / 2
#'       } else {
#'         # Attaches end point of sequence flow to the top of the gateway
#'         x_extra <- 0
#'         y_extra <-
#'           as.numeric(bpmn_shape_dimensions[[element_outgoing]][["height"]]) / 2
#'       }
#'     } else {
#'       # Attaches end point of sequence flow to the left side of the next element
#'       x_extra <-
#'         -as.numeric(bpmn_shape_dimensions[[element_outgoing]][["width"]]) / 2
#'       y_extra <- 0
#'     }
#'
#'     # Sets coordinates of third "waypoint" node
#'     xml_set_attr(third_waypoint_node,
#'                  "x",
#'                  x_y_coordinates[["x"]][which(x_y_coordinates[["id"]] == id_outgoing)] + x_extra)
#'     xml_set_attr(third_waypoint_node,
#'                  "y",
#'                  x_y_coordinates[["y"]][which(x_y_coordinates[["id"]] == id_outgoing)] + y_extra)
#'   }
#'
#' # Creates "waypoint" nodes as children from "BPMNPEdge" node
#' .xml.create.waypoint.nodes <-
#'   function(incoming_outgoing_elements_df,
#'            child_BPMNPlane_node,
#'            x_y_coordinates,
#'            bpmn_shape_dimensions) {
#'     # Retrieves necessary ids and elements to create the "waypoint" nodes
#'     id_sequenceFlow <-
#'       incoming_outgoing_elements_df[["id"]][which(incoming_outgoing_elements_df[["id"]] == xml_attr(child_BPMNPlane_node, "bpmnElement"))]
#'     id_incoming <-
#'       incoming_outgoing_elements_df[["incoming"]][which(incoming_outgoing_elements_df[["id"]] == id_sequenceFlow)]
#'     id_outgoing <-
#'       incoming_outgoing_elements_df[["outgoing"]][which(incoming_outgoing_elements_df[["id"]] == id_sequenceFlow)]
#'     element_incoming <-
#'       unique(incoming_outgoing_elements_df[["element"]][which(incoming_outgoing_elements_df[["id"]] == id_incoming)])[[1]]
#'     element_outgoing <-
#'       unique(incoming_outgoing_elements_df[["element"]][which(incoming_outgoing_elements_df[["id"]] == id_outgoing)])[[1]]
#'
#'     # Adds two "waypoint" nodes as children from "BPMNEdge" node
#'     # (because there will always be one starting point and one end point of the sequence flow)
#'     first_waypoint_node <-
#'       .xml.add.and.return.child(child_BPMNPlane_node, "di:waypoint")
#'     second_waypoint_node <-
#'       .xml.add.and.return.child(child_BPMNPlane_node, "di:waypoint")
#'
#'     # Sets coordinates of first "waypoint" node
#'     .xml.set.coordinates.first.waypoint.node(
#'       first_waypoint_node,
#'       x_y_coordinates,
#'       id_incoming,
#'       id_outgoing,
#'       element_incoming,
#'       bpmn_shape_dimensions
#'     )
#'
#'     # Sets coordinates of second "waypoint" node
#'     third_waypoint_node_needed <-
#'       .xml.set.coordinates.second.waypoint.node(
#'         second_waypoint_node,
#'         child_BPMNPlane_node,
#'         x_y_coordinates,
#'         id_outgoing,
#'         id_incoming,
#'         element_incoming,
#'         element_outgoing,
#'         bpmn_shape_dimensions
#'       )
#'
#'     # Creates third "waypoint" node as a child from "BPMNEdge" node if needed
#'     if (third_waypoint_node_needed) {
#'       # Adds "waypoint" node as a child from "BPMNEdge" node
#'       third_waypoint_node <-
#'         .xml.add.and.return.child(child_BPMNPlane_node, "di:waypoint")
#'
#'       # Sets coordinates of third "waypoint" node
#'       .xml.set.coordinates.third.waypoint.node(
#'         third_waypoint_node,
#'         child_BPMNPlane_node,
#'         x_y_coordinates,
#'         id_incoming,
#'         id_outgoing,
#'         element_incoming,
#'         element_outgoing,
#'         bpmn_shape_dimensions
#'       )
#'     }
#'   }
#'
#' # Creates "BPMNShape" and "BPMNEdge" nodes as children from "BPMNPlane" node
#' .xml.create.BPMNPlane.node.children <-
#'   function(bpmn,
#'            process_node,
#'            BPMNPlane_node,
#'            bpmn_shape_dimensions,
#'            incoming_outgoing_elements_df) {
#'     # Computes x and y coordinates for every BPMN element except sequence flows
#'     x_y_coordinates <-
#'       .compute.bpmn.element.coordinates(bpmn, bpmn_shape_dimensions)
#'
#'     # Adds "BPMNShape" or "BPMNEdge" nodes as children from "BPMNPlane" node
#'     bpmn_element_nodes <- xml_children(process_node)
#'     for (bpmn_element_node in bpmn_element_nodes) {
#'       element <- xml_name(bpmn_element_node)
#'       if (element == "sequenceFlow") {
#'         bpmndi_element <- "BPMNEdge"
#'       } else {
#'         bpmndi_element <- "BPMNShape"
#'       }
#'
#'       # Adds "BPMNShape" or "BPMNEdge" node as a child from "BPMNPlane" node
#'       child_BPMNPlane_node <-
#'         .xml.add.and.return.child(BPMNPlane_node,
#'                                   paste("bpmndi", bpmndi_element, sep = ":"))
#'       xml_set_attr(child_BPMNPlane_node,
#'                    "bpmnElement",
#'                    xml_attr(bpmn_element_node, "id"))
#'
#'       if (element != "sequenceFlow") {
#'         # Sets "isMarkerVisible" attribute to the "BPMNShape" node
#'         if (grepl("Gateway", element, fixed = TRUE)) {
#'           xml_set_attr(child_BPMNPlane_node,
#'                        "isMarkerVisible",
#'                        "true")
#'           # Changes "element" to "gateway" if "Gateway" is in "element"
#'           element <- "gateway"
#'         }
#'
#'         # Creates "Bounds" node as a child from "BPMNPShape" node
#'         .xml.create.bounds.node(
#'           child_BPMNPlane_node,
#'           bpmn_element_node,
#'           element,
#'           x_y_coordinates,
#'           bpmn_shape_dimensions
#'         )
#'       } else if (element == "sequenceFlow") {
#'         # Creates "waypoint" nodes as children from "BPMNPEdge" node
#'         .xml.create.waypoint.nodes(
#'           incoming_outgoing_elements_df,
#'           child_BPMNPlane_node,
#'           x_y_coordinates,
#'           bpmn_shape_dimensions
#'         )
#'       }
#'     }
#'   }
#'
#' # Creates "BPMNPlane" node as a child from "BPMNDiagram" node
#' .xml.create.BPMNPlane.node <-
#'   function(bpmn,
#'            process_node,
#'            BPMNDiagram_node,
#'            plural_of_bpmn_elements,
#'            bpmn_shape_dimensions) {
#'     # Adds "BPMNPlane" node as a child from "BPMNDiagram" node
#'     # (which is the "BPMNDiagram" container of "BPMNShape" and "BPMNEdge")
#'     BPMNPlane_node <-
#'       .xml.add.and.return.child(BPMNDiagram_node, "bpmndi:BPMNPlane")
#'     xml_set_attr(BPMNPlane_node,
#'                  "bpmnElement",
#'                  xml_attr(process_node, "id"))
#'     xml_set_attr(BPMNPlane_node, "id", paste0("BPMNPlane-", UUIDgenerate()))
#'
#'     # Creates data.frame of incoming and outgoing elements for every BPMN element node
#'     incoming_outgoing_elements_df <-
#'       .create.incoming.outgoing.elements.df(bpmn, plural_of_bpmn_elements)
#'
#'     # Creates "BPMNShape" and "BPMNEdge" nodes as children from "BPMNPlane" node
#'     .xml.create.BPMNPlane.node.children(
#'       bpmn,
#'       process_node,
#'       BPMNPlane_node,
#'       bpmn_shape_dimensions,
#'       incoming_outgoing_elements_df
#'     )
#'
#'     return(BPMNPlane_node)
#'   }
#'
#' # Creates "BPMNDiagram" node as a child from "definitions" node
#' .xml.create.BPMNDiagram.node <-
#'   function(bpmn_xml,
#'            bpmn,
#'            process_node,
#'            plural_of_bpmn_elements,
#'            bpmn_shape_dimensions) {
#'     # Adds "BPMNDiagram" node as a child from "definitions" node
#'     BPMNDiagram_node <-
#'       .xml.add.and.return.child(bpmn_xml, "bpmndi:BPMNDiagram")
#'     xml_set_attr(BPMNDiagram_node,
#'                  "id",
#'                  paste0("BPMNDiagram-", UUIDgenerate()))
#'
#'     # Creates "BPMNPlane" node as a child from "BPMNDiagram" node
#'     BPMNPlane_node <- .xml.create.BPMNPlane.node(
#'       bpmn,
#'       process_node,
#'       BPMNDiagram_node,
#'       plural_of_bpmn_elements,
#'       bpmn_shape_dimensions
#'     )
#'
#'     return(BPMNDiagram_node)
#'   }
