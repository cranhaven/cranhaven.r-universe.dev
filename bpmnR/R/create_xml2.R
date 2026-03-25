# ============================== MAIN FUNCTION =================================

#' Create XML document from BPMN object.
#'
#' This creates an XML document based on a BPMN object.
#'
#' @param bpmn A BPMN object as a list of data.frames for the BPMN elements.
#' @param ... Additional arguments passed to methods.
#'
#' @return An XML document for the XML-based interchange format for the BPMN process.
#'
#' @author Alessio Nigro
#'
#' @import DiagrammeR
#' @import DiagrammeRsvg
#' @import dplyr
#' @import rvest
#' @import tidyr
#' @import uuid
#' @importFrom stringr str_detect
#' @importFrom rlang .data
#' @importFrom purrr pmap map_chr pmap_chr
#' @examples
#'
#' library(dplyr)
#' nodes <- tibble(id = "task", name = "Task name", objectType = "task",
#' gatewayDirection = NA)
#' events <- tibble(id = c("start","end"), name = c("Start event","End event"),
#' objectType = c("startEvent","endEvent"))
#' flows <- tibble(id = c("flow1","flow2"), name = c("flow1","flow2"),
#' sourceRef = c("start","task"), targetRef = c("task","end"),
#' objectType = c("sequenceFlow","sequenceFlow"))
#' model <- create_bpmn(nodes, flows, events)
#' create_xml(model)
#'
#' @export
create_xml <- function(bpmn, ...) {
  UseMethod("create_xml")
}

#' @describeIn create_xml Create xml
#' @export
create_xml.bpmn <- function(bpmn, ...) {

  name <- NULL
  gatewayDirection <- NULL
  objectType <- NULL
  sourceRef <- NULL
  targetRef <- NULL
  old_id <- NULL
  new_id <- NULL
  x <- NULL
  y <- NULL
  id_incoming <- NULL
  id_outgoing <- NULL
  objectSubType <- NULL
  . <- NULL
  waypoints <- NULL
  data <- NULL




  file <- c('<bpmn:definitions',

            'xmlns:bpmn="http://www.omg.org/spec/BPMN/20100524/MODEL"',
            'xmlns:bpmndi="http://www.omg.org/spec/BPMN/20100524/DI"',
            'xmlns:dc="http://www.omg.org/spec/DD/20100524/DC"',
            'xmlns:di="http://www.omg.org/spec/DD/20100524/DI"',
            'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
            glue::glue('id= "{paste0(\'definitions-\', uuid::UUIDgenerate())}"'),
            'targetNamespace="http://bupar.net"',
            'exporter="bupaR-suite bpmnR, https://github.com/bupaverse/bpmnR"',
            'exporterVersion="0.0.1"',
            ">"
  )

  nodes <- bpmn$nodes %>% select(name, id, gatewayDirection, objectType)
  sequenceFlows <- bpmn$flows %>% select(name, id, sourceRef, targetRef, objectType)
  events <- bpmn$events %>% select(name, id, objectType)

  tibble(old_id = c(nodes$id, sequenceFlows$id, events$id)) %>%
    mutate(new_id = paste0("id_", uuid::UUIDgenerate(n = n()))) -> id_mapping

  get_new_id <- function(this_old_id) {

    id_mapping %>%
      filter(old_id == this_old_id) %>%
      pull(new_id)
  }

  nodes %>%
    mutate(id = map_chr(id, get_new_id)) -> nodes
  events %>%
    mutate(id = map_chr(id, get_new_id)) -> events

  sequenceFlows %>%
    mutate(id = map_chr(id, get_new_id)) %>%
    mutate(sourceRef = map_chr(sourceRef, get_new_id)) %>%
    mutate(targetRef = map_chr(targetRef, get_new_id)) -> sequenceFlows



  events  %>%
    left_join(sequenceFlows %>% select(id, sourceRef), by = c("id" = "sourceRef"), suffix = c("","_outgoing")) %>%
    left_join(sequenceFlows %>% select(id, targetRef), by = c("id" = "targetRef"), suffix = c("","_incoming")) -> events_enriched

  nodes  %>%
    left_join(sequenceFlows %>% select(id, sourceRef), by = c("id" = "sourceRef"), suffix = c("","_outgoing"), multiple = "all") %>%
    left_join(sequenceFlows %>% select(id, targetRef), by = c("id" = "targetRef"), suffix = c("","_incoming"), multiple = "all") %>%
    group_by(id, name, objectType, gatewayDirection) %>%
    nest() -> nodes_enriched


  nodes_enriched %>%
    select(id, name, objectType, gatewayDirection, data) %>%
    mutate(incoming = map(data, ~pmap_chr(distinct(.x, id_incoming), ~glue::glue('<bpmn:incoming>{..1}</bpmn:incoming>')))) %>%
    mutate(outgoing = map(data, ~pmap_chr(distinct(.x, id_outgoing), ~glue::glue('<bpmn:outgoing>{..1}</bpmn:outgoing>')))) %>%
    mutate(attr_gatewayDirection = ifelse(is.na(gatewayDirection), "", glue::glue('gatewayDirection="{gatewayDirection}"'))) %>%
    pmap(~glue::glue('<bpmn:{..3} id="{..1}" name="{..2}" {..8}>{paste(..6, collapse="")}{paste(..7, collapse = "")}</bpmn:{..3}>')) -> pr_nodes

  events_enriched %>%
    mutate(incoming = ifelse(is.na(id_incoming), "", glue::glue('<bpmn:incoming>{id_incoming}</bpmn:incoming>'))) %>%
    mutate(outgoing = ifelse(is.na(id_outgoing), "", glue::glue('<bpmn:outgoing>{id_outgoing}</bpmn:outgoing>'))) %>%
    pmap(~glue::glue('<bpmn:{..3} name="{..1}" id="{..2}">{..6}{..7}</bpmn:{..3}>')) -> pr_events

  pmap(sequenceFlows, ~glue::glue('<bpmn:sequenceFlow id="{..2}" name="{..1}" sourceRef="{..3}" targetRef="{..4}"/>')) -> pr_seqs

  process <- "<bpmn:process id='process123'>"


  diagram_prefix <- '<bpmndi:BPMNDiagram id="BPMNDiagram-b1dd2938-adfe-4ed3-aaa4-15faaef590da">
    <bpmndi:BPMNPlane bpmnElement="process123" id="BPMNPlane-a669060d-1424-4927-a4fb-2a036121dbf4">'

  diagram_suffix <- '</bpmndi:BPMNPlane>
  </bpmndi:BPMNDiagram>'

  # sequenceFlows %>%
  #   select(sourceRef, targetRef) %>%
  #   as.matrix() %>%
  #   igraph::graph_from_edgelist() %>%
  #   get_layout() %>%
  #   select(id = node, x = x_offsetnode, y = y_offsetnode) %>%
  #   #mutate(y = ifelse(y == 0, y + 1, y)) %>%
  #   mutate(y = y - min(y)) %>%
  #   mutate(x = x - min(x)) %>%
  #   mutate(across(c(x,y), ~.x*150)) %>%
  #   filter(!str_detect(id, "block_")) %>%
  #   #filter(!id %in% c("START","END_1")) %>%
  #   as.data.frame() -> layout
  sequenceFlows %>%
    get_layout2() %>%
    mutate(across(c(x,y), ~.x*2)) %>%
    mutate(y = -2*y) -> layout

  bind_rows(nodes, events) %>%
    mutate(objectSubType = ifelse(objectType %in% c("exclusiveGateway","inclusiveGateway","parallelGateway"), as.character(gatewayDirection), as.character(objectType))) %>%
    left_join(layout, by = 'id') %>%
    mutate(y = ifelse(objectType %in% c("exclusiveGateway","inclusiveGateway","parallelGateway"),
                      y + 15,
                      ifelse(objectType %in% c("startEvent", "endEvent","normalEndEvent"), y + 22, y))) -> nodes



  sequenceFlows %>%
    left_join(nodes, by = c("sourceRef" = "id"), suffix = c("","_source")) %>%
    rename(objectSubType_source = objectSubType, x_source = x , y_source = y)  %>%
    left_join(nodes, by = c("targetRef" = "id"), suffix = c("","_target")) %>%
    rename(objectSubType_target = objectSubType, x_target = x , y_target = y) %>%
    mutate(x_source = case_when(objectSubType_source == "startEvent" ~ x_source +36,
                                objectSubType_source == "task" ~ x_source + 100,
                                objectSubType_source %in% c("Diverging","Converging") ~ x_source + 50,
                                T ~ x_source)) %>%
    mutate(y_source = case_when(objectSubType_source == "startEvent" ~ y_source +36/2,
                                objectSubType_source == "task" ~ y_source + 80/2,
                                objectSubType_source %in% c("Diverging","Converging") ~ y_source + 50/2,
                                T ~ y_source)) %>%
    # mutate(y_source = ifelse(objectSubType_source == "Diverging", ((ifelse(y_target > y_source, 1, ifelse(y_target == y_source, 0.5, 0))) - 0.5)*50 + y_source, y_source)) %>%
    # mutate(y_target = ifelse(objectSubType_target == "Converging", ((ifelse(y_target < y_source, 1, ifelse(y_target == y_source, 0.5, 0))) - 0.5)*50 + y_target, y_target)) %>%
    # mutate(x_source = ifelse(objectSubType_source == "Diverging", (y_target != y_source)*-50/2 + x_source, x_source)) %>%
    # mutate(x_target = ifelse(objectSubType_target == "Converging", (y_target != y_source)*50/2 + x_target, x_target)) %>%
    # mutate(y_source = case_when(objectSubType_source == "Diverging" & y_target != y_source ~ y_source - 50/2,
    #                              T ~ y_source)) %>%
    mutate(y_target = case_when(objectSubType_target %in% c("endEvent", "startEvent","normalEndEvent") ~ y_target +36/2,
                                objectSubType_target == "task" ~ y_target + 80/2,
                                objectSubType_target %in% c("Diverging","Converging") ~ y_target + 50/2,
                                T ~ y_target)) %>%
    mutate(x_mid = case_when(objectSubType_source %in% c("Diverging") ~ x_source,
                                T ~ x_target)) %>%
    mutate(y_mid = case_when(objectSubType_source %in% c("Converging") ~ y_source,
                             objectSubType_source %in% c("Diverging") ~ y_target,
                             objectSubType_target %in% c("Converging") ~ y_source,
                             objectSubType_target %in% c("Diverging") ~ y_target,
                             T ~ y_target)) %>%
    mutate(., waypoints = pmap(., ~tibble(x = c(..10, ..18, ..16), y = c(..11, ..19, ..17)))) %>%
    mutate(waypoints = map(waypoints, ~mutate(.x, tag = glue::glue('<di:waypoint x="{x}" y="{y}" />')))) -> sequence_wp



  di_tasks <- pmap(nodes %>% filter(objectType == "task"), ~glue::glue('<bpmndi:BPMNShape bpmnElement="{..2}"><dc:Bounds height="80" width="100" x="{..6}" y="{..7}"/><bpmndi:BPMNLabel/></bpmndi:BPMNShape>'))
  di_gateways <- pmap(nodes %>% filter(objectType %in% c("exclusiveGateway","inclusiveGateway","parallelGateway")),
                      ~glue::glue('<bpmndi:BPMNShape bpmnElement="{..2}" isMarkerVisible="true"><dc:Bounds height="50" width="50" x="{..6}" y="{..7}"/><bpmndi:BPMNLabel/></bpmndi:BPMNShape>'))
  di_edges <- pmap(sequence_wp, ~glue::glue('<bpmndi:BPMNEdge bpmnElement="{..2}">{paste(..20$tag, collapse = "")}</bpmndi:BPMNEdge>'))
  #
  di_events <- pmap(nodes %>% filter(str_detect(objectType, "Event")), ~glue::glue('<bpmndi:BPMNShape bpmnElement="{..2}"><dc:Bounds height="36" width="36" x="{..6}" y="{..7}"/><bpmndi:BPMNLabel/></bpmndi:BPMNShape>'))


  paste(c('<?xml version="1.0" encoding="UTF-8"?>', paste(file, collapse = " "),
                process, pr_nodes, pr_events, pr_seqs, "</bpmn:process>",
                diagram_prefix, di_tasks,di_gateways, di_edges, di_events, diagram_suffix,
                "</bpmn:definitions>"), collapse = "\n")
}
