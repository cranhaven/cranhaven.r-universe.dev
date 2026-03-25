#' Control Flow Complexity (CFC) -------------------------------------------
#'
#' @param bpmn bpmn-object.
#'
#' @return Control-Flow-Complexity. Numeric vector of length 1
#' @export
#'
#' @examples
#' library(dplyr)
#' nodes <- tibble(id = "task", name = "Task name", objectType = "task",
#' gatewayDirection = NA)
#' events <- tibble(id = c("start","end"), name = c("Start event","End event"),
#' objectType = c("startEvent","endEvent"))
#' flows <- tibble(id = c("flow1","flow2"), name = c("flow1","flow2"),
#' sourceRef = c("start","task"), targetRef = c("task","end"),
#' objectType = c("sequenceFlow","sequenceFlow"))
#' model <- create_bpmn(nodes, flows, events)
#' calculate_CFC(model)
#'
#' @importFrom purrr list_along
#'
calculate_CFC <- function(bpmn) {

  gatewayDirection <- NULL
  objectType <- NULL
  sourceRef <- NULL
  targetRef <- NULL
  cfc_OR <- NULL
  gatewayType <- NULL



  seqs <- bpmn$flows
  gw <- bpmn$nodes %>% filter(objectType %in% c("exclusiveGateway","parallelGateway","inclusiveGateway"))

  if(nrow(gw) > 0) {

  gw_types <- gw$objectType %>% unique()
  gw_ids <- gw %>% filter(gatewayDirection == "Diverging") %>% distinct(objectType, id)

  seqs_ids <- seqs %>% select(sourceRef)
  gateways_as_source <- left_join(gw_ids, seqs_ids, by = c("id" = "sourceRef"))

  #cat("left_join of gateway id's and sequenceFlows sourceRef (unique)", sep = "\n")

  output <- list_along(gw_types)

  for(i in 1:length(gw_types))

  if (gw_types[i] == "exclusiveGateway") {
    n_splits <- gateways_as_source %>%
      filter(objectType == gw_types[i]) %>%
      group_by(id) %>% count()
    output[i] <- n_splits %>% pull(n) %>% sum()
  }

  else if (gw_types[i] == "inclusiveGateway") {
    n_splits <- gateways_as_source %>%
      filter(objectType == gw_types[i]) %>%
      group_by(id) %>%
      count()
    output[i] <- n_splits %>% mutate(cfc_OR = 2**n - 1) %>% pull(cfc_OR) %>% sum()
    # print(paste0("2^n - 1: ", "\n"))
    # print(n_splits %>% mutate(cfc_OR = 2**n - 1))
  }

  else if (gw_types[i] == "parallelGateway") {
    output[i] <- gateways_as_source %>%
      filter(objectType == gw_types[i]) %>%
      n_distinct()
  }

  return(sum(unlist(output)))
  } else {
    0
  }
}

# gw <- tmp$gateways
# seqs <- tmp$seqs
#
# gw <- gw %>% mutate(gatewayType = stringr::str_replace(gatewayType, pattern = "E", replacement = "e"))
#
# gw %>% rbind(gw %>% mutate(gatewayType = "inclusiveGateway")) %>%
#   rbind(gw %>% mutate(gatewayType = "parallelGateway")) -> art_gateways
#
# # split by gatewayType
# gateways_list <- art_gateways %>% split(art_gateways$gatewayType)
#
# map(gateways_list, ~CFC(.x, seqs))
#
# #





# gateways_list$exclusiveGateway %>% filter(gatewayDirection == "diverging") %>% nrow()
# tmp %>% glimpse()
# tmp
#
# ## def. 8: XOR-split
# gw <- tmp$gateways
# gw <- gw %>% mutate(gatewayType = stringr::str_replace(gatewayType, pattern = "E", replacement = "e"))
# gw %>% filter(gatewayDirection == "diverging") %>% nrow()
#
# gw %>% filter(gatewayDirection == "diverging") %>% n_distinct()
# ## def. 9: OR-split (=inclusiveGateway?)
# gw %>% filter(gatewayDirection == "diverging",
#               gatewayType %in% c("exclusiveGateway", "inclusiveGateway")) %>% group_by(gatewayType) %>%
#   mutate(n_splits = ifelse(gateway))






