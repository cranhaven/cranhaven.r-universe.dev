#' @title A calculation function for all metrics
#'
#'
#' @description {Creation object containing all metrics, which are :
#' the number of empty sequence flows, the number of duplicate tasks, the number of data objects, the number of pools, the number of swimlanes,
#' the number of message flows, the density, the coefficient of network connectivity, the average connector degree,
#' the maximum connector degree, the sequentiality, the cyclicity, the diameter, the depth, the token_split,
#' the control flow complexity, the connector mismatch, the connector heterogeneity and the crs}
#'
#' @param file_path file path of the BPMN file and
#' @param cross_connectivity_metric a param indicating whether cross_connectivity shall be calculated as well
#' @param signavio boolean which indicates whether the file stems from signavio
#' @param generate_new_path_log used when it is not possible to save the path log such as with the Rapid miner or in unit tests and examples
#' @return a tibble with one row and for each metric a column
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")
#' repetition_and_path_log <- create_path_and_repetition_log(file_path)}
#' calculate_metrics(file_path, generate_new_path_log = TRUE)
#' @export
calculate_metrics <-
  function(file_path,
           cross_connectivity_metric = TRUE,
           signavio = FALSE,
           generate_new_path_log = FALSE) {
    if(!generate_new_path_log ) {
      repetition_and_path_log <-
        create_path_and_repetition_log(file_path, signavio = signavio)
      usethis::use_data(repetition_and_path_log,  internal = TRUE, overwrite = TRUE)
      all_metrics <- tibble(
        file = file_path,
        size = size_process_model(file_path, signavio),
        empty_sequence_flows = n_empty_sequence_flows(file_path, signavio),
        duplicate_tasks = n_duplicate_tasks(file_path, signavio),
        number_pools = n_pools(file_path, signavio),
        number_data_objects = n_data_objects(file_path, signavio),
        number_swimlanes = n_swimlanes(file_path, signavio),
        number_message_flows = n_message_flows(file_path, signavio),
        density = density_process_model(file_path, signavio),
        coef_network_connectivity = coefficient_network_connectivity(file_path, signavio),
        avg_connector_degree = avg_connector_degree(file_path, signavio),
        max_connector_degree = max_connector_degree(file_path, signavio),
        connectivity_level_between_pools = connectivity_level_between_pools(file_path, signavio),
        sequentiality = sequentiality(file_path, signavio),
        cyclicity = cyclicity(file_path, signavio, path_log_already_created = TRUE),
        diameter = diameter(file_path, path_log_already_created = TRUE),
        depth = depth(file_path, path_log_already_created = TRUE),
        token_split = token_split(file_path, signavio),
        cfc = control_flow_complexity(file_path, signavio),
        connector_mismatch = connector_mismatch(file_path, signavio),
        connector_heterogeneity = connector_heterogeneity(file_path, signavio),
        separability = separability(file_path, signavio, path_log_already_created = TRUE),
        structuredness = structuredness(file_path, signavio, path_log_already_created = TRUE), 
        cyclomatic_metric = cyclomatic_metric(file_path, signavio, path_log_already_created = TRUE)
      )
      if (cross_connectivity_metric) {
        cross_connectivity_result <-
          cross_connectivity(file_path, signavio, path_log_already_created = TRUE)
        all_metrics <- cbind(all_metrics, cross_connectivity_result)
      }
    } else {
      all_metrics <- tibble(
        file = file_path,
        size = size_process_model(file_path, signavio),
        empty_sequence_flows = n_empty_sequence_flows(file_path, signavio),
        duplicate_tasks = n_duplicate_tasks(file_path, signavio),
        number_pools = n_pools(file_path, signavio),
        number_data_objects = n_data_objects(file_path, signavio),
        number_swimlanes = n_swimlanes(file_path, signavio),
        number_message_flows = n_message_flows(file_path, signavio),
        density = density_process_model(file_path, signavio),
        coef_network_connectivity = coefficient_network_connectivity(file_path, signavio),
        avg_connector_degree = avg_connector_degree(file_path, signavio),
        max_connector_degree = max_connector_degree(file_path, signavio),
        connectivity_level_between_pools = connectivity_level_between_pools(file_path, signavio),
        sequentiality = sequentiality(file_path, signavio),
        cyclicity = cyclicity(file_path, signavio, generate_new_path_log = TRUE),
        diameter = diameter(file_path, generate_new_path_log = TRUE),
        depth = depth(file_path, generate_new_path_log = TRUE),
        token_split = token_split(file_path, signavio),
        cfc = control_flow_complexity(file_path, signavio),
        connector_mismatch = connector_mismatch(file_path, signavio),
        connector_heterogeneity = connector_heterogeneity(file_path, signavio),
        separability = separability(file_path, signavio, generate_new_path_log = TRUE),
        structuredness = structuredness(file_path, signavio, generate_new_path_log = TRUE), 
        cyclomatic_metric = cyclomatic_metric(file_path, signavio,  generate_new_path_log = TRUE)
      )
      if (cross_connectivity_metric) {
        cross_connectivity_result <-
          cross_connectivity(file_path, signavio, generate_new_path_log = TRUE)
        all_metrics <- cbind(all_metrics, cross_connectivity_result)
      }
    }
    
    return(all_metrics)
  }

### Metrics

#' @title Size
#'
#' @description The size of a model is the number of tasks, gateways and events
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @return an integer indicating the size
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' size_process_model(file_path)
#' @export
size_process_model <-
  function (file_path, signavio = FALSE) {
    xml_internal_doc <- create_internal_doc(file_path, signavio)
    size <- number_tasks(xml_internal_doc, signavio) +
      number_AND_gateways(xml_internal_doc, signavio) +
      number_XOR_gateways(xml_internal_doc, signavio) +
      number_OR_gateways(xml_internal_doc, signavio) +
      number_complex_gateways(xml_internal_doc, signavio) +
      number_event_based_gateways(xml_internal_doc, signavio) +
      number_events(xml_internal_doc, signavio) +
      n_data_objects(file_path, signavio)
    return(size)
  }

#' @title Data Objects
#'
#' @description The number of data objects includes all data objects and data stores of a BPMN diagram
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @return an integer indicating the number of data objects
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' n_data_objects(file_path)
#' @export
n_data_objects <- function(file_path, signavio = FALSE) {
  xml_internal_doc <- create_internal_doc(file_path, signavio)
  if (!signavio) {
    data_objects <-
      getNodeSet(
        xml_internal_doc,
        "//bpmn:dataStoreReference | //bpmn:dataObjectReference | //dataStore"
      )
  } else {
    data_objects <-
      getNodeSet(
        xml_internal_doc,
        "//xmlns:dataStore | //xmlns:dataObject",
        namespace(xml_internal_doc)
      )
  }
  return(length(data_objects))
}

#' @title The connectivity level between pools
#'
#' @description The connectivity level between pools is the number of message flows over the number of pools
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @return an integer indicating the connectivity level between pools
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' connectivity_level_between_pools(file_path)
#' @export
connectivity_level_between_pools <-
  function(file_path, signavio = FALSE) {
    xml_internal_doc <- create_internal_doc(file_path, signavio)
    if (n_pools(file_path, signavio) == 0)
      return(n_message_flows(file_path, signavio))
    else
      return(
        n_message_flows(file_path, signavio) / n_pools(file_path, signavio)
      )
  }

#' @title Empty sequence flows
#'
#' @description Empty sequence flow is defined as a flow which connects a split parallel gateway with a join parallel gateway without any tasks in between
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @return an integer indicating the number of empty sequence flows
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' n_empty_sequence_flows(file_path)
#' @export
n_empty_sequence_flows <-
  function(file_path, signavio = FALSE) {
    xml_internal_doc <- create_internal_doc(file_path, signavio)
    #Take all sequence flow nodes and save the target and source in one object
    if (!signavio)
      sequence_flow_nodes <-
        getNodeSet(xml_internal_doc, "//bpmn:sequenceFlow | //sequenceFlow")
    else
      sequence_flow_nodes <-
        getNodeSet(xml_internal_doc,
                   "//xmlns:sequenceFlow",
                   namespace(xml_internal_doc))
    source_seq_flow <-
      unlist(xmlApply(sequence_flow_nodes, xmlGetAttr, name = "sourceRef"))
    target_seq_flow <-
      unlist(xmlApply(sequence_flow_nodes, xmlGetAttr, name = "targetRef"))
    
    #Get id of all split and join gateways which are of type parallel
    if (!signavio) {
      splits <-
        split_gateways(xml_internal_doc,
                       "//bpmn:parallelGateway | //parallelGateway",
                       signavio)
      split_activities <-
        split_gateways(
          xml_internal_doc,
          "//bpmn:task | //bpmn:sendTask | //bpmn:receiveTask |
          //bpmn:manualTask | //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
          //bpmn:subProcess | //bpmn:callActivity | //task",
          signavio
        )
      
      joins <-
        join_gateways(xml_internal_doc,
                      "//bpmn:parallelGateway | //parallelGateway",
                      signavio)
    } else {
      splits <-
        split_gateways(xml_internal_doc, "//xmlns:parallelGateway", signavio)
      split_activities <-
        split_gateways(
          xml_internal_doc,
          "//xmlns:task | //xmlns:sendTask |
          //xmlns:receiveTask | //xmlns:manualTask | //xmlns:businessRuleTask | //xmlns:userTask |
          //xmlns:scriptTask | //xmlns:subProcess | //xmlns:callActivity",
          signavio
        )
      
      joins <-
        join_gateways(xml_internal_doc, "//xmlns:parallelGateway", signavio)
    }
    splits <- c(split_activities, splits)
    #Check for each source and target node whether it is respectively a split and join node
    source_is_split_gateway <- source_seq_flow %in% splits
    target_is_join_gateway <- target_seq_flow %in% joins
    
    #Make a vector containing true when it is an empty sequence flow, ie the source is a split gateway
    #and the target is a join gateway
    empty_sequence_flows <-
      source_is_split_gateway & target_is_join_gateway
    
    #The sum of this vector is the number of sequence flows
    return (sum(empty_sequence_flows))
  }

#' @title Duplicate tasks
#'
#' @description Duplicate tasks are tasks which share the same name with other tasks
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @return an integer indicating the number of duplicate tasks
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' n_duplicate_tasks(file_path)
#' @export
n_duplicate_tasks <- function(file_path, signavio = FALSE) {
  xml_internal_doc <- create_internal_doc(file_path, signavio)
  number_occurences <- NULL
  tasks <-
    task_names(xml_internal_doc = xml_internal_doc, signavio = signavio)
  #calculate how many times a name of a task occurs in a process model
  if (length(tasks) > 0) {
    number_duplicates <- tasks %>%
      group_by(task_names) %>%
      summarise(number_occurences = (n() - 1)) %>%
      summarise(number_duplicates = sum(number_occurences))
    return(as.numeric(number_duplicates))
  } else
    return(0)
}

#' @title Number of pools
#'
#' @description Number of pools in the process models. A pool represents an organisation or an entity
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @return an integer indicating the number of pools
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' n_pools(file_path)
#' @export
n_pools <- function(file_path, signavio = FALSE) {
  xml_internal_doc <- create_internal_doc(file_path, signavio)
  if (!signavio)
    pools <-
      getNodeSet(xml_internal_doc, "//bpmn:participant | //process")
  else
    pools <-
      getNodeSet(xml_internal_doc,
                 "//xmlns:participant",
                 namespace(xml_internal_doc))
  return(length(pools))
}

#' @title Number of swimlanes
#'
#' @description Number of swimlanes in the pools. A swimlane represents a person, role or team
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @return an integer indicating the number of swimlanes
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' n_swimlanes(file_path)
#' @export
n_swimlanes <- function(file_path, signavio = FALSE) {
  xml_internal_doc <- create_internal_doc(file_path, signavio)
  if (!signavio)
    swimlanes <-
      getNodeSet(xml_internal_doc, "//bpmn:lane | //lane")
  else
    swimlanes <-
      getNodeSet(xml_internal_doc,
                 "//xmlns:lane",
                 namespace(xml_internal_doc))
  return(length(swimlanes))
}

#' @title Number of message flows
#'
#' @description Number of message flows. Message flows are used for communication between processes and link message events
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @return an integer indicating the number of message flows
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' n_message_flows(file_path)
#' @export
n_message_flows <- function(file_path, signavio = FALSE) {
  xml_internal_doc <- create_internal_doc(file_path, signavio)
  if (!signavio)
    message_flows <-
      getNodeSet(xml_internal_doc, "//bpmn:messageFlow | //messageFlow")
  else
    message_flows <-
      getNodeSet(xml_internal_doc,
                 "//xmlns:messageFlow",
                 namespace(xml_internal_doc))
  return(length(message_flows))
}

#' @title Density
#'
#' @description Density is defined as the number of sequence flows divided by the size times the size minus one
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @return an integer indicating the density
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' density_process_model(file_path)
#' @export
density_process_model <-
  function (file_path, signavio = FALSE) {
    xml_internal_doc <- create_internal_doc(file_path, signavio)
    density_diagram <-
      number_sequence_flows(xml_internal_doc, signavio) / ((
        size_process_model(file_path, signavio) - n_data_objects(file_path, signavio)
      ) * ((
        size_process_model(file_path, signavio) - n_data_objects(file_path, signavio)
      ) - 1))
    if (is.nan(density_diagram) | is.infinite(density_diagram))
      density_diagram <- 0
    return(density_diagram)
  }

#' @title Coefficient of network connectivity
#'
#' @description Coefficient of network connectivity is defined as the number of sequence flows divided by the size
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @return an integer indicating the coefficient of network connectivity
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' coefficient_network_connectivity(file_path)
#' @export
coefficient_network_connectivity <-
  function (file_path, signavio = FALSE) {
    xml_internal_doc <- create_internal_doc(file_path, signavio)
    coef_network_connectivity <-
      number_sequence_flows(xml_internal_doc, signavio) / (
        size_process_model(file_path, signavio) - n_data_objects(file_path, signavio)
      )
    if (is.nan(coef_network_connectivity) |
        is.infinite(coef_network_connectivity))
      coef_network_connectivity <- 0
    return(coef_network_connectivity)
  }

#' @title Cyclomatic metric of McCabe
#'
#' @description Cyclomatic metric takes into account the behavioral complexity of a process model. It is calculated by taking the number of activities minus
#' the number of events, gateways and connector activities plus the number of strongly connected components.
#' The number of strongly connected components is calculated by taking the number of exclusive gateways at depth level zero, when the depth is calculated only including exclusive gateways
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @param path_log_already_created boolean which indicates whether the path log has already been created before or not. When you are not sure, it is best to use the standard which is false
#' @param generate_new_path_log used when it is not possible to save the path log such as with the Rapid miner or in unit tests and examples
#' @param time_to_generate_path_log time which is the maximum time to generate a new path log in seconds. The standard setting is 1500 seconds.
#' @return an integer indicating the coefficient of network connectivity
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' cyclomatic_metric(file_path, generate_new_path_log = TRUE)
#' @export
cyclomatic_metric <-
  function (file_path,
            signavio = FALSE,
            path_log_already_created = FALSE,
            generate_new_path_log = FALSE,
            time_to_generate_path_log = 1500) {
    xml_internal_doc <- create_internal_doc(file_path, signavio)
    if(!path_log_already_created) {
      repetition_and_path_log <- tryCatch(expr = withTimeout(create_path_and_repetition_log(file_path, signavio = signavio), timeout = time_to_generate_path_log), 
                 TimeoutException = function(ex) "TimedOut")
      if(!generate_new_path_log ) {
        usethis::use_data(repetition_and_path_log,  internal = TRUE, overwrite = TRUE)
      }
    } 
    if(length(repetition_and_path_log) > 1) {
      if (!signavio) {
        join_activities <-
          join_gateways(
            xml_internal_doc,
            "//bpmn:task | //bpmn:sendTask | //bpmn:receiveTask |
            //bpmn:manualTask | //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
            //bpmn:subProcess | //bpmn:callActivity | //task",
            signavio
          )
        
      } else {
        join_activities <-
          join_gateways(
            xml_internal_doc,
            "//xmlns:task | //xmlns:sendTask |
            //xmlns:receiveTask | //xmlns:manualTask | //xmlns:businessRuleTask | //xmlns:userTask |
            //xmlns:scriptTask | //xmlns:subProcess | //xmlns:callActivity",
            signavio
          )
      }
      number_strongly_connected_components <-
        number_connected_components(repetition_and_path_log, xml_internal_doc, signavio)
      cyclomatic_metric <- number_tasks(xml_internal_doc, signavio) -
        number_XOR_gateways(xml_internal_doc, signavio) -
        length(join_activities) - 
        number_event_based_gateways(xml_internal_doc, signavio) + number_strongly_connected_components
    } else {
      cyclomatic_metric <- NA
    }
    
    
    return(cyclomatic_metric)
  }

#' @title Average connector degree
#'
#' @description Average connector degree is defined as the average incoming and outgoing sequence flows of all gateways and activities with at least two incoming or outgoing sequence flows
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @return an integer indicating the average connector degree
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' avg_connector_degree(file_path)
#' @export
avg_connector_degree <-
  function (file_path, signavio = FALSE) {
    xml_internal_doc <- create_internal_doc(file_path, signavio)
    number_gateways <- number_AND_gateways(xml_internal_doc, signavio) +
      number_XOR_gateways(xml_internal_doc, signavio) +
      number_OR_gateways(xml_internal_doc, signavio) +
      number_complex_gateways(xml_internal_doc, signavio) +
      number_event_based_gateways(xml_internal_doc, signavio)
    if (!signavio) {
      join_activities <-
        join_gateways(
          xml_internal_doc,
          "//bpmn:task | //bpmn:sendTask | //bpmn:receiveTask |
          //bpmn:manualTask | //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
          //bpmn:subProcess | //bpmn:callActivity | //task",
          signavio
        )
      split_activities <-
        split_gateways(
          xml_internal_doc,
          "//bpmn:task | //bpmn:sendTask | //bpmn:receiveTask |
          //bpmn:manualTask | //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
          //bpmn:subProcess | //bpmn:callActivity | //task",
          signavio
        )
    } else {
      join_activities <-
        join_gateways(
          xml_internal_doc,
          "//xmlns:task | //xmlns:sendTask | //xmlns:receiveTask |
          //xmlns:manualTask | //xmlns:businessRuleTask | //xmlns:userTask | //xmlns:scriptTask |
          //xmlns:subProcess | //xmlns:callActivity",
          signavio
        )
      split_activities <-
        split_gateways(
          xml_internal_doc,
          "//xmlns:task | //xmlns:sendTask | //xmlns:receiveTask |
          //xmlns:manualTask | //xmlns:businessRuleTask | //xmlns:userTask | //xmlns:scriptTask |
          //xmlns:subProcess | //xmlns:callActivity",
          signavio
        )
    }
    connector_activities <- c(join_activities, split_activities)
    number_connector_activities <-
      length(unique(connector_activities))
    
    if ((number_gateways + number_connector_activities) > 0)
      return((
        total_io_flows_gateways(xml_internal_doc, signavio) + number_io_flows_activities_with_id(xml_internal_doc, connector_activities, signavio)
      ) / (number_gateways + number_connector_activities)
      )
    else
      return(0)
  }

#' @title Maximum connector degree
#'
#' @description Maximum connector degree is defined as the gateway or activity with the most incoming and outgoing sequence flows
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @return an integer indicating the maximum connector degree
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' max_connector_degree(file_path)
#' @export
max_connector_degree <-
  function (file_path, signavio = FALSE) {
    xml_internal_doc <- create_internal_doc(file_path, signavio)
    return(max_io_flows_gateways_activities(xml_internal_doc, signavio))
  }

#' @title Separability
#'
#' @description {A cut vertex is a node which if removed, splits the diagram into two pieces
#' The consequence is that elements which are part of each path can be defined as a cut vertex
#' Separability is defined as the number of cut vertices divided by (the size of the model - 2)}
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @param path_log_already_created boolean which indicates whether the path log has already been created before or not. When you are not sure, it is best to use the standard which is false
#' @param generate_new_path_log used when it is not possible to save the path log such as with the Rapid miner or in unit tests and examples
#' @param time_to_generate_path_log time which is the maximum time to generate a new path log in seconds. The standard setting is 1500 seconds.
#' @return an integer indicating the separability
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' separability(file_path, generate_new_path_log = TRUE)
#' @export
separability <-
  function(file_path,
           signavio = FALSE,
           path_log_already_created = FALSE,
           generate_new_path_log = FALSE,
           time_to_generate_path_log = 1500) {
    xml_internal_doc <- create_internal_doc(file_path, signavio)
    if(!path_log_already_created) {
      repetition_and_path_log <- tryCatch(expr = withTimeout(create_path_and_repetition_log(file_path, signavio = signavio), timeout = time_to_generate_path_log), 
                 TimeoutException = function(ex) "TimedOut")
      if(!generate_new_path_log ) {
        usethis::use_data(repetition_and_path_log,  internal = TRUE, overwrite = TRUE)
      }
    } 
    if(length(repetition_and_path_log) > 1) {
    elements <- NULL
    #Take the intersection of all paths of the path log, filter the join and split element and remove duplicates
    if (length(repetition_and_path_log[[1]]) > 0) {
      intersection_elements <-
        reduce(repetition_and_path_log[[1]], intersect)
      intersection_elements <-
        intersection_elements[intersection_elements != "split" &
                                intersection_elements != "join"]
      intersection_elements <- unique(intersection_elements)
      if (!signavio) {
        split_elements <-
          split_gateways(
            xml_internal_doc,
            "//bpmn:parallelGateway | //parallelGateway | //bpmn:inclusiveGateway |
            //inclusiveGateway | //bpmn:complexGateway | //complexGateway | //bpmn:task | //bpmn:sendTask | //bpmn:receiveTask |
            //bpmn:manualTask | //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
            //bpmn:subProcess | //bpmn:callActivity | //task | //bpmn:eventBasedGateway | //eventBasedGateway |
            //bpmn:exclusiveGateway | //exclusiveGateway",
            signavio
          )
        join_elements <-
          join_gateways(
            xml_internal_doc,
            "//bpmn:parallelGateway | //parallelGateway | //bpmn:inclusiveGateway |
            //inclusiveGateway | //bpmn:complexGateway | //complexGateway | //bpmn:task | //bpmn:sendTask | //bpmn:receiveTask |
            //bpmn:manualTask | //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
            //bpmn:subProcess | //bpmn:callActivity | //task | //bpmn:eventBasedGateway | //eventBasedGateway |
            //bpmn:exclusiveGateway | //exclusiveGateway",
            signavio
          )
      } else {
        split_elements <-
          split_gateways(
            xml_internal_doc,
            "//xmlns:parallelGateway | //xmlns:inclusiveGateway |
            //xmlns:complexGateway  | //xmlns:task | //xmlns:sendTask | //xmlns:receiveTask |
            //xmlns:manualTask | //xmlns:businessRuleTask | //xmlns:userTask | //xmlns:scriptTask |
            //xmlns:subProcess | //xmlns:callActivity  | //xmlns:eventBasedGateway |
            //xmlns:exclusiveGateway",
            signavio
          )
        join_elements <-
          join_gateways(
            xml_internal_doc,
            "//xmlns:parallelGateway | //xmlns:inclusiveGateway |
            //xmlns:complexGateway  | //xmlns:task | //xmlns:sendTask | //xmlns:receiveTask |
            //xmlns:manualTask | //xmlns:businessRuleTask | //xmlns:userTask | //xmlns:scriptTask |
            //xmlns:subProcess | //xmlns:callActivity  | //xmlns:eventBasedGateway |
            //xmlns:exclusiveGateway",
            signavio
          )
      }
      both_split_join <- intersect(split_elements, join_elements)
      
      
      intersection_elements <- as.tibble(intersection_elements)
      colnames(intersection_elements) <- "elements"
      intersection_elements <- intersection_elements %>%
        filter(!(elements %in% both_split_join)) %>%
        pull(elements)
      
      if (!signavio)
        sequence_flow_nodes <-
        getNodeSet(xml_internal_doc, "//bpmn:sequenceFlow |
                   //sequenceFlow")
      else
        sequence_flow_nodes <-
        getNodeSet(xml_internal_doc,
                   "//xmlns:sequenceFlow",
                   namespace(xml_internal_doc))
      id_seq_flow <-
        unlist(xmlApply(sequence_flow_nodes, xmlGetAttr, name = "id"))
      source_seq_flow <-
        unlist(xmlApply(sequence_flow_nodes, xmlGetAttr, name = "sourceRef"))
      target_seq_flow <-
        unlist(xmlApply(sequence_flow_nodes, xmlGetAttr, name = "targetRef"))
      
      relations <-
        as.data.frame(cbind(id_seq_flow, source_seq_flow, target_seq_flow))
      if (length(relations) != 0) {
        n_cut_vertices <- relations %>%
          filter(
            source_seq_flow %in% intersection_elements &
              target_seq_flow %in% intersection_elements
          ) %>%
          summarise(n = n()) %>%
          pull(n)
      } else
        n_cut_vertices <- 0
      
      #Calculate the size of the model by taking all the unique elements of the path log
      size_model <- size_process_model(file_path, signavio)
      
      #Make a distinction between an empty model and a model containing more than 2 elements
      #Separability is equal to the number of cut vertices divided by the size of the model - 2,
      #if the size is bigger than 2, otherwise it is 0
      if (size_model > 2)
        separability <-
        n_cut_vertices / (size_model - n_data_objects(file_path, signavio) - 2)
      else
        separability <- 1
      
      return(separability)
    } else {
      return (1)
    }
    } else {
      return(NA)
    }
    }

#' @title Sequentiality
#'
#' @description Sequentiality is defined as the number of sequence flows connecting two tasks divided by the total number of sequence flows
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @return an integer indicating the sequentiality
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' sequentiality(file_path)
#' @export
sequentiality <- function (file_path, signavio = FALSE) {
  xml_internal_doc <- create_internal_doc(file_path, signavio)
  task_id <- NULL
  sequential <- NULL
  #Take all sequence flow nodes and save the target and source in one object
  if (!signavio)
    sequence_flow_nodes <-
    getNodeSet(xml_internal_doc, "//bpmn:sequenceFlow | //sequenceFlow")
  else
    sequence_flow_nodes <-
    getNodeSet(xml_internal_doc,
               "//xmlns:sequenceFlow",
               namespace(xml_internal_doc))
  source_seq_flow <-
    unlist(xmlApply(sequence_flow_nodes, xmlGetAttr, name = "sourceRef"))
  target_seq_flow <-
    unlist(xmlApply(sequence_flow_nodes, xmlGetAttr, name = "targetRef"))
  target_source <-
    as.data.frame(cbind(source_seq_flow, target_seq_flow))
  
  #get the id of all task and event_nodes
  task_ids <-
    task_names(xml_internal_doc,
               filter_non_connector_activities = TRUE,
               signavio)
  if (length(task_ids) > 0)
    task_ids <- task_ids %>% pull(task_id)
  if (!signavio)
    event_ids <-
    node_ids(
      xml_internal_doc,
      "//bpmn:startEvent | //bpmn:messageStartEvent | //bpmn:timerStartEvent |
      //bpmn:conditionalStartEvent | //bpmn:endEvent | //bpmn:messageEndEvent |
      //bpmn:terminateEndEvent | //bpmn:escalationEndEvent | //bpmn:errorEndEvent |
      //bpmn:compensationEndEvent | //bpmn:signalEndEvent | //bpmn:intermediateCatchEvent |
      //bpmn:intermediateThrowEvent | //bpmn:boundaryEvent | //startEvent | //endEvent | //intermediateEvent",
      sequential = TRUE,
      signavio
    )
  else
    event_ids <-
    node_ids(
      xml_internal_doc,
      "//xmlns:startEvent | //xmlns:messageStartEvent | //xmlns:timerStartEvent |
      //xmlns:conditionalStartEvent | //xmlns:endEvent | //xmlns:messageEndEvent |
      //xmlns:terminateEndEvent | //xmlns:escalationEndEvent | //xmlns:errorEndEvent |
      //xmlns:compensationEndEvent | //xmlns:signalEndEvent | //xmlns:intermediateCatchEvent |
      //xmlns:intermediateThrowEvent | //xmlns:boundaryEvent",
      sequential = TRUE,
      signavio
    )
  
  task_event_ids <- c(as.character(task_ids), event_ids)
  #take the sum of all sequence flows which connect two tasks
  target_source_both_task_event <- target_source %>%
    mutate(
      sequential = if_else(
        source_seq_flow %in% task_event_ids &
          target_seq_flow %in% task_event_ids,
        TRUE,
        FALSE
      )
    ) %>%
    summarise(sequentiality = sum(sequential))
  
  sequentiality <-
    target_source_both_task_event / number_sequence_flows(xml_internal_doc, signavio)
  return(as.numeric(sequentiality))
}

#' @title Cyclicity
#'
#' @description Cyclicity is defined as the number of nodes on a cycle divided by the total number of nodes
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @param path_log_already_created boolean which indicates whether the path log has already been created before or not. When you are not sure, it is best to use the standard which is false
#' @param generate_new_path_log used when it is not possible to save the path log such as with the Rapid miner or in unit tests and examples
#' @param time_to_generate_path_log time which is the maximum time to generate a new path log in seconds. The standard setting is 1500 seconds.
#' @return an integer indicating the cyclicity
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' cyclicity(file_path, generate_new_path_log = TRUE)
#' @export
cyclicity <-
  function(file_path,
           signavio = FALSE,
           path_log_already_created = FALSE,
           generate_new_path_log = FALSE,
           time_to_generate_path_log = 1500) {
    xml_internal_doc <- create_internal_doc(file_path, signavio)
    if(!path_log_already_created) {
      repetition_and_path_log <- tryCatch(expr = withTimeout(create_path_and_repetition_log(file_path, signavio = signavio), timeout = time_to_generate_path_log), 
                 TimeoutException = function(ex) "TimedOut")
      if(!generate_new_path_log ) {
        usethis::use_data(repetition_and_path_log,  internal = TRUE, overwrite = TRUE)
      }
    } 
    
    if(length(repetition_and_path_log) > 1) {
    #Take all elements which are part of the loops
    elements_on_cycle <-
      unique(unlist(repetition_and_path_log[[2]]))
    
    #filter out of elements on cycle all elements which have the name split or join
    elements_on_cycle <-
      elements_on_cycle[elements_on_cycle != "split" &
                          elements_on_cycle != "join"]
    
    #if the path log is empty, return zero, otherwise return the cyclicity
    cyclicity <-
      length(elements_on_cycle) / (
        size_process_model(file_path, signavio) - n_data_objects(file_path, signavio)
      )
    } else {
      cyclicity <- NA
    }
    return(cyclicity)
  }

#' @title Diameter
#'
#' @description Length of longest path, in practice the length of longest path.
#' The assumption is made that one repetition for each loop is allowed and these repetitions count as well for the diameter
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @param path_log_already_created boolean which indicates whether the path log has already been created before or not. When you are not sure, it is best to use the standard which is false
#' @param generate_new_path_log used when it is not possible to save the path log such as with the Rapid miner or in unit tests and examples
#' @param time_to_generate_path_log time which is the maximum time to generate a new path log in seconds. The standard setting is 1500 seconds.
#' @return an integer indicating the diameter
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' diameter(file_path, generate_new_path_log = TRUE)
#' @export
diameter <- function(file_path, 
                     signavio = FALSE,
                     path_log_already_created = FALSE,
                     generate_new_path_log = FALSE,
                     time_to_generate_path_log = 1500) {
  repetition_and_path_log <- NULL
  if(!path_log_already_created) {
    repetition_and_path_log <- tryCatch(expr = withTimeout(create_path_and_repetition_log(file_path, signavio = signavio), timeout = time_to_generate_path_log), 
               TimeoutException = function(ex) "TimedOut")
    if(!generate_new_path_log ) {
      usethis::use_data(repetition_and_path_log,  internal = TRUE, overwrite = TRUE)
    }
  } 
  if(length(repetition_and_path_log) > 1) {
  path_log <- repetition_and_path_log[[1]]
  
  #filter out all join and split elements as these are not relevant for the diameter
  if (length(path_log) > 0) {
    path_log <-
      lapply(path_log, function(path) {
        path[path != "join" & path != "split"]
      })
    
    #calculate the length of each path
    length_paths <- unlist(lapply(path_log, length))
    
    #The max length is the diameter
    return(max(length_paths))
  } else {
    return (0)
  }
  } else {
    return(NA)
  }
}

#' @title Structuredness
#'
#' @description Structuredness measures to which extent the process model can be divided into block structured structures (matching gateways)
#' Calculation: 1 - size of reduced process model / size of normal process model
#' To get the reduced process model, the following rules are applied:
#' -removal of trivial constructs (one incoming and one outgoing sequence flow)
#' -removal of matching gateways (for loops, this means first a join then a split, for all other gateways, it's the other way around)
#' -loops with other than XOR-gateways and non-matching gateways are kept
#' -gateways which are the consequence of multiple start or end events are removed
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @param generate_new_path_log used when it is not possible to save the path log such as with the Rapid miner or in unit tests and examples
#' @param path_log_already_created boolean which indicates whether the path log has already been created before or not. When you are not sure, it is best to use the standard which is false
#' @param time_to_generate_path_log time which is the maximum time to generate a new path log in seconds. The standard setting is 1500 seconds.
#' @return an integer indicating the structuredness
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' structuredness(file_path, generate_new_path_log = TRUE)
#' @export
structuredness <-
  function(file_path,
           signavio = FALSE,
           path_log_already_created = FALSE,
           generate_new_path_log = FALSE,
           time_to_generate_path_log = 1500) {
    xml_internal_doc <- create_internal_doc(file_path, signavio)
    
    if(!path_log_already_created) {
      repetition_and_path_log <- tryCatch(expr = withTimeout(create_path_and_repetition_log(file_path, signavio = signavio), timeout = time_to_generate_path_log), 
                 TimeoutException = function(ex) "TimedOut")
      if(!generate_new_path_log ) {
        usethis::use_data(repetition_and_path_log,  internal = TRUE, overwrite = TRUE)
      }
    } 
    if(length(repetition_and_path_log) > 1) {
    # Take the structured path_log
    structured_path_log <- repetition_and_path_log[[4]]
    
    # Keep all elements in the path log which are non-trivial, but remove splits and joins due to multiple start/end events
    # By getting the index of these elements
    indices_elements_to_keep <-
      lapply(structured_path_log, function(path) {
        #delete all trivial constructs (one or less than one incoming and/or outgoing flow) and splits and joins which are due to multiple start/end events
        non_trivial_constructs <-
          c(
            "AND-split",
            "AND-join",
            "OR-split",
            "OR-join",
            "XOR-join",
            "XOR-split",
            "XOR-loop-split",
            "XOR-loop-join",
            "Other-loop-split",
            "Other-loop-join"
          )
        indices_non_trivial_constructs <-
          which(path %in% non_trivial_constructs)
        indices_artifacts_before_constructs <-
          indices_non_trivial_constructs - 1
        indices_elements_to_keep <-
          unique(c(
            indices_non_trivial_constructs,
            indices_artifacts_before_constructs
          ))
        return(indices_elements_to_keep)
      })
    indices_elements_to_keep <-
      lapply(indices_elements_to_keep, sort)
    
    #Filter all elements with the corresponding index
    reduce_path <- function(path, keep_element_indices) {
      return (path[keep_element_indices])
    }
    reduced_path_log <-
      mapply(reduce_path,
             structured_path_log,
             indices_elements_to_keep,
             SIMPLIFY = FALSE)
    
    join_elements_and_loop_split <-
      c("AND-join",
        "OR-join",
        "XOR-join",
        "XOR-loop-split",
        "Other-loop-split")
    if (length(reduced_path_log) > 0) {
      non_structured_elements <-
        unstructuredElements(reduced_path_log, join_elements_and_loop_split)
    }
    
    #remove all duplicates and make a vector in order to get the elements of the reduced model
    
    
    if (length(reduced_path_log) == 0) {
      size_reduced_model <-
        size_process_model(file_path, signavio) - n_data_objects(file_path, signavio)
    } else {
      non_structured_elements <- unique(unlist(non_structured_elements))
      size_reduced_model <- length(non_structured_elements)
    }
    structuredness <-
      1 - (size_reduced_model / (
        size_process_model(file_path, signavio) - n_data_objects(file_path, signavio)
      ))
    } else {
      structuredness <- NA
    }
    return (structuredness)
  }

#' @title Depth
#'
#' @description Depth is defined as the the nesting of the process model.
#' If there is a split gateway, the depth is increased with one.
#' If there is a join gateway, the depth is decreased with one.
#' The cumulative sum is taken and the maximum of the cumulative sum is calculated for each path.
#' The nesting depth is the maximum of each path value
#' @param file_path document object created using the create_internal_document function
#' @param path_log_already_created boolean which indicates whether the path log has already been created before or not. When you are not sure, it is best to use the standard which is false
#' @param generate_new_path_log used when it is not possible to save the path log such as with the Rapid miner or in unit tests and examples
#' @param time_to_generate_path_log time which is the maximum time to generate a new path log in seconds. The standard setting is 1500 seconds.
#' @param signavio boolean which indicates whether the file stems from signavio
#' @return an integer indicating the depth
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' depth(file_path, generate_new_path_log = TRUE)
#' @export
depth <- function(file_path,
                  signavio = FALSE,
                  path_log_already_created = FALSE,
                  generate_new_path_log = FALSE,
                  time_to_generate_path_log = 1500) {
  
  if(!path_log_already_created) {
    repetition_and_path_log <- tryCatch(expr = withTimeout(create_path_and_repetition_log(file_path, signavio = signavio), timeout = time_to_generate_path_log), 
               TimeoutException = function(ex) "TimedOut")
    if(!generate_new_path_log ) {
      usethis::use_data(repetition_and_path_log,  internal = TRUE, overwrite = TRUE)
    }
  } 
  if(length(repetition_and_path_log) > 1) {
  path_log <- repetition_and_path_log[[1]]
  
  #Filter out of the path log only the elements with a join or split name
  
  path_log <-
    lapply(path_log, function(path) {
      path[path == "join" | path == "split"]
    })
  if (length(path_log) > 0) {
    #Filter all join elements which come before the first split
    split_indices <-
      lapply(path_log, function(path) {
        which(path == "split")
      })
    first_split_index_path <-
      unlist(lapply(split_indices, function(split_indices_per_path) {
        split_indices_per_path[1]
      }))
    path_after_split <- function(path, index) {
      if (!(is.na(index)))
        path[index:length(path)]
      else
        path
    }
    path_log_starting_splits <-
      mapply(path_after_split, path_log, first_split_index_path)
    
    #Replace all split elements with one and all join elements with minus one
    numerical_path_log <-
      lapply(path_log_starting_splits, function(path) {
        path[which(path == "split")] <- 1
        path[which(path == "join")] <- -1
        return(path)
      })
    
    #Take the cumulative sum for each path, take the max of the cumsum for each path
    #and take the maximum of all maximum values
    cum_sum_numerical_path_log <- lapply(numerical_path_log, cumsum)
    max_cum_sum <-
      unlist(lapply(cum_sum_numerical_path_log, function(path) {
        if (length(path) != 0)
          return(max(path))
        else
          return (0)
      }))
    depth <- max(max_cum_sum)
    return(depth)
  } else
    return(0)
  } else {
    return(NA)
  }
}


#' @title Token Split
#'
#' @description {Token split is defined as the sum of the outgoing flows of parallel, inclusive and complex gateways minus one,
#' because otherwise the token_split value is always one, while it should be zero if there are}
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @return an integer indicating the token_split
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' token_split(file_path)
#' @export
token_split <- function (file_path, signavio = FALSE) {
  xml_internal_doc <- create_internal_doc(file_path, signavio)
  if (!signavio)
    outgoing_flows_split_gateways <-
      number_outgoing_flows(
        xml_internal_doc,
        "//bpmn:parallelGateway | //parallelGateway | //bpmn:inclusiveGateway |
        //inclusiveGateway | //bpmn:complexGateway | //complexGateway | //bpmn:task | //bpmn:sendTask | //bpmn:receiveTask |
        //bpmn:manualTask | //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
        //bpmn:subProcess | //bpmn:callActivity | //task",
        filter_split = TRUE,
        signavio
      )
  else
    outgoing_flows_split_gateways <-
      number_outgoing_flows(
        xml_internal_doc,
        "//xmlns:parallelGateway  | //xmlns:inclusiveGateway |
        //xmlns:complexGateway | //xmlns:task | //xmlns:sendTask | //xmlns:receiveTask |
        //xmlns:manualTask | //xmlns:businessRuleTask | //xmlns:userTask | //xmlns:scriptTask |
        //xmlns:subProcess | //xmlns:callActivity",
        filter_split = TRUE,
        signavio
      )
  if (length(outgoing_flows_split_gateways) != 0) {
    return(sum(outgoing_flows_split_gateways - 1))
  } else {
    return(0)
  }
}

#' @title Coupling metric
#'
#' @description {Coupling metric is defined as the sum of the number of activities, AND-splits and a weighterd number of OR and XOR splits}
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @return an integer indicating the control flow complexity
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' coupling_metric(file_path)
#' @export
coupling_metric <-
  function (file_path, signavio = FALSE) {
    xml_internal_doc <- create_internal_doc(file_path, signavio)
    if (!signavio) {
    o_flows_exclusive <-
      number_outgoing_flows(xml_internal_doc,
                            "//bpmn:exclusiveGateway | //exclusiveGateway")
    o_flows_event <-
      number_outgoing_flows(xml_internal_doc,
                            "//bpmn:eventBasedGateway | //eventBasedGateway")
    o_flows_complex <-
      number_outgoing_flows(xml_internal_doc,
                            "//bpmn:complexGateway | //complexGateway")
    o_flows_inclusive <-
      number_outgoing_flows(xml_internal_doc,
                            "//bpmn:inclusiveGateway | //inclusiveGateway")
    
    i_flows_exclusive <-
      number_incoming_flows(xml_internal_doc,
                            "//bpmn:exclusiveGateway | //exclusiveGateway")
    i_flows_event <-
      number_incoming_flows(xml_internal_doc,
                            "//bpmn:eventBasedGateway | //eventBasedGateway")
    i_flows_complex <-
      number_incoming_flows(xml_internal_doc,
                            "//bpmn:complexGateway | //complexGateway")
    i_flows_inclusive <-
      number_incoming_flows(xml_internal_doc,
                            "//bpmn:inclusiveGateway | //inclusiveGateway")
    
    i_flows_activities <-
      number_incoming_flows(xml_internal_doc,
                            "//bpmn:task | //bpmn:sendTask | //bpmn:receiveTask |
                            //bpmn:manualTask | //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
                            //bpmn:subProcess | //bpmn:callActivity | //task")
    o_flows_activities <-
      number_outgoing_flows(xml_internal_doc,
                            "//bpmn:task | //bpmn:sendTask | //bpmn:receiveTask |
                            //bpmn:manualTask | //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
                            //bpmn:subProcess | //bpmn:callActivity | //task")
    } else {
      o_flows_exclusive <-
        number_outgoing_flows(xml_internal_doc, "//xmlns:exclusiveGateway", signavio = signavio)
      o_flows_event <-
        number_outgoing_flows(xml_internal_doc, "//xmlns:eventBasedGateway", signavio = signavio)
      o_flows_complex <-
        number_outgoing_flows(xml_internal_doc, "//xmlns:complexGateway", signavio = signavio)
      o_flows_inclusive <-
        number_outgoing_flows(xml_internal_doc, "//xmlns:inclusiveGateway", signavio = signavio)
      
      i_flows_exclusive <-
        number_incoming_flows(xml_internal_doc, "//xmlns:exclusiveGateway", signavio = signavio)
      i_flows_event <-
        number_incoming_flows(xml_internal_doc, "//xmlns:eventBasedGateway", signavio = signavio)
      i_flows_complex <-
        number_incoming_flows(xml_internal_doc, "//xmlns:complexGateway", signavio = signavio)
      i_flows_inclusive <-
        number_incoming_flows(xml_internal_doc, "//xmlns:inclusiveGateway", signavio = signavio)
      
      i_flows_activities <-
        number_incoming_flows(xml_internal_doc, "//xmlns:task | //xmlns:sendTask | //xmlns:receiveTask |
                              //xmlns:manualTask | //xmlns:businessRuleTask | //xmlns:userTask | //xmlns:scriptTask |
                              //xmlns:subProcess | //xmlns:callActivity", signavio = signavio)
      
      o_flows_activities <-
        number_outgoing_flows(xml_internal_doc, "//xmlns:task | //xmlns:sendTask | //xmlns:receiveTask |
                              //xmlns:manualTask | //xmlns:businessRuleTask | //xmlns:userTask | //xmlns:scriptTask |
                              //xmlns:subProcess | //xmlns:callActivity", signavio = signavio)
    }
    
    xor_join_activity_indices <- which(i_flows_activities > 0)
    i_flows_activities <-  i_flows_activities[xor_join_activity_indices]
    o_flows_activities <-  o_flows_activities[xor_join_activity_indices]
    
    xor_weights <- c(1/(i_flows_exclusive * o_flows_exclusive), 1/(i_flows_event*o_flows_event), 1/(i_flows_activities * o_flows_activities))
    or_weights <- c(1 + 1/((2^i_flows_inclusive - 1) * (2^o_flows_inclusive - 1)) * ( 1 - 1/(i_flows_inclusive * o_flows_inclusive)), 1 + 1/((2^i_flows_complex - 1) * (2^o_flows_complex - 1)) * ( 1 - 1/(i_flows_complex * o_flows_complex)))
    
    xor_weights <- sum(xor_weights)
    or_weights <- sum(or_weights)
    and_weights <- number_AND_gateways(xml_internal_doc, signavio)
    activity_weights <- number_tasks(xml_internal_doc, signavio) - length(xor_join_activity_indices)
    
    coupling_metric <- xor_weights + or_weights + and_weights + activity_weights
    
    return(coupling_metric)
  }

#' @title Cognitive weights
#'
#' @description {Cognitive weight is defined as a weighted sum of gateways and activities}
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @return an integer indicating the control flow complexity
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' cognitive_weight(file_path)
#' @export
cognitive_weight <-
  function (file_path, signavio = FALSE) {
    xml_internal_doc <- create_internal_doc(file_path, signavio)
    if(!signavio) {
    o_flows_exclusive <-
      number_outgoing_flows(xml_internal_doc,
                            "//bpmn:exclusiveGateway | //exclusiveGateway")
    o_flows_event <-
      number_outgoing_flows(xml_internal_doc,
                            "//bpmn:eventBasedGateway | //eventBasedGateway")
    o_flows_complex <-
      number_outgoing_flows(xml_internal_doc,
                            "//bpmn:complexGateway | //complexGateway")
    o_flows_inclusive <-
      number_outgoing_flows(xml_internal_doc,
                            "//bpmn:inclusiveGateway | //inclusiveGateway")
    o_flows_parallel <-
      number_outgoing_flows(xml_internal_doc,
                            "//bpmn:parallelGateway | //parallelGateway")
    o_flows_task <-
      number_outgoing_flows(xml_internal_doc,
                            "//bpmn:task | //bpmn:sendTask | //bpmn:receiveTask |
                            //bpmn:manualTask | //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
                            //bpmn:subProcess | //bpmn:callActivity | //task")
    } else {
      o_flows_exclusive <-
        number_outgoing_flows(xml_internal_doc, "//xmlns:exclusiveGateway", signavio = signavio)
      o_flows_event <-
        number_outgoing_flows(xml_internal_doc, "//xmlns:eventBasedGateway", signavio = signavio)
      o_flows_complex <-
        number_outgoing_flows(xml_internal_doc, "//xmlns:complexGateway", signavio = signavio)
      o_flows_inclusive <-
        number_outgoing_flows(xml_internal_doc, "//xmlns:inclusiveGateway", signavio = signavio)
      o_flows_parallel <-
        number_outgoing_flows(xml_internal_doc, "//xmlns:parallelGateway", signavio = signavio)
      o_flows_task <-
        number_outgoing_flows(xml_internal_doc, "//xmlns:task | //xmlns:sendTask | //xmlns:receiveTask |
                              //xmlns:manualTask | //xmlns:businessRuleTask | //xmlns:userTask | //xmlns:scriptTask |
                              //xmlns:subProcess | //xmlns:callActivity", signavio = signavio)
    }
    number_sequences <- length(o_flows_task == 1)
    number_parallel_splits <- length(o_flows_task > 1) + length(o_flows_parallel > 1)
    number_or_splits <- length(o_flows_inclusive > 1) + length(o_flows_complex > 1)
    number_xor_split_2 <- length(o_flows_exclusive == 2) + length(o_flows_event == 2)
    number_xor_splits_more <- length(o_flows_exclusive > 2) + length(o_flows_event > 2)
    
    cognitive_weight <- number_sequences + 2 * number_xor_split_2 + 3 * number_xor_splits_more + 4 * number_parallel_splits + 7 * number_or_splits
    return(cognitive_weight)
  }

#' @title Control flow complexity
#'
#' @description {Control flow complexity is defined as the sum of the outgoing of exclusive gateways, the number of parallel gateways
#' and two to the power of all outgoing sequence flows of the inclusive gateways}
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @return an integer indicating the control flow complexity
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' control_flow_complexity(file_path)
#' @export
control_flow_complexity <-
  function (file_path, signavio = FALSE) {
    xml_internal_doc <- create_internal_doc(file_path, signavio)
    if (!signavio) {
      o_flows_exclusive <-
        number_outgoing_flows(xml_internal_doc,
                              "//bpmn:exclusiveGateway | //exclusiveGateway")
      o_flows_event <-
        number_outgoing_flows(xml_internal_doc,
                              "//bpmn:eventBasedGateway | //eventBasedGateway")
      o_flows_complex <-
        number_outgoing_flows(xml_internal_doc,
                              "//bpmn:complexGateway | //complexGateway")
      o_flows_inclusive <-
        number_outgoing_flows(xml_internal_doc,
                              "//bpmn:inclusiveGateway | //inclusiveGateway")
      
      cfc <-
        number_split_gateways(
          xml_internal_doc,
          "//bpmn:parallelGateway | //parallelGateway |
          //bpmn:task | //bpmn:sendTask | //bpmn:receiveTask |
          //bpmn:manualTask | //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
          //bpmn:subProcess | //bpmn:callActivity | //task"
        ) +
        sum(o_flows_exclusive[o_flows_exclusive > 1]) +
        sum(o_flows_event[o_flows_event > 1]) +
        sum(2 ^ o_flows_inclusive[o_flows_inclusive > 1]) +
        sum(2 ^ o_flows_complex[o_flows_complex > 1]) -
        sum(o_flows_inclusive > 1) -
        sum(o_flows_complex > 1)
    } else {
      o_flows_exclusive <-
        number_outgoing_flows(xml_internal_doc, "//xmlns:exclusiveGateway", signavio = signavio)
      o_flows_event <-
        number_outgoing_flows(xml_internal_doc, "//xmlns:eventBasedGateway", signavio = signavio)
      o_flows_complex <-
        number_outgoing_flows(xml_internal_doc, "//xmlns:complexGateway", signavio = signavio)
      o_flows_inclusive <-
        number_outgoing_flows(xml_internal_doc, "//xmlns:inclusiveGateway", signavio = signavio)
      
      cfc <-
        number_split_gateways(
          xml_internal_doc,
          "//xmlns:parallelGateway  |
          //xmlns:task | //xmlns:sendTask | //xmlns:receiveTask |
          //xmlns:manualTask | //xmlns:businessRuleTask | //xmlns:userTask | //xmlns:scriptTask |
          //xmlns:subProcess | //xmlns:callActivity",
          signavio = signavio
        ) +
        sum(o_flows_exclusive[o_flows_exclusive > 1]) +
        sum(o_flows_event[o_flows_event > 1]) +
        sum(2 ^ o_flows_inclusive[o_flows_inclusive > 1]) +
        sum(2 ^ o_flows_complex[o_flows_complex > 1]) -
        sum(o_flows_inclusive > 1) -
        sum(o_flows_complex > 1)
      
    }
    
    return(cfc)
  }

#' @title Connector mismatch
#'
#' @description Connector mismatch is the absolute value of the difference between split gateways and join gateways for each type of gateway, ie parallel, exclusive, inclusive, complex and event based gateways
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @return an integer indicating the connector mismatch
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' connector_mismatch(file_path)
#' @export
connector_mismatch <-
  function (file_path, signavio = FALSE) {
    xml_internal_doc <- create_internal_doc(file_path, signavio)
    if (!signavio) {
      connector_mismatch <-
        abs(
          number_split_gateways(
            xml_internal_doc,
            "//bpmn:exclusiveGateway | //exclusiveGateway",
            signavio
          )
          - number_join_gateways(
            xml_internal_doc,
            "//bpmn:exclusiveGateway | //exclusiveGateway | //bpmn:task | //bpmn:sendTask | //bpmn:receiveTask |
            //bpmn:manualTask | //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
            //bpmn:subProcess | //bpmn:callActivity | //task",
            signavio
          )
        ) +
        abs(
          number_split_gateways(
            xml_internal_doc,
            "//bpmn:parallelGateway | //parallelGateway | //bpmn:task | //bpmn:sendTask | //bpmn:receiveTask |
            //bpmn:manualTask | //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
            //bpmn:subProcess | //bpmn:callActivity | //task",
            signavio
          )
          - number_join_gateways(
            xml_internal_doc,
            "//bpmn:parallelGateway | //parallelGateway",
            signavio
          )
        ) +
        abs(
          number_split_gateways(
            xml_internal_doc,
            "//bpmn:inclusiveGateway | //inclusiveGateway",
            signavio
          )
          - number_join_gateways(
            xml_internal_doc,
            "//bpmn:inclusiveGateway | //inclusiveGateway",
            signavio
          )
        ) +
        abs(
          number_split_gateways(
            xml_internal_doc,
            "//bpmn:complexGateway | //complexGateway",
            signavio
          )
          - number_join_gateways(
            xml_internal_doc,
            "//bpmn:complexGateway | //complexGateway",
            signavio
          )
        ) +
        abs(
          number_split_gateways(
            xml_internal_doc,
            "//bpmn:eventBasedGateway | //eventBasedGateway",
            signavio
          )
          - number_join_gateways(
            xml_internal_doc,
            "//bpmn:eventBasedGateway | //eventBasedGateway",
            signavio
          )
        )
    } else {
      connector_mismatch <-
        abs(
          number_split_gateways(xml_internal_doc, "//xmlns:exclusiveGateway", signavio)
          - number_join_gateways(
            xml_internal_doc,
            "//xmlns:exclusiveGateway | //xmlns:task | //xmlns:sendTask | //xmlns:receiveTask |
            //xmlns:manualTask | //xmlns:businessRuleTask | //xmlns:userTask | //xmlns:scriptTask |
            //xmlns:subProcess | //xmlns:callActivity",
            signavio
          )
        ) +
        abs(
          number_split_gateways(
            xml_internal_doc,
            "//xmlns:parallelGateway | //xmlns:task | //xmlns:sendTask | //xmlns:receiveTask |
            //xmlns:manualTask | //xmlns:businessRuleTask | //xmlns:userTask | //xmlns:scriptTask |
            //xmlns:subProcess | //xmlns:callActivity",
            signavio
          )
          - number_join_gateways(xml_internal_doc, "//xmlns:parallelGateway", signavio)
        ) +
        abs(
          number_split_gateways(xml_internal_doc, "//xmlns:inclusiveGateway", signavio)
          - number_join_gateways(xml_internal_doc, "//xmlns:inclusiveGateway", signavio)
        ) +
        abs(
          number_split_gateways(xml_internal_doc, "//xmlns:complexGateway", signavio)
          - number_join_gateways(xml_internal_doc, "//xmlns:complexGateway", signavio)
        ) +
        abs(
          number_split_gateways(xml_internal_doc, "//xmlns:eventBasedGateway", signavio)
          - number_join_gateways(xml_internal_doc, "//xmlns:eventBasedGateway", signavio)
        )
    }
    return(connector_mismatch)
  }

#' @title Connector heterogeneity
#'
#' @description Connector heterogeneity is defined as the sum of minus - p times the log of p of all gateways. p is defined as the number of a particular type of gateway divided by all gateways.
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @return an integer indicating the connector heterogeneity
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' connector_heterogeneity(file_path)
#' @export
connector_heterogeneity <-
  function (file_path, signavio = FALSE) {
    xml_internal_doc <- create_internal_doc(file_path, signavio)
    event_based_gateways <-
      number_event_based_gateways(xml_internal_doc, signavio)
    complex_gateways <-
      number_complex_gateways(xml_internal_doc, signavio)
    or_gateways <- number_OR_gateways(xml_internal_doc, signavio)
    if (!signavio) {
      and_gateways <-
        number_AND_gateways(xml_internal_doc, signavio) + length(
          split_gateways(
            xml_internal_doc,
            "//bpmn:task | //bpmn:sendTask | //bpmn:receiveTask |
            //bpmn:manualTask | //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
            //bpmn:subProcess | //bpmn:callActivity | //task",
            signavio
          )
        )
      xor_gateways <-
        number_XOR_gateways(xml_internal_doc, signavio) + length(
          join_gateways(
            xml_internal_doc,
            "//bpmn:task | //bpmn:sendTask | //bpmn:receiveTask |
            //bpmn:manualTask | //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
            //bpmn:subProcess | //bpmn:callActivity | //task",
            signavio
          )
        )
    } else {
      and_gateways <-
        number_AND_gateways(xml_internal_doc, signavio) + length(
          split_gateways(
            xml_internal_doc,
            "//xmlns:task | //xmlns:sendTask | //xmlns:receiveTask |
            //xmlns:manualTask | //xmlns:businessRuleTask | //xmlns:userTask | //xmlns:scriptTask |
            //xmlns:subProcess | //xmlns:callActivity",
            signavio
          )
        )
      xor_gateways <-
        number_XOR_gateways(xml_internal_doc, signavio) + length(
          join_gateways(
            xml_internal_doc,
            "//xmlns:task | //xmlns:sendTask | //xmlns:receiveTask |
            //xmlns:manualTask | //xmlns:businessRuleTask | //xmlns:userTask | //xmlns:scriptTask |
            //xmlns:subProcess | //xmlns:callActivity",
            signavio
          )
        )
    }
    total_gateways <-
      event_based_gateways + complex_gateways + or_gateways + and_gateways + xor_gateways
    
    #If there are no gateways, return 0
    if (total_gateways == 0)
      return(0)
    
    #Calculate the number of gateway categories present in the model
    categories_present <- 0
    
    if (event_based_gateways != 0)
      categories_present <- categories_present + 1
    if (complex_gateways != 0)
      categories_present <- categories_present + 1
    if (and_gateways != 0)
      categories_present <- categories_present + 1
    if (xor_gateways != 0)
      categories_present <- categories_present + 1
    if (or_gateways != 0)
      categories_present <- categories_present + 1
    
    #if there is only one category, return 1
    if (categories_present == 1 || categories_present == 0)
      return(0)
    
    #Calculate heterogeneity and return
    p_event <- event_based_gateways / total_gateways
    p_complex <- complex_gateways / total_gateways
    p_or <- or_gateways / total_gateways
    p_and <- and_gateways / total_gateways
    p_xor <- xor_gateways / total_gateways
    
    heterogeneity <-
      -p_event * if_else(p_event != 0, log(p_event, categories_present), as.double(0)) -
      p_complex * if_else(p_complex != 0,
                          log(p_complex, categories_present),
                          as.double(0)) -
      p_or * if_else(p_or != 0, log(p_or, categories_present), as.double(0)) -
      p_and * if_else(p_and != 0, log(p_and, categories_present), as.double(0)) -
      p_xor * if_else(p_xor != 0, log(p_xor, categories_present), as.double(0))
    
    return(heterogeneity)
  }

#' @title Cross Connectivity
#'
#' @description The cross-connectivity metric that measures the strength of the links between process model elements.
#' The definition of this new metric builds on the hypothesis that process models are easier understood and contain less errors if they have a high cross-connectivity.
#' The metric is calculated based on the creation of a data frame containing the values of all connections
#' @param file_path document object created using the create_internal_document function
#' @param signavio boolean which indicates whether the file stems from signavio
#' @param path_log_already_created boolean which indicates whether the path log has already been created before or not. When you are not sure, it is best to use the standard which is false
#' @param generate_new_path_log used when it is not possible to save the path log such as with the Rapid miner or in unit tests and examples
#' @param time_to_generate_path_log time which is the maximum time to generate a new path log in seconds. The standard setting is 1500 seconds.
#' @return an integer indicating the cross connectivity of a model
#' @examples
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' cross_connectivity(file_path, generate_new_path_log = TRUE)
#' @export
cross_connectivity <-
  function(file_path,
           signavio = FALSE,
           path_log_already_created = FALSE, 
           generate_new_path_log = FALSE,
           time_to_generate_path_log = 1500) {
    xml_internal_doc <- create_internal_doc(file_path, signavio)
    if(!path_log_already_created) {
      repetition_and_path_log <- 
        tryCatch(expr = withTimeout(create_path_and_repetition_log(file_path, signavio = signavio), timeout = time_to_generate_path_log), 
                 TimeoutException = function(ex) "TimedOut")
      if(!generate_new_path_log ) {
        usethis::use_data(repetition_and_path_log,  internal = TRUE, overwrite = TRUE)
      }
    } 
    type <- NULL
    start <- NULL
    end <- NULL
    values <- NULL
    if(length(repetition_and_path_log) > 0) {

    path_log <- repetition_and_path_log[[1]]
    if (!signavio) {
      nodes <-
      getNodeSet(
        xml_internal_doc,
        "//bpmn:exclusiveGateway | //bpmn:parallelGateway |
        //bpmn:inclusiveGateway | //bpmn:eventBasedGateway | //bpmn:complexGateway |
        //exclusiveGateway | //parallelGateway | //inclusiveGateway | //eventBasedGateway |
        //complexGateway | //bpmn:task | //bpmn:sendTask | //bpmn:receiveTask |
        //bpmn:manualTask | //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
        //bpmn:subProcess | //bpmn:callActivity | //task"
      )
    } 
    else {
      nodes <-
      getNodeSet(
        xml_internal_doc,
        "//xmlns:exclusiveGateway | //xmlns:parallelGateway |
        //xmlns:inclusiveGateway | //xmlns:eventBasedGateway | //xmlns:complexGateway |
        //xmlns:task | //xmlns:sendTask | //xmlns:receiveTask |
        //xmlns:manualTask | //xmlns:businessRuleTask | //xmlns:userTask | //xmlns:scriptTask |
        //xmlns:subProcess | //xmlns:callActivity",
        namespace(xml_internal_doc)
      )
    }
    #Check all children of the gateway node having the name incoming
    nodes_incoming <- xmlApply(nodes,
                               xmlElementsByTagName,
                               name = "incoming",
                               recursive = FALSE)
    nodes_incoming <- lapply(nodes_incoming, length)
    
    nodes_outgoing <- xmlApply(nodes,
                               xmlElementsByTagName,
                               name = "outgoing",
                               recursive = FALSE)
    nodes_outgoing <- lapply(nodes_outgoing, length)
    
    node_id <-
      as.character(unlist(xmlApply(nodes, xmlGetAttr, name = "id")))
    
    sum_incoming_outgoing <- mapply(function(incoming, outgoing) {
      return(incoming + outgoing)
    },
    nodes_incoming,
    nodes_outgoing)
    node_with_degree <-
      as.data.frame(cbind(node_id, sum_incoming_outgoing), stringsAsFactors = FALSE)
    if (!signavio) {
      parallel_ids <-
        node_ids(xml_internal_doc,
                 "//bpmn:parallelGateway | //parallelGateway",
                 signavio = signavio)
      exclusive_ids <-
        node_ids(
          xml_internal_doc,
          "//bpmn:exclusiveGateway | //exclusiveGateway | //bpmn:eventBasedGateway | //eventBasedGateway",
          signavio = signavio
        )
      inclusive_ids <-
        node_ids(
          xml_internal_doc,
          "//bpmn:inclusiveGateway | //inclusiveGateway | //bpmn:complexGateway | //complexGateway",
          signavio = signavio
        )
    } 
    else {
      parallel_ids <-
        node_ids(xml_internal_doc, "//xmlns:parallelGateway", signavio = signavio)
      exclusive_ids <-
        node_ids(
          xml_internal_doc,
          "//xmlns:exclusiveGateway | //xmlns:eventBasedGateway",
          signavio = signavio
        )
      inclusive_ids <-
        node_ids(
          xml_internal_doc,
          "//xmlns:inclusiveGateway | //xmlns:complexGateway",
          signavio = signavio
        )
    }
    node_with_degree <- node_with_degree %>%
      mutate(type = if_else(
        node_id %in% parallel_ids,
        "AND",
        if_else(
          node_id %in% exclusive_ids,
          "XOR",
          if_else(node_id %in% inclusive_ids, "OR", "Task")
        )
      )) %>%
      mutate(sum_incoming_outgoing = as.numeric(sum_incoming_outgoing)) %>%
      mutate(degree = if_else(
        type == "Task" | type == "AND",
        1,
        if_else(type == "XOR", 1 / sum_incoming_outgoing,
                (
                  1 / (2 ^ (sum_incoming_outgoing) - 1) + (2 ^ (sum_incoming_outgoing) - 2) / ((2 ^ (
                    sum_incoming_outgoing
                  ) - 1) * sum_incoming_outgoing)
                ))
      ))
    path_log <-
      lapply(path_log, function(path) {
        path[path != "join" & path != "split"]
      })
    
    
    if (length(path_log) > 0) {
      denominator <-
        ((
          size_process_model(file_path, signavio) - n_data_objects(file_path, signavio)
        ) * (
          size_process_model(file_path, signavio) - n_data_objects(file_path, signavio) - 1
        )
        )
      if (denominator != 0 && length(node_with_degree$degree) > 0) {
        value_connections <- 
          tryCatch(expr = withTimeout(valueOfConnectionPaths(path_log, node_with_degree), timeout = time_to_generate_path_log), 
                   TimeoutException = function(ex) NA)
        if (length(value_connections$values) > 0) {
          numerator <- value_connections %>%
            group_by(start, end) %>%
            summarise(values = max(values)) %>%
            ungroup() %>%
            distinct() %>%
            summarise(values = sum(values)) %>%
            pull(values)
          return(numerator / (denominator))
        } 
          else {
          return(0)
        }
      }
      else
        return(0)
    } 
    else {
      return(NA)
    } 
    }
  }

