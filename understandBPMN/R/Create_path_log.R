#' @title Path and repetition log
#'
#' @description {This function returns a list with four or three nested list objects:
#' - One for the paths:
#' Assumption: if a path contains a loop, the path contains one repetition (so two times) of the execution of this loop
#' Assumption: there is no difference made between the type of gateways.
#' So the path log is not a path log according to the definition found in the literature, but more a kind of a path log
#' Assumption: for each split and join in the log, an extra element is added with the name "split" or "join"
#' - One list object for the loops (repetitions) which start with a join and end with a join
#' - One list object for the loops (repetitions) which start with a split and end with a split
#'( - One list for the paths in which all gateways have a certain type)}
#' @param file_path internal document containing an xml
#' @param signavio boolean which indicates whether the file stems from signavio
#' @param add_path_log_for_structuredness a boolean value indicating whether the structured path log should be added. Is standard TRUE
#' @return a list containing the path log, a list of repetitions starting with join, a list of repetitions starting with split, (optional: structured path log)
#' @examples 
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' create_path_and_repetition_log(file_path)
#' @export
create_path_and_repetition_log <-
  function(file_path,
           add_path_log_for_structuredness = TRUE,
           signavio = FALSE) {
    xml_internal_doc <- create_internal_doc(file_path, signavio)
    #create a data frame with all sequence flow ids, sources and targets of the sequence flows
    if (!signavio) {
      sequence_flow_nodes <-
        getNodeSet(xml_internal_doc, "//bpmn:sequenceFlow |
                   //sequenceFlow")
    } else {
      sequence_flow_nodes <-
        getNodeSet(xml_internal_doc,
                   "//xmlns:sequenceFlow",
                   namespace(xml_internal_doc))
    }
    id_seq_flow <-
      unlist(xmlApply(sequence_flow_nodes, xmlGetAttr, name = "id"))
    source_seq_flow <-
      unlist(xmlApply(sequence_flow_nodes, xmlGetAttr, name = "sourceRef"))
    target_seq_flow <-
      unlist(xmlApply(sequence_flow_nodes, xmlGetAttr, name = "targetRef"))
    
    relations <-
      as.data.frame(cbind(id_seq_flow, source_seq_flow, target_seq_flow))
    
    if (!signavio) {
      boundary_events <-
        getNodeSet(xml_internal_doc,
                   "//bpmn:boundaryEvent |
                   //boundaryEvent")
    } else {
      boundary_events <-
        getNodeSet(xml_internal_doc,
                   "//xmlns:boundaryEvent",
                   namespace(xml_internal_doc))
    }
    if (length(boundary_events) > 0) {
      generate_random_string <- function() {
        return(paste(sample(c(
          0:9, letters, LETTERS
        )), collapse = ""))
      }
      source_seq_flow_boundary <-
        unlist(xmlApply(boundary_events, xmlGetAttr, name = "attachedToRef"))
      target_seq_flow_boundary <-
        unlist(xmlApply(boundary_events, xmlGetAttr, name = "id"))
      id_seq_flow_boundary <-
        sapply(1:length(source_seq_flow), function(index) {
          return(generate_random_string())
        })
      relations_boundary <-
        as.data.frame(
          cbind(
            id_seq_flow_boundary,
            source_seq_flow_boundary,
            target_seq_flow_boundary
          )
        )
      names(relations_boundary) <-
        c("id_seq_flow", "source_seq_flow", "target_seq_flow")
      relations <- rbind(relations, relations_boundary)
    }
    if (!signavio) {
      intermediate_throw_link_events <-
        getNodeSet(
          xml_internal_doc,
          "//bpmn:intermediateThrowEvent/bpmn:linkEventDefinition |
          //intermediateThrowEvent/linkEventDefinition"
        )
      intermediate_catch_link_events <-
        getNodeSet(
          xml_internal_doc,
          "//bpmn:intermediateCatchEvent/bpmn:linkEventDefinition |
          //intermediateCatchEvent/linkEventDefinition"
        )
    } else {
      intermediate_throw_link_events <-
        getNodeSet(
          xml_internal_doc,
          "//xmlns:intermediateThrowEvent/xmlns:linkEventDefinition",
          namespace(xml_internal_doc)
        )
      intermediate_catch_link_events <-
        getNodeSet(
          xml_internal_doc,
          "//xmlns:intermediateCatchEvent/xmlns:linkEventDefinition",
          namespace(xml_internal_doc)
        )
    }
    if (length(intermediate_throw_link_events) > 0 &
        length(intermediate_catch_link_events) > 0) {
      throw_ids <-
        unlist(xmlApply(intermediate_throw_link_events, xmlGetAttr, name = "id"))
      throw_names <-
        unlist(xmlApply(intermediate_throw_link_events, xmlGetAttr, name = "name"))
      throw_data_frame <- as.data.frame(cbind(throw_ids, throw_names))
      catch_ids <-
        unlist(xmlApply(intermediate_catch_link_events, xmlGetAttr, name = "id"))
      catch_names <-
        unlist(xmlApply(intermediate_catch_link_events, xmlGetAttr, name = "name"))
      catch_data_frame <- as.data.frame(cbind(catch_ids, catch_names))
      link_relations <-
        left_join(throw_data_frame,
                  catch_data_frame,
                  by = c("throw_names" = "catch_names")) %>%
        select(throw_ids, catch_ids)
      id_seq_flow_link <-
        sapply(1:length(link_relations$throw_ids), function(index) {
          return(generate_random_string())
        })
      relations_link <-
        as.data.frame(cbind(
          id_seq_flow_link,
          link_relations$throw_ids,
          link_relations$catch_ids
        ))
      names(relations_link) <-
        c("id_seq_flow", "source_seq_flow", "target_seq_flow")
      relations <- rbind(relations, relations_link)
    }
    #Create a list of all start events and initialize repetitions lists
    if (!signavio) {
      start_events <- getNodeSet(xml_internal_doc, "//bpmn:startEvent |
                                 //startEvent")
    } else {
      start_events <-
        getNodeSet(xml_internal_doc,
                   "//xmlns:startEvent",
                   namespace(xml_internal_doc))
    }
    path_log <- xmlApply(start_events, xmlGetAttr, name = "id")
    repetitions <- list()
    repetitions_till_split <- list()
    
    if (length(relations$id_seq_flow) > 0 && length(path_log) > 0) {
      path_log_with_repetitions <- createPathLog(path_log, relations)
      path_log <- path_log_with_repetitions[[1]]
      repetitions <- path_log_with_repetitions[[2]]
      repetitions_till_split <- path_log_with_repetitions[[3]]
    }
    # If a path log for structuredness must be created (with all splits and joins receiving a type), this is done using the call of a repetit
    if (add_path_log_for_structuredness) {
      path_log_structuredness <-
        create_structured_path_log(path_log,
                                   xml_internal_doc,
                                   repetitions,
                                   repetitions_till_split,
                                   signavio)
      return (list(
        path_log,
        repetitions,
        repetitions_till_split,
        path_log_structuredness
      ))
    } else
      return (list(path_log, repetitions, repetitions_till_split))
  }



#creates a structured path log, replacing splits and joins with loop-splits and normal splits and loop-joins and normal joins of a certain type
create_structured_path_log <-
  function(path_log,
           xml_internal_doc,
           repetitions,
           repetitions_till_split,
           signavio = FALSE) {
    task_id <- NULL
    if (!signavio) {
      parallel_ids <-
        node_ids(xml_internal_doc,
                 "//bpmn:parallelGateway | //parallelGateway")
      exclusive_ids <-
        node_ids(
          xml_internal_doc,
          "//bpmn:exclusiveGateway | //exclusiveGateway | //bpmn:eventBasedGateway | //eventBasedGateway"
        )
        event_ids <-
          node_ids(
            xml_internal_doc,
            "//bpmn:startEvent | //bpmn:messageStartEvent | //bpmn:timerStartEvent |
        //bpmn:conditionalStartEvent | //bpmn:endEvent | //bpmn:messageEndEvent |
        //bpmn:terminateEndEvent | //bpmn:escalationEndEvent | //bpmn:errorEndEvent |
        //bpmn:compensationEndEvent | //bpmn:signalEndEvent | //bpmn:intermediateCatchEvent |
        //bpmn:intermediateThrowEvent | //bpmn:boundaryEvent | //startEvent | //endEvent | //intermediateEvent"
          )
      inclusive_ids <-
        node_ids(
          xml_internal_doc,
          "//bpmn:inclusiveGateway | //inclusiveGateway | //bpmn:complexGateway | //complexGateway"
        )
    } else {
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
      event_ids <-
        node_ids(
          xml_internal_doc,
          "//xmlns:startEvent | //xmlns:messageStartEvent | //xmlns:timerStartEvent |
          //xmlns:conditionalStartEvent | //xmlns:endEvent | //xmlns:messageEndEvent |
          //xmlns:terminateEndEvent | //xmlns:escalationEndEvent | //xmlns:errorEndEvent |
          //xmlns:compensationEndEvent | //xmlns:signalEndEvent | //xmlns:intermediateCatchEvent |
          //xmlns:intermediateThrowEvent | //xmlns:boundaryEvent", signavio = signavio)
    }
    task_ids <- task_names(xml_internal_doc, signavio = signavio)
    if (length(task_ids) != 0)
      task_ids <- task_ids %>% pull(task_id)
    else
      task_ids <- vector(mode = "character")
    
    task_ids <- as.character(task_ids)
    
    split_artifacts <- lapply(path_log, function(path) {
      split_indices <- which(path == "split")
      split_artifact_indices <- split_indices - 1
      split_artifacts <-
        lapply(split_artifact_indices, function(index) {
          return(path[index])
        })
    })
    split_artifacts <- unique(unlist(split_artifacts))
    
    join_artifacts <- lapply(path_log, function(path) {
      join_indices <- which(path == "join")
      join_artifact_indices <- join_indices - 1
      join_artifacts <-
        lapply(join_artifact_indices, function(index) {
          return(path[index])
        })
    })
    join_artifacts <- unique(unlist(join_artifacts))
    
    if (length(repetitions) > 0) {
      loop_splits <- loop_splits(repetitions)
      loop_joins <- loop_joins(repetitions_till_split)
      
      xor_loop_splits <- intersect(loop_splits, exclusive_ids)
      xor_loop_joins <- intersect(loop_joins, exclusive_ids)
      
      other_loop_splits <- setdiff(loop_splits, xor_loop_splits)
      other_loop_joins <- setdiff(loop_joins, xor_loop_joins)
      
      path_log_loops <- lapply(path_log, function(path) {
        if (length(xor_loop_splits) > 0) {
          loop_split_indices <- which(path %in% xor_loop_splits) + 1
          path[loop_split_indices] <- "XOR-loop-split"
        }
        if (length(xor_loop_joins) > 0) {
          loop_join_indices <- which(path %in% xor_loop_joins) + 1
          path[loop_join_indices] <- "XOR-loop-join"
        }
        if (length(other_loop_splits) > 0) {
          loop_split_indices <- which(path %in% other_loop_splits) + 1
          path[loop_split_indices] <- "Other-loop-split"
        }
        if (length(other_loop_splits) > 0) {
          loop_join_indices <- which(path %in% other_loop_joins) + 1
          path[loop_join_indices] <- "Other-loop-join"
        }
        return(path)
      })
      
    } else {
      path_log_loops <- path_log
    }
    
    #check if there are more start_events. A start_join is a join which is the common join of the intersection of paths for two or more start_events
    start_events <- lapply(path_log_loops, function(path) {
      return(path[1])
    })
    different_start_events <- length(unique(start_events)) > 1
    unique_start_events <- unique(start_events)
    if (different_start_events) {
      intersection_per_start <-
        lapply(unique_start_events, function(start_event) {
          relevant_path_indices <- which(start_events == start_event)
          relevant_paths <-
            lapply(relevant_path_indices, function(index) {
              return (path_log_loops[[index]])
            })
          intersection_per_start <- reduce(relevant_paths, intersect)
          return (intersection_per_start)
        })

      number_starts <- 1:length(unique(start_events))
      #Calculate all combinations in order that all possible combinations can be used to calculate the intersection
      all_combinations <-
        lapply(number_starts, function(length_combination) {
          if (length_combination > 1) {
            combinations_df <-
              combn(1:length(number_starts), length_combination)
            combinations_list <-
              lapply(seq_len(ncol(combinations_df)), function(i)
                combinations_df[, i])
            return(combinations_list)
          } else {
            return(list())
          }
        })
      all_combinations <- unlist(all_combinations, recursive = FALSE)

      joins_due_to_different_starts <-
        lapply(all_combinations, function(combination) {
          relevant_intersections <- lapply(combination, function(index) {
            return (intersection_per_start[[index]])
          })
          intersection_between_start_paths <-
            reduce(relevant_intersections, intersect)
          index_first_common_join <-
            which(intersection_between_start_paths %in% join_artifacts)[1]
          return (intersection_between_start_paths[index_first_common_join])
        })
      joins_due_to_different_starts <-
        unique(unlist(joins_due_to_different_starts))

      #replace relevant join elements with start-join
      path_log_loops <- lapply(path_log_loops, function(path) {
        special_join_indices <-
          which(path %in% joins_due_to_different_starts) + 1
        path[special_join_indices] <- "start-join"
        return (path)
      })
    }

    #check if there are more end_events
    end_events <- lapply(path_log_loops, function(path) {
      return(path[length(path)])
    })

    different_end_events <- length(unique(end_events)) > 1
    unique_end_events <- unique(end_events)

    if (different_end_events) {
      intersection_per_end <-
        lapply(unique_end_events, function(end_event) {
          relevant_path_indices <- which(end_events == end_event)
          relevant_paths <-
            lapply(relevant_path_indices, function(index) {
              return (path_log_loops[[index]])
            })
          intersection_per_end <- reduce(relevant_paths, intersect)
          return (intersection_per_end)
        })

      number_ends <- 1:length(unique(end_events))
      #Calculate all combinations in order that all possible combinations can be used to calculate the intersection
      all_combinations <-
        lapply(number_ends, function(length_combination) {
          if (length_combination > 1) {
            combinations_df <- combn(1:length(number_ends), length_combination)
            combinations_list <-
              lapply(seq_len(ncol(combinations_df)), function(i)
                combinations_df[, i])
            return(combinations_list)
          } else {
            return(list())
          }
        })
      all_combinations <- unlist(all_combinations, recursive = FALSE)

      splits_due_to_different_ends <-
        lapply(all_combinations, function(combination) {
          relevant_intersections <- lapply(combination, function(index) {
            return (intersection_per_end[[index]])
          })
          intersection_between_end_paths <-
            reduce(relevant_intersections, intersect)
          common_split_indices <-
            which(intersection_between_end_paths %in% split_artifacts)
          index_last_common_split <-
            common_split_indices[length(common_split_indices)]
          return (intersection_between_end_paths[index_last_common_split])
        })
      splits_due_to_different_ends <-
        unique(unlist(splits_due_to_different_ends))

      #replace relevant join elements with end-split
      path_log_loops <- lapply(path_log_loops, function(path) {
        special_split_indices <-
          which(path %in% splits_due_to_different_ends) + 1
        path[special_split_indices] <- "end-split"
        return (path)
      })
    }

    #replace normal gateway join and split constructs
    
    path_log_replaced_splits <-
      lapply(path_log_loops, function(path) {
        join_indices <- which(path == "join")
        split_indices <- which(path == "split")
        node_before_splits_index <- split_indices - 1
        index_split_preceeded_by_join <-
          which(node_before_splits_index %in% join_indices)
        if (length(index_split_preceeded_by_join) > 0) {
          node_before_splits_index <-
            node_before_splits_index[-index_split_preceeded_by_join]
        }
        parallel_indices <- which(path %in% parallel_ids)
        parallel_split_indices <-
          intersect(parallel_indices, node_before_splits_index)
        exclusive_indices <-  which(path %in% exclusive_ids)
        exclusive_split_indices <-
          intersect(exclusive_indices, node_before_splits_index)
        inclusive_indices <-  which(path %in% inclusive_ids)
        inclusive_split_indices <-
          intersect(inclusive_indices, node_before_splits_index)
        task_indices <- which(path %in% task_ids)
        task_split_indices <-
          intersect(task_indices, node_before_splits_index)
        event_indices <- which(path %in% event_ids)
        event_split_indices <- 
          intersect(event_indices, node_before_splits_index)
        
        if (length(parallel_split_indices) > 0)
          path[parallel_split_indices + 1] <- "AND-split"
        if (length(task_split_indices) > 0)
          path[task_split_indices + 1] <- "AND-split"
        if (length(exclusive_split_indices) > 0)
          path[exclusive_split_indices + 1] <- "XOR-split"
        if (length(inclusive_split_indices) > 0)
          path[inclusive_split_indices + 1] <- "OR-split"
        if (length(event_split_indices) > 0)
          path[event_split_indices + 1] <- "AND-split"
        
        #if a join proceeds a split
        node_before_splits_index <- split_indices - 1
        node_before_splits_index <-
          node_before_splits_index[index_split_preceeded_by_join] - 1
        parallel_split_indices <-
          intersect(parallel_indices, node_before_splits_index)
        exclusive_split_indices <-
          intersect(exclusive_indices, node_before_splits_index)
        inclusive_split_indices <-
          intersect(inclusive_indices, node_before_splits_index)
        task_split_indices <-
          intersect(task_indices, node_before_splits_index)
        event_split_indices <- 
          intersect(event_indices, node_before_splits_index)
        if (length(parallel_split_indices) > 0)
          path[parallel_split_indices + 2] <- "AND-split"
        if (length(event_split_indices) > 0)
          path[event_split_indices + 2] <- "AND-split"
        if (length(task_split_indices) > 0)
          path[task_split_indices + 2] <- "AND-split"
        if (length(exclusive_split_indices) > 0)
          path[exclusive_split_indices + 2] <- "XOR-split"
        if (length(inclusive_split_indices) > 0)
          path[inclusive_split_indices + 2] <- "OR-split"
        
        return(path)
        
      })
    
    path_log_replaced_joins <-
      lapply(path_log_replaced_splits, function(path) {
        join_indices <- which(path == "join")
        node_before_joins_index <- join_indices - 1
        parallel_indices <- which(path %in% parallel_ids)
        parallel_join_indices <-
          intersect(parallel_indices, node_before_joins_index)
        exclusive_indices <-  which(path %in% exclusive_ids)
        exclusive_join_indices <-
          intersect(exclusive_indices, node_before_joins_index)
        inclusive_indices <-  which(path %in% inclusive_ids)
        inclusive_join_indices <-
          intersect(inclusive_indices, node_before_joins_index)
        task_indices <- which(path %in% task_ids)
        task_join_indices <-
          intersect(task_indices, node_before_joins_index)
        event_indices <- which(path %in% event_ids)
        event_join_indices <-
          intersect(event_indices, node_before_joins_index)
        
        if (length(parallel_join_indices) > 0)
          path[parallel_join_indices + 1] <- "AND-join"
        if (length(task_join_indices) > 0)
          path[task_join_indices + 1] <- "XOR-join"
        if (length(event_join_indices) > 0)
          path[event_join_indices + 1] <- "XOR-join"
        if (length(exclusive_join_indices) > 0)
          path[exclusive_join_indices + 1] <- "XOR-join"
        if (length(inclusive_join_indices) > 0)
          path[inclusive_join_indices + 1] <- "OR-join"
        
        return(path)
        
      })
    
    return(path_log_replaced_joins)
  }

#Join gateway of a loop is the gateway which has always the same artifact before
loop_joins <- function(repetitions_till_split) {
  start <- NULL
  artifact_before_join <- NULL
  join_artifact <- NULL
  needed_frequency <- NULL
  number <- NULL
  repetition_starts <-
    lapply(repetitions_till_split, function(repetition) {
      return (repetition[1])
    })
  repetition_starts <- data.frame(unlist(repetition_starts))
  colnames(repetition_starts) <- "start"
  repetition_starts <- repetition_starts %>%
    group_by(start) %>%
    summarise(needed_frequency = n())
  
  two_artifacts_before_till_join <-
    lapply(repetitions_till_split, function(repetition) {
      join_indices <- which(repetition == "join")
      #Make sure that index won't get out of bounds, so should be at least bigger than 2
      join_indices <- join_indices[join_indices > 2]
      start_element <- repetition[1]
      two_artifacts_before_till_join <-
        lapply(join_indices, function(join_index) {
          two_artifacts_before_till_join <-
            c(start_element, repetition[join_index - 2], repetition[join_index - 1], repetition[join_index])
          return (two_artifacts_before_till_join)
        })
      return(two_artifacts_before_till_join)
    })
  
  two_artifacts_before_till_join <-
    lapply(two_artifacts_before_till_join, unique)
  two_artifacts_before_till_join <-
    unlist(two_artifacts_before_till_join, recursive = FALSE)
  df_two_artifacts_before_till_join <-
    as.data.frame(do.call(rbind, two_artifacts_before_till_join))
  colnames(df_two_artifacts_before_till_join) <-
    c("start", "artifact_before_join", "join_artifact", "join")
  
  df_two_artifacts_before_till_join <-
    left_join(df_two_artifacts_before_till_join, repetition_starts)
  loop_joins <- df_two_artifacts_before_till_join %>%
    select(start,
           artifact_before_join,
           join_artifact,
           needed_frequency) %>%
    group_by(start,
             artifact_before_join,
             join_artifact,
             needed_frequency) %>%
    summarise(number = n()) %>%
    filter(needed_frequency == number) %>%
    ungroup() %>%
    pull(join_artifact) %>%
    unique()
  
  return(loop_joins)
}

#split gateway van een loop wordt altijd gevolgd door dezelfde activiteiten of gateways
loop_splits <- function(repetitions) {
  start <- NULL
  artifact_before <- NULL
  artifact_after <- NULL
  needed_frequency <- NULL
  number <- NULL
  repetition_starts <- lapply(repetitions, function(repetition) {
    return (repetition[1])
  })
  repetition_starts <- data.frame(unlist(repetition_starts))
  colnames(repetition_starts) <- "start"
  
  repetition_starts <- repetition_starts %>%
    group_by(start) %>%
    summarise(needed_frequency = n())
  
  start_split_artifact_before_after <-
    lapply(repetitions, function(repetition) {
      split_indices <- which(repetition == "split")
      start_element <- repetition[1]
      start_split_artifact_before_after <-
        lapply(split_indices, function(split_index) {
          start_split_artifact_before_after <-
            c(start_element, repetition[split_index - 1], repetition[split_index], repetition[split_index + 1])
          return (start_split_artifact_before_after)
        })
      return(start_split_artifact_before_after)
    })
  start_split_artifact_before_after <-
    lapply(start_split_artifact_before_after, unique)
  start_split_artifact_before_after <-
    unlist(start_split_artifact_before_after, recursive = FALSE)
  df_start_split_artifact_before_after <-
    as.data.frame(do.call(rbind, start_split_artifact_before_after))
  colnames(df_start_split_artifact_before_after) <-
    c("start", "artifact_before", "split", "artifact_after")
  
  df_start_split_artifact_before_after <-
    left_join(df_start_split_artifact_before_after, repetition_starts)
  loop_splits <- df_start_split_artifact_before_after %>%
    select(start, artifact_before, artifact_after, needed_frequency) %>%
    group_by(start, artifact_before, artifact_after, needed_frequency) %>%
    summarise(number = n()) %>%
    filter(needed_frequency == number) %>%
    ungroup() %>%
    pull(artifact_before) %>%
    unique()
  
  return(loop_splits)
}
