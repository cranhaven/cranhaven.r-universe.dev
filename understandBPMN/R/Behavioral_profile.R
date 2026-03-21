#' @title activity names repetitions
#'
#' @description { This functions returns a list containing the repetitions with their respective activity names
#' This can be useful for measuring the understandability using behavioral profiles.
#' }
#' @param repetition_and_path_log repetition and path log list object created by the function create_repetition_and_path_log
#' @param xml_internal_doc document object created using the create_internal_document function
#' @return a list containing the repetitions with their respective activity names
#' @examples \dontrun{activity_multiple_times_executed(log, doc, "A")}
#' @export
#'
activity_names_repetitions <-
  function(repetition_and_path_log,
           xml_internal_doc) {
    activity <- NULL
    task_id <- NULL
    all_tasks <- task_names(xml_internal_doc)
    all_tasks$task_id <- as.character(all_tasks$task_id)
    all_tasks$task_names <- as.character(all_tasks$task_names)
    ids_activity <- all_tasks %>%
      pull(task_id)
    repetitions <- repetition_and_path_log[[2]]
    repetitions <- lapply(repetitions, function(repetition) {
      repetition <- sapply(1:length(repetition), function(index) {
        task_name <- all_tasks %>%
          filter(task_id == repetition[index]) %>%
          pull(task_names)
      })
      repetition <- unlist(repetition)
    })
    return(unique(repetitions))
  }

#' @title activity sometimes multiple times executed
#'
#' @description { This functions returns true or false on whether or not an activity is sometimes multiple times executed
#' This can be useful for measuring the understandability using behavioral profiles.
#' }
#' @param repetition_and_path_log repetition and path log list object created by the function create_repetition_and_path_log
#' @param xml_internal_doc document object created using the create_internal_document function
#' @param activity the activity name
#' @param direct_parallel a table containing the direct and parallel relations
#' @return a boolean value indicating whether it is true that an activity can be executed multiple times in the same path
#' @examples \dontrun{activity_multiple_times_executed(log, doc, "A")}
#' @export
#'

activity_multiple_times_executed <-
  function(repetition_and_path_log,
           xml_internal_doc,
           activity,
           direct_parallel) {
    task_id <- NULL
    structured_path_log <- NULL
    first_task <- NULL
    second_task <- NULL
    relation <- NULL
    all_tasks <- task_names(xml_internal_doc)
    all_tasks$task_id <- as.character(all_tasks$task_id)
    all_tasks$task_names <- as.character(all_tasks$task_names)
    ids_activity <- all_tasks %>%
      filter(task_names == activity) %>%
      pull(task_id)
    repetitions <- repetition_and_path_log[[3]]
    repetitions <- unique(unlist(repetitions))
    activity_part_of_rep <-
      sapply(ids_activity, function(activity_id) {
        return(activity_id %in% repetitions)
      })
    
    filtered_path_log <-
      lapply(structured_path_log, function(path) {
        indices_element_to_keep <- which(path %in% ids_activity)
        return(path[indices_element_to_keep])
      })
    
    paths_contain_more_than_two_executions <-
      sapply(filtered_path_log, function(path) {
        if (length(path) >= 2)
          return(TRUE)
        else
          return(FALSE)
      })
    
    parallel_with_himself <- direct_parallel %>%
      select(first_task, second_task, relation) %>%
      filter(first_task == activity &
               second_task == activity & relation == "parallel") %>%
      summarise(n = n()) %>%
      pull(n)
    multiple_executions <-
      any(paths_contain_more_than_two_executions) |
      any(activity_part_of_rep) | parallel_with_himself > 0
    
    return(multiple_executions)
  }

#' @title activity sometimes not in traces
#'
#' @description { This functions returns true or false on whether or not an activity is sometimes not part of a trace
#' This can be useful for measuring the understandability using behavioral profiles.
#' }
#' @param repetition_and_path_log repetition and path log list object created by the function create_repetition_and_path_log
#' @param xml_internal_doc document object created using the create_internal_document function
#' @param activity the activity name
#' @return a boolean value indicating whether it is true on whether or not an activity is sometimes not part of a trace
#' @examples \dontrun{some_traces_without_activity(log, doc, "A")}
#' @export
#'

some_traces_without_activity <-
  function(repetition_and_path_log,
           xml_internal_doc,
           activity) {
    task_id <- NULL
    all_tasks <- task_names(xml_internal_doc)
    all_tasks$task_id <- as.character(all_tasks$task_id)
    all_tasks$task_names <- as.character(all_tasks$task_names)
    ids_activity <- all_tasks %>%
      filter(task_names == activity) %>%
      pull(task_id)
    
    structured_path_log <- repetition_and_path_log[[4]]
    
    parallel_splits <-
      split_gateways(
        xml_internal_doc,
        "//bpmn:parallelGateway | //parallelGateway |
        //bpmn:task | //bpmn:sendTask | //bpmn:receiveTask | //bpmn:manualTask |
        //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
        //bpmn:subProcess | //bpmn:callActivity | //task"
      )
    parallel_joins <-
      join_gateways(xml_internal_doc,
                    "//bpmn:parallelGateway | //parallelGateway")
    
    #Keep only the activities in the path log
    filtered_path_log <-
      lapply(structured_path_log, function(path) {
        indices_element_to_keep <- which(path %in% ids_activity)
        return(path[indices_element_to_keep])
      })
    
    all_paths_contain_activity <-
      sapply(filtered_path_log, function(path) {
        if (length(path) > 0)
          return(TRUE)
        else
          return(FALSE)
      })
    if (!all(all_paths_contain_activity)) {
      parallel_splits_with_relevant_activity <-
        lapply(structured_path_log, function(path) {
          indices_elements_parallel_splits <- which(path %in% parallel_splits)
          indices_elements_parallel_joins <-
            which(path %in% parallel_joins)
          indices_elements_activity <-
            which(path %in% ids_activity)
          parallel_splits_with_relevant_activity <-
            lapply(indices_elements_parallel_splits, function(index) {
              indices_elements_activity_after_split <-
                indices_elements_activity[indices_elements_activity > index]
              indices_elements_join_after_split <-
                indices_elements_parallel_joins[indices_elements_parallel_joins > index]
              if (length(indices_elements_activity_after_split) > 0 &&
                  length(indices_elements_join_after_split) > 0)
                if (indices_elements_activity_after_split[1] < indices_elements_join_after_split[1])
                  return(path[index])
            })
          return(parallel_splits_with_relevant_activity)
        })
      parallel_splits_with_relevant_activity <-
        unique(unlist(parallel_splits_with_relevant_activity))
      filtered_path_log <-
        lapply(structured_path_log, function(path) {
          indices_element_to_keep <-
            which(path %in% c(ids_activity,  parallel_splits_with_relevant_activity))
          return(path[indices_element_to_keep])
        })
      all_paths_contain_activity_or_parallel_gateway <-
        sapply(filtered_path_log, function(path) {
          if (length(path) > 0)
            return(TRUE)
          else
            return(FALSE)
        })
      return(!all(all_paths_contain_activity_or_parallel_gateway))
    } else {
      return(FALSE)
    }
  }

#' @title Relation in traces
#'
#' @description { This functions returns true or false on whether there exists always or sometimes an (indirect) relation between two activities in a process model.
#' This can be useful for measuring the understandability using behavioral profiles. Always means that wheneve activity 1 is part of the trace, activity 2 will some time follow activity 1.
#' Sometimes means that there should be at least one case where there is an indirect relation and at least one case where there is not.
#' The indirect relations between two activities due to a parallel construct are left out of scope for this function.
#' }
#' @param repetition_and_path_log repetition and path log list object created by the function create_repetition_and_path_log
#' @param xml_internal_doc document object created using the create_internal_document function
#' @param activity_1 the activity name of the first activity
#' @param activity_2 the activity name of the second activity in the relation
#' @param always a boolean value indicating whether there should be always a direct relation. If it is false, it is assumed to be tested for the sometimes case.
#' @param precede a boolean value indicating whether precede or follows relation is tested
#' @param alternate_response a boolean indicating whether an alternate response relation is tested
#' @param alternate_precedence a boolean indicating whether an alternate precedence relation is tested
#' @param chain_response a boolean indicating whether a chain response relation is tested
#' @param chain_precedence a boolean indicating whether a chain precedence relation is tested
#' @param filter_indirect a boolean value indicating whether indirect relations are targeted. If not, all relations are used
#' @param negation_alternate_precedence a boolean indicating whether a negation alternate precedence relation is tested
#' @param negation_alternate_response a boolean indicating whether a negation alternate response relation is tested
#' @return a boolean value indicating whether it is true that there is always or sometimes an indirect relation between activity_1 and activity_2
#' @examples \dontrun{traces_contain_relation(log, doc, "A", "F", TRUE, TRUE)}
#' @export
#'

traces_contain_relation <-
  function(repetition_and_path_log,
           xml_internal_doc,
           activity_1,
           activity_2,
           always = TRUE,
           filter_indirect = TRUE,
           precede = FALSE,
           alternate_response = FALSE,
           alternate_precedence = FALSE,
           chain_response = FALSE,
           chain_precedence = FALSE,
           negation_alternate_precedence = FALSE,
           negation_alternate_response = FALSE) {
    task_id <- NULL
    alternate <-
      alternate_response |
      alternate_precedence |
      negation_alternate_precedence | negation_alternate_response
    chain <- chain_response | chain_precedence
    #get all task names and ids
    all_tasks <- task_names(xml_internal_doc)
    all_tasks$task_id <- as.character(all_tasks$task_id)
    all_tasks$task_names <- as.character(all_tasks$task_names)
    
    #filter indirect or direct activities by setting this variable to 1 or 0. Indirect means that at least one activity is executed between activity_1 and activity_2
    if (filter_indirect) {
      indirect <- 1
    } else {
      indirect <- 0
    }
    
    #find all ids of activities with the name equal to activity_1
    ids_activity_1 <- all_tasks %>%
      filter(task_names == activity_1) %>%
      pull(task_id)
    
    #find all ids of activities with the name equal to activity_1
    ids_activity_2 <- all_tasks %>%
      filter(task_names == activity_2) %>%
      pull(task_id)
    
    #save the structured_path log in a different variable
    structured_path_log <- repetition_and_path_log[[4]]
    several_path_logs <- FALSE
    if (always |
        alternate_response | alternate_precedence) {
      structured_path_log_1 <-
        filtered_path_log_parallel(structured_path_log, xml_internal_doc, activity_1)
      structured_path_log_2 <-
        filtered_path_log_parallel(structured_path_log, xml_internal_doc, activity_2)
      combinations_log_1_2 <-
        expand.grid(1:length(structured_path_log_1),
                    1:length(structured_path_log_2))
      number_combinations <- dim(combinations_log_1_2)[1]
      structured_path_log <-
        lapply(1:number_combinations, function(combination_index) {
          path_log_1_index <- combinations_log_1_2$Var1[combination_index]
          path_log_2_index <-
            combinations_log_1_2$Var2[combination_index]
          return(intersect(structured_path_log_1[[path_log_1_index]], structured_path_log_2[[path_log_2_index]]))
        })
      if (length(structured_path_log) > 1) {
        several_path_logs <- TRUE
      } else {
        structured_path_log <-
          unlist(structured_path_log, recursive = FALSE)
      }
    }
    
    if (!several_path_logs)
      structured_path_log <- list(structured_path_log)
    
    #save all node_ids according to their type (parallel, exclusive, event and special element which indicate some type)
    task_ids <- all_tasks %>%
      pull(task_id)
    parallel_ids <-
      node_ids(xml_internal_doc,
               "//bpmn:parallelGateway | //parallelGateway")
    exclusive_ids <-
      node_ids(
        xml_internal_doc,
        "//bpmn:exclusiveGateway | //exclusiveGateway | //bpmn:eventBasedGateway | //eventBasedGateway"
      )
    inclusive_ids <-
      node_ids(
        xml_internal_doc,
        "//bpmn:inclusiveGateway | //inclusiveGateway | //bpmn:complexGateway | //complexGateway"
      )
    events <-
      node_ids(
        xml_internal_doc,
        "//bpmn:startEvent | //bpmn:messageStartEvent | //bpmn:timerStartEvent |
        //bpmn:conditionalStartEvent | //bpmn:endEvent | //bpmn:messageEndEvent |
        //bpmn:terminateEndEvent | //bpmn:escalationEndEvent | //bpmn:errorEndEvent |
        //bpmn:compensationEndEvent | //bpmn:signalEndEvent | //bpmn:intermediateCatchEvent |
        //bpmn:intermediateThrowEvent | //bpmn:boundaryEvent | //startEvent | //endEvent"
      )
    special_elements <-
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
        "Other-loop-join",
        "start-join",
        "end-split"
      )
    
    #Keep only the activities in the path log
    structured_path_log <-
      lapply(structured_path_log, function(structured_path_log) {
        structured_path_log <-
          lapply(structured_path_log, function(path) {
            indices_element_to_keep <- which(path %in% task_ids)
            return(path[indices_element_to_keep])
          })
      })
    
    #Check if relation in path exists
    if (!alternate & !chain) {
      if (!precede) {
        relation_in_path <-
          lapply(structured_path_log, function(structured_path_log) {
            relation_in_path <- sapply(structured_path_log, function(path) {
              indices_activity_1 <- which(path %in% ids_activity_1)
              indices_activity_2 <-
                which(path %in% ids_activity_2)
              
              if (length(indices_activity_1) > 0 &&
                  length(indices_activity_2) > 0) {
                #take the index of the last node with activity as name
                last_index_activity_2 <-
                  indices_activity_2[length(indices_activity_2)]
                #when indirect, there should be at least one activity between one and two, otherwise,
                #all relations are assumed to be found (direct and indirect), which means indirect = zero
                #this should be the case for all activity_1 nodes, otherwise false is returned
                if (always)
                  return(all(
                    last_index_activity_2 - indices_activity_1 > indirect
                  ))
                else
                  return(any(
                    last_index_activity_2 - indices_activity_1 > indirect
                  ))
              } else if (length(indices_activity_1) == 0) {
                #when activity 1 is not part of the path, return NA, because not much can be said about whether activity_1 is followed by activity_2
                return (NA)
              } else {
                #when activity 2 is not part of the path, return false, because activity_1 is in the trace but is not followed by activity_2
                return (FALSE)
              }
            })
          })
      } else {
        relation_in_path <-
          lapply(structured_path_log, function(structured_path_log) {
            relation_in_path <- sapply(structured_path_log, function(path) {
              indices_activity_1 <- which(path %in% ids_activity_1)
              indices_activity_2 <-
                which(path %in% ids_activity_2)
              if (length(indices_activity_1) > 0 &&
                  length(indices_activity_2) > 0) {
                #take the index of the last node with activity as name
                last_index_activity_2 <-
                  indices_activity_2[length(indices_activity_2)]
                #when indirect, there should be at least one activity between one and two, otherwise,
                #all relations are assumed to be found (direct and indirect), which means indirect = zero
                #this should be the case for all activity_1 nodes, otherwise false is returned
                if (always)
                  return(all(
                    last_index_activity_2 - indices_activity_1 > indirect
                  ))
                else
                  return(any(
                    last_index_activity_2 - indices_activity_1 > indirect
                  ))
              } else if (length(indices_activity_2) == 0) {
                #when activity 2 is not part of the path, return NA, because not much can be said about whether activity_1 is followed by activity_2
                return (NA)
              } else {
                #when activity 2 is not part of the path, return false, because activity_1 is in the trace but is not followed by activity_2
                return (FALSE)
              }
            })
          })
      }
      
    } else if (alternate_precedence | chain_precedence) {
      relation_in_path <-
        lapply(structured_path_log, function(structured_path_log) {
          relation_in_path <- sapply(structured_path_log, function(path) {
            if (alternate_precedence) {
              path <- path[path %in% c(ids_activity_1, ids_activity_2)]
              indices_activity_1 <- which(path %in% ids_activity_1)
              indices_activity_2 <- which(path %in% ids_activity_2)
              indices_activity_2_plus_one <- indices_activity_2 + 1
              two_activities_2_without_activity_1_between <-
                length(intersect(indices_activity_2, indices_activity_2_plus_one)) > 0
              if (length(indices_activity_2) > 0) {
                last_index_activity_2 <-
                  indices_activity_2[length(indices_activity_2)]
                indices_activity_1 <-
                  indices_activity_1[indices_activity_1 < last_index_activity_2]
                at_least_as_many_a1_as_2 <-
                  length(indices_activity_1) >= length(indices_activity_2)
                return(
                  !two_activities_2_without_activity_1_between &
                    at_least_as_many_a1_as_2
                )
              } else {
                if (length(indices_activity_1) > 0)
                  return(TRUE)
                else
                  return(NA)
              }
            }
            if (chain_precedence) {
              indices_activity_1 <- which(path %in% ids_activity_1)
              indices_activity_2 <- which(path %in% ids_activity_2)
              indices_directly_before_a2 <- indices_activity_2 - 1
              intersection_a1_directly_before_a2 <-
                intersect(indices_activity_1, indices_directly_before_a2)
              return(
                length(indices_activity_2) == length(intersection_a1_directly_before_a2)
              )
            }
            
          })
        })
      
    } else if (alternate_response | chain_response) {
      relation_in_path <-
        lapply(structured_path_log, function(structured_path_log) {
          relation_in_path <- sapply(structured_path_log, function(path) {
            if (alternate_response) {
              path <- path[path %in% c(ids_activity_1, ids_activity_2)]
              indices_activity_1 <- which(path %in% ids_activity_1)
              indices_activity_2 <- which(path %in% ids_activity_2)
              indices_activity_1_plus_one <- indices_activity_1 + 1
              two_activities_1_without_activity_2_between <-
                length(intersect(indices_activity_1, indices_activity_1_plus_one)) > 0
              if (length(indices_activity_1) > 0) {
                first_index_activity_1 <- indices_activity_1[1]
                indices_activity_2 <-
                  indices_activity_2[indices_activity_2 > first_index_activity_1]
                at_least_as_many_a2_as_1 <-
                  length(indices_activity_2) >= length(indices_activity_1)
                return(
                  !two_activities_1_without_activity_2_between &
                    at_least_as_many_a2_as_1
                )
              } else {
                if (length(indices_activity_2) > 0)
                  return(TRUE)
                else
                  return(NA)
              }
            }
            if (chain_response) {
              indices_activity_1 <- which(path %in% ids_activity_1)
              indices_activity_2 <- which(path %in% ids_activity_2)
              indices_directly_after_a1 <- indices_activity_1 + 1
              intersection_a2_directly_after_a1 <-
                intersect(indices_activity_2, indices_directly_after_a1)
              return(
                length(indices_activity_1) == length(intersection_a2_directly_after_a1)
              )
            }
          })
        })
    } else if (negation_alternate_response) {
      relation_in_path <-
        lapply(structured_path_log, function(structured_path_log) {
          sapply(structured_path_log, function(path) {
            indices_activity_1 <- which(path %in% ids_activity_1)
            indices_activity_2 <- which(path %in% ids_activity_2)
            if (length(indices_activity_1) > 0 &&
                length(indices_activity_2) > 0) {
              first_index_activity_1 <- min(indices_activity_1)
              last_index_activity_1 <- max(indices_activity_1)
              return(!(
                any(
                  indices_activity_2 > first_index_activity_1 &
                    indices_activity_2 < last_index_activity_1
                )
              ))
            } else {
              return(TRUE)
            }
            
          })
        })
    } else if (negation_alternate_precedence) {
      relation_in_path <-
        lapply(structured_path_log, function(structured_path_log) {
          sapply(structured_path_log, function(path) {
            indices_activity_1 <- which(path %in% ids_activity_1)
            indices_activity_2 <- which(path %in% ids_activity_2)
            if (length(indices_activity_1) > 0 &&
                length(indices_activity_2) > 0) {
              first_index_activity_2 <- min(indices_activity_2)
              last_index_activity_2 <- max(indices_activity_2)
              return((
                !any(
                  indices_activity_1 > first_index_activity_2 &
                    indices_activity_1 < last_index_activity_2
                )
              ))
            } else {
              return(TRUE)
            }
          })
        })
    }
    #filter all na values, because these are not relevant
    relation_in_path <-
      lapply(relation_in_path, function(relation_in_path) {
        relation_in_path <- relation_in_path[!is.na(relation_in_path)]
      })
    
    if (always) {
      #if always parameter is true, all values of relation_in_path need to be TRUE. If there are no relation in path values, FALSE is returned,
      #because then it is not the case that activity_1 is always (indirectly) followed by activity_2
      relation_in_path <-
        sapply(relation_in_path, function(relation_in_path) {
          if (length(relation_in_path) > 0)
            return(all(relation_in_path))
          else
            return(FALSE)
        })
      return(any(relation_in_path))
      
    } else
      #if always parameter is false, at least one time, there should exist at least one path in which activity_1 is not followed by activity_2 and
      #there should exist at least one path in which activity_1 is followed by activity_2
      relation_in_path <-
      sapply(relation_in_path, function(relation_in_path) {
        if (length(relation_in_path) > 0)
          return(any(relation_in_path) && any(!relation_in_path))
        else
          return(FALSE)
      })
    return(any(relation_in_path))
    
  }


#' @title Filter path log with only traces containing the parallel gateway together with the relevant activity
#'
#' @description { This functions returns a path log with no traces with a parallel gateway of which the given activity is part but not included
#' }
#' @param structured_path_log repetition and path log list object created by the function create_repetition_and_path_log
#' @param xml_internal_doc document object created using the create_internal_document function
#' @param activity_name name of the activity for the relevant filtering
#' @return the filtered path log
#' @examples \dontrun{direct_parallel_relations(repetition_and_path_log, xml_internal_doc)}
#' @export
filtered_path_log_parallel <-
  function(structured_path_log,
           xml_internal_doc,
           activity_name) {
    task_id <- NULL
    oldw <- getOption("warn")
    options(warn = -1)
    parallel_splits <-
      split_gateways(
        xml_internal_doc,
        "//bpmn:parallelGateway | //parallelGateway |
        //bpmn:task | //bpmn:sendTask | //bpmn:receiveTask | //bpmn:manualTask |
        //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
        //bpmn:subProcess | //bpmn:callActivity | //task"
      )
    
    parallel_joins <-
      join_gateways(xml_internal_doc,
                    "//bpmn:parallelGateway | //parallelGateway")
    other_splits <-
      split_gateways(
        xml_internal_doc,
        "//bpmn:exclusiveGateway | //exclusiveGateway | //bpmn:eventBasedGateway | //eventBasedGateway |
        //bpmn:inclusiveGateway | //inclusiveGateway | //bpmn:complexGateway | //complexGateway"
      )
    other_joins <-
      join_gateways(
        xml_internal_doc,
        "//bpmn:exclusiveGateway | //exclusiveGateway | //bpmn:eventBasedGateway | //eventBasedGateway |
        //bpmn:inclusiveGateway | //inclusiveGateway | //bpmn:complexGateway | //complexGateway"
      )
    
    task_ids <- task_names(xml_internal_doc) %>%
      filter(task_names == activity_name) %>%
      mutate(task_id = as.character(task_id)) %>%
      pull(task_id)
    
    parallel_gateway_with_activity_filter <-
      lapply(task_ids, function(task_id) {
        path_log_filtered_activity_id_parallel <-
          lapply(structured_path_log, function(path) {
            indices_to_filter <-
              which(
                path %in% c(
                  parallel_splits,
                  parallel_joins,
                  task_id,
                  other_splits,
                  other_joins
                )
              )
            indices_to_filter <- sort(indices_to_filter)
            return(path[indices_to_filter])
          })
        parallel_gateway_with_task_id_filter <-
          lapply(path_log_filtered_activity_id_parallel, function(path) {
            sub_path_indices <-
              which(path %in% c(parallel_splits, parallel_joins, task_id))
            sub_path <- path[sub_path_indices]
            parallel_split_indices <-
              which(sub_path %in% parallel_splits)
            parallel_join_indices <-
              which(sub_path %in% parallel_joins)
            task_indices <- which(sub_path %in% task_id)
            if (length(task_indices) > 0) {
              parallel_splits_before_task <-
                parallel_split_indices[parallel_split_indices < task_indices[1]]
              parallel_joins_before_task <-
                parallel_join_indices[parallel_join_indices < task_indices[1]]
              if (length(parallel_splits_before_task) > length(parallel_joins_before_task)) {
                sub_path_indices <- 1:length(sub_path)
                nesting_depth_path <-
                  sub_path[sub_path_indices < task_indices[1]]
                nesting_depth_path[parallel_splits_before_task] <-
                  1
                nesting_depth_path[parallel_joins_before_task] <-
                  -1
                nesting_depth_path[parallel_joins_before_task < parallel_splits_before_task[1]] <-
                  0
                nesting_depth_path <- cumsum(nesting_depth_path)
                
                indices_nesting_depth_equal_zero <-
                  which(nesting_depth_path == 0)
                if (length(indices_nesting_depth_equal_zero) == 0) {
                  index_after_last_nesting_depth_zero <- 1
                  parallel_split_task <-
                    sub_path[index_after_last_nesting_depth_zero]
                } else {
                  index_after_last_nesting_depth_zero <-
                    indices_nesting_depth_equal_zero[length(indices_nesting_depth_equal_zero)] + 1
                  parallel_split_task <-
                    sub_path[index_after_last_nesting_depth_zero]
                }
                index_parallel_split_task <-
                  which(path %in% parallel_split_task)
                task_indices <- which(path %in% task_id)
                sub_path <-
                  path[index_parallel_split_task[1]:task_indices[1]]
                sub_path_indices <-
                  which(sub_path %in% c(other_splits, other_joins))
                sub_path <- sub_path[sub_path_indices]
                other_split_indices <-
                  which(sub_path %in% other_splits)
                other_join_indices <-
                  which(sub_path %in% other_joins)
                if (length(other_split_indices) > length(other_join_indices)) {
                  nesting_depth_path <- sub_path
                  nesting_depth_path[other_split_indices] <- 1
                  nesting_depth_path[other_join_indices] <- -1
                  nesting_depth_path[other_join_indices < other_split_indices[1]] <-
                    0
                  nesting_depth_path <-
                    cumsum(nesting_depth_path)
                  indices_exclusive_nesting_depth_zero <-
                    which(nesting_depth_path == 0)
                  if (length(indices_exclusive_nesting_depth_zero) == 0) {
                    task_id <- sub_path[1]
                  } else {
                    task_id <-
                      sub_path[indices_exclusive_nesting_depth_zero[length(indices_exclusive_nesting_depth_zero)] + 1]
                  }
                }
                return(data.frame(
                  parallel_split = as.character(parallel_split_task),
                  task = as.character(task_id)
                ))
              }
            }
          })
      })
    parallel_gateway_with_activity_filter <-
      unlist(parallel_gateway_with_activity_filter, recursive = FALSE)
    parallel_gateway_with_activity_filter <-
      bind_rows(parallel_gateway_with_activity_filter)
    parallel_gateway_with_activity_filter <-
      parallel_gateway_with_activity_filter %>%
      distinct()
    if (length(parallel_gateway_with_activity_filter) > 0) {
      paths_to_filter <-
        lapply(1:length(parallel_gateway_with_activity_filter$parallel_split), function(gateway_index) {
          gateway <-
            parallel_gateway_with_activity_filter$parallel_split[gateway_index]
          activity <-
            parallel_gateway_with_activity_filter$task[gateway_index]
          path_indices_to_filter <-
            sapply(1:length(structured_path_log), function(path_index) {
              path <- structured_path_log[[path_index]]
              gateway_in_path <-
                length(which(path %in% gateway)) > 0
              activity_in_path <-
                length(which(path %in% activity)) > 0
              if (!activity_in_path & gateway_in_path)
                return(path_index)
            })
        })
      paths_to_filter <- lapply(paths_to_filter, unlist)
      
      filtered_path_log <-
        lapply(paths_to_filter, function(filter_vector) {
          lapply(1:length(structured_path_log), function(index) {
            if (!(index %in% filter_vector)) {
              return(structured_path_log[[index]])
            }
          })
        })
      filtered_path_log <-
        lapply(filtered_path_log, function(path_log) {
          return(path_log[-which(sapply(path_log, is.null))])
        })
      options(warn = oldw)
      return(filtered_path_log)
    } else {
      options(warn = oldw)
      return(list(structured_path_log))
    }
  }


#' @title Direct and parallel relations
#'
#' @description { This functions returns a table containing all direct and parallel relations between activities. The table contains five columns:
#' - the two first represent activity ids
#' - the third represents the type of relations, which is parallel or direct
#' - the last two columns are the corresponding activity names for the first two columns
#' }
#' @param repetition_and_path_log repetition and path log list object created by the function create_repetition_and_path_log
#' @param xml_internal_doc document object created using the create_internal_document function
#' @return a table as described in the description
#' @examples \dontrun{direct_parallel_relations(repetition_and_path_log, xml_internal_doc)}
#' @export

direct_parallel_relations <-
  function(repetition_and_path_log,
           xml_internal_doc) {
    oldw <- getOption("warn")
    options(warn = -1)
    second <- NULL
    number_of_occurences <- NULL
    task_id <- NULL
    sometimes <- NULL
    relation <- NULL
    first_task <- NULL
    second_task <- NULL
    X1 <- NULL
    X2 <- NULL
    exclusive <- NULL
    
    #save structured_path_log in a new variable and save all node ids in the corresponding category vector
    structured_path_log <- repetition_and_path_log[[4]]
    task_ids <- task_names(xml_internal_doc) %>%
      pull(task_id)
    parallel_ids <-
      node_ids(xml_internal_doc,
               "//bpmn:parallelGateway | //parallelGateway")
    exclusive_ids <-
      node_ids(
        xml_internal_doc,
        "//bpmn:exclusiveGateway | //exclusiveGateway | //bpmn:eventBasedGateway | //eventBasedGateway"
      )
    inclusive_ids <-
      node_ids(
        xml_internal_doc,
        "//bpmn:inclusiveGateway | //inclusiveGateway | //bpmn:complexGateway | //complexGateway"
      )
    events <-
      node_ids(
        xml_internal_doc,
        "//bpmn:startEvent | //bpmn:messageStartEvent | //bpmn:timerStartEvent |
        //bpmn:conditionalStartEvent | //bpmn:endEvent | //bpmn:messageEndEvent |
        //bpmn:terminateEndEvent | //bpmn:escalationEndEvent | //bpmn:errorEndEvent |
        //bpmn:compensationEndEvent | //bpmn:signalEndEvent | //bpmn:intermediateCatchEvent |
        //bpmn:intermediateThrowEvent | //bpmn:boundaryEvent | //startEvent | //endEvent"
      )
    special_elements <-
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
        "Other-loop-join",
        "start-join",
        "end-split"
      )
    
    #Keep only the tasks in the structured path log
    structured_path_log <-
      lapply(structured_path_log, function(path) {
        indices_element_to_keep <- which(path %in% task_ids)
        return(path[indices_element_to_keep])
      })
    
    #Save all direct relations in dataframes. However, relations of activity_1 and activity_2 with a parallel join in between are considered a direct relations as well.
    #Therefore, these should be recategorised in order that there is sometimes a direct and sometimes an indirect relation
    relations <- lapply(structured_path_log, function(path) {
      if (length(path) >= 2) {
        path_without_end <- as.character(path[1:(length(path) - 1)])
        path_without_start <- as.character(path[2:length(path)])
        relations <- cbind(path_without_end, path_without_start)
        relations <- as.data.frame(relations)
        colnames(relations) <- c("first", "second")
        relations$first <- as.character(relations$first)
        relations$second <- as.character(relations$second)
        return(relations)
      }
    })
    
    relations <- bind_rows(relations)
    #all direct relations receive the label "d_l_t_r" or "direct_left_to_right"
    direct_relations <- relations %>%
      unique() %>%
      mutate(relation = "d_l_t_r")
    if (nrow(direct_relations) > 0) {
      #to capture all relations, the reversed relations are added as well. These have the label "d_r_t_l"
      direct_relations_reversed <-
        cbind(direct_relations$second, direct_relations$first)
      direct_relations_reversed <-
        as.data.frame(direct_relations_reversed)
      colnames(direct_relations_reversed) <-
        c("first", "second")
      direct_relations_reversed$first <-
        as.character(direct_relations_reversed$first)
      direct_relations_reversed$second <-
        as.character(direct_relations_reversed$second)
      direct_relations_reversed <- direct_relations_reversed %>%
        mutate(relation = "d_r_t_l")
      all_direct_relations <-
        rbind(direct_relations, direct_relations_reversed)
    } else {
      all_direct_relations <- direct_relations %>%
        mutate(second = relation, first = relation)
    }
    
    #make a log which does not contain any exclusive gateways and special elements
    structured_path_log <- repetition_and_path_log[[4]]
    path_log_with_parallel_gateways <-
      lapply(structured_path_log, function(path) {
        indices_gateways <-
          which(path %in% c(exclusive_ids, special_elements))
        indices_all_elements <- 1:length(path)
        indices_element_to_keep <-
          setdiff(indices_all_elements, indices_gateways)
        return(path[indices_element_to_keep])
      })
    path_log_with_exclusive_gateways <-
      lapply(structured_path_log, function(path) {
        indices_gateways <-
          which(path %in% c(special_elements))
        indices_all_elements <- 1:length(path)
        indices_element_to_keep <-
          setdiff(indices_all_elements, indices_gateways)
        return(path[indices_element_to_keep])
      })
    
    #save parallel and inclusive split and joins in there corresponding vector
    parallel_splits <-
      split_gateways(
        xml_internal_doc,
        "//bpmn:parallelGateway | //parallelGateway |
        //bpmn:task | //bpmn:sendTask | //bpmn:receiveTask | //bpmn:manualTask |
        //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
        //bpmn:subProcess | //bpmn:callActivity | //task"
      )
    inclusive_splits <-
      split_gateways(
        xml_internal_doc,
        "//bpmn:inclusiveGateway | //inclusiveGateway |
        //bpmn:complexGateway | //complexGateway"
      )
    parallel_joins <-
      join_gateways(xml_internal_doc,
                    "//bpmn:parallelGateway | //parallelGateway")
    inclusive_joins <-
      join_gateways(
        xml_internal_doc,
        "//bpmn:inclusiveGateway | //inclusiveGateway |
        //bpmn:complexGateway | //complexGateway"
      )
    exclusive_splits <-
      split_gateways(
        xml_internal_doc,
        "//bpmn:exclusiveGateway | //exclusiveGateway | //bpmn:eventBasedGateway | //eventBasedGateway"
      )
    exclusive_joins <-
      join_gateways(
        xml_internal_doc,
        "//bpmn:exclusiveGateway | //exclusiveGateway | //bpmn:eventBasedGateway | //eventBasedGateway"
      )
    parallel_splits <- c(parallel_splits, inclusive_splits)
    parallel_joins <- c(parallel_joins, inclusive_joins)
    #for each parallel split
    parallel_relationships <-
      lapply(parallel_splits, function(parallel_split) {
        relevant_paths <-
          lapply(path_log_with_parallel_gateways, function(path) {
            parallel_index <- which(path %in% parallel_split)
            if (length(parallel_index) > 0)
              return(path[parallel_index[1]:length(path)])
          })
        null_indices <- which(sapply(relevant_paths, is.null))
        if (length(null_indices) > 0)
          relevant_paths <-
          relevant_paths[-null_indices]
        
        #Get all common elements for the paths containing the parallel split element
        intersection_elements <-
          reduce(relevant_paths, intersect)
        #Look for the common parallel or exclusive join in the paths
        intersection_parallel_joins <-
          which(intersection_elements %in% c(parallel_joins, inclusive_joins))
        #the first common join is the one relevant for determining which activities are in parallel
        parallel_join <-
          intersection_elements[intersection_parallel_joins[1]]
        #The activities between the split and join are saved in this variable
        sub_paths_between_split_join <-
          lapply(relevant_paths, function(path) {
            #the index of the parallel split
            index_after_parallel_split <-
              which(path == parallel_split)[1]
            #the index of the parallel join or inclusive join, if one exists, otherwise, all activities are assumed to be in parallel
            #and the index is set to the length of the path, however filter of direct and indirect relations should not be done after an exclusive join!!!
            index_before_parallel_join <-
              which(path == parallel_join)[1]
            if (is.na(index_before_parallel_join))
              index_before_parallel_join <- length(path)
            return (path[index_after_parallel_split:index_before_parallel_join])
          })
        second_elements_sub_paths <-
          sapply(sub_paths_between_split_join, function(sub_path) {
            if (length(sub_path) >= 3)
              return(sub_path[2])
          })
        second_elements_sub_paths <-
          unique(second_elements_sub_paths)
        
        elements_being_part_outgoing_flow_parallel_split <-
          lapply(second_elements_sub_paths, function(element) {
            if (length(element) > 0) {
              elements_of_path <-
                sapply(sub_paths_between_split_join, function(sub_path) {
                  if (element %in% sub_path)
                    if (length(sub_path) >= 2)
                      return(sub_path[2:length(sub_path) - 1])
                  
                })
            } else {
              elements_of_path <- character()
            }
            elements_of_path <- unique(unlist(elements_of_path))
            elements_of_path <-
              elements_of_path[elements_of_path %in% task_ids]
          })
        if (length(elements_being_part_outgoing_flow_parallel_split) > 1) {
          combinations_all_paths <-
            t(combn(
              1:length(elements_being_part_outgoing_flow_parallel_split),
              2
            ))
          combinations_elements_two_paths <-
            lapply(1:dim(combinations_all_paths)[1], function(index) {
              sub_path_1 <-
                elements_being_part_outgoing_flow_parallel_split[[combinations_all_paths[index, 1]]]
              sub_path_2 <-
                elements_being_part_outgoing_flow_parallel_split[[combinations_all_paths[index, 2]]]
              combinations_all_paths <-
                expand.grid(first = sub_path_1, second = sub_path_2)
            })
          combinations_elements_two_paths <-
            bind_rows(combinations_elements_two_paths)
          combinations_elements_two_paths_reversed <-
            data.frame(
              first = as.character(combinations_elements_two_paths$second),
              second = as.character(combinations_elements_two_paths$first)
            )
          combinations_elements_two_paths <-
            rbind(combinations_elements_two_paths,
                  combinations_elements_two_paths_reversed)
          combinations_elements_two_paths <-
            unique(combinations_elements_two_paths)
          combinations_elements_two_paths$first <-
            as.character(combinations_elements_two_paths$first)
          combinations_elements_two_paths$second <-
            as.character(combinations_elements_two_paths$second)
          combinations_elements_two_paths <-
            combinations_elements_two_paths
          #     combinations_elements_two_paths <- rbind(combinations_elements_two_paths, direct_exclusive_relationships)
          return(combinations_elements_two_paths)
        } else {
          return(data.frame(
            first = character(),
            second = character(),
            exclusive = logical()
          ))
        }
        
      })
    parallel_relationships <- bind_rows(parallel_relationships)
    direct_relations_part_parallel_gateway <-
      lapply(parallel_splits, function(parallel_split) {
        relevant_paths <-
          lapply(path_log_with_parallel_gateways, function(path) {
            parallel_index <- which(path %in% parallel_split)
            if (length(parallel_index) > 0)
              return(path[parallel_index[1]:length(path)])
          })
        null_indices <- which(sapply(relevant_paths, is.null))
        if (length(null_indices) > 0)
          relevant_paths <-
          relevant_paths[-null_indices]
        
        #Get all common elements for the paths containing the parallel split element
        intersection_elements <-
          reduce(relevant_paths, intersect)
        #Look for the common parallel or exclusive join in the paths
        intersection_parallel_joins <-
          which(intersection_elements %in% c(parallel_joins, inclusive_joins))
        #the first common join is the one relevant for determining which activities are in parallel
        parallel_join <-
          intersection_elements[intersection_parallel_joins[1]]
        #The activities between the split and join are saved in this variable
        sub_paths_between_split_join <-
          lapply(relevant_paths, function(path) {
            #the index of the parallel split
            index_after_parallel_split <-
              which(path == parallel_split)[1]
            #the index of the parallel join or inclusive join, if one exists, otherwise, all activities are assumed to be in parallel
            #and the index is set to the length of the path, however filter of direct and indirect relations should not be done after an exclusive join!!!
            index_before_parallel_join <-
              which(path == parallel_join)[1]
            if (is.na(index_before_parallel_join))
              index_before_parallel_join <- length(path)
            return (path[index_after_parallel_split:index_before_parallel_join])
          })
        sub_paths_between_split_join <-
          unique(sub_paths_between_split_join)
        direct_relations_part_parallel_gateway <-
          lapply(sub_paths_between_split_join, function(path) {
            path <- path[path %in% task_ids]
            if (length(path) > 1) {
              first <- path[1:length(path) - 1]
              second <- path[2:length(path)]
              direct_relations <-
                data.frame(first = as.character(first), second = as.character(second))
              direct_relations <- direct_relations %>%
                mutate(relation = "d_l_t_r")
              direct_relations_reversed <-
                data.frame(first = as.character(second), second = as.character(first))
              direct_relations_reversed <-
                direct_relations_reversed %>%
                mutate(relation = "d_r_t_l")
              direct_relations <-
                rbind(direct_relations, direct_relations_reversed)
              return(direct_relations)
            } else {
              return(data.frame(
                first = character(),
                second = character(),
                relation = character()
              ))
            }
          })
        direct_relations_part_parallel_gateway <-
          bind_rows(direct_relations_part_parallel_gateway)
        
      })
    direct_relations_part_parallel_gateway <-
      bind_rows(direct_relations_part_parallel_gateway)
    direct_relations_part_parallel_gateway <-
      direct_relations_part_parallel_gateway %>%
      mutate(sometimes = TRUE)
    exclusive_relationships <-
      lapply(exclusive_splits, function(exclusive_split) {
        #create a vector with all indexes of the path (numbers of path)
        path_indices <-
          1:length(path_log_with_exclusive_gateways)
        #create a vector which contains TRUE when the path contains the relevant parallel split element
        path_indices_selector <-
          sapply(path_indices, function(path_index) {
            return(exclusive_split %in% path_log_with_exclusive_gateways[[path_index]])
          })
        #keep only the paths with the relevant parallel split element
        relevant_paths <-
          path_log_with_exclusive_gateways[path_indices_selector]
        #Get all common elements for the paths containing the parallel split element
        intersection_elements <-
          reduce(relevant_paths, intersect)
        #Look for the common parallel or exclusive join in the paths
        intersection_exclusive_joins <-
          which(intersection_elements %in% exclusive_joins)
        #the first common join is the one relevant for determining which activities are in parallel
        exclusive_join <-
          intersection_elements[intersection_exclusive_joins[1]]
        #The activities between the split and join are saved in this variable
        sub_paths_between_split_join <-
          lapply(relevant_paths, function(path) {
            #the index of the parallel split
            index_after_exclusive_split <-
              which(path == exclusive_split)[1]
            #the index of the parallel join or inclusive join, if one exists, otherwise, all activities are assumed to be in parallel
            #and the index is set to the length of the path, however filter of direct and indirect relations should not be done after an exclusive join!!!
            index_before_exclusive_join <-
              which(path == exclusive_join)[1]
            if (is.na(index_before_exclusive_join))
              index_before_exclusive_join <- length(path)
            sub_path <-
              as.character(path[index_after_exclusive_split:index_before_exclusive_join])
            return (sub_path)
          })
        
        second_elements_sub_paths <-
          sapply(sub_paths_between_split_join, function(sub_path) {
            if (length(sub_path) >= 3)
              return(sub_path[2])
          })
        second_elements_sub_paths <-
          unique(second_elements_sub_paths)
        
        elements_being_part_outgoing_flow_exclusive_split <-
          lapply(second_elements_sub_paths, function(element) {
            if (length(element) > 0) {
              elements_of_path <-
                sapply(sub_paths_between_split_join, function(sub_path) {
                  if (element %in% sub_path)
                    if (length(sub_path) >= 2)
                      return(sub_path[2:length(sub_path) - 1])
                  
                })
            } else {
              elements_of_path <- character()
            }
            elements_of_path <- unique(unlist(elements_of_path))
            elements_of_path <-
              elements_of_path[elements_of_path %in% task_ids]
          })
        if (length(elements_being_part_outgoing_flow_exclusive_split) > 1) {
          combinations_all_paths <-
            t(combn(
              1:length(elements_being_part_outgoing_flow_exclusive_split),
              2
            ))
          combinations_elements_two_paths <-
            lapply(1:dim(combinations_all_paths)[1], function(index) {
              sub_path_1 <-
                elements_being_part_outgoing_flow_exclusive_split[[combinations_all_paths[index, 1]]]
              sub_path_2 <-
                elements_being_part_outgoing_flow_exclusive_split[[combinations_all_paths[index, 2]]]
              combinations_all_paths <-
                expand.grid(first = sub_path_1, second = sub_path_2)
            })
          combinations_elements_two_paths <-
            bind_rows(combinations_elements_two_paths)
          combinations_elements_two_paths_reversed <-
            data.frame(
              first = as.character(combinations_elements_two_paths$second),
              second = as.character(combinations_elements_two_paths$first)
            )
          combinations_elements_two_paths <-
            rbind(combinations_elements_two_paths,
                  combinations_elements_two_paths_reversed)
          combinations_elements_two_paths <-
            unique(combinations_elements_two_paths)
          combinations_elements_two_paths$first <-
            as.character(combinations_elements_two_paths$first)
          combinations_elements_two_paths$second <-
            as.character(combinations_elements_two_paths$second)
          combinations_elements_two_paths <-
            combinations_elements_two_paths %>%
            mutate(exclusive = TRUE)
          #     combinations_elements_two_paths <- rbind(combinations_elements_two_paths, direct_exclusive_relationships)
          return(combinations_elements_two_paths)
        } else {
          return(data.frame(
            first = character(),
            second = character(),
            exclusive = logical()
          ))
        }
      })
    
    exclusive_relationships <-
      bind_rows(exclusive_relationships)
    if (length(exclusive_relationships) != 0) {
      exclusive_relationships_filter <- exclusive_relationships %>%
        select(first, second)
      if (length(parallel_relationships) > 0) {
        interleaving_relations <-
          setdiff(parallel_relationships,
                  exclusive_relationships_filter)
      }
      else {
        interleaving_relations <-
          data.frame(first = character(),
                     second = character(),
                     relation = character())
      }
    } else {
      interleaving_relations <- parallel_relationships
    }
    interleaving_relations <- interleaving_relations %>%
      mutate(relation = "parallel")
    
    direct_parallel_relations <-
      rbind(interleaving_relations, all_direct_relations)
    if (nrow(direct_relations_part_parallel_gateway) > 0) {
      direct_parallel_relations <-
        left_join(
          direct_parallel_relations,
          direct_relations_part_parallel_gateway,
          by = c("first", "second", "relation")
        )
      direct_parallel_relations$sometimes[is.na(direct_parallel_relations$sometimes)] <-
        FALSE
      direct_parallel_relations <- direct_parallel_relations %>%
        mutate(relation = if_else(sometimes, paste("s_", relation, sep = ""), relation)) %>%
        select(first, second, relation)
    }
    
    
    
    #add the task names as columns with the column names first_task and second_task
    all_tasks <- task_names(xml_internal_doc)
    all_tasks$task_id <- as.character(all_tasks$task_id)
    all_tasks$task_names <- as.character(all_tasks$task_names)
    direct_parallel_relations <-
      left_join(direct_parallel_relations,
                all_tasks,
                by = c("first" = "task_id"))
    direct_parallel_relations <-
      left_join(direct_parallel_relations,
                all_tasks,
                by = c("second" = "task_id"))
    colnames(direct_parallel_relations) <-
      c("first", "second", "relation", "first_task", "second_task")
    
    #filter the relations which have a parallel join gateway in between the two activities, because these relations are sometimes direct and sometimes indirect
    #the assigned relation is s_d_l_t_r or
    relations_parallel_join <-
      relations_with_parallel_join_split(repetition_and_path_log, xml_internal_doc)
    direct_parallel_relations <-
      left_join(direct_parallel_relations,
                relations_parallel_join,
                by = c("first", "second"))
    direct_parallel_relations$sometimes[is.na(direct_parallel_relations$sometimes)] <-
      FALSE
    direct_parallel_relations <- direct_parallel_relations %>%
      mutate(
        relation = if_else(
          sometimes &
            relation != "s_d_l_t_r"  &
            relation != "s_d_r_t_l",
          paste("s_", relation, sep = ""),
          relation
        )
      ) %>%
      select(first, second, relation, first_task, second_task) %>%
      filter(!is.na(first_task) & !is.na(second_task)) %>%
      distinct()
    
    options(warn = oldw)
    return(direct_parallel_relations)
  }

#returns a data frame with all relations of activities which contain a parallel join
relations_with_parallel_join_split <-
  function(repetition_and_path_log,
           xml_internal_doc) {
    task_id <- NULL
    
    #get all parallel and inclusive joins
    parallel_joins <-
      join_gateways(xml_internal_doc,
                    "//bpmn:parallelGateway | //parallelGateway")
    parallel_splits <-
      split_gateways(xml_internal_doc,
                     "//bpmn:parallelGateway | //parallelGateway")
    inclusive_joins <-
      join_gateways(
        xml_internal_doc,
        "//bpmn:inclusiveGateway | //inclusiveGateway |
        //bpmn:complexGateway | //complexGateway"
      )
    inclusive_splits <-
      split_gateways(
        xml_internal_doc,
        "//bpmn:inclusiveGateway | //inclusiveGateway |
        //bpmn:complexGateway | //complexGateway"
      )
    #get all task ids
    task_ids <- task_names(xml_internal_doc) %>%
      pull(task_id)
    
    structured_path_log <- repetition_and_path_log[[4]]
    #filter relevant relations (for each path)
    relevant_relations <-
      lapply(structured_path_log, function(path) {
        #get indices of parallel joins and activities
        parallel_join_indices <-
          which(path %in% c(
            parallel_joins,
            inclusive_joins,
            parallel_splits,
            inclusive_splits
          ))
        activity_indices <- which(path %in% task_ids)
        #retrieve activity id before parallel_join
        activities_before_parallel_join <-
          sapply(parallel_join_indices, function(index) {
            activity_indices_before_join <-
              activity_indices[activity_indices < index]
            last_activity_before_join <-
              path[activity_indices_before_join[length(activity_indices_before_join)]]
            return(as.character(last_activity_before_join))
          })
        #retrieve activity id after parallel_join
        activities_after_parallel_join <-
          sapply(parallel_join_indices, function(index) {
            activity_indices_after_join <-
              activity_indices[activity_indices > index]
            last_activity_after_join <-
              path[activity_indices_after_join[1]]
            return(as.character(last_activity_after_join))
          })
        #make data frame with relation
        relations <-
          as.data.frame(cbind(
            first = as.character(activities_before_parallel_join),
            second = as.character(activities_after_parallel_join)
          ))
        relations$first <- as.character(relations$first)
        relations$second <- as.character(relations$second)
        return (relations)
      })
    
    #bind rows of all objects in list, such that one data frame is made and add sometimes = TRUE
    relevant_relations <- bind_rows(relevant_relations) %>%
      distinct() %>%
      mutate(sometimes = TRUE)
    
    #add reversed relations as well and return data frame
    relevant_relations_reversed <-
      data_frame(
        first = relevant_relations$second,
        second = relevant_relations$first,
        sometimes = TRUE
      )
    relevant_relations <-
      rbind(relevant_relations, relevant_relations_reversed)
    return(relevant_relations)
  }
