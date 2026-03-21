#include <Rcpp.h>
using namespace Rcpp;

NumericVector whichRccp(CharacterVector vector_for_searching, String match) {
  NumericVector matches;
  for(int i = 0; i < vector_for_searching.size(); i++) {
    if(vector_for_searching[i] == match) {
      matches.push_back(i);
    }
  }
  return(matches);
}

int whichIndex(CharacterVector vector_for_searching, String match) {
  int matchIndex = -1;
  for(int i = 0; matchIndex == -1 && i < vector_for_searching.size(); i++) {
    if(vector_for_searching[i] == match) {
      matchIndex = i;
    }
  }
  return(matchIndex);
}

CharacterVector subsetVector(CharacterVector vector_to_subset, NumericVector indices) {
  CharacterVector subsettedVector;
  for(int i = 0; i < indices.size(); i++) {
    int index = indices[i];
    subsettedVector.push_back(vector_to_subset[index]);
  }
  return(subsettedVector);
}

NumericVector subsetNumericVector(NumericVector vector_to_subset, NumericVector indices) {
  NumericVector subsettedVector;
  for(int i = 0; i < indices.size(); i++) {
    int index = indices[i];
    subsettedVector.push_back(vector_to_subset[index]);
  }
  return(subsettedVector);
}

List searchLoop(CharacterVector path, CharacterVector next_artifacts) {
  CharacterVector entire_loop_till_join;
  CharacterVector entire_loop_till_split;
  CharacterVector next_artifacts_filtered;
  bool loop_found = false;
  String first_element = "no loop found";
  NumericVector indices_split = whichRccp(path, "split");
  NumericVector indices_join = whichRccp(path, "join");
  int loop_index_join_2 = indices_join.size() - 1;
  int index_join_2 = indices_join[loop_index_join_2];
  NumericVector split_before_join_2 = indices_split[indices_split < index_join_2];
  int loop_index_split_1 = split_before_join_2.size() - 1;
  for(int i = next_artifacts.size() - 1; i >= 0; i--) {
    String next_artifact = next_artifacts[i];
    int number_occurences = 0;
    for(int j = 0; j < path.size(); j++) {
      if(next_artifact == path[j]) {
        number_occurences++;
      }
    }
    if(number_occurences < 1) {
      next_artifacts_filtered.push_back(next_artifact);
    }
  }
  while(loop_index_join_2 > 0 && !loop_found) {
    while(loop_index_split_1 >= 0 && !loop_found) {
      int loop_length = path.size() - 1 - index_join_2;
      int index_join_1 = indices_split[loop_index_split_1] - 1 - loop_length;
      if(index_join_1 >= 0) {
        CharacterVector first_repetition = path[seq(index_join_1, indices_split[loop_index_split_1] - 1)];
        CharacterVector second_repetition = path[seq(index_join_2, path.size() - 1)];
        if(first_repetition.size() > 2) {
          loop_found = is_true(all(first_repetition == second_repetition));
          if(loop_found) {
            entire_loop_till_join = path[seq(index_join_1 - 1, index_join_2)];
            entire_loop_till_split = path[seq(indices_split[loop_index_split_1] - 1, path.size() - 1)];
          }
        }
      }
      loop_index_split_1 = loop_index_split_1 - 1;
    }
    loop_index_join_2 = loop_index_join_2 - 1;
    index_join_2 = indices_join[loop_index_join_2];
    split_before_join_2 = indices_split[indices_split < index_join_2];
    loop_index_split_1 = split_before_join_2.size() - 1;
  }

  if(loop_found) {
    return (List::create(Named("first_element") = next_artifacts_filtered,
    Named("entire_loop_till_join") = entire_loop_till_join,
    Named("entire_loop_till_split") = entire_loop_till_split));
  } else {
    return (List::create(next_artifacts_filtered));
  }
}

// [[Rcpp::export]]

List createPathLog(List path_log, DataFrame relations) {
  int i = 0;
  List repetitions = List::create();
  List repetitions_till_split = List::create();
  //For all start_events (and actually all paths)
  while(i < path_log.size()) {
    //Repeat until the end of a path is reached
    while(true) {
      // Check current artifact (last element in the current path)
      CharacterVector current_path = path_log[i];
      String current_artifact = current_path[current_path.size() - 1];

      // Identify any joins and splits related to the current artifact
      int number_in_flows_current_artifact = whichRccp(relations[2], current_artifact).size();

      // Identify all linked artifacts which follow the current artifact
      NumericVector indices_source_seq_flow = whichRccp(relations[1], current_artifact);
      CharacterVector next_artifacts = subsetVector(relations[2], indices_source_seq_flow);
      int number_out_flows_current_artifact = next_artifacts.size();


      bool join_before_artifact = number_in_flows_current_artifact >= 2;
      bool split_after_artifact = number_out_flows_current_artifact >= 2;

      // if a join was identified (more incoming activities linked to current activity), add "join" element
      if(join_before_artifact) {
        current_path.push_back("join");
      }
      // if there are any artifacts after current artifact, the end of the path is not reached and elements should be added
      if(next_artifacts.size() != 0) {
        // if there is more than one artifact which follows the current artifac
        CharacterVector first_part_path = clone(current_path);
        if(split_after_artifact) {
          // Save the current path in a variable
          //Check for repetitions (loops). This only possible with a split
          List loop_found = searchLoop(first_part_path, next_artifacts);
          next_artifacts = loop_found[0];
          if(next_artifacts.size() == 0) {
            break;
          }
          //If a loop is found, the first element of the loop is removed from the naxt_artifacts list and the repetition
          //is added to the repetitions lists
            //IntegerVector index_loop_found_element = match(first_element, next_artifacts);
            //int index_loop_element = index_loop_found_element[0] - 1;
            //next_artifacts.erase(index_loop_element);
          if(loop_found.size() == 3) {
            repetitions.push_back(loop_found[1]);
            repetitions_till_split.push_back(loop_found[2]);
          }
          // The element split is added and the first element in next artifact is added to the path. For the other
          // next artifacts, a path is added to the path log containing the first part and an artifact of next artifacts
          for(int j = 0; j < next_artifacts.size(); j++) {
            if(j == 0) {
              current_path.push_back("split");
              String new_artifact = next_artifacts[j];
              current_path.push_back(new_artifact);

            } else {
              CharacterVector new_path = first_part_path;
              String new_artifact = next_artifacts[j];
              new_path.push_back("split");
              new_path.push_back(new_artifact);
              path_log.push_back(new_path);
            }
          }
        }
        //If there is one next artifact, just add it to the path log
        else {
          NumericVector indices_split = whichRccp(current_path, "split");
          NumericVector indices_join = whichRccp(current_path, "join");
          String new_artifact = next_artifacts[0];
          if(indices_split.size() + 5 < indices_join.size()) {
            break;
          } else {
          current_path.push_back(new_artifact);
          }
        }
        path_log[i] = current_path;
      } else {
        // If there are no next artifacts, stop the searching for next artifacts and continue to the next path in the loop
        break;
      }
    }
    i = i + 1;
  }
  List path_repetition_log = List::create(path_log, repetitions, repetitions_till_split);
  return(path_repetition_log);
}



// [[Rcpp::export]]
DataFrame valueOfConnectionPaths(List path_log, DataFrame valueNodes) {
  CharacterVector node_ids = valueNodes[0];
  NumericVector node_values = valueNodes[3];
  CharacterVector start_nodes;
  CharacterVector end_nodes;
  List subPaths;
  NumericVector values;
  int numberDuplicates = 0;
  int numberSubPaths = 0;
  for(int m = 0; m < path_log.size(); m++) {
    CharacterVector path = path_log[m];
    int i = 0;
    int j = 1;
    while(i <= path.size() - 2) {
      CharacterVector subPathToInvestigate = CharacterVector::create(path[i]);
      while(j <= path.size() - 1) {
        numberSubPaths++;
        subPathToInvestigate.push_back(path[j]);
        bool subPathInList = false;
        for(int k = 0; !subPathInList && k < subPaths.length(); k++) {
          CharacterVector subPath = subPaths[k];
          if(subPath.size() == subPathToInvestigate.size() && is_true(all(subPath == subPathToInvestigate))) {
            subPathInList = true;
          }
        }
        if(!subPathInList) {
        String start_node = path[i];
        String end_node = path[j];
        double value = 1;
        for(int k = i; k <= j; k++) {
          int node_index = whichIndex(node_ids,path[k]);
          double node_value;
          if(node_index == -1) {
             node_value = 1;
          } else {
            node_value = node_values[node_index];
          }
          value = value * node_value;
        }
        start_nodes.push_back(start_node);
        end_nodes.push_back(end_node);
        values.push_back(value);
        subPaths.push_back(subPathToInvestigate);
        } else {
          numberDuplicates++;
        }
        j = j + 1;
      }
      i = i + 1;
      j = i + 1;
    }
    if(path.size() == 1) {
      int node_index = whichIndex(node_ids,path[0]);
      double value;
      if(node_index == -1) {
        value = 1;
      } else {
        value = node_values[node_index];
      }
      String start_node = path[0];
      String end_node = path[0];
      start_nodes.push_back(start_node);
      end_nodes.push_back(end_node);
      values.push_back(value);
    }
    //Rcpp::Rcout << m << " of " << path_log.size() << " done"<<std::endl;
  }
  //Rcpp::Rcout << "number duplicates " << numberDuplicates <<std::endl;
  //Rcpp::Rcout << "number subpaths " << numberSubPaths <<std::endl;
  DataFrame result = DataFrame::create(Named("start") = start_nodes, 
                                       Named("end") = end_nodes,
                                       Named("values") = values);
  return(result);
}


