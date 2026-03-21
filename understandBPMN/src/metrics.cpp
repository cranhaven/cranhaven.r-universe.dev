#include <Rcpp.h>
using namespace Rcpp;

IntegerVector searchJoinLoopSplits(CharacterVector path) {
  IntegerVector matches;
  if(path.size() > 0) {
  for(int i = 0; i < path.size(); i++) {
    if(path[i] == "AND-join" || path[i] == "XOR-join" || path[i] == "OR-join" || path[i] == "XOR-loop-split" || path[i] == "Other-loop-split")
    matches.push_back(i);
  }
  }
  return(matches);
}

LogicalVector stringInVector(CharacterVector vector, String string) {
  bool vectorInString = false;
  for(int i = 0; i < vector.length() && !vectorInString; i++) {
    vectorInString = vectorInString || string == vector[i];
  }
  return(LogicalVector::create(vectorInString));
}


// [[Rcpp::export]]
CharacterVector unstructuredElements(List pathLog, CharacterVector join_elements_and_loop_split) {
  CharacterVector unstructuredElements;
  CharacterVector trivialConstructs = CharacterVector::create("AND-split", "AND-join", "OR-split", "OR-join", "XOR-join",
  "XOR-split", "XOR-loop-split", "XOR-loop-join", "Other-loop-split",
  "Other-loop-join");
  for(int i = 0; i < pathLog.size(); i++) {
    CharacterVector path = pathLog[i];
    IntegerVector indices_join_loop_splits = searchJoinLoopSplits(path);
    while(indices_join_loop_splits.size() > 0) {
      int first_index_join_loop_split = indices_join_loop_splits[0];
      String element_first_join_loop_split = path[first_index_join_loop_split];
      if(first_index_join_loop_split <= 2) {
        for(int i = first_index_join_loop_split; i >= 0; i--) {
          if(path[i] != "XOR-loop-split" && path[i] != "Other-loop-split" && path[i] != "OR-join" && path[i] != "XOR-join" && path[i] != "AND-join" &&
             path[i] != "XOR-loop-join" && path[i] != "Other-loop-join" && path[i] != "OR-split" && path[i] != "XOR-split" && path[i] != "AND-split") {
            unstructuredElements.push_back(path[i]);
          }
          path.erase(i);
        }
        indices_join_loop_splits = searchJoinLoopSplits(path);
      } else {
      String expected_element_before;
      if(element_first_join_loop_split == "XOR-loop-split")
      expected_element_before = "XOR-loop-join";
      else if (element_first_join_loop_split  == "Other-loop-split")
      expected_element_before = "Other-loop-join";
      else if (element_first_join_loop_split == "XOR-join")
      expected_element_before = "XOR-split";
      else if (element_first_join_loop_split == "AND-join")
      expected_element_before = "AND-split";
      else if (element_first_join_loop_split == "XOR-join")
      expected_element_before = "OR-split";

      if(path[first_index_join_loop_split] == "Other-loop-split") {
        unstructuredElements.push_back(path[first_index_join_loop_split - 1]);
      }

      if(path[first_index_join_loop_split - 2] == "Other-loop-join") {
        unstructuredElements.push_back(path[first_index_join_loop_split - 3]);
      }

      if(expected_element_before != path[first_index_join_loop_split - 2]) {
        unstructuredElements.push_back(path[first_index_join_loop_split - 1]);
        unstructuredElements.push_back(path[first_index_join_loop_split - 3]);
      }

      if(first_index_join_loop_split + 1 < path.length()) {
        LogicalVector stringPartOfVector = stringInVector(trivialConstructs, path[first_index_join_loop_split + 1]);
        bool stringPart = stringPartOfVector[0];
        if(!stringPart) {
          path.erase(first_index_join_loop_split);
          path.erase(first_index_join_loop_split - 1);
        } else {
          path.erase(first_index_join_loop_split);
        }
      } else {
        path.erase(first_index_join_loop_split);
        path.erase(first_index_join_loop_split - 1);
      }
      path.erase(first_index_join_loop_split - 2);
      path.erase(first_index_join_loop_split - 3);
      }
      indices_join_loop_splits = searchJoinLoopSplits(path);
      } 
      
    if(path.length() > 0) {
      for(int i = 0; i < path.length(); i++) {
        if(path[i] != "XOR-loop-split" && path[i] != "Other-loop-split" && path[i] != "OR-join" && path[i] != "XOR-join" && path[i] != "AND-join" &&
           path[i] != "XOR-loop-join" && path[i] != "Other-loop-join" && path[i] != "OR-split" && path[i] != "XOR-split" && path[i] != "AND-split") {
          unstructuredElements.push_back(path[i]);
        }
      }
    }
  }
  return(unstructuredElements);
}

// [[Rcpp::export]]
double maxV(NumericVector vector) {
  return(max(vector));
}
