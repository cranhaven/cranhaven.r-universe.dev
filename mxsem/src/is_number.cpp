#include "string_operations.h"
#include <cctype>

bool is_number(const std::string& str){

  if(str.size() <= 0)
    return(false);

  // check if there are multiple dots in the string
  bool has_dot = false;
  // counter to distinguish first from later elements
  // because the first one can be am minus sign:
  int counter = 0;
  for(char c: str){
    if((counter == 0) &&
       (c == '-')){
      if(str.size() <= 1)
        return(false);
      counter++;
      continue;
    }
    if(c == '.'){
      // if there already is a dot, it is not a number
      if(has_dot)
        return(false);
      has_dot = true;
      counter++;
      continue;
    }
    if(isdigit(c)){
      counter++;
      continue;
    }

    return(false);
  }
  return(true);
}
