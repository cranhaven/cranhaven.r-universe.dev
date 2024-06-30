#include <Rcpp.h>

//' find_model_name
//'
//' checks for a model name in the syntax
//' @param syntax lavaan like syntax
//' @return vector with (1) model name and (2) model syntax
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List find_model_name(const std::string& syntax){

   unsigned int loc{0};
   int n_equals{0}; // counts the number of equal signs in a line.
   unsigned int name_start{0}, name_end{0};
   bool found_start = false;

   for(char c: syntax){
     if(!found_start){
       // search for = signs
       switch(c){
       case '=':
         n_equals++;
         break;
       default:
         // reset
         n_equals = 0;
       break;
       }
     }else{
       // search for name end
       if(c == '\n'){
         name_end = loc;
         break;
       }
       loc++;
       continue;
     }

     if(n_equals >= 3){
       found_start = true;
       name_start  = loc - n_equals;
     }

     loc++;
   }

   std::string model_name;
   if(found_start && (name_end < syntax.size()) && (name_end > name_start)){
     std::string model_name_full = {syntax.begin() + name_start,
                                    syntax.begin() + name_end};
     for(char c: model_name_full){
       switch(c){
       case ' ':
         break;
       case '\n':
         break;
       case '=':
         break;
       default:
         model_name += c;
       }
     }
   }

   std::string model_syntax = {syntax.begin() + name_end, syntax.end()};

   if(model_syntax.size() == 0){
     Rcpp::stop("Found no model in your syntax.");
   }

   return(Rcpp::List::create(
       Rcpp::Named("model_name") = model_name,
       Rcpp::Named("model_syntax") = model_syntax
   ));

 }
