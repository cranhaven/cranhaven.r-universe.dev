#include <Rcpp.h>
#include "string_operations.h"
#include "clean_syntax.h"
#include "check_syntax.h"
#include "parameter_table.h"
#include "create_algebras.h"
#include "add_elements.h"
#include "find_variables.h"
#include "scale_latent_variables.h"

void add_user_defined(const std::vector<std::string>& equations,
                      parameter_table& pt){
  for(std::string eq: equations){

    // if this is a user specified special element in curly braces, we
    // add it to the parameter table
    if(eq[0] == '{'){
      pt.user_defined.push_back(eq);
      }

  }
}

void add_effects(const std::vector<std::string>& equations,
                 parameter_table& pt){

  equation_elements eq_elem;

  for(std::string eq: equations){

    // if this is a user specified special element in curly braces, we skip the
    // rest
    if(eq[0] == '{')
      continue;

    std::vector<std::string> check_for = {"=~", "~~", "~"};

    for(std::string c_for: check_for){

      if(eq.find(c_for) != std::string::npos){

        check_equation(eq);

        eq_elem = split_string_once(eq, c_for);

        check_lhs(eq_elem.lhs);

        std::vector<str_rhs_elem> rhs_elems = split_eqation_rhs(eq_elem.rhs);

        for(auto& rhs_elem: rhs_elems){

          pt.add_line();

          pt.lhs.at(pt.lhs.size()-1) = eq_elem.lhs;
          pt.modifier.at(pt.lhs.size()-1) = rhs_elem.modifier;
          pt.op.at(pt.lhs.size()-1) = c_for;
          pt.rhs.at(pt.lhs.size()-1) = rhs_elem.rhs;

        }
        // we don't check the subsequent elements
        break;
      }

    }
  }
}

void add_bounds(const std::vector<std::string>& equations,
                parameter_table& pt){

  equation_elements eq_elem;
  // we now check for bounds. These should be added to parameters which is
  // why we first looked for the loadings, etc.
  for(std::string eq: equations){

    // if this is a user specified special element in curly braces, we skip the
    // rest
    if(eq[0] == '{')
      continue;

    std::vector<std::string> check_for = {">", "<"};

    for(std::string c_for: check_for){

      if(eq.find(c_for) != std::string::npos){

        equation_elements eq_elem = split_string_once(eq, c_for);

        bool was_found = false;
        for(unsigned int i = 0; i < pt.modifier.size(); i++){

          if(pt.modifier.at(i).compare(eq_elem.lhs) == 0){
            was_found = true;
            if(c_for.compare(">") == 0)
              pt.lbound.at(i) = eq_elem.rhs;
            if(c_for.compare("<") == 0)
              pt.ubound.at(i) = eq_elem.rhs;
          }

        }

        if(!was_found)
          Rcpp::stop("Found a constraint on the following parameter: " + eq_elem.lhs +
            ", but could not find this parameter in your model.");
      }
    }

  }
}

std::string remove_outer_braces(const std::string str){

  if((str[0] != '{') || (str[str.size()-1] != '}')){
    Rcpp::stop( str + " has unbalanced curly braces");
  }

  return(str.substr(1, str.size() - 2));
}

bool pt_remove_outer_braces(parameter_table& pt){

  bool has_curly = false;

  for(unsigned int i = 0; i < pt.lhs.size(); i++){

    // check lhs
    if(pt.lhs.at(i)[0] == '{'){
      has_curly = true;
      pt.lhs.at(i) = remove_outer_braces(pt.lhs.at(i));
    }
    if(pt.rhs.at(i)[0] == '{'){
      has_curly = true;
      pt.rhs.at(i) = remove_outer_braces(pt.rhs.at(i));
    }
    if(pt.modifier.at(i)[0] == '{'){
      has_curly = true;
      pt.modifier.at(i) = remove_outer_braces(pt.modifier.at(i));
    }
  }

  return(has_curly);
}


parameter_table make_parameter_table(const std::string& syntax,
                                     bool add_intercept,
                                     bool add_variance,
                                     bool add_exogenous_latent_covariances,
                                     bool add_exogenous_manifest_covariances,
                                     bool scale_latent_variance,
                                     bool scale_loading){

  const std::vector<std::string> equations = clean_syntax(syntax);

  check_cleaned(equations);

  parameter_table pt;

  add_user_defined(equations, pt);

  add_effects(equations, pt);

  add_bounds(equations, pt);

  // Now add transformations (mxAlgebra)
  make_algebras(equations,
                pt);

  // clean user defined elements: remove outer braces
  // bool has_curly = pt_remove_outer_braces(pt);
  // if(has_curly)
  //   Rcpp::warning("Found curly braces in the model syntax. This is extremely experimental and highly discouraged! Please make sure to thoroughly check the model returend by mxsem!");

  pt.vars = find_variables(pt);

  // automatically add some elements:
  if(add_variance)
    add_variances(pt);
  if(add_intercept)
    add_intercepts(pt);

  if(add_exogenous_latent_covariances)
    add_covariances(pt.vars.latents, pt);
  if(add_exogenous_manifest_covariances)
    add_covariances(pt.vars.manifests, pt);

  if(scale_latent_variance)
    scale_latent_variances(pt);
  if(scale_loading)
    scale_loadings(pt);

  return(pt);
}

//' parameter_table_rcpp
//'
//' creates a parameter table from a lavaan like syntax
//' @param syntax lavaan like syntax
//' @param add_intercept should intercepts for manifest variables be automatically added?
//' @param add_variance should variances for all variables be automatically added?
//' @param add_exogenous_latent_covariances should covariances between exogenous latent variables be
//' added automatically?
//' @param add_exogenous_manifest_covariances should covariances between exogenous manifest variables be
//' added automatically?
//' @param scale_latent_variance should variances of latent variables be set to 1?
//' @param scale_loading should the first loading of each latent variable be set to 1?
//' @return parameter table
// [[Rcpp::export]]
Rcpp::List parameter_table_rcpp(const std::string& syntax,
                               bool add_intercept,
                               bool add_variance,
                               bool add_exogenous_latent_covariances,
                               bool add_exogenous_manifest_covariances,
                               bool scale_latent_variance,
                               bool scale_loading){
   parameter_table pt = make_parameter_table(syntax,
                                             add_intercept,
                                             add_variance,
                                             add_exogenous_latent_covariances,
                                             add_exogenous_manifest_covariances,
                                             scale_latent_variance,
                                             scale_loading);

   Rcpp::DataFrame pt_Rcpp = Rcpp::DataFrame::create(Rcpp::Named("lhs") = pt.lhs,
                                                     Rcpp::Named("op") = pt.op,
                                                     Rcpp::Named("rhs") = pt.rhs,
                                                     Rcpp::Named("modifier") = pt.modifier,
                                                     Rcpp::Named("lbound") = pt.lbound,
                                                     Rcpp::Named("ubound") = pt.ubound,
                                                     Rcpp::Named("free") = pt.free);
   Rcpp::DataFrame pt_algebras = Rcpp::DataFrame::create(Rcpp::Named("lhs") = pt.alg.lhs,
                                                         Rcpp::Named("op") = pt.alg.op,
                                                         Rcpp::Named("rhs") = pt.alg.rhs);
   Rcpp::List pt_variables = Rcpp::List::create(Rcpp::Named("manifests") = pt.vars.manifests,
                                                Rcpp::Named("latents") = pt.vars.latents);

   Rcpp::List combined = Rcpp::List::create(
     Rcpp::Named("parameter_table") = pt_Rcpp,
     Rcpp::Named("user_defined") = pt.user_defined,
     Rcpp::Named("algebras") = pt_algebras,
     Rcpp::Named("variables") = pt_variables,
     Rcpp::Named("new_parameters") = pt.alg.new_parameters,
     Rcpp::Named("new_parameters_free") = pt.alg.new_parameters_free
   );

   return(combined);
 }
