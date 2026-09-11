#include <Rcpp.h>
#include <barry/barry.hpp>
#include <barry/models/defm.hpp>

using namespace Rcpp;

//' Extract the counters from a DEFM model
//'
//' Counters are functions that are defined in terms of the change statistics.
//' The counters also contain a hasher that is used internally to check whether
//' an array's support is cached or not (see details).
//' @details
//' If the hash of an array--which are built using each counters' individual
//' hashing functions--matches an existing array, then, the DEFM models reduce
//' computational burden by recycling computations of the normalizing constant.
//' For example, if a model only includes terms (counters) that do not feature
//' individual-level characteristics like gender or age, then most likely all
//' arrays in that model will use the same normalizing constant.
//' @param model An object of class [DEFM].
//' @returns
//' - The function `get_counters` returns an external pointer to an object of 
//' class `DEFM_counters`.
//' @export
//' @name get_counters
// [[Rcpp::export(rng = false, name = 'get_counters')]]
SEXP get_counters_cpp(SEXP & model) {

    if (!Rf_inherits(model, "DEFM"))
        stop(
            "The function `get_counters()` only works for objects of class DEFM."
        );

    Rcpp::XPtr< defm::DEFM > ptr(model);
    Rcpp::XPtr< defm::DEFMCounters > res(
        ptr->get_counters(),
        false
    );

    res.attr("class") = "DEFM_counters";

    return res;

}

// [[Rcpp::export(rng = false, name = "print_defm_counters", invisible = true)]]
SEXP print_counters_cpp(SEXP & x) 
{

    if (!Rf_inherits(x, "DEFM_counters"))
        stop("The passed object is not of class DEFM_counters.");

    Rcpp::XPtr< defm::DEFMCounters >(x)->print();

    return x;
}

//' @export
//' @rdname get_counters
//' @param counters An object of class `DEFM_counters`.
//' @param i Integer from 0 to nterms - 1. Counter to get.
//' @returns
//' - The method `[.DEFM_counters` returns an individual counter of class
//' `DEFM_counter`.
// [[Rcpp::export(rng = false, name = "`[.DEFM_counters`")]]
SEXP get_counter_cpp(SEXP & counters, size_t i)
{

    if (!Rf_inherits(counters, "DEFM_counters"))
        stop("The passed object is not of class DEFM_counters.");
    
    Rcpp::XPtr< defm::DEFMCounters > counters_ptr(counters);

    if (counters_ptr->size() <= i)
        stop(
            "There are only " +
            std::to_string(counters_ptr->size()) +
            " counters. Remember that indexing starts at 0."
        );

    Rcpp::XPtr< defm::DEFMCounter > counter(
        &counters_ptr->operator[](i),
        false
    );

    counter.attr("class") = "DEFM_counter";

    return counter;

}

// [[Rcpp::export(rng = false, invisible = true, name = "print_defm_counter")]]
SEXP print_counter_cpp(SEXP & x) {

    Rcpp::XPtr< defm::DEFMCounter >(x)->print();

    return x;

}

//' @export
//' @rdname get_counters
//' @param counter An object of class `DEFM_counter`.
//' @param new_name,new_desc Strings with the new name and new description, 
//' respectively. If empty, no side effect.
//' @details
//' The function `set_counter_info()` can be used to modify a counter name
//' and description. This is especially useful when a name is particularly
//' long.
//' @returns
//' - `set_counter_info()` invisibly returns the modified counter.
// [[Rcpp::export(rng = false, name = "set_counter_info", invisible = true)]]
SEXP set_counter_info_cpp(
    SEXP & counter,
    std::string new_name = "",
    std::string new_desc = ""
) {

    if (!Rf_inherits(counter, "DEFM_counter"))
        stop("The object `counter` is not of class `DEFM_counter`.");

    Rcpp::XPtr< defm::DEFMCounter > ptr(counter);

    if (new_name != "")
        ptr->set_name(new_name);

    if (new_desc != "")
        ptr->set_description(new_desc);

    return counter;

}

// [[Rcpp::export(rng = false, name = "as_list_defm_counter")]]
List as_list_defm_counter_cpp(SEXP & x)
{
    if (!Rf_inherits(x, "DEFM_counter"))
        stop("The object `counter` is not of class `DEFM_counter`.");

    Rcpp::XPtr< defm::DEFMCounter > ptr(x);

    return List::create(
        _["name"] = wrap(ptr->get_name()),
        _["description"] = wrap(ptr->get_description())
    );
    
}

//' @export
//' @rdname get_counters
//' @returns
//' - The `length` method for `DEFM_counters` returns the number of counters
//' in the vector. This should match the return from [nterms_defm()].
// [[Rcpp::export(rng = false, name = "length.DEFM_counters")]]
SEXP length_defm_counters(SEXP & x)
{
    if (!Rf_inherits(x, "DEFM_counters"))
        stop("The passed object is not of class DEFM_counters.");
    
    return wrap(Rcpp::XPtr< defm::DEFMCounters >(x)->size());
}
