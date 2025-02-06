# Guidelines

The code is organized as follows:

- One file per method, except for operators.
- Name file after method.
- Place helper functions used across methods in utils file.
- Container type order in files: set, unordered set, multiset, unordered multiset, map, unordered map, multimap, unordered multimap, stack, queue, priority 
queue, vector, deque, forward list, list.
- Use templates in C++ methods (other than constructors), if that saves space.
- Use 1-based indices in R methods.
- Use Rcpp vector types (IntegerVector, etc.), not R's cryptic SEXP types.
