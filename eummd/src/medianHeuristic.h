#ifndef GUARD_medianheuristic_h
#define GUARD_medianheuristic_h

#include<vector>
#include <cstddef>

// template <class vectype>
// typename vectype::value_type medianHeuristic(vectype X);


double medianHeuristic(std::vector<double> X);

double medianHeuristicAlreadySorted(std::vector<double> X);

double naive_multiv_medianHeuristic(const std::vector<double>& Z, 
                                    int dZi, 
                                    int nZi, 
                                    int kmethod=1);

#endif
