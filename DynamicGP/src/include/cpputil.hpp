#ifndef __CPPUTIL_HPP__
#define __CPPUTIL_HPP__
#include<iostream>
#include<string>
double quantile(double *, double, int);
void writeVector(std::ostream& os, std::string name, double* vec, int len);
#endif
