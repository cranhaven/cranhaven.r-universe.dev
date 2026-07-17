#include <Rcpp.h>
#include <string>
#include <algorithm>

using namespace Rcpp;

// Function to check if a character is a digit
bool is_digit(char c) {
  return std::isdigit(static_cast<unsigned char>(c));
}

// Custom comparator for mixed sorting
bool mixedorder_compare(const std::string& str1, const std::string& str2) {
  auto it1 = str1.begin();
  auto it2 = str2.begin();

  while (it1 != str1.end() && it2 != str2.end()) {
    if (is_digit(*it1) && is_digit(*it2)) {
      // Compare numeric parts as integers
      size_t len1 = 0, len2 = 0;
      int num1 = std::stoi(&(*it1), &len1);
      int num2 = std::stoi(&(*it2), &len2);

      if (num1 < num2) return true;
      if (num1 > num2) return false;

      it1 += len1;
      it2 += len2;
    } else {
      // Compare non-numeric parts
      if (*it1 < *it2) return true;
      if (*it1 > *it2) return false;

      ++it1;
      ++it2;
    }
  }

  // Handle the case where one string is a prefix of the other
  return it1 == str1.end() && it2 != str2.end();
}

// Function to perform mixed sorting
// [[Rcpp::export]]
IntegerVector get_MIXEDSORT_cpp(CharacterVector x, bool dec = true) {
  IntegerVector indices = Rcpp::seq_along(x) - 1;

  // Convert the input vectors to strings outside the comparator
  std::vector<std::string> strings;
  for (int i = 0; i < x.size(); ++i) {
    strings.push_back(Rcpp::as<std::string>(x[i]));
  }

  auto comparator = [strings, dec](int i, int j) {
    if (dec) {
      return mixedorder_compare(strings[j], strings[i]);
    } else {
      return mixedorder_compare(strings[i], strings[j]);
    }
  };

  std::sort(indices.begin(), indices.end(), comparator);

  return indices + 1;
}

