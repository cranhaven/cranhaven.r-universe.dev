
#ifndef STRINGCONSTANTS_H_
#define STRINGCONSTANTS_H_

#include <string_view>

namespace coretools::str {
constexpr std::string_view whitespaces     = " \t";
constexpr std::string_view whitespacesPlus = " \f\n\r\t\v+"; // All whitespaces and '+' sign for numbers
}

#endif
