from numpy import *

def print_array(N, denum, perLine=8):
    for i in r_[0:N:perLine]:
        s = "\t    "
        for j in r_[0:perLine]:
            s += "%.9e, "%pow(10., -(i+j)/denum)
            if i+j == N-1:
                s = s[:-1] # get rid of trailing ','
                break
        print(s[:-1])

header = """
/*
 * PhredTables.cpp
 *
 *      Automatically created by Python-Script PhredProbabilityTypesTables.py
 */

#include "PhredTables.h"
#include <array>
#include <cstddef>

namespace coretools::phredtables {
"""

footer = """
} // end namespace coretools::phredtables
"""

print(header)

print("""
double phred2linear(size_t I) noexcept {
	constexpr std::array<double, 256> map = {
""", end="")
print_array(256, 10., 6)
print("""};
	return map[I];
}
""")

print("""
double hpphred2linear(size_t I) noexcept {
	static const std::array<double, 65536> map = {
""", end="")
print_array(65536, 1000., 6)
print("""};
	return map[I];
}
""")

print(footer)
