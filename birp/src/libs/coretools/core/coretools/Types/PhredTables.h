#ifndef PHREDTABLES_H_
#define PHREDTABLES_H_

#include <cstddef>
namespace coretools::phredtables {

double phred2linear(size_t I) noexcept;
double hpphred2linear(size_t I) noexcept;

}

#endif
