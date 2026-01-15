#ifndef ___JACKHEADER___
#define ___JACKHEADER___

#include "symbolicQspray.h"

using namespace QSPRAY;
using namespace RATIOOFQSPRAYS;
using namespace SYMBOLICQSPRAY;


typedef std::vector<int> Partition;

class pairHasher {
public:
  size_t operator()(const std::pair<int, int>& ij) const {
    size_t seed = 0;
    seed ^= ij.first + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    seed ^= ij.second + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    return seed;
  }
};

template <typename T>
using IntIntMap = std::unordered_map<std::pair<int, int>, T, pairHasher>;

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
int _N(Partition, Partition);

template <typename T>
T _betaratio(Partition, Partition, int, T);

int weight(Partition);

template <typename numT>
numT ipow(numT, unsigned);

#endif
