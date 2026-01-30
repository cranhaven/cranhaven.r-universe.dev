#ifndef PAIRHASH_H
#define PAIRHASH_H

// for unordered_map < <uint32_t, uint32_t>, T >
namespace std {
template <>
struct hash<std::pair<uint32_t, uint32_t>> {
  inline uint64_t operator()(const pair<uint32_t, uint32_t>& k) const {
    //should produce no collisions
    //http://stackoverflow.com/a/24693169/1069256
    //return f << (CHAR_BIT * sizeof(size_t) / 2) | s;
    //http://stackoverflow.com/questions/2768890/how-to-combine-two-32-bit-integers-into-one-64-bit-integer?lq=1
    return (uint64_t) k.first << 32 | k.second;
  }
};
}

#endif
