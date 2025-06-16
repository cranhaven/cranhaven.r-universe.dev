#include <iostream>

// For monitoring behavior of string compare replacement
// No need of predeclaration/header since it will be loaded by linker
#include <cstring>
extern "C" {
int strncmp_debug(const char* s1, const char* s2, int n) {
  std::cout << "Print from strcmp0\n";
  std::cout << "  arg1: " << s1 << "\n";
  std::cout << "  arg2: " << s2 << "\n";
  std::cout << "  arg3: " << n << "\n";
  return strncmp(s1, s2, n);
}
}

