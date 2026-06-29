#include <string>
#include <algorithm>
bool is_ok(char c) {
  return (!(std::isdigit(c) || c=='.'));
}
bool is_number(std::string& s)
{
  return !s.empty() && std::find_if(s.begin(), s.end(), is_ok) == s.end();
}
