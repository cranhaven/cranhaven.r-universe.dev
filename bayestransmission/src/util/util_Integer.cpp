#include "util/Integer.h"
#include <ostream>
#include <string>

namespace util {

Integer::Integer(int x) : Object()
{
    value = x;
}

int Integer::intValue() const
{
    return value;
}

void Integer::set(int n)
{
    value = n;
}

std::string Integer::className() const
{
    return "Integer";
}

void Integer::write(std::ostream &os) const
{
    os << value;
}

} // namespace util
