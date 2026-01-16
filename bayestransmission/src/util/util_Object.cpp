#include "util/Object.h"
#include <ostream>

namespace util {

Object::Object()
{
    index = ++indexcounter;
}

Object::~Object()
{
}

long Object::hash()
{
    return index;
}

int Object::compare(Object *y)
{
    if (y == 0 || y->index < index)
        return 1;
    if (index < y->index)
        return -1;
    return 0;
}

std::string Object::className() const
{
    return "Object";
}

void Object::write(std::ostream &os) const
{
    os << className() << "[" << index << "]";
}

void Object::write(std::ostream &os)
{
    return static_cast<const Object*>(this)->write(os);
}

} // namespace util
