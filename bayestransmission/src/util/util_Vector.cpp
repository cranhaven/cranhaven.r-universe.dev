#include "util/Vector.h"
#include <ostream>
#include <string>

namespace util {

void Vector::ensure(int m)
{
    if (m <= cap)
        return;

    int newcap = cap;
    while (m > newcap)
        newcap *= 2;

    Object **newx = new Object*[newcap];

    for (int i=0; i<n; i++)
        newx[i] = x[i];
    for (int i=n; i<newcap; i++)
        newx[i] = 0;

    delete x;
    x = newx;
    cap = newcap;
}

Vector::Vector(int c) : Object()
{
    cap = c;
    n = 0;
    x = new Object*[cap];
    for (int i=0; i<cap; i++)
        x[i] = 0;
}

Vector::~Vector()
{
    if (x)
        delete x;
}

string Vector::className() const
{
    return "Vector";
}

void Vector::write(ostream &os) const
{
    Object::write(os);
    os << "(" << n << "/" << cap << ")";
    for (int i=0; i<size(); i++)
        os << "\n\t" << x[i];
}

} // namespace util
